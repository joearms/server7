-module(elib2_erl_metadata).

-uuid("f0ecb768-2e0e-4717-b34e-c9e0169d6551").

-tags([erlang,metadata,uuid,tags]).

-description("this module adds metadata and uuids to a file, or extracts metadata from a file").

%% -compile(export_all).

-export([add_tags/0, add_tags/1,
	 update_db/0, update_db/1,
	 extract_meta/1,
	 warn_non_exported/2]).

%% the only reason I import these function below here is to test
%% the import resolution code in this module

-import(lists,[append/1]).
-import(filelib, [wildcard/1]).
-import(file, [read_file/1]).
-import(file, [write_file/2]).

%% add_tags()     add keyword, description and uuid tags to a all
%%                modules in the current directory
%% add_tags(Dir)  Dir = Abs dir name
%%                This is a *descructive* operation that modifies the contents
%%                of the Erlang files

%% update_db()
%% update_db(Dir) update the global db
%%                no warnings etc. are given

%% analyse()      analyse the global db with all the files in the local directory


add_tags() ->
    add_tags(this_dir()).

add_tags(Dir) ->
    [add_meta_to_file(I) || I <- erl_files(Dir)].

update_db() ->
    update_db(this_dir()).

%% X is a file or directory

update_db(X) ->
    Out = glob_dir(),
    case filelib:is_dir(X) of
	true ->
	    [update_db(File, Out) || File <- erl_files(X)];
	false ->
	    update_db(X, Out)
    end.

erl_files(Dir) ->
    [I || I <- filelib:wildcard(Dir ++ "/*.erl"), 
	  valid_file(I)].

valid_file(F) ->
    case filename:basename(F) of
	".#" ++ _ -> false;
	_         -> true
    end.
    
update_db(File, Out) ->
    %% we can save ourselfs a lot of bother if we just use the time stamps
    Module = filename:basename(File, ".erl"),
    OutFile = Out ++ Module ++ ".binf",
    %% io:format("Outfile=~p~n",[OutFile]),
    case needs_updating(File, OutFile) of
	true ->
	    case extract_meta(File) of
		{ok, Meta} ->
		    %% find the module and location
		    Location = maps:get(location, Meta),
		    %% io:format("Adding:~p => ~p ~p~n",[Module, Location, OutFile]),
		    case file:read_file(OutFile) of
			{error,enoent} ->
			    %% this is the first time we made the file so things are
			    %% easy
			    Term = #{vsns => [Meta],
				     primary => Location,
				     review_primary => false},
			    write_meta(OutFile, Term);
			{ok, Bin} ->
			    Old = binary_to_term(Bin),
			    %% see if this version is in the DB
			    Vsns = maps:get(vsns, Old), 
			    case find_version(File, Vsns, []) of
				{found, Rest} ->
				    %% replace with the new version
				    %% since this is just a replacement we don't need to
				    %% change the primary not the review status
				    NewVsns = [Meta|Rest],
				    Term1 = Old#{vsns := NewVsns},
				    write_meta(OutFile, Term1);
				not_found ->
				    %% this file was not found so we now have a new version
				    %% we don't change the primary location but we do set
				    %% the review flag
				    NewVsns = [Meta|Vsns],
				    Term1 = Old#{vsns := NewVsns,
						 review_primary => true},
				    write_meta(OutFile, Term1)
			    end
		    end;
		{error, E} ->
		    io:format("Cannot extract metadata from:~p reason: ~p~n",[File,E])
	    end;
	false ->
	    io:format("Unchanged : ~p~n",[Module])
    end.

write_meta(OutFile, Term) ->
    io:format("Updating metadata for:~p~n",[OutFile]),
    ok = file:write_file(OutFile, term_to_binary(Term)).

find_version(Loc, [#{location := Loc}|T], Rest) ->
    {found, T ++ Rest};
find_version(Loc, [H|T], L) ->
    find_version(Loc, T, [H|L]);
find_version(_, [], _) ->
    not_found.

%% add_meta_to_file(F) -> 
%%   1) parses file
%%      aborts if it cannot parse the file
%%   2) if the file has a UUID and keywords, description
%%      return these values
%%   3) modify the file if necessary adding correct meta data
%%   4) analyses forms
%    5) updates the global information about this module

%% add_tags(File) => {ok, #{file=> F, tags => tags, desc => tags, uuid=> uuid}}
%%                   |  {error, str}

add_meta_to_file(File) ->
    case epp:parse_file(File, "", "") of
	{ok, Forms} ->
	    Extra1 = add_uuid_str(Forms),
	    Extra2 = add_tags_str(Forms),
	    Extra3 = add_desc_str(Forms),
	    Extra  = Extra1 ++ Extra2 ++ Extra3,
	    case Extra of 
		"" ->
		    ok;
		_ -> 
		    {Before, After} = split_file(File),
		    io:format("** warning: ~s has been modifed~n",[File]),
		    write_file(File, [Before,Extra,After])
	    end;
	_ ->
	    {error,{cannot_parse_file, File}}
    end.

extract_meta(AbsFileName) ->
    case epp:parse_file(AbsFileName, "", "") of
	{ok, Forms} ->
	    MapE = analyse_forms(Forms),
	    Map5 = MapE#{module => list_to_atom(filename:basename(AbsFileName, ".erl")),
			 location => AbsFileName,
			 last_modified => filelib:last_modified(AbsFileName),
			 size          => filelib:file_size(AbsFileName)},
	    {ok, Map5};
	_Error ->
	    {error, {cannot_parse, AbsFileName}}
    end.

glob_dir() ->
    Dest = os:getenv("HOME") ++ "/nobackup/erlmods/",
    case filelib:is_dir(Dest) of
	true ->
	    Dest;
	false ->
	    io:format("The directory ~s does not exist~n"
		      "Please create it you wish to run this program~n"
		      "*** Running this program might modify your erlang source code files~n"
		      "*** and will write data to the above directory~n" ,[Dest]),
	    exit(eNoDir)
    end.

%% read_meta(File) ->  #Meta
%% check(File) -> true
%%   Adds meta data to a file if it is not there
%%   and updates the global meta data database

%%   #Meta has the following keys
%%   module, uuid, tags, description, time_last_modified, size
%%   functions, exports

%% uuid, tags, description are taken from annotatations

 uuid() ->
    uuid:to_string(uuid:uuid4()).


add_tags_str(Forms) ->
    Tags = lists:append([I || {attribute,_,tags,I} <- Forms]),
    case Tags of
	[] -> "-tags([k1,k2]).\n\n";
	_  -> ""
    end.

add_desc_str(Forms) ->
    Desc = lists:append([I || {attribute,_,description,I} <- Forms]),
    case Desc of
	[] -> "-description(\"edit me\").\n\n";
	_  -> ""
    end.
    
add_uuid_str(Forms) ->
    case [I || {attribute,_,uuid,I} <- Forms] of
	[_UUid] -> "";
	[] ->
	    UUid = uuid(), %% generate a new uuid
	    %% note we spell out the uuid with $'s here so that the regexp will not match it 
	    [$-,$u,$u,$i,$d,$(,$"] ++ UUid ++ "\").\n\n";
	_ -> 
	    ""
    end.


split_file(File) ->
    %% read file and split after -uuid(...)
    %% failing that after -module(...)
    {ok, B} = read_file(File),
    case re:split(B,"(-uuid\\(.*\\).*\\n)",[notempty,{parts,2}]) of
	[Before,Match,After] ->
	    {<<Before/binary,Match/binary,$\n>>, After};
	_ ->
	    case re:split(B,"(-module\\(.*\\).*\\n)",[notempty,{parts,2}]) of
		[Before,Match,After] ->
		    {<<Before/binary,Match/binary,$\n>>, After};
		_ ->
		    io:format("cannot spit module:~p~n",[File]),
		    exit(1)
	    end
    end.

%% analyse forms(Forms) ->
%%  #{export_all => Bool, 
%%    exports => [{F,A]], 
%%    called = [{M,F,A}] 
%%    defined => [{F,A}]

analyse_forms(Forms) ->
    M0 = #{imports => [],
	   export_all => false,
	   tags => [],
	   exports => [],
	   defined => [],
	   called => []},
    M1 = lists:foldl(fun analyse_form/2, M0, Forms),
    #{imports := I, called := C, defined := Def1, exports := E0} = M1,
    %% io:format("M1=~p~n",[M1]),    
    C1 = resolve_called(C, I),
    %% io:format("resolved=~p~n",[C1]),
    %% I'll sort the entries - this is just to make the data easier to
    %% read if I pront it out
    M2 = M1#{called := lists:sort(C1), defined := lists:sort(Def1),
	     exports := lists:sort(E0)},
    %% finally we remove the import declarations .. no so relevant really
    maps:remove(imports, M2).

analyse_form({attribute,_,import,{Mod,FAL}}, #{imports := I} = M ) -> 
    I1 = [{Fa,Mod} || Fa <- FAL] ++ I,
    M#{imports := I1};
analyse_form({attribute,_,compile,export_all}, M) ->
    M#{export_all := true};
analyse_form({attribute,_,export,L}, #{exports := E0} = M) ->
    M#{exports := L ++ E0};
analyse_form({function,_,Name,Arity,Clauses}, 
	     #{defined := D, called := C} = M) ->
    C1 = add_called(Clauses, C),
    M#{defined := [{Name,Arity}|D], called := C1};
analyse_form({attribute,_,module,Mod}, M) ->
    M#{module => Mod};
analyse_form({attribute,_,uuid,U}, M) ->
    M#{uuid  => U};
analyse_form({attribute,_,tags,T}, #{tags:=T0} = M) ->
    M#{tags := T ++ T0};
analyse_form({attribute,_,description,D}, M) ->
    M#{description => D};
analyse_form(_X, M) -> 
    %% io:format("skipping:~p~n",[X]),
    M.
    
add_called({call,_,{atom,_,Func}, Args}, L) ->
    Fa = {'$nomod', Func, length(Args)},
    L1 = addem(Fa, L),
    %% and recursively in the arguments
    add_called(Args, L1);
add_called({call,_,{remote,_,
			{atom,_,Mod},
			{atom,_,Func}},
		Args}, L) ->
    Fa = {Mod, Func, length(Args)},
    L1 = addem(Fa, L),
    %% and recursively in the arguments
    add_called(Args, L1);
add_called(T, L) when is_tuple(T) ->
    %% io:format("expand:~p~n",[T]),
    add_called(tuple_to_list(T), L);
add_called([H|T], L) ->
    L1 = add_called(H, L),
    add_called(T, L1);
add_called([], L) ->
    L;
add_called(_, L) ->
    %% io:format("skipping:~p~n",[X]),
    L.

addem(X, L) ->
    case lists:member(X, L) of
	true -> L;
	false -> [X|L]
    end.

resolve_called([{'$nomod',F,A}|T], Imports) ->
    %% nomod means there was no module given
    %% check if this was in an import declaraction
    case lists:keysearch({F,A}, 1, Imports) of
	{value,{_,Mod}} ->
	    [{Mod,F,A}|resolve_called(T, Imports)];
	false ->
	    %% ths might be in the module Erlang
	    case erlang:is_builtin(erlang, F, A) of
		true ->
		    [{erlang,F,A}|resolve_called(T, Imports)];
		false ->
		    [{'$self',F,A}|resolve_called(T, Imports)]
	    end
    end;
resolve_called([H|T], I) ->
    [H|resolve_called(T, I)];
resolve_called([], _) ->
    [].
	    

%% the file Out depnds upon In.
%% Out might not exist
%% needs_updating is true if 
%%   time_last_modified(In) > time_last_modified(Out) 
%%   or Out does not exist

needs_updating(In, Out) ->
    case filelib:is_file(Out) of
	true ->
	    %% check the time stamps
	    case {filelib:last_modified(In), filelib:last_modified(Out)} of
		{T1, T2} when T1 > T2 ->
		    true;
		_ ->
		    false
	    end;
	false ->
	    true
    end.
    
this_dir() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

warn_non_exported(Mod, FAs) ->
    case get_primary(Mod) of
	{found, Map} ->
	    [warn_if_not_defined(Mod, I, Map) || I <- FAs];
	not_found ->
	    void
    end.

warn_if_not_defined(Mod, FA, #{export_all := true, defined := D}) ->
    warn_if_not_member(Mod, FA, D);
warn_if_not_defined(Mod, FA, #{export_all := false, exports := E}) ->
    warn_if_not_member(Mod, FA, E).

warn_if_not_member(Mod, FA, L) ->
    case lists:member(FA, L) of
	true -> void;
	false ->
	    {Func,Arity} = FA,
	    io:format("*** ~w : ~w/~w might not be defined~n", [Mod,Func,Arity])
    end.

get_primary(Mod) ->
    Glob = glob_dir(),
    File = Glob ++ atom_to_list(Mod) ++ ".binf",
    %% io:format("File=~p~n",[File]),
    case file:read_file(File) of
	{ok, Bin} ->
	    Map = binary_to_term(Bin),
	    Prim = maps:get(primary, Map),
	    Vsns = maps:get(vsns, Map),
	    get_primary(Prim, Vsns);
	{error, _} ->
	    not_found
    end.

get_primary(Loc, [#{location := Loc}=H|_]) -> {found, H};
get_primary(Loc, [_|T])                    -> get_primary(Loc, T);
get_primary(_, [])                         -> not_found.



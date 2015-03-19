-module(abc).

-uuid("af9f4485-1aaa-4d6c-b1f9-768f38feab20").

-tags([k1,k2]).


-description("foo").

 
-compile(export_all).

-export([check/0, update_meta/1, report/1]).

%% the only reason I import these function below here is to test
%% the import resolution code in this module

-import(lists,[append/1]).
-import(filelib, [wildcard/1]).
-import(file, [read_file/1]).
-import(file, [write_file/2]).

%% the most commonly used command is check
%% the meta data is added to 

test() ->
    {ok, Cwd} = file:get_cwd(),
    %% add_meta_to_file("./elib2_erl_metadata.erl").
    extract_meta(Cwd ++ "/elib2_erl_metadata.erl").

%% update<_meta(F) -> 
%%   1) parses file
%%      aborts if it cannot parse the file
%%   2) if the file has a UUID and keywords, description
%%      return these values
%%   3) modify the file if necessary adding correct meta data
%%   4) analyses forms
%    5) updates the global information about this module

%% update_meta(File) => {ok, #{file=> F, tags => tags, desc => tags, uuid=> uuid}}
%%                   |  {error, str}

add_meta_to_file(File) ->
    case epp:parse_file(File, "", "") of
	{ok, Forms} ->
	    {Map1, Extra1} = add_tags(Forms, #{}, []),
	    {Map2, Extra2} = add_desc(Forms, Map1, Extra1),
	    {Map3, Extra3} = add_uuid(Forms, Map2, Extra2),
	    possible_update_file(File, Extra3);
	_ ->
	    {error,"cannot parse file"}
    end.

add_meta_to_mods() ->
    add_meta_to_all_erl_in_dir(".").

add_meta_to_all_erl_in_dir(Dir) ->
    L = filelib:wildcard(Dir ++ "/*.erl"),
    [add_meta_to_file(I) || I <- L].

extract_meta(AbsFileName) ->
    case epp:parse_file(AbsFileName, "", "") of
	{ok, Forms} ->
	    MapE = analyse_forms(Forms),
	    Map5 = MapE#{module => list_to_atom(filename:basename(AbsFileName, ".erl")),
			 location => AbsFileName,
			 last_modified => filelib:last_modified(AbsFileName),
			 size          => filelib:file_size(AbsFileName)},
	    {ok, Map5};
	Error ->
	    {error, {cannot_parse, AbsFileName}}
    end.


update_meta(aMeta) ->
    21.

update_glob_data(Dir, #{module := Mod, location := Loc} =  Map) ->
    io:format("update:~p~n", [{Mod, Dir,Map}]),
    File = Dir ++ atom_to_list(Mod) ++ ".info",
    io:format("file:~p~n",[File]),
    case file:read_file(File) of
	{error, enoent} ->
	    Data = #{locations    => [Loc],
		     master       => Loc,
		     needs_master => false,
		     meta         => [Map]},
	    ok = file:write_file(File, term_to_binary(Data)),
	    io:format("updates:~p~n",[File]),
	    ok;
	{ok, Bin} ->
	    OldData = binary_to_term(Bin),
	    exit({oop, OldData})
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


check() ->
    %% check the current directory for metadata
    {ok, Cwd} = file:get_cwd(),
    F = [Cwd ++ "/" ++ I || I <- filelib:wildcard("*.erl")],
    [{I, extract_meta(I)}  || I <- F].


%% read_meta(File) ->  #Meta
%% check(File) -> true
%%   Adds meta data to a file if it is not there
%%   and updates the global meta data database

%%   #Meta has the following keys
%%   module, uuid, tags, description, time_last_modified, size
%%   functions, exports

%% uuid, tags, description are taken from annotatations

report(Dir) ->
    to_html(check(Dir)).

to_html(L) ->
    io:format("L=~p~n",[L]),
    ["<table>",
     [["<tr>",format_item(I),"</tr>\n"] || I <- L],
     "</table>"].

format_item({File,Value}) ->
    [td(File), format_value(Value)].

format_value(#{error := E}) ->
    td(red("**error**" ++ E));
format_value(#{tags := Tags, description := D, uuid := U}) ->
    [td(format_tags(Tags)), td(red(D)), td(U)].

format_tags(L) ->
    [[atom_to_list(I)," "] || I <- L].

td(X) ->
    ["<td>",X,"</td>"].

red(X) ->
    ["<font color='red'>",X,"</font>"].

check(Dir) ->
    R = Dir ++ "/*.erl",
    F = [I || I <- filelib:wildcard(R)],
    [{I, update_meta(I)}  || I <- F].

 uuid() ->
    uuid:to_string(uuid:uuid4()).


add_tags(Forms, Map, Extra) ->
    Tags = lists:append([I || {attribute,_,tags,I} <- Forms]),
    case Tags of
	[] ->
	    {Map#{tags=>[k1,k2]}, "-tags([k1,k2]).\n\n" ++ Extra};
	_ -> 
	    {Map#{tags=>Tags}, Extra}
    end.

add_desc(Forms, Map, Extra) ->
    Desc = lists:append([I || {attribute,_,description,I} <- Forms]),
    case Desc of
	[] ->
	    {Map#{description=>"edit me"}, "-description(\"edit me\").\n\n" ++ Extra };
	_ -> 
	    {Map#{description => Desc}, Extra}
    end.
    
add_uuid(Forms, Map, Extra) ->
    case [I || {attribute,_,uuid,I} <- Forms] of
	[UUid] -> 
	    {Map#{uuid=>UUid}, Extra};
	[] ->
	    UUid = uuid(), %% generate a new uuid
	    {Map#{uuid => UUid}, [$-,$u,$u,$i,$d,$(,$"] ++ UUid ++ "\").\n\n" ++ Extra};
	_ -> 
	    {Map#{error => "** mutiple UUIDs in file"}, Extra}
    end.
	
possible_update_file(_, "") -> ok;
possible_update_file(File, Extra) -> 
    {Before, After} = split_file(File),
    io:format("** warning: ~s has been modifed~n",[File]),
    write_file(File, [Before,Extra,After]).

split_file(File) ->
    %% read file and split after uuid(...)
    %% failing that after        module(...)
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

pre(X) ->
    iolist_to_binary(io_lib:format("<pre>~p</pre>~n",[X])).

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

add_all_defined_funcs_to_exported(#{defined := D, exports := E} = M) ->
    E1 = lists:foldl(fun addem/2, E, D),
    M#{exports := lists:sort(E1), defined := lists:sort(D)}.

analyse_form({attribute,_,import,{Mod,FAL}}, #{imports := I} = M ) -> 
    I1 = [{Fa,Mod} || Fa <- FAL] ++ I,
    M#{imports := I1};
analyse_form({attribute,_,compile,export_all}, M) ->
    M#{export_all := true};
analyse_form({attribute,_,export,L}, #{exports := E0} = M) ->
    M#{exports := L ++ E0};
analyse_form({function,_,Name,Arity,Clauses}, 
	     #{imports := I, defined := D, called := C} = M) ->
    C1 = add_called(Clauses, C),
    M1 = M#{defined := [{Name,Arity}|D], called := C1};
analyse_form({attribute,_,module,Mod}, M) ->
    M#{module => Mod};
analyse_form({attribute,_,uuid,U}, M) ->
    M#{uuid  => U};
analyse_form({attribute,_,tags,T}, #{tags:=T0} = M) ->
    M#{tags := T ++ T0};
analyse_form({attribute,_,description,D}, M) ->
    M#{description => D};
analyse_form(X, M) -> 
    io:format("skipping:~p~n",[X]),
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
	    
update_called(X, Y) ->
    X.



    
		 
    

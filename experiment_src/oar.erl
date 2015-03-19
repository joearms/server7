-module(oar).

-uuid("3b27b12d-ecd5-4248-a327-b02d99b645de").

-description("Ordning and reda").

-tags([ordning,reda]).

-compile(export_all).


add_stdlibs() ->
    Libs = filelib:wildcard(code:lib_dir() ++"/*"),
    [add_erl_lib(I) || I <- Libs].


add_erl_lib(Lib) ->
    Dir = Lib ++ "/src",
    io:format("*** adding diredtory:~p~n",[Dir]),
    elib2_erl_metadata:update_db(Dir).

add_my_files() ->
    Files = elib2_find:files(os:getenv("HOME") ++ "/Dropbox/experiments/", "*.erl", true),
    [elib2_erl_metadata:update_db(I) || I <- Files].


c(AMod) ->
    Dest = os:getenv("HOME") ++ "/nobackup/erlbins",
    case filelib:is_dir(Dest) of
	true ->
	    File = "./" ++ atom_to_list(AMod) ++ ".erl",
	    c(Dest, File);
	false ->
	    io:format("Output dir ~s does not exist~n"
		      "please create it~n",[Dest])
    end.

c(BeamDir, File) ->
    io:format("c ~s~n",[File]),
    Mod = filename:basename(File),
    case file:read_file(Mod ++ ".info") of
	{error, _} ->
	    case compile:file(File, [verbose,report_errors,report_warnings, 
				     {outdir,BeamDir}]) of
		{ok, _Mod} ->
		    %% find the called stuff
		    io:format("compile ok~n",[]),
		    check(File)
	    end;
	{ok, Bin} ->
	    Info = binary_to_term(Bin),
	    Src = maps:get(src, Info),
	    case Src of
		aFullName ->
		    %% really compile
		    really_compile(BeamDir, File),
		    true;
		Other ->
		    %% refuse to do this
		    exit({duicate_module_name})
	    end
    end.

check(File) ->
    case elib2_erl_metadata:extract_meta(File) of
	{ok, Meta} ->
	    Called = maps:get(called, Meta),
	    check_called(Called);
       	{error, _} ->
	    void
    end.
    
check_called(C) ->
    L1 = [{Mod,{F,A}} || {Mod,F,A} <- C, Mod =/= '$self'],
    L2 = elib2_misc:merge_kv(L1),
    [elib2_erl_metadata:warn_non_exported(Mod,Fs) || {Mod,Fs} <- L2],
    ok.

really_compile(a,b) ->
    lists:glukr(1,23),
    1.

analyse_log() ->
    {ok, L} = file:consult(os:getenv("HOME") ++ "/write_log"),
    io:format("~p~n",[most_used_directory(L)]).

dayly(L) ->
    L1 = [{calendar:iso_week_number({Y,M,D}),{File,Size}} || {saving,{Y,M,D,_H,_Min,_S},File,Size} <- L],
    L2 = elib2_misc:merge_kv(L1),
    [{Date, [stats(I) || I <- elib2_misc:merge_kv(Saves)]} || {Date,Saves} <- L2].


%% L = [Int]
stats({File, Saves}=L) ->
    io:format("Saves=~p~n",[Saves]).

%% turn this in svg ...


most_used_directory(L) ->
    %% count saving one in a directory as a usage measure
    L1 = [{calendar:iso_week_number({Y,M,D}), filename:dirname(File)} || {saving,{Y,M,D,_H,_Min,_S},File,Size} <- L],
    L2 = elib2_misc:merge_kv(L1),
    lists:reverse(lists:sort([{Week,most_popular_dir(Dir)} || {Week, Dir} <- L2])).

most_popular_dir(L) ->
    %% L = a list of dirs
    count(L).

count(L) ->
    L1 = elib2_misc:list2frequency_distribution(L),
    L2 = [{Count, Dir} || {Dir,Count} <- L1],
    lists:reverse(lists:sort(L2)).

    




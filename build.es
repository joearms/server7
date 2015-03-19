#!/usr/bin/env escript

main(_) ->
    %% dependenciies are stored under tmp

    filelib:ensure_dir("./tmp/"),

    %% fetch all the code and check the right branches

    fetch("cowlib", "git://github.com/ninenines/cowlib.git"),
    fetch("cowboy", "git://github.com/ninenines/cowboy.git"),
    fetch("ranch", "git://github.com/ninenines/ranch.git "),

    checkout("cowlib", "1.2.0"),
    checkout("cowboy", "90ae31998e8d0887b9efe4b441136ac047708bb9"),
    checkout("ranch", "1.0.0"),

    %% these have to be compiled with the correct include paths

    compile("cowlib", "../include"),
    compile("cowboy", "../../../tmp"), %% so the inlude_lib in cowboy works
    compile("ranch", "../include"),

    %% make those pesky .app files

    make_app("cowlib"),
    make_app("cowboy"),
    make_app("ranch"),
    
    %% now we're ready to rock and roll
    %% compile everything in src
    cd("src"),
    os:cmd("erlc *.erl"),
    cd(".."),
    AppName = "esimple_webserver",
    Target = os:getenv("HOME") ++ "/bin/" ++ AppName,
    BeamDirs =
	["./tmp/cowboy/src",
	 "./tmp/cowlib/src",
	 "./tmp/ranch/src",
	 "./src"],
    pack(AppName, Target, BeamDirs).


fetch(Name, Ref) ->
    filelib:ensure_dir("./tmp/"),
    Dir = "tmp/" ++ Name,
    case filelib:is_dir(Dir) of
	true ->
	    io:format("~s exists~n",[Name]);
	false ->
	    Cmd = "git clone " ++ Ref ++ " " ++ "tmp/" ++ Name, 
	    io:format("fetching ~s (~s) ~n",[Name, Cmd]),
	    os:cmd(Cmd)
    end.

checkout(Name, Tag) ->
    file:set_cwd("tmp/" ++ Name),
    R = os:cmd("git checkout " ++ Tag),
    io:format("R=~p~n",[R]),
    file:set_cwd("../.."),
    pwd("top").

 
compile(Name, Include) ->
    io:format("Compiling ~s~n",[Name]),
    file:set_cwd("tmp/" ++ Name ++ "/src"),
    pwd("src dir"),
    Cmd = "erlc -I " ++ Include ++ " *.erl",
    io:format("Cmd: ~s~n",[Cmd]),
    R = os:cmd(Cmd),
    io:format("R ++++++::~s~n",[R]),
    pwd("here1"),
    file:set_cwd("../../.."),
    pwd("top 2").
    

    %% file:set_cwd("src"),
    %% pwd(),
    %% Cmd = "erlc -I " ++ Include ++ " *.erl",
    %% io:format("~s~n",[Cmd]),
    %% V = os:cmd(Cmd),
    %% io:format("~s~n",[V]).

pwd(A) ->
    {ok, X} = file:get_cwd(),
    io:format("~s =  ~s~n",[A,X]).

cd(X) ->
    file:set_cwd(X).

pack(AppName, Dest, Dirs) ->
    MainMod = "starter",
    %% Dest must be in your path
    io:format("Dirs:~p~n",[Dirs]),
    Files = [I || D <- Dirs, I <- files(D)] ++ internal_files(),
    io:format("Files:~p~n",[Files]),
    Contents = [get_file(I) || I <- Files],
    %% io:format("Contents=~p~n",[Contents]),
    {ok, {"mem", ZipBin}} = zip:create("mem", Contents, [memory]),
    Shebang = "#!/usr/bin/env escript\n",
    Comments = "%% mutiline comments dont work ...\n",
    %% multiline comments give an error
    %% #! /usr/bin/env escript
    %% 
    %%! -escript main Module
   
    Args = io_lib:format("%%! -escript main ~s\n",[MainMod]),
    Script = iolist_to_binary([Shebang, 
			       Comments, 
			       Args, ZipBin]),

    file:write_file(Dest, Script),
    os:cmd("chmod u+x " ++ Dest),
    io:format("Created: ~s~n",[Dest]),
    init:stop().

    
internal_files() ->
    filelib:wildcard("*.js") 
	++ filelib:wildcard("*.html") 
	++ filelib:wildcard("*.css")
	++ filelib:wildcard("*.ico")
	++ filelib:wildcard("*.ehe").


get_file(File) ->
    %% io:format("read:~p~n",[File]),
    case file:read_file(File) of
	{ok, Bin} ->
	    Bin1 = strip(File, Bin),
	    {filename:basename(File), Bin1};
	_ ->
	    exit({read_error, File})
    end.

strip(File, Bin) ->
    case filename:extension(File) of
	".beam" ->
	    {ok, {_,Bin1}} = beam_lib:strip(Bin),
	    Bin1;
	_ ->
	    Bin
    end.

abort_if_errors(L) ->
    E = [F || {F,error} <- L],
    case E of
	[] -> void;
	_ ->
	    io:format("There files could not be compiled:~p~n",[E]),
	    exit(aborted)
    end.

get_code(L) ->
    [compile(File) || Pattern <- L,File <- filelib:wildcard(Pattern)].

compile(F) ->
    io:format("compiling:~s~n",[F]),
    {F, compile:file(F,[binary,compressed,report_errors,report_warnings])}.


need(Key, L) ->
    case maps:is_key(Key, L) of
	true  -> maps:get(Key, L);
	false -> exit({missing,key,Key})
    end.



files(Dir) ->
    filelib:wildcard(Dir ++ "/*.beam") ++
	filelib:wildcard(Dir ++ "/*.app").

make_app(Name) ->
    {ok, [{application,A,R}]} = 
	file:consult("./tmp/" ++ Name ++ "/src/" ++ Name ++ ".app.src"),
    %% find the module names
    L = filelib:wildcard("./tmp/" ++ Name ++ "/src/*.erl"),
    Mods = [mod_name(I) || I <- L],
    io:format("************here:~p => ~p~n",[R, Mods]),
    R1 = subst(R, Mods),
    F = {application,A,R1},
    io:format("************here:~p ~n",[F]),
    Out = "./tmp/" ++ Name ++ "/src/" ++ Name ++ ".app", 
    {ok, S} = file:open(Out, [write]),
    io:format(S,"~p.~n", [F]),
    file:close(S),
    io:format("Written:~p~n",[Out]).

subst([{modules,[]}|T], Mods) -> [{modules,Mods}|T];
subst([H|T], Mods)            -> [H|subst(T, Mods)];
subst([], _)                  -> [].

mod_name(F) ->
    list_to_atom(filename:basename(F, ".erl")).

    

    
    
	 
	 


    

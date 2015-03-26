-module(esimple_webserver).

-uuid("95db17d9-dd5a-4f53-bd6c-bba5d8fc7d30").

-tags([simple,erlang,webserver]).

-description("A simple erlang webserver").

-compile(export_all).

-define(trace(X),(io:format("~p:~p ~p~n",[?MODULE,?LINE,X]))).

internal_files() ->
    ["jquery.min.js",
     "svg_lib.js",
     "websock.js"].

%% batch is the entry point from a makefile
%% main is the entry point from an escript

batch() ->
    main(["8080"]),
    receive
	after infinity ->
		void
	end.

main([]) ->
    %% no port
    main(["8080"]);

main([X]) ->
    case (catch list_to_integer(X)) of
	{'EXIT', _} ->
	    io:format("Usage esimple_webserver <Port>~n"),
	    init:stop();
	Port ->
	    Name = is_script(),
	    io:format("Name:~p~n",[Name]),
	    Handle = fetch_internal_code(Name),
	    io:format("Handle:~p~n",[Handle]),
	    R = make_request_handler(Handle),
	    Env = #{request_handler => R, port => Port},
	    Dispatch = cowboy_router:compile(
			 [
			  {'_', [
				 {"/websocket/[...]", esimple_ws_handler, 0},
				 {'_', esimple_http_handler, 
				  Env}
				]}]),
	    V = cowboy:start_http(http, 100, 
				  [{port, Port}],
				  [{env, [{dispatch, Dispatch}]}]),
	    io:format("go to http://localhost:" ++ X ++ "/index.html:~n",[]),
	    register(?MODULE, spawn(fun() -> loop([]) end)),
	    io:format("yes:V:~p~n",[V]),
	    io:format("here:~p~n",[whereis(?MODULE)]),
	    receive
	    after
		infinity ->
		    void
	    end
    end.

is_script() ->
    case (catch escript:script_name()) of
	{'EXIT',_} ->
	    no;
	File ->
	    {yes, File}
    end.

loop(S) ->
    receive
	_ ->
	    loop(S)
    end.

rpc(Q) ->
    ?MODULE ! {self(), Q},
    receive
	{?MODULE, R} ->
	    R
    end.

fetch_internal_code({yes, File}) ->	   
    case escript:extract(File, []) of
	{ok, [_,_,_,{archive,Bin}]} ->
	    case zip:zip_open(Bin, [memory]) of
		{ok, Handle} ->
		    Handle;
		{error, E} ->
		    io:format("Cannot open archive:~p~n",[E]),
		    exit(fatal)
	    end;
	E ->
	    io:format("Cannot extract archive from executable:~p~n",[E])
    end;
fetch_internal_code(no) ->
    void.


%%----------------------------------------------------------------------
%% a request handler is a fun with the following type signature
%%  Request(File, Args) -> {ok, MimeType, Headers, Bin} | {error, Why}
%%  it is called from the dispatcher


make_request_handler(ZIP) ->
    fun(File, Args, Req) ->
	    Parts = filename:split(File),
	    Ext   = filename:extension(File),
	    ?trace({parts,Parts,ext,Ext}),
	    process_request(Parts, Ext, File, Args, ZIP, Req)
    end.

remap("/Users/" ++ _ = F) ->
    F;
remap("/" ++ File) ->
    "./" ++ File;
remap(F) ->
    F.

process_request(["/","cgi",ModStr],_,_,Args, Zip, Req) ->
    Mod = list_to_atom(ModStr),
    case lists:member(Mod, mod_allow:allow()) of
	true ->
	    Mod:process(Args, Req);
	false ->
	    {ok, ".html", "mod_not_allowed"}
    end;
process_request(["/","weblib"],_,_,Args, Zip, Req) ->
    ?trace({weblib,Args}),
    [{<<"mod">>,MB},{<<"func">>,FB}] = Args,
    Mod = bin2atom(MB),
    Func = bin2atom(FB),
    {ok, A, _} = cowboy_req:body_qs(Req),
    [{<<"string">>,Json}] = A,
    Erl = mochijson2:decode(Json),
    ?trace({weblib,Mod,Func,Json,Erl}),
    Result = apply(Mod, Func, [Erl]),
    {ok,".html",Result};

process_request(["/"| Parts], ".web", File, [{<<"page">>,Page}|_]=Args, Zip, _) ->

    F1 = remap(File),
    ?trace({her1,file,File,f1,F1}),
    InFile = F1,
    OutFile = "./tmp/" ++ filename:basename(InFile, ".web") ++ ".erl",
    Mod = list_to_atom(filename:basename(File,".web")),
    ?trace({web,input,InFile,output,OutFile,mod,Mod,Args}),
    case recompile_and_load(InFile, OutFile, Mod) of 
	ok ->
	    Func = list_to_atom(binary_to_list(Page)),
	    ?trace({mod,Mod,margs,Func}),
	    HTML = app_compiler_vsn2:get_page(Mod, Func, Args),
	    {ok, ".html", HTML};
	{error, What} ->
	    {ok, ".html", ["Erorr recompiling ", File, lib_html:pre(What)]}
    end;
process_request(Parts, Ext, File, Args, Zip, _) ->    
    F1 = remap(File),
    ?trace({parts,Parts,f1,F1,ext,Ext}),
    case file:read_file(F1) of
	{ok, Bin} ->
	    process_request1(Parts, Ext, File, Args, Bin);
	{error, _} = X ->
	    ?trace({cannot,read,F1}),
	    io:format("** read the zip:~p not yet done~n",[Zip]),
	    X
    end.

recompile_and_load(InFile, OutFile, Mod) ->
    case elib2_misc:out_of_date(InFile, OutFile) of
	true ->
	    ?trace(recompiling),
	    %% cross compile the source
	    app_compiler_vsn2:compile(InFile),
	    Ret = compile:file(OutFile),
	    ?trace({compiler_returns,Ret}),
	    case Ret of
		{ok, Mod} ->
		    code:purge(Mod), %% always do this
		    Ret1 = code:load_abs(Mod),
		    ?trace({code_loader,Ret1}),
		    ok;
		Other ->
		    {error, Other}
	    end;
	false ->
	    ?trace(module_not_recompiled),
	    ok
    end.


%%----------------------------------------------------------------------
%% process_request -- this where all event end up
%%   Parts = list of file segments
%%   Ext = file extension
%%   File = origoinal filename
%%   Args = ...
%%   Bin =  file contents

%% The following special processing occurs
%%   .ehe => expanded
%%   .md  => expanded
%%   .erl => evaluated


process_request1(Parts, ".ehe", File, Args, Bin) ->
    ?trace({setupehe,File,Args}),
    %% interesing ehe_vsn4 offers parse_string
    case ehe_vsn4:parse_string(binary_to_list(Bin)) of
	{ok, Parse} ->
	    ?trace({ehe, parsed,ok}),
	    %% Make an environment and expand
	    {_Binding,BinOut} = ehe_vsn4:eval(Parse, Args),
	    {ok, ".ehe", BinOut};
	{error, E} ->
	    {error,E}
    end;
process_request1(["sites",Site,"books", Book,File],".md", _File, Args, Bin) ->
    ?trace({ho,Site,Book,File}),
    %% step one assume .md contains ehe so we expand it
    case ehe_vsn4:parse_string(binary_to_list(Bin)) of
	{ok, Parse} ->
	    ?trace({ehemd, parsed,Parse}),
	    %% Make an environment and expand
	    {_Binding,BinOut} = ehe_vsn4:eval(Parse, Args),
	    %% BinOut is assumed to be MD
	    ?trace({outhere,BinOut}),
	    ParsedMd = md_vsn7:parse_string(binary_to_list(BinOut)),
	    Content = make_html(ParsedMd),
	    ?trace({content,Content}),
	    B10 = expand_site_template(File, Site, Book, Content),
	    {ok, ".html", B10};
	{error, E} ->
	    {error,E}
    end;
process_request1(Parts, ".md", File, Args, Bin) ->
    %% md parsing always works
    %% no such thing as an error ...
    %% EEGH
    Parse = md_vsn7:parse_string(binary_to_list(Bin)),
    Bin1 = make_html(Parse),
    ?trace({md,parsed,ok}),
    %% ?trace({t2b,Bin}),
    Header = header(),
    {ok, ".html", <<Header/binary, Bin1/binary>>};
process_request1(_,Ext,_,_,Bin) ->
    %% No special processing
    {ok, Ext, Bin}.

make_html(L) ->
    %% ?trace({make_html,L}),
    Tree = {node,body,#{},
            [
             {node,'div',#{class=>"chapter"}, L}
            ]},
    tree_lib:tree_to_binary(Tree).

header() ->
    <<"<meta charset='UTF-8'>\n<link rel='stylesheet' href='md.css'/>\n">>.


expand_site_template(File, Site, Book, Content) ->
    TemplateFile = "./sites/" ++ Site ++ "/templates/main.ehe",
    Sidebar = make_sidebar(Site, Book, File),
    case file:read_file(TemplateFile) of
	{ok, Bin} ->
	    {ok, Parse} = ehe_vsn4:parse_string(binary_to_list(Bin)),
	    {_, Result} = ehe_vsn4:eval(Parse, 
					[{'File',File},
					 {'SideBar',Sidebar},
					 {'Content',Content}]),
	    Result;
	E ->
	    ?trace({cannot_read_site_template,Site,File}),
	    <<"cannot find site template">>
    end.
	    
make_sidebar(Site, Book, Current) ->
    Re = "./sites/" ++ Site ++ "/books/" ++ Book ++ "/*.md",
    L = filelib:wildcard(Re),
    ?trace({files,L,current,Current}),
    %% example of L =  ["./sites/demo1/books/doc/ehe.md",
    %%                  "./sites/demo1/books/doc/howto.md"]
    L1 = [filename:basename(I) || I <- L],
    ?trace({l1,L1,current,Current}),
    iolist_to_binary([make_link(Site, Book, I, Current) || I<- L1]).

make_link(Site, Book, I, Current) ->
    Href = "/sites/" ++ Site ++ "/books/" ++ Book ++ "/" ++ I,
    Class = link_class(I, Current),
    ?trace({title,I}),
    ["<li class='",Class,"'><a href='",Href,"'>",filename_to_title(I),"</a></li>"].

link_class(X, X) -> "selected";
link_class(_, _) -> "not_selected".

filename_to_title(".md")  -> [];
filename_to_title([$_|T]) -> [$\s|filename_to_title(T)];
filename_to_title([H|T])  -> [H|filename_to_title(T)].

bin2atom(B) ->
    list_to_atom(binary_to_list(B)).

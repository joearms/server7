-module(app_compiler_vsn2).
-compile(export_all).
-import(lists, [reverse/1]).

-define(trace(X),(io:format("~p:~p ~p~n",[?MODULE,?LINE,X]))).

%% Assume a file called test23.web
%% @html{main}
%%    <? include_js(foo) ?>
%%    <h1>Hello</h1>
%%    <? A,B,C,foo(X) ?>
%% @html{foo}
%% @html{nice}
%% <style>
%%  ddd
%% </style>
%% @erlang
%% ...

test() ->
    batch(['test1.web']).


batch([Atom]) ->
    File = atom_to_list(Atom),
    file:set_cwd(".."),    
    compile("./site/" ++ File),
    init:stop().

get_page(Mod, Func, Args) ->
    case (catch get_page0(Mod, Func, Args)) of
	{'EXIT', Why} ->
	    io:format("something was wrong ::~p :: ~p~n",[{Mod,Func,Args},Why]);
	OK ->
	    OK
    end.

get_page0(Mod, Func, Args) ->
    Map = Mod:Func(Args),
    maps:get(html, Map).

compile(File) ->
    %% File is the input file :: something like /site/bingo.web
    %% We extract the module "bingo"
    %% We write two output files tmp/bingo.erl.tmp tmp/bngo.erl
    _CWD = file:get_cwd(),
    %% ?trace({compile,File, cwd, CWD}),
    Segments = get_segments(File),
    Mod = filename:basename(File,".web"),
    %% io:format("Segments=~p~n",[Segments]),
    Processed = [process_segment(I, Mod) || I <- Segments],
    Frags = make_fragments(Processed),
    Erl   = make_erlang(Processed),
    Out      = "./tmp/" ++ Mod ++ ".erl.tmp",
    FinalErl = "./tmp/" ++ Mod ++ ".erl",
    %% ?trace({out,Out,module,Mod,finalErl,FinalErl}),
    %% pull all the erlang to the front so we get module/imports ok
    Stuff = [header(Mod),Erl, Frags],
    %% ?trace({stuff,Stuff}),
    %% elib2_misc:check_io_list(Stuff),
    _Res = file:write_file(Out, Stuff),
    %% ?trace({pretty,Out,FinalErl,res,Res}),
    pretty(Out, FinalErl),
    io:format("Created: ~s~n",[FinalErl]).

get_segments(File) ->
    _Mod = filename:rootname(File),
    {ok, B} = file:read_file(File),
    %% convert to a string and stay a string NO Binaries
    %% to do unicode
    S = binary_to_list(B),
    collect_segments([$\n|S], []).

test1() ->
    file:write_file("magic.html", [gen1:main(1)]).

pretty(In, Out) ->
    {ok, L} = epp:parse_file(In,"",""),
    L1 = [[erl_pp:form(I),"\n"] || I <- L],
    file:write_file(Out, L1).

collect_segments("\n@html{" ++ T, L) ->
    {Body, Rest} = collect_stuff(T, []),
    {Name, T2} = get_name(Body, []),
    collect_segments(Rest, [{function0,html,Name,T2}|L]);
collect_segments("\n@markdown{" ++ T, L) ->
    {Body, Rest} = collect_stuff(T, []),
    {Name, T2} = get_name(Body, []),
    collect_segments(Rest, [{function0,md,Name,T2}|L]);
collect_segments("\n@erlang" ++ T, L) ->
    {X, T1} = collect_stuff(T, []),
    collect_segments(T1, [{erlang,X}|L]);
collect_segments([$\n,$%|T], L) ->
    T1 = skip_comment(T),
    collect_segments(T1, L);
collect_segments([], L) ->
    lists:reverse(L);
collect_segments(X, L) ->
    io:format("Unexpected stuff:~p~n",[X]),
    exit({ooooerr,X,L}).

header(Mod) ->
    ["-module(",Mod,").\n-compile(export_all).\n"].

make_fragments(L) ->
    %% collect all the html
    lists:flatten([code_gen1(Name, Body) || {function1,Name,Body} <- L]).

make_erlang(L) ->
    %% collect all the html
    lists:flatten([X || {erlang, X} <- L]).

code_gen1(Name, Body) ->
    lists:flatten(io_lib:format("~p(In) ->\n~s\n", 
				[list_to_atom(Name), Body])).

process_segment({function0, Type, Name, T1}, Mod) ->
    Segments   = collect_html_segments(T1, []),
    Code = code_gen(Segments, Type, Mod),
    %% io:format("Code:~s~n",[Code]),
    {function1, Name, Code};
process_segment({erlang, _} = X, _) ->
    X.

code_gen(L,Type, Mod) ->
    Vars = make_vars(L),
    L1 = lists:zip(Vars, L),
    Body = [code_gen1(X) || X <- L1],
    C = mk_combine(Mod, Type, Vars),
    lists:flatten(Body ++ [C]).

mk_combine(Mod, Type,L) ->
    %% Mod = str() Type = atom()
    io:format("mk_combine: Mod:~p Type:~p~n",[Mod,Type]),
    ["app_compiler_vsn2:combine_fragments(",
     Mod,",",atom_to_list(Type),",[",
     interlieve(",", L),
     "]).\n\n"].

interlieve(_,[H]) -> [H];
interlieve(Sep,[H1,H2|T]) -> [H1,Sep|interlieve(Sep,[H2|T])];
interlieve(_, []) -> []. 

code_gen1({Var,{inline_html,X}}) ->
    [Var," = ", 
     lists:flatten(io_lib:format("~p,\n",[#{html=> list_to_binary(X)}]))];

code_gen1({Var, {inline_erlang,X}}) ->
    [Var," = begin \n", X, "\n end,\n"].

make_vars(L) ->
    [mk_var(I) || I<- lists:seq(1, length(L))].

mk_var(I) ->
    "Var@" ++ integer_to_list(I).

get_name("}" ++ T, L) -> {reverse(L), T};
get_name([H|T], L)    -> get_name(T, [H|L]).

skip_comment("\n" ++ _ = T) -> T;
skip_comment([_|T]) -> skip_comment(T);
skip_comment([]) -> [].

collect_stuff("\n@html{" ++ _=T, L)     -> {reverse(L), T};
collect_stuff("\n@markdown{" ++ _=T, L) -> {reverse(L), T};
collect_stuff("\n@erlang" ++ _=T, L)    -> {reverse(L), T};
collect_stuff([], L)                    -> {reverse(L), []};
collect_stuff([H|T], L)                 -> collect_stuff(T, [H|L]).

collect_html_segments([], L) ->
    reverse(L);
collect_html_segments("<? " ++ T, L) ->
    {X, T1} = collect_inline_erlang(T, []),
    collect_html_segments(T1, [{inline_erlang,X}|L]);
collect_html_segments(T, L) ->
    {X, T1} = collect_inline_text(T, []),
    collect_html_segments(T1, [{inline_html,X}|L]).

collect_inline_erlang([], _)        -> exit({missing, "?>"});
collect_inline_erlang("?>" ++ T, L) -> {reverse(L), T};
collect_inline_erlang([H|T], L)     -> collect_inline_erlang(T, [H|L]).

collect_inline_text([], L)            -> {reverse(L), []};
collect_inline_text("<?" ++ _ = T, L) -> {reverse(L), T};
collect_inline_text([H|T], L)         -> collect_inline_text(T, [H|L]).

%%----------------------------------------------------------------------
%% THis gets done at runtime

combine_fragments(Mod, md, L) ->
    L1 = [#{html=>"", once => [{app_compiler_vsn2, md_css}]}|L],
    X = combine_fragments(Mod, html, L1),
    Str = maps:get(html, X),
    Bin = iolist_to_binary(Str),
    io:format("Here Str1=~p~n",[Bin]),
    Parse = md_vsn7:parse_string(binary_to_list(Bin)),
    Bin1 = make_html(Parse),
    X#{html := Bin1};
combine_fragments(Mod, html, L) ->
    io:format("combine_frags:~p ~n",[Mod]),
    io:format("Combining:~p~n",[L]),
    add_frags(L, [], []).

md_css(_) ->
    #{html => "<link rel='stylesheet' href='md.css'/>\n"}.


make_html(L) ->
    %% ?trace({make_html,L}),
    Tree = {node,body,#{},
            [
             {node,'div',#{class=>"chapter"}, L}
            ]},
    tree_lib:tree_to_binary(Tree).


add_frags([#{once := K, html := Html}|T], Once, L) ->
    {Once1, L1} = add_once(K, Once, L),
    add_frags(T, Once1, [Html|L1]);
add_frags([#{html := H}|T], Once, L) ->
    add_frags(T, Once, [H|L]);
add_frags([], Once, L) ->
    #{html => lists:reverse(L), once => Once}.

add_once([{Mod,Func}=H|T], Once, L) ->
    case lists:member(H, Once) of
	true ->
	    add_once(T, Once, L);
	false ->
	    %% call the things
	    io:format("Adding:~p~n",[{Mod,Func}]),
	    case (catch apply(Mod,Func,[void])) of
		#{html := Html} ->
		    add_once(T, [H|Once], [Html|L]); 
		Other ->
		    io:format("$$$$$$ ~p~n",[{Mod,Func,Other}]),
		    exit({oops,Mod,Func,Other})
	    end
    end;
add_once([O|T], Once, L) ->
    io:format("**** Add once error:~p~n",[O]),
    add_once(T, Once, L);
add_once([], Once, L) ->
    {Once, L}.


include(X) ->
    #{include => X}.


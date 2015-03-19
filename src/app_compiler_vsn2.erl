-module(app_compiler_vsn2).
-compile(export_all).
-import(lists, [reverse/1]).

-define(trace(X),(io:format("~p:~p ~p~n",[?MODULE,?LINE,X]))).

%% Assume a file called test23.web
%% @function{main}
%%    <? include_js(foo) ?>
%%    <h1>Hello</h1>
%%    <? A,B,C,foo(X) ?>
%% @function{foo}
%% @function{nice}
%% <style>
%%  ddd
%% </style>
%% @erlang
%% ...


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
    %% File something like /site/test1.web
    ?trace({compile,File}),
    Segments = get_segments(File),
    Mod = filename:basename(File,".web"),
    %% io:format("Segments=~p~n",[Segments]),
    Processed = [process_segment(I) || I <- Segments],
    Frags = make_fragments(Processed),
    Erl = make_erlang(Processed),
    Out = filename:rootname(File,".web") ++ ".erl.tmp",
    ?trace({out,Out,mod,Mod}),
    %% pull all the erlang to the front so we get module/imports ok
    file:write_file(Out, [header(Mod),Erl, Frags]),
    FinalErl = filename:rootname(File,".web") ++ ".erl",
    pretty(Out, FinalErl),
    io:format("Created: ~s~n",[FinalErl]).

get_segments(File) ->
    Mod = filename:rootname(File),
    {ok, B} = file:read_file(File),
    %% convert to a string and stay a string NO Binaries
    %% to do unicode
    S = binary_to_list(B),
    collect_segments([$\n|S], []).

test1() ->
    file:write_file("magic.html", [final(gen1:main(1))]).

pretty(In, Out) ->
    {ok, L} = epp:parse_file(In,"",""),
    L1 = [[erl_pp:form(I),"\n"] || I <- L],
    file:write_file(Out, L1).
    
test() ->
    compile("test1.web").


collect_segments("\n@function{" ++ T, L) ->
    {Body, Rest} = collect_stuff(T, []),
    {Name, T2} = get_name(Body, []),
    collect_segments(Rest, [{function0,Name,T2}|L]);
collect_segments("\n@erlang" ++ T, L) ->
    {X, T1} = collect_stuff(T, []),
    collect_segments(T1, [{erlang,X}|L]);
collect_segments([$\n,$%|T], L) ->
    T1 = skip_comment(T),
    collect_segments(T1, L);
collect_segments([], L) ->
    lists:reverse(L);
collect_segments(X, _) ->
    io:format("Unexpected stuff:~p~n",[X]),
    exit({ooooerr,X}).

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

process_segment({function0, Name, T1}) ->
    Segments   = collect_html_segments(T1, []),
    Code = code_gen(Segments),
    %% io:format("Code:~s~n",[Code]),
    {function1, Name, Code};
process_segment({erlang, _} = X) ->
    X.

code_gen(L) ->
    Vars = make_vars(L),
    L1 = lists:zip(Vars, L),
    Body = [code_gen1(X) || X <- L1],
    C = mk_combine(Vars),
    lists:flatten(Body ++ [C]).

mk_combine(L) ->
    ["app_compiler_vsn2:combine_fragments([",
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
skip_comment([H|T]) -> skip_comment(T);
skip_comment([]) -> [].

collect_stuff("\n@function{" ++ _=T, L) -> {reverse(L), T};
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

combine_fragments(L) ->
    io:format("Combining:~p~n",[L]),
    L1 = lists:foldl(fun add_frag/2, #{html=>[]}, L),
    final(L1).

final(#{html := H}) ->
    #{html => reverse(H)}.

add_frag(X, M) ->
    add_html(X, M).

add_html(#{html := H}, #{html := A} = M) ->
    M#{html := [H|A]};
add_html(_, M) ->
    M.

include(X) ->
    #{include => X}.


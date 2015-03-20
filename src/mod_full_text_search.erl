-module(mod_full_text_search).

-uuid("392e58b5-7395-40f2-a999-85e705748014").

-tags([fill,text,search]).

-description("Full text search. This is the adapterm module callable from javascript").


-compile(export_all).

-compile(export_all).

-define(trace(X),(io:format("~p:~p ~p~n",[?MODULE,?LINE,X]))).

process(X,Req) ->
    %% ?trace({here,X,Req}),
    {ok, A, _} = cowboy_req:body_qs(Req),
    ?trace({args,A}),
    do(A).

do([{<<"string">>,String}])->
    Bin = mochijson2:decode(String),
    ?trace({string,Bin}),
    Str = binary_to_list(Bin),
    Map = full_text_db:search(Str),
    Reply = format_response(Map),
    ?trace({reply,Reply}),
    {ok, ".html", Reply}.

format_response(#{alternatives:=A,files := F, chosen_keywords := K} = M) ->
    [
     "<h1>Alternatives</h1>",
     erl2html:pre(A),
     "<h1>Chosen keywords</h1>",
     erl2html:pre(K),
     
     [li(I) || I <- F]].

li(X) ->
    ["<li><a href='",X,"'>",X,"</a></li>\n"].

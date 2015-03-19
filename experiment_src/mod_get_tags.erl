-module(mod_get_tags).

-uuid("d7c54808-c91f-41e6-aca1-132de1953126").

-tags([k1,k2]).

-description("edit me").



-compile(export_all).

-define(trace(X),(io:format("~p:~p ~p~n",[?MODULE,?LINE,X]))).

process(X,Req) ->
    %% ?trace({here,X,Req}),
    {ok, A, _} = cowboy_req:body_qs(Req),
    ?trace(A),
    do(A).

do([{<<"tags">>,Tags}])->
    Z = mochijson2:decode(Tags),
    ?trace({tags,Z}),
    Reply = [ [<<"<p>">>,Tag,<<" is 10">> ] || Tag <- Z],
    ?trace({reply,Reply}),
    {ok, ".html", Reply}.

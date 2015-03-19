-module(starter).

-uuid("819a87d5-b306-4464-8267-f5cb3864b0f2").

-tags([application,starter,nice]).

-description("Starts applications order by number *** very nice ***").

-compile(export_all).

main() ->
    start().

main(_) ->
    start().


start() ->
    io:format("Starting appliction servers~n"),
    L = servers(),
    [start_server(I) || I <- L].

start_server({N, Name, F}) ->
    case file:consult(F) of
	{ok, [Map]} ->
	    %% io:format("~p ~p~n",[I,Map]),
	    do_start(N, Name, Map);
	E ->
	    io:format("Error consulting ~p (~p) aborting~n",[F, E]),
	    init:stop()
    end.

do_start(N, Name,  #{path := P, start:={M,F,A}, env := E, description := D}) ->
    io:format("~w : ~s :: Starting: ~s~n",[N, Name, D]),
    P1 = [elib2_misc:expand_env_vars(I) || I <- P],
    io:format("P1=~p~n",[P1]),
    [code:add_patha(I) || I <- P1],
    V = (catch apply(M,F,A)),
    io:format("starter: apply(~p,~p,~p) => ~p~n",[M,F,A,V]),
    true.

servers() ->
    L = filelib:wildcard("servers/*.start"),
    lists:sort([order(I) || I <- L]).

order(File) ->
    io:format("File:~p~n",[File]),
    [_,Ns,Name,"start"] = string:tokens(File, "/-."),
    {list_to_integer(Ns), Name, File}.



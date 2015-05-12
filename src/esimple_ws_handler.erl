-module(esimple_ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

-import(mochijson2, [encode/1, decode/1]).

init(Req, Opts) ->
    Path = cowboy_req:path(Req),
    Parts = binary_to_list(Path),
    P1 = string:tokens(Parts,"/"),
    %% io:format("ws_handler Parts:~p~n",[P1]),
    ["websocket", ModStr] = P1,
    Mod = list_to_atom(ModStr),
    Pid = spawn(Mod, start, [self()]),
    {cowboy_websocket, Req, Pid}.

websocket_handle({text, Json}, Req, Pid) ->
    %% io:format("Received:~p~n",[Json]),
    {struct, L} = decode(Json),
    L1 = [{binary_to_atom(Key),Val}||{Key,Val} <- L],
    M = maps:from_list(L1),
    %% io:format("Decoded = ~p ~p ~p~n",[L,M,Pid]),
    Pid ! {self(), M},
    {ok, Req, Pid};
websocket_handle(Data, Req, State) ->
    io:format("???*** esimple_ws_handler data:~p~n",[Data]),
    {ok, Req, State}.

binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).

websocket_info([{cmd,_}|_]=L, Req, Pid) ->
    B = list_to_binary(encode([{struct,L}])),
    {reply, {text, B}, Req, Pid, hibernate};
websocket_info(X, Req, Pid) when is_map(X) ->
    L = maps:to_list(X),
    B = list_to_binary(encode([{struct,L}])),
    {reply, {text, B}, Req, Pid, hibernate};


websocket_info(Info, Req, State) ->
    io:format("*** esimple_ws_handler info:~p~n",[Info]),
    {ok, Req, State}.

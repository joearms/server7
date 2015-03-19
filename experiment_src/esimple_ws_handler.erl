-module(esimple_ws_handler).

-uuid("9a0e1315-7530-414e-823f-e80c5d099995").

-tags([websockets,webserver]).

-description("Websockets handler for the simple web server").



-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, _State) ->
    S = self(),
    io:format("ws_handler: new_socket ~p~n",[S]),
    Path = cowboy_req:path(Req),
    io:format("ws_handler: Path ~p~n",[Path]),
    <<"/websocket/", BM/binary>> = Path,
    Mod = binary_to_atom(BM),
    Pid = spawn(Mod,start,[self()]),
    {cowboy_websocket, Req, Pid}.

websocket_handle({text,Data}, Req, Pid) ->
    %% io:format("ws_handler here1:~p~n",[Data]),
    Struct = svgweb_mochijson2:decode(Data),
    Map = struct2map(Struct),
    %% io:format("sending ~p to:~p~n",[Map, Pid]),
    Pid ! {self(), Map},
    {ok, Req, Pid}.

websocket_info({controlling_process,Pid}, Req, _) ->
    {ok, Req, Pid};
websocket_info({msg,Msg}, Req, Pid) ->
    io:format("info2:Msg:~p ~n",[Msg]),
    {reply, {text,Msg}, Req, Pid};
websocket_info(Maps, Req, Pid) ->
    S = [map2struct(I) || I <- Maps],
    %% io:format("S=~p~n",[S]),
    E = (catch svgweb_mochijson2:encode(S)),
    %% io:format("E=~p~n",[E]),
    {reply, {text,E}, Req, Pid}.


map2struct(M) when is_map(M)->
    L1 = maps:to_list(M),
    {struct, [{K,map2struct(V)} || {K,V} <- L1]};
map2struct(X) ->
    X.

binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).
		
struct2map({struct, L}) ->
    L1 = [{binary_to_atom(Key),struct2map(Val)} || {Key,Val} <- L],
    maps:from_list(L1);
struct2map(X) ->
    X.

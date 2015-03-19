%%%-------------------------------------------------------------------
%%% @author Armstrong Joe <ejoearm@ejoearm-eld>
%%% @copyright (C) 2015, Armstrong Joe
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2015 by Armstrong Joe <ejoearm@ejoearm-eld>
%%%-------------------------------------------------------------------
-module(full_text_db).

-uuid("72c573e1-a551-4a7e-99e0-dca17f13786a").

-tags([gen_server,fulltext]).

-description("gen server interface to fulltext db").


-behaviour(gen_server).

%% API
-export([start/0, search/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Dict = full_text_dict:read_dict(),
    {ok, Dict}.

search(Str) ->
    gen_server:call(?MODULE, {search, Str}).

handle_call({search, Str}, _From, Dict) ->
    Reply = full_text_dict:search(Str, Dict),
    {reply, Reply, Dict}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

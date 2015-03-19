%%%-------------------------------------------------------------------
%%% @author Armstrong Joe <ejoearm@ejoearm-eld>
%%% @copyright (C) 2015, Armstrong Joe
%%% @doc
%%%
%%% @end
%%% Created :  4 Mar 2015 by Armstrong Joe <ejoearm@ejoearm-eld>
%%%-------------------------------------------------------------------
-module(db_ets_pairs).

-uuid("8c586fdd-119e-46fc-a96a-d6db8fe00417").

-tags([k1,k2]).

-description("edit me").


-behaviour(gen_server).

%% -compile(export_all).		  

%% db_ets_pairs

%% example1:

%% db_ets_pairs: start_link(db_filenames)
%%    Makes a global process called db_filenames
%%    stored under the global directory $ets_tables
%% db_ets_pairs: bin2index(db_filenames, FileName :: binary() ) -> Index
%% db_ets_pairs: index2bin(db_filenames, Index) -> {ok, FileName:: binary()} | error

%% API
-export([start/1, bin2index/2,index2bin/2, max_index/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

bin2index(Name, Bin) when is_binary(Bin) ->
    gen_server:call(Name, {bin2index, Bin}).

index2bin(Name, Index) when is_integer(Index) ->
    gen_server:call(Name, {index2bin, Index}).

max_index(Name) ->
    gen_server:call(Name, max_index).

stop(Name) ->
    gen_server:call(Name, stop).

start(Tab) ->
    D = elib2_misc:glob_dir("ets_tables"),
    gen_server:start({local, Tab}, ?MODULE, [D,Tab], []).

init([Dir, Tab]) ->
    File = Dir ++ atom_to_list(Tab) ++ ".tab",
    case ets:file2tab(File) of
	{ok, Tid} ->
	    {ok, {Tid,File,0},10000};
	{error, _Reason}->
	    %% io:format("Tab:~p open error: ~p~n",[Tab,Reason]),
	    Tid = ets:new(table, [set, private]),
	    ets:insert(Tid, {free_index,1}),
	    {ok, {Tid,File,0},10000}
    end.

handle_call({bin2index,Bin}, _From, {Tid,Bfile,N} =State) ->
    case ets:lookup(Tid, Bin) of
	[{_,Index}] ->
	    {reply, Index, State, 10000};
	[] ->
	    Free = ets:lookup_element(Tid, free_index, 2),
	    ets:insert(Tid, 
		       [{free_index,Free+1},
			{Bin, Free},
			{Free,Bin}]),
	    State1 = dump({Tid,Bfile,N+1}),
	    {reply, Free, State1, 10000}
    end;
handle_call({index2bin,Index}, _From, {Tid,_File,_N}=State) ->
    Reply = case ets:lookup(Tid, Index) of
		[{_,Bin}] ->  Bin;
		[] -> []
	    end,
    {reply,  Reply, State, 10000};
handle_call(max_index, _From, {Tid,_File,_N}=State) ->
    Free = ets:lookup_element(Tid, free_index, 2),
    {reply, Free-1, State, 10000};
handle_call(stop, _From, State) ->
    dump(State),
    {stop, normal, stopped, void}.

handle_info(timeout, State) ->
    State1 = dump(State),
    {noreply, State1, 10000}. 

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Terminate:~p~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

dump({_,_,0}=S) -> S;
dump({Tid, File, N}) when N > 100 -> 
    io:format("dumping db~n"),
    ets:tab2file(Tid, File),

    {Tid,File,0};
dump(State) ->
    State.


-module(uuid_db).

-uuid("9a20a69e-f0d1-4b02-bd14-f4c104717da0").

-tags([k1,k2]).

-description("edit me").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, save_location/2, whereis_uuid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

%% save_location(UUid, File) is stored at File location

save_location(UUid, File) ->
    gen_server:call(?SERVER, {save_location,UUid,File}).

whereis_uuid(UUid) ->
    gen_server:call(?SERVER, {whereis,UUid}).

init([]) ->
    Home = os:getenv("HOME"),
    DbDir = Home ++ "/.erldbs/uuid",
    case filelib:is_dir(DbDir) of
	true ->
	    case bitcask:open(DbDir, [read_write]) of
		{error, E} ->
		    io:format("Cannot open DB=~s reaspon:~p~n",[DbDir, E]),
		    {stop, noDb1};
		Handle ->
		    io:format("uuid database opened~n"),
		    {ok, Handle}
	    end;
	false ->
	    io:format("The directory ~s does not exist please create it~n",[DbDir]),
	    {stop, noDb2}
    end.


handle_call({whereis_uuid, UUid}, _, Handle) ->
    B1 = list_to_binary(UUid),
    Key = <<$u,B1/binary>>,
    Files = case bitcask:get(Handle, Key) of
		{ok, B2} -> binary_to_term(B2);
		error    -> []
	    end,
    {reply, Files, Handle};
handle_call({save_location, UUid, File},_,Handle) ->
    %% The first thing to do is check that the file *really*
    %% is where we say
    %% it is and that it has an absolute name
    case File of
	"/" ++ _ ->
	    case filelib:is_regular(File) of
		true ->
		    %% the key = <<$u/UUid>>
		    %% value = list of files as a binary
		    %% but first we lookup the key
		    B1 = list_to_binary(UUid),
		    Key = <<$u,B1/binary>>,
		    case bitcask:get(Handle, Key) of
			{ok, B2} -> 
			    case binary_to_term(B2) of
				[] ->
				    Value = term_to_binary([File]),
				    ok = bitcask:put(Handle, Key, Value),
				    {reply, ok, Handle};
				Files ->
				    %% we might know this already ...
				    case lists:member(File, Files) of
					true ->
					    %% nothing to do
					    {reply, ok, Handle};
					false ->
					    Value = term_to_binary([File|Files]),
					    ok = bitcask:put(Handle, Key, Value),
					    {reply, ok, Handle}
				    end
			    end;
			not_found ->
			    Value = term_to_binary([File]),
			    ok = bitcask:put(Handle, Key, Value),
			    {reply, ok, Handle}
		    end;
		false ->
		    %% a non regular file might be a directory
		    {reply, {error,notregular}, Handle}
	    end;
	_ ->
	    %% The file name is not absolute
	    {reply, {error, not_absolute}, Handle}
    end;
handle_call(stop, _, Handle) ->
    bitcask:close(Handle),
    {stop, normal, stopped, void};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("terminate:~p~n",[{_Reason,_State}]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



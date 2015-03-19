-module(file_manager).

-uuid("6a9591a5-b4fe-4477-bc00-5b4d0c97d194").

-tags([k1,k2]).

-description("edit me").


-compile(export_all).

list_erlang_files() ->
    L = elib2_find:files(os:getenv("HOME") ++ "/Dropbox/experiments",
			 "*.erl", true),
    io:format("~w files found~n",[length(L)]),
    lists:foreach(fun(F) ->
			  Bin = unicode:characters_to_binary(F, unicode, utf8),
			  db_ets_pairs:bin2index(filename_db, Bin)
		  end, L).

forall() ->
    Max = db_ets_pairs:max_index(filename_db),
    for(1,Max,fun process/1).

for(N,N,F) -> F(N);
for(I,N,F) -> F(I), 
	      for(I+1,N,F).

process(I) ->
    F = db_ets_pairs:index2bin(filename_db, I),
    %%io:format("I:~p F=~p~n",[I, F]),
    W = elib1_indexer_plugin_erl:file(F),
    io:format("I:~p ~p~n",[I, length(W)]).


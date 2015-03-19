-module(full_text_dict).

-uuid("e041a799-f44f-47f3-a5d1-4aa552b8bd93").

-tags([full,text,fulltext,dictionary]).

-description("Full text search engine using a dictionary").


%% -compile(export_all).

-export([make_dictionary/0, make_dict1/0, search/1, search/2, read_dict/0]).

%% a full text search engine using a dictionary

make_dictionary() ->
    {T, Stats} = timer:tc(?MODULE, make_dict1, []),
    Files = maps:get(files, Stats),
    Seconds = T/1000000,
    Stats#{time => Seconds, files_per_second => trunc(Files/Seconds)}.

make_dict1() ->
    Dir = elib2_misc:glob_dir("erl_data"),
    D = dict:new(),
    {_, D1} = elib2_find:files(os:getenv("HOME") ++ "/Dropbox/experiments",
			       xmerl_regexp:sh_to_awk("*.erl"),
			       true,
			       fun add_to_dict/2,
			       {1,D}),
    io:format("~n"),
    BB = term_to_binary(D1),
    file:write_file(Dir ++ "/full_text.dict", BB),
    M = stats(D1),
    M#{size => size(BB)}.

add_to_dict(File, {N,D}) ->
    Bfilename = unicode:characters_to_binary(File, unicode, utf8),
    D1 = dict:store(N, Bfilename, D),
    Words = words_in_file(Bfilename),
    io:format("."),
    D2 = lists:foldl(fun(Word, Dict) ->
			     dict:append(Word, N, Dict)
		     end, 
		     D1, Words),
    {N+1,D2}.

words_in_file(F) ->
    [Word || {Word,_Freq} <- elib1_indexer_plugin_erl:file(F)].


%%----------------------------------------------------------------------

stats(D) ->
    dict:fold(fun stats/3, #{files => 0, filesizes=> 0, 
			     keysizes => 0, keywords => 0, refs=> 0}, D).

stats(Key, Filename, #{files := N, filesizes := S} = Map) when is_integer(Key) ->
    Map#{files := N+1, filesizes := S + size(Filename) };
stats(Key, L, #{keywords := N, refs := R, keysizes := K} = Map) when is_list(Key) ->
    Map#{keywords := N+1, refs := R + length(L), keysizes := K + length(Key)}.

%%----------------------------------------------------------------------
%% search assumes that the dictionary has been read into memory

search(Str) ->
    D = read_dict(),
    search(Str, D).

search(Str, Dict) ->
    Toks = string:tokens(Str, " "),
    Alternatives = [{I, best_keywords(I, Dict, 5)} || I <- Toks],
    Keywords = [hd(L) || {_,L} <- Alternatives],
    Results = [{I,lookup(I, Dict)} || I <- Keywords],
    Hits = [{I,length(L)} || {I,L} <- Results],
    Sets = [L || {_,L} <- Results],
    Final = trim(20, union(Sets)),
    Files = [lookup(I, Dict) || I <- Final],
    #{alternatives => Alternatives, chosen_keywords => Hits, files => Files}.

%% truncate the number of hits in case we get too many hits

trim(0, _)     -> [];
trim(N, [H|T]) -> [H|trim(N-1, T)]; 
trim(_, [])    -> []. 

union([A,B])   -> union(A,B); 
union([A,B|T]) -> union([union(A,B)|T]);
union([A])     -> A; 
union([])      -> [].

union(A, B) ->
    lists:filter(fun(I) ->
			 lists:member(I, B)
		 end, A).
			 
lookup(Word, Dict) ->
    case dict:find(Word, Dict) of
	{ok, L} -> L;
	error -> []
    end.

show(Tab, I) ->
    Loc = ets:lookup(Tab, I),
    io:format("~p~n",[Loc]).

%%----------------------------------------------------------------------

best(String) ->
    %% find the best ten keywords
    D = read_dict(),
    timer:tc(?MODULE, best, [String, D]).

%% Find the Max best keywords matching String

best_keywords(String, D, Max) ->
    A0= elib2_score:new(Max),  %% Max
    A1 = dict:fold(fun(Word, _, Acc) ->
			   compare(Word, String, Acc)
		   end, A0, D),
    Final = elib2_score:finalize(A1),
    [Key || {Key,_Score} <- Final].

compare(X, _, A) when is_integer(X) ->
    A;
compare(X, String, Acc) ->
    Sim = string_metrics:sim(X, String),
    elib2_score:add(X, Sim, Acc).

%%----------------------------------------------------------------------

file_to_term(F) ->
    {ok, B} = file:read_file(F),
    binary_to_term(B).

read_dict() ->
    Dir = elib2_misc:glob_dir("erl_data"),
    File = Dir ++ "/full_text.dict",
    case file:read_file(File) of
	{ok, Bin} ->
	    binary_to_term(Bin);
	{error, _} ->
	    exit({cannot_read,File})
    end.

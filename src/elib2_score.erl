-module(elib2_score).

%% oldname sherlock_best

%% Copyright (c) 2009 Whoomph Software AB (joearms@gmail.com). All rights reserved.
%% For license read the top level LICENSE file

%% used to keep track of the K best things in a collection

-export([new/1, add/3, finalize/1, test/0]).

%% new(N) -> Container                       %% make a container
%% add(Any, Score, Container) -> Container1  %% update the container
%% finalise(Container) -> [{Score,Any}]      %% the K best things in the 
%%                                           %% container

%% I should check in my book the correct way to type declare this

%% bug the same item can come twice


-type container() :: {container, Max::integer(), [{Term::any(), Score::integer()}]}.

-spec new(integer()) -> container().
-spec add(Term::any(), Score::integer(), C::container()) -> C1::container().
-spec finalize(container()) -> [{Term::any(), Score::integer()}].

test() ->
    C0 = new(3),
    C1 = add(foo,2,C0),
    C2 = add(foo,3,C1),
    [{foo,3}] = finalize(C2),
    yes.

%% we retain the N items with the best scores
%% Best is a numerical measure Bigger = Better

new(N) -> {container, N, []}.

add(Term, Score, {container,N,L}) ->
    L1 = inject(Term, Score, L),
    L2 = trim(N, L1),
    {container, N, L2}.

inject(Term, Score, [{X,S}|T]) when S > Score -> [{X,S}|inject(Term, Score, T)];
inject(Term, Score, T)                        -> 
    [{Term,Score}|remove_term(Term, T)].

remove_term(H,    [{H,_}|T])    -> T;
remove_term(Term, [H|T]) -> [H|remove_term(Term, T)];
remove_term(_,    [])       -> []. 

trim(0, _)     -> [];
trim(_, [])    -> [];
trim(N, [H|T]) -> [H|trim(N-1,T)].
    
finalize({container,_,L}) -> L.


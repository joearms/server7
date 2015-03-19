-module(factory).

-uuid("70330895-ae57-4a08-817d-512b158c6c9d").

-tags([factory,parallelize]).

-description("parallelise a sequnce of requests to evaluate a function").

-compile(export_all).

start(K, Fun) ->
    S = self(),
    Pid = spawn(fun() -> start(S, Fun, K) end),
    receive
	{Pid, ack} ->
	    Pid
    end.

start(Parent, Fun, K) ->
    a.

		  

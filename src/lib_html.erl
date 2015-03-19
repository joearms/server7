-module(lib_html).

-compile(export_all).

pre(X) ->
    ["<pre>",
     quote_lt(lists:flatten(io_lib:format("~p~n",[X]))),
     "</pre>"].
    
quote_lt([$<|T]) -> "&lt;" ++ quote_lt(T);
quote_lt([H|T])  -> [H|quote_lt(T)];
quote_lt([])     -> [].

quote([$<|T]) -> "&lt;" ++ quote(T);
quote([$&|T]) -> "&amp;" ++ quote(T);
quote([H|T])  -> [H|quote(T)];
quote([])     -> [].


-module(add_uuids).

-uuid("2c6dfedd-44fb-43ce-8da2-ead088fcec1e").

-compile(export_all).

-description("add uuids to erlang and html files").

-tags([uuid,erlang]).

uuid() ->
    uuid:to_string(uuid:uuid4()).

test() ->
    E = filelib:wildcard("*.erl"),
    [add_to_erl(I) || I<- E],
    H = filelib:wildcard("*.html"),
    [add_to_html(I) || I <- H],
    ok.

%%----------------------------------------------------------------------
add_to_erl(F) ->
    {ok, B} = file:read_file(F),
    add_to_erl(B, F).

add_to_erl(<<"%% uuid:",_/binary>>, _) ->
    void;
add_to_erl(Bin, F) ->
    io:format("adding uuid to:~p~n",[F]),
    file:write_file(F,["%% uuid:", uuid(), "\n", Bin]).

%%----------------------------------------------------------------------

add_to_html(F) ->
    {ok, B} = file:read_file(F),
    add_to_html(B, F).

add_to_html(<<"<!-- uuid:",_/binary>>, _) ->
    void;
add_to_html(Bin, F) ->
    io:format("adding uuid to:~p~n",[F]),
    file:write_file(F,["<!-- uuid:", uuid(), " -->\n", Bin]).

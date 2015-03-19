-module(erl_analyse_metadata).

-uuid("d71960b0-6a3b-4314-bcca-3e0a9aaa503f").

-tags([k1,k2]).

-description("edit me").

-compile(export_all).


check() ->
    {ok, Cwd} = file:get_cwd(),
    check(Cwd),
    true.

check(Cwd) ->
    F = [Cwd ++ "/" ++ I || I <- filelib:wildcard("*.erl")],
    [{I, check_meta(I)}  || I <- F].

check_meta(F) ->
    case elib2_erl_metadata:extract_meta(F) of
	{ok, #{uuid := U, tags := Tags, description := Desc}} ->
	    case Tags of
		[k1,k2] ->
		    io:format("*** ~s tags need editing~n",[filename:rootname(F)]);
		_ ->
		    void
	    end,
	    case Desc of
		"edit me" ->
		    io:format("*** ~s description needs editing~n",[filename:rootname(F)]);
		_ ->
		    void
	    end;
	{ok, _Map} ->
	    io:format("*** ~s is missing some metadata~n"
		      " please run add_meta_to_mods() and re-run~n",
		      [F]);
	{error, _} ->
	    io:format("*** ~s is unreadable~n", [F])
    end.


report(Dir) ->
    to_html(check(Dir)).

to_html(L) ->
    io:format("L=~p~n",[L]),
    ["<table>",
     [["<tr>",format_item(I),"</tr>\n"] || I <- L],
     "</table>"].

format_item({File,Value}) ->
    [td(File), format_value(Value)].

format_value(#{error := E}) ->
    td(red("**error**" ++ E));
format_value(#{tags := Tags, description := D, uuid := U}) ->
    [td(format_tags(Tags)), td(red(D)), td(U)].

format_tags(L) ->
    [[atom_to_list(I)," "] || I <- L].

td(X) ->
    ["<td>",X,"</td>"].

red(X) ->
    ["<font color='red'>",X,"</font>"].

pre(X) ->
    iolist_to_binary(io_lib:format("<pre>~p</pre>~n",[X])).

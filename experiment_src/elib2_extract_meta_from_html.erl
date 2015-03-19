-module(elib2_extract_meta_from_html).

-uuid("786804a9-425d-4fa2-a06b-7527bf4be2bc").

-tags([k1,k2]).



-export([test/0, file2meta/1, extract_meta/1]).

-keywords([meta_data,extract,html]).
-description("extract meta data from the head part of an HTML file").

%% extract meta data from the head part of an html file.
%% the file should begin something like

%% <head>
%%   <meta charset="UTF-8"/>
%%   <meta name="description" content="A long description"/>
%%   <meta name="keywords" content="HTML,CSS,XML,JavaScript"/>
%%   <meta name="author" content="joe armstrong"/>
%% </head>

test() ->
    file2meta("test_sax.html").

extract_meta(F) ->
    M = file2meta(F),
    case has_required_metadata(M) of
	true  -> {yes,M};
	false -> no
    end.

has_required_metadata(#{keywords := K, description := D}) when K =/= [], D =/= [] ->
    true;
has_required_metadata(_) ->
    false.

file2meta(F) ->
    io:format("processing:~p~n",[F]),
    case file:read_file(F) of
	{ok, B} ->
	    L = process(B),
	    maps:from_list(L);
	{error, _} ->
	    %% help debug this
	    exit({eNoFile,?MODULE,file2meta, F})
    end.

process(Bin) ->
    try trane:sax(Bin, fun collect/2, []) of
	L ->
	    L
    catch
	throw: {done,L1} ->
	    L1
    end.

collect({end_tag,"head"}, L) -> throw({done,L});
collect({tag,"meta",M}, L)   -> reduce(M) ++ L;
collect(_, L)                -> L.

reduce([{"name",N},{"content",C}]) -> [{list_to_atom(N),C}];
reduce([{X,Y}|T])                  -> [{list_to_atom(X),Y}|reduce(T)];
reduce([])                         -> [].
    




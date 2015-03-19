-module(brain).

-uuid("bb6e4b37-0457-47ef-a8ff-57b29cad0511").

-tags([brain,emulation,data_base]).

-description("this is an attempt to simpulate the mechansims in the brain").


-compile(export_all).

-import(lists, [reverse/1]).

%% extract meta data

test() ->
    elib2_extract_meta_from_html:file2meta("page.html").

test1() ->
    elib2_extract_meta_from_erl:file2meta("elib2_extract_meta_from_html.erl").


test3() ->
    L1 = elib2_find:files(".", "*.erl", true),
    [check_erl(I) || I <- L1].

test4() ->
    L1 = elib2_find:files(".", "*.html", true),
    [check_html(I) || I <- L1].

check_erl(F) ->
    case elib2_extract_meta_from_erl:file2meta(F) of
	{error, Why} ->
	    io:format("*** ~s => ~p~n",[F,Why]);
	{ok, #{desc := D}} ->
	    io:format("~s => ~s~n",[F, D])
    end.

check_html(F) ->
    case elib2_extract_meta_from_html:extract_meta(F) of
	no ->
	    case has_parent_dir_meta(F) of
		{yes, MetaP} ->
		    io:format("~s => ~p~n",[F, MetaP]);
		no ->
		    io:format("*** ~s has no metadata~n",[F])
	    end;
	{yes,D} ->
	    io:format("~s => ~p~n",[F, D])
    end.

test7() ->
    {ok, B} = file:read_file("page.html"),
    lists:reverse(trane:sax(B,fun(A,L) -> [A|L] end, [])).

			
has_parent_dir_meta(F) ->
    Dir = filename:dirname(F),
    case file:read_file(File = Dir ++ "/meta") of
	{ok, B} ->
	    {yes, parse_text_meta(File, binary_to_list(B))};
	{error, _} ->
	    no
    end.

parse_text_meta(File, L) ->
    Lines = get_lines(L),
    L1 = [prep(File, I) || I <- Lines],
    maps:from_list(L1).

prep(File, L) ->
    case string:tokens(L, ":") of
	["keywords",Y] -> {keywords,make_keywords(Y)};
	["description",D] -> {description,D};
	X -> exit({invalid,item,X,in,file,File})
    end.

make_keywords(L) ->
    L1 = string:tokens(L, ", "),
    [list_to_atom(I) || I <- L1].


get_lines([]) ->
    [];
get_lines(L) ->
    {Line, Str1} = get_line(L, []),
    [Line|get_lines(Str1)].

get_line("\n " ++ T, L) -> get_line(T, [$\s|L]);
get_line("\n" ++ T, L)  -> {reverse(L), T};
get_line([H|T], L)      -> get_line(T, [H|L]);
get_line([], L)         -> {reverse(L), []}.
    


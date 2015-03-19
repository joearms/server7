-module(md_inlines).

-uuid("2c09154e-af6f-46d7-af51-b0806596efcc").

-tags([k1,k2]).

-description("edit me").


-export([parse/1]).

%% -compile(export_all).

-import(lists, [reverse/1]).

parse(Str) ->
    case (catch parse0(Str)) of
	{'EXIT', Why} ->
	    trace({'FIXME**',Str,Why}),
	    "*** Error ***";
	Ok ->
	    Ok
    end.

parse0(Str) ->
    %% io:format("~p:Parse:~p~n",[?MODULE,Str]),
    Str1 = parse_special(Str),
    p(Str1).

parse_special("^\n" ++ T) -> [{enode,br}|parse_special(T)];
parse_special("``" ++ T)  -> [md_entities:entity("ldquo")|parse_special(T)];
parse_special("''" ++ T)  -> [md_entities:entity("rdquo")|parse_special(T)];
parse_special([H|T])      -> [H|parse_special(T)];
parse_special([])         -> [].

p("<<<" ++ T) -> get_inline_node(T, []); 
p([$\\,H|T])  -> [H|p(T)];  %% remove the escape here
p("[" ++ T)  -> get_link(T, link, []);
p("![" ++ T) -> get_link(T, image, []);
p("`" ++ T)  ->	p2(T, "`",  code);
p("~~" ++ T) -> p1(T, "~~", strike);
p("__" ++ T) -> p1(T, "__", bold);
p("**" ++ T) -> p1(T, "**", {span,yellow});
p("_" ++ T)  ->	p1(T, "_",  em);

p([H|T])     -> [H|p(T)];	
p([])        -> []. 

get_inline_node(">>>" ++ T, L) ->
    Parse = string2term(lists:reverse(L)),
    [Parse|p(T)];
get_inline_node([H|T], L) ->
    get_inline_node(T, [H|L]);
get_inline_node([], _) ->
    exit({eof,inlineNode}).
    
string2term(Str) ->
    {ok,Tokens,_} = erl_scan:string(Str ++ "."),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

make_node(link, Href, Title) ->
    {node,a, #{href=>Href}, Title};
make_node(image, Href, Title) ->
    {node, img, #{src=> Href, alt => Title}, []}.


get_link("](" ++ T, Tag, L) ->
    Title = reverse(L),
    {Href, T1} = get_href(T, []),
    [make_node(Tag, Href, Title)|p(T1)];
get_link([H|T], Tag, L) ->
    get_link(T, Tag, [H|L]);
get_link([], _, L) ->
    io:format("bad link:~p~n",[reverse(L)]),
    p(tl(reverse(L))).

get_href(")" ++ T, L) ->
    {reverse(L), T};
get_href([H|T], L) ->
    get_href(T, [H|L]);
get_href([], L) ->
    io:format("bad href:~p~n",[reverse(L)]),
    exit(link).

% p2 does not recursivly scan the contents

p2(Str, StartStop, Tag) ->	 
    p2(Str, StartStop, StartStop, Tag).

p2(T, Start, Stop, Tag) ->
    case find_stop(T, Stop, []) of
	no                   -> Start ++ p(T);
	{yes, Before, After} -> [{node,Tag,#{},Before}|p(After)]
    end.

p1(Str, StartStop, Tag) ->	 
    p1(Str, StartStop, StartStop, Tag).

p1(T, Start, Stop, Tag) ->
    case find_stop(T, Stop, []) of
	no                   -> Start ++ p(T);
	{yes, Before, After} -> [make_node(Tag,p(Before))|p(After)]
    end.

make_node({Tag,Class},Children) when is_atom(Tag) ->
    {node,Tag,#{class=>Class},Children};
make_node(Tag,Children) when is_atom(Tag) ->
    {node,Tag,#{}, Children}.

find_stop([], _, _) ->
    no;
find_stop(Str, Prefix, L) ->
    case is_prefix(Prefix, Str) of
	{yes, Rest} -> {yes, reverse(L), Rest};
	no          -> find_stop(tl(Str), Prefix, [hd(Str)|L])
    end.

is_prefix([H|T1], [H|T2]) -> is_prefix(T1, T2);
is_prefix([], L)          -> {yes, L};
is_prefix(_, _)           -> no.

trace(X) ->
    io:format("~p:~p ~p~n",[?MODULE,?LINE,X]).

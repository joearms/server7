-module(esdoc_xml).

-uuid("3b5de15b-27a9-46ad-acf1-e02d4ca909d7").

-tags([k1,k2]).

-description("edit me").


-import(lists, [reverse/1]).

-export([file_to_tree/1, 
	 tree_to_iol/1,
	 render_tree_to_file/2,
	 must_be_a_tree/1,
	 string_to_tree/1]).

%% tree
%%   {Node,Tag,#{},[Childen]
%%   Int

%% file_to_tree(File) -> Tree
%% string_to_tree(Str) -> Tree
%% tree_to_binary(Tree) -> Bin
%% must_be_a_tree(Tree) -> void | EXIT
%% render_tree_to_file(Tree, File)

file_to_tree(File) ->
    {ok, Bin} =  file:read_file(File),
    string_to_tree(Bin).

string_to_tree(Bin) ->
    Sax = binary2sax(Bin),
    Sax1 = simplify(Sax),
    parse_toks(Sax1).

tree_to_binary(Tree) ->
    tree_to_iol(Tree).

binary2sax(Bin) ->
    Collect = fun(X, _Loc, L) -> [X|L] end,
    case
	xmerl_sax_parser:stream(Bin,
				[skip_external_dtd,
				 {event_fun,Collect},
				 {event_state,[]}]) 
	
    of
	{ok, S, <<>>} -> 
	    lists:reverse(S);
	{fatal_error, Location, Reason, EndTags,_} ->
	    io:format("Error:~p~nLocation=~p~nEndtags=~p~n",
		      [Reason,Location,EndTags]),
	    exit(ebadXML)
    end.


render_tree_to_file(Tree, File) ->
    IOL = tree_to_iol(Tree),
    file:write_file(File, IOL).

%% simplify removes junk from the sax parse that 
%% I'm not interested in

simplify(L) ->
    simplify(L, []).

simplify([{startDTD,_,_,_}=H|T], L) ->
    T1 = skip_dtd(T),
    simplify(T1, [H|L]);
simplify([{attributeDecl,_,_,_,_,_}|T], L) ->
    simplify(T, L);
simplify([{startElement,_,Tag,_,A}|T], L) ->
    simplify(T, [{start,list_to_atom(Tag),reduce_args(A)}|L]);
simplify([{characters,Str}|T], L) ->
    simplify(T, lists:reverse(Str,L));
simplify([{endPrefixMapping,_}|T], L) ->
    simplify(T, L);
simplify([{startPrefixMapping,_,_}|T], L) ->
    simplify(T, L);
simplify([{comment,_}=H|T], L) ->
    simplify(T, [H|L]);
simplify([{ignorableWhitespace,S}|T], L) ->
    simplify(T, [{ws,S}|L]);
simplify([{endElement,_,Tag,_}|T], L) ->
    simplify(T, [{stop,list_to_atom(Tag)}|L]);
simplify([H|T], L) when H =:= startDocument; H =:= endDocument;
			H =:= startCDATA; H =:= endCDATA ->
    simplify(T, [H|L]);
simplify([{startPrefixMapping,"xi",_}|T], L) ->
    %% drop this
    simplify(T, L);
simplify([{endPrefixMapping,"xi"}|T], L) ->
    %% drop this
    simplify(T, L);

simplify([], L) ->
    lists:reverse(L).


skip_dtd([endDTD|T]) -> T;
skip_dtd([_|T]) -> skip_dtd(T).

reduce_args(L) ->
    L1 = [reduce_arg(I) || I <- L],
    maps:from_list(L1).

reduce_arg({_,_,Tag,Val}) -> 
    {list_to_atom(Tag),Val};
reduce_arg(X) ->
    io:format("reduce_arg:~p~n",[X]),
    exit(oops).

escape(B) when is_binary(B) -> escape(binary_to_list(B));
escape([$<|T])              -> "&lt;" ++ escape(T);
escape([H|T])               ->  [H|escape(T)];
escape([])                  ->  [].

tree_to_iol({enode,Tag}) ->
    empty_node(Tag,[]);
tree_to_iol({node,Tag,Args,[]}) ->
    %% [empty_node(Tag, Args)];
    [start_node(Tag, Args),end_node(Tag)];
tree_to_iol({node,Tag,Args,Children}) ->
    [start_node(Tag,Args),
     [tree_to_iol(I) || I <- Children],
     end_node(Tag)];
tree_to_iol([{node,Tag,Args,Children}|T]) ->	%%eraad
    [start_node(Tag,Args),
     [tree_to_iol(I) || I <- Children],
     end_node(Tag)
     ++ tree_to_iol(T)
    ];
tree_to_iol([]) -> [];
tree_to_iol({cdata,Str}) ->
    ["<![CDATA[",
     unicode:characters_to_binary(Str, unicode, utf8),
     "]]>"];
tree_to_iol({comment,Str}) ->
    ["<!--",
     unicode:characters_to_binary(Str, unicode, utf8),
     "-->"];
tree_to_iol({processingInstruction,Tag,Str}) ->
    ["<?",Tag," ",
     unicode:characters_to_binary(Str, unicode, utf8),
     "?>"];
tree_to_iol(N) when is_integer(N) ->
    unicode:characters_to_binary([N], unicode, utf8);
tree_to_iol({ws, S}) ->
    unicode:characters_to_binary(S, unicode, utf8);
tree_to_iol(Wot) ->
    io:format("Tree to IOL: bad node~p~n",[Wot]),
    exit({eBadNode,Wot}).

start_node(Tag, Args) when is_map(Args) ->
    start_node(Tag, maps:to_list(Args));
start_node(Tag, Args) ->
    ["<", to_str(Tag), [format_args(I) || I <- Args],">\n"].

empty_node(Tag, Args) when is_map(Args) ->
    empty_node(Tag, maps:to_list(Args));
empty_node(Tag, Args) ->
    ["<", to_str(Tag), [format_args(I) || I <- Args],"/>\n"].

format_args({Key,Val}) -> [" ",to_str(Key),"=\"",to_str(Val),"\""].

end_node(Tag) ->     
%% eraad    ["</",Tag,">"].
    ["</",to_str(Tag),">\n"].


to_str(A) when is_atom(A) ->
    atom_to_list(A);
to_str(N) when is_integer(N) ->
    integer_to_list(N);
to_str(X) ->
    X.

parse_toks([startDocument, {startDTD,_,_,_}=DocType|T]) ->
    parse_toks(DocType, T);
parse_toks([startDocument|T]) ->
    parse_toks(none, T).

parse_toks(DocType, Toks) ->
    {Tree, Rest} = to_tree(Toks),
    case ok_ending(Rest) of
	true  -> {DocType, Tree};
	false -> exit({badStuff,following_parse,Rest})
    end.

to_tree(L) ->
    to_tree1(L).

to_tree1([{comment,_}|T1]) ->
    to_tree1(T1);
to_tree1([{start,Tag,Attrs}|T1]) ->
    {Children,T2} = collect_until(Tag, T1, []),
    {{node,Tag,Attrs,Children}, T2}.

collect_until(Tag, [{stop,Tag}|T], L) ->
    {reverse(L), T};
collect_until(Tag, [{start,_,_}|_]=X, L) ->
    {Node, T1} = to_tree1(X),
    collect_until(Tag, T1, [Node|L]);
collect_until(Tag, [{comment, _X}=H|T], L) ->
    collect_until(Tag, T, [H|L]);
collect_until(Tag, [{ws, _}|T], L) ->
    collect_until(Tag, T, L);
collect_until(Tag, [startCDATA,{str,S},endCDATA|T], L) ->
    collect_until(Tag, T, [{cdata,S}|L]);

collect_until(Tag, [H|T], L) ->
    collect_until(Tag, T, [H|L]).

ok_ending([]) -> true;
ok_ending([{ws,_}|T]) -> ok_ending(T);
ok_ending([endDocument|T]) ->  ok_ending(T).

must_be_a_tree({enode,Tag}) ->
    true;
must_be_a_tree({node,N,Args,Children}) when is_list(Children) ->
    is_node_name(N),
    is_args(Args),
    [must_be_a_tree(I) || I <- Children];
must_be_a_tree(X) when is_integer(X) ->
    true;
must_be_a_tree(X) ->
    io:format("Not a tree:~p~n",[X]),
    exit(eNoTree).

is_node_name(X) when is_atom(X) -> true;
is_node_name(X) when is_list(X) -> true;
is_node_name(X) ->
    io:format("Bad nodename in tree:~p~n",[X]),
    exit(eNoTree).

is_args(X) when is_map(X) ->
    %% could do better here
    %% and check the keys and values
    true.

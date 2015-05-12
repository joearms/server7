-module(erdoc2_lib).

%% -include("cxpxml.hrl").

%% Change log:
%%    Made XML parse return a map and not a list

-export([
	 binary_to_simple_sax/1,
	 cdata_node/1,
	 end_node/1,
	 file_to_simple_sax/1,
	 file_to_xml/1,
	 locate_src/1,
	 make_ranid/1,
	 output_sax/2,
	 parse_toks/1,
	 sax_to_binary/1,
	 start_node/2,
	 str_node/1,
	 str_to_xml/1,
	 tree_to_iol/1,
	 to_tree/1,
	 xml_to_file/3,
	 validate_or_crash/1,
	 ws_node/1,
	 recover_meta/1,
	 add_cids/1,
	 store_meta/3,
	 update_paras/3
	]).

-import(lists,  [reverse/1]).

binary_to_simple_sax(Bin) ->
    Sax = binary2sax(Bin),
    %% elib1_misc:dump("test1.tmp", Sax),
    S1 = simplify(Sax),
    %% elib1_misc:dump("simple_sax.tmp", S1),
    S1.

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

file_to_xml(File) ->
    S = file_to_simple_sax(File),
    %% elib1_misc:dump("debug1.tmp",S),
    parse_toks(S).

str_to_xml(Str) ->
    Sax = str_to_simple_sax(Str),
    %% elib1_misc:dump("debug1.tmp",S),
    parse_toks(Sax).


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

ok_ending([]) -> true;
ok_ending([{ws,_}|T]) -> ok_ending(T);
ok_ending([endDocument|T]) ->  ok_ending(T).

str_to_simple_sax(Str) ->
    Sax = str2sax(Str),
    simplify(Sax).

str2sax(Str) ->
    Collect = fun(X, _Loc, L) -> [X|L] end,
    case
	xmerl_sax_parser:stream(Str,
			      [skip_external_dtd,
			       {event_fun,Collect},
			       {event_state,[]}]) 

    of
	{ok, S, []} -> 
	    lists:reverse(S);
	{fatal_error, Location, Reason, EndTags,_} ->
	    io:format("Error:~p~nLocation=~p~nEndtags=~p~n",
		      [Reason,Location,EndTags]),
	    exit(ebadXML)
    end.



file_to_simple_sax(File) ->
    Sax = file2sax(File),
    %% elib1_misc:dump("test1.tmp", Sax),
    S1 = simplify(Sax),
    %% elib1_misc:dump("simple_sax.tmp", S1),
    S1.

file2sax(File) ->
    Collect = fun(X, _Loc, L) -> [X|L] end,
    case
	xmerl_sax_parser:file(File,
			      [skip_external_dtd,
			       {event_fun,Collect},
			       {event_state,[]}]) 

    of
	{ok, S, <<>>} -> 
	    lists:reverse(S);
	{fatal_error, Location, Reason, EndTags,_} ->
	    io:format("Error:~p~nLocation=~p~nEndtags=~p~n",
		      [Reason,Location,EndTags]),
	    exit({ebadXML,File})
    end.

%% simple sax removes junk I'm not interested in

simplify(L) ->
    simplify(L, []).

simplify([{startDTD,_,_,_}=H|T], L) ->
    T1 = skip_dtd(T),
    simplify(T1, [H|L]);
simplify([{attributeDecl,_,_,_,_,_}|T], L) ->
    simplify(T, L);
simplify([{startElement,_,Tag,_,A}|T], L) ->
    simplify(T, [{start,Tag,reduce_args(A)}|L]);
simplify([{characters,Str}|T], L) ->
    simplify(T, [{str,Str}|L]);
simplify([{endPrefixMapping,_}|T], L) ->
    simplify(T, L);
simplify([{startPrefixMapping,_,_}|T], L) ->
    simplify(T, L);
simplify([{comment,_}=H|T], L) ->
    simplify(T, [H|L]);
simplify([{ignorableWhitespace,S}|T], L) ->
    simplify(T, [{ws,S}|L]);
simplify([{endElement,_,Tag,_}|T], L) ->
    simplify(T, [{stop,Tag}|L]);
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
    maps:from_list([reduce_arg(I) || I <- L]).

reduce_arg({_,_,Tag,Val}) -> 
    {list_to_atom(Tag),Val};
reduce_arg(X) ->
    io:format("reduce_arg:~p~n",[X]),
    exit(oops).


output_sax(File, Sax) ->
    IO= sax_to_iol(Sax, []),
    elib1_misc:check_io_list(IO),
    ok = file:write_file(File, IO),
    %% io:format("Written:~p~n",[File]).
    ok.

sax_to_binary(Sax) ->
    IO = sax_to_iol(Sax, []),
    elib1_misc:check_io_list(IO),
    list_to_binary(IO).

sax_to_iol([{comment,S}|T], L) ->
    sax_to_iol(T, [["<!--",S,"-->"]|L]);
sax_to_iol([startCDATA,{str,S},endCDATA|T], L) ->
    sax_to_iol(T, [["<![CDATA[",S,"]]>"]|L]);
sax_to_iol([startDocument|T], L) ->
    sax_to_iol(T, ["<?xml version=\"1.0\"?>\n"|L]);
sax_to_iol([endDocument], L) ->
    reverse(L);
sax_to_iol([{startDTD,Tag,[],File1}|T], L) ->
    sax_to_iol(T, [["<!DOCTYPE ",Tag," SYSTEM \"",File1,"\">\n"]|L]);
sax_to_iol([{start,Tag,Args}|T], L) -> 
    sax_to_iol(T, [start_node(Tag,Args)|L]);
sax_to_iol([{stop,Tag}|T], L) ->  
    sax_to_iol(T, [end_node(Tag)|L]);
sax_to_iol([{processingInstruction,Tag,Str}|T], L) ->
    sax_to_iol(T, [["<?",Tag," ",
		    unicode:characters_to_binary(Str, unicode, utf8),
		    "?>"]|L]);
sax_to_iol([{str,_S}=H|T], L) -> sax_to_iol(T, [str_node(H)|L]);
sax_to_iol([{ws, _S}=H|T], L) -> sax_to_iol(T, [ws_node(H)|L]);
sax_to_iol([], L)            -> reverse(L).

cdata_node({cdata,S}) ->
    unicode:characters_to_binary(escape(S), unicode, utf8).

str_node({str,S}) ->
    unicode:characters_to_binary(escape(S), unicode, utf8).

ws_node({ws,S}) ->
    unicode:characters_to_binary(S, unicode, utf8).


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

add_cids(File) ->
    L0 = erdoc_lib:file_to_simple_sax(File),
    elib1_misc:dump("sax.tmp", L0),
    Free = recover_meta(L0),
    case update_paras(L0, Free, []) of
	{Free, _} ->
	    void;
	{_Free1, L1} ->
	    L2 = store_meta(L1, Free, []),
	    file:rename(File, File ++ ".bak"),
	    erdoc_lib:output_sax(File, L2)
    end.

	    
recover_meta([{start,"meta",[{"max",Max}]}|_]) ->
    _Free = list_to_integer(Max)-1;
recover_meta([_|T]) ->
    recover_meta(T).

store_meta([{start,"meta",_}|T], Free, L) ->
    lists:reverse(L, [{start,"meta",[{"max",integer_to_list(Free-1)}]}|T]);
store_meta([H|T], Free, L) ->
    store_meta(T, Free, [H|L]).

update_paras([{start,"p",p,[{"c",_}]}=H|T], Free, _L) ->
    update_paras(T, Free, [H|T]);
update_paras([{start,"p",[]}|T], Free, L) ->
    update_paras(T, Free+1, [{start,"p",[{"c",integer_to_list(Free)}]}|L]);
update_paras([H|T], Free, L) ->
    update_paras(T, Free, [H|L]);
update_paras([], Free, L) ->
    {Free, lists:reverse(L)}.


escape(B) when is_binary(B) -> escape(binary_to_list(B));
escape([$<|T])              -> "&lt;" ++ escape(T);
escape([H|T])               ->  [H|escape(T)];
escape([])                  ->  [].


tree_to_iol({node,Tag,Args,[]}) ->
    [empty_node(Tag, Args)];
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
tree_to_iol({leaf,S}) ->
    unicode:characters_to_binary(escape(S), unicode, utf8);
tree_to_iol({str,S}) ->
    unicode:characters_to_binary(escape(S), unicode, utf8);
tree_to_iol({ws, S}) ->
    unicode:characters_to_binary(S, unicode, utf8);
tree_to_iol(Wot) ->
    io:format("Tree to IOL: bad node~p~n",[Wot]),
    exit({eBadNode,Wot}).


%%----------------------------------------------------------------------
%% tree_to_file(Tree)

xml_to_file(File, DTD, Tree) ->
%%	io:format("~p~n",[Tree]),
    Prolog = ["<?xml version=\"1.0\"?>\n",
	      format_dtd(DTD)],
    IO = tree_to_iol(Tree),
    elib1_misc:check_io_list(IO),
    ok = file:write_file(File, [Prolog,IO,"\n"]),
    io:format("Written:~p~n",[File]).


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

format_dtd({startDTD,Tag,[],File1}) ->
    ["<!DOCTYPE ",Tag," SYSTEM \"",File1,"\">\n"];
format_dtd(none) ->
    [].


%%----------------------------------------------------------------------

locate_src(Mod) ->
    io:format("locate_src for module:~p~n",[Mod]).


%%--------------------------------------
%% return a K character printable string
%% where the first letter   is  in a..zA..Z
%% and all other characters are in 0..9a..zA..Z

-spec make_ranid(K::integer()) -> string().

make_ranid(K) when K > 1 ->
    %% the first digit is 1..52 which is mapped onto a..z A..Z
    F = asciran(1,52),
    L = times(K-1, fun() -> asciran(1,62) end), 
    [F|L].

times(0, _) -> [];
times(N, F) -> [F()|times(N-1, F)]. 
    
asciran(Min,Max) ->  to_asci(ran(Min, Max)).

ran(Min, Max) -> crypto:rand_uniform(Min, Max+1).

to_asci(X) when  1 =< X, X =< 26  -> $a+X-1;
to_asci(X) when 27 =< X, X =< 52  -> $A+X-27;
to_asci(X) when 53 =< X, X =< 62  -> $0+X-53.

%%----------------------------------------------------------------------

validate_or_crash(File) ->
    io:format("Validating:~s~n",[File]),
    Where = code:which(?MODULE),
    %% io:format("Where=~p~n",[Where]),
    Val = filename:dirname(filename:dirname(Where)) ++ "/priv/validate ",
    _Ret = os:cmd(Val ++ File),
    %% io:format("Ret=~p~n",[Ret]),
    {ok, Bin} = file:read_file("./errors"),
    case Bin of
	<<>> ->
	    io:format("Well done. Validation succeeded for:~s~n~n", [File]),
	    ok;
	_ ->
	    %% write the errors to a file
	    Base = filename:basename(File),
	    Errs = Base ++ ".errs",
	    file:write_file(Errs, Bin),
	    io:format("Botheration. Validation failed see:~s I give up~n",[Errs]),
	    exit(eValidate)
    end.


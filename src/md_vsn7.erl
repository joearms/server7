-module(md_vsn7).

-uuid("8d216cfd-b220-45a0-81e4-82daabdfe498").

-tags([markdown]).

-description("Yet another attempt at a markdown processor").



-compile(export_all).

-import(lists, [reverse/1, reverse/2]).

-export([parse_string/1, trim/1, test/0]).

%% file_to_tree(File) ->
%%     P = parse_file(File),
%%     Tree = {node,content,#{},P},
%%     %% elib2_misc:dump("debug.tmp", Tree),
%%     Tree.

bug0() -> parse_string("x\n\n\n").

bug1() -> parse_string("x''\n\n\n").

parse_string(Str) ->
    case (catch parse_string0(Str)) of
	{'EXIT', Why} ->
	    trace({cannot_parse,Str}),
	    exit(bad);
	Other ->
	    Other
    end.

parse_string0(Str) ->
    Str1  = expand_tabs(Str),
    Str2  = md_entities:expand_entities_in_string(Str1),
    Lines = string_to_lines(Str2),
    trace({lines,Lines}),
    parse_lines(Lines).

parse_lines(Lines) ->
    Blocks = lines_to_blocks(Lines, []),
    post_process(Blocks).

%% Blocks with a recursive structue
%% [pre]
%%   TTTT
%%
%% the block offset is taken from the forst line following the [pre] tag

%% One line headers
%% Start       Meaning      Termination
%% # h1 ...\n  header       EOL
%% |----\n   => hr        =>    EOL
%% |    TTTT => pre       => first non-blank line with characters in 1..4
%% |- TTTTT => li         => first non-blank line with 1..2
%% |* TTTTT => li       
%% |+ TTTTT => li
%% |> TTTTT => pre       => first line not starting >

%% Two line headers
%%
%% HHHH
%% ====\n 4 or more (h1) 
%%    WARNING if nonblank characters found in the remainer of the line
%%
%% HHHH    
%% ----\n 4 or more (h2)
%%    WARNING if nonblank characters found in the remainer of the line

%% TTTT    Display list (non-blank in col 1) (blank in col1 of line 2)
%%  BBBB  <1 blank terminted by first non-blanks in cols 1..K 
%%         (Rec)

%% [Class] Div with class (non-blank in col 1) (blank in col1 of line 2)
%%  BBBB  <1 blank terminted by first non-blanks in cols 1..K 
%%         (Rec)

%% Regions (fenced block)
%% ```tag
%% ```

%% [Tag]
%%  BBB tagged region (Rec) termnation rule

lines_to_blocks([empty|T], L) -> lines_to_blocks(T, L);
lines_to_blocks([], L)        -> reverse(L);

%% from here on we should be able to decide how much to collect
%% first line alone

lines_to_blocks(["```" ++ X|T], L) ->
    {Fenced, T1} = collect_fenced_block(trim(X), T),
    lines_to_blocks(T1, [Fenced|L]);
lines_to_blocks(["----" ++ H|T], L) ->
    %% four dashes is a hr
    warn(H, $-), %% warning if any characters other than
             %% Blank/-/\n
    lines_to_blocks(T,[{node,hr,#{},[]}|L]);
lines_to_blocks(["---" ++ _|T], L) ->
    %% three dashes is a parameter block
    {Content, T1} = collect_param_block(T),
    Parse = [toks(I) || I <- Content],
    lines_to_blocks(T1, [{node,params,maps:from_list(Parse), []}|L]);
lines_to_blocks(["#" ++ H|T], L) ->
    %% one of more hashes is a header1 2 3
    Header = parse_header(H, 1),
    lines_to_blocks(T, [Header|L]);
lines_to_blocks([H1,"===="++ H2, empty|T], L) ->
    warn(H2, $=),
    lines_to_blocks(T, [{node,h1,#{},trim(H1)}|L]);
lines_to_blocks([H1,"----"++ H2, empty|T], L) ->
    warn(H2, $-),
    lines_to_blocks(T, [{node,h2,#{}, trim(H1)}|L]);
lines_to_blocks([[C,$\s|H]|T], L) when C == $-; C == $*; C==$+ ->
    %% -bTTT
    %% *bTTT
    %% +bTTT
    %%  Some kind of list
    %%   Continued ...
    {Body,T1} = collect_body(T, 2, [H]),
    Parse = parse_lines(Body),
    lines_to_blocks(T1, [{li,C,Parse}|L]);
lines_to_blocks([[$\[|First],[S|Second]|T], L) when S == $\s ->
    %% Div with class.
    %% [TTTT    Tag
    %%  BBBBB  Body
    First1    = remove_last_bracket(First),		       
    Tag       = trim(First1),
    Nblanks   = 1 + compute_offset(Second),
    H1        = drop_n_chars(Nblanks-1, Second),
    {Body,T1} = collect_body(T, Nblanks, [H1]),
    Parse     = parse_lines(Body),
    lines_to_blocks(T1, [{node,'div',#{class=>Tag},Parse}|L]);
lines_to_blocks([[H|First],[S|Second]|T], L) when H =/= $\s, S == $\s ->
    %% Definiton lists.
    %% TTTT    Tag
    %%  BBBBB  Body
    Tag       = trim([H|First]),
    Nblanks   = 1 + compute_offset(Second),
    H1        = drop_n_chars(Nblanks-1, Second),
    {Body,T1} = collect_body(T, Nblanks, [H1]),
    Parse     = parse_lines(Body),
    lines_to_blocks(T1, [{def,Tag,Parse}|L]);
lines_to_blocks(["    "++H|T], L) ->
    {Body,T1} = collect_body(T, 4, [H]),
    %% The body of a preformatted is NOT recursivly scanned
    %% but it does contain empty lines
    %% we just return a string here
    %% so turn the empty lines into blank lines append all the blocks togther
    Body1 = lists:map(fun(empty) -> "\n";
		         (X) -> X
		      end, Body),
    Body2 = lists:append(Body1),
    lines_to_blocks(T1, [{node,pre,#{}, Body2}|L]);
lines_to_blocks(["> " ++ H|T], L) ->
    {{node,p,_,B}, T1} = collect_para([H|T]),
    lines_to_blocks(T1, [{node, blockquote,#{}, B}|L]);
lines_to_blocks(T, L) ->
    trace({its,a,parar,T}),
    {Para, T1} = collect_para(T),
    lines_to_blocks(T1, [Para|L]).

%%----------------------------------------------------------------------
%% after collecting the blocks we merge some of them in
%% a post processing phase

post_process([{li,_,_}|_]=A) ->
    {Inner, T1} = lists:splitwith(fun is_li/1, A),
    Inner1 = [{node,li,#{},X} || {li,_,X} <- Inner],
    [{node, ul,#{}, Inner1}|post_process(T1)];
post_process([{def,_,_}|_]=A) ->
    {Inner, T1} = lists:splitwith(fun is_def/1, A),
    Inner1 = [[{node,dt,#{},Head},{node,dd,#{},Body}] ||
		 {def, Head, Body} <- Inner],
    Inner2 = lists:append(Inner1),
    [{node,dl,#{},Inner2}|post_process(T1)];
post_process([H|T]) ->
    [H|post_process(T)];
post_process([]) ->
    [].

%%----------------------------------------------------------------------
%% Collectors

collect_fenced_block(Type, T) ->
    %% the first character in the fenced block is
    %% the first charcter in T 
    {Content, After}  = collect_fenced(T, []),
    Block = case Type of
		"" -> {node, pre, #{}, Content};
		_  -> {node, pre, #{class => Type},Content}
	    end,
    {Block, [empty|After]}.

collect_fenced(["```\n"|T], L) -> {reverse(L), T};
collect_fenced([empty|T], L)   -> collect_fenced(T, [$\n|L]);
collect_fenced([H|T], L)       -> collect_fenced(T, reverse(H,L));
collect_fenced([], L)          -> 
    io:format("bad fenced block starting:~p~n",[R=reverse(L)]),
    {R, []}.

%%----------------------------------------------------------------------
%% ----
%% Key1: Value1
%% Key2: Value2
%% ----

collect_param_block(L) ->
    F = fun("---\n") -> false; (_) -> true end,
    case lists:splitwith(F, L) of
	{Before, [_|After]} ->
	    {Before, After};
	_ ->
	    exit(badParameterBLock)
    end.

%%----------------------------------------------------------------------
%% collect a paragraph stopping at an empty line or eof

collect_para(Lines) -> collect_para(Lines, []).
    
collect_para([empty|T], L) -> {make_para(L), T};
collect_para([], L)        -> {make_para(L), []};
collect_para([H|T], L)     -> collect_para(T, [H|L]).

make_para(L) ->
    trace({make_para,L}),
    Str = lists:append(lists:reverse(L)),
    trace({str,Str}),
    %% io:format("Para=~p~n",[Str]),
    {node, p, #{}, parse_inlines(Str)}.

%%----------------------------------------------------------------------

collect_body([empty|T], 0, L) ->
    %% if the offset is zero we stop at the next blank line
    {reverse(L), T};
collect_body([empty|T], N, L) when N > 0 ->
    %% empty lines get do not terminate an indented block
    collect_body(T, N, [empty|L]);
collect_body([H|T], N, L) ->
    case has_n_blanks(H, N) of
	{yes, Rest} ->
	    collect_body(T, N, [Rest|L]);
	no ->
	    {reverse(L), [H|T]}
    end;
collect_body([], _, L) ->
    {reverse(L), []}.

%%----------------------------------------------------------------------

is_def({def,_,_}) -> true;
is_def(_)         -> false.

is_li({li,_,_}) -> true;
is_li(_) -> false.

toks(Str) ->
    case string:tokens(Str,":\n") of
	[A,B] ->
	    {list_to_atom(trim(A)), trim(B)};
	_ ->
	    io:format("invalid item:~p~n",[Str]),
	    error
    end.

%%----------------------------------------------------------------------
%% Generate a warning if Str does not only contain the character C

warn(Str, C) ->  warn(Str, C, Str).

warn([H|T], H, S)               -> warn(T, H, S);
warn([], _, _)                  -> void;
warn([10], _, _)                -> void;
warn([H|T], H, S) when H == $\s -> warn(T, H, S);
warn(_, C, Str)                 ->
    io:format("Bad character in string expecting only:(~c) found(~w)~n",
	      [C,Str]).
   
%%----------------------------------------------------------------------
%% Count the number of #'s in a hashed header
%% return {node,h1,#{}, Title} 
%%        {node,h2,#{], Title} or 
%%        {node, header,#{level=>N::string()}, Title}

parse_header([$#|T], N) -> parse_header(T, N+1);
parse_header(T, N)      -> make_header_node(N, trim(T)).

make_header_node(1, Str) -> {node,h1,#{}, Str};
make_header_node(2, Str) -> {node,h2,#{}, Str};
make_header_node(N, Str) -> {node,header,#{level=>i2s(N)}, Str}.

%%----------------------------------------------------------------------

string_to_lines([]) ->
    [];
string_to_lines(L) ->
    {Line, L1} = get_line(L, []),
    [check_empty(Line)|string_to_lines(L1)].

get_line("\r\n" ++ T, L) -> {reverse([$\n|L]), T};
get_line("\n" ++ T, L)   -> {reverse([$\n|L]), T};
get_line([H|T], L)       -> get_line(T, [H|L]);
get_line([], L)          -> {reverse(L), []}.

check_empty(L) ->
    case is_empty_line(L) of
	true  -> empty;
	false -> L
    end.

%%----------------------------------------------------------------------
%% expand tabs with a tab stop of Width

expand_tabs(Str) ->
    expand_tabs(0, Str, 4).

%% N is the current column
expand_tabs(_N,[],_) ->
    [];
expand_tabs(_, [$\n|T], W) ->
    [$\n|expand_tabs(0, T, W)];
expand_tabs(N,[$\t|Xs], W) ->
    N1 = W*(N div W) + W,
    [$\s || _ <- lists:seq(N,N1-1)] ++ expand_tabs(N1,Xs,W);
expand_tabs(N,[X|Xs],W) ->
    [X|expand_tabs(N+1,Xs,W)].


%%----------------------------------------------------------------------
%% test cases

test() ->
    test_parse(simple_para1, "aaa\n", [{node,p,#{},"aaa\n"}]),
    test_parse(simple_para2, "aaa", [{node,p,#{},"aaa"}]),
    test_parse(simple_para3, "aaa\nbbb\n", [{node,p,#{},"aaa\nbbb\n"}]),

    test_parse(red,"[red]\n\s[pre]\n\s\saaaa\n\s\sbbbb\n",
	       [{node,'div',#{class=> "red"},
		 [{node,'div',#{class => "pre"},
		   [{node,p,#{},"aaaa\nbbbb\n"}]
		  }]
		}]),

    test_parse(pre,"[pre]\n\sa\n\sb\nc\n",
	       [{node,'div',#{class => "pre"},[{node,p,#{},"a\nb\n"}]},
		{node,p,#{},"c\n"}]),

    test_parse(tag_with_empty_line,"[tag]\n\sa\n\n\sb\n",
	       [{node,'div',#{class => "tag"},
		 [{node,p,#{},"a\n"},{node,p,#{},"b\n"}]}]),
    


    test_parse(skip_to_header, "\n\n\n#\sabc\n\n123\n456",
	       [{node,h1,#{},"abc"},{node,p,#{},"123\n456"}] ),

    test_parse(dl1, "def abc\n\sone\n\stwo\n", 
	       [{node,dl,#{},
		 [{node,dt,#{},"def abc"},
		  {node,dd,#{},[{node,p,#{},"one\ntwo\n"}]}]}]),

    %% now the recursive case
    test_parse(para_in_list, 
	       "* aaa\n  bbb\n",
	       [{node,ul,#{},[{node,li,#{},[{node,p,#{},"aaa\nbbb\n"}]}]}]),
    horray.

test_parse(N, Str, Parse) ->
    io:format("-----\ntest:~p~n",[N]),
    case parse_string(Str) of
	Parse ->
	    io:format("Test:~p succeded~n",[N]);
	Parse1 ->
	    io:format("Test:~p failed~n",[N]),
	    io:format("In:~p~nStr:~p~nExpected:~p~n     Was:~p~n",
		      [N,Str,Parse,Parse1]),
	    exit({failed,N})
    end.


%%----------------------------------------------------------------------
%% compute_offset(Str) -> N
%%  N is the position of the first non-blank character in Str

compute_offset(Str) -> compute_offset(Str, 0).

compute_offset([$\s|T], N) -> compute_offset(T, N+1);
compute_offset(_, N)       -> N. 

%%----------------------------------------------------------------------

i2s(K) -> integer_to_list(K).

parse_inlines(L) ->  md_inlines:parse(L).

remove_last_bracket(L) ->
    case reverse(L) of
	"\n]" ++ T -> reverse(T);
	_          -> L
    end.

trim(Str) ->
    Str1 = remove_whitespace(Str),
    Str2 = reverse(Str1),
    Str3 = remove_whitespace(Str2),
    reverse(Str3).
	
remove_whitespace([H|T]) when H == $\s; H == $\t; 
			      H == $\n; H == $\r -> 
    remove_whitespace(T);
remove_whitespace(X) -> X.

is_empty_line(X) -> [] == remove_whitespace(X).
    
drop_n_chars(0, L) -> L;
drop_n_chars(N, L) -> drop_n_chars(N-1, tl(L)). 

has_n_blanks(X, 0)      -> {yes, X};
has_n_blanks([$\s|T],N) -> has_n_blanks(T, N-1);
has_n_blanks(_, _)      -> no.


trace(X) ->
    io:format("~p:~p ~p~n",[?MODULE,?LINE,X]).


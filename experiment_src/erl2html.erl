-module(erl2html).

-uuid("6c33d6e1-f17b-4326-9193-5114cca68896").

-description("Convert Erlang to HTML").

-tags([erlang,html]).


%% -module(doc42_erl2html).

-export([
	 erl2html/1, 
	 all_erlang_to_html/0, 
	 render/1,
	 tagerl/1,
	 taglist_to_html/1,
	 test/0,
	 start/2,
	 pre/1
	]).

-compile(export_all).



%% version 5 of the tagger

%% Converting Erlang to html is *tricky*
%%  We have the following goals:
%%
%% 1) Preserve the formatting of the origonal, as regards white space and
%%    comments. Only add coloring and hypertext links
%% 2) Add commenting "tags" so that people can comment on the code
%% 3) Provide hypertext links into the documentation (where this exists)
%% 4) Make some attent to color "broken" code - ie code that is not parsable
%% 5) Try to identify the start of each function


%% These goals are complicated for the following reasons:
%%  - A file with the extension .erl might not even contain erlang
%%  - The file might not contain parseable Erlang code
%%  - Macros and include files make life difficult

%% 1) links in imports  *done*
%% 2) links in exports  *done*
%% 3) colors in keywords *done*
%% 4) resolve links in usage of imported forms  *done*
%% 5) add outward links to xml documentation
%% 6) add links to erlang BIFS
%% 7) add loads of comments to this module to describe how it works

%% Bugs
%% Many hypertext links may point to non-existent objects.
%% In particular cross references to the documentation might be
%% incorrect.

%% Notes:
%% The Erlang function Mod:Func/N has the following the links defined:
%%    <a name="Mod.html#Func/N"></a>     -- the code
%%    <a name="Mod_doc.html#Func/N></a>  -- the documentation
%% -compile(export_all).

-import(lists, [reverse/1,reverse/2,sort/1]).

-export([to_html/1]).

%% transform a single file to HTML

test() ->
    make_html(#{outdir => os:getenv("HOME") ++ "/nobackup/erlmods/",
		spec => [
			 "./"
			]}).

make_html(#{outdir := Out, spec := L}) ->
    case filelib:is_dir(Out) of
	true ->
	    [make_html(Out, I) || I <- L];
	false ->
	    io:format("Output dir ~s does not exist~n"
		      "please create it~n",[Out])
    end.

make_html(Out, X) ->
    case filelib:is_dir(X) of
	true ->
	    io:format("listing:~p~n",[X]),
	    {ok, L} = file:list_dir(X),
	    io:format("L=~p~n",[L]),
	    [make_html(Out, X ++ "/" ++ I) || I <- L];
	false ->
	    case filename:extension(X) of
		".erl" -> erl2html(X, Out);
		_      -> void
	    end
    end.

erl2html(File) ->
    erl2html(File, "./gen/").

erl2html(File, OutDir) ->
    io:format("transforming:~p~n",[File]),
    Base = filename:basename(File, ".erl"),
    io:format("~s~n",[File]),
    Forms = tagerl(File),
    HTML = render(Forms),
    Out = OutDir ++ Base ++ ".html",
    ok = file:write_file(Out, HTML),
    io:format("Created:~p~n",[Out]).

start(Src, Dest) ->
    %% check for environment variables
    io:format("erl2html plugin started Src=~p Dest=~p~n",[Src,Dest]).

must_have(K, L) ->
    case proplists:get_value(K, L, no) of
	no ->
	    exit({missing,env,variable,K});
	Val ->
	    Val
    end.

bug() ->
    erl2html("/Users/joe/installed/otp_src_R14B02/lib/crypto/src/crypto.erl").

test1() ->
    erl2html("erl2html.erl").

erl_install() ->
    case os:getenv("HOME") of
	"/home/joe" ->
	    "/home/joe/installed/otp_src_R15B/lib/";
	"/Users/joe" ->
	    "/Users/joe/installed/otp_src_R14B02/lib/"
    end.

%% transform all erlang in the erlang distribution to html

all_erlang_to_html() ->
    L = elib1_find:files(erl_install(), "*.erl", true),
    [erl2html(I) || I <- L].


tagerl(File) ->
    Forms = read_raw_forms(File),
    Forms1 = [process_form(I) || I <- Forms],
    dump("forms1.tmp", [Forms1]),
    Forms2 = resolve_import_links(Forms1),
    dump("forms2.tmp", [Forms2]),
    Forms2.

%% Parse a single form
%% and colorize the tokens

process_form(Toks) ->
    Toks1 = compress_tokens(Toks),
    {Ws, Toks2} = isolate_ws(Toks1),
    Toks3 = renumber_tokens(Toks2),
    Toks4 = make_parsable(Toks3),
    Toks5 = sneaky_expand_macro(Toks4),
    Parse = parse(Toks5, Toks2),
    {Descriptor, Toks6} = post_process(Parse, Toks3),
    {form3, Descriptor, Ws, Toks6}.

post_process({attribute,_,import,{Mod,_}}=A, Toks) ->
    Toks1 = minimize_tokens(Toks),
    Toks2 = fix_import_annotation(Mod, Toks1),
    Toks3 = fix_attname(Toks2),
    {A, Toks3};
post_process({attribute,_,export,_}=A, Toks) ->
    Toks1 = minimize_tokens(Toks),
    Toks2 = fix_export_annotation(Toks1),
    Toks3 = fix_attname(Toks2),
    {A, Toks3};
post_process({attribute,_,spec,{What,_Parse}}, Toks) ->
    Toks1 = minimize_tokens(Toks),
    {{spec1,What}, Toks1};
post_process({attribute,_,_,_}=A, Toks) ->
    Toks1 = minimize_tokens(Toks),
    Toks2 = fix_attname(Toks1),
    {A, Toks2};
post_process({function,_,Name,Arity,Clauses}, Toks) ->
    S = summary(Clauses, []),
    Toks1 = minimize_tokens(Toks),
    %% io:format("S=~p~nToks1=~p~n",[S, Toks1]),
    Toks2 = patch_token_list(Toks1, S),
    Toks3 = patch_function_names(Toks2, a2s(Name)),
    %% the purpose of calling test_heuristic here is to make sure the
    %% heuristic function test gets really well tested - it (the heurstic)
    %% test is called otherwise when function parsing fails
    test_heuristic(Name, Arity, Toks),
    {{function1,Name,Arity}, Toks3};
post_process({special,_}=X, Toks) ->
    {X, minimize_tokens(Toks)}.

parse([], _) ->
    {special, empty};
parse(Toks, Toks1) ->
    case erl_parse:parse_form(Toks) of
	{ok, Parse1} ->
	    Parse1;
	{error, Why} ->
	    Toksb = make_parsable(Toks1),
	    parse1(Toksb, Why)
    end.

parse1([{'-',_},{atom,_,X}|_]=Toks, Error) ->
    case lists:member(X, special()) of
	true ->
	    {special, X};
	false ->
	    io:format("** error1:~p~n Toks=~p~n",[Error, Toks]),
	    {special, unknown}
    end;
parse1(Toks, Error) ->
    case test_for_function(Toks) of
	{yes, Name, Arity, _} ->
	    %% io:format("Hooray:~p/~p~n",[Name,Arity]),
	    {special, {heurstic, Name, Arity}};
	no ->
	    io:format("** error2:~p~n Toks=~p~n",[Error, Toks]),
	    %% exit(aaaa),
	    {special, unknown}
    end.


%%----------------------------------------------------------------------
%% renumber_tokens(Toks) -> Toks
%%   this add a sequence number 1,2,3.. to the tokens
%%   which *replaces* the regualar line number.
%%   Why do this?
%%   This is used so that we can extract data from the parse tree and relate
%%   it back to the tokens from which the parse was derived

renumber_tokens(L) ->
    renumber_tokens(L, 1, []).

renumber_tokens([{Tag,_,Str,Val}|T], N, L) -> 
    renumber_tokens(T, N+1, [{Tag,N,Str,Val}|L]);
renumber_tokens([{Tag,_,Str}|T], N, L) ->
    renumber_tokens(T, N+1, [{Tag,N,Str}|L]);
renumber_tokens([], _, L) ->
    reverse(L).

%%----------------------------------------------------------------------
%% read_raw_forms(File) -> {[WS], [Toks]}
%% 
%%    read all raw forms. A raw form is a sequence of tokens terminated by
%%    dot. WS
%%    Isolate the white space *before* the start of the form
%%    reduce the token in the form to a managable form

read_raw_forms(File) ->
    Str = file2string(File),
    loop(erl_scan:tokens([], Str, {1,1}, [return,text]), []).

loop({done, {eof,_}, eof}, L) ->
    reverse(L);
loop({done, {ok, Toks, _}, eof}, L) ->
    reverse([Toks|L]);
loop({done, {ok, Toks, Ln}, Str1}, L) ->
    loop(erl_scan:tokens([], Str1, Ln, [return,text]), [Toks|L]);
loop({more, X}, L) ->
    loop(erl_scan:tokens(X, eof, {1,1}, [return,text]), L).

%%----------------------------------------------------------------------
%% the scanner returns rather more information than we need
%% so we only extract what we need later

compress_tokens([{Tag,L,Val}|T]) ->
    Text = proplists:get_value(text, L),
    Line = proplists:get_value(line, L),
    [{Tag,Line,Text,Val}|compress_tokens(T)];
compress_tokens([{Tag,L}|T]) ->
    Text = proplists:get_value(text, L),
    Line = proplists:get_value(line, L),
    [{Tag,Line,Text}|compress_tokens(T)];
compress_tokens([]) ->
    [].

%%----------------------------------------------------------------------
%% forms begin with possible WS - we remove this first
%% isolate white space

isolate_ws(L) -> isolate_ws(L, []).

isolate_ws([{white_space,_Ln,_,Str}|T],L) ->
    isolate_ws(T, [{ws,Str}|L]);
isolate_ws([{comment,_Ln,_,Str}|T],L) ->
    isolate_ws(T, [{comment,Str}|L]);
isolate_ws(T, L) ->
    {reverse(L), T}.

%%----------------------------------------------------------------------
%% This is a bt sneaky - we repace ?XXXX by the atom '$$MACRO' in the token
%% stream - this is so we don't need to keep track of and expand macros
%% since the parser will just see a regular function call

sneaky_expand_macro([{'?',Ln},_|T]) ->
    [{atom,Ln,'$$MACRO$$'}|sneaky_expand_macro(T)];
sneaky_expand_macro([H|T]) ->
    [H|sneaky_expand_macro(T)];
sneaky_expand_macro([]) ->
    [].

%%----------------------------------------------------------------------
%% this converts tokens to the form required by the parser
%% up to now we retain white space and comments in the token stream
%% but these have to be remove prior to parsing

make_parsable([{white_space,_,_,_}|T]) -> make_parsable(T);
make_parsable([{comment,_,_,_}|T])     ->  make_parsable(T);
make_parsable([{Tag,Ln,_,Val}|T])      -> [{Tag,Ln,Val}|make_parsable(T)];
make_parsable([{Tag,Ln,_Val}|T])       -> [{Tag,Ln}|make_parsable(T)];
make_parsable([H|T])                   -> [H|make_parsable(T)];
make_parsable([])                      -> []. 

%%----------------------------------------------------------------------
%% special forms not recognised by the parser
%% noramlly epp.erl will remove these from the token stream
%% and do what is implied by the command

special() ->
    [define,include,ifdef,ifndef,else,endif, include_lib].

%%----------------------------------------------------------------------

minimize_tokens([{Tag,Ln,_Val}|T]) ->
    [{Tag,Ln}|minimize_tokens(T)];
minimize_tokens([{Tag,Ln,Str,_}|T]) ->
    [{Tag,Ln,Str}|minimize_tokens(T)];
minimize_tokens([]) ->
    [].

%%----------------------------------------------------------------------
%% summarize information in the parse tree
%% we'll use this information later to fix up the
%% token list. For example, we might tag atom number 23 followed by 24
%% as {remoteFunCall, ...}

%% actually this code can be simplified since we know the tags are unique ...
 
summary({clause,_Ln,_Hd,_Guard,Body}, L) ->
    summary(Body, L);
summary({call,_,{remote,_,{atom,Ln,Mod},{atom,Ln1,Func}},Args}, L) ->
    Key = {Ln,a2s(Mod),Ln1,a2s(Func)},
    Val = {remoteFunCall, length(Args)},
    summary(Args, [{Key, Val}|L]);
summary({call,_,{atom,Ln,Func},Args}, L) ->
    Key = {Ln,a2s(Func)},
    Val = {localFunCall, length(Args)},
    summary(Args, [{Key, Val}|L]);
summary(X, L) when is_tuple(X) ->
    summary(tuple_to_list(X), L);
summary([H|T], L) ->
    summary(T, summary(H, L));
summary(_, L) ->
    L.
	
%%----------------------------------------------------------------------
%% patch_token_list takes the raw tokens and changes them
%% using information derived from the parse tree
    
patch_token_list([{atom,Ln1,F},{'(',Ln2}|T], Patches) ->
    case lists:keysearch({Ln1,F}, 1, Patches) of
	{value, {_,{localFunCall,Arity}}} ->
	    [{callLocal,F,Arity},{'(',Ln2}|
	     patch_token_list(T, Patches)];	    
	false ->    
	    [{atom,Ln1,F},{'(',Ln2}|
	     patch_token_list(T, Patches)]
    end;
patch_token_list([{atom,Ln1, M},{':',Ln2},{atom,Ln3,F}|T], Patches) ->
    case lists:keysearch({Ln1,M,Ln3,F}, 1, Patches) of
	{value, {_,{remoteFunCall,Arity}}} ->
	    [{atom,Ln1,M},{':',Ln2},{callRemote,M,F,Arity}|
	     patch_token_list(T, Patches)];
	false ->
	    [{atom,Ln1, M},{':',Ln2},{atom,Ln3,F}|
	     patch_token_list(T, Patches)]
    end;
patch_token_list([H|T], Patches) ->
    [H|patch_token_list(T, Patches)];
patch_token_list([], _) ->
    [].

%%----------------------------------------------------------------------
%% quote some things for html

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].

prolog() -> "
<!DOCTYPE html>
<meta charset='UTF-8'>
<link rel='stylesheet' href='./erl2html.css' />".

i2s(A) -> integer_to_list(A).
a2s(A) ->  atom_to_list(A).
		   
%%----------------------------------------------------------------------

taglist_to_html(L) -> [to_html(I) || I <- L].
    

render(L) -> 
    [prolog(),"<pre>\n",[render1(I) || I <- L],"</pre>\n"].

render1({form3,Obj,Ws,L}) -> 
    [render_tag(Obj),  taglist_to_html(Ws), taglist_to_html(L)].

render_tag({attribute,_,_,_})      -> "";
render_tag(error)                  -> "";
render_tag({special, {heurstic, Name, Arity}}) ->
    ["<a name='",a2s(Name),"/",i2s(Arity),"'></a>\n"];
render_tag({function1,Name,Arity}) -> 
    ["<a name='",a2s(Name),"/",i2s(Arity),"'></a>\n"];
render_tag(_X)                     -> [].

%% pre({unknown_tag,X}).
    
to_html({'$start',_}=X)      -> pre(X);
to_html({attributeTag,Str})  -> span("attribute", Str);
to_html({atom,_,Str})        -> span("atom", Str);
to_html({func_symbol,_,Str}) -> span("funcname", Str);
to_html({mod_symbol,_,Str})  -> span("modname", Str);
to_html({var, _, Str})       -> span("var", Str); 
to_html({comment,_, Str})    -> span("comment",Str);
to_html({comment,Str})       -> span("comment",Str);
to_html({ws,Str})            -> Str;
to_html({'case',_})          -> span("keyword","case"); 
to_html({'of',_})            -> span("keyword","of"); 
to_html({'end',_})           -> span("keyword","end"); 
to_html({string,_,Str})      -> span("string", quote(Str));
to_html({dot,_})             -> ".\n";
to_html({callRemote,M,F,A})  -> ["<a href='erlmod_",M,".html#",F,"/",i2s(A),"'>",F,"</a>"];
to_html({callLocal,F,A})     -> ["<a href='#",F,"/",i2s(A),"'>",F,"</a>"];
to_html({Tag,_Ln})           -> atom_to_list(Tag);
to_html({_,_,Str})           -> quote(Str);
to_html(X)                   -> pre(X).

span(Tag, Str) ->
    ["<span class='", Tag, "'>", Str, "</span>"].
    
pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

%%----------------------------------------------------------------------
%% This adds hypertext links to imported functions

resolve_import_links(L) ->
    Imports = [{F, Mod} || {form3,{attribute,_,import,{Mod,Fs}},_,_} <- L, F <- Fs],
    %% io:format("Imports=~p~n",[Imports]),
    [resolve_import_links(I, Imports) || I <- L].

resolve_import_links({form3,{function1,_,_}=F, Ws, Toks}, Imports) ->
    {form3, F, Ws,  [resolve_import_links1(I, Imports) || I <- Toks]};
resolve_import_links(X, _) -> X.

resolve_import_links1({callLocal,Func,Arity}=X, Imports) ->
    case lists:keysearch({list_to_atom(Func),Arity},1,Imports) of
	{value, {_,Mod}} ->
	    {callRemote,a2s(Mod),Func,Arity};
	false ->
	    %% this might be in the module erlang
	    Afunc = list_to_atom(Func),
	    %% case erlang:is_builtin(erlang, Afunc, Arity) of
	    case erl_internal:bif(Afunc, Arity) of
		true ->
		    {callRemote, "erlang", Func, Arity};
		false ->
		    X
	    end
    end;
resolve_import_links1(X, _) ->
    X.

%%----------------------------------------------------------------------
%% fix the import and export annotations adding hyper links to the function
%% names

fix_export_annotation(Toks) -> 
    fix_list(Toks, fun(Name, Arity) -> {callLocal,Name,Arity} end).

fix_import_annotation(Mod, Toks) -> 
    fix_list(Toks, fun(Name, Arity) -> {callRemote,a2s(Mod), Name,Arity} end).

fix_list(Toks, F) -> fix_list(Toks, F, []).

fix_list([{'[',_}=H|T], F, L) -> fix_list1(T, F, [H|L]);
fix_list([H|T], F, L)         -> fix_list(T, F, [H|L]).

fix_list1([{atom,_Ln,Name}|T], F, L) ->
    %% found the start of an export
    Arity = find_arity1(T),
    fix_list1(T, F, [F(Name,Arity)|L]);
fix_list1([H|T], F, L) ->
    fix_list1(T, F, [H|L]);
fix_list1([], _, L) ->
    reverse(L).

find_arity1([{integer,_,N}|_]) -> list_to_integer(N);
find_arity1([_|T])             -> find_arity1(T).

%%----------------------------------------------------------------------
%% any ATOM '(' left in the token stream is just the function name since
%% all other ATOM '(' combinations have been replace by {localFun, ...} or
%% {remoteFiunc, ..} annotations

patch_function_names([{atom,Ln1,Name},{'(',Ln2}|T], Name) ->
    [{func_symbol,Ln1,Name},{'(',Ln2}|patch_function_names(T, Name)];
patch_function_names([H|T], Name) ->
    [H|patch_function_names(T, Name)];
patch_function_names([], _) ->
    [].
    

%%----------------------------------------------------------------------
%% this colors the atom after the '-' in an annotation

fix_attname([{'-',L},{atom,_,N}|T]) -> [{'-',L},{attributeTag,N}|T];
fix_attname(X)                      -> X.

%%----------------------------------------------------------------------
%% Heuristic fallback

test_heuristic(Name, Arity, Toks) ->
    Toks1 = make_parsable(Toks),
    case test_for_function(Toks1) of
	{yes, Name, Arity, _} ->
	    %% io:format("test works !!!~n")
	    void;
	_  ->
	    io:format("heurstic might be broken ~p ~p ~p~n",
	    [Name,Arity,Toks])
    end.

test_for_function([{atom,_,Name},{'(',_}|Toks]) ->
    case find_arity(Toks) of
	{yes, Arity, Toks1} ->
	    {yes, Name, Arity, Toks1};
	no ->
	    no
    end;
test_for_function(_) ->
    no.

find_arity([{')',_}|T]) -> {yes, 0, T};
find_arity(T)           -> find_arity(T, 1, [')']).

%% enter a new level
find_arity([{'(',_}|T], N, S)  -> find_arity(T, N, [')'|S]);
find_arity([{'{',_}|T], N, S)  -> find_arity(T, N, ['}'|S]);
find_arity([{'[',_}|T], N, S)  -> find_arity(T, N, [']'|S]);
find_arity([{'<<',_}|T], N, S) -> find_arity(T, N, ['>>'|S]);

%% top-level comma
find_arity([{',',_}|T], N, [S])     -> find_arity(T, N+1, [S]);
%% top-level close
find_arity([{')',_}|T], N, [')'])   -> {yes, N, T};
%% any other close symbol 
find_arity([{X,_}|T], N, [X|S])     -> find_arity(T, N, S);
%% close brack with wrong stack symbol
find_arity([{')',_}|_], _, _)  -> no;
find_arity([{'}',_}|_], _, _)  -> no;
find_arity([{']',_}|_], _, _)  -> no;
find_arity([{'>>',_}|_], _, _) -> no;
%% anything else
find_arity([_|T], N, S) -> find_arity(T, N, S);
find_arity([], _, _)    -> no.



%%----------------------------------------------------------------------
%% these are self-evident

file2string(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin).

dump(_File, _X) ->    
    %% change the comment when debugging
    %% dump1(_File, _X),
    true.

dump1(File, Term) ->
    io:format("** dumping to ~s~n",[File]),
    {ok, S} = file:open(File, [write]),
    io:format(S, "~p.~n",[Term]), 
    file:close(S).

-file("./site/test1.erl.tmp", 1).

-module(test1).

-compile(export_all).

-import(lib_html, [pre/1]).

factorial(N) ->
    #{html => integer_to_list(fac(N))}.

fac(0) ->
    1;
fac(N) ->
    N * fac(N - 1).

reverse_h1(X) ->
    #{html => ["<h1>",lists:reverse(X),"</h1>"]}.

examples(L) ->
    #{html =>
          ["<table>",
           [ 
            row(I) ||
                I <- L
           ],
           "</table>"]}.

row({I,Description}) ->
    Func = atom_to_list(I),
    Str = "test1.web?page=run_demo&arg=" ++ Func,
    Link = ["<a href='",Str,"'>",Str,"</a>"],
    ["<tr><td>",Description,"</td><td>",Link,"</td></tr>"].

erl_show_the_code(Name) ->
    Segs = app_compiler_vsn2:get_segments("./site/test1.web"),
    Code = get_code(Name, Segs),
    H = [code_css(),
         "<div class='code'><pre>",
         "<h2>@function{",
         Name,
         "}</h2>",
         lib_html:quote(Code),
         "</pre></div>"],
    return_html(H).

code_css() ->
    "<style>div.code {background:#a2a2a2;border:1px solid black;width:7"
    "0%} </style>".

get_code(Name, [{function0,Name,Code}|_]) ->
    Code;
get_code(Name, [_|T]) ->
    get_code(Name, T);
get_code(_Name, []) ->
    "no code".

return_html(H) ->
    #{html => H}.

show_the_code([_,{<<"arg">>,X}]) ->
    Name = binary_to_list(X),
    erl_show_the_code(Name).

run_the_code([_,{<<"arg">>,X}]) ->
    Func = list_to_atom(binary_to_list(X)),
    io:format("Run:~p~n", [Func]),
    apply(test1, Func, [a]).

main(In) ->
    Var@1 =
        #{html =>
              <<"\n\n<style>\nbody {margin-left:100px}\n</style>\n\n<h1"
                ">Examples</h1>\n\n<li><a href=\"./test1.web?page=run_d"
                "emo&arg=demo1\">click me</a>\n\n\n">>},
    Var@2 =
        begin
            examples([{demo1,"styling with a class"},
                      {demo2,"reversed text inside an h1"},
                      {demo3,"factorial"},
                      {demo4,"inline javascript"},
                      {demo5,"svg1"},
                      {demo6,"piano"},
                      {demo7,"ajax round trip"},
                      {demo8,"an entry"},
                      {demo9,"a form"}])
        end,
    Var@3 = #{html => <<"\n\n">>},
    app_compiler_vsn2:combine_fragments([Var@1,Var@2,Var@3]).

run_demo(In) ->
    Var@1 = #{html => <<"\n\n<h1>Input</h1>\n\n">>},
    Var@2 = begin show_the_code(In) end,
    Var@3 = #{html => <<"\n\n<h1>Output</h1>\n\n">>},
    Var@4 = begin run_the_code(In) end,
    Var@5 = #{html => <<"\n">>},
    app_compiler_vsn2:combine_fragments([Var@1,Var@2,Var@3,Var@4,Var@5]).

demo1(In) ->
    Var@1 =
        #{html =>
              <<"\n\n<style>\nh1 {color: red}\n</style>\n\n<h1 class='r"
                "ed'>I'm a red header1</h1>\n\n<p>We can style objects "
                "using CSS\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).

demo2(In) ->
    Var@1 =
        #{html =>
              <<"\n\n<p>CSS cannot reverse characters in a string but E"
                "rlang can\n\n<h1> ">>},
    Var@2 = begin #{html => lists:reverse("I'm a header")} end,
    Var@3 = #{html => <<" </h1>\n">>},
    app_compiler_vsn2:combine_fragments([Var@1,Var@2,Var@3]).

demo3(In) ->
    Var@1 = #{html => <<"\n\n<p>Factorial 40 is ">>},
    Var@2 = begin factorial(40) end,
    Var@3 = #{html => <<"\n">>},
    app_compiler_vsn2:combine_fragments([Var@1,Var@2,Var@3]).

demo4(In) ->
    Var@1 =
        #{html =>
              <<"\n\n<button id=\"a\" onclick=\"woo()\">Click me and be"
                " amazed</button>\n\n<script>\nvar n = 0;\n\nfunction w"
                "oo(){\n   n++;\n   document.getElementById('a').innerH"
                "TML = \"you clicked the button \" + n + \" times\";\n}"
                "\n\n</script>\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).

demo5(In) ->
    Var@1 =
        #{html =>
              <<"\n<svg width=\"100\" height=\"100\">\n  <circle cx=\"5"
                "0\" cy=\"50\" r=\"40\" stroke=\"green\" \n\t  stroke-w"
                "idth=\"4\" fill=\"yellow\" \n\t  />\n</svg>\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).

demo6(In) ->
    Var@1 = #{html => <<"\n\n">>},
    Var@2 = begin piano_widget(a) end,
    Var@3 = #{html => <<"\n\n\n">>},
    app_compiler_vsn2:combine_fragments([Var@1,Var@2,Var@3]).

demo7(In) ->
    Var@1 = #{html => <<"\n\n<entry id=\"in\">\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).

demo8(In) ->
    Var@1 = #{html => <<"\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).

piano_widget(In) ->
    Var@1 =
        #{html =>
              <<"\n\n<svg xml:space=\"preserve\" width=\"161px\" height"
                "=\"120\">\n\n<!-- \n     Copyright (c)  2005 Lauri Kai"
                "la.\n     Permission is granted to copy, distribute an"
                "d/or modify this document\n     under the terms of the"
                " GNU Free Documentation License, Version 1.2\n     or "
                "any later version published by the Free Software Found"
                "ation;\n     with no Invariant Sections, no Front-Cove"
                "r Texts, and no Back-Cover Texts.\n     A copy of the "
                "license is included in the section entitled \"GNU\n   "
                "  Free Documentation License\".\n     \n     \n     \n"
                "     Intented to create a keyboard where key widths ar"
                "e\n     accurately in position. \n     \n     See http"
                "://www.mathpages.com/home/kmath043.htm\n     for the m"
                "ath.\n     \n     This keyboard has following properti"
                "es (x=octave width).\n     1. All white keys have equa"
                "l width in front (W=x/7).\n     2. All black keys have"
                " equal width (B=x/12).\n     3. The narrow part of whi"
                "te keys C, D and E is W - B*2/3\n     4. The narrow pa"
                "rt of white keys F, G, A, and B is W - B*3/4\n     \n-"
                "->\n\n<!-- White keys  -->\n<rect style=\"fill:white;s"
                "troke:black\" x=  \"0\" y=\"0\" width=\"23\" height=\""
                "120\" />\n<rect style=\"fill:white;stroke:black\" x= "
                "\"23\" y=\"0\" width=\"23\" height=\"120\" />\n<rect s"
                "tyle=\"fill:white;stroke:black\" x= \"46\" y=\"0\" wid"
                "th=\"23\" height=\"120\" />\n<rect style=\"fill:white;"
                "stroke:black\" x= \"69\" y=\"0\" width=\"23\" height="
                "\"120\" />\n<rect style=\"fill:white;stroke:black\" x="
                " \"92\" y=\"0\" width=\"23\" height=\"120\" />\n<rect "
                "style=\"fill:white;stroke:black\" x=\"115\" y=\"0\" wi"
                "dth=\"23\" height=\"120\" />\n<rect style=\"fill:white"
                ";stroke:black\" x=\"138\" y=\"0\" width=\"23\" height="
                "\"120\" />\n\n<!-- Black keys (overlap with the white "
                "keys) -->\n<rect style=\"fill:black;stroke:black\" x="
                "\"14.33333\" y=\"0\" width=\"13\" height=\"80\" />\n<r"
                "ect style=\"fill:black;stroke:black\" x=\"41.66666\" y"
                "=\"0\" width=\"13\" height=\"80\" />\n<rect style=\"fi"
                "ll:black;stroke:black\" x=\"82.25\" y=\"0\" width=\"13"
                "\" height=\"80\" />\n<rect style=\"fill:black;stroke:b"
                "lack\" x=\"108.25\" y=\"0\" width=\"13\" height=\"80\""
                " />\n<rect style=\"fill:black;stroke:black\" x=\"134.7"
                "5\" y=\"0\" width=\"13\" height=\"80\" />\n\n</svg>\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).

demo9(In) ->
    Var@1 =
        #{html =>
              <<"\n\n<h1>A form included from another web page</h1>\n\n"
                "NYI\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).

main1(In) ->
    Var@1 = #{html => <<"\n\n ">>},
    Var@2 = begin nice1(void) end,
    Var@3 = #{html => <<"   <!-- include a style sheet -->\n ">>},
    Var@4 = begin js1(void) end,
    Var@5 =
        #{html =>
              <<"    <!-- and some javascript -->\n\n<h1>Turning HTML i"
                "n Elang functions</h1>\n\n<p>HTML is a bit of a mess. "
                "It has no subroutines. HTML pages cannot\n<i>call</i> "
                "other pages - which makes them pretty useless.\n\n<p>I"
                "f we want to <i>call</i> some part of an HTML file we "
                "need a way\nof partitioning the file into segments. To"
                " do this I have made what I call a \n<b>.web</b> file."
                "\n\n<p>From now on we can just call fragments of web p"
                "ages just like\nthey are erlang code. The <b>last</b> "
                "item in a call MUST produce a map.\n\n<h1>Calling frag"
                "ments of html</h1>\n\n">>},
    Var@6 = begin bar(In) end,
    Var@7 = #{html => <<"\n\n<p> Factorial 10 is ">>},
    Var@8 = begin factorial(10) end,
    Var@9 =
        #{html =>
              <<"\n\n<h1>About</h1>\n\n<p>This is a fragment of HTML\n">>},
    app_compiler_vsn2:combine_fragments([Var@1,
                                         Var@2,
                                         Var@3,
                                         Var@4,
                                         Var@5,
                                         Var@6,
                                         Var@7,
                                         Var@8,
                                         Var@9]).

bar(In) ->
    Var@1 = #{html => <<"\n\n<p>This is bar\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).

nice1(In) ->
    Var@1 =
        #{html =>
              <<"\n\n<style>\nbody {margin-left:120px; background-color"
                ":#ffaabb}\n</style>\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).

js1(In) ->
    Var@1 =
        #{html =>
              <<"\n\n<script>\nfunction foo(){\n    alert(\"js called\""
                ");\n}\n</script>\n">>},
    app_compiler_vsn2:combine_fragments([Var@1]).




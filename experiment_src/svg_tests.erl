-module(svg_tests).
-compile(export_all).

test() ->
    Nconfig = 4,
    Tree = svg_staves:make(Nconfig),
    svg_lib:make_svg_file("test1.svg", 
			  #{width => 800, height => 400, background => green},
			  [Tree]),
    {X, Y} = svg_staves:top_of_stave(Nconfig, 2),

    Note = make_note(X, Y),
    svg_lib:make_svg_file("test2.svg", 
			  #{width => 800, height => 400, background => green},
			  [Tree,Note]).

make_note(X, Y) ->
    {node,circle,[{cx,X},{cy,Y},{radius,5},{fill,red}], []}.







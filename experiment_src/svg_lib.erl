-module(svg_lib).

-compile(export_all).
-import(lists, [reverse/1]).


make_svg_file(Out, Container, Forest) ->
    Content = [tree2xml(Tree) || Tree <- Forest],
    IO = template(Container, Content),
    elib2_misc:dump("tree", IO),
    elib2_misc:check_io_list(IO),
    ok = file:write_file(Out, IO).

tree2xml(Tree) ->
    erdoc2_lib:tree_to_iol(Tree).


template(#{width:=Width, height:=Ht, background:=Bg}, Content) -> 
    ["<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
        \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg xmlns='http://www.w3.org/2000/svg' version='1.1'
        xmlns:xlink='http://www.w3.org/1999/xlink'
        xml:space='preserve' color='",to_str(Bg),"'
        width='",
     i2s(Width),"' height='",i2s(Ht),"'>\n",
     Content,
     "\n</svg>\n"].

to_str(A) when is_atom(A) ->
    atom_to_list(A);
to_str(X) ->
    X.


i2s(N) ->
    integer_to_list(N).


    
     
    

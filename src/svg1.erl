-module(svg1).
-export([start/1, current_time/0]).
%%% NOTE: lines with three %%% show code when frames are introduced

start(Browser) ->
    SVG = make_svg(),
    Browser ! #{cmd => fill_div, id => svg, txt => SVG},
    running(Browser).

running(Browser) ->
    receive
	{Browser, #{clicked := <<"changered">>}} ->
	    Browser ! #{cmd => configure, id => c1,
			data => [[fill,green],
				 [r,30]]},
	    running(Browser);
	{Browser, #{clicked := <<"delblue">>}} ->
	    Browser ! #{cmd => delete_obj, id => c2},
	    running(Browser);
	{Browser, #{clicked := <<"addtree">>}} ->
	    Node = [{node, circle, 
		    #{cx=>60,  cy=>80, r => 30, fill=> orange},[]}],
	    Tree = list_to_binary(erdoc2_lib:tree_to_iol(Node)),
	    Browser ! #{cmd => append_div, id => svg1, txt => Tree},
	    running(Browser)
    after 1000 ->
      	    Browser ! #{cmd => fill_div, id =>clock, txt => current_time()},
     	    running(Browser)
    end.

idle(Browser) ->
    receive
	{Browser, #{clicked := <<"start">>}} ->
	    Browser ! #{cmd =>fill_div, id => clock, txt => <<"Starting">>},
	    running(Browser)
    end.

current_time() ->
    {Hour,Min,Sec} = time(),
    list_to_binary(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
				 [Hour,Min,Sec])).

make_svg() ->
    Nodes = [{node, circle, 
	      #{cx=>100, id=> c1, cy=>100, r => 20, fill=> red},[]},
	     {node, circle, 
	      #{id => c2, cx=>150, cy=>150, r => 10, fill=> blue},[]}
	    ],
    Tree = mk_svg(Nodes,400,200),
    list_to_binary(erdoc2_lib:tree_to_iol(Tree)).

mk_svg(Nodes,Width,Ht) ->
    {node,svg,
     #{id => "svg1",
       xmlns         => 'http://www.w3.org/2000/svg',
       'xmlns:xlink' => 'http://www.w3.org/1999/xlink', 
       width=> Width,
       height=>Ht},
     [{node,rect,#{style=>'fill:white;stroke:black',
		   x=>0,y=>0,width=>Width,height=>Ht},[]}|Nodes]}.


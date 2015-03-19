-module(connectors).
-compile(export_all).

start(Pid) ->
    io:format("connector started:~p~n",[Pid]),
    Bez = #{start => {100,100},
	    stop => {280,140},
	    c1 => {150,20},
	    c2 => {200,180}},
    Pid ! [#{cmd=> svg_add_canvas, 
	     parent => canvas,
	     id => top, 
	     width => 800, 
	     height => 500,
	     color=> <<"#ffffbb">>}
	  ],
    Pid ! make_bez(Bez),
    loop(Pid, Bez).

loop(Pid, Bez) ->
    receive
	{_,#{cmd := <<"drag_moved">>,
	     id := Tag,
	     x := X, y:=Y}} ->
	    Bez1 = update(Pid, Tag, X, Y, Bez),
	    loop(Pid, Bez1);
	Any ->
	    io:format("Recieved:~p~n",[Any]),
	    loop(Pid, Bez)
    end.


update(Pid, <<"c1">>, X, Y, B) ->
    Bez1 = B#{c1 => {X,Y}},
    Pid ! [#{cmd=>svg_configure,id=>bez,
	     args => [d,bez_path(Bez1)]},
	   #{cmd=>svg_configure,id=>line1,
	     args => [x2,X,y2,Y]}],
    Bez1;
update(Pid, <<"c2">>, X, Y, B) ->
    Bez1 = B#{c2 => {X,Y}},
    Pid ! [#{cmd=>svg_configure,id=>bez,
	     args => [d,bez_path(Bez1)]},
	   #{cmd=>svg_configure,id=>line2,
	     args => [x2,X,y2,Y]}],
    Bez1;
update(Pid, <<"start">>, X, Y, B) ->
    Bez1 = B#{start => {X,Y}},
    Pid ! [#{cmd=>svg_configure,id=>bez,
	     args => [d,bez_path(Bez1)]},
	   #{cmd=>svg_configure,id=>line1,
	     args => [x1,X,y1,Y]}],
    Bez1;
update(Pid, <<"stop">>, X, Y, B) ->
    Bez1 = B#{stop => {X,Y}},
    Pid ! [#{cmd=>svg_configure,id=>bez,
	     args => [d,bez_path(Bez1)]},
	   #{cmd=>svg_configure,id=>line2,
	     args => [x1,X,y1,Y]}],
    Bez1;
update(_,_,_,_,B) ->
    B.

    

make_bez(#{start := Start,
	   stop :=  Stop,
	   c1 := C1,
	   c2 := C2}=B) ->
    make_connector(Start, start, red) ++
	make_connector(Stop, stop, blue) ++
	make_connector(C1, c1, orange) ++
	make_connector(C2, c2, green) ++
	line(Start, C1, line1) ++
	line(Stop, C2, line2) ++
	[#{cmd => svg_add_object,
	   id => bez,
	   type => path,
	   parent => top,
	   fill => none,
	   'stroke-width' => 2,
	   stroke => blue,
	   d => bez_path(B)}].


bez_path(#{start:=Start, stop:=Stop, c1:=C1, c2:=C2}) ->
    list_to_binary([$M,pair(Start),$C,pair(C1),pair(C2),pair(Stop)]).

pair({X,Y}) ->
    [integer_to_list(X),$,,integer_to_list(Y),$\s].


line({X1,Y1}, {X2,Y2}, Id) ->    
    [#{cmd => svg_add_object, type => line, parent => top,
       id => Id, x1 => X1, y1 => Y1, x2 => X2, y2 => Y2, stroke => red}].

make_connector({X,Y} ,Id, Color) ->
    [#{cmd => svg_add_object, type => g, parent => top,
       id => Id, onmousedown => <<"selectElement(evt)">>,
       x => X, y => Y,
       transform => translate(X,Y)},
     #{cmd => svg_add_object, type => circle, parent => Id,
       class => control_point, 
       cx => 0, cy => 0, r=> 10},
     #{cmd => svg_add_object, type => circle, parent => Id,
       class => <<"invisible">>,fill=> Color,
       cx => 0, cy => 0, r=> 3}].

translate(X,Y) ->
    list_to_binary(["translate(", integer_to_list(X),",",
		    integer_to_list(Y),")"]).
    


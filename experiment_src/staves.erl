-module(staves).
-compile(export_all).

config(4) ->
    #{n => 4, g2 => 5, ht=>400, width=>800, 
      side => 20, w=>1, delta => 5}.

gs(#{n := N, w := W, g2 := G2, delta := Delta, ht := H}) ->
    Sh = 4*G2 + 5*W,
    G3 = (H - 2*Delta - 4*Sh)/(2+N-1),
    G1 = G3 + Delta,
    {G1, G3, Sh}.

top_of_stave(NConfig, N) ->
    Z = gs(config(NConfig)),
    top_of_stave1(N, Z).

top_of_stave1(0, {G1, _, _}) ->
    G1;
top_of_stave1(N, {G1, G3, Sh}) ->
    G1 + (N-1)*(Sh + G3).

test() ->
    compute_staves(4).

number_of_staves(N) ->
    maps:get_key(n, config(N)).

compute_staves(NConfig) ->
    #{n:=N, side := Start, ht := Ht, width:= Width, w:=W, g2:=G2} = config(NConfig),
    Inc = W + G2,
    L1 = lists:flatten(for(1,N, 
			   fun(I) ->
				   Top = top_of_stave(NConfig, I),
				   [Top, Top+Inc, Top+2*Inc, Top+3*Inc, Top+4*Inc]
			   end)),
    Ys = [trunc(I) || I <- L1],
    Xstart = Start,
    Xstop = Width - 2*Start,
    Draw =
	list_to_binary([[$M,i2s(Xstart), $\s,i2s(Y),
			 $H,i2s(Xstop)] || Y <- Ys]),
    [     
	  #{cmd => svg_add_object, 
	    type => g,
	    x=> 0, y=> 0,
	    transform => <<"translate(20,20)">>,
	    parent => top,
	    id => notes, 
	    stroke => blue},
	  #{cmd => svg_add_object,
	    type => rect,
	    parent => notes,
	    x=>0,
	    y=>0,
	    height => Ht,
	    width => Width,
	    fill => white},

	  #{cmd => svg_add_object, 
	    type => path,
	    parent => notes, 
	    d => Draw}].

i2s(I) ->
    integer_to_list(I).




for(N,N,F)   -> [F(N)];
for(I, N, F) -> [F(I)|for(I+1,N,F)].

    
    

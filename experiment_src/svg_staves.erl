-module(svg_staves).
-compile(export_all).

make(N) ->
    compute_staves(config(N)).

top_of_stave(Nconfig, Nstave) ->
    Config = config(Nconfig),
    X = maps:get(side, Config),
    Y = top_of_stave0(Config, Nstave),
    {X, trunc(Y)}.



test() ->
    make(4).

config(4) ->
    %% n => number of staves
    %% g2 => horizontal gap
    %% delta => ?
    %% w => line width
    #{n => 4, g2 => 5, ht=>400, width=>800,  side => 20, w=>1, delta => 5}.


compute_staves(#{n:=N, side := Start, ht := Ht, 
		 width:= Width, w:=W, g2:=G2} = Config) ->
    Inc = W + G2,
    L1 = lists:flatten(for(1,N, 
			   fun(I) ->
				   Top = top_of_stave0(Config, I),
				   [Top, Top+Inc, Top+2*Inc, Top+3*Inc, Top+4*Inc]
			   end)),
    %% L1 = list of Y coords of the staves
    Ys = [trunc(I) || I <- L1],
    %% Ys = list of Y coords of the staves (integers)
    Xstart = Start,
    Xstop = Width - 2*Start,
    %% Xstart and stop are constants
    Draw  = list_to_binary([[$M,i2s(Xstart),$\s,i2s(Y),
			     $H,i2s(Xstop)] || Y <- Ys]),
    {node, g, [{x,0},{y,0},{stroke,blue}],
     [{node,rect,[{x,0},{y,0},{height,Ht},{width,Width},
		  {fill,white}],[]},
      {node,path,[{d,Draw}],[]}]}.

gs(#{n := N, w := W, g2 := G2, delta := Delta, ht := H}) ->
    Sh = 4*G2 + 5*W,
    G3 = (H - 2*Delta - 4*Sh)/(2+N-1),
    G1 = G3 + Delta,
    {G1, G3, Sh}.

top_of_stave0(Config, N) -> top_of_stave1(N, gs(Config)).

top_of_stave1(0, {G1, _, _})   -> G1;
top_of_stave1(N, {G1, G3, Sh}) -> G1 + (N-1)*(Sh + G3).

number_of_staves(N) -> maps:get_key(n, config(N)).

i2s(I) -> integer_to_list(I).

for(N,N,F)   -> [F(N)];
for(I, N, F) -> [F(I)|for(I+1,N,F)].

    
    

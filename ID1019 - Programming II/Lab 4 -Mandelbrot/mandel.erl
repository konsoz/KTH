%% @author konstantin
%% @doc @todo Add description to mandel.


-module(mandel).
-compile(export_all).

mandelbrot(Width, Height, X, Y, K, Depth) ->
	Trans = fun(W, H) -> cmplx:new(X + K*(W-1), Y-K*(H-1)) end,
    rows(Width, Height, Trans, Depth, self()),
	collect(Height,[]).

rows(_, 0, _Trans, _Depth, Ctrl) -> ok;
rows(Width, Height, Trans, Depth, Ctrl) ->
	 spawn(fun()->report(Width, Height, Trans, Depth, Ctrl) end),
	 rows(Width, Height-1, Trans, Depth, self()).
	

row(0, Height, Trans, Depth, Colors) -> Colors; 
row(Width, Height, Trans, Depth, Colors) ->
		C = Trans(Width,Height),
		D = brot:mandelbrot(C, Depth),
		Color = color:convert(D,Depth),
		row(Width-1, Height, Trans, Depth, [Color|Colors]).
		
collect(0,Rows) -> Rows;
collect(H, Rows) ->
			receive 
				{row,H,Row} -> collect(H-1,[Row|Rows])
			end.
		
report(Width,Height,Trans,Depth,Ctrl) -> 
		Row = row(Width, Height, Trans, Depth, []),
		Ctrl ! Row.


demo() -> small(-0.3,0.9,0.13).
small(X,Y,X1) ->
	Width = 1920,
	Height = 1080,
	K = (X1 - X)/Width,
	Depth = 128,
	T0 = now(),
	Image = mandelbrot(Width, Height, X, Y, K, Depth),
	T = timer:now_diff(now(), T0),
	io:format("picture generated in ~w ms~n", [T div 1000]),
	ppm:write("zoom.ppm", Image).


big(X,Y,X1) ->
	Width = 1920,
	Height = 1080,
	K = (X1 - X)/Width,
	Depth = 64,
	T0 = now(),
	Image = mandelbrot(Width, Height, X, Y, K, Depth),
	T = timer:now_diff(now(), T0),
	io:format("picture generated in ~w ms~n", [T div 1000]),
	ppm:write("big.ppm", Image).


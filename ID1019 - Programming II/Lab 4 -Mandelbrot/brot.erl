%% @author konstantin
%% @doc @todo Add description to brot.


-module(brot).
-compile(export_all).

mandelbrot(C, M) ->
	Z0 = cmplx:new(0,0),
	I = 0,
	test(I, Z0, C, M).

test(I,Z0,C,0) -> 0;
test(I,Z0,C,M) -> 
			Z = cmplx:add(cmplx:sqr(Z0),C),
			Abs = cmplx:abs(Z), 
			if Abs >= 2 -> I;
			true -> test(I+1,Z,C,M-1)
			end.
			

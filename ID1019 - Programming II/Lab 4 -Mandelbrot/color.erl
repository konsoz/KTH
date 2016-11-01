%% @author konstantin
%% @doc @todo Add description to color.


-module(color).
-compile(export_all).

convert(Depth,Max) ->
		F = Depth/Max,
		A = F * 4,
		X = trunc(A),
		Y = trunc(255*(A-X)),
		getColor(X,Y).
		
getColor(0, Y) -> {Y, 0, 0};
getColor(1, Y) -> {255, Y, 0};
getColor(2, Y) -> {255-Y, 255, 0};
getColor(3, Y) -> {0, 255, Y};
getColor(4, Y) -> {0, 255-Y, 255}.
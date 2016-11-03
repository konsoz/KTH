-module(time).
-compile(export_all).


zero() -> 0.

inc(Name,T) -> T + 1.

merge(Ti,Tj) -> max(Ti,Tj).

leq(Ti,Tj) -> 
	case Ti =< Tj of
		true -> true;
		false -> false
	end.


clock([]) -> [];
clock(Nodes) -> [{hd(Nodes),0}|clock(tl(Nodes))].

update(Node,Time,Clock) -> lists:keyreplace(Node,1,Clock,{Node,Time}).

safe(Time,Clock) -> 
	LowestClock = lists:foldl(fun({_, T}, LowestSoFar) -> min(T, LowestSoFar) end, inf, Clock),
	leq(Time,LowestClock).



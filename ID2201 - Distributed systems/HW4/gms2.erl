-module(gms2).
-compile(export_all).
-define(ARGH,25).
-define(TIMEOUT,5000).

start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd ,Self) end)}.

start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd ,Grp, Self) end)}.

init(Id, Rnd ,Master) -> 
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, [], [Master]).

init(Id,Rnd,Grp,Master) ->
	random:seed(Rnd,Rnd,Rnd),
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		{view, [Leader|Slaves], Group} ->
			erlang:monitor(process, Leader),
			Master ! {view, Group},
			slave(Id, Master, Leader, Slaves, Group)
	end.


leader(Id, Master, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, Slaves, Group);
		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
			Master ! {view, Group2},
			leader(Id, Master, Slaves2, Group2);
		stop -> ok
		after 
			?TIMEOUT -> Master ! {error, "no reply from leader"}
	end.

slave(Id, Master, Leader, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, Slaves, Group);
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, Slaves, Group);
		{msg, Msg} ->
			Master ! Msg,
			slave(Id, Master, Leader, Slaves, Group);
		{view, [Leader|Slaves2], Group2} ->
			Master ! {view, Group2},
			slave(Id, Master, Leader, Slaves2, Group2);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, Slaves, Group);
		stop -> ok
	end.


election(Id, Master, Slaves, [_|Group]) ->
	Self = self(),
	case Slaves of 
		[Self|Rest] ->
			bcast(Id, {view, Slaves, Group}, Rest),
			Master ! {view, Group},
			leader(Id, Master, Rest, Group);
		[Leader|Rest] ->
			erlang:monitor(process, Leader),
		slave(Id, Master, Leader, Rest, Group)
	end.


bcast(Id, Msg, Nodes) -> lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
	case random:uniform(?ARGH) of
		?ARGH -> 
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ -> ok
end.
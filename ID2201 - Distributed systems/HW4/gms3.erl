-module(gms3).
-compile(export_all).
-define(ARGH,100).
-define(TIMEOUT,5000).

% Start node as leader
% Id: the node identifier
start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd ,Self) end)}.

% Start node as slave
% Id: the node identifier
% Grp: the address of a node in the group to connect to
start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd ,Grp, Self) end)}.

% Initialize node as leader (all alone)
% Id: the node identifier
% Rnd: a seed for the random number generator
% Master: this gms' worke
init(Id, Rnd ,Master) -> 
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, 0,[], [Master]).

% Initialize node as slave
% Master: this gms' worker
init(Id,Rnd,Grp,Master) ->
	random:seed(Rnd,Rnd,Rnd),
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		{view, N,[Leader|Slaves], Group} ->
			erlang:monitor(process, Leader),
			Master ! {view, Group},
			slave(Id, Master, Leader,N+1, {view, N,[Leader|Slaves], Group},Slaves, Group)
		after
			?TIMEOUT -> Master ! {error, "no reply from leader"}
	end.

% This procedure represents a node in the 'leader' state
% Id: the node identifier
% Master: the gms' worker
% N: next sequence number
% Group: a global-synchronized sequence of peers (from oldest to newest)
leader(Id, Master, N,Slaves, Group) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, N, Msg}, Slaves),
			Master ! Msg,
			leader(Id,Master,N+1,Slaves,Group);
		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, N,[self()|Slaves2], Group2}, Slaves2),
			Master ! {view, Group2},
			leader(Id, Master,N+1, Slaves2, Group2);
		stop -> ok
	end.

slave(Id,Master,Leader,N,Last,Slaves,Group) ->
	receive
		%% Receive a multicast request from the master - forward to the group leader
		{mcast, Msg} ->
			Leader ! {mcast, Msg}, 
			slave(Id,Master,Leader,N,Last,Slaves,Group);
		%% Receive a join request from a new peer - forward to group leader
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id,Master,Leader,N,Last,Slaves,Group);
		%% Receive a duplicate/old message from the leader - discard
		{msg, I, _} when I < N ->
			slave(Id,Master,Leader,N,Last,Slaves,Group);
		%% Receive a new message from the leader - deliver to the master
		{msg, I,Msg} ->
			Master ! Msg,
			slave(Id,Master,Leader,I+1,{msg, N,Msg},Slaves,Group);
		%% Receive a new view from the leader - deliver to the master
		{view, I,[Leader|Slaves2], Group2} ->
			Master ! {view, Group2},
			slave(Id,Master,Leader,I,{view, I,[Leader|Slaves2], Group2},Slaves2, Group2);
		%% Receive a notification from the monitor that the leader is down
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master,N,Last, Slaves, Group);
		stop -> ok
	end.

% This procedure represents a node in the 'election' state
% Id: the node identifier
% Master: the gms' worker
% N: the next sequence number
% Slaves: list of slave processes
% Last: the last global state-related message from the leader
% [_|Group]: the global-synchronized sequence of peers
election(Id, Master,N,Last,Slaves, [_|Group]) ->
	Self = self(),
	case Slaves of 
		[Self|Rest] ->
			bcast(Id, Last, Rest),
			bcast(Id, {view, N, Slaves, Group}, Rest),
			Master ! {view, Group},
			leader(Id, Master,N, Rest, Group);
		[Leader|Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader,N,Last, Rest, Group)
	end.


bcast(Id, Msg, Nodes) -> 
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
	case random:uniform(?ARGH) of
		?ARGH -> 
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ -> ok
end.
-module(node3).
-export([start/1,start/2]).
-define(Stabilize,1000).
-define(Timeout,5000).

% Start a node with a certain key on a new DHT
start(Id) -> start(Id, nil).

% Start a node with a certain key and join a DHT
start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

% Initialize a node
% Responsible for notifying the remote node in the target DHT
% and for scheduling the periodic stabilization
init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor,storage:create(),nil).

% Connect to a node in a DHT...or join a existing DHT
connect(Id, nil) -> {ok, {Id, nil,self()}};
connect(_, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			Ref = monitor(Peer),
			{ok, {Skey,Ref,Peer}}
		after 
			?Timeout -> io:format("Time out: no response~n",[])
	end.

node(Id, Predecessor, Successor,Store,Next) ->
	receive
		% a peer node wants to know our key
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor,Store,Next);
		% a new node informs us of its existence
		{notify, New} ->
			{Pred,Keep} = notify(New, Id, Predecessor,Store),
			node(Id, Pred, Successor,Keep,Next);
		% a potential predecessor needs to know our predecessor
		{request, Peer} ->
			request(Peer, Predecessor,Successor),
			node(Id, Predecessor, Successor,Store,Next);
		% our successor informs us about his current predecessor and successor
		{status, Pred, Nx} ->
			{Succ,Nxt} = stabilize(Pred, Id, Successor,Nx),
			node(Id, Predecessor, Succ,Store,Nxt);
		% received a probe from a client - pass it around!
		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor,Store,Next);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor,Store,Next);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor,Store,Next);
		% receive request to add a (key,value) to the datastore
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client,Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Added,Next);
		% lookup a value by key in the datastore
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store,Next);
		% receive part of someone's datastore to add to our own
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(Id, Predecessor, Successor, Merged,Next);
		{'DOWN', Ref, process, _, _} ->
			{Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
			node(Id, Pred, Succ, Store,Nxt);
		% stabilize ourselves periodically
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor,Store,Next);
		state ->
	    	io:format(' Id : ~w~n Predecessor : ~w~n Successor : ~w~n', [Id, Predecessor, Successor]),
	    	io:format("Store: "),
    		Printer = fun({Key,Value}) -> io:format("~w :: ~w ",[Key,Value]) end,
    		lists:foreach(Printer,Store),
	    	node(Id, Predecessor, Successor,Store,Next);
	    stop -> 
	    	bye;
	    _ ->
	    	io:format('Strange message received'),
	    	node(Id, Predecessor, Successor,Store,Next)
	end.

% Add a key-value pair to our storage or pass it to other node
add(Key, Value, Qref, Client, Id, {Pkey, _,_}, {_, _,Spid}, Store) ->
	case key:between(Key,Pkey,Id) of
		true ->
			Added = storage:add(Key,Value,Store),
			Client ! {Qref, ok},
			Added;
		false ->
			Spid ! {add,Key,Value,Qref,Client},
			Store
	end.

% Look up in our storage or pass it to other node
lookup(Key, Qref, Client, Id, {Pkey, _,_}, Successor, Store) ->
	case key:between(Key,Pkey,Id) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, _ ,Spid} = Successor,
			Spid ! {lookup,Key,Qref,Client}
	end.


create_probe(Id,{_,_,Spid}) -> 
	Spid ! {probe,Id,[Id],erlang:system_time(micro_seconds)}.

forward_probe(Ref,T,Nodes,Id,{_,_,Spid}) ->
	Spid ! {probe,Ref,Nodes++[Id],T}.

remove_probe(T,Nodes) ->
    Duration = erlang:system_time(micro_seconds)-T,
    io:format("--------PROBE--------~n"),
    io:format("Nodes:"),
    Printer = fun(E) -> io:format("~p ",[E]) end,
    lists:foreach(Printer,Nodes),
    io:format("~n Time = ~p~n",[Duration]),
    io:format("---------------------~n").


% notify
% Someone notifies us that we are his successor (he is our predecessor)
% We have to do some investigation, though...
% Returns: {{PredecessorKey, PredecessorPid}, Datastore}
notify({Nkey,_,Npid}, Id, Predecessor,Store) ->
	case Predecessor of
		nil ->
			Nref = monitor(Npid), 
			Keep = handover(Id, Store, Nkey, Npid),
			{{Nkey, Nref, Npid},Keep};
		{Pkey,Pref,_} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					drop(Pref),
					Nref = monitor(Npid),
					Keep = handover(Id, Store, Nkey, Npid),
					{{Nkey, Nref,Npid},Keep};
				false ->
					{Predecessor,Store}
			end
	end.

handover(Id,Store,Nkey,Npid) ->
	{OurStore,Rest} = storage:split(Nkey,Id,Store),
	Npid ! {handover,Rest},
	OurStore.


% request
% A potential predecessor needs to know our successor
% Peer: the PID of the peer requesting the information
% Predecessor: our current predecessor
request(Peer, Predecessor,{Skey, Sref, Spid}) ->
	case Predecessor of
		nil -> 
			Peer ! {status, nil,{Skey,Sref,Spid}};
		{Pkey, Pref,Ppid} ->
			Peer ! {status, {Pkey, Pref,Ppid},{Skey,Sref,Spid}}
	end.

stabilize({_, _,Spid}) -> Spid ! {request, self()}.

% stabilize
% Our successor informs us about his current predecessor
% Pred: our successor's current predecessor (RECEIVED REMOTELY)
% Nx: our successor's current successor (RECEIVED REMOTELY)
% Id: our key
% Successor: our successor's PID
% Returns: the pair {Successor, Next}
stabilize(Pred, Id, Successor,Nx) ->
	{Skey, Sref ,Spid} = Successor,
	case Pred of
		nil -> 
			Spid ! {notify,{Id,nil,self()}},
			{Successor,Nx};
		{Id, _,_} -> 
			{Successor,Nx};
		{Skey, _,_} -> 
			Spid ! {notify,{Id,nil,self()}},
			{Successor,Nx};
		{Xkey,_,Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true -> 
					Xpid ! {request,self()},
					drop(Sref),
					Xref = monitor(Xpid),
					{{Xkey, Xref,Xpid},Successor};
				false -> 
					Spid ! {notify,{Id,nil,self()}},
					{Successor,Nx}
			end
	end.

schedule_stabilize() -> timer:send_interval(?Stabilize, self(), stabilize).

monitor(Pid) -> erlang:monitor(process, Pid).
drop(nil) -> ok;
drop(Pid) -> erlang:demonitor(Pid, [flush]).


down(Ref, {_, Ref, _}, Successor, Next) -> {nil,Successor,Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, _,Npid}) ->
	self() ! stabilize,
	Nref = monitor(Npid),
	{Predecessor, {Nkey, Nref, Npid}, nil}.
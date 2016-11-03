-module(node1).
-compile(export_all).
-define(Stabilize,1000).
-define(Timeout,5000).

start(Id) -> start(Id, nil).

start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor).

connect(Id, nil) -> {ok, {Id, self()}};
connect(_, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok, {Skey,Peer}}
		after 
			?Timeout -> io:format("Time out: no response~n",[])
	end.

node(Id, Predecessor, Successor) ->
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor);
		{notify, New} ->
			Pred = notify(New, Id, Predecessor),
			node(Id, Pred, Successor);
		{request, Peer} ->
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor);
		{status, Pred} ->
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ);
		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor);
		state ->
	    	io:format(' Id : ~w~n Predecessor : ~w~n Successor : ~w~n', [Id, Predecessor, Successor]),
	    	node(Id, Predecessor, Successor);
	    _ ->
	    	io:format('Strange message received'),
	    	node(Id, Predecessor, Successor)
	end.

create_probe(Id,{_,Spid}) -> 
	Spid ! {probe,Id,[Id],erlang:system_time(micro_seconds)}.

forward_probe(Ref,T,Nodes,Id,{_,Spid}) ->
	Spid ! {probe,Ref,Nodes++[Id],T}.

remove_probe(T,Nodes) ->
    Duration = erlang:system_time(micro_seconds)-T,
    io:format("--------PROBE--------~n"),
    io:format("Nodes:"),
    Printer = fun(E) -> io:format("~p ",[E]) end,
    lists:foreach(Printer,Nodes),
    io:format("~n Time = ~p~n",[Duration]),
    io:format("---------------------~n").


notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil -> 
			{Nkey, Npid};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					{Nkey,Npid};
				false ->
					Predecessor
			end
	end.

request(Peer, Predecessor) ->
	case Predecessor of
		nil -> 
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.

stabilize({_, Spid}) -> Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil -> 
			Spid ! {notify,{Id,self()}},
			Successor;
		{Id, _} -> 
			Successor;
		{Skey, _} -> 
			Spid ! {notify,{Id,self()}},
			Successor;
		{Xkey, Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true -> 
					Xpid ! {request,self()},
					{Xkey, Xpid};
				false -> 
					Spid ! {notify,{Id,self()}},
					Successor
			end
	end.

schedule_stabilize() -> timer:send_interval(?Stabilize, self(), stabilize).
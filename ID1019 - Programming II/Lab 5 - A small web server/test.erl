%% @author konstantin
%% @doc @todo Add description to test.


-module(test).
-compile(export_all).

bench(Host, Port) ->
	Start = now(),
	run(5, Host, Port,self()),
	Finish = collect(5),
	timer:now_diff(Finish, Start).
	
	
collect(Number) ->
	if Number == 0 -> now();
		true ->  receive 
		 		 ok -> collect(Number-1) end 	
	 end.

run(N, Host, Port, Pid) ->
	if N == 0 -> ok;
	true -> spawn(fun() -> request_loop(Host, Port,100, Pid) end), run(N-1, Host, Port, Pid)
	end.

request_loop(Host, Port, Count, Pid) ->
	if Count == 0 -> Pid ! ok;
	   true -> request(Host, Port),
			   request_loop(Host, Port, Count-1, Pid)
	end.

request(Host, Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	{ok, Server} = gen_tcp:connect(Host, Port, Opt),
	gen_tcp:send(Server, http:get("foo")),
	Recv = gen_tcp:recv(Server, 0),
	case Recv of
		{ok, _} -> ok;
		{error, Error} -> io:format("test: error: ~w~n", [Error])
	end,
	gen_tcp:close(Server).


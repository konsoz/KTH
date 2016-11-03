	-module(logger).
-export([start/1, stop/1]).

start(Nodes) -> spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) -> 
	loop(time:clock(Nodes),[]).


loop(Clock,Queue) ->
	receive
		{log, From, Time, Msg} ->
			NewClock = time:update(From,Time,Clock),
			NewQueue = lists:keysort(2,Queue ++ [{From,Time,Msg}]),
			{SafeToLog, RemainingQueue} = lists:splitwith(fun({_, T, _}) -> time:safe(T,Clock) end, NewQueue),
			lists:foreach(fun(LogEntry) -> log(LogEntry) end, SafeToLog),
			loop(NewClock,RemainingQueue);
	stop -> ok 
	end.


log({From, Time, Msg}) -> io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
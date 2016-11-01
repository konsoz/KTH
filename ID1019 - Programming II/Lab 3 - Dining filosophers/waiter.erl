
-module(waiter).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, request/1, return/1]).

start()->
	spawn_link(fun() -> allavailable() end).

request(Waiter)->
	Waiter ! {request, self()},
	receive
		ok->ok;
		no->no
	end.

return(Waiter) ->
	Waiter ! {return, self()}, ok.

allavailable()->
	receive
		{request, Sender} -> Sender ! ok, oneavailable()
	end.

oneavailable()->
	receive
		{request, Sender} -> Sender ! ok, nonavailable();
		{return, _Sender} -> allavailable()
	end.

nonavailable() ->
	receive
		{request, Sender} -> Sender ! no, nonavailable();
		{return, _Sender} -> oneavailable()
	end.
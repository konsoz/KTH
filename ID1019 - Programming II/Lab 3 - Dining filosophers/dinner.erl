%% @author konstantin
%% @doc @todo Add description to dinner.


-module(dinner).

-compile(export_all).


start() -> spawn(fun() -> {Time, _} = timer:tc(dinner, init, []), io:fwrite("TID SYNKRONT 5 GÅNGER ÄTANDE: ~w~n",[Time]) end).

init() ->
	
		Waiter = waiter:start(),
		C1 = chopstick:start(),
		C2 = chopstick:start(),
		C3 = chopstick:start(),
		C4 = chopstick:start(),
		C5 = chopstick:start(),
		Ctrl = self(),
		
		
	philosopher:start(5, C1, C2, "Arendt", Ctrl, Waiter),
	philosopher:start(5, C2, C3, "Hypatia", Ctrl,Waiter),
	philosopher:start(5, C3, C4, "Simone", Ctrl,Waiter),
	philosopher:start(5, C4, C5, "Elizabeth", Ctrl,Waiter),
	philosopher:start(5, C5, C1, "Ayn", Ctrl,Waiter),
				

		
		wait(5, [C1, C2, C3, C4, C5]),
		io:format("EASY MAN").


wait(0, Chopsticks) -> lists:foreach(fun(C) -> chopstick:quit(C) end, Chopsticks);
wait(N, Chopsticks) ->
				receive
				done -> wait(N-1, Chopsticks);
				abort -> exit(abort)
				end.


benchmark() -> 
			 %start(),
			 {Time, Procid} = timer:tc(dinner, start, []),
			 io:fwrite("TID SYNKRONT 5 GÅNGER ÄTANDE: ~w~n",[Time]).


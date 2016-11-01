%% @author konstantin
%% @doc @todo Add description to philosofer.


-module(philosopher).

-compile(export_all).

start(Hungry,Right,Left,Name,Ctrl,Waiter) -> spawn_link(fun() -> think(Hungry,Right,Left,Name,Ctrl,Waiter) end).


think(0,_,_,Name,Ctrl,Waiter) ->
		  % io:format("~s is done eating~n", [Name]),
					  Ctrl ! done;
think(Hungry,Right,Left,Name,Ctrl,Waiter) ->
			 % io:format("~s is thinking~n", [Name]),
 			 sleep(50,100),
			 pick(Hungry,Right,Left,Name,Ctrl,Waiter).


pick(Hungry,Right,Left,Name,Ctrl,Waiter) ->
			 
	%Timeout = 100,
	
	case waiter:request(Waiter) of
		no -> % io:format("~s was not allowed to eat~n", [Name]),
			  think(Hungry,Right,Left,Name,Ctrl,Waiter);
	    ok -> % io:format("~s received the chopsticks~n", [Name]),
			  eat(Hungry,Right,Left,Name,Ctrl,Waiter)
	    end.
	
%% 	case chopstick:request(Left, Right, Timeout) of
%% 			ok -> % io:format("~s received the chopsticks~n", [Name]),
%% 				  eat(Hungry,Right,Left,Name,Ctrl,Waiter);
%% 			no -> % io:format("~s waited too long~n", [Name]),
%% 				  think(Hungry,Right,Left,Name,Ctrl,Waiter)
%% 	end.
%% 	
	
	
	
%%     case chopstick:request(Left, Timeout) of
%% 	  	no -> think(Hungry,Right,Left,Name,Ctrl,Waiter);
%%   		ok -> % io:format("~s received a left chopstick~n", [Name]),
%%        			
%% 			  case chopstick:request(Right, Timeout) of
%% 				  no -> case chopstick:return(Left) of
%% 							ok -> % io:format("~s returned a left chopstick~n", [Name]) ,
%% 								 think(Hungry,Right,Left,Name,Ctrl,Waiter);
%% 							_ -> error
%% 						end;
%% 			  
%% 				  ok -> % io:format("~s received a right chopstick~n", [Name]), 
%% 						eat(Hungry,Right,Left,Name,Ctrl,Waiter)
%% 			  end
%%     end.

return(Hungry,Right,Left,Name,Ctrl,Waiter) -> 

  	waiter:return(Waiter),
%% 	% io:format("~s returned chopsticks~n", [Name]),
	
%%   chopstick:return(Left),
%%   % io:format("~s returned a left chopstick~n", [Name]),
%%   chopstick:return(Right),
%%   % io:format("~s returned a right chopstick~n", [Name]),
%%   % io:format("~s will eat ~w more times~n", [Name, Hungry]),
  
  think(Hungry-1,Right,Left,Name,Ctrl,Waiter).


eat(Hungry,Right,Left,Name,Ctrl,Waiter) ->

  % io:format("~s is eating~n", [Name]),
  sleep(50,100),
  return(Hungry,Right,Left,Name,Ctrl,Waiter).
  

sleep(T,D) -> random:seed(now()) , timer:sleep(T + random:uniform(D)).



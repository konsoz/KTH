%% @author konstantin
%% @doc @todo Add description to chopstick.


-module(chopstick).

-export([start/0,return/1,quit/1, request/3]).

start() -> spawn_link(fun() -> available() end).

available() ->
	receive
			{request, Sender} -> 
				Sender ! {granted,self()}, 
				flush(),
				gone();
			quit -> ok
	end.

gone() ->
	receive
			{return,Sender} ->
				Sender ! {returned,self()},
				flush(),
				available();
			quit -> ok
	end.

%% request(Stick, Timeout) ->
%%  Stick ! {request,self()},
%%   receive
%%    {granted,_} -> ok
%%    after Timeout -> no
%%  end.

request(Left,Right,Timeout) ->
				Left ! {request,self()},
				Right ! {request,self()},
				receive
				{granted,Stick1} -> 
								 receive
									 {granted,_Stick2} -> ok
								 	  after Timeout -> Stick1 ! {return, self()}, no
								 end
				after Timeout -> no
				end.


return(Stick) -> Stick ! {return,self()},
				 receive 
					 {returned,_Senders} -> ok
				 end.



		
quit(Stick) -> Stick ! quit. 


flush() ->
        receive
                _ -> flush()
        after
                0 -> ok
        end.





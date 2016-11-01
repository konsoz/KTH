-module(server).

-export([start/7]).

%%% The start/7 process will start a mandel server and a print process

start(Width, Height, X, Y, K, Depth, File) ->
    {ok, spawn(fun() -> init(Width, Height, X, Y, K, Depth, File) end)}.

init(Width, Height, X, Y, K, Depth, File) ->
    {ok, Ctrl} = print:start(File, Width, Height),
    Trans = fun(W, H) -> {X + K*(W-1), Y-K*(H-1)} end,
    rows(Width, Height, Trans, Depth, Ctrl).


rows( _, 0, _, _, _)->
    done();
rows(W, H, Tr, Depth, Ctrl) ->
    receive 
	{request, From} ->
	    From ! {task, W, H, Tr, Depth, Ctrl},
	    rows(W, H-1, Tr, Depth, Ctrl);
	stop ->
	    ok
    end.
    
done() ->	    
    receive 
	{request, From} ->
	    From ! done,
	    done();
	stop ->
	    ok
    end.
    



    
    










		  
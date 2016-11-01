%% @author konstantin
%% @doc @todo Add description to env.


-module(env).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

new() -> [].

add(Id,Str,Env) -> [{Id,Str}|Env].
	

lookup(Id,[{Id,Str}|T]) -> {Id,Str};
lookup(Id,[{_,_}|T]) -> lookup(Id,T);
lookup(Id,[]) -> false.



%% ====================================================================
%% Internal functions
%% ====================================================================



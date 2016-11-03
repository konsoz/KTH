%%@author konstantin

-module(map).
-compile(export_all).

%Return empty map
new() -> [].

%Update the map with an new entry, the old entry is removed
update(Node, Links, Map) ->
    case lists:keysearch(Node, 1, Map) of
	{value, {_, _}} ->
	    [{Node, Links}|lists:keydelete(Node, 1, Map)];
	false ->
	    [{Node, Links}|Map]
    end.

%Return all nodes directly connected to Node
reachable(_,[]) -> [];
reachable(Node,Map) ->
	case lists:keyfind(Node,1,Map) of
		{_,Reachable} -> Reachable;
		false -> []
	end.

%Return all nodes
all_nodes([]) -> [];
all_nodes(Map) -> 
	Result = all_nodes2(Map,[]),
	Set = sets:from_list(Result),
    sets:to_list(Set).

all_nodes2([],Result) -> Result;
all_nodes2([{Node,[Link|Links]}|Rest],Result) -> all_nodes2([{Node,Links}|Rest],[Link|Result]);
all_nodes2([{Node,[]}|Rest],Result) -> all_nodes2(Rest,[Node|Result]).


	


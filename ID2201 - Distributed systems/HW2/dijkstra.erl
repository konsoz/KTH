%%@author konstantin

-module(dijkstra).
-compile(export_all).


% Returns the length of the shortest path to the node or 0 if the node is not found
entry(Node, Nodes) ->
    case lists:keysearch(Node, 1, Nodes) of
    {value, {Node, M, _}} ->
        M;
    false  ->
        0
end.


% Replaces the entry or Node in Sorted with a new entry having a new length N and Gateway. 
% The resulting list should be sorted. 
replace(Node,N,Gateway,Sorted) -> lists:keysort(2,lists:keyreplace(Node,1,Sorted,{Node,N,Gateway})).

% Update the list Sorted given the information that Node can be reached in N hops using Gateway.
% If no entry is found then no new entry is added.
% Only if we have a shorter path should we replace the existing entry.
update(Node, Gw, N, Nodes) ->
    M =  entry(Node, Nodes),
    if 
    N < M -> replace(Node, N, Gw, Nodes);
    true -> Nodes
    end.


%% • If there are no more entries in the sorted list then we are done and the
%% given routing table is complete.
%% • If the first entry is a dummy entry with an infinite path to a city we
%% know that the rest of the sorted list is also of infinite length and the
%% given routing table is complete.
%% • Otherwise, take the first entry in the sorted list, find the nodes in the
%% map reachable from this entry and for each of these nodes update the
%% Sorted list. The entry that you took from the sorted list is added to
%% the routing table.
iterate([], _, Table) ->
    Table;
iterate([{_Node, inf, _}|_], _Map, Table) ->
    Table;
iterate([{Node, N, Gw}|Nodes], Map, Table) ->
    Reachable = map:reachable(Node, Map),
    Updated = lists:foldl(fun(Nd, Acc) -> 
             update(Nd, Gw, N+1, Acc) end, Nodes, Reachable),
    iterate(Updated, Map, [{Node, Gw}|Table]).

%% First construct a initial sorted list with dummy entries with the length of infinity (Indirect Nodes)
%% Second construct gateways with length 0 and path set to itself
table(Gws, Map) ->
    Nodes = map:all_nodes(Map),
    Rest = lists:filter(fun (X) -> not lists:member(X, Gws) end, Nodes),
    Direct = lists:map(fun (Nd) -> {Nd,0,Nd} end, Gws),
    Indirect = lists:map(fun (Nd) -> {Nd,inf,na} end, Rest),
    Sorted = lists:append(Direct, Indirect),
    iterate(Sorted, Map, []).



route(Node,Table) ->
    case lists:keyfind(Node,1,Table) of
        {Node,Gateway} -> {ok,Gateway};
        false -> notfound
    end.

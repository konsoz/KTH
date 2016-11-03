
-module(hist).
-export([new/1, update/3]).


new(Name) ->
    D = dict:new(),
    dict:append(Name, inf, D).

update(Node, N, History) ->
    case dict:find(Node, History) of
        {ok,[Value|_]} ->
            if
                N > Value ->
                    Dict1 = dict:erase(Node, History),
                    Dict2 = dict:append(Node, N, Dict1),
                    {new, Dict2};
                true ->
                    old
            end;
        error ->
            {new, dict:append(Node, N, History)}
    end.

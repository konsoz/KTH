-module(storage).
-compile(export_all).



create() -> [].

add(Key,Value,Store) -> lists:append([{Key,Value}],Store).

lookup(Key,Store) -> 
	case lists:keyfind(Key,1,Store) of
		{Key,Value} -> 
			{Key,Value};
		false -> 
			false
	end.

split(From,To,Store) -> lists:partition(fun({K,_}) -> key:between(K, From, To) end, Store).

merge(Entries,Store) -> lists:append(Entries,Store).
-module(key).
-compile(export_all).

generate() -> rand:uniform(1000000000).

between(Key,From,To) -> 
	if 
		From =:= To -> 
			true;
		true ->  
	if 
		From < To ->
			if (Key > From) and (Key =< To) ->
				true;
			true -> false
			end;
		From > To ->
			if (Key > From) or (Key =< To) ->
				true;
			true -> false
			end
	end
end.


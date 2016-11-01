%% @author konstantin
%% @doc @todo Add description to hello_world.


-module(huffman).


%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

sample() -> "the quick brown fox jumps over the lazy dog
this is a sample text that we will use when we build
up a table we will only handle lower case letters and
no punctuation symbols the frequency will of course not
represent english but it is probably not that far off".

text() -> "this is something that we should encode".
test() ->
		Sample = sample(),
		Table = table(Sample),
		Text = text(),
		Seq = encode(Text, Table),
		Text = decode(Seq, Table).



%% ====================================================================
%% Encode
%% ====================================================================

encode(Text, Table) -> encode(Text,Table,[]).
encode([],Table,Seq) -> Seq; 
encode([Char|Rest],Table,Seq) -> encode(Rest,Table,Seq ++ lookup(Char,Table)).
								
lookup(Char, [{Char, Code}|_]) -> Code;
lookup(Char, [{_,Code}|T]) -> lookup(Char,T);
lookup(_,[]) -> false.


%% ====================================================================
%% Decode
%% ====================================================================

decode([], _Table) -> [];
decode(Seq, Table) -> {Char, Rest} = decode_char(Seq, 1, Table), [Char|decode(Rest, Table)].
decode_char(Seq, N, Table) ->
							{Code, Rest} = lists:split(N, Seq),
							case lists:keyfind(Code, 2, Table) of
							{Char,_} -> {Char,Rest} ;
							false -> decode_char(Seq, N+1, Table)
							end.




%% ====================================================================
%% Frequency
%% ====================================================================

						
freq(Sample) -> freq(Sample,[]).
freq([], Freq) ->  Freq;
freq([Char|Rest], Freq) -> 
					     Member = member(Char,Freq),
						 if Member == false -> freq(Rest,  [{Char, count([Char|Rest],Char)} | Freq]);
						    true -> freq(Rest,Freq)
						 end.

%% ====================================================================
%% Tree					 				
%% ====================================================================


table(Sample) -> 
				Freq = freq(Sample),
				Tree = huffman(Freq),
				codes(Tree).

huffman(Freq) -> huffman_tree(lists:keysort(3, huffman_leafs(Freq))).
huffman_leafs([]) -> [];
huffman_leafs([{Char,Freq}|T]) -> [{leaf,Char,Freq,na,na}|huffman_leafs(T)].

huffman_tree([H|[]]) -> H;
huffman_tree([First,Second|T]) ->
								{_,_,Freq1,_,_} = First,
								{_,_,Freq2,_,_} = Second,
								Node = {node,na,Freq1+Freq2,First,Second},
								Z = lists:keysort(3,[Node|T]),
								huffman_tree(Z).

codes(Tree) -> codes(Tree,[],[]).
codes({node,_,_,Left,Right},Path,Res) ->
									     Z = codes(Left,Path ++[0],Res),
											 codes(Right,Path ++ [1],Z);
codes({leaf,Char,_,_,_},Path,Res) -> Res ++ [{Char,Path}].
									   
									   
									   
	
%% ====================================================================
%% Utils
%% ====================================================================

member(X, [{X, _}|_]) -> true;
member(X, [{_,_}|T]) -> member(X,T);
member(_,[]) -> false.

count([H|T], X) -> if H == X -> if T == [] -> 1;
								true -> 1+count(T,X)
								end;
				 true -> if T == [] -> 0;
						true -> count(T,X)
						end
				 end.
				   
append(A, B) ->
			case A of
			[] -> B;
			[H|T] -> [H | append(T,B)]
			end.


	
%% ====================================================================
%% Benchmark
%% ====================================================================

alphabet_1() ->  Alphabet_1 = "asdfghjk". 
alphabet_2() ->  Alphabet_2 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".
alphabet_3() ->  Alphabet_3 = "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~".
alphabet_4() ->  Alphabet_4 = "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".
alphabet_5() -> unicode:characters_to_list(alphabet_3(), utf16).

benchmark(Text) -> 
			 Table = table(Text),
			 {TimeEnc,Seq} = timer:tc(huffman, encode,[Text,Table]),
			 io:fwrite("TID FÖR ENCODING: ~B~n",[TimeEnc]),
			 {TimeDec,_} = timer:tc(huffman, decode,[Seq,Table]),
			 io:fwrite("TID FÖR DECODING: ~B~n",[TimeDec]).
			 



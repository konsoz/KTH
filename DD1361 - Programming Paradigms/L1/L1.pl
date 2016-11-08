%Konstantin Sozinov

% 1. Fibonacci-talen
% fib(+N,?F)

% Fact - fibonacci number of 0 is 0
fib(0, 0).
% Rule - Y is the nth fibonacci number if X > 0 and fib(X,Y,_)
fib(X, Y) :- X > 0, fib(X, Y, _).	 

% Fact - fibonacci number of 1 is 1
fib(1, 1, 0).
% Rule - Res is nth fibonacci number if X > 1 and fib(X1,Acc,Y3) 
% where X1 is lower number in the sequence
fib(X, Res, Acc) :-
	X > 1,
	X1 is X - 1,
	fib(X1, Acc, Y3),
	Res is Acc + Y3.




% 2. Rövarspråket
% rovarsprak(?Text,?RövarText)

% Fact - rövarspråk of the empty list is empty list
rovarsprak([],[]).
% Rule - True if H is not a vocal and rovarsprak(T,Rest)
% I.e. catches a case when H is not a vocal 
rovarsprak([H|T],[H,111,H|Rest]) :- \+ vowel(H), rovarsprak(T,Rest).
% Rule - True if H is a vocal and rovarsprak(T,Rest)
% I.e. catches a case when H is a vocal
rovarsprak([H|T],[H|Rest]) :- vowel(H), rovarsprak(T,Rest). 

vowel(97). 
vowel(101). 
vowel(105).
vowel(111).
vowel(117).
vowel(121).

% 3. Medellängd

% medellangd(+Text,?AvgLen)

medellangd(Text, AvgLen) :-
	%% Create a list with every element as a one words length 
	splitText(Text,WordsLens,0),
	% Calculate amount of charachters
	sum_list(WordsLens,Sum),
	% Calculate amount of words
	length(WordsLens,Len),
	% Calculate AvgLen
	AvgLen is Sum / Len.

% splitText(+Text,-WordsLens,-WordLen)

% Fact - Length of empty list of words is a empty list of lengths 
splitText([],[],0).
% Fact - List of words length when have only one word left  
splitText([],[WordLen],WordLen) :- WordLen > 0.
% Rule - If H is alpha then add 1 to word length and take care of the rest
splitText([H|T],WordLens, WordLen) :- 
	char_type(H,alpha), 
	NewWordLen is WordLen + 1,
	splitText(T,WordLens,NewWordLen).
% Rule - If H is not alpha and WordLen is 0 -> it is a non alphabetic charachter	
splitText([H|T],WordLens,0) :- 
	\+ char_type(H,alpha), 
	splitText(T,WordLens,0).
% Rule if H is not alpha and WordLen is anything else then 0 -> a word is over, continue with rest of the text	
splitText([H|T],[WordLen|WordLens],WordLen) :- 
	WordLen > 0,
	\+char_type(H,alpha), 
	splitText(T,WordLens,0).

% 4. Skyffla
% skyffla(+List,?Shuffled)


skyffla([],[]) :- !.
skyffla(List,Shuffled) :-
	% Shuffle input list 
	skyffla(List,Temp,Shuffled1),
	% Do the same with temp 
	skyffla(Temp, Shuffled2),
	% Append every list to one list
	append(Shuffled1,Shuffled2,Shuffled).

% skyffla(+List,+Temp,?Shuffled)
% Fact - Shuffled list of a empty list is a empty list
skyffla([],[],[]).
% Rule - Add X2 to temp list and X1 to the result list and check the rest of the input list
skyffla([X1,X2|Xs],[X2|Temp],[X1|Result]) :- skyffla(Xs,Temp,Result).
% Rule - Add X to result list when only one element in input list left
skyffla([X],[],[X]). 

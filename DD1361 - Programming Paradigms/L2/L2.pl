% Konstantin Sozinov

% spider(?X)

spider(X) :-
	person(X),
	% Find all people in the network
	setof(P,person(P),Persons),
	% Delete the spider
	delete(Persons,X,PersonsWithOutSpider),
	% Find possible spiders
	find_spider(X,PersonsWithOutSpider).

find_spider(X,PersonsWithOutSpider) :-
	% Find all persons who spider knows (1st condition)
	setof(Z,knows_both(X,Z),SpiderFreinds),
	% Build list of possible conspirators with X as a spider (2nd condition)
	subsets(SpiderFreinds,[],PossibleConspirators),
	% Check if other persons (not spider or conspirator) knows some conspirator 
	not_knows_anyone_outside(PersonsWithOutSpider,PossibleConspirators), !.


% not_knows_anyone_outside(+Persons,+Conspirators)

% Base fact, if the list of persons is empty then it is true
not_knows_anyone_outside([],_).
% If P is a member of conspirators, then just do nothing and check the rest 
not_knows_anyone_outside([P|Persons],Conspirators) :-
	member(P,Conspirators),
	not_knows_anyone_outside(Persons,Conspirators).
% If P is not a member of conspirators (other person), 
% check if he knows someone in conspirators list.	
not_knows_anyone_outside([P|Persons],Conspirators) :-
	\+ member(P,Conspirators),
	knows_someone_in_list(P,Conspirators),
	not_knows_anyone_outside(Persons,Conspirators).

% subsets(+Perons, +PossibleConspirators, ?Result)
% This predicate will build possible conspirators list from
% the list of persons who spider knows

% Base fact, we are done when persons list is empty
subsets([],Conspirators, Conspirators).
% If P not knows someone in PossibleConspirators list then
% P can be a conspirator
subsets([P|Persons], PossibleConspirators,Result):-
	\+ knows_someone_in_list(P,PossibleConspirators),
	subsets(Persons, [P|PossibleConspirators],Result).
% Otherwise P cannot be a conspirator, then check other persons	
subsets([_|Persons], PossibleConspirators, Result):-
	subsets(Persons, PossibleConspirators,Result).

% knows_someone_in_list(+Person,+Conspirators)
% Check if a person knows someone in the list

% Base fact, if the list of conspirators is empty, 
% this means person did not know someone in the list
knows_someone_in_list(_,[]) :- false.
% Rule, if a person knows c, we are done, cut other solutions
knows_someone_in_list(Person,[C|Conspirators]) :- knows_both(Person,C), !.
% Rule, if a person did not know c, check the rest of conspirators
knows_someone_in_list(Person, [_|Conspirators]) :- 
	knows_someone_in_list(Person,Conspirators).


% knows_both(+Person1,+Person2)
% Check if Person1 knows Person2 and other way around
knows_both(P1,P2) :- knows(P1,P2).
knows_both(P1,P2) :- knows(P2,P1).

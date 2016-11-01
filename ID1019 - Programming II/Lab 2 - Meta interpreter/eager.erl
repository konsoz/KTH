%% @author konstantin
%% @doc @todo Add description to eager.


-module(eager).

%% ====================================================================
%%  Evaluate expressions
%% ====================================================================
-compile(export_all).


eval_expr({atm, Id}, _) -> {ok, Id};
eval_expr({var, Id}, Env) ->
							case env:lookup(Id, Env) of
							{Id,Str} -> {ok,Str};
							false -> error
							end;
eval_expr({cons, Head, Tail}, Env) ->
							case eval_expr(Head,Env) of
							error -> error;
							{ok, Id} -> 
										case eval_expr(Tail,Env) of
									    error -> error;
										{ok, Rest} -> {ok, {Id,Rest}}
										end
							end;
eval_expr({switch,Exp,Clauses},Env) -> 
									   case eval_expr(Exp,Env) of
									   error -> error;
									   {ok,Str} -> eval_clause(Clauses,Env,Str)
									   end.
										   
eval_clause([{clause,Ptr,Seq}|T],Env,Str) ->										   
									      case eval_match(Ptr,Str,Env) of 
									      fail -> eval_clause(T,Env,Str);		
								          {ok,NewEnv} -> eval_seq(Seq,NewEnv)	   
									      end;
eval_clause([],_,_) -> error.


%% ====================================================================
%%  Pattern mattching
%% ====================================================================

eval_match(ignore, _, Env) -> {ok, Env};
eval_match({atm, Id},Id,Env) -> {ok, Env};
eval_match({var, Id},Str,Env) ->
								  case env:lookup(Id, Env) of
								  false -> {ok, env:add(Id,Str,Env)};
								  {Id, Str} -> {ok, Env};
								  {Id, _} -> fail
								  end;
eval_match({cons, Head, Tail}, {First,Rest}, Env) ->
								         			case eval_match(Head, First , Env) of
										 			{ok, NewEnv} -> eval_match(Tail, Rest, NewEnv);
										 			fail -> fail
                                         			end;
eval_match(_, _, _) -> fail.


%% ====================================================================
%% Sequences evaluation 
%% ====================================================================

eval_seq(Seq) -> eval_seq(Seq,[]).
eval_seq([Exp], Env) -> eval_expr(Exp, Env); %dafuq?
eval_seq([{match, Ptr, Exp}|Seq], Env) ->
										  case eval_expr(Exp, Env) of   %kolla om strukture finns?
										  error -> error; 
										  {ok, Str} ->
													  case eval_match(Ptr, Str, Env) of  %om strukture finns, patternmacha?
													  fail -> error;
													  {ok, NewEnv} -> eval_seq(Seq, NewEnv) 
													  end
										  end.


















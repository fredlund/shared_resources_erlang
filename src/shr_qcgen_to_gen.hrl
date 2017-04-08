initial_state(Args,_) ->
  initial_state(Args).

command(State,_CorrState) ->
  ?LET(Command,command(State),
       begin
	 ?LOG("command/2 generated ~p~n",[Command]),
	 {call,_,F,Args} = Command,
	 case F of
	   nop -> [];
	   _ -> case Args of
		  [Element|RestArgs] ->
		    [{Element,{F,RestArgs}}];
		  _ ->
		    io:format("*** Error: malformed command ~p~n",[Command]),
		    error(badarg)
		end
	 end
       end).

next_state(State,Result,Commands,_CorrState) ->
  ?LOG("Commands are ~p~n",[Commands]),
  case Commands of
    [] ->
      State;
    [{_,{F,Args}}] ->
      next_state(State,Result,{call,?MODULE,F,Args})
  end.

precondition(State,Commands,_CorrState) ->
  ?LOG("precondition: Commands=~p~n",[Commands]),
  Result =
    case Commands of
      [] ->
	true;
      [{_,{F,Args}}] ->
	Call = {call,?MODULE,F,Args},
	precondition(State,Call)
    end,
  ?LOG("precondition returns ~p~n",[Result]),
  Result.

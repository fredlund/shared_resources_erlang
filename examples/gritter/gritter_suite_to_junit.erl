-module(gritter_suite_to_junit).

-export([junit_tests/2,junit_tests/3,junit_tests/4]).
-export([initial_translator_state/1, junit_build_state/5, junit_output/3]).

-record(state,
	{
	  started    %% Started testing?
	  ,options    %% Testing options
	  ,test_gen_state
	  ,test_corr_state
	  ,test_gen_module
	  ,test_corr_module
	  ,completion_time
	  ,controller_ports
	  ,environment_ports
	  ,start_fun
	  ,stop_fun
	  ,jobs_alive
	  ,counter
	}).

-record(gstate,
	{
	  max_uids  %% number of uids to use
	  ,blocked   %% uids blocked (a set)
	  ,following %% which uids are followed by an uid (uid -> set(uid))
	  ,resends   %% uid accepts resends from uid ({uid,uid} -> bool)
	  ,num_msgs_to_read %% number of remaining messages to read for uid
	  ,msgs_present     %% messages remaining to read for uid
	  ,msgs_maybe_present %% messages possibly remaining to read for uid
	}).

-record(tr_state,
	{
	  blocked_at
	  ,unblocked_at
	  ,return_values
	  ,exceptions
	}).

-record(command,
	{
	  raw,
	  calltype=controller,   %% controller or environment
	  actor=undefined,      %% integer or undefined
	  call,       %% {F,Args}
	  options=[]     %% list of values
	}).

junit_tests(SuiteFile,File) ->
  junit_tests(SuiteFile,File,0).

junit_tests(SuiteFile,File,InitCounter) ->
  junit_tests(SuiteFile,File,InitCounter,false).

junit_tests(SuiteFile,File,InitCounter,TestsAreOptional) ->
  {ok,B} = file:read_file(SuiteFile),
  TestSuite = binary_to_term(B),
  {ok,F} = file:open(File++".java",[write]),
  io:format
    (F,
     "import sequenceTester.*;~n"++
       "import org.junit.Test;~n"++
       "import org.junit.Assert.*;~n"++
       "import java.util.HashSet;~n"++
       "import java.util.Random;~n"++
       "import java.util.Set;~n"++
       "import es.upm.babel.cclib.Tryer;~n"++
       "~n",
     []),
  io:format
    (F,
     "public abstract class ~s {~n~n"++
       "abstract Object startController();~n~n",
     [File]),
  GetInit = fun (State) -> State#state.test_gen_state end,
  case TestSuite of
    {_, Tests} ->
      put(counter,InitCounter),
      lists:foreach
	(fun ([_MaxUsers,RawTest]) -> 
	     {NormalTest,InitState} = 
	       test_to_junit:dynamic_test_to_test(RawTest,GetInit),
	     Test =
	       test_to_gritter_test(NormalTest),
	     TestName = 
	       "test"++
	       integer_to_list(get(counter))++
	       if TestsAreOptional -> "_optional"; true -> "" end,
	     io:format("converted test case:~n~p~n",[Test]),
	     io:format
	       (F,
		"~n@Test~npublic void ~s() {~nCallSeq seq1 = new CallSeq(\"~s\",~n"++
		  "startController(),~n",
		[TestName,TestName]),
	     Result = 
	       test_to_junit:translate(Test,gritter_gnr,?MODULE,void,InitState),
	     io:format
	       (F,
		"~s); ~nseq1.run();~n}~n",
		[Result]),
	     put(counter,get(counter)+1);
	     (Other) ->
	     io:format("*** Warning: malformed test:~n~p~n",[Other])
	 end, Tests)
  end,
  io:format(F,"~n}~n",[]),
  file:close(F).

test_to_gritter_test([]) ->
  [];
test_to_gritter_test([{call,_,start,_}|Rest]) ->
  test_to_gritter_test(Rest);
test_to_gritter_test([{call,_,do_cmds,[Cmd]}|Rest]) ->
  [[{Cmd#command.actor,Cmd#command.call}]|test_to_gritter_test(Rest)].

non_empty_test([]) -> false;
non_empty_test([{_,_,{call,_,F,_}}|Rest]) ->
  lists:member(F,[seguir,dejarDeSeguir,enviar,leer]) orelse non_empty_test(Rest);
non_empty_test([Other|_]) ->
  io:format("Strange test:~n~p~n",[Other]),
  throw(bad).

initial_translator_state(_) ->
  {ok,#tr_state{blocked_at=[],return_values=[],unblocked_at=[],exceptions=[]}}.

junit_build_state(Call={Name,_},N,State,NewState,TranslateState) ->
  case lists:member(Name,[seguir,dejarDeSeguir,enviar,leer]) of
    true ->
      ShouldRaiseException =
	case Call of
	  {seguir,[Uid,Uid,_]} ->
	    true;
	  {dejarDeSeguir,[Uid1,Uid2]} ->
	    not(sets:is_element(Uid2,following(Uid1,State)));
	  _ ->
	    false
	end,
      {ok,
       compute_new_translateState
	 (Call,N,State,NewState,TranslateState,ShouldRaiseException)};
    false ->
      false
  end.

junit_output(Call,N,TranslateState) ->
  case Call of
    {seguir,[Uid1,Uid2,Regritos]} ->
      "new Call(new Seguir("++integer_to_list(Uid1)++","++integer_to_list(Uid2)++","++
	atom_to_list(Regritos)++"), "++
	return_or_exception(N,TranslateState)++", "++
	"Block.unblocks(true))";

    {dejarDeSeguir,[Uid1,Uid2]} ->
      "new Call(new DejarDeSeguir("++integer_to_list(Uid1)++","++integer_to_list(Uid2)++"), "++
	return_or_exception(N,TranslateState)++", "++
	"Block.unblocks(true))";

    {enviar,[Uid,Msg,Regrito]} ->
      "new Call(new Enviar("++integer_to_list(Uid)++",\""++Msg++"\","++
	atom_to_list(Regrito)++"), "++
	return_or_exception(N,TranslateState)++", "++
	translate_blocking_check(N,TranslateState)++"))";

    {leer,[Uid]} ->
      "new Call(new Leer("++integer_to_list(Uid)++"), "++
	(case lists:keyfind(N,1,TranslateState#tr_state.return_values) of
	   false ->
	     "Return.noCheck(), ";
	   {_,ReturnValues} ->
	     lists:foldl
	       (fun (Value,Acc) ->
		    Acc++","++io_lib:format("\"~s\"",[Value])
		end,
		"Return.returns(true",
		ReturnValues)++"),"
	 end)++
	translate_blocking_check(N,TranslateState)++"))\n"
  end.

possible_reads(Uid,State) ->
  sets:to_list
    (sets:union(msgs_present(Uid,State),msgs_maybe_present(Uid,State))).

return_or_exception(N,TS) ->
  case lists:keyfind(N,1,TS#tr_state.exceptions) of
    {_,true} -> 
      "Return.returns(false,(new GestorGritter.PreViolationSharedResourceException()).getClass())";
    _ ->
      "Return.shouldReturn(true)"
  end.

compute_new_translateState(Call,N,State,NewState,
			   #tr_state
			   {blocked_at=BlockedAt,
			    unblocked_at=UnblockedAt,
			    return_values=ReturnValues,
			    exceptions=Exceptions}=TranslateState,
			   ShouldRaiseException) ->
  Uid =
    uid(Call),
  New = 
    sets:subtract(NewState#gstate.blocked,State#gstate.blocked),
  Finished = 
    sets:subtract(State#gstate.blocked,NewState#gstate.blocked),
  io:format
    ("at call ~p: New=~p Finished=~p~n",
     [Call,sets:to_list(New),sets:to_list(Finished)]),
  NewBlockedAt =
    case sets:size(New)>0 of
      true -> [{N,Uid}|BlockedAt];
      false -> BlockedAt
    end,
  BlockTimes =
    lists:map
      (fun (Uid2) -> find_block_time(Uid2,BlockedAt) end, 
       sets:to_list(Finished)),
  NewUnblockedAt =
    case sets:size(Finished)>0 of
      true -> 
	lists:keystore(N,1,UnblockedAt,{N,BlockTimes});
      false ->
	UnblockedAt
    end,
  ReturnValue = 
    case Call of
      {enviar,[_,Msg|_]} -> Msg;
      _ -> void
    end,
  NewReturnValues1 = 
    lists:foldl
      (fun (BlockTime,RV) ->
	   lists:keystore(BlockTime,1,RV,{BlockTime,[ReturnValue]})
       end, ReturnValues, BlockTimes),
  NewReturnValues = 
    case sets:size(New)==0 of
      true -> 
	PossibleReturnValues = possible_reads(uid(Call),State),
	lists:keystore(N,1,NewReturnValues1,{N,PossibleReturnValues});
      false ->
	NewReturnValues1
    end,
  NewExceptions =
    lists:keystore(N,1,Exceptions,{N,ShouldRaiseException}),
  io:format
    ("Finished=~w BlockTimes=~w NewUnblockedAt=~w~nReturnValues=~w~n",
     [sets:to_list(Finished),BlockTimes,NewUnblockedAt,NewReturnValues]),
  TranslateState#tr_state
    {blocked_at=NewBlockedAt,
     unblocked_at=NewUnblockedAt,
     exceptions=NewExceptions,
     return_values=NewReturnValues}.

uid({_,[Uid|_]}) ->
  Uid.

find_block_time(Uid,[{N,Uid}|_]) ->
  N;
find_block_time(Uid,[_|Rest]) ->
  find_block_time(Uid,Rest).

translate_blocking_check(N,TranslateState) ->
  Blocks =
    case lists:keyfind(N,1,TranslateState#tr_state.blocked_at) of
      false -> false;
      _ -> true
    end,
  Finished = 
    case lists:keyfind(N,1,TranslateState#tr_state.unblocked_at) of
      false -> [];
      {_,Fs} -> Fs
    end,
  lists:foldl
    (fun (BlockTime,Acc) -> Acc++","++integer_to_list(BlockTime) end,
     if
       Blocks ->
	 "Block.unblocks(false";
       true ->
	 "Block.unblocks(true"
     end,
     Finished).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

following(Id,State) ->
  case lists:keyfind(Id,1,State#gstate.following) of
    false -> sets:new();
    {_,Set} -> Set
  end.
  
msgs_maybe_present(Uid,State) ->
  case lists:keyfind(Uid,1,State#gstate.msgs_maybe_present) of
    false -> sets:new();
    {_,Value} -> Value
  end.

msgs_present(Uid,State) ->
  case lists:keyfind(Uid,1,State#gstate.msgs_present) of
    false -> sets:new();
    {_,Value} -> Value
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

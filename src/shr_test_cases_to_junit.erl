-module(shr_test_cases_to_junit).

-record(state,{file,prefix,counter,callrep,classname,indent,marshaller}).

-include("tester.hrl").

%%-define(debug,true).
-include("debug.hrl").

-export([gen_junit_tests/9]).
-export([symbVar/1]).



gen_junit_tests(ClassName,TestCases,Prefix,CallRep,Orderer,Marshaller,ConfigDescFun,ControllerArgFun,CheckerClassConstructorFun) ->
  FileName = ClassName++".java",
  {ok,File} = file:open(FileName,[write]),
  State =
    #state
    {
      classname=ClassName,
      file=File,prefix=Prefix,counter=1,
      callrep=CallRep,
      indent=indent_len(1),
      marshaller=Marshaller
    },
  OrderedTestCases = Orderer(TestCases),
  gen_junit_tests(OrderedTestCases,ConfigDescFun,ControllerArgFun,CheckerClassConstructorFun,State).

shr_dir() ->
  filename:dirname(code:which(?MODULE)).

shr_priv_dir() ->
  shr_dir()++"/../priv".
  
gen_junit_tests([],_ConfigDescFun,_ControllerArgFun,CheckerClassConstructorFun,State) ->
  ok = file:close(State#state.file);
gen_junit_tests([TC|Rest],ConfigDescFun,ControllerArgFun,CheckerClassConstructorFun,State) ->
  TestCase = TC#test_case.test_case,
  BasicTestCase = shr_test_jobs:basic_test_case(TestCase),
  SimpleTestCase =
    lists:map 
      (fun (Cmds) ->  
	   [Jobs,_,_,_] = element(4,Cmds),
	   Jobs
       end, BasicTestCase),
  ?LOG("~n~nWill generate code for test case:~n~p~n~n",[SimpleTestCase]),
  DataSpec = shr_test_jobs:test_data_spec(TestCase),
  WaitingSpec = shr_test_jobs:test_waiting_spec(TestCase),
  GenModule = shr_test_jobs:gen_module(TestCase),
  GenState = shr_test_jobs:initial_gen_state(TestCase),
  {Info,InitialState} = 
    shr_step_resource:initial_state(DataSpec,WaitingSpec,GenModule,GenState,[]),
  try shr_step_resource:repeat_step(SimpleTestCase,InitialState,Info) of
      StateSpace -> output_test_case(TestCase,StateSpace,ConfigDescFun,ControllerArgFun,CheckerClassConstructorFun,State)
  catch throw:not_deterministic ->
      io:format("*** Warning: test case is not deterministic.~n"),
      shr_test_jobs:print_test_case(TC)
  end,
  gen_junit_tests(Rest,ConfigDescFun,ControllerArgFun,CheckerClassConstructorFun,State#state{counter=State#state.counter+1}).


statesUnblocks({branching,{_,StatesUnblocks,_,_}}) ->
  StatesUnblocks.

remaining_commands({branching,{_,_,_,Cmds}}) ->
  Cmds.

output_test_case(TestCase,StateSpace,ConfigDescFun,ControllerArgFun,CheckerClassConstructorFun,State) ->
  Name = io_lib:format("test_~s_~p",[State#state.prefix,State#state.counter]),
  I1 = indent_len(1),
  I2 = indent_len(2),
  CheckerStr =
    if
      CheckerClassConstructorFun=/=undefined ->
        "UnitTest.installChecker(new "++CheckerClassConstructorFun(TestCase)++");";
      true ->
        ""
    end,
  io:format
    (State#state.file,
     indent(I1,"@Test")++
       indent(I1,"@DisplayName(\""++Name++"\")")++
       indent(I1,"public void ~s(TestInfo testInfo) {")++
       indent(I2,CheckerStr)++
       indent(I2,"")++
       indent(I2,"UnitTest t =")++
       indent(I2+2,"new UnitTest")++
       indent(I2+2,"(")++
       indent(I2+3,"testInfo.getDisplayName(),")++
       indent(I2+3,ConfigDescFun(TestCase)++",")++
       indent(I2+3,"startController("++ControllerArgFun(TestCase)++"),"),
     [Name]),
  io:format
    (State#state.file,
     "~s",
     [output_state_space(StateSpace,State#state{indent=I2+3})]),
  io:format
    (State#state.file,
     indent(I2+2,");")++
       indent(I2,"Assertions.assertTimeoutPreemptively(Duration.ofSeconds(30), () -> { t.run(); });")++
       nl(I1,"}"),
     []).

extract_sequence({State,[]}) ->
  {sequence,[],State};
extract_sequence({State,[{Transition,Next}]}) ->
  case extract_sequence(Next) of
    {sequence,Sequence,FinalState} ->
      {sequence,[Transition|Sequence],FinalState};
    {branching_point,Sequence,BranchingCalls,BranchingTransitions} ->
      {branching_point,[Transition|Sequence],BranchingCalls,BranchingTransitions}
  end;
extract_sequence({State,Transitions}) ->
  ?LOG("Nondeterminism: ~p =>~n~p~n",[State,Transitions]),
  [{Transition,_}|_] = Transitions,
  {branching_point,[],Transition#transition.calls,Transitions}.

output_state_space(StateSpace,State) ->
  ExtSequence = extract_sequence(StateSpace),
  case ExtSequence of
    {sequence,Sequence,FinalState} ->
      output_sequence_final
	(Sequence,nil,State);
    {branching_point,Sequence,BranchingCalls,BranchingTransitions} ->
      NewBranchingTransitions =	
	rename_jobs(BranchingCalls,BranchingTransitions),
      output_sequence_final
	(Sequence,{BranchingCalls,NewBranchingTransitions},State)
  end.

rename_jobs(Calls,Transitions) ->
  Min = min_job(Calls),
  rename_jobs(Min,void,Transitions).

rename_jobs(_,_,[]) ->
  [];
rename_jobs(MinJob,MaxJob,[Transition|Rest]) ->
  NewTransition = do_rename(MinJob,MaxJob,Transition),
  NewMaxJob = max(MinJob-1,max_job(NewTransition)),
  ?LOG
    ("renamed~n~p~nusing ~p and ~p yielded~n~p~n",
     [Transition,MinJob,MaxJob,NewTransition]),
  ?LOG
    ("min and max job for~n~p~nis ~p and ~p~n",
     [NewTransition,MinJob,NewMaxJob]),
  [NewTransition|rename_jobs(MinJob,NewMaxJob,Rest)].
    
do_rename(_,void,Transition) ->
  Transition;
do_rename(MinJob,MaxJob,Transition) ->
  do_rename_1(MinJob,MaxJob,Transition).

do_rename_1(MinJob,MaxJob,Job) when is_record(Job,job) ->
  Pid = Job#job.pid,
  if
    Pid >= MinJob ->  
      NewPid = Pid-MinJob+MaxJob+1,
      Job#job{pid=NewPid};
    true ->
      Job
  end;
do_rename_1(MinJob,MaxJob,T) when is_tuple(T) ->
  list_to_tuple(do_rename_1(MinJob,MaxJob,tuple_to_list(T)));
do_rename_1(MinJob,MaxJob,[Hd|Tl]) ->
  [do_rename_1(MinJob,MaxJob,Hd)|do_rename_1(MinJob,MaxJob,Tl)];
do_rename_1(_,_,T) ->
  T.

min_job([First|Rest]) ->
  FirstJobPid = First#job.pid,
  lists:foldl(fun (Job,M) -> max_job_call(Job,M) end, FirstJobPid, Rest)+1.

max_job(Transition) ->
  max_job(Transition,void).
max_job(Job,Max) when is_record(Job,job) ->
  Pid = Job#job.pid,
  if
    (Max==void) or (Pid>Max) ->
      Pid;
    true ->
      Max
  end;
max_job(T,Max) when is_tuple(T) ->
  max_job(tuple_to_list(T),Max);
max_job([Hd|Tl],Max) ->
  max_job(Tl,max_job(Hd,Max));
max_job(_,Max) ->
  Max.

max_job_call(Job,Max) ->
  Pid = Job#job.pid,
  if
    (Max==void) or (Pid>Max) ->
      Pid;
    true ->
      Max
  end.

indent(State) when is_record(State,state) ->
  lists:duplicate(State#state.indent,$ );
indent(N) when is_integer(N) ->
  lists:duplicate(N,$ ).

indent(N,S) when is_integer(N), N>=0, is_list(S) ->
  "\n"++indent(N)++S.

nl(N,S) when is_integer(N), N>=0, is_list(S) ->
  "\n"++indent(N)++S++"\n".

indent_len(N) ->
  N*2.

output_sequence_final(Sequence,Final,State) ->
  I = State#state.indent,
  if
    Sequence=/=[] ->
      io_lib:format
	(indent(I,"Util.sequenceEndsWith")++
	   indent(I,"(new TestCall[] {")++
	   output_sequence(Sequence,Final,State#state{indent=I+2})++
	   indent(I+1,"},")++
	   "~s"++
	   indent(I,")"),
	 [output_final(Final,State#state{indent=I+1})]);
    true ->
      output_final(Final,State#state{indent=I})
  end.

output_sequence(Items,Final,State) ->
  combine
    (lists:map
       (fun (Item) ->
	    I = State#state.indent,
	    Calls = Item#transition.calls,
            FailedPres = Item#transition.failed_pres,
            Returns = Item#transition.returns,
	    Unblocked = Item#transition.unblocked,
	    case Calls of
	      [Call] ->
		CallRep = (State#state.callrep)(Call),
                ?LOG
                  ("one call ~p~ncallrep=~s returns=~p~nfailed_pres=~p~nUnblocked=~p~n",
                   [Call, CallRep, Returns, FailedPres, Unblocked]),
		UnblocksCall = 
		  (lists:keyfind(Call#job.pid,#job.pid,Unblocked)=/=false)
                  orelse
                  (lists:keyfind(Call#job.pid,#job.pid,FailedPres)=/=false),
                CallRepReturn =
                  CallRep
                  ++(case oracle(Call,Returns,FailedPres,State) of
                       "" -> "";
                       Other -> ".o("++Other++")"
                     end)
                  ++".n(\""++symbVar(Call#job.pid)++"\")",
		Unblocks_non_locally =
		  lists:keydelete(Call#job.pid,#job.pid,Unblocked),
		Unblocks =
		  unblocks(Unblocks_non_locally,Returns,State),
		if
		  UnblocksCall ->
		    ?LOG
		      ("unblocked(~s)~ntransition=~p~n",[CallRepReturn,Item]),
		    io_lib:format
		      (indent(I,"TestCall.unblocks(~s~s)"),
		       [CallRepReturn,pre(",",Unblocks)]);
		  true ->
		    io_lib:format
		      (indent(I,"TestCall.blocks(~s~s)"),
		       [CallRepReturn,pre(",",Unblocks)])
		end;
	      [_|_] ->
		io_lib:format
		  (indent(I,"TestCall.must")++
		     indent(I,"(")++
		     "~s,"++
		     indent(I+1,"~s")++
		     indent(I,")"),
		   [make_calls(Calls,State#state{indent=I+1}),
		    unblocks(Unblocked,Returns,State)])
	    end
	end, Items),
     ",").

pre(_,"") ->
  "";
pre(Str,Continuation) ->
  Str++Continuation.

combine([],_) ->
  "";
combine([Item],_) ->
  Item;
combine([Item|Rest],Combinator) ->
  Item++Combinator++combine(Rest,Combinator).

make_calls(Calls,State) ->
  I = State#state.indent,
  CallsString =
    combine
      (lists:map
	 (fun (Call) ->
	      CallRep = (State#state.callrep)(Call),
	      io_lib:format
		(indent(I+1,"~s.n(\"~s\")"),
		 [CallRep,symbVar(Call#job.pid)])
	  end, Calls),
       ","),
  io_lib:format
    (indent(I,"new Call[] {")++
       "~s"++
       indent(I,"}"),
     [CallsString]).

unblocks(Calls,Returns,State) ->
  NeedPairs = 
    lists:any
      (fun (Call) ->
           case shr_utils:find(fun ({Job,_,_}) -> Job#job.pid==Call#job.pid end, 
                     Returns) of
             {_,ReturnValue,_} -> true;
             _ -> false
           end
       end, Calls),
  ?LOG("needPairs=~p~n",[NeedPairs]),
  if
    NeedPairs ->
      "Arrays.asList("++
      lists:foldl
        (fun (UnblockedCall,Acc) ->
             RightElement = 
               case oracle(UnblockedCall,Returns,[],State) of
                 "" -> "Check.returns()";
                 Other -> Other
               end,
             UnblocksComma = if Acc=="" -> ""; true -> "," end,
             "new Pair<String,Oracle>(\""++symbVar(UnblockedCall#job.pid)++"\","++RightElement++")"++UnblocksComma++Acc
         end, "", Calls)++")";
    true ->
      lists:foldl
        (fun (UnblockedCall,Acc) ->
             UnblocksComma = if Acc=="" -> ""; true -> "," end,
             "\""++symbVar(UnblockedCall#job.pid)++"\""++UnblocksComma++Acc
         end, "", Calls)
  end.

oracle(Call,Returns,FailedPres,State) ->
  case shr_utils:find(fun ({Job,_,_}) -> Job#job.pid==Call#job.pid end, Returns) of
    {_,ReturnValue,Checker} ->
      ?LOG("Checker is ~p~n",[Checker]),
      case ReturnValue of
        _ when Checker=/=undefined ->
          "Check.lambda(xyz -> "++shr_symb:printSeqExpr(Checker)++")";
        {var,_} ->
          "";
        _ ->
          "Check.returns("++marshall_term(ReturnValue,State)++")"
      end;
    false ->
      case lists:keyfind(Call#job.pid,#job.pid,FailedPres) of
        false ->
          "";
        _ ->
          "Check.raisesException(precondFailedExc)"
      end
  end.

output_final(FinalState,State) ->
  I = State#state.indent,
  case FinalState of
    nil ->
      indent(I,"new Nil()");
    {BranchingCalls,Transitions} ->
      CallsString =
	make_calls(BranchingCalls,State#state{indent=I+1}),
      AlternativesString =
	combine
	  (lists:map
	     (fun ({Transition,Continuation}) ->
		  ?LOG("Alternative transition is~n~p~n",[Transition]),
                  Returns = Transition#transition.returns,
		  AltUnblocks = Transition#transition.unblocked,
		  indent(I+1,"Alternative.alternative(")++
		    output_state_space(Continuation,State#state{indent=I+2})++
		    ","++
		    indent(I+2,unblocks(AltUnblocks,Returns,State))++
		    ")"
	      end, Transitions),
	   ","),
      io_lib:format
	(indent(I,"Branches.branches")++
	   indent(I,"(")++
	   "~s,~s"++
	   indent(I,")"),
	 [CallsString,AlternativesString])
  end.

symbVar(Id ) ->      
  io_lib:format("call_~p",[Id]).

copy_file(From,State) ->
  case file:open(From,[read]) of
    {ok,FromFile} -> 
      copy_file1(FromFile,State),
      ok = file:close(FromFile);
    {error,Reason} ->
      io:format
	("failed to open file ~p for reading due to ~p~n",
	 [From,Reason]),
      throw(bad)
  end.

copy_file1(From,State) ->
  case file:read_line(From) of
    {ok,Data} ->
      io:format(State#state.file,"~s",[Data]),
      copy_file1(From,State);
    eof ->
      ok
  end.
  
marshall_term(Term,State) ->
  if
    State#state.marshaller=/=undefined ->
      (State#state.marshaller)(Term);
    true ->
      Term
  end.
  



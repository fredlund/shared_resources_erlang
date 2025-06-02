-module(shr_test_cases_to_junit).

-record(state,{file,prefix,counter,callrep,classname,indent,marshaller,data_module}).

-include("tester.hrl").

-define(debug,true).
-include("debug.hrl").

-export([gen_junit_tests/8]).
-export([symbVar/1]).



gen_junit_tests(ClassName,TestCases,Prefix,CallRep,Orderer,Marshaller,ConfigDescFun,ControllerArgFun) ->
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
  gen_junit_tests(OrderedTestCases,ConfigDescFun,ControllerArgFun,State).

shr_dir() ->
  filename:dirname(code:which(?MODULE)).

shr_priv_dir() ->
  shr_dir()++"/../priv".
  
gen_junit_tests([],_ConfigDescFun,_ControllerArgFun,State) ->
  ok = file:close(State#state.file);
gen_junit_tests([TC|Rest],ConfigDescFun,ControllerArgFun,State) ->
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
  DataModule = shr_utils:module(DataSpec),
  GenState = shr_test_jobs:initial_gen_state(TestCase),
  {Info,InitialState} = 
    shr_step_resource:initial_state(DataSpec,WaitingSpec,GenModule,GenState,[]),
  NewState = State#state{data_module=DataModule},
  try shr_step_resource:repeat_step(SimpleTestCase,InitialState,Info) of
      StateSpace -> output_test_case(TestCase,StateSpace,ConfigDescFun,ControllerArgFun,NewState)
  catch throw:not_deterministic ->
      io:format("*** Warning: test case is not deterministic.~n"),
      shr_test_jobs:print_test_case(TC)
  end,
  gen_junit_tests(Rest,ConfigDescFun,ControllerArgFun,NewState#state{counter=State#state.counter+1}).


statesUnblocks({branching,{_,StatesUnblocks,_,_}}) ->
  StatesUnblocks.

remaining_commands({branching,{_,_,_,Cmds}}) ->
  Cmds.

output_test_case(TestCase,StateSpace,ConfigDescFun,ControllerArgFun,State) ->
  Controller = ControllerArgFun(TestCase),
  Name = io_lib:format("test_~s_~p",[State#state.prefix,State#state.counter]),
  I1 = indent_len(1),
  I2 = indent_len(2),
  io:format
    (State#state.file,
     indent(I1,"@Test")++
     indent(I1,"public void ~s() {")++
     "~s~s"++indent(I1)++"}~n",
     [Name,indent(I2,Controller),output_state_space(StateSpace,State#state{indent=I2})]).

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
	(output_sequence(Sequence,Final,State#state{indent=I+1})++"\n~s~n",
	 [output_final(Final,State#state{indent=I+1})]);
    true ->
      output_final(Final,State#state{indent=I})
  end.

output_sequence(Items,Final,State) ->
  lists:append(lists:map
                 (fun (Item) ->
                      output_transition(State,Item)
                  end, Items)).

output_transition(State,Transition) ->
  I = State#state.indent,
  Calls = Transition#transition.calls,
  FailedPres = Transition#transition.failed_pres,
  Unblocked = Transition#transition.unblocked,
  EndState = Transition#transition.endstate,
  case Calls of
    [Call] ->
      CallRep = 
        (State#state.callrep)(Call),
      ?LOG
         ("one call ~p~ncallrep=~s failed_pres=~p~nUnblocked=~p~nendstate=~p~n",
          [Call, CallRep, FailedPres, Unblocked,EndState]),
      UnblocksCall = 
        (lists:keyfind(Call#job.pid,#job.pid,Unblocked)=/=false)
        orelse
          (lists:keyfind(Call#job.pid,#job.pid,FailedPres)=/=false),
      Var = 
        symbVar(Call#job.pid),
      ?LOG("Call=~p~n",[Call#job.call]),
      ?LOG("DataModule=~p~n",[State#state.data_module]),
      Type =
        case Call#job.call of
          {_,Operation,_} ->
            try (State#state.data_module):return_type(Operation)
            catch _:_ -> "?"
            end;
          _ -> 
            "?"
        end,
      Decl =
        "Call<"++Type++"> "++Var,
      DeclAndCall = 
        Decl ++ " = " ++ CallRep,
      Unblocks_non_locally =
        lists:keydelete(Call#job.pid,#job.pid,Unblocked),
      Unblocks =
        unblocks(Unblocks_non_locally),
      CallCode =
        if
          UnblocksCall ->
            ?LOG
               ("unblocked(~s)~ntransition=~p~n",[CallRep,Transition]),
            io_lib:format(indent(I,"~s.assertUnblocks(~s);"),[DeclAndCall,Unblocks]);
          true ->
            io_lib:format(indent(I,"~s.assertBlocks(~s);"),[DeclAndCall,Unblocks])
        end,
      ReturnCodes = output_call_returns(I,Transition),
      CallCode++if ReturnCodes =/= ""-> "\n"++ReturnCodes; true -> "" end;
    [_|_] ->
      io_lib:format
        (indent(I,"TestCall.must")++
           indent(I,"(")++
           "~s,"++
           indent(I+1,"~s")++
           indent(I,")"),
         [make_calls(Calls,State#state{indent=I+1}),
          unblocks(Unblocked)])
  end.

output_call_returns(I,Transition) ->
  Returns = Transition#transition.returns,
  Unblocked = Transition#transition.unblocked,
  lists:foldl
    (fun (Call,Acc) -> 
         NVar = 
           symbVar(Call#job.pid),
         NReturnedValue = 
           find_return(Call#job.pid,Returns),
         NReturnCond = 
           find_return_cond(Call#job.pid,Returns),
         NIsVarReturn = 
           case NReturnedValue of {ok,{var,_}} -> true; _ -> false end,
         String =
           case {NReturnedValue,NReturnCond} of
             {{ok,NValue}, {ok,undefined}} when NValue=/=void, not(NIsVarReturn) ->
               io_lib:format(indent(I,"~s.assertReturnsValue(~p);"),[NVar,NValue]);
             {_, {ok,NCond}} when NCond=/=true, NCond=/=undefined ->
               shr_symb:printSeqExpr(NCond);
             _ ->
               ""
           end,
         if Acc =/= "" -> Acc++"\n"++String; true -> String end
     end, "", Unblocked).

find_return(JobId,[]) ->
  false;
find_return(JobId,[{Job,Value,_}|Rest]) ->
  if
    JobId == Job#job.pid ->
      {ok,Value};
    true -> 
      find_return(JobId,Rest)
  end.

find_return_cond(JobId,[]) ->
  false;
find_return_cond(JobId,[{Job,_,Cond}|Rest]) ->
  if
    JobId == Job#job.pid ->
      {ok,Cond};
    true -> 
      find_return_cond(JobId,Rest)
  end.

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

combine_terminate([],_) ->
  "";
combine_terminate([Item],Combinator) ->
  Item++Combinator;
combine_terminate([Item|Rest],Combinator) ->
  Item++Combinator++combine_terminate(Rest,Combinator).

make_calls(Calls,State) ->
  I = State#state.indent,
  CallsString =
    combine_terminate
      (lists:map
	 (fun (Call) ->
        Type =
          case Call#job.call of
            {_,Operation,_} ->
              try (State#state.data_module):return_type(Operation)
              catch _:_ -> "?"
              end;
            _ -> 
              "?"
          end,
        Decl = "Call<"++Type++"> "++symbVar(Call#job.pid),
	      CallRep = (State#state.callrep)(Call),
	      io_lib:format
		(indent(I+1,"~s = ~s"),
		 [Decl,CallRep])
	  end, Calls),
       ";"),
  ExecString = 
    combine
      (lists:map
	 (fun (Call) -> symbVar(Call#job.pid) end, Calls),
       ","),
  Var = "e_"++integer_to_list(newVar()),
  String =
    io_lib:format("~s~n"++indent(I)++"Execute ~s = Execute.exec(~s);~n",
                  [CallsString,Var,ExecString]),
  {Var,String}.

unblocks(Calls) ->
  lists:foldl
    (fun (UnblockedCall,Acc) ->
         UnblocksComma = if Acc=="" -> ""; true -> "," end,
         ""++symbVar(UnblockedCall#job.pid)++""++UnblocksComma++Acc
     end, "", Calls).

callCond(Call,Returns,FailedPres,State) ->
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
          "Check.raisesException(IllegalArgumentException.class)"
      end
  end.

output_final(FinalState,State) ->
  I = State#state.indent,
  case FinalState of
    nil ->
      "";
    {BranchingCalls,Transitions} ->
      {Var,CallsString} =
        make_calls(BranchingCalls,State#state{indent=I+1}),
      AlternativesString =
        combine
          (lists:map
             (fun ({Transition,Continuation}) ->
                  ?LOG("Alternative transition is~n~p~n",[Transition]),
                  AltUnblocks = Transition#transition.unblocked,
                  indent(I+1,"() -> { \n")++
                    indent(I+2, "SeqAssertions.assertUnblocks("++Var++",List.of("++unblocks(AltUnblocks)++"));\n")++
                    output_call_returns(I+2,Transition)++"\n"++
                    output_state_space(Continuation,State#state{indent=I+2}) ++
                    "; }"
              end, Transitions),
           ","),
      io_lib:format
        (indent(I,"~s")
         ++indent(I,"SeqAssertions.checkAlternatives")++
           indent(I,"(")++
           "~s"++
           indent(I,");"),
         [CallsString,AlternativesString])
  end.

%% output_final(FinalState,State) ->
%%   I = State#state.indent,
%%   case FinalState of
%%     nil ->
%%       "";
%%     {BranchingCalls,Transitions} ->
%%       CallsString =
%%         make_calls(BranchingCalls,State#state{indent=I+1}),
%%       AlternativesString =
%%         combine
%%           (lists:map
%%              (fun ({Transition,Continuation}) ->
%%                   ?LOG("Alternative transition is~n~p~n",[Transition]),
%%                   Returns = Transition#transition.returns,
%%                   AltUnblocks = Transition#transition.unblocked,
%%                   indent(I+1,"() -> { assertUnblocks(Arrays.asList(")++
%%                     output_state_space(Continuation,State#state{indent=I+2})++
%%                     indent(I+2,unblocks(AltUnblocks,Returns,State))++
%%                     ")); }"
%%               end, Transitions),
%%            ","),
%%       io_lib:format
%%         (indent(I,"~s")
%%          ++indent(I,"int winner = checkAlternatives")++
%%            indent(I,"(")++
%%            "~s"++
%%            indent(I,");"),
%%          [CallsString,AlternativesString])
%%   end.

symbVar(Id ) ->      
  io_lib:format("call_~p",[Id]).

newVar() ->
  {OldCounter,NewCounter} =
    case erlang:get(var_counter) of
      undefined ->
        {0,1};
      N ->
        {N,N+1}
    end,
  erlang:put(var_counter,NewCounter),
  OldCounter.

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
  



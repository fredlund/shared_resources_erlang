-module(shr_step_resource).

%% A single step semantics for a resource (until a state is stable)

%%-define(debug,true).
-include("debug.hrl").

-include("tester.hrl").

-record(info,
	{
	  data_module :: atom(),
	  wait_module :: atom(),
	  gen_module,
	  options
	}).

-record(state,
	{
	  state,
	  waitstate,
	  genstate,
	  calls,
	  waiting
	}).


-export([initial_state/5,step/3,repeat_step/3]).


initial_state(StateSpec,WaitSpec,GenModule,GenState,Options) ->  
  StateMod = shr_utils:module(StateSpec),
  WaitMod = shr_utils:module(WaitSpec),
  State = shr_utils:initial_state(StateSpec,Options),
  WaitState = shr_utils:initial_state(WaitSpec,[{data_module,StateMod}|Options]),
   {
     #info
     {
       data_module=StateMod, 
       wait_module=WaitMod,
       gen_module=GenModule,
       options=Options
     },
     #state
     {
       state=State, 
       waitstate=WaitState,
       genstate=GenState,
       calls=[],
       waiting=[]
     }
   }.

repeat_step(Commands,State,Info) ->
  repeat_step(Commands,State,Info,1).

repeat_step([],State,_Info,_Counter) ->
  {State,[]};
repeat_step([First|Rest],State,Info,Counter) ->
  ?LOG("Will execute~n~p~nin state~n~p~n",[First,State]),
  Transitions = step(First,State,Info,Counter),
  NewCounter = length(First)+Counter,
  {State,
   lists:map
     (fun (Transition) ->
	  S1 = Transition#transition.endstate,
	  {Transition,repeat_step(Rest,S1,Info,NewCounter)}
      end, Transitions)}.

step(Commands,State,Info) ->
  Transitions = 
    step(Commands,State,Info,1),
  CheckDeterministic = 
    proplists:get_value(check_deterministic,Info#info.options,true),
  if 
    not(CheckDeterministic) ->
      Transitions;
    true ->
      UnblockSet = sets:new(),
      check_unblocks_are_unique(Transitions,UnblockSet)
  end.

check_unblocks_are_unique([],_UnblockSet) -> 
  true;
check_unblocks_are_unique([Transition|Rest],UnblockSet) -> 
  TransitionUnblocks = Transition#transition.unblocked,
  case sets:is_element(TransitionUnblocks,UnblockSet) of
    true ->
      io:format
	("*** Warning: unblock sets are not unique:~n~p and ~p~n",
	 [TransitionUnblocks,sets:to_list(UnblockSet)]),
      throw(not_deterministic);
    false ->
      check_unblocks_are_unique
	(Rest,sets:add_element(TransitionUnblocks,UnblockSet))
  end.

step(Commands,State,Info,OldCounter) ->
  ?LOG("step(~p) in ~p~n",[Commands,State]),

  {JobCalls,_NewCounter} =
    lists:foldl
      (fun (Command, {Acc,Counter}) ->
	   {F,Args} = Command#command.call,
	   Type = Command#command.port,
	   Job = 
	     #job
	     {
	       pid=Counter,
	       call={Type,F,Args},
	       info=Command#command.options,
               symbolicResult={var,Counter}
	     },
	   {[Job|Acc], Counter+1}
       end, {[], OldCounter}, Commands),

  EnabledJobCalls = 
    lists:filter
      (fun (JobCall) -> 
	   (data_module(Info)):pre(shr_call(JobCall),State#state.state) 
       end, JobCalls),

  if EnabledJobCalls=/=[] -> ?LOG("enabled: ~p~n",[EnabledJobCalls]); true -> ok end,

  FailedPres =
    lists:filter
      (fun (JobCall) -> 
	   not((data_module(Info)):pre(shr_call(JobCall),State#state.state))
       end, JobCalls),

  if FailedPres=/=[] -> ?LOG("failed: ~p~n",[FailedPres]); true -> ok end,

  IsExecutable =
    lists:all
      (fun (JobCall) ->
	 Calls = 
	   lists:map
	     (fun (Job) ->
		  {Type,F,Args} = Job#job.call,
		  {Type,{F,Args},Job#job.info}
	      end, JobCalls),
	 case (gen_module(Info)):precondition(State#state.genstate,Calls,void) of
	   true -> true;
	   false ->
	     io:format
	       ("*** Warning: following a command ~p which cannot be completed~n in state~n~p~n",
		[JobCall,State#state.genstate]),
	     false
	 end
       end, JobCalls),
  if
    not(IsExecutable) -> 
      case proplists:get_value(fail_not_executable,Info#info.options,false) of
	true ->
	  throw(not_executable);
	false ->
	  []
      end;
    true ->
      CallState = 
        State#state{waiting=EnabledJobCalls},
      Transition = 
	#transition
        {calls=JobCalls,unblocked=[],returns=[],endstate=CallState,failed_pres=FailedPres},
      NewTransitions = 
        merge_transitions(step(Transition,Info)),
      lists:map
	(fun (NewTransition) ->
	     Result = {EnabledJobCalls,NewTransition#transition.unblocked},
	     NState = NewTransition#transition.endstate,
             Cmds = 
                     lists:map
                     (fun (Job) -> Job#job.call end,
                      NewTransition#transition.calls),
	     NewGenState =
	       (gen_module(Info)):next_state
		 (NState#state.genstate,Result,Cmds,void),
	     NewNState = NState#state{genstate=NewGenState},
	     NewTransition#transition{endstate=NewNState}
	 end, NewTransitions)
  end.

step(Transition,Info) when not(is_list(Transition)) ->
  step([Transition],Info);
step(Transitions,Info) ->
  ?LOG
     ("~n~nOld Transitions=~n~p~n",
      [Transitions]),
  NewTransitions = 
    merge_transitions
      (lists:flatmap
	 (fun (Transition) ->
	      do_step(Transition,Info)
	  end, Transitions)),
  if
    NewTransitions == Transitions ->
      Transitions;
    true ->
      step(NewTransitions,Info)
  end.

do_step(Transition,Info) ->
  ?LOG("do_step(~p)~nwith state ~p~n",[Transition#transition.calls,Transition#transition.endstate]),
  State = Transition#transition.endstate,
  WaitingModule = waiting_module(Info),
  DataModule = data_module(Info),
  AcceptNewTransitions =
    lists:map
      (fun (WaitingCallJob) ->
	   {WaitInfo,NewWaitState} =
	     WaitingModule:new_waiting
	       (shr_call(WaitingCallJob),
		State#state.waitstate,
		State#state.state),
           ?LOG
              ("~p: call ~p with waitstate ~p returned new waitinfo~n ~p and waitstate ~p~n",
               [WaitingModule,shr_call(WaitingCallJob),State#state.waitstate,WaitInfo,NewWaitState]),
	   NewState =
	     State#state
	     {
	       waiting = lists:delete(WaitingCallJob,State#state.waiting),
	       calls = [WaitingCallJob#job{waitinfo=WaitInfo}|State#state.calls],
	       waitstate = NewWaitState
	     },
	   Transition#transition{endstate=NewState}
       end, State#state.waiting),
  ?LOG("accepted:~n~p~n",[AcceptNewTransitions]),
  EnabledCalls =
    lists:filter
      (fun (Job) ->
	   DataModule:cpre(shr_call(Job),State#state.state)
                   andalso WaitingModule:priority_enabled
                             (shr_call(Job),
                              Job#job.waitinfo,
                              State#state.waitstate,
                              State#state.state)
       end, State#state.calls),
  ?LOG("Enabled: ~p~n",[EnabledCalls]),
  CallNewTransitions =
    lists:flatmap
      (fun (Call) ->
	   NewTransition =
	     Transition#transition
	     {unblocked=[Call|Transition#transition.unblocked]},
           SymVar = 
             Call#job.symbolicResult,
	   NewDataStatesAndReturns =
             begin
               ReturnValues = 
                 case lists:member({return_value,2},DataModule:module_info(exports)) of
                   true ->
                     case DataModule:return_value(shr_call(Call),State#state.state) of
                       {'$shr_nondeterministic',RValues} -> RValues;
                       Other -> [Other]
                     end;
                   false ->
                     [SymVar]
                 end,
               lists:flatmap
                 (fun (ReturnValue) ->
                      ?LOG("ReturnValue is ~p of ~p~n",[ReturnValue,shr_call(Call)]),
                      ReturnCheck =
                        case (ReturnValue==SymVar) orelse
                             shr_symb:is_symbolic(ReturnValue) of
                          true ->
                            Check = 
                              try DataModule:return(State#state.state,shr_call(Call),undefined,SymVar) of
                                  Ch ->
                                  ?LOG("Ch is ~p~n",[Ch]),
                                  case shr_symb:is_symbolic(Ch) of
                                    true -> Ch;
                                    false -> undefined
                                  end
                              catch _:_ -> undefined end;
                          false -> undefined
                        end,
                      Returns = {Call,ReturnValue,ReturnCheck},
                      ?LOG("Returns=~p~n",[Returns]),
                      ?LOG("call post(~p)~nin ~p~n",[Call,State#state.state]),
                      try DataModule:post(shr_call(Call),ReturnValue,State#state.state,SymVar) of
                          {'$shr_nondeterministic',NewStates} -> 
                          lists:map(fun (NS) -> {NS,Returns} end, NewStates);
                          NewDataState -> 
                          [{NewDataState,Returns}]
                      catch Error:Reason ->
                          io:format
                            ("~p:post: executing ~p~n with return value ~p in state~n~p~nfailed with ~p:~p~nStacktrace:~n~p~n",
                             [DataModule,shr_call(Call),ReturnValue,State#state.state,Error,Reason,erlang:get_stacktrace()]),
                          error(bad)
                      end
                  end, ReturnValues)
             end,
	   lists:map
	     (fun ({NewDataState,NewReturn}) ->
                  ?LOG("post_waiting(~p) info=~p state=~p~n",[Call,Call#job.waitinfo,State#state.waitstate]),
		  NewWaitState = 
		    WaitingModule:post_waiting
		      (shr_call(Call),Call#job.waitinfo,
		       State#state.waitstate,NewDataState),
		  NewState =
		    State#state
		    {
		      state = NewDataState,
		      waitstate = NewWaitState,
		      calls = lists:delete(Call,State#state.calls)
		    },
		  NewTransition#transition
                    {
                    endstate=NewState,
                    returns=[NewReturn|NewTransition#transition.returns]
                   }
	      end, NewDataStatesAndReturns)
       end, EnabledCalls),
  ?LOG("CallNewStates=~n~p~n",[CallNewTransitions]),
  case merge_transitions(AcceptNewTransitions++CallNewTransitions) of
    [] -> [Transition];
    New -> New
  end.

shr_call(Job) ->
  {_Type,F,Args} = Job#job.call,
  {F,Args}.

merge_transitions(Transitions) ->	   
  lists:usort
    (lists:map
       (fun (Transition) ->
	    State = Transition#transition.endstate,
	    Unblocked = Transition#transition.unblocked,
	    NewState = 
	      State#state
	      {
		waiting = lists:sort(State#state.waiting),
		calls = lists:sort(State#state.calls)
	      },
	    #transition
	      {
	      calls = lists:sort(Transition#transition.calls),
	      endstate = NewState,
	      unblocked = lists:sort(Unblocked),
              failed_pres = lists:sort(Transition#transition.failed_pres),
	      returns = lists:sort(Transition#transition.returns)
	     }
	end, Transitions)).

data_module(Info) ->
  Info#info.data_module.
waiting_module(Info) ->
  Info#info.wait_module.
gen_module(Info) ->
  Info#info.gen_module.



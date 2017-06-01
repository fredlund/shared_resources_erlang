-module(shr_step_resource).

%% A single step semantics for a resource (until a state is stable)

%%-define(debug,true).
-include("debug.hrl").

-include("tester.hrl").

-record(info,
	{
	  data_module :: atom(),
	  wait_module :: atom(),
	  gen_module
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
       gen_module=GenModule
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
  step(Commands,State,Info,1).

step(Commands,State,Info,Counter) ->
  {JobCalls,NewCounter} =
    lists:foldl
      (fun (Command, {Acc,Counter}) ->
	   {F,Args} = Command#command.call,
	   Type = Command#command.port,
	   Job = 
	     #job
	     {
	       pid=Counter,
	       call={Type,F,Args},
	       info=Command#command.options
	     },
	   {[Job|Acc], Counter+1}
       end, {[], Counter}, Commands),
  EnabledJobCalls = 
    lists:filter
      (fun (JobCall) -> 
	   (data_module(Info)):pre(JobCall,State#state.state) 
       end, JobCalls),
  lists:foreach
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
	     throw(not_deterministic)
	 end
     end, JobCalls),
  CallState = 
    State#state{waiting=EnabledJobCalls},
  Transition = 
    #transition{calls=JobCalls,unblocked=[],returns=[],endstate=CallState},
  NewTransitions = merge_transitions(step(Transition,Info)),
  lists:map
    (fun (Transition) ->
	 Result = {EnabledJobCalls,Transition#transition.unblocked},
	 NState = Transition#transition.endstate,
	 NewGenState =
	   (gen_module(Info)):next_state
	     (NState#state.genstate,Result,void,void),
	 NewNState = NState#state{genstate=NewGenState},
	 Transition#transition{endstate=NewNState}
     end, NewTransitions).

step(Transition,Info) when not(is_list(Transition)) ->
  step([Transition],Info);
step(Transitions,Info) ->
  ?LOG
     ("~n~nOld StatesTransitions=~n~p~n",
      [StatesTransitions]),
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
  State = Transition#transition.endstate,
  WaitingModule = waiting_module(Info),
  DataModule = data_module(Info),
  AcceptNewTransitions =
    lists:map
      (fun (WaitingCall) ->
	   {WaitInfo,NewWaitState} =
	     WaitingModule:new_waiting
	       (WaitingCall,
		State#state.waitstate,
		State#state.state),
	   NewState =
	     State#state
	     {
	       waiting = lists:delete(WaitingCall,State#state.waiting),
	       calls = [WaitingCall#job{waitinfo=WaitInfo}|State#state.calls],
	       waitstate = NewWaitState
	     },
	   Transition#transition{endstate=NewState}
       end, State#state.waiting),
  EnabledCalls =
    lists:filter
      (fun (Job) ->
	   DataModule:cpre(shr_call(Job),State#state.state)
       end, State#state.calls),
  CallNewTransitions =
    lists:flatmap
      (fun (Call) ->
	   NewTransition =
	     Transition#transition
	     {unblocked=[Call|Transition#transition.unblocked]},
	   NewDataStates =
	     case DataModule:post(Call#job.call,void,State#state.state) of
	       {'$shr_nondeterministic',NewStates} -> NewStates;
	       NewDataState -> [NewDataState]
	     end,
	   lists:map
	     (fun (NewDataState) ->
		  NewWaitState = 
		    WaitingModule:post_waiting
		      (Call#job.call,Call#job.waitinfo,
		       State#state.waitstate,NewDataState),
		  NewState =
		    State#state
		    {
		      state = NewDataState,
		      waitstate = NewWaitState,
		      calls = lists:delete(Call,State#state.calls)
		    },
		  NewTransition#transition{endstate=NewState}
	      end, NewDataStates)
       end, EnabledCalls),
  ?LOG("CallNewStates=~n~p~n",[CallNewStates]),
  case merge_transitions(AcceptNewTransitions++CallNewTransitions) of
    [] -> [Transition];
    New -> New
  end.

shr_call(Job) ->
  {Type,F,Args} = Job#job.call,
  {F,Args}.

merge_transitions(Transitions) ->	   
  lists:usort
    (lists:map
       (fun (Transition) ->
	    State = Transition#transition.endstate,
	    Unblocked = Transition#transition.unblocked,
	    Returns = lists:keysort(1,Transition#transition.returns),
	    NewState = 
	      State#state
	      {
		waiting = lists:sort(State#state.waiting),
		calls = lists:sort(State#state.calls)
	      },
	    Transition#transition
	      {
	      calls = lists:sort(Transition#transition.calls),
	      endstate = NewState,
	      unblocked = lists:sort(Unblocked),
	      returns = Returns
	     }
	end, Transitions)).

data_module(Info) ->
  Info#info.data_module.
waiting_module(Info) ->
  Info#info.wait_module.
gen_module(Info) ->
  Info#info.gen_module.



-module(shr_step_resource).

%% A single step semantics for a resource (until a state is stable)

%%-define(debug,true).
-include("debug.hrl").

-include("tester.hrl").
%%-include("shr_step.hrl").

-record(info,
	{
	  data_module :: atom(),
	  wait_module :: atom(),
	  gen_module,
	  counter
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

repeat_step(Commands,State,Info) ->
  repeat_step1(Commands,[State],Info).
repeat_step1([],States,_) -> States;
repeat_step1([First|Rest],States,Info) -> 
  repeat_step1
    (Rest,
     lists:flatmap(fun (State) -> step(First,State,Info) end, States),
     Info).

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
       counter=0
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

step(Commands,State,Info) ->
  Counter = Info#info.counter,
  {JobCalls,NewCounter} =
    lists:foldl
      (fun (Command, {Acc,Counter}) ->
	   io:format("Command is ~p~n",[Command]),
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
       end, {[], Info#info.counter}, Commands),
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
  CallState = State#state{waiting=EnabledJobCalls},
  NewStates = merge_states_and_unblocked(step(CallState,Info)),
  ResultingStates =
    lists:map
      (fun ({NState,Unblocked}) ->
	   Result = {EnabledJobCalls,Unblocked},
	   NewGenState =
	     (gen_module(Info)):next_state
	       (NState#state.genstate,Result,void,void),
	   {NState#state{genstate=NewGenState}, Unblocked}
       end, NewStates),
  {
    ResultingStates,
    Info#info{counter=NewCounter}
  }.

step(State,Info) when not(is_list(State)) ->
  step([{State,[]}],Info);
step(StatesUnblocked,Info) ->
  ?LOG
     ("~n~nOld StatesUnblocked=~n~p~n",
      [StatesUnblocked]),
  NewStatesUnblocked = 
    merge_states_and_unblocked
      (lists:flatmap
	 (fun ({State,Unblocked}) ->
	      do_step(State,Unblocked,Info)
	  end, StatesUnblocked)),
  ?LOG
     ("~n~nNewStatesUnblocked=~n~p~n",
      [NewStatesUnblocked]),
  if
    NewStatesUnblocked == StatesUnblocked ->
      StatesUnblocked;
    true ->
      step(NewStatesUnblocked,Info)
  end.

do_step(State,Unblocked,Info) ->
  WaitingModule = waiting_module(Info),
  DataModule = data_module(Info),
  AcceptNewStates =
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
	   {NewState,Unblocked}
       end, State#state.waiting),
  ?LOG("AcceptNewStates=~n~p~n",[AcceptNewStates]),
  EnabledCalls =
    lists:filter
      (fun (Job) ->
	   DataModule:cpre(shr_call(Job),State#state.state)
       end, State#state.calls),
  ?LOG("EnabledCalls=~p~n",[EnabledCalls]),
  CallNewStates =
    lists:flatmap
      (fun (Call) ->
	   NewUnblocked = [Call|Unblocked],
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
		  {NewState,NewUnblocked}
	      end, NewDataStates)
       end, EnabledCalls),
  ?LOG("CallNewStates=~n~p~n",[CallNewStates]),
  case merge_states_and_unblocked(AcceptNewStates++CallNewStates) of
    [] -> [{State,Unblocked}];
    New -> New
  end.

shr_call(Job) ->
  {Type,F,Args} = Job#job.call,
  {F,Args}.

merge_states_and_unblocked(StatesAndUnblocked) ->	   
  Normalized =
    lists:map
      (fun ({State,Unblocked}) ->
	   {
	     State#state
	     {
	       waiting = lists:sort(State#state.waiting),
	       calls = lists:sort(State#state.calls)
	     },
	     lists:sort(Unblocked)
	   }
       end, StatesAndUnblocked),
  lists:usort(Normalized).

data_module(Info) ->
  Info#info.data_module.
waiting_module(Info) ->
  Info#info.wait_module.
gen_module(Info) ->
  Info#info.gen_module.



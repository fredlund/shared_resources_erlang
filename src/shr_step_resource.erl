-module(shr_step_resource).

%% A single step semantics for a resource (until a state is stable)

%%-define(debug,true).
-include("debug.hrl").

-include("shr_step.hrl").

-record(info,
	{
	  data_module :: atom(),
	  wait_module :: atom(),
	  counter
	}).

-record(state,
	{
	  state,
	  waitstate,
	  calls,
	  waiting 
	}).


-export([initial_state/3,step/3]).

initial_state(StateSpec,WaitSpec,Options) ->  
  StateMod = shr_utils:module(StateSpec),
  WaitMod = shr_utils:module(WaitSpec),
  State = shr_utils:initial_state(StateSpec,Options),
  WaitState = shr_utils:initial_state(WaitSpec,[{data_module,StateMod}|Options]),
   {
     #info
     {
       data_module=StateMod, 
       wait_module=WaitMod,
       counter=0
     },
     #state
     {
       state=State, 
       waitstate=WaitState,
       calls=[],
       waiting=[]
     }
   }.

bigstep(CallSequence,State,Info) ->
  bigstep(CallSequence,State,Info,[],1).

bigstep([],State,_Info,History,_Counter) -> 
  {sequence,{State,lists:reverse(History)}};
bigstep([RawCalls|Rest],State,Info,History,Counter) ->
  {Calls,NewCounter} = 
    lists:foldl
      (fun (RawCall,{AccCalls,AccCounter}) ->
	   case RawCall of
	     {_F,_Args} -> 
	       {[#call{call=RawCall,id=AccCounter}|AccCalls],AccCounter+1}
	   end
       end, {[],Counter}, RawCalls),
  case step(Calls,State,Info) of
    [{NewState,Unblocked}] -> 
      HistoryItem = #history_item{calls=Calls,unblocked=Unblocked},
      bigstep(Rest,NewState,Info,[HistoryItem|History],NewCounter);
    Other=[_|_] -> 
      {branching,{Calls,Other,lists:reverse(History),Rest}}
  end.

step(Calls,States,Info) when is_list(States) ->
  merge_states_and_unblocked
    (lists:flatmap (fun (State) -> step(Calls,State,Info) end, States));
step(PreCalls,State,Info) ->
  ?LOG("step(calls=~p~nstate=~p~n",[PreCalls,State]),
  Calls = 
    if
      is_list(PreCalls) ->
	PreCalls;
      true ->
	[PreCalls]
    end,
  EnabledCalls = 
    lists:filter
      (fun (Call) -> 
	   (data_module(Info)):pre(Call,State#state.state) 
       end, Calls),
  step(State#state{waiting=EnabledCalls},Info).

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
	       calls = [WaitingCall#call{waitinfo=WaitInfo}|State#state.calls],
	       waitstate = NewWaitState
	     },
	   {NewState,Unblocked}
       end, State#state.waiting),
  ?LOG("AcceptNewStates=~n~p~n",[AcceptNewStates]),
  EnabledCalls =
    lists:filter
      (fun (Call) ->
	   DataModule:cpre(Call#call.call,State#state.state)
       end, State#state.calls),
  ?LOG("EnabledCalls=~p~n",[EnabledCalls]),
  CallNewStates =
    lists:flatmap
      (fun (Call) ->
	   NewUnblocked = [Call|Unblocked],
	   NewDataStates =
	     case DataModule:post(Call#call.call,void,State#state.state) of
	       {'$shr_nondeterministic',NewStates} -> NewStates;
	       NewDataState -> [NewDataState]
	     end,
	   lists:map
	     (fun (NewDataState) ->
		  NewWaitState = 
		    WaitingModule:post_waiting
		      (Call#call.call,Call#call.waitinfo,
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



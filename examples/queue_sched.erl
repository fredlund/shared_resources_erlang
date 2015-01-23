%% A queue scheduler that preserves ordering for continously enabled processes

-module(queue_sched).

-behaviour(resource_wait_implementation).

-export([init/2,new_waiting/3,priority_enabled/4,post_waiting/4]).

-record(state,{statemodule,counter,enabled,not_enabled,time}).

init([StateMod],_) ->
  #state{statemodule=StateMod,counter=0,enabled=[],not_enabled=[],time=0}.

new_waiting(Call,State,DataState) ->
  Time = State#state.time,
  Counter = State#state.counter,
  Item = {Counter,Time,Call},
  NewState =
    case (State#state.statemodule):cpre(Call,DataState) of
      true -> State#state{enabled=State#state.enabled++[Item]};
      false -> State#state{not_enabled=[Item|State#state.not_enabled]}
    end,
  {Counter,NewState#state{counter=Counter+1,time=Time+1}}.

priority_enabled(_Call,CallCounter,State,_DataState) ->
  case State#state.enabled of
    [{_,Time,_}|_] -> find_at_time(CallCounter,Time,State#state.enabled);
    _ -> false
  end.

post_waiting(_Call,CallCounter,State,DataState) ->
  Time =
    State#state.time,
  OldEnabled =
    lists:keydelete(CallCounter,1,State#state.enabled),
  {StillEnabled,NewNotEnabled} =
    analyze_enabledness(OldEnabled,DataState,State#state.statemodule),
  {NewEnabled0,StillNotEnabled} =
    analyze_enabledness(State#state.not_enabled,DataState,State#state.statemodule),
  NewEnabled =
    lists:map(fun ({CallCounter0,_,Call0}) -> {CallCounter0,Time,Call0} end, NewEnabled0),
  State#state
    {enabled=StillEnabled++NewEnabled,not_enabled=StillNotEnabled++NewNotEnabled,time=Time+1}.

find_at_time(CallCounter,Time,Enabled) ->
  case Enabled of
    [{CallCounter,Time,_}|_] ->
      true;
    [{_,Time,_}|Rest] ->
      find_at_time(CallCounter,Time,Rest);
    _ ->
      false
  end.

analyze_enabledness(Items,DataState,StateMod) ->
  lists:foldl
    (fun (Item={_,_,Call},{E,N}) ->
	 case StateMod:cpre(Call,DataState) of
	   true -> {[Item|E],N};
	   false -> {E,[Item|N]}
	 end
     end, {[],[]}, Items).


	 
	 
    


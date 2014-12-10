%% A queue scheduler that preserves ordering for continously enabled processes

-module(queue_sched).

-behaviour(resource_wait_implementation).

-export([init/1,new_waiting/3,priority_enabled/4,post_waiting/4]).

init([StateMod,_N,_MaxWeight]) ->
  {ok,{StateMod,0,{[],[]}}}.

new_waiting(Call,{StateMod,Counter,{Queue,NonEnabled}},DataState) ->
  {Counter,
   {StateMod,
    Counter+1,
    remove_non_enabled
      (Queue++NonEnabled++[{Counter,Call}],DataState,StateMod)}}.

priority_enabled(_Call,CallCounter,{_,_,{[{CallCounter,_}|_],_}},_) ->
  true;
priority_enabled(_,_,_,_) ->
  false.

post_waiting(_Call,CallInfo,{StateMod,Counter,{Queue,NonEnabled}},DataState) ->
  NewQueue = lists:keydelete(CallInfo,1,Queue),
  {StateMod,Counter,
   remove_non_enabled(NewQueue++NonEnabled,DataState,StateMod)}.

remove_non_enabled(Queue,DataState,StateMod) ->
  Enabled =
    lists:filter
      (fun ({_,Call}) -> StateMod:cpre(Call,DataState) end,
       Queue),
  NotEnabled =
    lists:filter
      (fun ({_,Call}) -> not(StateMod:cpre(Call,DataState)) end,
       Queue),
  {Enabled,NotEnabled}.


	 
	 
    


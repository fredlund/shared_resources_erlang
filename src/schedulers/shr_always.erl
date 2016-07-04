-module(shr_always).

-behaviour(shr_wait_implementation).

-export([initial_state/2, new_waiting/3, priority_enabled/4, post_waiting/4]).

initial_state(_,_) ->
  void.

new_waiting(_Call,State,_DataState) ->
  {void,State}.

priority_enabled(_Call,_CallInfo,_State,_DataState) ->
  true.

post_waiting(_Call,_CallInfo,State,_DataState) ->
  State.
    


%% First come first served

-module(shr_fcfs).

-behaviour(shr_wait_implementation).

-export([initial_state/2,new_waiting/3,priority_enabled/4,post_waiting/4]).

initial_state(_,_) ->
  {0,0}.

new_waiting(_Call,{Counter,Current},_) ->
  {Counter,{Counter+1,Current}}.

priority_enabled(_Call,CallCounter,{_Counter,Current},_) ->
  CallCounter==Current.

post_waiting(_Call,_CallInfo,{Counter,Current},_) ->
  {Counter,Current+1}.
    


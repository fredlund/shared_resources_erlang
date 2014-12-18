%% No priority

-module(always).

-behaviour(resource_wait_implementation).

-export([init/1,new_waiting/3,priority_enabled/4,post_waiting/4]).

init(_) ->
  {ok,void}.

new_waiting(_Call,State,_DataState) ->
  {void,State}.

priority_enabled(_Call,_CallInfo,_State,_DataState) ->
  true.

post_waiting(_Call,_CallInfo,State,_DataState) ->
  State.
    


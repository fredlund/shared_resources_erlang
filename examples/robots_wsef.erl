%% Priority for smallest weight to always exit first

-module(robots_wsef).

-behaviour(resource_wait_implementation).

-export([init/1,new_waiting/3,priority_enabled/4,post_waiting/4]).

init([N,_MaxWeight]) ->
  {ok,{N,lists:map(fun (I) -> {I,[]} end, lists:seq(1,N-1))}}.

new_waiting(_Call,WS,_DS) ->
  {void,WS}.

priority_enabled({call,exit,[_R,N,W]},_,{Max,Waiters},_) ->
  if
    N==Max-1 ->
      true;
    true ->
      {_,OldWaiters} = lists:keyfind(N+1,1,Waiters),
      W == lists:max(OldWaiters)
  end.

post_waiting({call,enter,[_R,N,W]},_CallInfo,WS={Max,Waiters},_) ->
  if
    N==Max-1 ->
      WS;
    true ->
      {_,OldWaiters} = lists:keyfind(N+1,1,Waiters),
      {Max,lists:keystore(N+1,1,Waiters,{N+1,[W|OldWaiters]})}
  end;
post_waiting(_,_,WS,_) ->
  WS.




%% Priority for smallest weight to always exit first

-module(robots_wsef).

-behaviour(resource_wait_implementation).

-export([init/1,new_waiting/3,priority_enabled/4,post_waiting/4]).

init([_,N,_MaxWeight]) ->
  {N,lists:map(fun (I) -> {I,[]} end, lists:seq(0,N-1))}.

new_waiting(_Call,WS,_DS) ->
  {void,WS}.

priority_enabled({exit,[_R,N,W]},_,{Max,Waiters},_) ->
  if
    N==Max-1 ->
      true;
    true ->
      {_,OldWaiters} = lists:keyfind(N,1,Waiters),
      W == lists:min(OldWaiters)
  end;
priority_enabled(_Call,_Info,_WS,_DS) ->
  true.

post_waiting({enter,[_R,N,W]},_CallInfo,WS={Max,Waiters},_) ->
  if
    N==Max-1 ->
      WS;
    true ->
      {_,OldWaiters} = lists:keyfind(N,1,Waiters),
      {Max,lists:keystore(N,1,Waiters,{N,[W|OldWaiters]})}
  end;
post_waiting({exit,[_R,N,W]},_CallInfo,{Max,Waiters},_) ->
  {_,OldWaiters} = lists:keyfind(N,1,Waiters),
  {Max,lists:keystore(N,1,Waiters,{N,OldWaiters--[W]})}.

  




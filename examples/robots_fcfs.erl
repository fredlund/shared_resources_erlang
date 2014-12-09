-module(robots_fcfs).

-export([init/2, pre/2,cpre/2,post/2]).
-export([wait_init/2,new_waiting/3,priority_enabled/4,post_waiting/3]).

-behaviour(resource_implementation).

-record(robots,{n,max_weight,corridors,warehouses}).

init(N,MaxWeight) ->
  {ok,
   #robots
   {n=N,
    max_weight=MaxWeight,
    warehouses=lists:map(fun (I) -> {I,0} end, lists:seq(0,N-1)),
    corridors=lists:map(fun (I) -> {I,false} end, lists:seq(1,N-1))}}.

pre({call,enter,[_R,N,W]},State) ->
  is_integer(N) andalso (N>=0) andalso (N<State#robots.n)
    andalso is_integer(W) andalso (W>=0);
pre({call,exit,[_R,N,W]},State) ->
  is_integer(N) andalso (N>=0) andalso (N<State#robots.n)
    andalso is_integer(W) andalso (W>=0).

cpre({call,enter,[_R,N,W]},State) ->
  (weight(N,State)+W) =< State#robots.max_weight;
cpre({call,exit,[_R,N,_W]},State) ->
  (N==State#robots.n-1) orelse (not(occupied(N+1,State))).

post({call,enter,[_R,N,W]},State) ->
  NewState =
    if
      N==0 -> add_weight(W,N,State);
      true -> add_weight(W,N,remove_robot(N,State))
    end,
  {void,NewState};
post({call,exit,[_R,N,W]},State) ->
  NewState =
    if
      N==State#robots.n-1 -> add_weight(-W,N,State);
      true -> add_weight(-W,N,add_robot(N+1,State))
    end,
  {void,NewState}.

add_weight(W,N,State) ->
  {_,OldWeight} = lists:keyfind(N,1,State#robots.warehouses),
  State#robots
    {warehouses=lists:keystore(N,1,State#robots.warehouses,{N,OldWeight+W})}.

remove_robot(N,State) ->
  State#robots
    {corridors=lists:keystore(N,1,State#robots.warehouses,{N,false})}.

add_robot(N,State) ->
  State#robots
    {corridors=lists:keystore(N,1,State#robots.warehouses,{N,true})}.

weight(N,State) ->
  {_,Weight} = lists:keyfind(N,1,State#robots.warehouses),
  Weight.

occupied(N,State) ->
  {_,Occupied} = lists:keyfind(N,1,State#robots.corridors),
  Occupied.

wait_init(_N,_MaxWeight) ->
  {ok,{0,0}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

new_waiting(_Call,{Counter,Current},_) ->
  {Counter,{Counter+1,Current}}.

priority_enabled(_Call,CallCounter,{_Counter,Current},_) ->
  CallCounter==Current.

post_waiting(_Call,{Counter,Current},_) ->
  {Counter,Current+1}.
    


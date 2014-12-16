-module(robots).

-behaviour(resource_data_implementation).

-export([init/1,pre/2,cpre/2,post/2]).
-export([n/1,num_naves/1,max_weight/1]).
-export([corridor/2,warehouse/2]).

-include("robots.hrl").

init([N,NumNaves,MaxWeight]) ->
  #robots
    {
     n=N,
     num_naves=NumNaves,
     max_weight=MaxWeight,
     warehouses=lists:map(fun (I) -> {I,0} end, lists:seq(0,N-1)),
     corridors=lists:map(fun (I) -> {I,false} end, lists:seq(1,N-1))
    }.

num_naves(State) ->
  State#robots.num_naves.

n(State) ->
  State#robots.n.

max_weight(State) ->
  State#robots.max_weight.

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

corridor(N,State) ->
  Result=lists:nth(N+1,State#robots.corridors),
  io:format("corridor(~p,~p) -> ~p~n",[N,State,Result]),
  Result.

warehouse(N,State) ->
  Result=lists:nth(N+1,State#robots.warehouses),
  io:format("warehouses(~p,~p) -> ~p~n",[N,State,Result]),
  Result.



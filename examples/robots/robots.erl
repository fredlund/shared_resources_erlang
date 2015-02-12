-module(robots).

-behaviour(resource_data_implementation).

-export([init/2,pre/2,cpre/2,post/2,return/3,return_value/2]).
-export([num_naves/1,max_weight/1]).

-include("robots.hrl").

init([NumNaves,MaxWeight],_Options) ->
  #robots
    {
     num_naves=NumNaves,
     max_weight=MaxWeight,
     warehouses=lists:map(fun (I) -> {I,0} end, lists:seq(0,NumNaves-1)),
     corridors=lists:map(fun (I) -> {I,false} end, lists:seq(1,NumNaves-1))
    }.

num_naves(State) ->
  State#robots.num_naves.

max_weight(State) ->
  State#robots.max_weight.

pre({enter,[_R,N,W]},State) ->
  is_integer(N) andalso (N>=0) andalso (N<State#robots.num_naves)
    andalso is_integer(W) andalso (W>=0);
pre({exit,[_R,N,W]},State) ->
  is_integer(N) andalso (N>=0) andalso (N<State#robots.num_naves)
    andalso is_integer(W) andalso (W>=0).

cpre({enter,[_R,N,W]},State) ->
  (weight(N,State)+W) =< State#robots.max_weight;
cpre({exit,[_R,N,_W]},State) ->
  (N==(State#robots.num_naves-1)) orelse (not(occupied(N+1,State))).

post({enter,[_R,N,W]},State) ->
  NewState =
    if
      N==0 -> add_weight(W,N,State);
      true -> add_weight(W,N,remove_robot(N,State))
    end,
  NewState;
post({exit,[_R,N,W]},State) ->
  NewState =
    if
      N==State#robots.num_naves-1 -> add_weight(-W,N,State);
      true -> add_weight(-W,N,add_robot(N+1,State))
    end,
  NewState.

return(_State,_Call,_Result) ->
  true.

return_value(_,_) ->
  void.

add_weight(W,N,State) ->
  {_,OldWeight} = lists:keyfind(N,1,State#robots.warehouses),
  State#robots
    {warehouses=lists:keystore(N,1,State#robots.warehouses,{N,OldWeight+W})}.

remove_robot(N,State) ->
  State#robots
    {corridors=lists:keystore(N,1,State#robots.corridors,{N,false})}.

add_robot(N,State) ->
  State#robots
    {corridors=lists:keystore(N,1,State#robots.corridors,{N,true})}.

weight(N,State) ->
  {_,Weight} = lists:keyfind(N,1,State#robots.warehouses),
  Weight.

occupied(N,State) ->
  {_,Occupied} = lists:keyfind(N,1,State#robots.corridors),
  Occupied.




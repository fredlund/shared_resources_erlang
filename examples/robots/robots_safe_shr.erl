-module(robots_safe_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([num_naves/1,max_weight/1]).
-export([print_state/1]).

%%-define(debug,true).
-include("../../src/debug.hrl").


-include("robots.hrl").

initial_state(_,Options) ->
  NumWarehouses = proplists:get_value(num_warehouses,Options),
  #robots
    {
     num_naves=NumWarehouses
     ,max_weight=proplists:get_value(weight_limit,Options)
     ,warehouses=lists:map(fun (I) -> {I,0} end, lists:seq(0,NumWarehouses-1))
     ,corridors=lists:map(fun (I) -> {I,false} end, lists:seq(1,NumWarehouses-1))
    }.

num_naves(State) ->
  State#robots.num_naves.

max_weight(State) ->
  State#robots.max_weight.

pre({Operation,_Args=[_R,N,W]},State) when is_record(State,robots) ->
  Return = 
    lists:member(Operation,[enter,entered,exit,exited])
    andalso is_integer(N) andalso (N>=0) andalso (N<State#robots.num_naves)
    andalso is_integer(W) andalso (W>=0),
  ?TIMEDLOG
     ("Operation=~p Args=~p State=~p => ~p~n",
      [Operation,_Args,State,Return]),
  Return.

cpre({enter,[_R,N,W]},State) when is_record(State,robots) ->
  (weight(N,State)+W) =< State#robots.max_weight;
cpre({entered,_},State) when is_record(State,robots) ->
  true;
cpre({exit,[_R,N,_W]},State) when is_record(State,robots) ->
  (N==(State#robots.num_naves-1)) orelse (not(occupied(N+1,State)));
cpre({exited,_},State) when is_record(State,robots) ->
  true.

post({enter,[_R,N,W]},_Result,State) when is_record(State,robots) ->
  NewState =
    if
      N==0 -> add_weight(W,N,State);
      true -> add_weight(W,N,State)
    end,
  NewState;
post({entered,[_R,N,_W]},_Result,State) when is_record(State,robots) ->
  NewState =
    if
      N==0 -> State;
      true -> remove_robot(N,State)
    end,
  NewState;
post({exit,[_R,N,_W]},_Result,State) when is_record(State,robots) ->
  NewState =
    if
      N==State#robots.num_naves-1 -> State;
      true -> add_robot(N+1,State)
    end,
  NewState;
post({exited,[_R,N,W]},_Result,State) when is_record(State,robots) ->
  NewState =
    if
      N==State#robots.num_naves-1 -> add_weight(-W,N,State);
      true -> add_weight(-W,N,State)
    end,
  NewState.

return(_State,_Call,_Result) ->
  true.

return_value(_Call,_State) ->
  underspecified.

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

print_state(#robots{corridors=Corridors,warehouses=Warehouses}) ->
  io_lib:format
    ("{corridors=~p,warehouses=~p}",
     [Corridors,Warehouses]).



-module(robots_safe_protocol).

%% We can do a generic protocol as well, for handling inits

-behaviour(shr_protocol).

-export([initial_state/2,postcondition/3,next_state/3]).

-record(state,
	{
	  num_warehouses
	  ,weight_limit
	  ,weight
	  ,next
	}).

%%-define(debug,true).
-include("../../src/debug.hrl").

initial_state(_,Options) ->
  State =
    #state
    {
    num_warehouses=proplists:get_value(num_warehouses,Options)
    ,weight_limit=proplists:get_value(weight_limit,Options)
    ,weight=0
    ,next={controller,enter,0}
   },
  {ok,State}.

postcondition(Turn,{Operation,[_RobotId,Warehouse,Weight]},State) ->
  ?LOG
     ("~p: postcondition Turn=~p Warehouse=~p Weight=~p"
      ++" State is~n  ~p~n",
      [Operation,Turn,Warehouse,Weight,State]),
  (State#state.next=={Turn,Operation,Warehouse})
    andalso 
    if
      Operation==enter, Turn==controller -> 
	Weight>=State#state.weight;
      State#state.next=/={controller,enter,0} -> 
	Weight==State#state.weight;
      true ->
	true
    end.

next_state(Turn,{Operation,[_,Warehouse,Weight]},State) ->
  Next =
    if
      Turn==controller, ((Operation==enter) or (Operation==exit)) ->
	{environment,Operation,Warehouse};
      Turn==controller, Operation==entered ->
	{controller,exit,Warehouse};
      Turn==controller, Operation==exited ->
	{controller,enter,Warehouse+1};

      Turn==environment, Operation==enter ->
	{controller,entered,Warehouse};
      Turn==environment, Operation==exit ->
	{controller,exited,Warehouse}
    end,
  State#state{next=Next,weight=Weight}.
  



  
    
  

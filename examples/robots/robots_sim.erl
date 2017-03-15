-module(robots_sim).

-export([run/3]).

-define(debug,true).
-include("../../src/debug.hrl").


run(N,ControllerAndEnvironment,Options) when is_integer(N), N>=1 ->
  {A,B,C} = os:timestamp(),
  random:seed(A,B,C),
  NumWarehouses = proplists:get_value(num_warehouses,Options),
  WeightLimit = proplists:get_value(weight_limit,Options),
  Self = self(),
  lists:foreach
    (fun (Id) ->
	 [ControllerReg,EnvironmentReg] =
	   lists:nth(Id,ControllerAndEnvironment),
	 Controller = element(2,ControllerReg),
	 Environment = element(2,EnvironmentReg),
	 spawn_link
	   (fun () ->
		run(Id,Controller,Environment,NumWarehouses,WeightLimit),
		Self!done
	    end)
     end, lists:seq(1,N)),
  lists:foreach(fun (_) -> receive done -> ok end end, lists:seq(1,N)).

run(Id,Controller,Environment,NumWarehouses,WeightLimit) ->
  {A,B,C} = os:timestamp(),
  random:seed(A,B,C),
  Weight = random:uniform(WeightLimit),
  run(0,Weight,Id,Controller,Environment,NumWarehouses,WeightLimit).

run(Warehouse,_Weight,_Id,_Controller,_Environment,NumWarehouses,_WeightLimit) 
  when Warehouse>=NumWarehouses ->
  ok;
run(Warehouse,Weight,Id,Controller,Environment,NumWarehouses,WeightLimit) 
  when Warehouse>=0, Warehouse<NumWarehouses ->
  ?LOG("controller:enter(~p,~p,~p)~n",[Id,Warehouse,Weight]),
  shr_calls:call(Controller,{enter,[Id,Warehouse,Weight]}),
  ?LOG("environment:enter(~p,~p,~p)~n",[Id,Warehouse,Weight]),
  shr_calls:call(Environment,{enter,[Id,Warehouse,Weight]}),
  ?LOG("controller:exit(~p,~p,~p)~n",[Id,Warehouse,Weight]),
  shr_calls:call(Controller,{exit,[Id,Warehouse,Weight]}),
  ?LOG("environment:exit(~p,~p,~p)~n",[Id,Warehouse,Weight]),
  shr_calls:call(Environment,{exit,[Id,Warehouse,Weight]}),
  WeightIncrement = random:uniform(100),
  NewWeight = min(WeightLimit,Weight+WeightIncrement),
  run(Warehouse+1,NewWeight,Id,Controller,Environment,NumWarehouses,WeightLimit).

  
  

%%% @author fred <fred@uppsala.localdomain>
%%% @copyright (C) 2016, fred
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2016 by fred <fred@uppsala.localdomain>

-module(robots_grn_rnd).

-behaviour(shr_gnr_implementation).

-include_lib("eqc/include/eqc.hrl").
-include("../../src/tester.hrl").


-export([initial_state/2,start/2,started/2,command/2,precondition/2,next_state/3,stop/1]).
-export([enter/3,exit/3]).
-export([print_finished_job_info/2, print_started_job_info/2]).

%% -- State ------------------------------------------------------------------
-record(state,{num_warehouses,weight_limit,num_robots,started}).

initial_state(_,Options) ->
  #state
    {
      num_robots=proplists:get_value(num_robots,Options)
      ,num_warehouses=proplists:get_value(num_warehouses,Options)
      ,weight_limit=proplists:get_value(weight_limit,Options)
    }.

start(_State,Options) ->
  robots_startup:startup(Options).

started(State,_) ->
  State.

command(State,_) ->
  ?LET({RobotId,Warehouse,Weight,IsEnter},
       {
	 choose(1,State#state.num_robots)
	,choose(0,State#state.num_warehouses-1)
	,choose(0,State#state.weight_limit)
	,bool()
       },
       if
	 IsEnter -> [{?MODULE,enter,[RobotId,Warehouse,Weight],[]}];
	 true -> [{?MODULE,exit,[RobotId,Warehouse,Weight],[]}]
       end).

precondition(_State,_Commands) ->
  true.

next_state(State,_Result,_Args) ->
  State.

enter(Robot,Warehouse,Weight) -> 
  robots_controller_via_protocol:enter(Robot,Warehouse,Weight),
  robots_environment_via_protocol:enter(Robot,Warehouse,Weight).

exit(Robot,Warehouse,Weight) -> 
  robots_controller_via_protocol:exit(Robot,Warehouse,Weight),
  robots_environment_via_protocol:exit(Robot,Warehouse,Weight).

stop(_State) ->
  exit(shr_utils:get(eqc_process),brutal_kill),
  timer:sleep(100).

print_finished_job_info(Job,TS) ->
  case Job#job.call of
    {_,_,[R,_,_]} -> io_lib:format("~p",[R])
  end.

print_started_job_info(Job,TS) ->
  shr_utils:print_mfa(Job#job.call).


  

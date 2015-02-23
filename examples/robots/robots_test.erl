-module(robots_test).

-compile(export_all).

-include("../../testing/src/fstate.hrl").

test() ->
  Id = "test",
  CP =  ["examples/robots/java/classes/",
	 "examples/robots/java/implementation",
	 "examples/robots/cclib.jar"],
/home/fred/gits/src/cctester/test  DataSpec =
    {robots,[4,1000]},
  WaitSpec =
    {always,[]},
  TestingSpec = 
    {fsms,[{10,{robot_fsm,[4]}}]},  %% 10 robots for a system of 4 warehouses
  Options =
    [{max_par,0},{id,Id},{cp,CP},
     {implementation,{robot_java_impl,[]}},
     {global_state,void}],  
  tester:test(Options,DataSpec,WaitSpec,TestingSpec).


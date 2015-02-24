-module(robots_test).

-compile(export_all).

-include("../../testing/src/fstate.hrl").

test() ->
  Id = "test",
  CP =  ["examples/robots/java/classes/",
	 "examples/robots/java/implementation",
	 "examples/robots/java/cclib.jar"],
  DataSpec =
    {robots,[4,1000]},
  WaitSpec =
    {always,[]},
  TestingSpec = 
    {fsms,[{10,{robot_fsm,[4,robot_java_impl]}}]},  
  Options =
    [{max_par,0},{id,Id},{cp,CP},
     {print_testcase,true},
     {implementation,{robot_java_impl,[]}},
     {global_state,void}],  
  tester:test(Options,DataSpec,WaitSpec,TestingSpec).

testpar() ->
  Id = "test",
  CP =  ["examples/robots/java/classes/",
	 "examples/robots/java/implementation",
	 "examples/robots/java/cclib.jar"],
  DataSpec =
    {robots,[4,1000]},
  WaitSpec =
    {always,[]},
  TestingSpec = 
    {fsms,[{10,{robot_fsm,[4,robot_java_impl]}}]},  
  Options =
    [{max_par,3},{id,Id},{cp,CP},
     {implementation,{robot_java_impl,[]}},
     {print_testcase,true},
     {global_state,void}],  
  tester:test(Options,DataSpec,WaitSpec,TestingSpec).


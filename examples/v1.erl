-module(v1).

-compile(export_all).

test() ->
  Id = "test",
  CP =  ["/home/fred/practica_1_complete/G-4F1M/110175/20140605-170257"],
  DataSpec = robots,
  WaitSpec = fcfs,
  TestingSpec = robot_commands,
  tester:test(CP,Id,DataSpec,WaitSpec,TestingSpec).



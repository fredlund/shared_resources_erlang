-module(v1).

-compile(export_all).

test() ->
  Id = "test",
  CP =  ["/home/fred/gits/src/cctester/test/classes/","/home/fred/practica_1_complete/G-4F1M/110175/20140605-170257"],
  DataSpec = {robots,[3,1000]},
  WaitSpec = {fcfs,[]},
  TestingSpec = {robot_commands,[10]},
  tester:test(CP,Id,DataSpec,WaitSpec,TestingSpec).
  



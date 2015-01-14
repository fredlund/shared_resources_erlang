-module(v1).

-compile(export_all).

test() ->
  Id = "test",
  CP =  ["/home/fred/gits/src/cctester/test/classes/",
%%	 "/home/fred/practica_1_complete/G-4F1M/110175/20140605-170257",
	 "/home/fred/practica_1_complete/G-4F1M/000999/20140522-143819/",
	 "/home/fred/gits/src/cctester/test/cclib.jar"],
  DataSpec = {robots,[4,1000]},
  WaitSpec = {fcfs,[]},
  TestingSpec = {robot_commands,[10,4]},
  tester:test(CP,Id,DataSpec,WaitSpec,TestingSpec).

test2() ->
  Id = "test",
  CP =  ["/home/fred/gits/src/cctester/test/classes/",
%%	 "/home/fred/practica_1_complete/G-4F1M/110175/20140605-170257",
	 "/home/fred/practica_1_complete/G-4F1M/000999/20140522-143819/",
	 "/home/fred/gits/src/cctester/test/cclib.jar"],
  DataSpec = {robots,[4,1000]},
  WaitSpec = {always,[]},
  TestingSpec = {robot_commands,[10,4]},
  tester:test(CP,Id,DataSpec,WaitSpec,TestingSpec).
  



-module(testm).

-compile(export_all).

test() ->
  Id = "test",
  CP =  ["/home/fred/gits/src/cctester/test/classes/",
%%	 "/home/fred/practica_1_complete/G-4F1M/110175/20140605-170257",
	 "/home/fred/practica_1_complete/G-4F1M/000999/20140522-143819/",
	 "/home/fred/gits/src/cctester/test/cclib.jar"],
  DataSpec = {multibuffer,[10]},
  WaitSpec = {always,[]},
  TestingSpec = {multibuffer_commands,[10,7,7]},
  tester:test(CP,Id,DataSpec,WaitSpec,TestingSpec).


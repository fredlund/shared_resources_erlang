-module(testm).

-compile(export_all).

test() ->
  Id = {id,"test"},
  CP =  {cp,
	 ["/home/fred/gits/src/cctester/test/classes/",
%%	 "/home/fred/practica_1_complete/G-4F1M/110175/20140605-170257",
	 "/home/fred/practica_1_complete/G-4F1M/000999/20140522-143819/",
	 "/home/fred/gits/src/cctester/test/cclib.jar"]},
  DataSpec = {multibuffer,[10]},
  WaitSpec = {always,[]},
  TestingSpec = {multibuffer_commands,[10,7,7,multbuf1]},
  tester:test([{needs_java,false},CP,Id],DataSpec,WaitSpec,TestingSpec).




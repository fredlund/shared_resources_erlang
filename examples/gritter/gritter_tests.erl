-module(gritter_tests).

-include_lib("eqc/include/eqc.hrl").

-export([sim/1]).
-export([prop_sol_1/0,prop_sol_1/3]).

sim(N) ->

  shr_utils:setup_shr(),

  %% We need a controller, first start the resource and then wrap it in
  %% a generic Erlang resource implementation
  [Controller] = 
    shr_supervisor:add_childproc
      (gritter_shr, 
       fun () ->
	   shr_gen_resource:start_link(gritter_shr,shr_always,[])
       end),

  %% Finally start the simulation
  gritter_sim:run(N,Controller,[]).
  
prop_sol_1() ->
  ?FORALL(N,choose(3,8),innerprop(N,'GestorGritterMonitor',"examples/gritter/classes","examples/gritter/libs")).

prop_sol_1(Class,Dir,LibPath) ->
  ?FORALL(N,choose(3,8),innerprop(N,Class,Dir,LibPath)).

innerprop(N,Class,Dir,LibPath) ->
  ControllerOpts = 
    [
     {data_spec,gritter_shr}
     ,{waiting_spec,shr_always}
    ],
  GeneratorOption = 
    [{test_gen_spec,{shr_gnr_fsms,[{N,gritter_gnr_fsm}]}}],
%%    [{test_gen_spec,{shr_gnr_fsms,[{N,gritter_gnr_fsm}]}}],
%%    [{test_gen_spec,{gritter_gnr,N}}],
  CorrOption = 
    [{test_corr_spec,shr_corr_resource}],
  StartFun =
    [{start_fun,start_controller(Class,Dir,LibPath)}],
  StopFun =
    [{stop_fun,stop_java()}],
  MaxUids = 
    [{max_uids,N}],
  Options =
    ControllerOpts++GeneratorOption++CorrOption++StartFun++StopFun++MaxUids,
  eqc:on_output
    (fun shr_test_jobs:eqc_printer/2,
     shr_test_jobs:prop_res(Options)).

start_controller(Class,Directory,LibPath) ->  
  fun (_Options) ->
      ClassPath =
	[
	 LibPath++"/cclib.jar"
	 ,LibPath++"/net-datastructures-4-0.jar"
	 ,LibPath++"/net-datastructures-5-0.jar"
	 ,LibPath++"/positionList.jar"
	 ,LibPath++"/fifoaed.jar"
	 ,LibPath++"/lifoaed.jar"
	 ,LibPath++"/jcsp-core.jar"
	 ,LibPath
	 ,Directory
	],
      %%io:format("ClassPath is~n~p~n",[ClassPath]),
      {ok,Java} =
	java:start_node([{call_timeout,infinity},
			 {add_to_java_classpath,ClassPath}]),
      shr_utils:put(java,Java),
      Controller = java:new(Java,Class,[]),
      case Class of
	'GestorGritterCSP' -> 
	  PM = java:new(Java,'org.jcsp.lang.ProcessManager',[Controller]),
	  java:call(PM,start,[]);
	_ ->
	  ok
      end,
      shr_supervisor:add_childproc
	(controller,
	 fun () ->
	     shr_java_controller:start_link(Controller,[])
	 end)
  end.

stop_java() ->
  fun (_Options) ->
      try java:terminate(shr_utils:get(java)) catch _:_ -> ok end
  end.

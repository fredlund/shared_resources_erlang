-module(gritter_tests).

-include_lib("eqc/include/eqc.hrl").

-export([sim/1]).
-export([prop_sol_1/0,prop_sol_1/3]).
-export([test_users/2,gen_suite/3]).
-export([make_test_script/4]).

sim(N) ->

  %% Start the supervisor
  shr_simple_supervisor:restart(self()),

  ControllerOpts = 
    [
     {data_spec,gritter_shr}
     ,{waiting_spec,shr_always}
    ],
    
  %% We need a controller, first start the resource and then wrap it in
  %% a generic Erlang resource implementation
  [Controller] = 
    shr_simple_supervisor:add_childproc
      (gritter_shr, 
       fun () ->
	   shr_gen_resource:start_link(ControllerOpts,[])
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
    [{test_gen_spec,{shr_gnr_fsms,[{N,gritter_gnr_fsm2}]}}],
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
  fun (Options) ->
      MaxUids = proplists:get_value(max_uids,Options),
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
      Ports =
	lists:flatmap
	  (fun (_) ->
	       shr_simple_supervisor:add_childproc
		 (fun () ->
		      shr_java_controller:start_link_client
			(Controller,[])
		  end)
	   end, lists:seq(1,MaxUids)),
      lists:foreach
	(fun ({Id,Pid}) -> shr_register:register({controller,Id},Pid) end,
	 lists:zip(lists:seq(1,length(Ports)),Ports))
  end.

stop_java() ->
  fun (_Options) ->
      try java:terminate(shr_utils:get(java)) catch _:_ -> ok end
  end.

test_users(Class,EntregaDir) ->
  ClassFile =
    atom_to_list(Class)++".class",
  Entregas =
    find_entregas:find_entregas(ClassFile,EntregaDir),
  if
    Entregas==[] ->
      io:format
	("*** Error: cannot find entregas in directory ~s (using file ~s)~n",
	 [EntregaDir,ClassFile]),
      throw(bad);
    true ->
      lists:foreach
	(fun ({User,Group,Dir,_Time}) ->
	     io:format
	       ("~n~nChecking user ~s (group ~s) in Dir ~s ~n",
		[User,Group,Dir]),
	     case eqc:quickcheck(prop_sol_1(Class,Dir,"examples/gritter/libs")) of
	       true -> io:format("User ~p PASSES~n", [User]);
	       false -> io:format("User ~p FAILS~n", [User])
	     end
	 end, Entregas)
  end.
  
gen_suite(Class,EntregaDir,SuiteFile) ->
  ClassFile =
    atom_to_list(Class)++".class",
  Entregas =
    find_entregas:find_entregas(ClassFile,EntregaDir),
  if
    Entregas==[] ->
      io:format
	("*** Error: cannot find entregas in directory ~s (using file ~s)~n",
	 [EntregaDir,ClassFile]),
      throw(bad);
    true ->
      Suite =
	lists:foldl
	  (fun ({User,Group,Dir,_Time},Suite) ->
	       io:format
		 ("~n~nChecking user ~s (group ~s) in Dir ~s ~n",
		  [User,Group,Dir]),
	       case eqc:counterexample(prop_sol_1(Class,Dir,"examples/gritter/libs")) of
		 true -> 
		   io:format("User ~p PASSES~n", [User]),
		   Suite;
		 CounterExample -> 
		   io:format("User ~p FAILS~n", [User]),
		   [CounterExample|Suite]
	       end
	   end, [], Entregas),
      SuiteBinary = term_to_binary({random,Suite}),
      file:write_file(SuiteFile,SuiteBinary)
  end.
 
make_test_script(TestClasses,ClassFile,EntregaDir,ScriptFile) ->
  Entregas = find_entregas:find_entregas(ClassFile,EntregaDir),
  {ok,F} = file:open(ScriptFile,[write]),
  io:format(F,"#!/bin/sh~n~n",[]),
  io:format(F,"make testing_classes~n~n",[]),
  lists:foreach
    (fun ({User,Group,Dir,Time}) ->
	 UserString = 
	   io_lib:format("User ~s (group ~s) (at time ~p)",[User,Group,Time]),
	 lists:foreach
	   (fun (TestClass) ->
		io:format
		  (F,
		   "echo \"Testing ~s with ~p\"~n",
		   [UserString,TestClass]),
		io:format
		  (F,
		   "java -cp ~s:testing_classes:libs/cclib.jar:/usr/share/java/junit4.jar:/usr/share/java/hamcrest-core-1.2.jar:libs/net-datastructures-5-0.jar:libs/positionList.jar:libs/jcsp.jar:libs/sequenceTester.jar:libs/fifoaed.jar:libs/lifoaed.jar org.junit.runner.JUnitCore "++TestClass++"~n",[Dir]),
		io:format
		  (F,
		   "if [ $? -eq 0 ]~nthen~n  echo \"Result:: ~s PASSED ~s\"~n"++
		     "else~n  echo \"Result:: ~s FAILED ~s\"~n"++
		     "fi~n",
		  [UserString,TestClass,UserString,TestClass]),
		io:format
		  (F,
		   "echo~necho~n~n",
		   [])
	    end, TestClasses)
     end, Entregas),
  ok = file:close(F).

%% ~/gits/src/shared_resources_erlang_examples$ erl -sname tst-pa ebin -pa examples/gritter/ebin/ -pa ~/gits/src/aed_labs/ebin/ | tee utput

%% Test users without collecting a test suite (less useful)
%% gritter_tests:test_users('GestorGritterMonitor',"/home/fred/2016may_moni").
%% gritter_tests:test_users('GestorGritterCSP',"/home/fred/2016may_csp_tmp").

%% Test users generating a test suite from failed tests
%% gritter_tests:gen_suite('GestorGritterMonitor',"/home/fred/2016may_moni_final","2016may_moni_final.suite").
%% gritter_tests:gen_suite('GestorGritterCSP',"/home/fred/2016may_csp_final","2016may_csp_final.suite").

%% Copy entregas selecting useful ones
%% find_entregas:copy_entregas("GestorGritterMonitor.class","/home/fred/2016may_moni",beginning,{2016,06,4,10,0,0},get_groups:groupfuns(),"/home/fred/2016may_moni_final").
%% find_entregas:copy_entregas("GestorGritterCSP.class","/home/fred/2016may_csp",beginning,{2016,06,4,10,0,0},get_groups:groupfuns(),"/home/fred/2016may_csp_final").
%% find_entregas:copy_entregas("GestorGritterMonitor.class","/home/fred/2016may_moni",beginning,infinity,get_groups:groupfuns(),"/home/fred/2016may_moni_final").
%% find_entregas:copy_entregas("GestorGritterCSP.class","/home/fred/2016may_csp",beginning,infinity,get_groups:groupfuns(),"/home/fred/2016may_csp_final").

%% Generate Java code from test suites
%% gritter_suite_to_junit:junit_tests("2016may_csp_final.suite","TestCSPGritter", 100, false).
%% gritter_suite_to_junit:junit_tests("2016may_moni.suite","TestMonitorGritter", 200, false).

%% Test users using a set of Junit tests
%% gritter_tests:make_test_script("TestGritterV3","GestorGritterMonitor.class","/home/fred/2016may_moni_tmp","tests.sh").

%% Test users using a set of Junit tests
%% gritter_tests:make_test_script(["TestGritterMonitor","Tmon","Tmon_opcionales","Tmon_final"],"GestorGritterMonitor.class","/home/fred/2016may_moni_final","tests_moni_final.sh").
%%
%% gritter_tests:make_test_script(["TestGritterCSP","Tcsp","Tcsp_opcionales","Tcsp_final"],"GestorGritterCSP.class","/home/fred/2016may_csp_final","tests_csp_final.sh").


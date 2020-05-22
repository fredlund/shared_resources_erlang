-module(carretera).

-include("tester.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("ecsv/include/ecsv.hrl").

-compile(export_all).

cars() ->
  [
   {car_gnr_fsm,["volvo",1,{weight,2000}]},
   {car_gnr_fsm,["saab",1,{weight,2000}]},
   {car_gnr_fsm,["vw",8,{weight,2000}]},
   {car_gnr_fsm,["toyota",1,{weight,2000}]},
   {car_gnr_fsm,["citroen",2,{weight,2000}]},
   {car_gnr_fsm,["fiat",3,{weight,2000}]}
  ].

sample() ->
  CommonOpts = 
    [
     {data_spec,{carretera_shr,[{distance,3},{carriles,2}]}}
    ,{waiting_spec,{shr_always,[]}}
    ,{test_gen_spec,
      {shr_gnr_fsms,cars() ++ [{tick_gnr_fsm,[]}]}}
    ],
  shr_sample_resource:generate(CommonOpts).

debug() ->
  shr_debug:debug
    (fun () ->
	 shr_gen_resource:start
           ({carretera_shr,[{distance,3},{carriles,2}]},shr_always,[])
     end).

debug2() ->
  ClassPath = 
    [
     "java/classes",
     "/home/fred/Downloads/cclib-0.4.9.jar",
     "/home/fred/svns/courses/aed/trunk/lib/aedlib.jar"
    ],
  {ok,Java} = 
    java:start_node([{call_timeout,infinity},
                     {add_to_java_classpath,ClassPath}]),
  Class =
    'cc.carretera.CarreteraMonitor',
  Controller = 
    java:new(Java,Class,[3,2]),
  shr_debug:debug
    (fun () ->
	 shr_java_controller:start_link(Controller,[])
     end).

test() ->
  DataSpec = {carretera_shr,[]},
  WaitSpec = shr_always,
  Class =
    'cc.carretera.CarreteraMonitor',
  Dir = 
    ".",
  Prop =
    shr_test_resource_implementation:prop_tri
      (
      {shr_gnr_fsms,cars() ++ [{tick_gnr_fsm,[]}]},
      start_controller(Class,"",[{distance,4},{carriles,2}]),
      stop_java(),
      DataSpec,
      WaitSpec,
      void,
      [
       no_par,
       {completion_time,200},
       {global_state,escritor_gnr_fsm:initial_global_state()},
       {global_constraint_fun,fun escritor_gnr_fsm:global_constraint/3}
      ]
     ),
  shr_test_jobs:check_prop(fun (_Opts) -> Prop end,[]).

start_controller(Class,Dirs,Options) ->  
  Distance = proplists:get_value(distance,Options),
  Carriles = proplists:get_value(carriles,Options),
  fun (_Options) ->
      ClassPath = 
	Dirs  ++
	[
         "/home/fred/gits/shared_resources_erlang/examples/carretera/java/classes",
         "/home/fred/Downloads/cclib-0.4.9.jar",
	 "/home/fred/svns/courses/cc/lib/jcsp-1.1-rc4/jcsp.jar",
         "/home/fred/svns/courses/aed/trunk/lib/aedlib.jar"
        ],
      io:format("ClassPath is ~p~n",[ClassPath]),
      {ok,Java} =
	shr_java_node:start_node([{call_timeout,infinity},
			      %%{java_verbose,"FINER"},
			      %%{log_level,all},
                              %%			      {java_options,["--add-opens","java.base/jdk.internal.loader=ALL-UNNAMED"]},
                              {enter_classes,[Class]},
			      {add_to_java_classpath,ClassPath}]),
      timer:sleep(1000),
      shr_utils:put(java,Java),
      io:format("will call new ~p(~p,~p)~n",[Class,Distance,Carriles]),
      Controller = java:new(Java,Class,[Distance,Carriles]),
      %%io:format("Location of ~p is ~p~n",[Class,print_where(Java,Controller)]),
      case Class of
	'cc.carretera.CarreteraCSP' -> 
	  PM = 
            report_java_exception
              (java:new(Java,'org.jcsp.lang.ProcessManager',[Controller])),
	  report_java_exception(java:call(PM,start,[]));
	_ ->
	  ok
      end,
      shr_supervisor:add_childproc
	(controller,
	 fun () ->
	     shr_java_controller:start_link
               (Controller,
                [{result_converter,fun convert_result/1}])
	 end)
  end.

report_java_exception(Exception={java_exception,_}) ->
  io:format("~n*** Error: unexpected Java exception~n",[]),
  java:report_java_exception(Exception);
report_java_exception(List) when is_list(List) ->
  lists:foreach(fun (Element) -> report_java_exception(Element) end, List),
  List;
report_java_exception(Other) ->
  Other.

print_term(Term) ->
  case java:is_object_ref(Term) of
    true ->
      io_lib:format("~s",[java:string_to_list(java:call(Term,toString,[]))]);
    false ->
      io_lib:format("~p",[Term])
  end.

print_where(Java,Object) ->
  Class = java:call(Object,getClass,[]),
  ClassLoader = java:call(Class,getClassLoader,[]),
  if
    ClassLoader==null ->
      "cannot find classloader~n";
    true ->
      ok
  end,
  Name = java:call(Class,getCanonicalName,[]),
  NameReplaced = 
    java:string_to_list
      (java:call
	 (Name,
	  replace,
	  [java:list_to_string(Java,"."),
	   java:list_to_string(Java,"/")])),
  Resource = 
    java:call
      (ClassLoader,
       getResource,
       [java:list_to_string(Java,NameReplaced++".class")]),
  java:string_to_list(java:call(Resource,toString,[])).

convert_result(Result) ->
  PreConvertedResult = shr_java_controller:std_converter(Result),
  case java:is_object_ref(PreConvertedResult) of
    true ->
      case java:instanceof(PreConvertedResult,'cc.carretera.Pos') of
        true ->
          {
          java:call(PreConvertedResult,getSegmento,[]),
          java:call(PreConvertedResult,getCarril,[])
         };
        false -> PreConvertedResult
      end;
    false -> PreConvertedResult
  end.

stop_java() ->
  fun (_Options) ->
      try java:terminate(shr_utils:get(java)) catch _:_ -> ok end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% carretera:test_users_par_with_class('cc.carretera.CarreteraCSP',["160223","160243","160347","16I034","16M022","16M044","16M072"]).
%% carretera:test_users_nopar_with_class('cc.carretera.CarreteraMonitor',["150291"]).

test_users_nopar() ->
  test_users_with_class('cc.carretera.CarreteraMonitor',[no_par]).
test_users_nopar(Users) ->
  test_users_with_class('cc.carretera.CarreteraMonitor',[no_par],Users).
test_users_nopar_csp() ->
  test_users_with_class('cc.carretera.CarreteraCSP',[no_par]).
test_users_par() ->
  test_users_with_class('cc.carretera.CarreteraMonitor',[no_junit]).
test_users_par_csp() ->
  test_users_with_class('cc.carretera.CarreteraCSP',[no_junit]).

test_users_nopar_with_class(Class,Users) ->
  test_users_with_class(Class,[no_par],Users).
test_users_par_with_class(Class,Users) ->
  test_users_with_class(Class,[],Users).

test_users_with_class(Class,PreOptions) ->
  if
    Class == 'cc.carretera.CarreteraMonitor' ->
      test_users_mon(PreOptions);
    true ->
      test_users_csp(PreOptions)
  end.
test_users_with_class(Class,PreOptions,Users) ->
  File = 
    if
      Class=='cc.carretera.CarreteraMonitor' -> "CarreteraMonitor.java";
      true -> "CarreteraCSP.java"
    end,
  EntregaDir =
    if 
      Class=='cc.carretera.CarreteraMonitor' -> "/home/fred/cc_2020_mon_exp";
      true -> "/home/fred/cc_2020_csp_mon_exp"
    end,
  test_users(Class,File,EntregaDir,PreOptions,Users).

test_users_mon(PreOptions) ->
  test_users('cc.carretera.CarreteraMonitor',"CarreteraMonitor.java","/home/fred/cc_2020_mon_exp",PreOptions,all).
%%  test_users('cc.carretera.CarreteraMonitor',"CarreteraMonitor.java","/home/fred/gits/src/cc_2020/buggy_carretera",PreOptions).
test_users_csp(PreOptions) ->
  test_users('cc.carretera.CarreteraCSP',"CarreteraCSP.java","/home/fred/cc_2020_csp_jul_reduced",PreOptions,all).

test_users(Class,File,EntregaDir,PreOptions,Users) ->
  put(failing_tests,[]),
  {ok,EntregaInfo} = read_entrega_info("/home/fred/cc_2020_mon_exp/prac1.csv"),
  Entregas = find_entregas(File,EntregaDir),
  LenEntregas = length(Entregas),
  if
    Users==all ->
      io:format("Will test ~p entregas.~n",[LenEntregas]);
    true ->
      ok
  end,
  lists:foreach
    (fun ({Id,Entrega={Name,_}}) ->
         DoTest = 
           lists:member(Name,Users) 
           orelse
           ((Users==all)
            andalso
            case lists:keyfind(Name,1,EntregaInfo) of
              false ->
                io:format("*** WARNING: cannot find group ~s~n",[Name]),
                true;
              Tuple ->
                element(2,Tuple)=/="0"
            end),
         if
           DoTest ->
             mtest(Class,Entrega,PreOptions);
           true ->
             ok
         end
     end, lists:zip(lists:seq(1,LenEntregas),Entregas)),
  case get(failing_tests) of
    [] -> ok;
    FailingTestCases when is_list(FailingTestCases) ->
      F = unique_filename(),
      ok = file:write_file(F,term_to_binary({failed,FailingTestCases})),
      io:format("wrote failed test cases to ~s~n",[F])
  end.

find_entregas(LFile,Target) ->
  WildCard = Target++"/*",
  lists:map
    (fun (Dir) ->
	 Group = filename:basename(Dir),
	 {Group,Dir}
     end,filelib:wildcard(WildCard)).

mtest(Class,{Group,Dir},PreOptions) ->
  mtest(Class,Group,Dir,PreOptions).
mtest(Class,Group,Dir,PreOptions) ->
  timer:sleep(500),
  io:format
    ("~n~n~nTesting group ~p with implementation in ~p~n~n",
     [Group,Dir]),
  PreProp =
    ?FORALL({Distance,Carriles},{eqc_gen:choose(1,5),eqc_gen:choose(1,3)},
    shr_test_resource_implementation:prop_tri
      (
      {shr_gnr_fsms,cars() ++ [{tick_gnr_fsm,[]}]},
      start_controller
        (Class,
         [Dir++"/classes","/home/fred/gits/src/cc_2020/carreteraClasses"],
         [{distance,Distance},{carriles,Carriles}|PreOptions]),
      stop_java(),
      {carretera_shr,[{distance,Distance},{carriles,Carriles}]},
      shr_always,
      void,
      [{completion_time,200}|PreOptions]
      %% [{completion_time,350}|PreOptions]
     )),
  Prop =
    case lists:member(no_par,PreOptions) of
      true ->
	PreProp;
      false ->
	?ALWAYS(2,PreProp)
    end,
  Result = shr_test_jobs:check_prop(fun (_Opts) -> Prop end,[]),
  if
    not(Result) ->
      io:format("~nGroup ~p failed.~n",[Group]),
      AllFailingTestCases =
        lists:filter
          (fun (TestCase) -> not(TestCase#test_case.test_result) end, 
           shr_test_jobs:return_test_cases()),
      io:format("Failing test cases:~n~p~n",[AllFailingTestCases]),
      case lists:member(no_junit,PreOptions) of
        true ->
          [FailedNonrunnableTestCase|_] = AllFailingTestCases,
          io:format
            ("Failed test case:~n~s~n~n",
             [shr_test_jobs:print_test_case(FailedNonrunnableTestCase)]);
        false ->
          case find_a_runnable_failing_test_case(AllFailingTestCases) of
            {ok,FailedRunnableTestCase} ->
              NewFailingTests =
                case get(failing_tests) of
                  undefined -> 
                    [FailedRunnableTestCase];
                  L when is_list(L) -> 
                    [FailedRunnableTestCase|L]
                end,
              put(failing_tests,NewFailingTests),
              F = unique_filename("tmp"),
              file:write_file(F,term_to_binary({failed,NewFailingTests})),
              io:format
                ("Failed runnable test case:~n~s~n~n",
                 [shr_test_jobs:print_test_case(FailedRunnableTestCase)]);
            false ->
              io:format
                ("No runnable failed test case exists~n",
                 []),
              [FailedNonrunnableTestCase|_] = AllFailingTestCases,
              NewFailingTests =
                case get(failing_tests) of
                  undefined -> 
                    [FailedNonrunnableTestCase];
                  L when is_list(L) -> 
                    [FailedNonrunnableTestCase|L]
                end,
              put(failing_tests,NewFailingTests),
              F = unique_filename("tmp"),
              file:write_file(F,term_to_binary({failed,NewFailingTests})),
              io:format
                ("Failed nonrunnable test case:~n~s~n~n",
                 [shr_test_jobs:print_test_case(FailedNonrunnableTestCase)]),
              print_test_case_diagnostics(FailedNonrunnableTestCase)
          end
      end;
    true -> 
      io:format("~nGroup ~p succeeded.~n",[Group])
  end,
  Result.

find_a_runnable_failing_test_case([]) ->
  false;
find_a_runnable_failing_test_case([TestCase|Rest]) ->
  case is_runnable(TestCase) of
    true -> {ok,TestCase};
    false -> find_a_runnable_failing_test_case(Rest)
  end.

print_test_case_diagnostics(TestCase) ->
  is_runnable(TestCase).

is_runnable(TC) ->
  TestCase = TC#test_case.test_case,
  DataSpec = shr_test_jobs:test_data_spec(TestCase),
  WaitingSpec = shr_test_jobs:test_waiting_spec(TestCase),
  BasicTestCase = shr_test_jobs:basic_test_case(TestCase),
  SimpleTestCase =
    lists:map 
      (fun (Cmds) ->  
	   [Jobs,_,_] = element(4,Cmds),
	   Jobs
       end, BasicTestCase),
  GenModule = shr_test_jobs:gen_module(TestCase),
  GenState = shr_test_jobs:initial_gen_state(TestCase),
  {Info,InitialState} =
    shr_step_resource:initial_state(DataSpec,WaitingSpec,GenModule,GenState,[]),
  io:format("Checking if ~p is runnable~n",[SimpleTestCase]),
  try shr_step_resource:repeat_step(SimpleTestCase,InitialState,Info) of
      StateSpace -> 
      true
  catch throw:not_deterministic -> 
      false;
        Class:Reason ->
      io:format
        ("Running shr_step on testcase~n~p~nwith initial state ~p~nfailed due to ~p:~p~nStacktrace:~n~p~n",
         [SimpleTestCase,InitialState,Class,Reason,erlang:get_stacktrace()])
  end.

unique_filename() ->
  unique_filename1("").
unique_filename(Dir) ->
  unique_filename1(Dir++"/").
unique_filename1(PreFix) ->
  {A,B,C} = os:timestamp(),
  io_lib:format(PreFix++"carretera_test_suite_~p_~p_~p.suite",[A,B,C]).

%% carretera:create_entrega_dir_from_bugs("/home/fred/svns/courses/cc/2017-2018-s2/practicas/codigo/testing/sequenceTester/examples/carretera/monitors/","buggy_carretera","javac -d . -cp /home/fred/svns/courses/aed/trunk/lib/aedlib.jar:/home/fred/Downloads/cclib-0.4.9.jar:/home/fred/svns/courses/cc/lib/jcsp-1.1-rc4/jcsp.jar *java").
%%

create_entrega_dir_from_bugs(FromDir,ToDir,CompileFun) ->
  case file:read_file_info(ToDir) of
    {ok,_} ->
      io:format
        ("~n*** Error: directory to create ~s already exists~n",
         [ToDir]),
      error(bad);
    _ ->
      ok
  end,
  {ok,Files} = file:list_dir(FromDir),
  JavaFiles = 
    lists:filter
      (fun (FileName) -> 
           case filename:extension(FileName) == ".java"of
             true ->
               case re:run(FileName,"^bug[0-9]+_") of
                 {match,[{0,_}]} -> true;
                 _ -> false
               end;
             false -> false
           end
       end, 
       Files),
  Pattern = 
    "^bug[0-9]+_(.*)$",
  Capture =
    [{capture,all_but_first,list}],
  case JavaFiles of
    [] ->
      io:format
        ("~n*** Error: no buggy files found in ~s~n",
         [FromDir]),
      error(bad);
    [First|Rest] ->
      {match,[DesiredFileName]} = re:run(First,Pattern,Capture),
      create_dir(FromDir,JavaFiles,ToDir,DesiredFileName,CompileFun,Pattern,Capture)
  end.

create_dir(FromDir,JavaFiles,ToDir,DesiredFileName,CompileFun,Pattern,Capture) ->
  FakeGroup = "G-4F1M",
  StartId = 150000,
  FakeHour = "20180507-122200",
  check_not_error(utils:cmd_with_status("mkdir "++ToDir)),
  GroupDir = ToDir++"/"++FakeGroup,
  check_not_error(utils:cmd_with_status("mkdir "++GroupDir)),
  create_entregas(FromDir,JavaFiles,DesiredFileName,CompileFun,GroupDir,StartId,FakeHour).

create_entregas(_,[],_,_,_,_,_) ->
  ok;
create_entregas(FromDir,[JavaFile|Rest],DesiredFileName,CompileFun,Dir,Id,FakeHour) ->
  UserDir = Dir++"/"++integer_to_list(Id),
  check_not_error(utils:cmd_with_status("mkdir "++UserDir)),
  HourDir = UserDir++"/"++FakeHour,
  check_not_error(utils:cmd_with_status("mkdir "++HourDir)),
  check_not_error(utils:cmd_with_status("cp /home/fred/svns/courses/cc/2017-2018-s2/practicas/codigo/carretera/*java "++HourDir)),
  check_not_error(utils:cmd_with_status("mv "++FromDir++"/"++JavaFile++" "++HourDir++"/"++DesiredFileName)),
  {ok,CurrDir} = file:get_cwd(),
  ok = file:set_cwd(HourDir),
  check_not_error(utils:cmd_with_status(CompileFun)),
  ok = file:set_cwd(CurrDir),
  create_entregas(FromDir,Rest,DesiredFileName,CompileFun,Dir,Id+1,FakeHour).

check_not_error({0,_}) ->
  ok;
check_not_error({N,Text}) ->
  io:format
    ("~n*** Error: command failed with result code ~p:~n~s~n",
     [N,Text]),
  error(bad).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% carretera:tests_to_junit("quepasa_test_suite_1527_177866_906330.suite").
%% carretera:tests_to_junit("quepasa_test_suite_may_mon.suite").

%% inst_check:subst_rounds([[{crearGrupo,[3,"grupoC"]}],[{crearGrupo,[2,"grupoC"]}]],[[{crearGrupo,[3,"grupoA"]}],[{crearGrupo,[2,"grupoA"]}]]).
%% carretera:tests_to_junit("quepasa_test_suite_1527_188179_115880.suite").
%% carretera:tests_to_junit("quepasa_2018_may_mon_1527.suite").
%% carretera:tests_to_junit("quepasa_2018_may_mon.suite").
%% carretera:tests_to_junit("quepasa_test_suite_csp_may.suite").
%% get_groups:get_java_groups('cc.carretera.CarreteraMonitor',"CarreteraMonitor.java","/home/fred/cc_2020_mon_jun").
%%
%% carretera:tests_to_junit("TesterJulMon","mon_jul","quepasa_test_suite_1531_242991_709018.suite").
%% carretera:tests_to_junit("TesterJulCSP","csp_jul","quepasa_test_suite_1531_287626_712097.suite").


tests_to_junit(FileName) ->
  tests_to_junit("TestsExpr","exp",FileName).

tests_to_junit(TesterPrefix,TestPrefix,FileName) ->
  {ok,B} = file:read_file(FileName),
  {failed,TestCases} = binary_to_term(B),
  shr_test_cases_to_junit:gen_junit_tests
    (TesterPrefix,
     TestCases,
     TestPrefix,
     {carretera_shr,[]},
     shr_always,
     callrep(),
     fun order_test_cases/1).

order_test_cases(TestCases) ->
  SimplifiedTestCases =
    lists:map
      (fun (TC) ->
	   {
	     TC,
	     begin
	       TestCase = TC#test_case.test_case,
	       BasicTestCase = shr_test_jobs:basic_test_case(TestCase),
	       lists:map 
		 (fun (Cmds) ->
		      [Jobs,_,_] = element(4,Cmds),
		      lists:map
			(fun (Command) -> Command#command.call end,
			 Jobs)
		  end, BasicTestCase)
	     end
	   }
       end, TestCases),
  UniqueTestCases = skip_identical_testcases(SimplifiedTestCases,[]),
  SortedUniqueTestCases = sort_test_cases(UniqueTestCases),
  lists:map(fun ({TC,_}) -> TC end, SortedUniqueTestCases).

skip_identical_testcases([],TCs) -> TCs;
skip_identical_testcases([TC={TestCase,SimplifiedTestCase}|Rest],TCs) ->
  if
    SimplifiedTestCase == [] ->
      skip_identical_testcases(Rest,TCs);
    true ->
      SortedCallsTestCase =
	lists:map
	  (fun (Calls) ->
	       lists:sort(fun sort_calls/2, Calls)
	   end, SimplifiedTestCase),
      case my_member(fun ({_,OtherTC}) ->
			 inst_check:inst_check(SortedCallsTestCase,OtherTC)
		     end, TCs) of
	true ->
	  skip_identical_testcases(Rest,TCs);
	false ->
	  skip_identical_testcases(Rest,[TC|TCs])
      end
  end.

sort_test_cases(L) ->
  lists:sort(fun ({_,S1},{_,S2}) -> sort_testcases(S1,S2) end, L).

callrep() ->
  fun (JobCall) ->
      case JobCall#job.call of
	{_,Name,Args} ->
	  io_lib:format("new ~s(~s)",[map_name(Name),print_args(Args)])
      end
  end. 

my_member(F,[]) ->
  false;
my_member(F,[Elem|Rest]) ->
  case F(Elem) of
    true ->
      true;
    false ->
      my_member(F,Rest)
  end.

sort_testcases(T1,T2) ->
  LenT1 = length(T1),
  LenT2 = length(T2),
  if
    LenT1 < LenT2 -> true;
    LenT2 < LenT1 -> false;
    LenT1 == 0 -> true;
    true ->
      Calls1 = hd(T1),
      Calls2 = hd(T2),
      if
	length(Calls1) < length(Calls2) -> true;
	length(Calls2) < length(Calls1) -> false;
	true ->
	  case sort_calls(hd(Calls1),hd(Calls2)) of
	    N when N<0 ->
	      true;
	    N when N>0 ->
	      false;
	    0 ->
	      sort_testcases(tl(T1),tl(T2))
	  end
      end
  end.

sort_calls({F1,_},{F2,_}) -> op_value(F1) - op_value(F2).
op_value(crearGrupo) -> 0;
op_value(anadirMiembro) -> 1;
op_value(salirGrupo) -> 2;
op_value(mandarMensaje) -> 3;
op_value(leer) -> 3.

map_name(crearGrupo) -> "CrearGrupo";
map_name(anadirMiembro) -> "AnadirMiembro";
map_name(salirGrupo) -> "SalirGrupo";
map_name(mandarMensaje) -> "MandarMensaje";
map_name(leer) -> "Leer".

print_args([]) ->
  "";
print_args([Arg]) ->
  io_lib:format("~p",[Arg]);
print_args([Arg|Rest]) ->
  io_lib:format("~p,~s",[Arg,print_args(Rest)]).

read_entrega_info(CSVFileName) ->
  {ok,File} = file:open(CSVFileName,[read]),
  try
    ecsv:process_csv_file_with
      (File,
       fun process_entrega_info/2,
       [],
       #ecsv_opts{delimiter=$,})
  after file:close(File)
  end.

process_entrega_info({eof},Acc) ->
  Acc;
process_entrega_info({newline,["Group"|_]},Acc) ->
  Acc;
process_entrega_info({newline,L},Acc) when is_list(L) ->
  [Group,Mark,Notes,Date] = L,
  [{Group,Mark,Notes,Date}|Acc];
process_entrega_info({newline,Line}, _Changes) ->
  io:format("*** Error: malformed network line: ~p~n",[Line]),
  throw(bad).

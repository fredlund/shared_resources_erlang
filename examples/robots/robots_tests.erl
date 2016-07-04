-module(robots_tests).

-export([sim/3,rndtest/3]).
-export([sample/1]).
-export([gentest/1, check_test/1]).

%% QuickCheck properties
-export([prop_gentest/0,prop_sample/0]).

-include_lib("eqc/include/eqc.hrl").

sim(N,NumWarehouses,MaxWeight) ->
  %% Common options
  CommonOpts = 
    [
     {num_warehouses,NumWarehouses}
     ,{weight_limit,MaxWeight}
     ,{num_robots,N}
     ,{data_spec,robots_shr}
     ,{waiting_spec,shr_always}
    ],

  %% Start the supervisor
  shr_simple_supervisor:restart(self()),

  %% Start Controller, Environment, ProtocolMachines
  ProtocolPorts = robots_startup:startup(CommonOpts),
  
  %% Finally start the simulation
  robots_sim:run(N,ProtocolPorts,CommonOpts).

rndtest(N,NumWarehouses,MaxWeight) ->
  %% Common options
  CommonOpts = 
    [
     {num_warehouses,NumWarehouses}
     ,{weight_limit,MaxWeight}
     ,{num_robots,N}
    ],

  shr_test_jobs:check_prop
    (
    [
     {test_gen_spec,{robots_grn_rnd,[]}}
     ,{test_corr_spec,{robots_corr_exceptions,[]}}
     ,{notice_exits,false}
    ]++CommonOpts
    ).

sample(Safe) ->
  {Gnr,DataSpec} = 
    if
      Safe -> {robots_gnr_fsm_safe,robots_safe_shr};
      true -> {robots_gnr_fsm,robots_shr}
    end,
  {N,NumWarehouses,MaxWeight} =
    eqc_gen:pick({eqc_gen:choose(1,5),eqc_gen:choose(1,5),1000}),
  %% Common options
  CommonOpts = 
    [
     {num_warehouses,NumWarehouses}
     ,{weight_limit,MaxWeight}
     ,{num_robots,N}
     ,{data_spec,DataSpec}
     ,{waiting_spec,shr_always}
     ,{test_gen_spec,{shr_gnr_fsms,[{N,Gnr}]}}
     ,{ports,lists:map(fun (_) -> [void,void] end, lists:duplicate(N,N))}
    ],
  shr_sample_resource:generate(CommonOpts).

prop_sample() ->
  ?FORALL
     (Bool,
      bool(),
      begin
	sample(Bool), true
      end).

gentest(TestingTime) when is_integer(TestingTime), TestingTime>0 ->
  eqc:quickcheck(eqc:testing_time(TestingTime,prop_gentest())).

prop_gentest() ->
  ?FORALL
     (Test={_Generator,_Protocol},
      gen_combination(),
      begin
	ShouldSucceed = should_succeed(Test),
	io:format("Testing ~p; should succeed=~p~n",[Test,ShouldSucceed]),
	Result = check_test(Test),
	InnerSuccess = (succeeds(Result) == ShouldSucceed),
	if
	  not(InnerSuccess) ->
	    io:format("*** Experiment ~p fails...~n",[Test]),
	    io:format("Expected result ~p~n",[ShouldSucceed]),
	    io:format("Result is~n  ~p~n",[Result]);
	  true ->
	    ok
	end,
	InnerSuccess
      end).

check_test(Test) ->
  eqc:counterexample(innerprop(Test)).

succeeds(Result) ->
  true == Result.

innerprop(Test) ->
  ?FORALL
     ({NumRobots,WarehouseConfig,NoPar},
      {eqc_gen:choose(2,20),gen_warehouse_config(),oneof([[no_par],[]])},

      begin
	CommonOptions =
	  [{num_robots,NumRobots}]
	  ++ WarehouseConfig
	  ++ NoPar,
	GPCGOptions =
	  gen_combination_option(CommonOptions,Test),
	Options =
	  CommonOptions++GPCGOptions,
	eqc:on_output
	  (fun shr_test_jobs:eqc_printer/2,
	   shr_test_jobs:prop_res(Options))
      end).

gen_combination() ->
  Combinations = 
    [ {Generator,Protocol} ||
      Generator <- generators(),
      Protocol <- protocols() ],
  eqc_gen:frequency(lists:map(fun adjust_frequency/1, Combinations)).

gen_combination_option(Options,{Generator,Protocol}) ->
  N = proplists:get_value(num_robots,Options),
  GeneratorOption =
    [{test_gen_spec,{shr_gnr_fsms,[{N,Generator}]}}],
  ProtocolOption = 
    [{start_fun,start_fun(Protocol)}],
  Correctness =
    if
      Protocol == robots_safe_protocol -> robots_safe_shr;
      true -> robots_shr
    end,
  CorrectnessOption =
    [{data_spec,Correctness}],
  GeneratorOption++ProtocolOption++CorrectnessOption++
    [
     {waiting_spec,shr_always}
     ,{is_environment_port,fun ({environment,_}) -> true; (_) -> false end}
     ,{test_corr_spec,shr_corr_resource}
    ].

start_fun(Protocol) ->
  fun (StartOptions) -> 
      robots_startup:startup([{protocol_implementation,Protocol}|StartOptions]) 
  end.

generators() ->
  [robots_gnr_fsm, robots_gnr_fsm_safe].

protocols() ->
  [robots_protocol, robots_safe_protocol].

should_succeed({Generator,Protocol}) ->
  if
    Generator==robots_gnr_fsm_safe, Protocol==robots_safe_protocol -> true;
    true -> false
  end.

adjust_frequency(Item={Generator,Protocol}) ->
  Freq =
    if
      Generator==robots_gnr_fsm, Protocol==robots_protocol -> 7;
      Generator==robots_gnr_fsm_safe, Protocol==robots_safe_protocol -> 7;
      true -> 1
    end,
  {Freq,Item}.

gen_warehouse_config() ->
  ?LET({NumWarehouses,
	MaxWeight},
       {eqc_gen:choose(1,5),
	eqc_gen:oneof([100,200,300,500,1000])},
       [
	{num_warehouses,NumWarehouses}
	,{weight_limit,MaxWeight}
       ]).

  
  

	    

	    

  

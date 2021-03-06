-module(mergesort_tests).

-include_lib("eqc/include/eqc.hrl").
-include("../../src/resources.hrl").

-export([prop_gentest/0]).
-export([prop_run/0]).

-export([test/0]).
-export([test1/0]).
-export([test2/0]).
-export([test3/0]).
-export([test4/0]).
-export([test5/0]).
-export([test6/0]).
-export([debug/0]).
-export([debug2/1]).
-export([debug3/0]).
-export([run/0]).
-export([runs/0]).
-export([runs1/0]).
-export([runs2/0]).
-export([runs3/0]).

mergesort_N_buf_to_2(Scheduler) ->
  #rsystem
    {
     operations=[left,right,output],
     resources=[{r,{shr_resource,{mergesort_n_buf_shr,[2]},Scheduler}}],
     external_mapping=
       fun ({left,Args}) -> {r,{in,[1|Args]}};
	   ({right,Args}) -> {r,{in,[2|Args]}};
	   (Other) -> {r,Other}
       end
    }.

mergesort_N(N,ResourceSpec) ->
  N1Resources = 
    lists:map
      (fun (I) ->
	   {r(I),ResourceSpec}
       end, lists:seq(1,N-1)),
  #rsystem
    {
     operations=[in,output],
     resources=N1Resources,
     pre=fun ({in,[I,_]}) -> (I>0) andalso (I<N+1);
	     ({output,[]}) -> true;
	     (_) -> false
	 end,
     external_mapping=
       fun ({in,[1,Msg]}) -> {r(1),{left,[Msg]}};
	   ({in,[I,Msg]}) -> {r(I-1),{right,[Msg]}};
	   ({output,[]}) -> {r(N-1),{output,[]}}
       end,
     linking=
       if
	 N>2 ->
	   lists:map
	     (fun (Id) -> {{r(Id),output},{r(Id+1),left}} end,
	      lists:seq(1,N-2));
	 true ->
	   []
       end
    }.
  
r(N) ->  
  list_to_atom("r"++integer_to_list(N)).

test() ->
  test(mergesort_n_shr,[no_par]).

test1() ->
  test(mergesort_n_buf_shr,[no_env_wait,no_par,{resource,mergesort_N_buf_to_2(shr_always)}]).

test2() ->
  test(mergesort_n_buf_shr,[no_par]).

test3() ->
  test(mergesort_n_buf_shr,[no_par]).

test4() ->
  test(mergesort_n_buf_shr,[no_par,{generator,mergesort_gnr},{print_testcase,true}]).

test5() ->
  shr_test_jobs:check_prop
    (fun (Opts) -> test_prop(2,mergesort_n_buf_shr,Opts) end,
     [{silent,[{mergesorter,in}]},no_par,{generator,mergesort_gnr}]).

test6() ->
  shr_test_jobs:check_prop
    (fun (Opts) -> test_prop(2,mergesort_n_buf_shr,Opts) end,
     [{imp_scheduler,shr_queue_sched2},
      {silent,[{mergesorter,in}]},
      no_par,
      {generator,mergesort_gnr}]).

test(Specification,Options) ->
  shr_test_jobs:check_prop
    (fun (Opts) -> 
	 ?LET(N,choose(2,5),test_prop(N,Specification,Opts))
     end,
     Options).

test_prop(N,Specification,Options) ->
  Generator = 
    case proplists:get_value(generator,Options) of
      undefined ->
	{shr_gnr_fsms,[{N,mergesort_gnr_fsm_input},mergesort_gnr_fsm_output]};
      G when is_atom(G) ->
	{G,[N,proplists:get_value(no_par,Options,false)]};
      G ->
	G
    end,
  ImpScheduler =
    case proplists:get_value(imp_scheduler,Options) of
      undefined -> shr_always;
      Sched -> Sched
    end,
  Imp =
    case proplists:get_value(imp,Options) of
      undefined -> mergesort_2_shr;
      I -> I
    end,
  Resource =
    case proplists:get_value(resource,Options) of
      undefined -> {shr_resource,Imp,ImpScheduler};
      R -> R
    end,
  shr_test_resource_implementation:prop_tri
    (#rtest
     {
       generator=Generator,
       start_implementation=
	 fun (_) ->
	     shr_supervisor:add_childproc
	       (mergesorter,
		fun () ->
		    shr_composite_resource:start_link
		      (mergesort_N(N,Resource),[],[])
		end)
	 end,
       resource={Specification,[N]},
       options=Options
     }).
  
debug() ->
  shr_debug:debug
    (fun () -> 
	 shr_gen_resource:start(mergesort_2_shr,shr_always,[]) 
     end).

debug2(N) ->
  shr_debug:debug
    (fun () ->
	 shr_composite_resource:start_link(mergesort_N(N,{shr_resource,mergesort_2_shr,shr_always}),[],[])
     end).

debug3() ->
  shr_debug:debug
    (fun () -> 
	 shr_gen_resource:start({mergesort_n_buf_shr,[4]},shr_always,[]) 
     end).

prop_run() ->
  Fun = 
    fun (N,Sequence) -> 
	fun () ->
	    shr_utils:setup_shr(),
	    shr_supervisor:add_childproc
	      (mergesorter,
	       fun () ->
		   shr_composite_resource:start_link
		     (mergesort_N(N,{shr_resource,mergesort_2_shr}),[],[])
	       end),
	    shr_run:run(Sequence,[no_env_wait]) 
	end
    end,
  ?FORALL({N,Sequence},?LET(N,choose(2,10),{N,sequence(N)}),
	  not_exception(Fun(N,Sequence))).

not_exception(Fun) ->
  try Fun(), true
  catch Class:Reason -> 
      io:format
	("*** Error: raised exception~n  ~p:~p~nStacktrace:~n  ~p~n",
	 [Class,Reason,erlang:get_stacktrace()]),
      false 
  end.

sequence(N) -> 
  ?LET(NItems,choose(1,30),
       lists:duplicate(NItems,commands(N))).

commands(N) ->
  ?LET(NCommands,choose(1,3),
       if
	 NCommands==1 -> command(N);
	 true -> lists:duplicate(NCommands,command(N))
       end).

command(N) ->
  oneof([{mergesorter,output,[]},
	 {mergesorter,in,[choose(1,N),choose(1,10)]}]).

run() ->
  shr_utils:setup_shr(),
  shr_supervisor:add_childproc
    (mergesorter,
     fun () ->
	 shr_composite_resource:start_link
	   (mergesort_N(2,{shr_resource,mergesort_2_shr}),[],[])
     end),
  shr_run:print_run
    (shr_run:run
       (
       [
	{mergesorter,in,[1,1]}
       ,{mergesorter,in,[1,2]}
       ,{mergesorter,in,[1,3]}
       ,{mergesorter,in,[2,4]}
       ,{mergesorter,output,[]}
       ,{mergesorter,output,[]}
       ],
       [no_env_wait]
      )).

runs() ->
  Runs =
    (shr_run:runs
       (
       [
	{mergesorter,in,[1,1]}
       ,{mergesorter,in,[1,2]}
       ,{mergesorter,in,[1,3]}
       ,{mergesorter,in,[2,4]}
       ,{mergesorter,output,[]}
       ,{mergesorter,output,[]}
       ],
       fun () ->
	   shr_supervisor:add_childproc
	     (mergesorter,
	      fun () ->
		  shr_composite_resource:start_link
		    (mergesort_N(2,{shr_resource,mergesort_2_shr}),[],[])
	      end)
       end,
       20*1000,
       [no_env_wait]
      )),
  lists:foreach
    (fun (Run) ->
	 io:format("~n"),
	 shr_run:print_run(Run)
     end, Runs).

runs1() ->
  Runs =
    (shr_run:runs
       (
       [
	{1,{mergesorter,in,[1,1]}}
       ,{1,{mergesorter,in,[1,2]}}
       ,{1,{mergesorter,in,[1,3]}}
       ,{mergesorter,in,[2,4]}
       ,{mergesorter,output,[]}
       ,{mergesorter,output,[]}
       ],
       fun () ->
	   shr_supervisor:add_childproc
	     (mergesorter,
	      fun () ->
		  shr_composite_resource:start_link
		    (mergesort_N(2,{shr_resource,mergesort_2_shr}),[],[])
	      end)
       end,
       20*1000,
       [no_env_wait]
      )),
  lists:foreach
    (fun (Run) ->
	 io:format("~n"),
	 shr_run:print_run(Run)
     end, Runs).

runs2() ->
  Runs =
    (shr_run:runs
       (
       [
	{mergesorter,in,[1,1]}
       ,{mergesorter,in,[1,2]}
       ,{mergesorter,in,[1,3]}
       ,{mergesorter,in,[2,4]}
       ,{mergesorter,output,[]}
       ,{mergesorter,output,[]}
       ],
       fun () ->
	   shr_supervisor:add_childproc
	     (mergesorter,
	      fun () ->
		  shr_gen_resource:start_link({mergesort_n_buf_shr,[2]},shr_always,[])	      end)
       end,
       20*1000,
       [no_env_wait]
      )),
  lists:foreach
    (fun (Run) ->
	 io:format("~n"),
	 shr_run:print_run(Run)
     end, Runs).

runs3() ->
  Runs =
    (shr_run:runs
       (
       [
	{mergesorter,in,[1,1]}
       ,{mergesorter,in,[1,2]}
       ,{mergesorter,in,[1,3]}
       ,{mergesorter,in,[2,4]}
       ,{mergesorter,output,[]}
       ,{mergesorter,output,[]}
       ],
       fun () ->
	   shr_supervisor:add_childproc
	     (mergesorter,
	      fun () ->
		  shr_composite_resource:start_link
		    (mergesort_N
		       (2,{shr_resource,mergesort_2_shr,shr_queue_sched2}),
		     [],[])
	      end)
       end,
       20*1000,
       [no_env_wait]
      )),
  lists:foreach
    (fun (Run) ->
	 io:format("~n"),
	 shr_run:print_run(Run)
     end, Runs).

prop_gentest() ->
  ?FORALL
     (Test,
      {
	oneof([mergesort_n_shr,mergesort_n_buf_shr])
      ,oneof([shr_always,shr_queue_sched2,shr_queue_sched1])
      ,bool()
      ,oneof([mergesort_gnr,fsm])
      },
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

should_succeed({_Specification,ImplementationScheduler,Hide,Generator}) ->
  Hide 
    andalso
      ((ImplementationScheduler=/=shr_always)
       orelse (Generator =/= mergesort_gnr)).

check_test(Test) ->     
  eqc:counterexample(innerprop(Test)).

succeeds(Result) ->
  true == Result.

innerprop({Specification,ImplementationScheduler,Hide,Generator}) ->
  ?FORALL
     ({N,NoPar},
      {eqc_gen:choose(2,20),oneof([[no_par],[]])},
      begin
	HideOption =
	  if 
	    Hide -> [{silent,[{mergesorter,in}]}];
	    true -> []
	  end,
	ImplementationSchedulerOption =
	  [{imp_scheduler,ImplementationScheduler}],
	GeneratorOption =
	  if
	    Generator==fsm ->
	      [{shr_gnr_fsms,
		[{N,mergesort_gnr_fsm_input},mergesort_gnr_fsm_output]}];
	    true ->
	      [{generator,Generator}]
	  end,
	Options =
	  NoPar
	  ++ HideOption
	  ++ ImplementationSchedulerOption
	  ++ GeneratorOption,
	eqc:on_output
	  (fun shr_test_jobs:eqc_printer/2,
	   test_prop(N,Specification,Options))
      end).

  
       
    
   
   
  

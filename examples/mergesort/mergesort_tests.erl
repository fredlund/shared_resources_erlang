-module(mergesort_tests).

-include_lib("eqc/include/eqc.hrl").
-include("../../src/resources.hrl").

-export([prop_gentest/0]).

-export([test/0]).
-export([test2/0]).
-export([test3/0]).
-export([debug/0]).
-export([debug2/1]).
-export([debug3/0]).

mergesort_N(N) ->
  N1Resources = 
    lists:map
      (fun (I) ->
	   {r(I),{shr_resource,mergesort_2_shr}}
       end, lists:seq(1,N-1)),
  #rsystem
    {
     operations=[in,output],
     resources=N1Resources,
     pre=fun ({in,[I,_]}) -> (I>0) andalso (I<N+1);
	     ({output,[]}) -> true
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

test2() ->
  test(mergesort_n_buf_shr,[{enforce_progress,false},no_par]).

test3() ->
  test(mergesort_n_buf_shr,[{enforce_progress,true},no_par]).

test(Specification,Options) ->
  shr_test_jobs:check_prop
    (fun (Opts) -> 
	 ?LET(N,choose(2,20),test_prop(N,Specification,Opts))
     end,
     Options).

test_prop(N,Specification,Options) ->
  shr_test_resource_implementation:prop_tri
    (#rtest
     {
       generator=
	 {shr_gnr_fsms,
	  [{N,mergesort_gnr_fsm_input},mergesort_gnr_fsm_output]},
       start_implementation=
	 fun (_) ->
	     shr_supervisor:add_childproc
	       (mergesorter,
		fun () ->
		    shr_composite_resource:start_link(mergesort_N(N),[],[])
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
	 shr_composite_resource:start_link(mergesort_N(N),[],[])
     end).

debug3() ->
  shr_debug:debug
    (fun () -> 
	 shr_gen_resource:start({mergesort_n_buf_shr,[4]},shr_always,[]) 
     end).

prop_gentest() ->
  ?FORALL
     (Test={_Specification,_EnforceProgress},
      {oneof([mergesort_n_shr,mergesort_n_buf_shr]),bool()},
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

should_succeed({mergesort_n_buf_shr,false}) ->
  true;
should_succeed(_) ->
  false.

check_test(Test) ->     
  eqc:counterexample(innerprop(Test)).

succeeds(Result) ->
  true == Result.

innerprop({Specification,EnforceProgress}) ->
  ?FORALL
     ({N,NoPar},
      {eqc_gen:choose(2,20),oneof([[no_par],[]])},
      begin
	Options =
	  [{enforce_progress,EnforceProgress}]
	  ++ NoPar,
	eqc:on_output
	  (fun shr_test_jobs:eqc_printer/2,
	   test_prop(N,Specification,Options))
      end).

  
       
    
   
   
  

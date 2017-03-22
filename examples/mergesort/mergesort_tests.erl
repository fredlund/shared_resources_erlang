-module(mergesort_tests).

-include_lib("eqc/include/eqc.hrl").
-include("../../src/rsystem.hrl").

-export([test/0]).
-export([test2/0]).
-export([test3/0]).
-export([testN/0]).
-export([debug/0]).
-export([debugs/0]).
-export([debug2/0]).
-export([debugN/1]).

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

mergesort_4() ->
  #rsystem
    {
     operations=[in,output],
     resources=[{r1,{shr_resource,mergesort_2_shr}},
		{r2,{shr_resource,mergesort_2_shr}},
		{r3,{shr_resource,mergesort_2_shr}}],
     pre=fun ({in,[I,_]}) -> (I>0) andalso (I<5);
	     ({output,[]}) -> true
	 end,
     external_mapping=
       fun ({in,[1,Msg]}) -> {r1,{left,[Msg]}};
	   ({in,[2,Msg]}) -> {r1,{right,[Msg]}};
	   ({in,[3,Msg]}) -> {r2,{left,[Msg]}};
	   ({in,[4,Msg]}) -> {r2,{right,[Msg]}};
	   ({output,[]}) -> {r3,{output,[]}}
       end,
     linking=[{{r1,output},{r3,left}}, {{r2,output},{r3,right}}]
    }.

test() ->
  test(mergesort_n_shr,[]).

test2() ->
  test(mergesort_n_buf_shr,[{enforce_progress,false}]).

test3() ->
  test(mergesort_n_buf_shr,[{enforce_progress,true}]).

test(Specification,Options) ->
  shr_test_jobs:check_prop
    (fun (Options) ->
	shr_test_resource_implementation:prop_tri
	  ({shr_gnr_fsms,
	    [{4,mergesort_gnr_fsm_input},
	     mergesort_gnr_fsm_output]},
	   fun (_) ->
	       [MergeSorter] =
		 shr_simple_supervisor:add_childproc
		   (implementation,
		    fun () ->
			shr_composite_resource:start_link(mergesort_4(),[],[])
		    end),
	       shr_register:register(mergesorter,MergeSorter)
	   end,
	   void,
	   {Specification,[4]},
	   void,
	   void,
	   Options)
     end,[no_par|Options]).

testN() ->
  shr_test_jobs:check_prop
    (fun (Options) ->
	 ?LET(N,
	      choose(2,20),
	      shr_test_resource_implementation:prop_tri
		({shr_gnr_fsms,
		  [{N,mergesort_gnr_fsm_input},
		   mergesort_gnr_fsm_output]},
		 fun (_) ->
		     [MergeSorter] =
		       shr_simple_supervisor:add_childproc
			 (implementation,
			  fun () ->
			      shr_composite_resource:start_link(mergesort_N(N),[],[])
			  end),
		     shr_register:register(mergesorter,MergeSorter)
		 end,
		 void,
		 {mergesort_n_buf_shr,[N]},
		 void,
		 void,
		 Options))
     end,[no_par|[{enforce_progress,false}]]).

debug() ->
  shr_simple_supervisor:restart(self()),
  shr_simple_supervisor:add_childproc
    (shr_register,
     fun () -> shr_register:start_link() end),
  [MergeSorter] =
    shr_simple_supervisor:add_childproc
      (mergesort_2,
       fun () -> shr_gen_resource:start(mergesort_2_shr,shr_always,[]) end),
  shr_debug:debug(MergeSorter).

debugs() ->
  shr_simple_supervisor:restart(self()),
  shr_simple_supervisor:add_childproc
    (shr_register,
     fun () -> shr_register:start_link() end),
  [MergeSorter] =
    shr_simple_supervisor:add_childproc
      (mergesort_2,
       fun () -> shr_gen_resource:start({mergesort_n_buf_shr,[4]},shr_always,[]) end),
  shr_debug:debug(MergeSorter).

debug2() ->
  shr_simple_supervisor:restart(self()),
  shr_simple_supervisor:add_childproc
    (shr_register,
     fun () -> shr_register:start_link() end),
  [MergeSorter] =
    shr_simple_supervisor:add_childproc
      (mergesort_4,
       fun () -> 
	   shr_composite_resource:start_link(mergesort_4(),[],[])
       end),
  shr_debug:debug(MergeSorter).

debugN(N) ->
  shr_simple_supervisor:restart(self()),
  shr_simple_supervisor:add_childproc
    (shr_register,
     fun () -> shr_register:start_link() end),
  [MergeSorter] =
    shr_simple_supervisor:add_childproc
      (mergesort_N,
       fun () -> 
	   shr_composite_resource:start_link(mergesort_N(N),[],[])
       end),
  shr_debug:debug(MergeSorter).

  
  
  
       
    
   
   
  

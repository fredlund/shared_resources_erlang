-module(multibuffer_tests).

-include_lib("eqc/include/eqc.hrl").
-include("../../src/corr_resource_state.hrl").

-export([prop_gentest/0]).
-export([test_combinations/0]).
-export([check_test/1]).

prop_gentest() ->
  ?FORALL
    (Test,
     oneof(combinations()),
     begin
       ShouldSucceed = should_succeed(Test),
       io:format("Checking ~p; should_succeed=~p~n",[Test,ShouldSucceed]),
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
     
combinations() ->
  [
   {Implementation,Scheduler,EnforceProgress} ||
    Implementation <- implementations(),
    Scheduler <- schedulers(),
    EnforceProgress <- [true,false]
  ].

test_combinations() ->
  lists:foreach
    (fun (Combination={Implementation,Scheduler,EnforceProgress}) ->
	 io:format
	   ("~nImplementation: ~p Scheduler: ~p Enforce progress: ~p ~s~n~n",
	    [Implementation,
	     Scheduler,
	     EnforceProgress,
	     case check_test(Combination) of
	       true ->
		 "SUCCEEDS";
	       _ ->
		 "FAILS"
	     end])
     end, combinations()).

succeeds(true) ->
  true;
succeeds(_) ->
  false.

check_test(Test) ->
  eqc:counterexample(innerprop(Test)).

innerprop({Implementation,Scheduler,EnforceProgress}) ->
  ?FORALL
     ({Capacity,NumReaders,NumWriters},
      {choose(1,15),choose(0,10),choose(0,10)},
      begin
	ResourceSpec =
	  {multibuffer_shr,[Capacity]},
	Gnr =
	  [{test_gen_spec,
	    {shr_gnr_fsms,
	     [{NumReaders,
	       {multibuffer_reader_fsm,[Capacity]}},
	      {NumWriters,
	       {multibuffer_writer_fsm,[Capacity]}}]}}],
	DataSpec =
	  [{data_spec,ResourceSpec}],
	WaitSpec =
	  [{waiting_spec,Scheduler}],
	EnforceProgressSpec =
	  [{enforce_progress,EnforceProgress}],
	CorrSpec =
	  [{test_corr_spec,shr_corr_resource}],
	Start =
	  [{start_fun,
	    fun (_) ->
		io:format
		  ("start_fun is called~n",
		   []),
		Result =
		  shr_simple_supervisor:add_childproc
		    (Implementation, 
		     fun () -> Implementation:start_link([NumReaders+NumWriters,Capacity],[]) end),
		[_|APIs] = Result,
		lists:foreach
		  (fun ({Id,Pid}) -> 
		       shr_register:register({multibuffer,Id},Pid) 
		   end,
		   lists:zip(lists:seq(1,length(APIs)),APIs))
	    end}],
	Limit =
	  [{limit_card_state,
	    fun (State) ->
		Length = length(State#corr_res_state.states),
		Length > 128
	    end}],
	Options =
	  Gnr
	  ++DataSpec
	  ++WaitSpec
	  ++EnforceProgressSpec
	  ++CorrSpec
	  ++Limit
	  ++Start,
	eqc:on_output
	  (fun shr_test_jobs:eqc_printer/2,
	   shr_test_jobs:prop_res(Options))
      end).

should_succeed({Implementation,Scheduler,EnforceProgress}) ->
  if
    Implementation==multbuf1 ->
      Scheduler==shr_always;

    Implementation==multbuf2 ->
      not(EnforceProgress) orelse (Scheduler==shr_fcfs);

    Implementation==multbuf3 ->
      (Scheduler=/=shr_fcfs) andalso (Scheduler=/={shr_smallest_first,[multibuffer_shr]})
  end.

implementations() ->
  [
   multbuf1
   ,multbuf2
   ,multbuf3
  ].

schedulers() ->
  [shr_always,shr_fcfs,{shr_queue_sched1,[multibuffer]},{shr_queue_sched2,[multibuffer]},{shr_smallest_first,[multibuffer]}].


  
  




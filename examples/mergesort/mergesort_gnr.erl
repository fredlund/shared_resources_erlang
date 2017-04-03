-module(mergesort_gnr).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(shr_gnr_implementation).

%%-define(debug,true).
-include("../../src/debug.hrl").
-include("../../src/tester.hrl").

-define(MAX,20).

-compile(export_all).

-record(state,
	{
	  inputs,
	  output_blocked,
	  parallel,
	  n_inputs
	}).

-include("../../src/shr_qcgen_to_gen.hrl").

initial_state([NInputs,PermitParallelCalls]) ->
  #state
    {
     inputs=list_to_tuple(lists:duplicate(NInputs,0))
    ,output_blocked=false
    ,parallel=PermitParallelCalls
    ,n_inputs=NInputs
    }.

output_pre(State) ->
  not(State#state.output_blocked).

output_args(_State) ->
  [mergesorter].

output_next(State,Result,_Args) ->
  output_unblocked(Result,State#state{output_blocked=true}).

in_pre(_State) ->
  true.

in_args(State) ->
  ?LET(N,eqc_gen:choose(1,State#state.n_inputs),
       begin
	 B = element(N,State#state.inputs),
	 ?LET(E,eqc_gen:choose(B,max(B+1,?MAX)),
	      [mergesorter,N,eqc_gen:choose(B,E)])
       end).

in_next(State,Result,[N,Value]) ->
  output_unblocked
    (Result,
     State#state{inputs=setelement(N,State#state.inputs,Value)}).

output_unblocked(Result,State) ->
  {_NewJobs,FinishedJobs} = Result,
  case lists:any(fun (Job) -> 
		     case Job#job.call of
		       {_,output,_} -> true;
		       {_,output,_,_} -> true;
		       _ -> false
		     end
		 end, FinishedJobs) of
    true -> State#state{output_blocked=false};
    false -> State
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Job,_TS) ->
  {_,Operation,Args} = Job#job.call,
  shr_utils:print_mfa({Operation,Args}).

print_started_job_info(Job,_TS) ->
  {_,F,Args} = Job#job.call,
  shr_utils:print_mfa({F,Args}).

print_state(State) ->
  io_lib:format
    ("{inputs=~p,output_blocked=~p,parallel=~p,n_inputs=~p}~n",
     [
      State#state.inputs
     ,State#state.output_blocked
     ,State#state.parallel
     ,State#state.n_inputs
     ]).




  
	 


-module(mergesort_gnr).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(shr_gnr_implementation).

%%-define(debug,true).
-include("../../src/debug.hrl").
-include("../../src/tester.hrl").

-compile(export_all).

-record(gstate,
	{
	  inputs,
	  output_blocked,
	  parallel
	}).

initial_state({NInputs,PermitParallelCalls}) ->
  #gstate
    {
     inputs=lists:duplicate(NInputs,0)
     ,output_blocked=false
     ,parallel=PermitParallelCalls
    }.



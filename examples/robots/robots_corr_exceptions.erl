%%% @author fred <fred@uppsala.localdomain>
%%% @copyright (C) 2016, fred
%%% @doc
%%%
%%% @end
%%% Created : 17 Feb 2016 by fred <fred@uppsala.localdomain>

-module(robots_corr_exceptions).

-behaviour(shr_corr_implementation).

-include_lib("eqc/include/eqc.hrl").

-include("../../src/tester.hrl").

-export([initial_state/2,postcondition/4,next_state/4]).

%%-define(debug,true).
-include("../../src/debug.hrl").


-record(state,{num_warehouses,weight_limit,num_robots,started}).

initial_state(_,Options) ->
  #state
    {
      num_robots=proplists:get_value(num_robots,Options)
      ,num_warehouses=proplists:get_value(num_warehouses,Options)
      ,weight_limit=proplists:get_value(weight_limit,Options)
    }.

postcondition(_State, _Value, {_,FinishedJobs}, _TS) ->
  lists:all
    (fun (Job) ->
	 case Job#job.result of
	   {exit,_Pid,controller_call} ->
	     true;
	   {exit,_Pid,{protocol_postcondition_failed,_}} ->
	     true;
	   {exit,_,Reason} ->
	     io:format("Bad exception ~p~n",[Reason]),
	     false;
	   _Other ->
	     true
	 end
     end, FinishedJobs).

next_state(State,_Result,_Args,_TS) ->
  State.

  


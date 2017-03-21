-module(mergesort_gnr_fsm_output).

-export([initial_state/3,precondition/4,command/3,next_state/4]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

%%-define(debug,true).
-include("../../src/debug.hrl").

-include_lib("eqc/include/eqc.hrl").

initial_state(_,_,_) -> 
  {ok,output}.

precondition(Id,_,_,{_,_,_}) ->
  true.

command(Id,output,_GlobalState) ->
  {mergesorter,output,[]}.

next_state(Id,State,GS,{_,_,_}) ->
  {State,GS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[Id,_]} -> io_lib:format("~p",[Id])
  end.

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  shr_utils:print_mfa(Call).

print_state(_,_State,_IsBlocked) ->
  "".


  


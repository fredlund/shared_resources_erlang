-module(mergesort_gnr_fsm_input).

-export([initial_state/3,precondition/4,command/3,next_state/4]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

%%-define(debug,true).
-include("../../src/debug.hrl").

-include_lib("eqc/include/eqc.hrl").

initial_state(_,_,_) ->
  {ok,0}.

precondition(Id,N,_,{_,_,[_Id,N1]}) when is_integer(N) ->
  N1 >= N.

command(Id,N,_GlobalState) when is_integer(N) ->
  {mergesorter,in,[Id,eqc_gen:choose(N,1000)]}.

next_state(Id,N,GS,{_,_,[_Id,N]}) when is_integer(N) ->
  N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[Id,_]} -> io_lib:format("~p",[Id])
  end.

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  shr_utils:print_mfa(Call).

print_state(_,N,_IsBlocked) when is_integer(N) ->
  io_lib:format("~p~n",[N]).



  


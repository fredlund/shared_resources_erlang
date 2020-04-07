-module(tick_gnr_fsm).

-export([initial_state/3,precondition/5,command/4,next_state/5]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

initial_state(_,_,_) ->
  {ok,0}.
 
precondition(_,_,_,_,_) ->
  true.

command(_,_,_,_) ->
  {controller,tick,[]}.

next_state(_,State,GlobalState,_,_) ->
  {State+1,GlobalState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  "tick()".

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  shr_utils:print_mfa(Call).

print_state(_,State,IsBlocked) -> 
  if
    IsBlocked -> "";
    true -> io_lib:format("~p",[State])
  end.


  

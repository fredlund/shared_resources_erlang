-module(lector_gnr_fsm).

-export([initial_state/3,precondition/5,command/4,next_state/5]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

%%-define(debug,true).
-include("debug.hrl").

-include_lib("eqc/include/eqc.hrl").

-record(state,{uid}).

initial_state(Id,_,_Options) ->
  {ok,#state{uid=Id}}.

precondition(_Id,State,_GS,CorrState,Command) ->
  true.

command(_Id,State,GlobalState,CorrState) ->
  ResState = escritor_gnr_fsm:res_state(CorrState),
  {controller,leer,[State#state.uid]}.
      
next_state(_Id,State,GlobalState,CorrState,_) ->
  {State,GlobalState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[I|_]} -> io_lib:format("l(~p)",[I])
  end.

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[I|_]} -> io_lib:format("l(~p): ~s",[I,shr_utils:print_mfa(Call)])
  end.

print_state(_,State,IsBlocked) ->      
  if
    IsBlocked -> "";
    true -> io_lib:format("lector(~p)",[State#state.uid])
  end.




  

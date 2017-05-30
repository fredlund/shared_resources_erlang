-module(cinta_gnr_fsm).

-export([initial_state/3,precondition/4,command/3,next_state/4]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

%%-define(debug,true).
-include("debug.hrl").


-include_lib("eqc/include/eqc.hrl").


-record(state,{next=solicitarAvance}).

initial_state(_Id,_,_Options) ->
  {ok,#state{}}.

precondition(_Id,State,_GlobalState,Command) ->
  {_,Type,_} = Command,
  State#state.next == Type.

command(_Id,State,_GlobalState) ->
  case State#state.next of
    solicitarAvance ->
      {controller,solicitarAvance,[]};
    contenedorNuevo ->
      {controller,contenedorNuevo,[]}
  end.

next_state(_Id,State,GlobalState,_) ->
  NextLocalState =
    case State#state.next of
      solicitarAvance -> State#state{next=contenedorNuevo};
      _ -> State#state{next=solicitarAvance}
    end,
  {NextLocalState,GlobalState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(_,_Id,_State,_GlobalState) ->
  "cinta".

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  shr_utils:print_mfa(Call).

print_state(_,State,IsBlocked) ->      
  if
    IsBlocked -> "";
    true -> io_lib:format("~p",[State#state.next])
  end.




  

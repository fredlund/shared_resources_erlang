-module(car_gnr_fsm).

-export([initial_state/3,precondition/5,command/4,next_state/5]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

-record(state,{distance,carriles,doing,car,velocidad,pos}).

initial_state(_Id,[Car,Velocidad],Options) ->
  {carretera_shr,DataOpts} = proplists:get_value(data_spec,Options),
  Distance = proplists:get_value(distance,DataOpts),
  Carriles = proplists:get_value(carriles,DataOpts),
  {
    ok,
    #state
    {distance=Distance,carriles=Carriles,
     doing=outside,car=Car,velocidad=Velocidad}
  }.
 
precondition(_,_,GlobalState,_CorrState,_) ->
  true.

command(_Id,#state{distance=Distance,doing=Doing,pos=Pos,car=Car,velocidad=Velocidad}=State,_GlobalState,_CorrState) ->
  case State#state.doing of
    outside -> {controller,enter,[Car,Velocidad]};
    moving -> {controller,moving,[Car]};
    move when Pos<Distance-1 -> {controller,move,[Car,Velocidad]};
    move -> {controller,exit,[Car]};
    exited -> stopped
  end.

next_state(MachineId,#state{pos=Pos}=State,GlobalState,_,Call) ->
  NewLocalState =
    case Call of
      {_,enter,_} ->
        State#state{doing=moving,pos=0};
      {_,moving,_} ->
        State#state{doing=move};
      {_,move,_} ->
        State#state{doing=moving,pos=Pos+1};
      {_,exit,_} ->
        State#state{doing=exited,pos=Pos+1}
    end,
  {NewLocalState,GlobalState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[Car|_]} -> io_lib:format("~p",[Car])
  end.

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  shr_utils:print_mfa(Call).

print_state(_,State,IsBlocked) -> 
  if
    IsBlocked -> "";
    true -> io_lib:format("~p",[State])
  end.


  

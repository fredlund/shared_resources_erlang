-module(robot_gnr_fsm).

-export([initial_state/3,precondition/4,command/3,next_state/4]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

%%-define(debug,true).
-include("debug.hrl").

-define(MIN_FACTOR,1000).

-include_lib("eqc/include/eqc.hrl").


-record(state,{i,min_p_pieza,max_p_contenedor,next=notificarPeso}).

initial_state(Id,[_N,Min_P_Pieza,Max_P_Contenedor],_Options) ->
  if
    (Min_P_Pieza rem ?MIN_FACTOR) == 0,
    (Max_P_Contenedor rem ?MIN_FACTOR) == 0,
    Min_P_Pieza < Max_P_Contenedor ->
      {ok,#state{i=Id,min_p_pieza=Min_P_Pieza,max_p_contenedor=Max_P_Contenedor}}
  end.

precondition(_Id,State,_GS,Command) ->
  {_,Type,_} = Command,
  State#state.next == Type.

command(_Id,State,_GlobalState) ->
  case State#state.next of
    notificarPeso ->
      {controller,notificarPeso,
       [State#state.i-1,new_weight(State)]};
    permisoSoltar ->
      {controller,permisoSoltar,
       [State#state.i-1]}
  end.

next_state(_Id,State,GlobalState,_) ->
  NextLocalState =
    case State#state.next of
      notificarPeso -> State#state{next=permisoSoltar};
      _ -> State#state{next=notificarPeso}
    end,
  {NextLocalState,GlobalState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_weight(State) ->
  ?LET
     (Factor,
      choose(State#state.min_p_pieza div ?MIN_FACTOR,State#state.max_p_contenedor div ?MIN_FACTOR),
      Factor*?MIN_FACTOR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[I|_]} -> io_lib:format("~p",[I])
  end.

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  shr_utils:print_mfa(Call).

print_state(_,State,IsBlocked) ->      
  if
    IsBlocked -> "";
    true -> io_lib:format("~p",[State#state.next])
  end.




  

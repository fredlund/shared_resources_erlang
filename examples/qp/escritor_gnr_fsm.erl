-module(escritor_gnr_fsm).

-export([initial_state/3,precondition/5,command/4,next_state/5]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).
-export([initial_global_state/0,global_constraint/3]).
-export([res_state/1]).

-behaviour(shr_fsm).

%%-define(debug,true).
-include("debug.hrl").

-include_lib("eqc/include/eqc.hrl").
-include("corr_resource_state.hrl").

%% local state
-record(state,{uid}).

initial_state(Id,_,_Options) ->
  {ok,#state{uid=Id}}.

initial_global_state() ->
  [].
  %%#global_state{}.

global_constraint(Cmds,GlobalState,CorrState) ->
  true.

precondition(_Id,State,_GS,CorrState,Command) ->
  true.

command(_Id,State,GlobalState,CorrState) ->
  Uid = State#state.uid,
  ResState = res_state(CorrState),
  GrupoParaCrear =
    case grupo_para_crear(Uid,ResState) of
      [] -> 
        {1,{controller,crearGrupo,[Uid,oneof(group_names())]}};
      GPP when is_list(GPP) ->
        {30,{controller,crearGrupo,[Uid,oneof(GPP)]}}
    end,
  MiembrosParaAnadir =
    case miembros_para_anadir(Uid,ResState) of
      [] -> 
        {1,{controller,anadirMiembro,[Uid,oneof(group_names()),uid_name()]}};
      GM when is_list(GM) -> 
	io:format("miembros para anadir=~p~n",[GM]),
        {30,
        ?LET({Grupo,Miembros},
             oneof(GM),
             ?LET(NuevoMiembro,oneof(Miembros),
                  {controller,anadirMiembro,[Uid,Grupo,NuevoMiembro]}))}
    end,
  GruposParaSalir = 
    case grupos_para_salir(Uid,ResState) of
      [] -> 
        {1,{controller,salirGrupo,[Uid,oneof(group_names())]}};
      Grupos when is_list(Grupos) -> 
        {5,
         ?LET(Grupo,oneof(Grupos),
              {controller,salirGrupo,[Uid,Grupo]})}
    end,
  MandaMensaje =
    case grupos_miembro(Uid,ResState) of
      [] -> 
        {1,{controller,mandarMensaje,[Uid,oneof(group_names()),mensaje()]}};
      Grs when is_list(Grs) -> 
        {30,
         ?LET(Grupo,oneof(Grs),
              {controller,mandarMensaje,[Uid,Grupo,mensaje()]})}
    end,
  Commands =
    [
      GrupoParaCrear,
      MiembrosParaAnadir,
      GruposParaSalir,
      MandaMensaje
    ],
  frequency(Commands).

next_state(_Id,State,GlobalState,CorrState,_Call) ->
  {State,GlobalState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

res_state(CorrState) ->
  States = CorrState#corr_res_state.states,
  [OneState|_] = States,
  OneState#onestate.sdata.

grupo_para_crear(Uid,ResState) ->
  group_names()--qp_shr:nombres_grupos(ResState).

miembros_para_anadir(Uid,ResState) ->
  MyGroups = 
    lists:filter
      (fun (GroupName) -> qp_shr:creador(GroupName,ResState)==Uid end, 
       qp_shr:nombres_grupos(ResState)),
  lists:foldl
    (fun (GrupoNombre,Acc) ->
         RemainingUsers = 
           qp_shr:uid_usarios(ResState) -- qp_shr:miembros(GrupoNombre,ResState),
         if
           RemainingUsers == [] ->
             Acc;
           true ->
             [{GrupoNombre,RemainingUsers}|Acc]
         end
     end, [], MyGroups).

grupos_para_salir(Uid,ResState) ->
  lists:filter
    (fun (GroupName) -> 
         qp_shr:creador(GroupName,ResState)=/=Uid
           andalso lists:member(Uid,qp_shr:miembros(GroupName,ResState))
     end, qp_shr:nombres_grupos(ResState)).

grupos_miembro(Uid,ResState) ->
  lists:filter
    (fun (GroupName) -> 
         lists:member(Uid,qp_shr:miembros(GroupName,ResState))
     end, qp_shr:nombres_grupos(ResState)).

group_names() ->
  ["grupoA","grupoB","grupoC"].
uid_name() ->
  choose(1,10).

mensaje() ->
  oneof(["hola","hej","hi","bonjour","goddag","guten tag"]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[I|_]} -> io_lib:format("e(~p)",[I])
  end.

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[I|_]} -> io_lib:format("e(~p): ~s",[I,shr_utils:print_mfa(Call)])
  end.

print_state(_,State,IsBlocked) ->      
  if
    IsBlocked -> "";
    true -> io_lib:format("~p",[State])
  end.

my_oneof([]) ->
  try 2/0
  catch _:_ ->
      io:format("oneof([]): stacktrace:~n~p~n",[erlang:get_stacktrace()]),
      error(bad)
  end;
my_oneof(L) when is_list(L) ->
  oneof(L).





  

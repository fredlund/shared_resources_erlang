-module(qp_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).
-export([usario/2,nombres_grupos/1,uid_usarios/1,miembros/2,creador/2]).
-export([mensaje/3]).

-record(state,{grupos=[],usarios=[]}).
-record(grupo,{nombre,creador,miembros}).
-record(usario,{uid,mensajes}).
-record(mensaje,{remitente,grupo,contenidos}).

%%-define(debug,true).
-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


initial_state(_,_) ->
  #state{}.

pre({crearGrupo,[Creador,NombreGrupo]},State) ->
  not(lists:member(NombreGrupo,nombres_grupos(State)));
pre({anadirMiembro,[Creador,NombreGrupo,Miembro]},State) ->
  lists:member(NombreGrupo,nombres_grupos(State))
    andalso not(lists:member(Miembro,miembros(NombreGrupo,State)))
    andalso Creador == creador(NombreGrupo,State);
pre({salirGrupo,[Miembro,NombreGrupo]},State) ->
  lists:member(NombreGrupo,nombres_grupos(State))
    andalso creador(NombreGrupo,State)=/=Miembro
    andalso lists:member(Miembro,miembros(NombreGrupo,State));
pre({mandarMensaje,[Miembro,NombreGrupo,_Mensaje]},State) ->
  lists:member(NombreGrupo,nombres_grupos(State))
    andalso lists:member(Miembro,miembros(NombreGrupo,State));
pre({leer,[Uid]},State) ->
  true.

cpre({crearGrupo,[_Creador,_NombreGrupo]},_State) ->
  true;
cpre({anadirMiembro,[_Creador,_NombreGrupo,_Miembro]},_State) ->
  true;
cpre({salirGrupo,[_Miembro,_NombreGrupo]},_State) ->
  true;
cpre({mandarMensaje,[_Miembro,_NombreGrupo,_Mensaje]},_State) ->
  true;
cpre({leer,[Uid]},State) ->
  tiene_mensaje_por_leer(Uid,State).

post({crearGrupo,[Creador,NombreGrupo]},_Return,State) ->
  NewState = ensure_exists(Creador,State),
  anadir_grupo(Creador,NombreGrupo,NewState);
post({anadirMiembro,[Creador,NombreGrupo,Miembro]},_Return,State) ->
  NewState = ensure_exists(Miembro,State),
  modificar_grupo(anadirMiembro(Miembro,NombreGrupo,NewState),NewState);
post({salirGrupo,[Miembro,NombreGrupo]},_Return,State) ->
  NewState = 
    modificar_grupo(borrar_miembro(Miembro,NombreGrupo,State),State),
  modificar_usario
    (borrar_mensajes_desde_grupo(Miembro,NombreGrupo,NewState),
     NewState);
post({mandarMensaje,[Miembro,NombreGrupo,Mensaje]},_Return,State) ->
  lists:foldl
    (fun (NewUsario,S) -> modificar_usario(NewUsario,S) end,
     State,
     anadir_mensaje(Mensaje,Miembro,NombreGrupo,State));
post({leer,[Uid]},_Return,State) ->
  {_Mensaje,NewState} = leer(Uid,State),
  NewState.

return(State,Call,Result) ->
  case pre(Call,State) of
    false ->
      Result=={exception,'cc.qp.PreconditionFailedException'};
    true ->
      case Call of
        {leer,[Uid]} ->
          {Mensaje,_State} = leer(Uid,State),
          Result==Mensaje;
        _ ->
          Result==void
      end
  end.

return_value(Call,State) ->
  case Call of
    {leer,[Uid]} ->
      {Mensaje,_State} = leer(Uid,State),
      Mensaje;
    _ ->
      void
  end.

print_state(State) ->
  io_lib:format
    ("~p~n",[State]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grupos(State) ->
  State#state.grupos.

set_grupos(Grupos,State) ->
  State#state{grupos=Grupos}.

nombres_grupos(State) ->
  lists:map(fun nombre/1, grupos(State)).

grupo(Nombre,State) ->
  case lists:keyfind(Nombre,#grupo.nombre,grupos(State)) of
    Grupo when is_record(Grupo,grupo) ->
      Grupo;
    false ->
      io:format
        ("~n***Error: grupo ~p not found in state~n~p~n",
         [Nombre,State]),
      error(bad);
    _ ->
      io:format
        ("~n***Error: state is malformed; grupo ~p stored in state~n~p~n",
         [Nombre,State]),
      error(bad)
  end.

usarios(State) ->
  State#state.usarios.

set_usarios(Usarios,State) ->
  State#state{usarios=Usarios}.

uid_usarios(State) ->
  lists:map(fun uid/1, usarios(State)).

uid(Usario) ->
  Usario#usario.uid.

modificar_usario(Usario,State) ->
  set_usarios
    (lists:keystore(uid(Usario),#usario.uid,usarios(State),Usario),
     State).

mensajes(Usario) ->
  Usario#usario.mensajes.

borrar_mensaje(Usario) ->
  [_|Rest] = mensajes(Usario),
  Usario#usario{mensajes=Rest}.

usario(Uid,State) ->
  lists:keyfind(Uid,#usario.uid,usarios(State)).

creador(Nombre,State) ->
  (grupo(Nombre,State))#grupo.creador.

miembros(Nombre,State) ->
  miembros(grupo(Nombre,State)).

miembros(Grupo) ->
  Grupo#grupo.miembros.

nombre(Grupo) ->
  Grupo#grupo.nombre.

remitente(Mensaje) ->
  Mensaje#mensaje.remitente.

grupo(Mensaje) ->
  Mensaje#mensaje.grupo.

contenidos(Mensaje) ->
  Mensaje#mensaje.contenidos.

modificar_grupo(Grupo,State) ->
  set_grupos
    (lists:keystore(nombre(Grupo),#grupo.nombre,grupos(State),Grupo),
     State).

anadir_grupo(Creador,NombreGrupo,State) ->
  Grupo = #grupo{nombre=NombreGrupo,creador=Creador,miembros=[Creador]},
  set_grupos([Grupo|grupos(State)],State).

anadirMiembro(Usario,Nombre,State) ->
  Grupo = grupo(Nombre,State),
  Grupo#grupo{miembros=[Usario|miembros(Grupo)]}.

borrar_miembro(Usario,Nombre,State) ->
  Grupo = grupo(Nombre,State),
  Grupo#grupo{miembros=lists:delete(Usario,miembros(Grupo))}.

borrar_mensajes_desde_grupo(Uid,NombreGrupo,State) ->
  Usario = usario(Uid,State),
  NewMensajes = 
    lists:filter
      (fun (Msg) -> Msg#mensaje.grupo =/= NombreGrupo end,
       Usario#usario.mensajes),
  Usario#usario{mensajes=NewMensajes}.

tiene_mensaje_por_leer(Uid,State) ->
  case usario(Uid,State) of
    false -> false;
    Usario -> mensajes(Usario) =/= []
  end.

anadir_mensaje(Mensaje,Remitente,NombreGrupo,State) ->
  Grupo = grupo(NombreGrupo,State),
  Miembros = miembros(Grupo),
  NewMensajes = 
    [
     {To,
      #mensaje
      {
        contenidos=Mensaje,
        remitente=Remitente,
        grupo=NombreGrupo
      }
     }
     || To <- Miembros
    ],
  lists:map
    (fun ({Destinatario,NewMensaje}) ->
         Usario = usario(Destinatario,State),
         Usario#usario{mensajes=mensajes(Usario)++[NewMensaje]}
     end, NewMensajes).

leer(Uid,State) ->
  Usario = usario(Uid,State),
  [Msg|_] = mensajes(Usario),
  {Msg,modificar_usario(borrar_mensaje(Usario),State)}.

ensure_exists(Uid,State) ->
  case usario(Uid,State) of
    false ->
      State#state{usarios=[#usario{uid=Uid,mensajes=[]}|State#state.usarios]};
    _ ->
      State
  end.

mensaje(Remitente, Grupo, Contenidos) ->
  #mensaje
    {
     remitente=Remitente,
     grupo=Grupo,
     contenidos=Contenidos
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




  

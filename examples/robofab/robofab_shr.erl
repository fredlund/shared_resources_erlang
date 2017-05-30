-module(robofab_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(state,{peso,pendientes,min_p_pieza,max_p_contenedor,n}).

%%-define(debug,true).
-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


initial_state([N,Min_P_Pieza,Max_P_Contenedor],_) ->
  ?LOG("N=~p Max_P_Contenedor=~p~n",[N,Max_P_Contenedor]),
  #state
    {
     peso=0
     ,pendientes=list_to_tuple(lists:duplicate(N,0))
     ,min_p_pieza=Min_P_Pieza
     ,max_p_contenedor=Max_P_Contenedor
     ,n=N
    }.


pre(_,_) -> true.

cpre({permisoSoltar,[I]},State) ->
  peso(State)+pendiente(I,State) =< max_p_contenedor(State);
cpre(_Msg={solicitarAvance,[]},State) ->
  ?LOG("~p state=~s~n",[_Msg,print_state(State)]),
  lists:all(fun (I) -> 
		peso(State)+pendiente(I,State) > max_p_contenedor(State)
	    end, lists:seq(0,n(State)-1));
cpre({notificarPeso,_},_) -> 
  true;
cpre({contenedorNuevo,_},_) -> 
  true.

post({notificarPeso,[I,P]},_Return,State) ->
  set_pendiente(I,P,State);
post({permisoSoltar,[I]},_Return,State) ->
  PesoI = pendiente(I,State),
  set_peso(PesoI+peso(State),set_pendiente(I,0,State));
post({contenedorNuevo,[]},_Return,State) ->
  set_peso(0,State);
post(_,_,State) ->
  State.

return(_State,_Call,Result) ->
  Result==void.

return_value(_Call,_State) ->
  void.
  
print_state(State) ->
  io_lib:format
    ("{n=~p,max_p_contenedor=~p,peso=~p,pendientes=~p)~n",
     [n(State),max_p_contenedor(State),peso(State),pendientes(State)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pendientes(State) ->
  State#state.pendientes.

pendiente(I,State) ->
  element(I+1,pendientes(State)).

peso(State) ->
  State#state.peso.

max_p_contenedor(State) ->		
  State#state.max_p_contenedor.

n(State) ->
  State#state.n.

set_pendiente(I,P,State) ->
  State#state{pendientes=setelement(I+1,pendientes(State),P)}.

set_peso(Peso,State) ->
  State#state{peso=Peso}.



  

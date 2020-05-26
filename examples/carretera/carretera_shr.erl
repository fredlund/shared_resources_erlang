-module(carretera_shr).

-define(XLIMIT,3).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/4,return/4]).
-export([print_state/1]).


-record(state,{segmentos=[],distance,carriles}).
-record(segmento,{segmento,coches=[]}).
-record(coche,{pos,coche=undefined,tks=undefined}).

-define(debug,true).
-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


initial_state(DataOptions,_GeneralOptions) ->
  Distance = proplists:get_value(distance,DataOptions),
  Carriles = proplists:get_value(carriles,DataOptions),
  %% io:format("~p: Distance=~p Carriles=~p~n",[?MODULE,Distance,Carriles]),
  #state
    {
     distance=Distance,
     carriles=Carriles,
     segmentos=lists:map(fun (X) -> #segmento{segmento=X,coches=[]} end, lists:seq(1,Distance))
    }.

pre(_Call,_State) ->
  Result = true,
  ?LOG("pre(~p,~p)~n=> ~p~n",[_Call,_State,Result]),
  Result.

cpre(Call,State) ->
  Result = 
    case Call of
      {entrar,[_CocheId,_Velocidad]} ->  
        segmento_tiene_carril_libre(1,State);
      {avanzar,[CocheId,_Velocidad]} ->
        X = segmento_donde_esta_el_coche(CocheId,State),
        segmento_tiene_carril_libre(X+1,State);
      {circulando,[CocheId]} ->
        llegado(CocheId,State);
      {tick,[]} -> true;
      {salir,_} -> true
    end,
  ?LOG("cpre(~p,~p)~n => ~p~n",[Call,State,Result]),
  Result.

post(Call,_Return,State,SymbolicReturn) ->
  Result =
    case Call of
      {entrar,[CocheId,Velocidad]} ->
        avanzar_hasta_segmento(1,SymbolicReturn,CocheId,Velocidad,State);
      {avanzar,[CocheId,Velocidad]} ->
        Segmento = segmento_donde_esta_el_coche(CocheId,State),
        avanzar_hasta_segmento(Segmento,SymbolicReturn,CocheId,Velocidad,
                        liberar_hueco(CocheId,State));
      {salir,[CocheId]} ->
        liberar_hueco(CocheId,State);
      {tick,_} ->
        tick(State);
      {circulando,_} ->
        State
    end,
  ?LOG("post(~p,~p,~p)~n=> ~p~n",[Call,_Return,State,Result]),
  Result.

check_liberar_hueco_returned(Segmento,State,SymbolicResult) ->
  Coches = coches_en_segmento(Segmento,State),
  shr_symb:andp
    ([
      shr_symb:eqp(Segmento,pos_segmento(SymbolicResult)),
      shr_symb:leqp(0,pos_carril(SymbolicResult)),
      shr_symb:leqp(pos_carril(SymbolicResult),State#state.carriles)
      | lists:map(fun (Coche) -> shr_symb:notp(shr_symb:eqp(SymbolicResult,Coche#coche.pos)) end, Coches)
     ]).

return(State,Call,Result,SymbolicResult) ->
  Return =
    case Call of
      {entrar,[_CocheId,_Velocidad]} ->
        check_liberar_hueco_returned(1,State,SymbolicResult);
      {avanzar,[CocheId,_Velocidad]} ->
        Segmento = segmento_donde_esta_el_coche(CocheId,State),
        check_liberar_hueco_returned(Segmento+1,State,SymbolicResult);
      _ -> Result==void
    end,
  ?LOG("return(~p,~p,~p)~n=> ~p~n",[State,Call,Result,Return]),
  Return.

print_state(State) ->
  io_lib:format
    ("~p~n",[State]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

segmento(Segmento,State) ->
  case lists:keyfind(Segmento, #segmento.segmento, State#state.segmentos) of
    SegmentRec when is_record(SegmentRec,segmento) -> SegmentRec
  end.

coches_en_segmento(Segmento,State) ->
  (segmento(Segmento,State))#segmento.coches.

segmento_donde_esta_el_coche(CocheId,State) ->
  case shr_utils:find(fun (Segment) -> 
                          lists:exists
                            (fun (Coche) -> Coche#coche.coche==CocheId end, 
                             Segment#segmento.coches)
                      end, State#state.segmentos) of
    S when is_record(S,segmento) -> S#segmento.segmento
  end.

coche(CocheId,State) ->
  lists:foldl
    (fun (Segment,Coche) ->
         if
           not(is_record(Coche,coche)) ->
             lists:keyfind(CocheId, #coche.coche, Segment#segmento.coches);
           true ->
             Coche
         end
     end, void, State#state.segmentos).

segmento_tiene_carril_libre(Segmento,State) ->
  length(coches_en_segmento(Segmento,State)) < State#state.carriles.

avanzar_hasta_segmento(Segmento,SymbolicResult,CocheId,Velocidad,State) ->
  ?LOG
    ("avanzar_hasta_segmento(~p,~p,~p)~nin ~p~n",
     [Segmento,CocheId,Velocidad,State]),
  Coche = 
    #coche{coche=CocheId,pos={Segmento,pos_carril(SymbolicResult)},tks=Velocidad},
  SegmentoRec = 
    segmento(Segmento,State),
  NewSegment =
    SegmentoRec#segmento{coches=[Coche|SegmentoRec#segmento.coches]},
  State#state
    {segmentos=lists:keyreplace(Segmento,#segmento.segmento,State#state.segmentos,NewSegment)}.

liberar_hueco(CocheId,State) ->
  Segmento = segmento_donde_esta_el_coche(CocheId,State),
  SegmentoRec = segmento(Segmento,State),
  Coches = SegmentoRec#segmento.coches,
  NewCoches = lists:keydelete(CocheId, #coche.coche, Coches),
  NewSegment = SegmentoRec#segmento{coches=NewCoches},
  State#state
    {segmentos=lists:keyreplace(Segmento, #segmento.segmento, State#state.segmentos, NewSegment)}.

llegado(CocheId,State) ->
  Coche = coche(CocheId,State),
  Coche#coche.tks==0.

tick(State) ->
  State#state
    {segmentos=
       lists:map
         (fun (Segment) ->
              Segment#segmento
                {coches=
                   lists:map
                     (fun (Coche) -> 
                          case Coche#coche.tks of
                            N when is_integer(N), N>0 ->
                              Coche#coche{tks=N-1};
                            _ ->
                              Coche
                          end
                      end, Segment#segmento.coches)}
          end, State#state.segmentos)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pos_carril(Pos) ->
  shr_symb:sfun(fun ({_,Carril}) -> Carril end, void, [Pos]).

pos_segmento(Pos) ->
  shr_symb:sfun(fun ({Segmento,_}) -> Segmento end, void, [Pos]).



                         
        




  

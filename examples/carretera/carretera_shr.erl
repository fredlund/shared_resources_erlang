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

post(Call,_Return,SymbolicReturn,State) ->
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
  andp
    ([
      eqp(Segmento,pos_segmento(SymbolicResult)),
      leqp(0,pos_carril(SymbolicResult)),
      leqp(pos_carril(SymbolicResult),State#state.carriles)
      | lists:map(fun (Coche) -> notp(eqp(SymbolicResult,Coche#coche.pos)) end, Coches)
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
     end, State#state.segmentos).

segmento_tiene_carril_libre(Segmento,State) ->
  length(coches_en_segmento(Segmento,State)) < State#state.carriles.

avanzar_hasta_segmento(Segmento,SymbolicResult,CocheId,Velocidad,State) ->
  ?LOG
    ("avanzar_hasta_segmento(~p,~p,~p)~nin ~p~n",
     [Segmento,CocheId,Velocidad,State]),
  SegmentoRec = 
    segmento(Segmento,State),
  Coche = 
    #coche{coche=CocheId,pos={Segmento,pos_carril(SymbolicResult),tks=Velocidad}},
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
  sfun(?MODULE,pos_carril_sfun,[Pos]).

pos_segmento(Pos) ->
  sfun(?MODULE,pos_segmento_sfun,[Pos]).

eqp(T1,T2) ->
  sfun(?MODULE,eq_sfun,[T1,T2]).

andp(T1,T2) ->
  sfun(?MODULE,and_sfun,[T1,T2]).
  
orp(T1,T2) ->
  sfun(?MODULE,or_sfun,[T1,T2]).

leqp(T1,T2) ->
  sfun(?MODULE,leq_sfun,[T1,T2]).

ltp(T1,T2) ->
  sfun(?MODULE,lt_sfun,[T1,T2]).

geqp(T1,T2) ->
  sfun(?MODULE,geq_sfun,[T1,T2]).

gtp(T1,T2) ->
  sfun(?MODULE,gt_sfun,[T1,T2]).

notp(T) ->
  sfun(?MODULE,not_sfun,[T]).

andp([]) ->
  true;
andp([T]) ->
  T;
andp([T1,T2]) ->
  andp(T1,T2);
andp([T|Rest]) ->
  andp(T,andp(Rest)).

orp([]) ->
  true;
orp([T]) ->
  T;
orp([T1,T2]) ->
  orp(T1,T2);
orp([T|Rest]) ->
  orp(T,orp(Rest)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sfun(M,F,A) ->
  {'$sfun',M,F,A}.

is_sfun({'$sfun',_,_,_}) ->
  true;
is_sfun(_) ->
  false.

exec_sfun({'$sfun',M,F,A}) ->
  apply(M,F,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_sfun(T) ->
  check_term(is_fun/1,T).

is_symbolic(T) ->
  check_term(is_symb_var/1,T).

check_term(F,T) ->
  F(T) orelse check_term1(F,T).

check_term1(F,T) ->
  case T of
    _ when is_tuple(T) ->
      check_term1(F,tuple_to_list(T));
    [Hd|Tl] ->
      check_term(F,Hd) orelse check_term(Tl,Hd);
    _ when is_map(T) ->
      check_term1(F,maps:values(T));
    _ ->
      false
  end.

subst(VarMap,T) ->
  case T of
    {var,_} ->
      case lists:keyfind(T,1,VarMap) of
        false ->
          io:format
            ("~n*** ERROR: map ~p~ndoes not contain variable ~p~n",
             [VarMap,T]),
          error(bad);
        {_,Value} ->
          Value
      end;
    _ when is_tuple(T) ->
      list_to_tuple(subst(VarMap,tuple_to_list(T)));
    [Hd|Tl] ->
      [subst(VarMap,Hd)|subst(VarMap,Tl)];
    _ when is_map(T) ->
      maps:from_list(subst(VarMap,maps:to_list(T)));
    _ ->
      T
  end.

eval(T) ->
  evalHead(subexpr_eval(T)).

evalHead(T) ->
  case T of
    {'$sfun',M,F,Args} ->
      Ground = 
        (not(has_sfun(Args))) andalso (not(is_symbolic(Args))),
      if
        Ground ->
          exec_sfun(T);
        true ->
          T
      end;
    _ -> T
  end.

subexpr_eval(T) ->
  case evalHead(T) of
    NewT when T=/=NewT -> NewT;
    _ ->
      case T of
        _ when is_tuple(T) ->
          list_to_tuple(subexpr_eval(tuple_to_list(T)));
        [Hd|Tl] ->
          [subexpr_eval(Hd)|subexpr_eval(Tl)];
        _ when is_map(T) ->
          maps:from_list(subexpr_eval(maps:to_list(T)));
        _ ->
          T
      end
  end.
                   
                       
                         
        




  

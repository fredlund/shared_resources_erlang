-module(carretera_shr).

-define(XLIMIT,3).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(state,{cells=[],distance,carriles}).
-record(cell,{location,coche=undefined,tks=undefined}).

%%-define(debug,true).
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
     cells=
       lists:flatmap
         (fun (X) -> 
              lists:map
                (fun (Y) -> 
                     #cell{location={X,Y}}
                 end, lists:seq(1,Carriles))
          end,
          lists:seq(1,Distance))
    }.

pre(_Call,_State) ->
  Result = true,
  ?LOG("pre(~p,~p) => ~p~n",[_Call,_State,Result]),
  Result.

cpre(Call,State) ->
  Result = 
    case Call of
      {entrar,[_CocheId,_Velocidad]} ->  
        segmento_has_free_carril(1,State);
      {avanzar,[CocheId,_Velocidad]} ->
        {X,_Y} = _Location = location(CocheId,State),
        segmento_has_free_carril(X+1,State);
      {circulando,[CocheId]} ->
        arrived(CocheId,State);
      {tick,[]} -> true;
      {salir,_} -> true
    end,
  ?LOG("cpre(~p,~p) => ~p~n",[Call,State,Result]),
  Result.

post(Call,Return,State) ->
  Result =
    case Call of
      {entrar,[CocheId,Velocidad]} ->
        avanzar_to_cell(Return,CocheId,Velocidad,State);
      {salir,[CocheId]} ->
        Location = location(CocheId,State),
        free_cell(Location,State);
      {avanzar,[CocheId,Velocidad]} ->
        Location = location(CocheId,State),
        avanzar_to_cell(Return,CocheId,Velocidad,
                        free_cell(Location,State));
      {tick,_} ->
        tick(State);
      {circulando,_} ->
        State
    end,
  ?LOG("post(~p,~p,~p) => ~p~n",[Call,Return,State,Result]),
  Result.

return(State,Call,Result) ->
  Return =
    case Call of
      {entrar,[_CocheId,_Velocidad]} ->
        check_free_cell_returned(1,State,Result);
      {avanzar,[CocheId,_Velocidad]} ->
        {Segmento,_} = location(CocheId,State),
        check_free_cell_returned(Segmento+1,State,Result);
      _ -> Result==void
    end,
  ?LOG("return(~p,~p,~p) => ~p~n",[State,Call,Result,Return]),
  Return.

check_free_cell_returned(Segmento,State,Result) ->
  lists:member
    (Result,
     lists:map(fun (Cell) -> Cell#cell.location end, free_cells_at_segmento(Segmento,State))).

return_value(Call,State) ->
  Result =
    case Call of
      {entrar,[_,_]} ->
        shr_utils:nondeterministic(free_positions_at_segmento(1,State));
      {avanzar,[CocheId,_]} ->
        {X,_} = location(CocheId,State),
        shr_utils:nondeterministic(free_positions_at_segmento(X+1,State));
      Call ->
        ?LOG("call ~p should not return anything~n",[Call]),
        void
    end,
  ?LOG("return_value(~p,~p) => ~p~n",[Call,State,Result]),
  Result.

print_state(State) ->
  io_lib:format
    ("~p~n",[State]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cells_at_segmento(Segmento,State) ->
  lists:filter(fun (#cell{location={X,_}}) -> Segmento==X end, State#state.cells).

free_cells_at_segmento(Segmento,State) ->
  lists:filter(fun (#cell{coche=Coche}) -> Coche==undefined end, cells_at_segmento(Segmento,State)).
                    
free_positions_at_segmento(Segmento,State) ->
  lists:map(fun (Cell) -> Cell#cell.location end, free_cells_at_segmento(Segmento,State)).

segmento_has_free_carril(Segmento,State) ->
  free_cells_at_segmento(Segmento,State) =/= [].

cell(Location,State) ->
  case lists:keyfind(Location,#cell.location,State#state.cells) of
    Cell when is_record(Cell,cell) ->
      Cell;
    false ->
      ?LOG
        ("~n*** Error: there is no cell at ~p~nCells=~p~n",
         [Location,State#state.cells]),
      error(bad)
  end.

location(CocheId,State) ->
  location1(CocheId,State#state.cells,State#state.cells).
location1(CocheId,[Cell|Rest],Cells) ->
  if
    Cell#cell.coche == CocheId ->
      Cell#cell.location;
    true -> 
      location1(CocheId,Rest,Cells)
  end;
location1(CocheId,[],Cells) ->
  io:format
    ("~n*** Error: could not find car ~p in cells~n  ~p~n",
     [CocheId,Cells]),
  error(bad).

avanzar_to_cell(Location,CocheId,Velocidad,State) ->
  ?LOG
    ("avanzar_to_cell(~p,~p,~p)~nin ~p~n",
     [Location,CocheId,Velocidad,State]),
  Cell = cell(Location,State),
  if
    Cell#cell.coche==undefined ->
      NewCell = 
        Cell#cell{coche=CocheId,tks=Velocidad},
      State#state
        {cells=lists:keystore(Location,#cell.location,State#state.cells,NewCell)}
  end.

free_cell(Location,State) ->
  Cell = cell(Location,State),
  if
    Cell#cell.coche=/=undefined ->
      NewCell = 
        Cell#cell{coche=undefined,tks=undefined},
      State#state
        {cells=lists:keystore(Location,#cell.location,State#state.cells,NewCell)}
  end.

arrived(CocheId,State) ->
  Cell = cell(location(CocheId,State),State),
  Cell#cell.tks==0.

tick(State) ->
  State#state
    {cells=
       lists:map
         (fun (Cell) ->
              case Cell#cell.tks of
                N when is_integer(N), N>0 ->
                  Cell#cell{tks=N-1};
                _ ->
                  Cell
              end
          end, State#state.cells)}.
                     





  

-module(carretera_control_shr).

-define(NUM_SEGMENTS,3).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(state,{segments=[]}).
-record(segment,{location,coche=undefined,tiempoQueda=undefined}).
-record(coche,{id,velocidad}).

%%-define(debug,true).
-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


initial_state(_,_) ->
  #state
    {segments=
       lists:flatmap
         (fun (X) -> 
              [{#segment{location={X,0}}},#segment{location={X,1}}]
          end,
          lists:seq(0,?NUM_SEGMENTS-1))}.

pre(_,_State) ->
  true.

cpre({enter,[_CocheId,_Velocidad]},State) ->
  free_segment({0,0},State) orelse free_segment({0,1},State);
cpre({move,[CocheId,_Velocidad]},State) ->
  {X,_Y} = Location = location(CocheId,State),
  (timeRemaining(Location,State) =< 0)
    andalso (free_segment({X,0},State) orelse free_segment({X,1},State));
cpre(_,_State) ->
  true.

post({enter,[CocheId,Velocidad]},Return={_X,_Y},State) ->
  Coche = #coche{id=CocheId,velocidad=Velocidad},
  set_segment(Return,#segment{coche=Coche,tiempoQueda=Velocidad},State);
post({move,[CocheId,_Velocidad]},Return={_X,_Y},State) ->
  Location = location(CocheId,State),
  Coche = car_in_location(Location,State),
  set_segment
    (Return,
     #segment{coche=Coche,tiempoQueda=Coche#coche.velocidad},
     free_segment(Location,State));
post({exit,[CocheId,_Velocidad]},_Return,State) ->
  Location = location(CocheId,State),
  free_segment(Location,State);
post(tick,_Return,State) ->
  tick(State).

return(State,{enter,[CocheId,_Velocidad]},Result) ->
  check_return(State,CocheId,Result);
return(State,{move,[CocheId,_Velocidad]},Result) ->
  check_return(State,CocheId,Result);
return(_State,_,Result) ->
  Result==void.

check_return(State,CocheId,Result) ->
  {X,_Y} = location(CocheId,State),
  FreeNextLocations = 
    lists:filter
      (fun (Location) -> free_segment(Location,State) end,
       [{X+1,0},{X+1,1}]),
  lists:member(Result,FreeNextLocations).

return_value(Call,State) ->
  case Call of
    {enter,[_,_]} ->
      select_next_location({-1,0},State);
    {move,[CocheId,_]} ->
      select_next_location(location(CocheId,State),State);
    _ ->
      void
  end.

select_next_location(Location={X,Y},State) ->
  case free_segment({X+1,Y},State) of
    true -> Location;
    false -> {X+1,Y rem 2}
  end.

print_state(State) ->
  io_lib:format
    ("~p~n",[State]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

free_segment(Location,State) ->
  (segment(Location,State))#segment.coche==undefined.

segment(Location,State) ->
  lists:keyfind(Location,#segment.location,State#state.segments).

location(CocheId,State) ->
  location1(CocheId,State#state.segments).
location1(CocheId,[Segment|Rest]) ->
  if
    Segment#segment.coche =/= undefined ->
      SegmentCocheId = (Segment#segment.coche)#coche.id,
      if
        CocheId == SegmentCocheId ->
          Segment#segment.location;
        true ->
          location1(CocheId,Rest)
      end;
    true -> location1(CocheId,Rest)
  end.
    
timeRemaining(Location,State) ->
  Segment = segment(Location,State),
  Segment#segment.tiempoQueda.

set_segment(Location,Segment,State) ->
  State#state{segments=lists:keyreplace(Location,#segment.location,State#state.segments,Segment)}.

car_in_location(Location,State) ->  
  Segment=segment(Location,State),
  Segment#segment.coche.

tick(State) ->
  State#state
    {segments = 
       lists:map
         (fun (Segment) ->
              TiempoQueda = Segment#segment.tiempoQueda,
              if
                TiempoQueda=/=undefined, TiempoQueda>0 ->
                  Segment#segment{tiempoQueda=TiempoQueda-1};
                true ->
                  Segment
              end
          end, State#state.segments)}.





  

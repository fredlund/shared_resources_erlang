-module(carretera_shr).

-define(XLIMIT,3).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(state,{segments=[],distance,carriles}).
-record(segment,{location,coche=undefined,tiempoQueda=undefined}).

%%-define(debug,true).
-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


initial_state(_,Options) ->
  Distance = proplists:get_value(distance,Options),
  Carriles = proplists:get_value(carriles,Options),
  #state
    {
     distance=Distance,
     carriles=Carriles,
     segments=
       lists:flatmap
         (fun (X) -> 
              lists:map
                (fun (Y) -> 
                     #segment{location={X,Y}}
                 end, lists:seq(0,Carriles))
          end,
          lists:seq(0,Distance))
    }.

pre(_Call,_State) ->
  ?LOG("pre(~p,~p)~n",[_Call,_State]),
  true.

cpre({enter,[_CocheId]},State) ->
  isfree_segment({0,0},State) orelse isfree_segment({0,1},State);
cpre({move,[CocheId]},State) ->
  {X,_Y} = Location = location(CocheId,State),
  ?LOG
    ("move(~p,~p); location=~p seg=~p~n",
     [CocheId,_Velocidad,Location,segment(Location,State)]),
  (timeRemaining(Location,State) =< 0)
    andalso (isfree_segment({X+1,0},State) orelse isfree_segment({X+1,1},State));
cpre({moving,[CocheId,_Velocidad]},State) ->
  arrived(CocheId,State);
cpre(_,_State) ->
  true.

post({enter,[CocheId]},Return={_X,_Y},State) ->
  move_to_segment(Return,CocheId,Velocidad,State);
post({exit,[CocheId]},_Return,State) ->
  Location = location(CocheId,State),
  (timeRemaining(Location,State) =< 0);
post({move,[CocheId]},Return={_X,_Y},State) ->
  Location = location(CocheId,State),
  move_to_segment(Return,CocheId,Velocidad,
                  free_segment(Location,State));
post({tick,_},_Return,State) ->
  tick(State).

return(State,{enter,[CocheIdd,_Velocidad]},Result) ->
  check_return(State,CocheId,Result);
return(State,{move,[CocheId]},Result) ->
  check_return(State,CocheId,Result);
return(_State,_,Result) ->
  Result==void.

check_return(State,CocheId,Result) ->
  {X,_Y} = location(CocheId,State),
  FreeNextLocations = 
    lists:filter
      (fun (Location) -> isfree_segment(Location,State) end,
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

select_next_location({X,Y},State) ->
  case isfree_segment({X+1,Y},State) of
    true -> {X+1,Y};
    false -> {X+1,(Y+1) rem 2}
  end.

print_state(State) ->
  io_lib:format
    ("~p~n",[State]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isfree_segment(Location,State) ->
  (segment(Location,State))#segment.coche==undefined.

segment(Location,State) ->
  case lists:keyfind(Location,#segment.location,State#state.segments) of
    Segment when is_record(Segment,segment) ->
      Segment;
    false ->
      ?LOG
        ("~n*** Error: there is no segment at ~p~nSegments=~p~n",
         [Location,State#state.segments]),
      error(bad)
  end.

location(CocheId,State) ->
  location1(CocheId,State#state.segments).
location1(CocheId,[Segment|Rest]) ->
  ?LOG("location1(~p,~p)~n",[CocheId,Segment]),
  if
    Segment#segment.coche == CocheId ->
      Segment#segment.location;
    true -> 
      location1(CocheId,Rest)
  end.
    
timeRemaining(Location,State) ->
  Segment = segment(Location,State),
  Segment#segment.tiempoQueda.

move_to_segment(Location,CocheId,Velocidad,State) ->
  ?LOG
    ("move_to_segment(~p,~p,~p)~nin ~p~n",
     [Location,CocheId,Velocidad,State]),
  Segment = segment(Location,State),
  if
    Segment#segment.coche==undefined ->
      NewSegment = 
        Segment#segment{coche=CocheId,tiempoQueda=Velocidad},
      State#state
        {segments=lists:keyreplace(Location,#segment.location,State#state.segments,NewSegment)}
  end.

free_segment(Location,State) ->
  Segment = segment(Location,State),
  if
    Segment#segment.coche=/=undefined ->
      NewSegment = 
        Segment#segment{coche=undefined,tiempoQueda=undefined},
      State#state
        {segments=lists:keyreplace(Location,#segment.location,State#state.segments,NewSegment)}
  end.

arrived(CocheId,State) ->
  Segment = segment(location(CocheId,State),State),
  Segment#segment.tiempoQueda==0.

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





  

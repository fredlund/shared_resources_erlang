-module(carretera_shr).

-define(XLIMIT,3).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(state,{segments=[],distance,carriles,time=0}).
-record(segment,{location,coche=undefined,arrivalTime=undefined}).

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
  io:format("~p: Distance=~p Carriles=~p~n",[?MODULE,Distance,Carriles]),
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
  Result = true,
  ?LOG("pre(~p,~p) => ~p~n",[_Call,_State,Result]),
  Result.

cpre(Call,State) ->
  Result = 
    case Call of
      {entrar,[_CocheId,_Velocidad]} ->  
        isfree_segment({0,0},State) orelse isfree_segment({0,1},State);
      {avanzar,[CocheId,_Velocidad]} ->
        {X,_Y} = _Location = location(CocheId,State),
        ?LOG
           ("avanzar(~p,~p); location=~p seg=~p~n",
            [CocheId,_Velocidad,_Location,segment(_Location,State)]),
        isfree_segment({X+1,0},State) orelse isfree_segment({X+1,1},State);
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
        avanzar_to_segment(Return,CocheId,Velocidad,State);
      {salir,[CocheId]} ->
        Location = location(CocheId,State),
        free_segment(Location,State);
      {avanzar,[CocheId,Velocidad]} ->
        Location = location(CocheId,State),
        avanzar_to_segment(Return,CocheId,Velocidad,
                        free_segment(Location,State));
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
      {entrar,[CocheId,_Velocidad]} ->
        check_return({-1,0},State,CocheId,Result);
      {avanzar,[CocheId,_Velocidad]} ->
        check_return(location(CocheId,State),State,CocheId,Result);
      _ -> Result==void
    end,
  ?LOG("return(~p,~p,~p) => ~p~n",[State,Call,Result,Return]),
  Return.

check_return({X,_Y},State,CocheId,Result) ->
  PreFreeNextLocations = 
    lists:filter
      (fun (Location) -> isfree_segment(Location,State) end,
       [{X+1,0},{X+1,1}]),
  FreeNextLocations =
    lists:map(fun ({X,Y}) -> {X+1,Y+1} end, PreFreeNextLocations),
  lists:member(Result,FreeNextLocations).

return_value(Call,State) ->
  PreResult =
    case Call of
      {entrar,[_,_]} ->
        select_next_location({-1,0},State);
      {avanzar,[CocheId,_]} ->
        select_next_location(location(CocheId,State),State);
      Call ->
        ?LOG("call ~p should not return anything~n",[Call]),
        void
    end,
  Result = 
    case PreResult of
      {X,Y} -> {X+1,Y+1};
      Other -> Other
    end,
  ?LOG("return_value(~p,~p) => ~p~n",[Call,State,Result]),
  Result.

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
  location1(CocheId,State#state.segments,State#state.segments).
location1(CocheId,[Segment|Rest],Segments) ->
  if
    Segment#segment.coche == CocheId ->
      Segment#segment.location;
    true -> 
      location1(CocheId,Rest,Segments)
  end;
location1(CocheId,[],Segments) ->
  io:format
    ("~n*** Error: could not find car ~p in segments~n  ~p~n",
     [CocheId,Segments]),
  error(bad).

avanzar_to_segment(PreLocation,CocheId,Velocidad,State) ->
  ?LOG
    ("avanzar_to_segment(~p,~p,~p)~nin ~p~n",
     [PreLocation,CocheId,Velocidad,State]),
  Location = 
    case PreLocation of
      {X,Y} -> {X-1,Y-1}
    end,
  Segment = segment(Location,State),
  if
    Segment#segment.coche==undefined ->
      NewSegment = 
        Segment#segment{coche=CocheId,arrivalTime=Velocidad+State#state.time},
      State#state
        {segments=lists:keystore(Location,#segment.location,State#state.segments,NewSegment)}
  end.

free_segment(Location,State) ->
  Segment = segment(Location,State),
  if
    Segment#segment.coche=/=undefined ->
      NewSegment = 
        Segment#segment{coche=undefined,arrivalTime=undefined},
      State#state
        {segments=lists:keystore(Location,#segment.location,State#state.segments,NewSegment)}
  end.

arrived(CocheId,State) ->
  Segment = segment(location(CocheId,State),State),
  State#state.time >= Segment#segment.arrivalTime.

tick(State) ->
  State#state{time=State#state.time+1}.





  

-module(control_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return_value/2]).
-export([print_state/1]).

-record(state,{presencia,tren,color}).

%%-define(debug,true).
-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


initial_state(_,_) ->
  #state
    {
     presencia=false,
     tren=list_to_tuple(lists:duplicate(4,0)),
     color=list_to_tuple(lists:duplicate(4,verde))
    }.

pre({leerCambioSemaforo,[Id,_]},_State) ->
  Id =/= 0;
pre({avisarPasoPorBaliza,[Id]},_State) ->
  Id =/= 0;
pre(_,_State) ->
  true.

cpre({leerCambioBarrera,[A]},State) ->
  A =/= ((tren(1,State) + tren(2,State)) == 0);
cpre({leerCambioFreno,[A]},State) ->
  A =/= 
    ((tren(1,State) > 1)
     or (tren(2,State) > 1) 
     or ((tren(2,State) == 1) and presencia(State)));
cpre({leerCambioSemaforo,[Id,C]},State) ->
  C == color(Id,State);
cpre(_,_) ->
  true.

post({avisarPresencia,[Presente]},_Return,State) ->
  Color1 =
    %% The beauty of Erlang if
    case tren(1,State) > 0 of
      true -> rojo;
      false ->
        case (tren(1,State) == 0) and ((tren(2,State) > 0) or presencia(State)) of
        true -> amarillo;
        false ->
            case ((tren(1,State) == 0) and (tren(2,State) == 0) and not(presencia(State))) of
              true -> verde
            end
        end
    end,

  Color2 =
    case (tren(2,State) > 0) or presencia(State) of
      true -> rojo;
      false ->
        case (tren(2,State) == 0) and not(presencia(State)) of
          true -> verde
        end
    end,

  Color3 =
    verde,

  set_color
    (3,Color3,
     set_color
       (2,Color2,
        set_color(1,Color1, set_presencia(Presente,State))));

post({avisarPasoPorBaliza,Id},_,State) ->
  set_tren
    (Id-1,tren(Id-1,State)-1,
     set_tren(Id,tren(Id,State)+1,State));

post(_,_,State) ->
  State.

return_value({leerCambioBarrera,_},State) ->
  tren(1,State) + tren(2,State) == 0;
return_value({leerCambioFreno,_},State) ->
  (tren(1,State) > 1)
    or (tren(2,State) > 1)
    or ((tren(2,State) == 1) and presencia(State));
return_value({leerCambioSemaforo,[Id,_]},State) ->
  color(Id,State);
return_value({print,[]},State) ->
  State;
return_value(_,_) ->
  void.

print_state(State) ->
  io_lib:format
    ("~p~n",[State]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tren(Id,State) ->
  element(Id+1,State#state.tren).

set_tren(Id,Tren,State) ->
  State#state{tren=setelement(Id,State#state.tren,Tren)}.

color(Id,State) ->
  element(Id+1,State#state.color).

set_color(Id,Color,State) ->
  State#state{color=setelement(Id,State#state.color,Color)}.

presencia(State) ->  
  State#state.presencia.

set_presencia(Presente,State) ->
  State#state{presencia=Presente}.


  

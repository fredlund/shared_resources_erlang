%% @doc Especificación del recurso de enclavamiento
-module(enclavamiento_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).

%% Zonas y coches en el cruce
-record(state,{z1, z2, x}).
-record(signals, {s1, s2, s3, b, f}).

%% Estado inicial
initial_state([],_Options) ->
  #state{z1 = 0, z2 = 0, x = 0}.

%% Precondiciones
pre({baliza,[_N]}, _State) -> true;

pre({cruce,[_Presencia]}, _State) -> true;

pre({semaforo,[_N, _Actual]}, _State) -> true;

pre({barrera,[_Abierta]}, _State) -> true;

pre({freno,[_Accionado]}, _State) -> true.

%% Condiciones de sincronización
cpre({baliza,[_N]}, _State) -> true;

cpre({cruce,[_Presencia]}, _State) -> true;

cpre({semaforo,[1, Actual]}, State) ->
  Signals = signals(State),
  Signals#signals.s1 /= Actual;
cpre({semaforo,[2, Actual]}, State) ->
  Signals = signals(State),
  Signals#signals.s2 /= Actual;
cpre({semaforo,[3, Actual]}, State) ->
  Signals = signals(State),
  Signals#signals.s3 /= Actual;

cpre({barrera,[Abierta]}, State) ->
  Signals = signals(State),
  Signals#signals.b /= Abierta;

cpre({freno,[Accionado]}, State) ->
  Signals = signals(State),
  Signals#signals.f /= Accionado.

%% Post-condiciones (estado)
post({baliza, [1]}, _Result, State) ->
  Z1 = State#state.z1,
  State#state{z1 = Z1 + 1};
post({baliza, [2]}, _Result, State) ->
  Z1 = State#state.z1,
  Z2 = State#state.z2,
  State#state{z1 = Z1 - 1,
              z2 = Z2 + 1};
post({baliza, [3]}, _Result, State) ->
  Z2 = State#state.z2,
  State#state{z2 = Z2 - 1};

post({cruce, [Presencia]}, _Result, State) ->
  State#state{x = Presencia};

post({semaforo, [_N, _Actual]}, _Result, State) ->
  State;

post({barrera, [_Abierta]}, _Result, State) ->
  State;

post({freno, [_Accionado]}, _Result, State) ->
  State.

%% Post-condiciones (resultados)
return(_State, {baliza, [_N]}, undefined) -> true;

return(_State, {cruce, [_Presencia]}, undefined) -> true;

return(State, {semaforo, [1, _Actual]}, Result) ->
  Signals = signals(State),
  Result == Signals#signals.s1;
return(State, {semaforo, [2, _Actual]}, Result) ->
  Signals = signals(State),
  Result == Signals#signals.s2;
return(State, {semaforo, [3, _Actual]}, Result) ->
  Signals = signals(State),
  Result == Signals#signals.s3;

return(State, {barrera, [_Abierta]}, Result) ->
  Signals = signals(State),
  Result == Signals#signals.b;
  
return(State, {freno, [_Accionado]}, Result) ->
  Signals = signals(State),
  Result == Signals#signals.f.
  
%% Concrete implementation
return_value({baliza, [_N]}, _State) -> undefined;

return_value({cruce, [_Presencia]}, _State) -> undefined;

return_value({semaforo, [1, _Actual]}, State) ->
  Signals = signals(State),
  Signals#signals.s1;
return_value({semaforo, [2, _Actual]}, State) ->
  Signals = signals(State),
  Signals#signals.s2;
return_value({semaforo, [3, _Actual]}, State) ->
  Signals = signals(State),
  Signals#signals.s3;

return_value({barrera, [_Abierta]}, State) ->
  Signals = signals(State),
  Signals#signals.b;
  
return_value({freno, [_Accionado]}, State) ->
  Signals = signals(State),
  Signals#signals.f.
            
%% Funciones auxiliares
signals(#state{z1 = Z1, z2 = Z2, x = X}) ->
  S1 = if Z1 > 0 -> rojo;
          Z2 > 0; X -> amarillo;
          true -> verde
       end,
  S2 = if Z2 > 0; X -> rojo;
          true -> verde
       end,
  #signals{s1 = S1,
           s2 = S2,
           s3 = verde,
           b = true,
           f = false}.

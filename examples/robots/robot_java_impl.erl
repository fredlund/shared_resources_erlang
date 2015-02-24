-module(robot_java_impl).

-compile(export_all).

start(_Args,Options) ->
  NodeId = test_utils:start_java(Options),
  case java:new(NodeId,'ControlAccesoNavesMonitor',[]) of
    Exc = {java_exception,_} -> 
      java:report_java_exception(Exc),
      throw(bad);
    Controller ->
      tester:store_data(controller,Controller),
      Controller
  end.

enter(_Robot,Nave,Peso) ->
  java:call(tester:get_data(controller),solicitarEntrar,[Nave,Peso]).

exit(_Robot,Nave,Peso) ->
  java:call(tester:get_data(controller),solicitarSalir,[Nave,Peso]).



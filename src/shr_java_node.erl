-module(shr_java_node).

-export([start_node/0,start_node/1,node/0]).

start_node() ->
  start_node([]).

start_node(Options) ->
  try
    case shr_storage:read(java_node) of
      false -> ok;
      {_,NodeId} -> java:terminate(NodeId)
    end
  catch _:_ ->
      ok
  end,
  {ok,NewNodeId} = java:start_node(Options),
  shr_storage:write(java_node,NewNodeId),
  {ok,NewNodeId}.

node() ->
  {_,NodeId} = shr_storage:read(java_node),
  NodeId.


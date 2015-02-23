-module(test_utils).

-compile(export_all).

start_java(Options) ->
  CP = proplists:get_value(cp,Options,[]),
  ets:insert(?MODULE,{cp,CP}),
  try
    java:start_node
      ([{java_verbose,"SEVERE"},
	{call_timeout,infinity},
	{java_exception_as_value,true},
	{add_to_java_classpath,CP}]) of
    {ok,NId} ->
      tester:store_data(node,NId),
      NId
  catch _:_ ->
      io:format
	("~n*** Error: cannot start java. Is the javaerlang library installed?~n"),
      throw(bad)
  end.

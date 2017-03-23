-record(rtest,
	{
	  generator,
	  start_implementation=void,
	  stop_implementation=void,
	  resource,
	  scheduler=void,
	  test_observer=void,
	  options=[]
	}).

-record(rsystem,
	{
	  parameters=[],
	  operations=[],
	  resources=[],
	  processes=[],
	  pre=fun (_) -> true end,
	  external_mapping,
	  linking=[]
	}).
	  

-record(onestate,
	{
	  incoming,   %% Incoming jobs, not yet accepted
	  waiting,    %% Accepted jobs, waiting to be executed
	  sdata,      %% Data state
	  swait       %% Priority state
	}).
-record(state,
	{
	  started,    %% Started testing?
	  states,     %% Set of possible (onestate) states
	  test_state, %% State for testing
	  dataSpec,   %% Module implementing the data specification
	  waitSpec,   %% Module implementing the priority specification
	  testingSpec %% Module implementing the testing specification
	}).
-record(job,
	{
	  pid,        
	  call        
	}).

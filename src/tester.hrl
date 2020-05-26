-record(job,
	{
	  pid
	  ,call
	  ,result
	  ,waitinfo
          ,symbolicResult
	  ,info
	}).

-record(command,
	{
	  raw,
	  port=void,   
	  call,          %% {F,Args}
	  options=[]     %% list of values
	}).

-record(test_case,{test_case,test_result}).

-record(transition,
        {
          calls,                     %% executed calls
          unblocked,                 %% unblocked calls
          returns,                   %% return values (and return check) for
                                     %% unblocked calls
          failed_pres,               %% calls whose precondition fails
          endstate,                   %% next state,
          symVarCounter
        }).

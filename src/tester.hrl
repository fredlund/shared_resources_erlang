-record(job,
	{
	  pid
	  ,call
	  ,result
	  ,waitinfo
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

-record(transition,{calls,unblocked,returns,endstate,failed_pres}).

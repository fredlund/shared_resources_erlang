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

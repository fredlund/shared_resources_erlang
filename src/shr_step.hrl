-record(history_item,
	{
	  calls,
	  unblocked
	}).

-record(call,
	{
	  call,
	  waitinfo,
	  id
	}).

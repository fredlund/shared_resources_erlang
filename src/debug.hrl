-ifdef(debug).
-define(LOG(X,Y),
	io:format("~s",[io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

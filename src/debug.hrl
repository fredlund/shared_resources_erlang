-ifdef(debug).
-define(LOG(X,Y),
	io:format("~p(~p): ~s",[?MODULE,self(),io_lib:format(X,Y)])).
-define(TIMEDLOG(X,Y),
	io:format("~p: ~p(~p): ~s",[shr_utils:milliseconds_after(),?MODULE,self(),io_lib:format(X,Y)])).
-define(RAWLOG(X,Y),
	io:format("~s",[io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-define(TIMEDLOG(X,Y),true).
-define(RAWLOG(X,Y),true).
-endif.

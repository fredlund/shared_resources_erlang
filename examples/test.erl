-module(test).

-compile(export_all).

test() ->
  R = resource:start(robots_fcfs,[3,1000],[]),
  ParentPid = self(),
  spawn(
    fun () ->
	resource:call(R,enter,[0,0,900]),
	resource:call(R,exit,[0,0,900]),
	ParentPid!ok
    end),
  spawn(
    fun () ->
	resource:call(R,enter,[1,0,200]),
	resource:call(R,exit,[1,0,200]),
	ParentPid!ok
    end),
  count_returns(2).

count_returns(0) ->
  ok;
count_returns(N) when is_integer(N), N>0 ->
  receive
    ok -> count_returns(N-1)
  end.


  



  

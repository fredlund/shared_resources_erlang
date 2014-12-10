-module(test).

-compile(export_all).

test1f() ->
  test(fcfs,example1).

test1a() ->
  test(always,example1).

test1w() ->
  test(robots_wsef,example1).

test1q() ->
  test(queue_sched,example1).

test2f() ->
  test(fcfs,example2).

test2a() ->
  test(always,example2).

test2w() ->
  test(robots_wsef,example2).

test2q() ->
  test(queue_sched,example2).

test(PriorityModule,Example) ->
  R = resource:start_link(robots,PriorityModule,[3,1000],[]),
  ?MODULE:Example(R).

example1(R) ->
  ParentPid = self(),
  spawn(
    fun () ->
	resource:call(R,enter,[0,0,900]),
	resource:call(R,exit,[0,0,900]),
	ParentPid!ok
    end),
  spawn(
    fun () ->
	resource:call(R,enter,[2,0,1200]),
	resource:call(R,exit,[2,0,1200]),
	ParentPid!ok
    end),
  spawn(
    fun () ->
	resource:call(R,enter,[1,0,200]),
	resource:call(R,exit,[1,0,200]),
	ParentPid!ok
    end),
  count_returns(3).

example2(R) ->
  ParentPid = self(),
  resource:call(R,enter,[0,0,900]),
  resource:call(R,exit,[0,0,900]),
  spawn(
    fun () ->
	resource:call(R,enter,[1,0,300]),
	resource:call(R,exit,[1,0,300]),
	ParentPid!ok
    end),
  spawn(
    fun () ->
	resource:call(R,enter,[2,0,400]),
	resource:call(R,exit,[2,0,400]),
	ParentPid!ok
    end),
  spawn(
    fun () ->
	resource:call(R,enter,[3,0,100]),
	resource:call(R,exit,[3,0,100]),
	ParentPid!ok
    end),
  timer:sleep(200),
  resource:call(R,enter,[0,1,900]),
  timer:sleep(200),
  count_returns(3).

count_returns(0) ->
  ok;
count_returns(N) when is_integer(N), N>0 ->
  receive
    ok -> count_returns(N-1)
  end.


  



  

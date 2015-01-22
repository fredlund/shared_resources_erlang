-module(testm).

-compile(export_all).

test1a() ->
  test(10,multbuf1,{always,[]}).
test1f() ->
  test(10,multbuf1,{fcfs,[]}).
test1q() ->
  test(10,multbuf1,{queue_sched,[multibuffer]}).

test2a() ->
  test(10,multbuf2,{always,[]}).
test2f() ->
  test(10,multbuf2,{fcfs,[]}).
test2q() ->
  test(10,multbuf2,{queue_sched,[multibuffer]}).

test(Max,Imp,Prio) ->
  io:format("Testing ~p under priority ~p with max=~p~n",[Imp,Prio,Max]),
  DataSpec = {multibuffer,[Max]},
  TestingSpec = {multibuffer_commands,[Max,7,7,Imp]},
  tester:test([{needs_java,false}],DataSpec,Prio,TestingSpec).




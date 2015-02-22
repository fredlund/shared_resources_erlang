-module(testm).

-compile(export_all).

test1a() ->
  test(10,multbuf1,{always,[]}).
test1f() ->
  test(10,multbuf1,{fcfs,[]}).
test1q() ->
  test(10,multbuf1,{queue_sched,[multibuffer]}).
test1q2() ->
  test(10,multbuf1,{queue_sched2,[multibuffer]}).
test1sm() ->
  test(10,multbuf1,{smallest_first,[multibuffer]}).

test2a() ->
  test(10,multbuf2,{always,[]}).
test2f() ->
  test(10,multbuf2,{fcfs,[]}).
test2q() ->
  test(10,multbuf2,{queue_sched,[multibuffer]}).
test2q2() ->
  test(10,multbuf2,{queue_sched2,[multibuffer]}).
test2sm() ->
  test(10,multbuf2,{smallest_first,[multibuffer]}).

test3a() ->
  test(10,multbuf3,{always,[]}).
test3f() ->
  test(10,multbuf3,{fcfs,[]}).
test3q() ->
  test(10,multbuf3,{queue_sched,[multibuffer]}).
test3q2() ->
  test(10,multbuf3,{queue_sched2,[multibuffer]}).
test3sm() ->
  test(10,multbuf3,{smallest_first,[multibuffer]}).

testAll() ->
  Combinations =
    product
      (
      [
       [multbuf1,multbuf2,multbuf3]
       ,[{always,[]},{fcfs,[]},{queue_sched,[multibuffer]},{queue_sched2,[multibuffer]},
	 {smallest_first,[multibuffer]}]
       ,[[{enforce_progress,true}],[{enforce_progress,false}]]
      ]
     ),
  Results =
    lists:map
      (fun (Spec=[Implementation,Scheduler,Options]) ->
	   io:format("~nBefore running a test:~n~n"),
	   case test(10,Implementation,Scheduler,Options) of
	     false -> {Spec,false,eqc:counterexample()};
	     true -> {Spec,true,void}
	   end
       end,
       Combinations),
  io:format("~n~n~nSummary:~n"),
  lists:foreach
    (fun ({Spec,Success,_Counterexample}) ->
	 case Success of 
	   true ->
	     io:format("~nSpec ~p succeeded~n",[Spec]);
	   false ->
	     io:format("~nSpec ~p failed~n",[Spec])
	 end
     end, Results),
  io:format("~n~n~nCounterexamples:~n"),
  lists:foreach
    (fun ({Spec,Success,_Counterexample}) ->
	 case Success of 
	   true ->
	     ok;
	   false ->
	     io:format
	       ("~nSpec ~p failed~n",
		[Spec])
	 end
     end, Results).

product(Sets) ->
  product(Sets,[]).

product([],Elems) ->
  [(lists:reverse(Elems))];
product([Set|RestSets],Elems) ->
  lists:flatmap
    (fun (Elem) ->
	 product(RestSets,[Elem|Elems])
     end, Set).

test(Max,Imp,Prio) ->
  test(Max,Imp,Prio,[]).
test(Max,Imp,Prio,Options) ->
  io:format("Testing ~p under priority ~p with max=~p and options ~p~n",[Imp,Prio,Max,Options]),
  DataSpec = {multibuffer,[Max]},
  TestingSpec =
    %% {multibuffer_commands,[Max,7,7,Imp]},
    {fsms,[{7,{multibuffer_reader_fsm,[Max,Imp]}},
	   {7,{multibuffer_writer_fsm,[Max,Imp]}}]},
  tester:test(Options++[{start_fun,start(Imp,Max)},{no_par,true},{needs_java,false}],DataSpec,Prio,TestingSpec).

start(Implementation,Max) ->
  fun (_,_) ->
      Implementation:start(Max)
  end.




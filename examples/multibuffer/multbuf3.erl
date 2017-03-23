-module(multbuf3).
-compile(export_all).

%%-define(debug,true).
-include("../../src/debug.hrl").

start_link([Max],Options) ->
  BufferPid = spawn_link(fun () -> multibuffer(Max,[],[]) end),
  register(multibuffer,BufferPid),
  ?TIMEDLOG("registered ~p as multibuffer~n",[BufferPid]),
  {ok,BufferPid}.

multibuffer(Max,L,Queue) ->
  receive
    Call ->
      ?TIMEDLOG("Got call ~p in state {~p,~p,~p}~n",[Call,Max,L,Queue]),
      case shr_calls:msg(Call) of
	{init,[]} -> 
	  multibuffer(Max,[],[]);
	_ -> 
	  NewQueue = Queue++[Call],
	  runJobs(Max,L,NewQueue,[])
      end
  end.
      
runJobs(Max,L,[],Seen) ->
  multibuffer(Max,L,lists:reverse(Seen));
runJobs(Max,L,[Call|Rest],Seen) ->
  ?TIMEDLOG
     ("Max=~p L=~p Queue=~p Seen=~p~n",
      [Max,L,[Msg|Rest],Seen]),
  case shr_calls:msg(Call) of
    {put,[R]} ->
      case Max >= length(R)+length(L) of
	true ->
	  shr_calls:reply(Call,ok),
	  runJobs(Max,L++R,lists:reverse(Seen,Rest),[]);
	false ->
	  runJobs(Max,L,Rest,[Call|Seen])
      end;
    {get,[N]} ->
      case N =< length(L) of
	true ->
	  {Prefix,Suffix} = lists:split(N,L),
	  shr_calls:reply(Call,{ok,Prefix}),
	  runJobs(Max,Suffix,lists:reverse(Seen,Rest),[]);
	false ->
	  runJobs(Max,L,Rest,[Call|Seen])
      end
  end.
  
%%% multibuffer_tests:check_test({multbuf3,{shr_queue_sched2,[multibuffer_shr]},true}).
%%% 
%%% 

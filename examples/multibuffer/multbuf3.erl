-module(multbuf3).
-compile(export_all).

%%-define(debug,true).
-include("../../src/debug.hrl").

start_link([N,Max],Options) ->
  BufferPid = spawn_link(fun () -> multibuffer(Max,[],[]) end),
  APIPids = 
    lists:map
      (fun (_) -> spawn_link(fun () -> api() end) end,
       lists:duplicate(N,N)),
  register(multibuffer,BufferPid),
  ?TIMEDLOG("registered ~p as multibuffer~n",[BufferPid]),
  {ok,[BufferPid|APIPids]}.

multibuffer(Max,L,Queue) ->
  receive
    Msg={put,_} ->
      ?TIMEDLOG("Got msg ~p in state {~p,~p,~p}~n",[Msg,Max,L,Queue]),
      NewQueue = Queue++[Msg],
      runJobs(Max,L,NewQueue,[]);
    Msg={get,_} ->
      ?TIMEDLOG("Got msg ~p in state {~p,~p,~p}~n",[Msg,Max,L,Queue]),
      NewQueue = Queue++[Msg],
      runJobs(Max,L,NewQueue,[]);
    init ->
      multibuffer(Max,[],[])
  end.
      
runJobs(Max,L,[],Seen) ->
  multibuffer(Max,L,lists:reverse(Seen));
runJobs(Max,L,[Msg|Rest],Seen) ->
  ?TIMEDLOG
     ("Max=~p L=~p Queue=~p Seen=~p~n",
      [Max,L,[Msg|Rest],Seen]),
  case Msg of
    {put,{R,Pid}} ->
      case Max >= length(R)+length(L) of
	true ->
	  Pid!ok,
	  runJobs(Max,L++R,lists:reverse(Seen,Rest),[]);
	false ->
	  runJobs(Max,L,Rest,[Msg|Seen])
      end;
    {get,{N,Pid}} ->
      case N =< length(L) of
	true ->
	  {Prefix,Suffix} = lists:split(N,L),
	  Pid!{ok,Prefix},
	  runJobs(Max,Suffix,lists:reverse(Seen,Rest),[]);
	false ->
	  runJobs(Max,L,Rest,[Msg|Seen])
      end
  end.

api() ->
  receive
    Call ->
      case shr_calls:msg(Call) of
	{put,[R]} ->
	  shr_calls:reply(Call,put1(R)),
	  api();
	{get,[N]} ->
	  shr_calls:reply(Call,get1(N)),
	  api();
	Other ->
	  ?TIMEDLOG("*** Error: incorrectly formed message ~p~n",[Other]),
	  throw(bad)
      end
  end.

put1(R) ->
  ?TIMEDLOG("put(~p)~n",[R]),
  Result = put2(R),
  ?TIMEDLOG("put(~p) returns~n",[R]), 
  Result.

put2(R) ->
  multibuffer!{put,{R,self()}},
  receive
    ok -> void;
    nok -> timer:sleep(1), put2(R)
  end.

get1(N) ->
  ?TIMEDLOG("get(~p)~n",[N]),
  Result = get2(N),
  ?TIMEDLOG("get(~p) returns ~p~n",[N,Result]), 
  Result.

get2(N) ->
  multibuffer!{get,{N,self()}},
  receive
    {ok,R} -> R;
    nok -> timer:sleep(1), ?MODULE:get2(N)
  end.
  


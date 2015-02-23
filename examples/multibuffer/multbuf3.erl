-module(multbuf3).
-compile(export_all).

start([Max],Options) ->
  case whereis(multibuffer) of
    undefined ->
      register(multibuffer,spawn(fun () -> multibuffer(Max,[],[]) end));
    _ ->
      exit(whereis(multibuffer),killed),
      timer:sleep(5),
      start([Max],Options)
  end.

multibuffer(Max,L,Queue) ->
  receive
    Msg={put,_} ->
      NewQueue = Queue++[Msg],
      runJobs(Max,L,NewQueue,[]);
    Msg={get,_} ->
      NewQueue = Queue++[Msg],
      runJobs(Max,L,NewQueue,[]);
    init ->
      multibuffer(Max,[],[])
  end.
      
runJobs(Max,L,[],Seen) ->
  multibuffer(Max,L,lists:reverse(Seen));
runJobs(Max,L,[Msg|Rest],Seen) ->
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

put(R) ->
  link(whereis(multibuffer)),
  put1(R).
put1(R) ->
  multibuffer!{put,{R,self()}},
  receive
    ok -> void;
    nok -> timer:sleep(1), put1(R)
  end.

get(N) ->
  link(whereis(multibuffer)),
  get1(N).
get1(N) ->
  multibuffer!{get,{N,self()}},
  receive
    {ok,R} -> R;
    nok -> timer:sleep(1), ?MODULE:get1(N)
  end.
  


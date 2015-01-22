-module(multbuf3).
-compile(export_all).

start(Max) ->
  case whereis(multibuffer) of
    undefined ->
      register(multibuffer,spawn(fun () -> multibuffer(Max,[],[]) end));
    _ ->
      exit(whereis(multibuffer),killed),
      timer:sleep(5),
      start(Max)
  end.

multibuffer(Max,L,Queue) ->
  receive
    Msg={put,_} ->
      runJobs(Max,L,Queue++[Msg]);
    Msg={get,_} ->
      runJobs(Max,L,Queue++[Msg]);
    init ->
      multibuffer(Max,[],[])
  end.
      
runJobs(Max,L,Queue) ->
  {NewL,NewQueue} =
    lists:foldl
      (fun (Msg={put,{R,Pid}},{FL,Acc}) ->
	   case Max >= length(R)+length(L) of
	     true ->
	       Pid!ok,
	       {FL++R,Acc};
	     false ->
	       {FL,[Msg|Acc]}
	   end;
	   (Msg={get,{N,Pid}},{FL,Acc}) ->
	   case N =< length(L) of
	     true ->
	       {Prefix,Suffix} = lists:split(N,L),
	       Pid!{ok,Prefix},
	       {Suffix,Acc};
	     false ->
	       {FL,[Msg|Acc]}
	   end
       end, {L,[]}, Queue),
  multibuffer(Max,NewL,lists:reverse(NewQueue)).

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
  


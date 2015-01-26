-module(multbuf2).
-compile(export_all).

%% A trivial implementation that serves calls strictly-in-order

start(Max) ->
  case whereis(multibuffer) of
    undefined ->
      register(multibuffer,spawn(fun () -> multibuffer(Max,[]) end));
    _ ->
      exit(whereis(multibuffer),killed),
      timer:sleep(5),
      start(Max)
  end.

multibuffer(Max,L) ->
  receive
    {put,{R,Pid}} ->
      case Max >= length(R)+length(L) of
	true ->
	  Pid!ok,
	  multibuffer(Max,L++R);
	false ->
	  wait_forever()
      end;
    {get,{N,Pid}} ->
      case N =< length(L) of
	true ->
	  {Prefix,Suffix} = lists:split(N,L),
	  Pid!{ok,Prefix},
	  multibuffer(Max,Suffix);
	false ->
	  Pid!nok,
	  wait_forever()
      end;
    init ->
      multibuffer(Max,[])
  end.

wait_forever() ->
  receive _ -> wait_forever() end.

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
  


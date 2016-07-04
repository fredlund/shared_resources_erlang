-module(multbuf2).
-compile(export_all).

%%-define(debug,true).
-include("../../src/debug.hrl").

%% A trivial implementation that serves calls strictly-in-order

start_link([N,Max],Options) ->
  BufferPid = spawn_link(fun () -> multibuffer(Max,[]) end),
  APIPids = 
    lists:map
      (fun (_) -> spawn_link(fun () -> api() end) end,
       lists:duplicate(N,N)),
  register(multibuffer,BufferPid),
  ?TIMEDLOG("registered ~p as multibuffer~n",[BufferPid]),
  {ok,[BufferPid|APIPids]}.

multibuffer(Max,L) ->
  receive
    _Msg={put,{R,Pid}} ->
      case Max >= length(R)+length(L) of
	true ->
	  Pid!ok,
	  multibuffer(Max,L++R);
	false ->
	  wait_forever()
      end;
    _Msg={get,{N,Pid}} ->
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

api() ->
  receive
    Call ->
      case shr_calls:msg(Call) of
	{put,[R]} ->
	  shr_calls:reply(Call,put1(R)),
	  api();
	{get,[N]} ->
	  shr_calls:reply(Call,get1(N)),
	  api()
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
  


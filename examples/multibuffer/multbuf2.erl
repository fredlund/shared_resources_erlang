-module(multbuf2).
-compile(export_all).

%%-define(debug,true).
-include("../../src/debug.hrl").

%% A trivial implementation that serves calls strictly-in-order

start_link([Max],Options) ->
  BufferPid = spawn_link(fun () -> multibuffer(Max,[]) end),
  register(multibuffer,BufferPid),
  ?TIMEDLOG("registered ~p as multibuffer~n",[BufferPid]),
  {ok,BufferPid}.

multibuffer(Max,L) ->
  receive
    Call ->
      case shr_calls:msg(Call) of
	{put,[R]} ->
	  case Max >= length(R)+length(L) of
	    true ->
	      shr_calls:reply(Call,ok),
	      multibuffer(Max,L++R);
	    false ->
	      wait_forever()
	  end;
	{get,[N]} ->
	  case N =< length(L) of
	    true ->
	      {Prefix,Suffix} = lists:split(N,L),
	      shr_calls:reply(Call,Prefix),
	      multibuffer(Max,Suffix);
	    false ->
	      wait_forever()
	  end;
	{init,[]} ->
	  shr_calls:reply(Call,ok),
	  multibuffer(Max,[])
      end
   end.

wait_forever() ->
  receive _ -> wait_forever() end.

  


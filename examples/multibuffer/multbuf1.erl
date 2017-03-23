-module(multbuf1).
-compile(export_all).

%% A trivial implementation that serves continously enabled calls in order
%%
%% For the continously enabled scheduler it fails for instance the following
%% trace (because of the simultaenous execution of the calls the Erlang sends
%% have a race condition): 
%%
%% Test failed with reason {postcondition,false}
%%
%% Command sequence:
%% -----------------
%%
%%  start [[{needs_java,false}],
%%       multibuffer_commands,
%%       {teststate,10,7,7,0,0,multbuf1}] 
%%  << <0.1732.0>:get(4) >> 
%%  << <0.1733.0>:get(1) >> 
%%  << <0.1734.0>:get(1) >> 
%%  << <0.1735.0>:put([0]),
%%     <0.1736.0>:put([0,0,0]) >> -- unblocks <0.1735.0>:put([0]) -> void, <0.1736.0>:put([0,0,0]) -> void, <0.1732.0>:get(4) -> [0,0,0,0]
%%

%%-define(debug,true).
-include("../../src/debug.hrl").

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
	      multibuffer(Max,L)
	  end;
	{get,[N]} ->
	  case N =< length(L) of
	    true ->
	      {Prefix,Suffix} = lists:split(N,L),
	      shr_calls:reply(Call,Prefix),
	      multibuffer(Max,Suffix);
	    false ->
	      multibuffer(Max,L)
	  end;
	{init,[]} ->
	  shr_calls:reply(Call,ok),
	  multibuffer(Max,[])
      end
  end.

  


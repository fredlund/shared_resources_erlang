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
	  Pid!nok,
	  multibuffer(Max,L)
      end;
    _Msg={get,{N,Pid}} ->
      case N =< length(L) of
	true ->
	  {Prefix,Suffix} = lists:split(N,L),
	  Pid!{ok,Prefix},
	  multibuffer(Max,Suffix);
	false ->
	  Pid!nok,
	  multibuffer(Max,L)
      end;
    init ->
      multibuffer(Max,[])
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
  


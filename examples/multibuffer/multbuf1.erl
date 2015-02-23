-module(multbuf1).
-compile(export_all).

%% A trivial implementation that serves continously enabled calls in order
%%
%% For the continously enabled scheduler it fails for instance the following
%% trace (because of the simulatenous execution of the calls the Erlang sends
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

start([Max],Options) ->
  case whereis(multibuffer) of
    undefined ->
      register(multibuffer,spawn(fun () -> multibuffer(Max,[]) end));
    _ ->
      exit(whereis(multibuffer),killed),
      timer:sleep(5),
      start([Max],Options)
  end.

multibuffer(Max,L) ->
  receive
    {put,{R,Pid}} ->
      case Max >= length(R)+length(L) of
	true ->
	  Pid!ok,
	  multibuffer(Max,L++R);
	false ->
	  Pid!nok,
	  multibuffer(Max,L)
      end;
    {get,{N,Pid}} ->
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
  


-module(multibuffer).

-behaviour(resource_data_implementation).

-export([init/2,pre/2,cpre/2,post/2,return/3,return_value/2]).

-record(state,{max,seq}).

init([MAX],_Options) ->
  #state{max=MAX,seq=[]}.


pre({put,[R]},State) ->
  length(R) =< State#state.max div 2;

pre({get,[N]},State) ->
  N =< State#state.max div 2.


cpre({put,[R]},State) ->
  length(R) =< State#state.max-length(State#state.seq);

cpre({get,[N]},State) ->
  length(State#state.seq) >= N.


post({put,[R]},State) ->
  State#state{seq=State#state.seq++R};

post({get,[N]},State) ->
  State#state{seq=lists:nthtail(N,State#state.seq)}.


%% Any return values is ok for put
return(_State,{put,_},Result) ->
  case Result of
    {'EXIT',_} -> false;
    _ -> true
  end.

%% A concrete return value for get
return_value(State,{get,[N]}) ->
  {Prefix,_} = lists:split(N,State#state.seq),
  Prefix.




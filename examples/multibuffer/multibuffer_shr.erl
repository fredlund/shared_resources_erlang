-module(multibuffer_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).

-record(state,{max,seq}).

initial_state([MAX],_Options) ->
  #state{max=MAX,seq=[]}.


pre({put,[R]},State) ->
  length(R) =< State#state.max div 2;

pre({get,[N]},State) ->
  N =< State#state.max div 2.


cpre({put,[R]},State) ->
  length(R) =< State#state.max-length(State#state.seq);

cpre({get,[N]},State) ->
  length(State#state.seq) >= N.

post({put,[R]},_Result,State) ->
  State#state{seq=State#state.seq++R};

post({get,[N]},_Result,State) ->
  State#state{seq=lists:nthtail(N,State#state.seq)}.


%% Any return values is ok for put
return(_State,{put,_},Result) ->
  true;
return(State,{get,[N]},Result) ->
  {Prefix,_} = lists:split(N,State#state.seq),
  Prefix==Result.

%% A concrete return value for get
return_value(Call,State) ->
  case Call of
    {get,[N]} -> 
      {Prefix,_} = lists:split(N,State#state.seq),
      Prefix;
    _ ->
      underspecified
  end.
      
      




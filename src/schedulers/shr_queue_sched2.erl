%% A queue scheduler that preserves ordering for enabled processes
%% (and preserves the ordering when processes become non-enabled)

-module(shr_queue_sched2).

-behaviour(shr_wait_implementation).

-export([initial_state/2,new_waiting/3,priority_enabled/4,post_waiting/4]).

-record(state,{statemodule,queue,time}).

%%-define(debug,true).
-include("../../src/debug.hrl").

initial_state(_,Options) ->
  StateMod = proplists:get_value(state_module,Options),
  #state{statemodule=StateMod,queue=orddict:new(),time=0}.

new_waiting(Call,State,_DataState) ->
  Time = State#state.time,
  {Time,
   State#state{queue=State#state.queue++[{Time,Call}],time=Time+1}}.

priority_enabled(_Call,Time,State,DataState) ->
  ?LOG
    ("priority_enabled(~p) @time ~p;~nstate=~p~nDataState=~p~n",
     [_Call,Time,State,DataState]),
  AnyEarlier = 
    any_earlier
      (Time,
       fun (_,Call) -> (State#state.statemodule):cpre(Call,DataState) end,
       State#state.queue),
  ?LOG
     ("priority_enabled(~p) @time ~p; earlier=~p~n",
      [_Call,Time,AnyEarlier]),
  false == AnyEarlier.

post_waiting(Call,Time,State,_DataState) ->
  State#state{queue=State#state.queue--[{Time,Call}]}.

any_earlier(_Time,_F,[]) -> false;
any_earlier(Time,F,[{Time1,Call1}|Rest]) ->
  if
    Time=<Time1 -> false;
    true -> 
      case F(Time1,Call1) of
	true -> {ok,{Time1,Call1}};
	false -> any_earlier(Time,F,Rest)
      end
  end.


	 
	 
    


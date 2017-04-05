%% A queue scheduler that preserves ordering for enabled processes

-module(shr_smallest_first).

-behaviour(shr_wait_implementation).

-export([initial_state/2,new_waiting/3,priority_enabled/4,post_waiting/4]).

-record(state,{statemodule,queue,time}).

initial_state(_,Options) ->
  StateMod = proplists:get_value(state_module,Options),
  #state{statemodule=StateMod,queue=orddict:new(),time=0}.

new_waiting(Call,State,_DataState) ->
  Time = State#state.time,
  Size = 
    case Call of
      {put,[R]} -> length(R);
      {get,[N]} -> N
    end,
  Item = {Size,Time,Call},
  {Item,
   State#state{queue=State#state.queue++[Item],time=Time+1}}.

priority_enabled(_Call,{Size,_Time,_Call},State,DataState) ->
  false ==
    lists:any
      (fun ({Size0,_,Call0}) ->
	   (Size0 < Size) andalso (State#state.statemodule):cpre(Call0,DataState)
       end,
       State#state.queue).

post_waiting(_Call,Item,State,_DataState) ->
  State#state{queue=State#state.queue--[Item]}.



	 
	 
    


-module(robot_fsm).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

-record(state,{n_naves,next,weight}).

-define(PESO_FACTOR,10).


init(Id,[N_NAVES]) ->
  #state{n_naves=N_NAVES,next={enter,0},weight=100}.

precondition(Id,State,_GlobalState,{CallType,Nave,Weight}) ->
  ({CallType,Nave} == State#state.next)
    andalso if
	      CallType==exit -> Weight==State#state.weight;
	      true -> Weight >= State#state.weight
	    end.

command(Id,State,_GlobalState) ->
 case State#state.next of
   stopped ->
     stopped;
   {exit,Nave} ->
     {exit,Nave,State#state.weight};
   {enter,Nave} ->
     {enter,Nave,peso(State#state.weight)}
 end.

next_state(Id,State,_GlobalState,Call) ->
  NAVES_LIMIT = State#state.n_naves-1,
  case Call of
    {exit,NAVES_LIMIT,W} ->
      State#state{next=stopped,weight=W};
    {enter,N,W} ->
      State#state{next={exit,N},weight=W};
    {exit,N,W} ->
      State#state{next={enter,N+1},weight=W}
  end.

peso(P) ->
  Pdiv = P div 100,
  ?LET(X,eqc_gen:choose(Pdiv,?PESO_FACTOR),X*100).

     
     
	
  


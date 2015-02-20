-module(robot_fsm).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

-record(rstate,{n_naves,next,weight,controller}).


-define(PESO_FACTOR,10).


init(Id,[N_NAVES]) ->
  #rstate{n_naves=N_NAVES,next={enter,0},weight=100}.

precondition(Id,State,_GlobalState,{_,CallType,[_,Nave,Weight]}) ->
  ({CallType,Nave} == State#rstate.next)
    andalso if
	      CallType==exit -> Weight==State#rstate.weight;
	      true -> Weight >= State#rstate.weight
	    end.

command(Id,State,_GlobalState) ->
 case State#rstate.next of
   stopped ->
     stopped;
   {exit,Nave} ->
     {?MODULE,exit,[Id,Nave,State#rstate.weight]};
   {enter,Nave} ->
     {?MODULE,enter,[Id,Nave,peso(State#rstate.weight)]}
 end.

next_state(Id,State,_GlobalState,Call) ->
  NAVES_LIMIT = State#rstate.n_naves-1,
  case Call of
    {_,exit,[_,NAVES_LIMIT,W]} ->
      {State#rstate{next=stopped,weight=W},void};
    {_,enter,[_,N,W]} ->
      {State#rstate{next={exit,N},weight=W},void};
    {_,exit,[_,N,W]} ->
      {State#rstate{next={enter,N+1},weight=W},void}
  end.

peso(P) ->
  Pdiv = P div 100,
  ?LET(X,eqc_gen:choose(Pdiv,?PESO_FACTOR),X*100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enter(Robot,Nave,Peso) ->     
  java:call(tester:get_data(controller),solicitarEntrar,[Nave,Peso]).

exit(Robot,Nave,Peso) ->     
  java:call(tester:get_data(controller),solicitarSalir,[Nave,Peso]).

     
	
  


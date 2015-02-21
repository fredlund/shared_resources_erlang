-module(robot_fsm).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

-include("../../testing/src/tester.hrl").

-record(rstate,{n_naves,next,controller}).


-define(PESO_FACTOR,11).


init(Id,[N_NAVES]) ->
  #rstate{n_naves=N_NAVES,next={enter,0,100}}.

precondition(Id,#rstate{next={NCallType,NNave,NWeight}},_,
	     {_,CallType,[_,Nave,Weight]}) ->
  (CallType==NCallType)
    andalso (Nave==NNave)
    andalso if
	      CallType==exit -> Weight==NWeight;
	      true -> Weight >= NWeight
	    end.

command(Id,#rstate{next=Next},_GlobalState) ->
 case Next of
   stopped ->
     stopped;
   {exit,Nave,Weight} ->
     {?MODULE,exit,[Id,Nave,Weight]};
   {enter,Nave,Weight} ->
     {?MODULE,enter,[Id,Nave,peso(Weight)]}
 end.

next_state(Id,State=#rstate{next=Next,n_naves=N_NAVES},GS,Job) ->
  NavesLimit =
    N_NAVES-1,
  NewNext =
    case Next of
      {exit,NavesLimit,_} ->
	stopped;
      {exit,Nave,Weight} ->
	{enter,Nave+1,Weight};
      {enter,Nave,Weight} ->
	{exit,Nave,Weight}
    end,
  {State#rstate{next=NewNext},GS}.

peso(P) ->
  Pdiv = P div 100,
  ?LET(X,eqc_gen:choose(Pdiv,?PESO_FACTOR),X*100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enter(Robot,Nave,Peso) ->     
  java:call(tester:get_data(controller),solicitarEntrar,[Nave,Peso]).

exit(Robot,Nave,Peso) ->     
  java:call(tester:get_data(controller),solicitarSalir,[Nave,Peso]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Job,Id,State,GlobalState) ->
  case Job#job.call of
    {_,_,[R,_,_]} -> io_lib:format("~p",[R])
  end.

print_started_job_info(Job,Id,State,GlobalState) ->
  case Job#job.call of
    {_,CallType,[R,N,P]} -> io_lib:format("~p(~p,~p,~p)",[CallType,R,N,P])
  end.

print_state(MachineId,#rstate{next={CallType,Nave,Weight}},IsBlocked) ->      
  case {CallType,Nave,IsBlocked} of
    {enter,0,false} -> "";
    _ -> io_lib:format("~p(~p,~p,~p)",[CallType,MachineId,Nave,Weight])
  end.



-module(robot_fsm).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

-include("../../testing/src/tester.hrl").

-record(rstate,{n_naves,next,controller,implementation}).


-define(PESO_FACTOR,10).


init(Id,[N_NAVES,Implementation]) ->
  #rstate
    {n_naves=N_NAVES,
     next={Implementation,enter,[Id,0,100]},
     implementation=Implementation}.

precondition(Id,
	     #rstate{next={NModule,NCallType,[NId,NNave,NWeight]}},_,
	     {Module,CallType,[Id,Nave,Weight]}) ->
  {NModule,NCallType,NId,NNave} == {Module,CallType,Id,Nave}
  andalso if
	    CallType==exit -> Weight==NWeight;
	    true -> Weight >= NWeight
	  end.

command(Id,#rstate{next=Next,implementation=Implementation},_GlobalState) ->
 case Next of
   {Implementation,enter,[Id,Nave,Weight]} ->
     {Implementation,enter,[Id,Nave,peso(Weight)]};
   _ ->
     Next
 end.

next_state(Id,State=#rstate{next=Next,n_naves=N_NAVES,implementation=Implementation},GS,Job) ->
  NavesLimit =
    N_NAVES-1,
  NewNext =
    case Job#job.call of
      {_,exit,[_,NavesLimit,_]} ->
	stopped;
      {_,exit,[_,Nave,Weight]} ->
	{Implementation,enter,[Id,Nave+1,Weight]};
      {_,enter,[_,Nave,Weight]} ->
	{Implementation,exit,[Id,Nave,Weight]}
    end,
  {State#rstate{next=NewNext},GS}.

peso(P) ->
  Pdiv = P div 100,
  ?LET(X,eqc_gen:choose(Pdiv,?PESO_FACTOR),X*100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Job,Id,State,GlobalState) ->
  case Job#job.call of
    {_,_,[R,_,_]} -> io_lib:format("~p",[R])
  end.

print_started_job_info(Job,Id,State,GlobalState) ->
  case Job#job.call of
    {_,CallType,[R,N,P]} -> io_lib:format("~p(~p,~p,~p)",[CallType,R,N,P])
  end.

print_state(_,#rstate{next={_,CallType,[Id,Nave,Weight]}},IsBlocked) ->      
  case {CallType,Nave,IsBlocked} of
    {enter,0,false} -> "";
    _ -> io_lib:format("~p(~p,~p,~p)",[CallType,Id,Nave,Weight])
  end.



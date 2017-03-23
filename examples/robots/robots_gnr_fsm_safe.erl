-module(robots_gnr_fsm_safe).

-export([initial_state/3,precondition/4,command/3,next_state/4]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

%%-define(debug,true).
-include("../../src/debug.hrl").


-include_lib("eqc/include/eqc.hrl").


-record(rstate,{num_warehouses,next,myid}).


-define(PESO_FACTOR,10).


initial_state(Id,_,Options) ->
  NumWarehouses = proplists:get_value(num_warehouses,Options),
  {ok,#rstate{myid=Id,num_warehouses=NumWarehouses,next={controller,enter,[Id,0,100]}}}.

precondition(Id,
	     #rstate{next={NType,NCallType,[NId,NNave,NWeight]}},_,
	     Cmd) ->
  {Type,CallType,[Id,Nave,Weight]} = strip_id(Cmd),
  {NType,NCallType,NId,NNave} == {Type,CallType,Id,Nave}
  andalso if
	    (CallType==enter) andalso (Type==controller) -> Weight>=NWeight;
	    true -> Weight == NWeight
	  end.

command(Id,State=#rstate{next=Next},_GlobalState) ->
  Command =
    case Next of
      {controller,enter,[Id,Nave,Weight]} ->
	{controller,enter,[Id,Nave,peso(Weight)]};
      _ ->
	Next
    end,
  ?TIMEDLOG
     ("command(id=~p,next=~p,controller=~p) =>~n ~p~n",
      [Id,Next,controller,Command]),
  add_id(Command,State).

next_state(Id,State=#rstate{num_warehouses=NUM_WAREHOUSES},GS,Call) ->
  NavesLimit =
    NUM_WAREHOUSES-1,
  NewNext =
    case strip_id(Call) of
      {controller,Fun,Args} when ((Fun==enter) or (Fun==exit)) ->
	{environment,Fun,Args};
      {controller,entered,Args} ->
	{controller,exit,Args};
      {controller,exited,[_,Nave,Weight]} ->
	{controller,enter,[Id,Nave+1,Weight]};

      {environment,enter,Args} ->
	{controller,entered,Args};
      {environment,exit,[_,NavesLimit,_]} ->
	stopped;
      {environment,exit,Args} ->
	{controller,exited,Args}
    end,
  {State#rstate{next=NewNext},GS}.

peso(P) ->
  Pdiv = P div 100,
  ?LET(X,eqc_gen:choose(Pdiv,?PESO_FACTOR),X*100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[R,_,_]} -> io_lib:format("~p",[R])
  end.

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  shr_utils:print_mfa(strip_id(Call)).

print_state(_,#rstate{next={_,CallType,[Id,Nave,Weight]}},IsBlocked) ->      
  case {CallType,Nave,IsBlocked} of
    {enter,0,false} -> "";
    _ -> io_lib:format("~p(~p,~p,~p)",[CallType,Id,Nave,Weight])
  end.

add_id({Type,Command,Args},State) ->
  {{Type,State#rstate.myid},Command,Args};
add_id(Other,_) ->
  Other.

strip_id({{Type,_},Command,Args}) ->
  {Type,Command,Args};
strip_id(Other) ->
  Other.



  

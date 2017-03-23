-module(multibuffer_reader_fsm).

-export([initial_state/3,precondition/4,command/3,next_state/4]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

-include_lib("eqc/include/eqc.hrl").


-record(mstate,{max,myid=Id}).


initial_state(Id,[Max],_) ->
  {ok,#mstate{max=Max,myid=Id}}.

precondition(_Id,_State,_GS,_Call) ->
  true.

command(_Id,#mstate{max=Max,myid=MyId},_GS) ->
  {{multibuffer_user,MyId},get,[choose(0,Max div 2)]}.

next_state(_Id,State,GS,_Job) ->
  {State,GS}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(_Call,Id,_State,_GlobalState) ->
  io_lib:format("~p",[Id]).

print_started_job_info(Call,Id,_State,_GlobalState) ->
  case Call of
    {_,CallType,[Arg]} -> io_lib:format("~p:~p(~p)",[Id,CallType,Arg])
  end.

print_state(Id,_State,IsBlocked) ->      
  if
    IsBlocked -> io_lib:format("~p:r",[Id]);
    true -> ""
  end.




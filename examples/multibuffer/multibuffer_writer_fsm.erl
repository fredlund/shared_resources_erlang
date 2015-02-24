-module(multibuffer_writer_fsm).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

-include("../../testing/src/tester.hrl").

-record(mstate,{max,implementation}).


init(_Id,[Max,Implementation]) ->
  #mstate{max=Max,implementation=Implementation}.

precondition(_Id,_State,_GS,_Call) ->
  true.

command(_Id,#mstate{max=Max,implementation=Implementation},_) ->
  {Implementation,put,[nats(Max div 2)]}.

next_state(_Id,State,GS,_Job) ->
  {State,GS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nats(N) ->
  ?LET(Num,eqc_gen:choose(1,N),lists:duplicate(Num,nat())).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(_Job,Id,_State,_GlobalState) ->
  io_lib:format("~p",[Id]).

print_started_job_info(Job,Id,_State,_GlobalState) ->
  case Job#job.call of
    {_,CallType,[Arg]} -> io_lib:format("~p:~p(~p)",[Id,CallType,Arg])
  end.

print_state(Id,_State, IsBlocked) ->      
  if
    IsBlocked -> io_lib:format("~p:w",[Id]);
    true -> ""
  end.




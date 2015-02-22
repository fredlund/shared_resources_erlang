-module(multibuffer_reader_fsm).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

-include("../../testing/src/tester.hrl").

-record(mstate,{max,implementation}).


init(Id,[Max,Implementation]) ->
  #mstate{max=Max,implementation=Implementation}.

precondition(_Id,_State,_GS,_Call) ->
  true.

command(_Id,#mstate{implementation=Implementation,max=Max},_GlobalState) ->
  {Implementation,get,[choose(1,Max div 2)]}.

next_state(_Id,State,GS,_Job) ->
  {State,GS}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Job,Id,State,GlobalState) ->
  io_lib:format("~p",[Id]).

print_started_job_info(Job,Id,State,GlobalState) ->
  case Job#job.call of
    {_,CallType,[Arg]} -> io_lib:format("~p:~p(~p)",[Id,CallType,Arg])
  end.

print_state(Id,_State,_IsBlocked) ->      
  io_lib:format("~p:r",[Id]).



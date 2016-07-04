-module(gritter_gnr_fsm).

-export([initial_state/3,precondition/4,command/3,next_state/4]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

%%-define(debug,true).
-include("../../src/debug.hrl").


-include_lib("eqc/include/eqc.hrl").


-record(rstate,{num_readers}).


initial_state(_Id,_,Options) ->
  NumReaders = proplists:get_value(max_uids,Options),
  {ok,#rstate{num_readers=NumReaders}}.

precondition(_,_,_,_) ->
  true.

command(Id,#rstate{num_readers=NumReaders},_GlobalState) ->
  oneof
    (
    [
     {controller,leer,[Id]}
     ,{controller,seguir,[Id,reader(NumReaders),bool()]}
     ,{controller,dejarDeSeguir,[Id,reader(NumReaders)]}
     ,{controller,enviar,[Id,grito(),bool()]}
    ]
   ).

grito() ->
  oneof(["m1","m2","m3"]).

reader(NumReaders) ->
  choose(1,NumReaders).

next_state(_,State,GlobalState,_) ->
  {State,GlobalState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[R|_]} -> io_lib:format("~p",[R])
  end.

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  shr_utils:print_mfa(Call).

print_state(_,_State,_IsBlocked) ->      
  "reader".


  

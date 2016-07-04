-module(gritter_gnr_fsm2).

-export([initial_state/3,precondition/4,command/3,next_state/4]).
-export([print_started_job_info/4, print_finished_job_info/4, print_state/3]).

-behaviour(shr_fsm).

%%-define(debug,true).
-include("../../src/debug.hrl").


-include_lib("eqc/include/eqc.hrl").


-record(rstate,{num_readers,seguiendo}).


initial_state(_Id,_,Options) ->
  NumReaders = proplists:get_value(max_uids,Options),
  {ok,#rstate{num_readers=NumReaders,seguiendo=sets:new()}}.

precondition(_,_,_,_) ->
  true.

command(Id,#rstate{num_readers=NumReaders,seguiendo=Seguiendo},_GlobalState) ->
  LeerFrequency = 
    case sets:size(Seguiendo) of
      0 -> 1;
      N -> 5
    end,
  frequency
    (
    [
     {LeerFrequency,{controller,leer,[Id]}}
     ,{5,seguir_call(Id,NumReaders)}
     ,{5,dejarDeSeguir_call(Id,NumReaders,Seguiendo)}
     ,{5,{controller,enviar,[Id,grito(),bool()]}}
    ]
   ).

seguir_call(Id,NumReaders) ->
  ?LET(Id2,
       frequency
	 (lists:map
	    (fun (Id2) -> 
		 if 
		   Id==Id2 -> {1,Id2};
		   true -> {3,Id2}
		 end
	     end, readers(NumReaders))),
       ?LET(Regritos,
	    bool(),
	    {controller,seguir,[Id,Id2,Regritos]})).

dejarDeSeguir_call(Id,NumReaders,Seguiendo) ->
  ?LET(Id2,
       frequency
	 (lists:map
	    (fun (Id2) -> 
		 case sets:is_element(Id2,Seguiendo) of
		   true -> {5,Id2};
		   false -> {1,Id2}
		 end
	     end, readers(NumReaders))),
       {controller,dejarDeSeguir,[Id,Id2]}).

next_state(_Id,State=#rstate{seguiendo=Seguiendo},GS,Call) ->
  LocalState =
    case Call of
      {controller,seguir,[_,Id2,_Regritos]} ->
	State#rstate{seguiendo=sets:add_element(Id2,Seguiendo)};
      {controller,dejarDeSeguir,[_,Id2]} ->
	State#rstate{seguiendo=sets:del_element(Id2,Seguiendo)};
      _ ->
	State
    end,
  {LocalState,GS}.

grito() ->
  oneof(["m1","m2","m3"]).

readers(NumReaders) ->
  lists:seq(1,NumReaders).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
	
print_finished_job_info(Call,_Id,_State,_GlobalState) ->
  case Call of
    {_,_,[R|_]} -> io_lib:format("~p",[R])
  end.

print_started_job_info(Call,_Id,_State,_GlobalState) ->
  shr_utils:print_mfa(Call).

print_state(_,_State,_IsBlocked) ->      
  "reader".


  

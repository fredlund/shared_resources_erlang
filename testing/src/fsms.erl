-module(fsms).

-include_lib("eqc/include/eqc.hrl").
-include("tester.hrl").

-export([initial_state/0,start/2,started/2,init_state/2,precondition/3,
	 command/2,strip_call_info/1,next_state/4]).

-define(MAX_CONCURRENT,3).
-define(MAX_STATES,400).

-record(fstate,{machines,global_state,options,start,started,blocked}).

initial_state() ->
  #fstate{}.

init_state(PreMachineSpec,Options) ->
  StartFun =
    proplists:get_value(start_fun,Options,void),
  StartedFun =
    proplists:get_value(started_fun,Options,void),
  GlobalState =
    proplists:get_value(global_state,Options,void),
  MachineSpec =
    lists:foldl
      (fun ({N,MachineWithMachineInit},Acc) when is_integer(N) ->
	   lists:duplicat(N,MachineWithMachineInit)++Acc;
	   (MachineWithInit,Acc) ->
	   [MachineWithInit|Acc]
       end, [], PreMachineSpec),
  #fstate
    {
      machines=
       lists:map
	 (fun ({I,{Machine,Init}}) ->
	      {I,{Machine,Machine:init(I,Init)}}
	  end, 
	  lists:zip(lists:seq(1,length(MachineSpec)),MachineSpec)),
      options=Options,
      start=StartFun,
      started=StartedFun,
      blocked=[],
      global_state=GlobalState
    }.

start(NodeId,State) ->
  Start = State#fstate.start,
  if
    is_function(Start) -> Start(NodeId,State);
    true -> ok
  end.

started(State,Result) ->
  Started = State#fstate.started,
  if
    is_function(Started) ->
      State#fstate{global_state=Started(State,Result)};
    true ->
      State
  end.		     

precondition(_,State,Commands) ->
  lists:all
    (fun ({I,Command}) ->
	 {_,{Machine,MachineState}} = lists:keyfind(I,1,State#fstate.machines),
	 Machine:precondition(I,MachineState,State#fstate.global_state,Command)
     end, Commands).

command(State,TesterState) ->
  [command1(State,TesterState)].
command1(State,TesterState) ->
  command1(State,TesterState,0).
command1(State,TesterState,NPars) ->
  case length(State#fstate.blocked)<length(State#fstate.machines) 
    andalso permit_par(State,NPars) of
    false -> [];
    true ->
      ?LET({Command,NewState},
	   gen_mach_cmd(State),
	   case limit_states(State,TesterState) of
	     true ->
	       [Command];
	     false ->
	       ?LET(NextCommands,
		    command1(NewState,TesterState,NPars),
		    [Command|NextCommands])
	   end)
  end.

gen_mach_cmd(State) ->
  NonBlocked =
    lists:filter
      (fun ({I,_}) -> not(lists:member(I,State#fstate.blocked)) end,
       State#fstate.machines),
  case NonBlocked of
    [] ->
      {tester:make_void_call(),State};
    false ->
      ?LET({MachId,{Machine,MachineState}},
	   oneof(NonBlocked),
	   begin
	     NewBlocked = [MachId|State#fstate.blocked],
	     NewState = State#fstate{blocked=NewBlocked},
	     case Machine:command(MachId,MachineState,State#fstate.global_state) of
	       stopped ->
		 gen_mach_cmd(NewState);
	       CmdGen ->
		 {{MachId,CmdGen},NewState}
	     end
	   end)
  end.

strip_call_info({_,Call}) -> 
  Call.

next_state(State,_TesterState,Result,[Commands]) ->
  {_,FinishedJobs} =
    Result,
  FinishedMachines =
    lists:map
      (fun (Job) ->
	   {MachineId,_} = Job#job.callinfo,
	   MachineId
       end, FinishedJobs),
  NewState =
    lists:foldl
      (fun ({I,Command},S) ->
	   {_,{Machine,MachineState}} =
	     lists:keyfind(I,1,S#fstate.machines),
	   {NewMachineState,NewGlobalState} =
	     Machine:next_state
	       (I,MachineState,State#fstate.global_state,Command),
	   S#fstate
	     {machines=
		lists:keyreplace
		  (I,1,S#fstate.machines,{I,{Machine,NewMachineState}}),
	      global_state=NewGlobalState}
       end, Commands, State),
  NewState#fstate{blocked=NewState#fstate.blocked--FinishedMachines}.

permit_par(State,NPars) ->
  case proplists:get_value(max_par,State#fstate.options,?MAX_CONCURRENT) of
    false ->
      false;
    N when is_integer(N), N>0 ->
      NPars >= N
  end.

limit_states(State,TesterState) ->
  case proplists:get_value(limit_card_state,State#fstate.options,?MAX_STATES) of
    false ->
      false;
    N when is_integer(N), N>0 ->
      TesterState >= N
  end.

    

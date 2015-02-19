-module(fsms).

-include_lib("eqc/include/eqc.hrl").

-export([initial_state/0,start/2,init_state/2,precondition/3,command/2,
	 prepare/1,next_state/4]).

-define(MAX_CONCURRENT,3).
-define(MAX_STATES,400).

-record(fstate,{machines,options,start}).

initial_state() ->
  #fstate{}.

init_state({N,MachineWithMachineInit,Start},Options) ->
  Machines = lists:duplicate(N,MachineWithMachineInit),
  init_state({Start,Machines},Options);
init_state({Start,Machines},Options) ->
  #fstate
    {
     machines=
       lists:map
	 (fun ({I,{Machine,Init}}) ->
	      {I,{Machine,Machine:init(I,Init)}}
	  end, 
	  lists:zip(lists:seq(1,length(Machines)),Machines)),
     options=Options,
     start=Start
    }.

start(NodeId,State) ->
  Start = State#fstate.start,
  if
    is_function(Start) -> Start(NodeId,State);
    true -> ok
  end.

precondition(_,State,Commands) ->
  lists:all
    (fun ({I,Command}) ->
	 {_,{Machine,MachineState}} = lists:keyfind(I,1,State#fstate.machines),
	 Machine:precondition(I,MachineState,Command)
     end, Commands).

command(State,TesterState) ->
  [command1(State,TesterState)].
command1(State,TesterState) ->
  command1(State,TesterState,0,[]).
command1(State,TesterState,NPars,UsedMachines) ->
  case length(UsedMachines)<length(State#fstate.machines) 
    andalso permit_par(State,NPars) of
    false -> [];
    true ->
      ?LET({Command,NewUsedMachines},
	   gen_mach_cmd(State,UsedMachines),
	   case limit_states(State,TesterState) of
	     true -> [Command];
	     false ->
	       ?LET(NextCommands,
		    command1(State,TesterState,NPars,NewUsedMachines),
		    [Command|NextCommands])
	   end)
  end.

gen_mach_cmd(State,UsedMachines) ->
  case length(UsedMachines)>=length(State#fstate.machines) of
    true ->
      {tester:make_void_call(),UsedMachines};
    false ->
      ?LET({MachId,{Machine,MachineState}},
	   oneof(lists:filter
		   (fun ({I,_}) -> not(lists:member(I,UsedMachines)) end,
		    State#fstate.machines)),
	   case Machine:command(MachId,MachineState) of
	     stopped ->
	       gen_mach_cmd(State,[MachId|UsedMachines]);
	     CmdGen ->
	       {{MachId,CmdGen},[MachId|UsedMachines]}
	   end)
  end.

prepare(Commands) ->
  lists:map(fun ({_,Command}) -> Command end,Commands).

next_state(State,_TesterState,_Result,[Commands]) ->
  lists:foldl
    (fun ({I,Command},S) ->
	 {_,{Machine,MachineState}} = lists:keyfind(I,1,S#fstate.machines),
	 NewMachineState = Machine:next_state(I,MachineState,Command),
	 S#fstate
	   {machines=
	      lists:keyreplace
		(I,1,S#fstate.machines,{I,{Machine,NewMachineState}})}
     end, Commands, State).

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

    

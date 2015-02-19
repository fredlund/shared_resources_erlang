-module(fsms).

-include_lib("eqc/include/eqc.hrl").

-export([initial_state/0,init_state/3,precondition/3,command/2,
	 postcondition/3,prepare/1,next_state/3]).

-define(MAX_CONCURRENT,3).
-define(MAX_STATES,400).

-record(fstate,{machines,options}).

initial_state() ->
  #fstate{}.

init_state(N,MachineWithMachineInit,Options) ->
  Machines = lists:duplicate(N,MachineWithMachineInit),
  init_state(Machines,Options).

init_state(Machines,Options) ->
  #fstate
    {
     machines=
       lists:map
	 (fun ({I,{Machine,Init}}) ->
	      {I,{Machine,Machine:init(I,Init)}}
	  end, 
	  lists:zip(lists:seq(1,length(Machines)),Machines)),
     options=Options
    }.

precondition(_,State,Commands) ->
  lists:all
    (fun ({I,Command}) ->
	 {_,{Machine,MachineState}} = lists:keyfind(I,1,State#fstate.machines),
	 Machine:precondition(MachineState,Command)
     end, Commands).

command(State,CardTestState) ->
  [command1(State,CardTestState)].
command1(State,CardTestState) ->
  command1(State,CardTestState,0,[]).
command1(State,CardTestState,NPars,UsedMachines) ->
  case length(UsedMachines)<length(State#fstate.machines) 
    andalso permit_par(State,NPars) of
    false -> [];
    true ->
      ?LET({Command,NewUsedMachines},
	   gen_mach_cmd(State,UsedMachines),
	   case limit_states(State,CardTestState) of
	     true -> [Command];
	     false ->
	       ?LET(NextCommands,
		    command1(State,CardTestState,NPars,NewUsedMachines),
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

postcondition(_,_,_) ->
  true.

prepare(Commands) ->
  lists:map(fun ({_,Command}) -> Command end,Commands).

next_state(State,_Result,[Commands]) ->
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

limit_states(State,CardTestState) ->
  case proplists:get_value(limit_card_state,State#fstate.options,?MAX_STATES) of
    false ->
      false;
    N when is_integer(N), N>0 ->
      CardTestState >= N
  end.

    

-module(shr_gnr_fsms).

-behaviour(shr_gnr_implementation).

-export([print_finished_job_info/2, print_started_job_info/2,
         print_state/1]).

-export([initial_state/2,precondition/3,command/2,next_state/4]).

-include_lib("eqc/include/eqc.hrl").
-include("tester.hrl").

%%-define(debug,true).
-include("debug.hrl").

-include("fsmstate.hrl").

-define(MAX_CONCURRENT,3).
-define(MAX_STATES,400).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_finished_job_info(Job,TS) ->
  MachId = proplists:get_value(machine_id,Job#job.info),
  {_,{Machine,MachineState,_}} = lists:keyfind(MachId,1,TS#fstate.machines),
  try Machine:print_finished_job_info(Job#job.call,MachId,MachineState,TS#fstate.global_state)
  catch _:_ -> io_lib:format("~p",[Job#job.call]) end.

print_started_job_info(Job,TS) ->
  MachId = proplists:get_value(machine_id,Job#job.info),
  {_,{Machine,MachineState,_}} = lists:keyfind(MachId,1,TS#fstate.machines),
  try Machine:print_started_job_info(Job#job.call,MachId,MachineState,TS#fstate.global_state)
  catch _:_ -> io_lib:format("~p",[Job#job.call]) end.
      
print_state(State=#fstate{blocked=Blocked,machines=Machines,options=Options}) ->
  case proplists:get_value(fsm_printer,Options) of
    Printer when is_function(Printer) ->
      Printer(State);
    _ ->
      combine
	(Machines,
	 "||",
	 fun ({MachineId,{Machine,MachineState,_}}) ->
	     IsBlocked =
	       lists:member(MachineId,Blocked),
	     String =
	       try Machine:print_state(MachineId,MachineState,IsBlocked)
	       catch _:_ -> io_lib:format("~p",[MachineState]) end,
	     IsBlockedString =
	       if
		 IsBlocked -> "*";
		 true -> ""
	       end,
	     String ++ IsBlockedString
	 end)
  end.

combine([],_,_) ->
  "";
combine([Element|Rest],Combinator,F) ->
  ElementString = F(Element),
  RestString = combine(Rest,Combinator,F),
  if
    ElementString=/="", RestString=/="" ->
      ElementString ++ Combinator ++ RestString;
    true ->
      ElementString ++ RestString
  end.

initial_state(PreMachineSpecs,PreOptions) ->
  Options =
    case proplists:get_value(no_par,PreOptions,undefined) of
      true ->
	[{max_par,1}|PreOptions];
      false ->
	[{max_par,?MAX_CONCURRENT}|PreOptions];
      _ ->
	PreOptions
    end,
  StartFun =
    proplists:get_value(start_fun,Options,void),
  StopFun =
    proplists:get_value(stop_fun,Options,void),
  GlobalState =
    proplists:get_value(global_state,Options,void),
  MachineSpecs =
    lists:foldl
      (fun ({N,MachineWithMachineInit},Acc) when is_integer(N) ->
	   lists:map
	     (fun (N) -> {seq,N,MachineWithMachineInit} end, lists:seq(1,N))
	     ++Acc;
	   (MachineWithInit,Acc) ->
	   [{machine,MachineWithInit}|Acc]
       end, [], PreMachineSpecs),
  #fstate
    {
      machines=
       lists:map
	 (fun ({I,PreMachineSpec}) ->
	      {PreArgs,MachineSpec} =
		case PreMachineSpec of
		  {seq,N,Spec} -> {[N],Spec};
		  {machine,Spec} -> {[void],Spec}
		end,
	      {Machine,Init} =
		case MachineSpec of
		  Mach when is_atom(Mach) -> {Mach,[]};
		  {Mach,_} when is_atom(Mach) -> MachineSpec
		end,
	      init_machine(I,Machine,PreArgs++[Init,Options])
	  end, 
	  lists:zip(lists:seq(1,length(MachineSpecs)),MachineSpecs)),
      options=Options,
      start=StartFun,
      stop=StopFun,
      blocked=[],
      global_state=GlobalState
    }.

init_machine(I,Machine,Args) ->
  case apply(Machine,initial_state,Args) of
    {ok,InitialState} ->
      {I,{Machine,InitialState,[]}};
    {ok,InitialState,MachineOptions} ->
      {I,{Machine,InitialState,MachineOptions}}
  end.

precondition(#fstate{blocked=Blocked,machines=Machines,global_state=GlobalState},Commands,_) ->
  lists:all
    (fun ({Type,{F,Args},Info}) ->
	 Call = {Type,F,Args},
	 MachineId = proplists:get_value(machine_id,Info),
	 {_,{Machine,MachineState,_Opts}} = lists:keyfind(MachineId,1,Machines),
	    Machine:precondition
	      (MachineId,MachineState,GlobalState,Call)
	   andalso not(lists:member(MachineId,Blocked))
     end, Commands).

command(State,CorrState) ->
  Result = command1(State,1,CorrState),
  ?LOG
    ("Command generated: ~p~n",
     [Result]),
  Result.
command1(State,NPars,CorrState) ->
  ?LOG
     ("fsms:command - blocked=~p~nmachines=~p permit_par(_,~p)=~p~n",
      [State#fstate.blocked,
       State#fstate.machines,
       NPars,
       permit_par(State,NPars)]),
  case length(State#fstate.blocked)<length(State#fstate.machines) 
    andalso permit_par(State,NPars) of
    false -> 
      ?LOG
	 ("fsms:command1 - no command can be generated;~n"
	  ++"length(blocked)=~p length(machines)=~p permit_par()=~p~n",
	  [length(State#fstate.blocked),
	   length(State#fstate.machines), 
	   permit_par(State,NPars)]),
      [];
    true ->
      ?LET({Command,NewState},
	   gen_mach_cmd(State),
	   case Command of
	     void -> [];
	     _ ->
	       case limit_states(State,CorrState) of
		 true ->
		   ?LOG("limiting states..~n",[]),
		   [Command];
		 false ->
		   ?LET(NextCommands,
			command1(NewState,NPars+1,CorrState),
			[Command|NextCommands])
	       end
	   end)
  end.

gen_mach_cmd(State) ->
  NonBlocked =
    lists:filter
      (fun ({I,_}) -> not(lists:member(I,State#fstate.blocked)) end,
       State#fstate.machines),
  ?LOG
    ("fsms:gen_mach_cmd - length(NonBlocked) = ~p~n",
     [length(NonBlocked)]),
  case NonBlocked of
    [] ->
      {void,State};
    _ ->
      ?LET({MachId,{Machine,MachineState,_}},
	   oneof(NonBlocked),
	   begin
	     ?LOG
		("fsms:gen_mach_cmd - selected one machine ~p~nmach: ~p~n"
		 ++"machinestate=~p~n",
		 [MachId,Machine,MachineState]),
	     NewBlocked = [MachId|State#fstate.blocked],
	     NewState = State#fstate{blocked=NewBlocked},
	     ?LET(Generated,
		  Machine:command(MachId,MachineState,State#fstate.global_state),
		  case Generated of
		    stopped ->
		      gen_mach_cmd(NewState);
		    {Type,F,Args} ->
		      {{Type,{F,Args},[{machine_id,MachId}]},NewState}
		  end)
	   end)
  end.

permit_par(State,NPars) ->
  case proplists:get_value(max_par,State#fstate.options,?MAX_CONCURRENT) of
    false ->
      false;
    MaxPar when is_integer(MaxPar), MaxPar>=0 ->
      MaxPar >= NPars
  end.

limit_states(State,CorrState) ->
  case proplists:get_value(limit_card_state,State#fstate.options) of
    undefined ->
      false;
    Fun when is_function(Fun) ->
      Fun(CorrState)
  end.

jobs_to_machines(Jobs) ->
  lists:map
    (fun (Job) ->
	 Machine = proplists:get_value(machine_id,Job#job.info),
	 Machine
     end, Jobs).

next_state(State,Result,_Commands,_CorrState) ->
  {NewJobs,FinishedJobs} =
    Result,
  RemainingNewMachines = 
    jobs_to_machines(NewJobs) -- jobs_to_machines(FinishedJobs),
  NewBlocked =
      (State#fstate.blocked -- jobs_to_machines(FinishedJobs))
  ++ RemainingNewMachines,
  NewState =
    lists:foldl
      (fun (Job,S) ->
	   MachineId = 
	     proplists:get_value(machine_id,Job#job.info),
	   {_,{Machine,MachineState,MachineOpts}} =
	     lists:keyfind(MachineId,1,S#fstate.machines),
	   {NewMachineState,NewGlobalState} =
	     Machine:next_state
	       (MachineId,
		MachineState,
		State#fstate.global_state,
		Job#job.call),
	   S#fstate
	     {machines=
		lists:keyreplace
		  (MachineId,1,S#fstate.machines,
		   {MachineId,{Machine,NewMachineState,MachineOpts}}),
	      global_state=NewGlobalState}
       end, State, FinishedJobs),
  NewState#fstate{blocked=NewBlocked}.


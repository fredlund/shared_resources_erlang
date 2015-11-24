-module(fsms).

-include_lib("eqc/include/eqc.hrl").
-include("tester.hrl").

-export([initial_state/0,start/2,started/2,init/2,precondition/3,
	 command/2,strip_call_info/1,next_state/4]).
-export([print_finished_job_info/2,print_started_job_info/2,print_state/1]).

%%-define(debug,true).
-include("../../src/debug.hrl").

-define(MAX_CONCURRENT,3).
-define(MAX_STATES,400).

-include("fstate.hrl").

initial_state() ->
  #fstate{}.

init(PreMachineSpec,PreOptions) ->
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
  StartedFun =
    proplists:get_value(started_fun,Options,void),
  GlobalState =
    proplists:get_value(global_state,Options,void),
  MachineSpec =
    lists:foldl
      (fun ({N,MachineWithMachineInit},Acc) when is_integer(N) ->
	   lists:duplicate(N,MachineWithMachineInit)++Acc;
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

start(State,Options) ->
  Start = State#fstate.start,
  NewState =
    if
      is_function(Start) -> Start(State,Options);
      true -> State
    end,
  case proplists:get_value(implementation,Options) of
    {Module,Args} when is_atom(Module) ->
      Module:start(Args,State#fstate.options);
    _ ->
      ok
  end,
  NewState.

started(State,Result) ->
  Started = State#fstate.started,
  if
    is_function(Started) ->
      Started(State,Result);
    true ->
      State
  end.		     

precondition(_,#fstate{blocked=Blocked,machines=Machines,global_state=GlobalState},Commands) ->
  lists:all
    (fun ({I,Command}) ->
	 {_,{Machine,MachineState}} = lists:keyfind(I,1,Machines),
	 Machine:precondition(I,MachineState,GlobalState,Command)
	   andalso not(lists:member(I,Blocked))
     end, Commands).

command(State,TesterState) ->
  command1(State,TesterState).
command1(State,TesterState) ->
  command1(State,TesterState,1).
command1(State,TesterState,NPars) ->
  ?LOG
     ("fsms:command - blocked=~p~nmachines=~p~n",
      [State#fstate.blocked,
       State#fstate.machines]),
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
	       case limit_states(State,TesterState) of
		 true ->
		   [Command];
		 false ->
		   ?LET(NextCommands,
			command1(NewState,TesterState,NPars+1),
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
      ?LET({MachId,{Machine,MachineState}},
	   oneof(NonBlocked),
	   begin
	     ?LOG
		("fsms:gen_mach_cmd - selected one machine ~p~nmach: ~p~n"
		 ++"machinestate=~p~n",
		 [MachId,Machine,MachineState]),
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

%% careful with void!
strip_call_info({_,Call}) -> 
  Call.

jobs_to_machines(Jobs) ->
  lists:map
    (fun (Job) ->
	 {Machine,_} = Job#job.callinfo,
	 Machine
     end, Jobs).

next_state(State,_TesterState,Result,[_Commands]) ->
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
	   {I,_Command} = Job#job.callinfo,
	   {_,{Machine,MachineState}} =
	     lists:keyfind(I,1,S#fstate.machines),
	   {NewMachineState,NewGlobalState} =
	     Machine:next_state
	       (I,MachineState,State#fstate.global_state,Job),
	   S#fstate
	     {machines=
		lists:keyreplace
		  (I,1,S#fstate.machines,{I,{Machine,NewMachineState}}),
	      global_state=NewGlobalState}
       end, State, FinishedJobs),
  NewState#fstate{blocked=NewBlocked}.

permit_par(State,NPars) ->
  case proplists:get_value(max_par,State#fstate.options,?MAX_CONCURRENT) of
    false ->
      false;
    MaxPar when is_integer(MaxPar), MaxPar>=0 ->
      MaxPar >= NPars
  end.

limit_states(State,TesterState) ->
  case proplists:get_value(limit_card_state,State#fstate.options,?MAX_STATES) of
    false ->
      false;
    N when is_integer(N), N>0 ->
      TesterState >= N
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_finished_job_info(Job=#job{callinfo={MachId,_}},TS) ->
  {_,{Machine,MachineState}} = lists:keyfind(MachId,1,TS#fstate.machines),
  try Machine:print_finished_job_info(Job,MachId,MachineState,TS#fstate.global_state)
  catch _:_ -> io_lib:format("~p",[Job#job.call]) end.

print_started_job_info(Job=#job{callinfo={MachId,_}},TS) ->
  {_,{Machine,MachineState}} = lists:keyfind(MachId,1,TS#fstate.machines),
  try Machine:print_started_job_info(Job,MachId,MachineState,TS#fstate.global_state)
  catch _:_ -> io_lib:format("~p",[Job#job.call]) end.
      
print_state(State=#fstate{blocked=Blocked,machines=Machines,options=Options}) ->
  case proplists:get_value(fsm_printer,Options) of
    Printer when is_function(Printer) ->
      Printer(State);
    _ ->
      combine
	(Machines,
	 "||",
	 fun ({MachineId,{Machine,MachineState}}) ->
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

	  

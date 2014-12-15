-module(tester).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-record(onestate,{incoming,waiting,sdata,swait}).
-record(state,{started,states,dataSpec,waitSpec,glue}).
-record(job,{pid,call}).

api_spec() ->
  #api_spec{}.

callouts(_,_) ->
  ?EMPTY.

init_state(DataSpec,WaitSpec,Glue) ->
  #state
    {
     started=false,
     states=[#onestate{jobs=[],blocked=[],data=DataSpec:init(),wait=WaitSpec:init()}],
     dataSpec=DataSpec,
     waitSpec=WaitSpec,
     glue=Glue
    }.

command(State) ->
  Alternatives =
    [{call,?MODULE,start,[]} ||
      not(started(State))] 
    ++
    Glue:alternatives(State),
  if
    Alternatives==[] ->
      io:format("No alternatives in state~n~p~n",[State]);
    true ->
      ok
  end,
  eqc_gen:oneof(Alternatives).

start() ->
  [{cp,CP}] =
    ets:lookup(?MODULE,cp),
  {ok,NodeId} =
    java:start_node
      ([{java_verbose,"SEVERE"},
	{call_timeout,infinity},
	{java_exception_as_value,true},
	{add_to_java_classpath,CP}]),
  ok.

do_cmds(PreCommands) ->
  Commands =
    lists:filter
      (fun (Command) -> calltype_in_call(Command)=/=void end,
       PreCommands),
  ParentPid =
    self(),
  NewJobs =
    lists:map
      (fun (Call={M,F,Args}) ->
           #job{pid=
                  spawn
                    (fun () ->
                         Result=apply(M,F,Args),
                         ParentPid!{#job{pid=self(),call=Call},Result}
                     end),
                call=Call}
       end, Commands),
  FinishedJobs = wait_for_jobs(),
  {NewJobs,FinishedJobs}.

wait_for_jobs() ->
  timer:sleep(?COMPLETION_TIME),
  receive_completions().

receive_completions() ->
  receive
    X={Job,_Result} when is_record(Job,job) ->
      [X|receive_completions()];
    X ->
      io:format("unknown message ~p received~n",[X]),
      throw(bad)
  after 0 -> []
  end.

void() ->
  ok.

precondition(State,Call) ->     
  lists:all
    (fun (IndState) -> precondition_a(IndState,State,Call) end,
     State#state.states).

precondition_a(IndState,State,Call) ->
  case Call of
    {_,_,start,_,_} ->
      not(started(State));
    {_,_,void,_,_} ->
      true;
    {_,_,do_job,Job,_} ->
      started(State)
	andalso (State#state.waitSpec):precondition(Job,State#state
  end.

next_state(State,Result,Call) ->
  case Call of
    {_,_,start,_,_} ->
      State#state{started=true};
    {_,_,void,_,_} ->
      State;
    {_,_,do_cmds,[PreCommands],_} ->
      {NewJobs,FinishedJobs} = 
	Result,
      RemainingJobs =
	lists:filter
	  (fun (Job) ->
	       not(lists:any(fun ({Job1,_}) -> Job==Job1 end, FinishedJobs))
	   end, lists:sort(NewJobs++jobs(State))),
      viable_orderings
	(State,NewJobs,FinishedJobs,set_jobs(RemainingJobs,State2))
  end.

postcondition(State,Call,Result) ->
  case Call of
    {_,_,do_job,_JobCall,_} ->
      {NewJobs,FinishedJobs} = 
	Result,
      case lists:any(fun job_finished_with_exception/1,FinishedJobs) of
	true ->
	  java_exception;
	false ->
	  RemainingJobs =
	    lists:filter
	      (fun (Job) ->
		   not(lists:any(fun ({Job1,_}) -> Job==Job1 end, FinishedJobs))
	       end, lists:sort(NewJobs++jobs(State))),
	  viable_orderings
	    (NewJobs,FinishedJobs,set_jobs(RemainingJobs,State));
    _ -> true
  end.

job_finished_with_exception({_,Result}) ->
  case Result of
    {java_exception,_Exc} -> true;
    _ -> false
  end.

job_precondition_is_true(Job,State) ->
  case Job of
    #job{call={_,exit,[R,N,P]}} ->
      if
	N==(?NUM_NAVES-1) ->
	  lists:member({R,P},warehouse(N,State));
	true ->
	  lists:member({R,P},warehouse(N,State))
	    andalso ([] == corridor(N+1,State))
      end;
    #job{call={_,enter,[R,N,P]}} ->
      PesoNave = peso_in_warehouse(N,State),
      if
	N==0 ->
	  (P+PesoNave =< ?MAX_PESO_EN_NAVE);
	true ->
	  case corridor(N,State) of
	    [{R1,P1}] ->
	      (R1==R) andalso (P>=P1) andalso (P+PesoNave =< ?MAX_PESO_EN_NAVE);
	    _ ->
	      false
	  end
      end
  end.

job_next_state(Job,State) ->
  case Job of
    #job{call={_,exit,[R,N,P]}} ->
      State1 = delete_from_warehouse({R,P},N,State),
      if
	N==(?NUM_NAVES-1) ->
	  State1;
	true ->
	  add_to_corridor({R,P},N+1,State1)
      end;
    #job{call={_,enter,[R,N,P]}} ->
      State1 = add_to_warehouse({R,P},N,State),
      if
	N==0 ->
	  State1;
	true ->
	  delete_from_corridor({R,P},N,State1)
      end
  end.

peso_in_warehouse(N,State) ->
  lists:foldl(fun ({_,P},Peso) -> P+Peso end, 0, warehouse(N,State)).

viable_orderings(State,FirstJobs,OtherFinishedJobs,RemainingJobs) ->
  first_steps(State,FirstJobs,OtherFinishedJobs,RemainingJobs).

first_steps(State,FirstJobs,OtherFinishedJobs,RemainingJobs) ->
  {NewStates,FirstJobs} ->
  lists:foldl
    (fun (Jobs,Acc) ->
	 [Job|RestJobs] = Jobs,
	 case job_precondition_is_true(Job,State) of
	   true ->
	     State1 = remove_from_blocked(robot(Job),State),
	     [{RestJobs,job_next_state(Job,State1)}|Acc];
	   false ->
	     Acc
	 end
     end, [], JobAlternatives).
  
  
  viable_orderings([{lists:map(fun ({Job,_}) -> Job end, Jobs),State}]).
    
viable_orderings(JobsState) ->
  case JobsState of
    [] -> [];
    [{_Jobs=[],State}] -> State;
    Result=[{_Jobs=[],_}|_] -> 
      io:format
	("*** Warning: result~n~p~nis not deterministic~n",[Result]),
      throw(bad);
    _ ->
      Viables=
	lists:flatmap
	  (fun ({Jobs,State}) ->
	       run_jobs(alternatives(Jobs),State)
	   end,
	   JobsState),
      Merged = merge_jobs_and_states(Viables),
      viable_orderings(Merged)
  end.

merge_jobs_and_states(JobsAndStates) ->
  lists:usort(JobsAndStates).

run_jobs(JobAlternatives,State) ->
  lists:foldl
    (fun (Jobs,Acc) ->
	 [Job|RestJobs] = Jobs,
	 case job_precondition_is_true(Job,State) of
	   true ->
	     State1 = remove_from_blocked(robot(Job),State),
	     [{RestJobs,job_next_state(Job,State1)}|Acc];
	   false ->
	     Acc
	 end
     end, [], JobAlternatives).

alternatives(Jobs) ->
  [ [FirstJob|lists:delete(FirstJob,Jobs)] ||
    FirstJob <- Jobs ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_table(CP,Id) ->
  case ets:info(?MODULE) of
    undefined ->
      ok;
    _ ->
      [{pid,Pid}] = ets:lookup(?MODULE,pid),
      erlang:exit(Pid,kill),
      ets:delete(?MODULE)
  end,
  spawn
    (fun () ->
	 ets:new(?MODULE,[named_table,public]),
	 ets:insert(?MODULE,{pid,self()}),
	 wait_forever()
     end),
  wait_until_stable(),
  ets:insert(?MODULE,{cp,CP}),
  ets:insert(?MODULE,{id,Id}).

wait_until_stable() ->
  case ets:info(?MODULE) of
    L when is_list(L) ->
      ok;
    _ ->
      wait_until_stable()
  end.

wait_forever() ->
  receive _ -> wait_forever() end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(CP,Id,DataSpec,WaitSpec,Glue) ->
  init_table(CP,Id),
  check_prop(DataSpec,WaitSpec,Glue).

check_prop(DataSpec,WaitSpec,Glue) ->
  case eqc:quickcheck(eqc:on_output(fun eqc_printer/2,prop_ok(DataSpec,WaitSpec,Glue))) of
    false ->
      io:format("~n~n***FAILED~n");
    true ->
      io:format("~n~nPASSED~n",[])
  end.

prop_ok(DataSpec,WaitSpec,Glue) ->
  ?FORALL
     (Cmds, eqc_dynamic_cluster:dynamic_commands(?MODULE,init_state(DataSpec,WaitSpec,Glue)),
      ?CHECK_COMMANDS
	 ({H, DS, Res},
	  ?MODULE,
	  Cmds,
	  begin
	    case ets:lookup(?MODULE,controller) of
	      [{controller,Controller}] ->
		spawn
		  (fun () ->
		       timer:sleep(5000),
		       try java:terminate(java:node_id(Controller))
		       catch _:_ -> ok end
		   end);
	      _ ->
		io:format("*** Warning: controller missing?~n"),
		ok
	    end,
	    if
	      Res == ok ->
		true;
	      true ->
		print_counterexample(Cmds,H,DS,Res),
		false
	    end
	  end)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% To make eqc not print the horrible counterexample
eqc_printer(Format,String) ->
  case Format of
    "~p~n" -> ok;
    _ -> io:format(Format,String)
  end.

print_counterexample(Cmds,H,_DS,Reason) ->
  io:format("~n~nTest failed with reason ~p~n",[Reason]),
  {FailingCommandSequence,_} = lists:split(length(H)+1,Cmds),
  ReturnValues = 
    case Reason of
      {exception,_} ->
	(lists:map(fun ({_,_,Result}) -> Result end, H))++[Reason];
      _ ->
	(lists:map(fun ({_,_,Result}) -> Result end, H))
    end,
  io:format("~nCommand sequence:~n"),
  io:format("-----------------~n~n"),
  print_commands(lists:zip(tl(FailingCommandSequence),ReturnValues)),
  io:format("~n~n").

print_commands([]) ->
  ok;
print_commands([{Call,Result}|Rest]) ->
  ResultString =
    case Call of
      {_,_,do_job,_JobCall,_} ->
	{Job,Unblocked,NotBlocked} =
	  Result,
	BlockedString =
	  if
	    NotBlocked -> "did not block";
	    true -> "blocked"
	  end,
	UnblockStr =
	  lists:foldl
	    (fun ({UnblockedJob,_Res},Acc) ->
		 io_lib:format("~sunblocks ~p,",[Acc,robot(UnblockedJob)])
	     end, " -- ",
	     lists:filter
	       (fun ({Job1,_}) -> Job=/=Job1 end, Unblocked)),
	ExceptionStr =
	  lists:foldl
	    (fun ({UnblockedJob,Res},Acc) ->
		 case Res of
		   {java_exception,Exc} ->
		     report_java_exception(Exc),
		     io_lib:format
		       ("~s ~p raised exception",
			[Acc,robot(UnblockedJob)]);
		   _ ->
		     Acc
		 end
	     end, "", Unblocked),
	BlockedString++UnblockStr++ExceptionStr;
      _ -> ""
    end,
  CallString =
    case Call of
      {_,_,do_job,[_,Name,Args],_} ->
	io_lib:format("~p ~p",[Name,Args]);
      {_,_,Name,Args,_} ->
	io_lib:format("~p ~p",[Name,Args])
    end,
  io:format("  ~s ~s~n",[CallString,ResultString]),
  print_commands(Rest).

report_java_exception(Exception) ->
  io:format("*** Warning: unexpected Java exception~n"),
  Err = java:get_static(java:node_id(Exception),'java.lang.System',err),
  java:call(Exception,printStackTrace,[Err]).

robot(Call) ->
  case Call#job.call of
    {_,exit,[R,_,_]} -> R;
    {_,enter,[R,_,_]} -> R
  end.

robot_in_call(Call) ->
  case Call of
    [_,exit,[R,_,_]] -> R;
    [_,enter,[R,_,_]] -> R
  end.

type_in_call(Call) ->
  case Call of
    [_,Type,_] -> Type
  end.

warehouse_in_call(Call) ->
  case Call of
    [_,exit,[_,N,_]] -> N;
    [_,enter,[_,N,_]] -> N
  end.
  
started(State) ->
  State#state.started.

corridor(N,State) ->
  element(N+1,State#state.corridor).

warehouse(N,State) ->
  element(N+1,State#state.warehouses).

blocked(State) ->
  State#state.blocked.

delete_from_warehouse(Element,N,State) ->
  State#state
    {warehouses=
       setelement
	 (N+1,
	  State#state.warehouses,
	  lists:delete(Element,warehouse(N,State)))}.

add_to_corridor(Element,N,State) ->
  State#state
    {corridor=setelement(N+1,State#state.corridor,[Element])}.
  
add_to_warehouse(Element,N,State) ->
  State#state
    {warehouses=
       setelement
	 (N+1,
	  State#state.warehouses,
	  lists:sort([Element|warehouse(N,State)]))}.

delete_from_corridor(_Element,N,State) ->
  State#state
    {corridor=setelement(N+1,State#state.corridor,[])}.

add_to_blocked(Element,State) ->  
  State#state
    {blocked=lists:sort([Element|State#state.blocked])}.

jobs(State) ->
  State#state.jobs.

set_jobs(Jobs,State) ->
  State#state{jobs=lists:sort(Jobs)}.

remove_from_blocked(Element,State) ->
  State#state
    {blocked=lists:delete(Element,State#state.blocked)}.
  
inc_enters(State) ->  
  State#state
    {num_enters=State#state.num_enters+1}.

num_enters(State) ->
  State#state.num_enters.

set_enters(N,State) ->  
  State#state{num_enters=N}.


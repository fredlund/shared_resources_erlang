-module(tester).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-define(COMPLETION_TIME,100).

-record(onestate,
	{
	  incoming,   %% Incoming jobs, not yet accepted
	  waiting,    %% Accepted jobs, waiting to be executed
	  sdata,      %% Data state
	  swait       %% Priority state
	}).
-record(state,
	{
	  started,    %% Started testing?
	  states,     %% Set of possible (onestate) states
	  dataSpec,   %% Module implementing the data specification
	  waitSpec,   %% Module implementing the priority specification
	  testingSpec %% Module implementing the testing specification
	}).
-record(job,
	{
	  pid,        
	  call        
	}).

api_spec() ->
  #api_spec{}.

callouts(_,_) ->
  ?EMPTY.

init_state(DataSpec,WaitSpec,TestingSpec) ->
  #state
    {
     started=false,
     states=
       [
	#onestate
	{
	  incoming=[],
	  waiting=[],
	  data=DataSpec:init(),
	  wait=WaitSpec:init()}
       ],
     dataSpec=DataSpec,
     waitSpec=WaitSpec,
     testingSpec=TestingSpec
    }.

command(State) ->
  Alternatives =
    [{call,?MODULE,start,[]} ||
      (not(started))]
    ++
    TestingSpec:alternatives(State),
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
  NodeId.

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
    (fun (IndState) -> precondition_ind(IndState,State,Call) end,
     State#state.states).

precondition_ind(IndState,State,Call) ->
  case Call of
    {_,_,start,_,_} ->
      started(State)=/=true;
    {_,_,void,_,_} ->
      true;
    {_,_,do_job,Job,_} ->
      started(State)
	andalso (State#state.testSpec):precondition(Job,IndState#onestate.sdate)
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
      viable_orderings(add_new_jobs(NewJobs,State),FinishedJobs)
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
	  viable_orderings(add_new_jobs(NewJobs,State),FinishedJobs)
      end;
    _ -> true
  end.

job_finished_with_exception({_,Result}) ->
  case Result of
    {java_exception,_Exc} -> true;
    _ -> false
  end.

viable_orderings(State,FinishedJobs) ->
  if
    FinishedJobs==[] ->
      State;
    true ->
      FirstStatesAndJobs = accept_one_incoming(State,FinishedJobs),
      FinalStates = finish_jobs(State,FirstStateAndJobs,[]),
      check_remaining_jobs(State,FinalStates),
      State#state{states=FinalStates}
  end.

accept_one_incoming(State,FinishedJobs) ->
  NewStates =
    lists:flatmap
      (fun (IndState) ->
	   lists:map
	     (fun (IncomingJob) ->
		  case job_precondition_is_true(Job,IndState,State) of
		    true -> [job_new_waiting(Job,IndState,State)];
		    false -> []
		  end
	      end, 
	      IndState#onestate.incoming)
       end, State#state.states),
  merge_jobs_and_states
    (lists:map(fun (NewState) -> {NewState,FinishedJobs} end, NewStates)).
    
finish_jobs(State,[],FinishedJobs) ->
  lists:usort(lists:map(fun ({S,_}) -> S end, Finished));
finish_jobs(State,StatesAndJobs,FinishedJobs) ->
  NewStatesAndJobs =
    lists:flatmap
      (fun ({IndState,FJobs}) ->
	   NewAcceptStates =
	     lists:map
	       (fun (Job) ->
		    case job_pre_is_true(Job,IndState,State) of
		      true ->
			[{job_new_waiting(Job,IndState,State),FJobs}];
		      false ->
			[{job_no_longer_waiting(Job,IndState,State),FJobs}]
		    end
		end, 
		IndState#onestate.incoming),
	   NewFinishStates =
	     lists:map
	       (fun (Job) ->
		    case job_cpre_is_true(Job,IndState,State)
		      andalso job_priority_enabled_is_true(Job,IndState,State) of
		      true ->
			[{job_next_state(Job,IndState,State),
			  FJobs--[Job]}];
		      false -> []
		    end
		end,
		IndState#onestate.incoming),
	   NewAcceptStates++NewFinishStates
       end, ActiveStatesAndJobs),
  case NewStatesAndJobs of
    [] ->
      io:format
	("*** Error: remaining jobs cannot be executed by the model~n",
	 []),
      throw(bad);
    _ -> 
      {ActiveStatesAndJobs,Finished} = 
	lists:foldl
	  (fun (SJ={S,Jobs},{A,F}) ->
	       if
		 (Jobs=/=[]) orelse (S#indstate.waiting=/=[])->
		   {[SJ|A],F};
		 true ->
		   {A,[S|F]}
	       end
	   end,
	   {[],[]},
	   StatesAndJobs),
      finish_jobs
	(State,
	 merge_jobs_and_states(ActiveStatesAndJobs),
	 lists:usort(Finished++FinishedJobs))
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

job_cpre_is_true(Job,IndState,State) ->
  (State#state.dataSpec):cpre(IncomingJob#job.call,IndState#onestate.sdata).

job_pre_is_true(Job,IndState,State) ->
  (State#state.dataSpec):pre(IncomingJob#job.call,IndState#onestate.sdata).

job_priority_enabled_is_true(Job,IndState,State) ->
  (State#state.dataSpec):pre(IncomingJob#job.call,IndState#onestate.wdata,IndState#onestate.sdata).

job_new_waiting(Job,IndState,State) ->
  NewWaitState = 
    (State#state.waitSpec):new_waiting
      (Job#job.call,
       Incoming#onestate.wdata,
       Incoming#onestate.sdata),
  IndState#onestate
    {
    wdata=NewWaitState,
    incoming=State#state.incoming--[Job],
    waiting=lists:usort([Job|IndState#onestate.waiting])
   }.

job_no_longer_waiting(Job,IndState,State) ->
  IndState#onestate
    {
    incoming=Incoming--[Job]
   }.

job_new_waiting(Job,IndState,State) ->
  NewDataState =
    (State#state.dataSpec):post
      (Job#job.call,
       Incoming#onestate.sdata),
  NewWaitState = 
    (State#state.waitSpec):new_waiting
      (Job#job.call,
       Incoming#onestate.wdata,
       NewDataState),
  IndState#onestate
    {
    wdata=NewWaitState,
    sdata=NewDataState,
    incoming=State#state.incoming--[Job]
   }.

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

test(CP,Id,DataSpec,WaitSpec,TestingSpec) ->
  init_table(CP,Id),
  check_prop(DataSpec,WaitSpec,TestingSpec).

check_prop(DataSpec,WaitSpec,TestingSpec) ->
  case eqc:quickcheck(eqc:on_output(fun eqc_printer/2,prop_ok(DataSpec,WaitSpec,TestingSpec))) of
    false ->
      io:format("~n~n***FAILED~n");
    true ->
      io:format("~n~nPASSED~n",[])
  end.

prop_ok(DataSpec,WaitSpec,TestingSpec) ->
  ?FORALL
     (Cmds, eqc_dynamic_cluster:dynamic_commands(?MODULE,init_state(DataSpec,WaitSpec,TestingSpec)),
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

blocked(State) ->
  State#state.blocked.

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
  

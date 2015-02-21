-module(tester).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

%%-define(debug,true).
-include("../../src/debug.hrl").

-define(COMPLETION_TIME,50).

-include("tester.hrl").

api_spec() ->
  #api_spec{}.

initial_state() ->
  ?LOG("~p:initial_state~n",[?MODULE]),
  #state{}.

init_state(Options,{DataSpec,DI},{WaitSpec,WI},{TestingSpec,TI}) ->
  ?LOG("~p:init_state~n",[?MODULE]),
  #state
    {
     started=false,
     states=
       [
	#onestate
	{
	  incoming=[],
	  waiting=[],
	  sdata=DataSpec:init(DI,Options),
	  swait=WaitSpec:init(WI,Options)
	}
       ],
     options=Options,
     test_state=TestingSpec:init(TI,Options),
     dataSpec=DataSpec,
     waitSpec=WaitSpec,
     testingSpec=TestingSpec
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_pre(State) ->
  not(State#state.started).

start_args(State) ->
  [State#state.options,State#state.testingSpec,State#state.test_state].

start(Options,TestSpec,TestState) ->
  Id =
    proplists:get_value(id,Options,unknown),
  NodeId =
    case proplists:get_value(needs_java,Options,true) of
      true ->
	CP = proplists:get_value(cp,Options,[]),
	ets:insert(?MODULE,{cp,CP}),
	?LOG("CP is ~p~n",[CP]),
	try
	  java:start_node
	    ([{java_verbose,"SEVERE"},
	      {call_timeout,infinity},
	      {java_exception_as_value,true},
	      {add_to_java_classpath,CP}]) of
	  {ok,NId} ->
	    store_data(node,NId),
	    NId
	catch _:_ ->
	    io:format
	      ("~n*** Error: cannot start java. Is the javaerlang library installed?~n"),
	    throw(bad)
	end;
      false -> void
    end,
  ets:insert(?MODULE,{id,Id}),
  TestSpec:start(NodeId,TestState).

start_post(_State,_,_Result) ->
  true.

start_next(State,Result,_) ->
  TestState = State#state.test_state,
  StartedTestState = (State#state.testingSpec):started(TestState,Result),
  State#state{started=true,test_state=StartedTestState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_cmds_pre(State) ->
  State#state.started.

do_cmds_args(State) ->
  Commands = (State#state.testingSpec):command(State#state.test_state,State),
  [Commands,State#state.testingSpec].

do_cmds_pre(State,[Commands,_]) ->
  (State#state.testingSpec):precondition(State,State#state.test_state,filter_commands(Commands)).

do_cmds(Commands,TestingSpec) ->
  ParentPid =
    self(),
  NewJobs =
    lists:map
      (fun (CallInfo) ->
	   {M,F,Args} = Call = strip_call_info(TestingSpec,CallInfo),
           #job{pid=
                  spawn
                    (fun () ->
                         Result = apply(M,F,Args),
                         ParentPid!#job{pid=self(),call=Call,callinfo=CallInfo,result=Result}
                     end),
		callinfo=CallInfo,
                call=Call}
       end, Commands),
  ?LOG("New jobs are ~p~n",[NewJobs]),
  if
    NewJobs==[] ->
      {[],[]};
    true ->
      FinishedJobs = wait_for_jobs(),
      {non_void_jobs(NewJobs),non_void_jobs(FinishedJobs)}
  end.

do_cmds_post(State,Args,Result) ->
  {NewJobs,FinishedJobs} = Result,
  if
    NewJobs==[] ->
      if
	FinishedJobs==[] -> 
	  true;
	true -> 
	  io:format
	    ("~n*** Error: there are calls~n~s~n"++
	       "that have been completed by the implementation "++
	       "even though no new calls were made~n",
	     [FinishedJobs]),
	  print_test_state(State),
	  maybe_print_model_state(State),
	  false
      end;
    true ->
  case proplists:get_value(verbose,State#state.options,false) of
    true ->
      io:format
	("~npostcondition: new jobs=~n~p~ncompleted jobs=~n~p~nstate=~n~p~n",
	 [NewJobs,FinishedJobs,State]);
    false ->
      ok
  end,
  try
    ?LOG
       ("Postcondition: result=~p~n",[Result]),
    case lists:any(fun job_finished_with_exception/1,FinishedJobs) of
      true ->
	java_exception;
      false ->
	case calculate_next_state(add_new_jobs(NewJobs,State),FinishedJobs,safety,State) of
	  false -> false;
	  _ -> case calculate_next_state(add_new_jobs(NewJobs,State),FinishedJobs,both,State) of
		 false -> false;
		 {ok,NewState} -> 
		   %% Finally check whether some non-finished job is finishable in all
		   %% possible model states
		   (not(proplists:get_value(enforce_progress,State#state.options,true)))
		     orelse 
		   check_remaining_jobs(State,NewState#state.states)
	       end
	end
    end of
    false ->
      io:format("postcondition false for ~p~n",[Args]),
      false;
    true ->
      true;
    Other ->
      io:format("strange postcondition ~p~n",[Other]),
      false
  catch _:Reason ->
      io:format("postcondition raises ~p~nStacktrace:~n~p~n",
		[Reason,
		 erlang:get_stacktrace()]),
      false
  end
  end.

do_cmds_next(State,Result,[Commands,_]) ->
  try
    {NewJobs,FinishedJobs} = Result,
    if
      NewJobs==[] ->
	State;
      true ->
	{ok,NewState} =
	  calculate_next_state(add_new_jobs(NewJobs,State),FinishedJobs,both,State),
	NewTestState =
	  (State#state.testingSpec):next_state
	    (NewState#state.test_state,
	     NewState,
	     Result,
	     [Commands]),
	NewState#state{test_state=NewTestState}
    end
  catch _:_ ->
      io:format("~n*** Warning: next raises exception~n"),
      io:format("~p~n",[erlang:get_stacktrace()]),
      State
  end.

strip_call_info(TestingSpec,CallInfo) ->
  try_execute(TestingSpec,strip_call_info,[CallInfo],CallInfo).

make_void_call() -> {?MODULE,void,[]}.

valid_jobs(Jobs,State) -> 
  [OneIndState|_] = State#state.states,
  lists:filter
    (fun (Job) ->
	   job_pre_is_true(Job,OneIndState,State) end,
     Jobs).

non_void_jobs(Jobs) ->
  lists:filter(fun (Job) -> not(is_void_job(Job)) end, Jobs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter_commands(Commands) ->
  lists:filter(fun (Command) -> not(is_void_call(Command)) end, Commands).

store_data(Key,Value) ->
  ets:insert(?MODULE,{Key,Value}).

get_data(Key) ->
  [{Key,Value}] = ets:lookup(?MODULE,Key),
  Value.

wait_for_jobs() ->
  timer:sleep(?COMPLETION_TIME),
  receive_completions().

receive_completions() ->
  receive
    Job when is_record(Job,job) ->
      [Job|receive_completions()];
    X ->
      io:format("unknown message ~p received~n",[X]),
      throw(bad)
  after 0 -> []
  end.

void() ->
  ok.

job_finished_with_exception(Job) ->
  case Job#job.result of
    {java_exception,_Exc} -> true;
    {'EXIT',_,_} -> true;
    _ -> false
  end.

%% Calculate the next model state (a set of possible states) given the
%% set of finished jobs. 
%%
calculate_next_state(State,FinishedJobs,WhatToCheck,OrigState) ->
  %% First always "accept" an incoming new job
  %% (since otherwise the execution would still be blocked)
  FirstStatesAndJobs =
    if
      WhatToCheck==both ->
	accept_one_incoming(State,FinishedJobs);
      true ->
	%% if we are not checking scheduling we can just move the incoming jobs to waiting
	lists:map
	  (fun (IndState) ->
	       {IndState#onestate
		{waiting=lists:usort(IndState#onestate.waiting++IndState#onestate.incoming),
		 incoming=[]},
		FinishedJobs}
	   end, State#state.states)
    end,
  ?LOG("WC=~p after first:~n~p~n",[WhatToCheck,FirstStatesAndJobs]),

  %% Now finish all remaining jobs, FirstStatesAndJobs is
  %% a list of pairs (State,RemainingJobs) where State is
  %% still a viable State, and RemainingJobs is the set of finished
  %% jobs remaining to execute; once no jobs remain the state moves to
  %% the third parameter.
  case finish_jobs(State,FirstStatesAndJobs,[],WhatToCheck,OrigState) of
    false -> false;
    {ok,FinishStates} -> {ok,State#state{states=FinishStates}}
  end.

accept_one_incoming(State,FinishedJobs) ->
  NewStates =
    lists:flatmap
      (fun (IndState) ->
	   lists:map
	     (fun (Job) -> job_new_waiting(Job,IndState,State) end,
	      IndState#onestate.incoming)
       end, State#state.states),
  merge_jobs_and_states
    (lists:map(fun (NewState) -> {NewState,FinishedJobs} end, NewStates)).
    
%% Terminate when no non-finished states remain
finish_jobs(_,[],FinishedStates,_WhatToCheck,_) ->
  ?LOG("WC=~p Finished=~p~n",[WhatToCheck,FinishedStates]),
  {ok,lists:usort(FinishedStates)};
finish_jobs(State,StatesAndJobs,FinishedStates,WhatToCheck,OrigState) ->
  ?LOG("WC=~p States:~n~p~nFinished=~p~n",[WhatToCheck,StatesAndJobs,FinishedStates]),
  {NewStatesAndJobs,NewFinishedStates} =
    %% Recurse over the list of possible states (and finished jobs in each state)
    lists:foldl
      (fun ({IndState,FJobs},{NSJ,NF}) ->
	   if
	     %% Nothing to do when no more jobs, and incoming is empty; 
	     %% move state to finished states
	     FJobs==[], IndState#onestate.incoming==[] ->
	       {NSJ,[IndState|NF]};
	     true ->
	       %% Accepting moves
	       NewAcceptStates =
		 if
		   WhatToCheck==both ->
		     lists:map
		       (fun (Job) -> {job_new_waiting(Job,IndState,State),FJobs} end,
			IndState#onestate.incoming);
		   true ->
		     []
		 end,
	       %% A call finishes
	       NewFinishStates =
		 lists:flatmap
		   (fun (Job) ->
			case find_job(Job,IndState#onestate.waiting) of
			  {ok,QueueJob} ->
			    case
			      %% We have to be careful here -- the job in the state
			      %% has the waiting info while the completed job has
			      %% the correct result
			      job_is_executable(QueueJob,IndState,State,WhatToCheck)
			      andalso job_returns_correct_value(Job,IndState,State) of
			      true ->
				[{job_next_state(QueueJob,IndState,State,WhatToCheck),
				  delete_job(Job,FJobs)}];
			      false ->
				[]
			    end;
			  false -> []
			end
		    end,
		    FJobs),
	       {NewAcceptStates++NewFinishStates++NSJ,NF}
	   end
       end, {[],FinishedStates}, StatesAndJobs),
  if
    NewStatesAndJobs==[], NewFinishedStates==[], WhatToCheck==safety ->
      io:format
	("~n*** Error: there are calls that have been completed by the implementation "++
	   "which cannot be completed by the model (without considering priority)~n"),
      print_test_state(OrigState),
      maybe_print_model_state(OrigState),
      false;

    NewStatesAndJobs==[], NewFinishedStates==[] -> 
      io:format
	("~n*** Error: there are calls that have been completed by the implementation "++
	   "which cannot be completed by the model (when considering priority)~n"),
      print_test_state(OrigState),
      maybe_print_model_state(OrigState),
      false;

    true ->
      finish_jobs
	(State,
	 merge_jobs_and_states(NewStatesAndJobs),
	 lists:usort(NewFinishedStates),
	 WhatToCheck,
	 OrigState)
  end.

maybe_print_model_state(State) ->
  case State#state.states of
    [IndState] ->
      io:format
	("Final model state:~n~s~nSchedule state:~n~s~n",
	 [print_model_state(IndState#onestate.sdata,State#state.dataSpec),
	  print_schedule_state(IndState#onestate.swait,State#state.waitSpec)]);
    _ ->
      ok
  end.

print_test_state(#state{test_state=TS,testingSpec=TestModule}) ->
  io:format
    ("Final test state:~n~s~n",
     [try TestModule:print_state(TS)
      catch _:_ -> io_lib:format("~p",[TS]) end]).
      
print_model_state(ModelState,ModelSpec) ->
  try ModelSpec:print_state(ModelState)
  catch _:_ -> io_lib:format("~p",[ModelState]) end.

print_schedule_state(ScheduleState,ScheduleSpec) ->
  try ScheduleSpec:print_state(ScheduleState)
  catch _:_ -> io_lib:format("~p",[ScheduleState]) end.
      
find_active_jobs(StatesAndJobs) ->
  lists:foldl
    (fun (SJ={S,Jobs},{A,F}) ->
	 if
	   (Jobs=/=[]) orelse (S#onestate.incoming=/=[]) -> {[SJ|A],F};
	   true -> {A,[S|F]}
	 end
     end,
     {[],[]},
     StatesAndJobs).

%% Check whether remaining jobs (which have not finished) can be finished by the model 
check_remaining_jobs(OrigState,FinalStates) ->
  ?LOG("FinalStates=~n~p~n",[FinalStates]),
  {SuccessStates,JobsPerFailedState} =
    lists:foldl
      (fun (IndState,{S,J}) ->
	   case executable_jobs(IndState#onestate.waiting,IndState,OrigState,both) of
	     [] -> {[IndState|S],J};
	     Jobs -> {S,[Jobs|J]}
	   end
       end, 
       {[],[]},
       FinalStates),
  if
    SuccessStates==[] -> 
      io:format
	("~n*** Error: at least one of the following calls can be completed by the model "++
	   "but have not been completed:~n~p~n",
	 [lists:usort
	    (lists:flatmap
	       (fun (Jobs) ->
		    lists:map(fun (Job) -> Job#job.call end, Jobs)
		end, JobsPerFailedState))]),
      print_test_state(OrigState),
      maybe_print_model_state(OrigState),
      false;
    true ->
      true
  end.

executable_jobs(Jobs,IndState,State,WhatToCheck) ->
  lists:filter(fun (Job) -> job_is_executable(Job,IndState,State,WhatToCheck) end, Jobs).

job_eq(Job1,Job2) ->
  (Job1#job.pid==Job2#job.pid) andalso (Job1#job.call==Job2#job.call).

find_job(Job,List) ->
  find(fun (ListJob) -> job_eq(Job,ListJob) end, List).

find(_F,[]) -> false;
find(F,[Elem|Rest]) -> 
  case F(Elem) of 
    true ->
      {ok,Elem};
    false ->
      find(F,Rest)
  end.

job_exists(Job,JobList) ->
  lists:any(fun (ListJob) -> job_eq(Job,ListJob) end, JobList).

is_void_job(Job) ->
  is_void_call(Job#job.call).

is_void_call(Command) ->
  Command=={?MODULE,void,[]}.

delete_job(Job,JobList) ->
  lists:filter(fun (ListJob) -> not(job_eq(ListJob,Job)) end, JobList).

minus_jobs(JobList1,JobList2) ->
  lists:foldl(fun (Job2,Acc) -> delete_job(Job2,Acc) end, JobList1, JobList2).

merge_jobs_and_states(JobsAndStates) ->
  lists:usort(JobsAndStates).

job_is_executable(Job,IndState,State,WhatToCheck) ->
  job_cpre_is_true(Job,IndState,State)
    andalso ((WhatToCheck==safety) orelse job_priority_enabled_is_true(Job,IndState,State)).

job_returns_correct_value(Job,IndState,State) ->
  try (State#state.dataSpec):return_value(IndState#onestate.sdata,resource_call(Job#job.call)) of
      Value -> 
      Value =:= Job#job.result
  catch _:_ ->
      (State#state.dataSpec):return
	(IndState#onestate.sdata,resource_call(Job#job.call),Job#job.result)
  end.

job_cpre_is_true(Job,IndState,State) ->
  (State#state.dataSpec):cpre(resource_call(Job#job.call),IndState#onestate.sdata).

job_pre_is_true(Job,IndState,State) ->
  (State#state.dataSpec):pre(resource_call(Job#job.call),IndState#onestate.sdata).

job_priority_enabled_is_true(Job,IndState,State) ->
  (State#state.waitSpec):priority_enabled(resource_call(Job#job.call),Job#job.waitinfo,IndState#onestate.swait,IndState#onestate.sdata).

job_new_waiting(Job,IndState,State) ->
  {JobWaitInfo,NewWaitState} = 
    (State#state.waitSpec):new_waiting
      (resource_call(Job#job.call),
       IndState#onestate.swait,
       IndState#onestate.sdata),
  UpdatedJob =
    Job#job{waitinfo=JobWaitInfo},
   IndState#onestate
   {
     swait=NewWaitState,
     incoming=IndState#onestate.incoming--[Job],
     waiting=lists:usort([UpdatedJob|IndState#onestate.waiting])
   }.

job_next_state(Job,IndState,State,WhatToCheck) ->
  NewDataState =
    (State#state.dataSpec):post
      (resource_call(Job#job.call),
       IndState#onestate.sdata),
  NewWaitState = 
    if
      WhatToCheck==both ->
	(State#state.waitSpec):post_waiting
	  (resource_call(Job#job.call),
	   Job#job.waitinfo,
	   IndState#onestate.swait,
	   NewDataState);
      true ->
	IndState#onestate.swait
    end,
  IndState#onestate
    {
    swait=NewWaitState,
    sdata=NewDataState,
    waiting=delete_job(Job,IndState#onestate.waiting)
   }.

add_new_jobs(NewJobs,State) ->
  ValidNewJobs = 
    valid_jobs(NewJobs,State),
  State#state
    {states =
       lists:map
	 (fun (IndState) ->
	      IndState#onestate{incoming=IndState#onestate.incoming++ValidNewJobs}
	  end, State#state.states)}.

resource_call({_,Fun,Args}) ->
  {Fun,Args}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(Options,DataSpec,WaitSpec,TestingSpec) ->
  init_table(),
  check_prop(Options,DataSpec,WaitSpec,TestingSpec).

check_prop(Options,DataSpec,WaitSpec,TestingSpec) ->
  Result =
    eqc:quickcheck
      (eqc:on_output
	 (fun eqc_printer/2,prop_ok(Options,DataSpec,WaitSpec,TestingSpec))),
  if
    not(Result) ->
      io:format("~n~n***FAILED~n");
    true ->
      io:format("~n~nPASSED~n",[])
  end,
  Result.

prop_ok(Options,DataSpec,WaitSpec,TestingSpec) ->
  ?FORALL
     (Cmds,
      ?LET
	 (SCmds,
	  eqc_dynamic_cluster:dynamic_commands(?MODULE,init_state(Options,DataSpec,WaitSpec,TestingSpec)),
	  custom_shrinking1(SCmds)),
      ?CHECK_COMMANDS
	 ({H, DS, Res},
	  ?MODULE,
	  Cmds,
	  begin
	    try java:terminate(get_data(node)) catch _:_ -> ok end,
	    if
	      Res == ok ->
		true;
	      true ->
		print_counterexample(Cmds,H,DS,Res),
		false
	    end
	  end)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

custom_shrinking1(Cmds) ->
  Cmds.
  %%io:format("custom_shrinking:~n~p~n",[Cmds]),
  %%?SHRINK
    %% (Cmds,
      %%[ custom_shrinking1(Shrunk) || Shrunk <- shrink_commands(Cmds) ]).

shrink_commands(Cmds) ->
  %%io:format("shrink_commands: ~p~n",[Cmds]),
  %% [Cmds].
  Result = rewrite_pars(Cmds),
  %%io:format("shrink_commands on~n~p~nreturns~n~p~n",[Cmds,Result]),
  Result.

rewrite_pars(Cmds) ->
  rewrite_pars(Cmds,[],[]).
rewrite_pars([],_Prefix,Alternatives) ->
  Alternatives;
rewrite_pars([Command|Rest],Prefix,Alternatives) ->
  case Command of
    {call,Module,do_cmds,[L],_} when length(L)>1 ->
      Sequentialisations = all_orderings(Prefix,L,Rest,Module,do_cmds),
      rewrite_pars(Rest,Prefix++[Command],Sequentialisations++Alternatives);
    _ ->
      rewrite_pars(Rest,Prefix++[Command],Alternatives)
  end.

all_orderings(Prefix,Commands,Suffix,Module,Fun) ->
  lists:map
    (fun (Ordering) ->
	 Prefix
	   ++(lists:map(fun (MFA) -> {call,Module,Fun,[[MFA]],[]} end, Ordering))
	   ++Suffix
     end, all_orderings(Commands)).

all_orderings(Elements) ->
  all_orderings(Elements,Elements).
all_orderings([],_) ->
  [];
all_orderings([First|Rest],Elements) ->
  FirstOrderings = 
    case all_orderings(Elements--[First]) of
      [] -> [[First]];
      Orderings -> lists:map(fun (Ordering) -> [First|Ordering] end, Orderings)
    end,
  FirstOrderings++all_orderings(Rest,Elements).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% To make eqc not print the horrible counterexample
eqc_printer(Format,String) ->
  case Format of
    "~p~n" -> ok;
    _ -> io:format(Format,String)
  end.

print_counterexample(Cmds,H,FailingState,Reason) -> 
  try
    io:format
      ("~n~nTest failed with reason ~p~n",
       [Reason]),
    {FailingCommandSequence,_} =
      lists:split(length(H)+1,Cmds),
    ReturnValues = 
      case Reason of
	{exception,_} ->
	  (lists:map(fun result_value_from_history/1, H))++[Reason];
	_ ->
	  (lists:map(fun result_value_from_history/1, H))
      end,
    io:format("~nCommand sequence:~n"),
    io:format("-----------------~n~n"),
    print_commands
      (lists:zip(tl(FailingCommandSequence),ReturnValues),FailingState),
    io:format("~n~n")
  catch _:Exception ->
      io:format
	("~n*** Warning: print_counterexample raised an exception ~p~n"
	 ++"Stacktrace:~n~p~n",
	 [Exception,erlang:get_stacktrace()]),
      ok
  end.

result_value_from_history({_,_,_,Result}) ->
  Result;
result_value_from_history({_,_,Result}) ->
  Result.

print_commands([],_State) ->
  ok;
print_commands([{Call,Result}|Rest],State) ->
  ResultString =
    case Call of
      {_,_,do_cmds,_Cmds,_} ->
	{_Jobs,Unblocked} =
	  Result,
	UnblockStr =
	  if
	    Unblocked==[] ->
	      "";
	    true ->
	      "-- unblocks "++
	      lists:foldl
		(fun (UnblockedJob,Acc) ->
		     JobStr = print_finished_job_info(UnblockedJob,State),
		     if
		       Acc=="" -> JobStr;
		       true -> Acc++", "++JobStr
		     end
		 end, "", Unblocked)
	  end,
	ExceptionStr =
	  lists:foldl
	    (fun (UnblockedJob,Acc) ->
		 case UnblockedJob#job.result of
		   {java_exception,Exc} ->
		     io:format("~n"),
		     report_java_exception(Exc),
		     io:format("~n"),
		     io_lib:format
		       ("~s ~s raised exception",
			[Acc,print_started_job_info(UnblockedJob,State)]);
		   _ ->
		     Acc
		 end
	     end, "", Unblocked),
	UnblockStr++ExceptionStr;
      _ -> ""
    end,
  CallString =
    case Call of
      {_,_,do_cmds,[_Commands,_],_} ->
	{Jobs,_} = Result,
	io_lib:format("<< ~s >>",[print_jobs(fun print_started_job_info/2,"",Jobs,State)]);
      {_,_,start,_,_} ->
	"start";
      {_,_,Name,Args,_} ->
	io_lib:format("~p ~p",[Name,Args])
    end,
  io:format("  ~s ~s~n",[CallString,ResultString]),
  print_commands(Rest,State).

print_cmds(Acc,[]) -> Acc;
print_cmds(Acc,[{_,Fun,Args}|Rest]) ->
  Comma = if Acc=="" -> Acc; true -> ",\n     " end,
  case Fun of
    void -> print_cmds(io_lib:format("~s~svoid",[Acc,Comma]),Rest);
    _ -> print_cmds(io_lib:format("~s~s~p ~p",[Acc,Comma,Fun,Args]),Rest)
  end.

print_jobs(_,Acc,[],_) -> Acc;
print_jobs(Printer,Acc,[Job|Rest],State) ->
  Comma = if Acc=="" -> Acc; true -> ",\n     " end,
  print_jobs(Printer,io_lib:format("~s~s~s",[Acc,Comma,Printer(Job,State)]),Rest,State).

print_finished_job_info(Job,#state{testingSpec=TestModule,test_state=TS}) ->
  try TestModule:print_finished_job_info(Job,TS)
  catch _:_Reason ->
      io_lib:format("~p",[Job])
  end.

print_started_job_info(Job,#state{testingSpec=TestModule,test_state=TS}) ->
  try TestModule:print_started_job_info(Job,TS)
  catch _:_Reason ->
      io_lib:format("~p",[Job])
  end.

report_java_exception(Exception) ->
  io:format("~n*** Warning: unexpected Java exception~n"),
  Err = java:get_static(java:node_id(Exception),'java.lang.System',err),
  java:call(Exception,printStackTrace,[Err]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_execute(Module,Function,Args,DefaultValue) ->
  case lists:member({Function,length(Args)},Module:module_info(exports)) of
    true ->
      apply(Module,Function,Args);
    false ->
      DefaultValue
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_table() ->
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
  wait_until_stable().

wait_until_stable() ->
  case ets:info(?MODULE) of
    L when is_list(L) ->
      ok;
    _ ->
      wait_until_stable()
  end.

wait_forever() ->
  receive _ -> wait_forever() end.

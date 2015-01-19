-module(tester).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

%%-define(debug,true).
-include("../../src/debug.hrl").

-define(COMPLETION_TIME,100).

-include("tester.hrl").

api_spec() ->
  #api_spec{}.

initial_state() ->
  ?LOG("~p:initial_state~n",[?MODULE]),
  #state{}.

init_state({DataSpec,DI},{WaitSpec,WI},{TestingSpec,TI}) ->
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
	  sdata=DataSpec:init(DI),
	  swait=WaitSpec:init(WI)}
       ],
     test_state=TestingSpec:init(TI),
     dataSpec=DataSpec,
     waitSpec=WaitSpec,
     testingSpec=TestingSpec
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_pre(State) ->
  not(State#state.started).

start_args(State) ->
  [State#state.testingSpec].

start(TestSpec) ->
  [{cp,CP}] =
    ets:lookup(?MODULE,cp),
  ?LOG("CP is ~p~n",[CP]),
  try
    java:start_node
      ([{java_verbose,"SEVERE"},
	{call_timeout,infinity},
	{java_exception_as_value,true},
	{add_to_java_classpath,CP}]) of
    {ok,NodeId} ->
      TestSpec:start(NodeId),
      NodeId
  catch _:_ ->
      io:format("~n*** Error: cannot start java. Is the javaerlang library installed?~n"),
      throw(bad)
  end.

start_post(_State,_,_Result) ->
  true.

start_next(State,_Result,_) ->
  State#state{started=true}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_cmds_pre(State) ->
  State#state.started.

do_cmds_args(State) ->
  [(State#state.testingSpec):command(State#state.test_state,State)].

do_cmds_pre(State,[Commands]) ->
  (State#state.testingSpec):precondition(State,State#state.test_state,filter_commands(Commands)).

do_cmds(Commands) ->
  ?LOG("Commands are ~p~n",[Commands]),
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
  ?LOG("New jobs are ~p~n",[NewJobs]),
  if
    NewJobs==[] ->
      {[],[]};
    true ->
      FinishedJobs = wait_for_jobs(),
      {lists:filter(fun (Job) -> not(is_void_job(Job)) end, NewJobs),
       lists:filter(fun ({Job,_ReturnValue}) -> not(is_void_job(Job)) end, FinishedJobs)}
  end.

do_cmds_post(State,Args,Result) ->
try
  {NewJobs,FinishedJobs} = 
    Result,
  ?LOG
     ("Postcondition: result=~p~n",[Result]),
  case lists:any(fun job_finished_with_exception/1,FinishedJobs) of
    true ->
      java_exception;
    false ->
      FJobs = lists:map(fun ({Job,_}) -> Job end,FinishedJobs),
      case calculate_next_state(add_new_jobs(NewJobs,State),FJobs,safety) of
	false -> false;
	_ -> case calculate_next_state(add_new_jobs(NewJobs,State),FJobs,both) of
	       false -> false;
	       _ -> 
		 %% Finally check whether some non-finished job is finishable in all
		 %% possible model states
		 check_remaining_jobs(State,State#state.states)
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
end.

do_cmds_next(State,Result,Args) ->
  try
    {NewJobs,FinishedJobs} = Result,
    ?LOG("Next_state: result=~p~n",[Result]),
    FJobs = lists:map(fun ({Job,_}) -> Job end,FinishedJobs),
    if
      NewJobs==[] -> State;
      true ->
	{ok,NewState} = calculate_next_state(add_new_jobs(NewJobs,State),FJobs,both),
	NewTestState =
	  (State#state.testingSpec):next_state
	    (NewState#state.test_state,
	     NewState,
	     Result,
	     Args),
	NextState = NewState#state{test_state=NewTestState},
	?LOG("next_state: ~p~n",[NextState]),
	NextState
    end
  catch _:_ ->
      io:format("~n*** Warning: next raises exception~n"),
      io:format("~p~n",[erlang:get_stacktrace()]),
      State
  end.

make_void_call() -> {?MODULE,void,[]}.

valid_jobs(Jobs,State) -> 
  [OneIndState|_] = State#state.states,
  lists:filter
    (fun (Job) ->
	   job_pre_is_true(Job,OneIndState,State) end,
     Jobs).

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
    X={Job,_Result} when is_record(Job,job) ->
      [X|receive_completions()];
    X ->
      io:format("unknown message ~p received~n",[X]),
      throw(bad)
  after 0 -> []
  end.

void() ->
  ok.

job_finished_with_exception({_,Result}) ->
  case Result of
    {java_exception,_Exc} -> true;
    {'EXIT',_,_} -> true;
    _ -> false
  end.

%% Calculate the next model state (a set of possible states) given the
%% set of finished jobs. 
%%
calculate_next_state(State,FinishedJobs,WhatToCheck) ->
  %% First always "accept" an incoming new job
  %% (since otherwise the execution would still be blocked)
  FirstStatesAndJobs =
    if
      WhatToCheck==both ->
	accept_one_incoming(State,FinishedJobs);
      true ->
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
  case finish_jobs(State,FirstStatesAndJobs,[],WhatToCheck) of
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
    
finish_jobs(_,[],FinishedStates,WhatToCheck) ->
  ?LOG("WC=~p Finished=~p~n",[WhatToCheck,FinishedStates]),
  {ok,lists:usort(FinishedStates)};
finish_jobs(State,StatesAndJobs,FinishedStates,WhatToCheck) ->
  ?LOG("WC=~p States:~n~p~nFinished=~p~n",[WhatToCheck,StatesAndJobs,FinishedStates]),
  {NewStatesAndJobs,NewFinishedStates} =
    lists:foldl
      (fun ({IndState,FJobs},{NSJ,NF}) ->
	   if
	     FJobs==[], IndState#onestate.incoming==[] ->
	       {NSJ,[IndState|NF]};
	     true ->
	       NewAcceptStates =
		 if
		   WhatToCheck==both ->
		     lists:map
		       (fun (Job) -> {job_new_waiting(Job,IndState,State),FJobs} end,
			IndState#onestate.incoming);
		   true ->
		     []
		 end,
	       NewFinishStates =
		 lists:flatmap
		   (fun (Job) ->
			case job_exists(Job,FJobs)
			  andalso job_is_executable(Job,IndState,State,WhatToCheck) of
			  true ->
			    [{job_next_state(Job,IndState,State,WhatToCheck),
			      delete_job(Job,FJobs)}];
			  false -> []
			end
		    end,
		    IndState#onestate.waiting),
	       {NewAcceptStates++NewFinishStates++NSJ,NF}
	   end
       end, {[],FinishedStates}, StatesAndJobs),
  if
    NewStatesAndJobs==[], NewFinishedStates==[], WhatToCheck==safety ->
      io:format
	("~n*** Error: there are calls that have been completed by the implementation "++
	   "which cannot be completed by the model (without considering priority)~n"),
      false;

    NewStatesAndJobs==[], NewFinishedStates==[] -> 
      io:format
	("~n*** Error: there are calls that have been completed by the implementation "++
	   "which cannot be completed by the model (when considering priority)~n"),
      false;

    true ->
      finish_jobs
	(State,
	 merge_jobs_and_states(NewStatesAndJobs),
	 lists:usort(NewFinishedStates),
	 WhatToCheck)
  end.

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
check_remaining_jobs(State,FinalStates) ->
  ?LOG("FinalStates=~n~p~n",[FinalStates]),
  {SuccessStates,JobsPerFailedState} =
    lists:foldl
      (fun (IndState,{S,J}) ->
	   case executable_jobs(IndState#onestate.waiting,IndState,State,both) of
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
      false;
    true ->
      true
  end.

executable_jobs(Jobs,IndState,State,WhatToCheck) ->
  lists:filter(fun (Job) -> job_is_executable(Job,IndState,State,WhatToCheck) end, Jobs).

job_eq(Job1,Job2) ->
  (Job1#job.pid==Job2#job.pid) andalso (Job1#job.call==Job2#job.call).

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
     (Cmds,
      ?LET
	 (SCmds,
	  eqc_dynamic_cluster:dynamic_commands(?MODULE,init_state(DataSpec,WaitSpec,TestingSpec)),
	  custom_shrinking1(SCmds)),
      ?CHECK_COMMANDS
	 ({H, DS, Res},
	  ?MODULE,
	  Cmds,
	  begin
	    if
	      Res == ok ->
		true;
	      true ->
		print_counterexample(Cmds,H,DS,Res,TestingSpec),
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
  Result = lists:map(fun cleanup_commands/1, rewrite_pars(Cmds)),
  io:format("shrink_commands on~n~p~nreturns~n~p~n",[Cmds,Result]),
  Result.

cleanup_commands(Cmds) ->
  lists:foldr(fun (Cmd,Acc) -> 
		case Cmd of
		  {Call,Module,Fun,Args,_} ->
		    [{Call,Module,Fun,Args,[]}|Acc];
		  _ ->
		    Acc
		end
	    end, [], Cmds).

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
	   ++(lists:map(fun (MFA) -> [{call,Module,Fun,[MFA],[]}] end, Ordering))
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

print_counterexample(Cmds,H,_FailingState,Reason,TestingSpec) ->
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
    print_commands(lists:zip(tl(FailingCommandSequence),ReturnValues),TestingSpec),
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

print_commands([],_TestingSpec) ->
  ok;
print_commands([{Call,Result}|Rest],TestingSpec) ->
  ResultString =
    case Call of
      {_,_,do_cmds,_Cmds,_} ->
	{_Jobs,Unblocked} =
	  Result,
	UnblockStr =
	  lists:foldl
	    (fun ({UnblockedJob,_Res},Acc) ->
		 io_lib:format("~sunblocks ~s,",[Acc,print_unblocked_job(UnblockedJob,TestingSpec)])
	     end, " -- ", Unblocked),
	ExceptionStr =
	  lists:foldl
	    (fun ({UnblockedJob,Res},Acc) ->
		 case Res of
		   {java_exception,Exc} ->
		     io:format("~n"),
		     report_java_exception(Exc),
		     io:format("~n"),
		     io_lib:format
		       ("~s ~s raised exception",
			[Acc,print_unblocked_job(UnblockedJob,TestingSpec)]);
		   _ ->
		     Acc
		 end
	     end, "", Unblocked),
	UnblockStr++ExceptionStr;
      _ -> ""
    end,
  CallString =
    case Call of
      {_,_,do_cmds,[Commands],_} ->
	io_lib:format("<< ~s >>",[print_cmds("",Commands)]);
      {_,_,Name,Args,_} ->
	io_lib:format("~p ~p",[Name,Args])
    end,
  io:format("  ~s ~s~n",[CallString,ResultString]),
  print_commands(Rest,TestingSpec).

print_cmds(Acc,[]) -> Acc;
print_cmds(Acc,[{_,Fun,Args}|Rest]) ->
  Comma = if Acc=="" -> Acc; true -> ",\n     " end,
  case Fun of
    void -> print_cmds(io_lib:format("~s~svoid",[Acc,Comma]),Rest);
    _ -> print_cmds(io_lib:format("~s~s~p ~p",[Acc,Comma,Fun,Args]),Rest)
  end.

print_unblocked_job(Job,{TestModule,_}) ->
  try TestModule:print_unblocked_job_info(Job)
  catch _:_Reason ->
      io_lib:format("~p",[Job])
  end.

report_java_exception(Exception) ->
  io:format("~n*** Warning: unexpected Java exception~n"),
  Err = java:get_static(java:node_id(Exception),'java.lang.System',err),
  java:call(Exception,printStackTrace,[Err]).


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

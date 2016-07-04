-module(shr_corr_resource).

-behaviour(shr_corr_implementation).

-export([initial_state/2,postcondition/4,next_state/4]).

%% private exports
-export([job_new_waiting/3, executable_jobs/5, job_next_state/6]).

%%-define(debug,true).
-include("debug.hrl").

-include("tester.hrl").
-include("corr_resource_state.hrl").

-record(onestate,{incoming,waiting,sdata,swait}).

initial_state(_,Options) ->
  ?TIMEDLOG("initial_state~n",[]),
  WaitSpec = proplists:get_value(waiting_spec,Options),
  DataSpec = proplists:get_value(data_spec,Options),
  #corr_res_state
    {
     started=false
     ,states=
       [
	#onestate
	{
	  incoming=[]
	  ,waiting=[]
	  ,sdata=shr_utils:initial_state(DataSpec,Options)
	  ,swait=shr_utils:initial_state(WaitSpec,Options)
	}
       ]
     ,options = Options
     ,data_module = shr_utils:module(DataSpec)
     ,waiting_module = shr_utils:module(WaitSpec)
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

postcondition(State,_Args,Result,TS) ->
  {NewJobs,FinishedJobs} = Result,
  ?TIMEDLOG("NewJobs=~p~nFinishedJobs=~p~n",[NewJobs,FinishedJobs]),
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
	  maybe_print_model_state(State),
	  false
      end;
    true ->
  case proplists:get_value(verbose,State#corr_res_state.options,false) of
    true ->
      io:format
	("~npostcondition: new jobs=~n~p~ncompleted jobs=~n~p~nstate=~n~p~n",
	 [NewJobs,FinishedJobs,State]);
    false ->
      ok
  end,
  try
    ?LOG("Postcondition: result=~p~n",[Result]),
    case accept_incoming(add_new_jobs(NewJobs,State),FinishedJobs,safety,State) of
      false -> false;
      _ -> case accept_incoming(add_new_jobs(NewJobs,State),FinishedJobs,both,State) of
	     false -> false;
	     {ok,NewState} -> 
	       %% Finally check whether some non-finished job is finishable in all
	       %% possible model states
	       (not(proplists:get_value(enforce_progress,State#corr_res_state.options,true)))
		 orelse 
		 check_remaining_jobs(State,NewState#corr_res_state.states,TS)
	   end
    end of
    false ->
      io:format
	("postcondition false after starting new jobs:~n  ~s~n",
	   [shr_test_jobs:print_jobs(NewJobs,TS)]),
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

next_state(State,Result,_,_TS) ->
  try
    {NewJobs,FinishedJobs} = Result,
    if
      NewJobs==[] ->
	State;
      true ->
	{ok,NewState} =
	  accept_incoming(add_new_jobs(NewJobs,State),FinishedJobs,both,State),
	NewState
    end
  catch _:_ ->
      io:format("~n*** Warning: next raises exception~n"),
      io:format("~p~n",[erlang:get_stacktrace()]),
      State
  end.

%%valid_jobs(Jobs,State) -> 
%%  [OneIndState|_] = State#corr_res_state.states,
%%  DataModule = State#corr_res_state.data_module,
%%  lists:filter
%%    (fun (Job) ->
%%	   job_pre_is_true(Job,OneIndState,DataModule) end,
%%     Jobs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Calculate the next model state (a set of possible states) given the
%% set of finished jobs. 
%%
accept_incoming(State,FinishedJobs,WhatToCheck,OrigState) ->
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
	   end, State#corr_res_state.states)
    end,
  ?LOG("WC=~p after first:~n~p~n",[WhatToCheck,FirstStatesAndJobs]),

  %% Now finish all remaining jobs, FirstStatesAndJobs is
  %% a list of pairs (State,RemainingJobs) where State is
  %% still a viable State, and RemainingJobs is the set of finished
  %% jobs remaining to execute; once no jobs remain the state moves to
  %% the third parameter.
  case finish_jobs(State,FirstStatesAndJobs,[],WhatToCheck,OrigState) of
    false -> false;
    {ok,FinishStates} -> {ok,State#corr_res_state{states=FinishStates}}
  end.

accept_one_incoming(State,FinishedJobs) ->
  WaitingModule =
    State#corr_res_state.waiting_module,
  NewStates =
    lists:flatmap
      (fun (IndState) ->
	   lists:map
	     (fun (Job) -> job_new_waiting(Job,IndState,WaitingModule) end,
	      IndState#onestate.incoming)
       end, State#corr_res_state.states),
  merge_jobs_and_states
    (lists:map(fun (NewState) -> {NewState,FinishedJobs} end, NewStates)).
    
%% Terminate when no non-finished states remain
finish_jobs(_,[],FinishedStates,_WhatToCheck,_) ->
  ?LOG
     ("WC=~p Finished=~p~n",
      [_WhatToCheck,FinishedStates]),
  {ok,lists:usort(FinishedStates)};
finish_jobs(State,StatesAndJobs,FinishedStates,WhatToCheck,OrigState) ->
  ?LOG("WC=~p States:~n~p~nFinished=~p~n",
       [WhatToCheck,StatesAndJobs,FinishedStates]),
  DataModule = State#corr_res_state.data_module,
  WaitingModule = State#corr_res_state.waiting_module,
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
		       (fun (Job) -> {job_new_waiting(Job,IndState,WaitingModule),FJobs} end,
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
			      job_is_executable(QueueJob,IndState,DataModule,WaitingModule,WhatToCheck)
			      andalso job_returns_correct_value(Job,IndState,DataModule) of
			      true ->
				[{job_next_state(QueueJob,Job#job.result,IndState,DataModule,WaitingModule,WhatToCheck),
				  delete_job(Job,FJobs)}];
			      false ->
				[]
			    end;
			  false ->
			    ?LOG
			      ("Job ~p is missing~nIndState:~n~p~n",
			       [Job,IndState]),
			    []
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
	   "which cannot be completed by the model (when checking return values and without considering priority)~n"),
      maybe_print_model_state(OrigState),
      false;

    NewStatesAndJobs==[], NewFinishedStates==[] -> 
      io:format
	("~n*** Error: there are calls that have been completed by the implementation "++
	   "which cannot be completed by the model (when considering priority)~n"),
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
  case State#corr_res_state.states of
    [IndState] ->
      io:format
	("Final model state:~n  ~s~nSchedule state:~n  ~s~n",
	 [print_model_state(IndState#onestate.sdata,State#corr_res_state.data_module),
	  print_schedule_state(IndState#onestate.swait,State#corr_res_state.waiting_module)]);
    _ ->
      ok
  end.

print_model_state(ModelState,ModelSpec) ->
  try ModelSpec:print_state(ModelState)
  catch _:_ -> io_lib:format("~p",[ModelState]) end.

print_schedule_state(ScheduleState,ScheduleSpec) ->
  try ScheduleSpec:print_state(ScheduleState)
  catch _:_ -> io_lib:format("~p",[ScheduleState]) end.
      
%% Check whether remaining jobs (which have not finished) can be finished by the model 
check_remaining_jobs(OrigState,FinalStates,TS) ->
  ?LOG("FinalStates=~n~p~n",[FinalStates]),
  DataModule = OrigState#corr_res_state.data_module,
  WaitingModule = OrigState#corr_res_state.waiting_module,
  {SuccessStates,JobsPerFailedState} =
    lists:foldl
      (fun (IndState,{S,J}) ->
	   case executable_jobs(IndState#onestate.waiting,IndState,DataModule,WaitingModule,both) of
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
	   "but have not been completed:~n~s~n",
	 [shr_test_jobs:print_jobs
	    (lists:usort
	       (fun (Job1,Job2) -> Job1#job.call == Job2#job.call end,
		lists:flatten(JobsPerFailedState)), TS)]),
      maybe_print_model_state(OrigState),
      false;
    true ->
      true
  end.

executable_jobs(Jobs,IndState,DataModule,WaitingModule,WhatToCheck) ->
  lists:filter(fun (Job) -> job_is_executable(Job,IndState,DataModule,WaitingModule,WhatToCheck) end, Jobs).

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

delete_job(Job,JobList) ->
  lists:filter(fun (ListJob) -> not(job_eq(ListJob,Job)) end, JobList).

merge_jobs_and_states(JobsAndStates) ->
  lists:usort(JobsAndStates).

job_is_executable(Job,IndState,DataModule,WaitingModule,WhatToCheck) ->
  job_cpre_is_true(Job,IndState,DataModule)
    andalso ((WhatToCheck==safety) orelse job_priority_enabled_is_true(Job,IndState,WaitingModule)).

job_returns_correct_value(Job,IndState,DataModule) ->
  Result=
  DataModule:return
    (IndState#onestate.sdata,resource_call(Job#job.call),Job#job.result),
  ?LOG
    ("Job ~p returns correct value ~p? ~p~n",
     [Job#job.call,Job#job.result,Result]),
  Result.

job_cpre_is_true(Job,IndState,DataModule) ->
  DataModule:cpre(resource_call(Job#job.call),IndState#onestate.sdata).

job_pre_is_true(Job,IndState,DataModule) ->
  DataModule:pre(resource_call(Job#job.call),IndState#onestate.sdata).

job_priority_enabled_is_true(Job,IndState,WaitingModule) ->
  WaitingModule:priority_enabled(resource_call(Job#job.call),Job#job.waitinfo,IndState#onestate.swait,IndState#onestate.sdata).

job_new_waiting(Job,IndState,WaitingModule) ->
  {JobWaitInfo,NewWaitState} = 
    WaitingModule:new_waiting
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

job_next_state(Job,Result,IndState,DataModule,WaitingModule,WhatToCheck) ->
  NewDataState =
    case job_pre_is_true(Job,IndState,DataModule) of
      false ->
	IndState#onestate.sdata;
      true ->
	DataModule:post
	  (resource_call(Job#job.call),
	   Result,
	   IndState#onestate.sdata)
    end,
  NewWaitState = 
    if
      WhatToCheck==both ->
	WaitingModule:post_waiting
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
  ValidNewJobs = NewJobs,
%%  ValidNewJobs = 
%%    valid_jobs(NewJobs,State),
  State#corr_res_state
    {states =
       lists:map
	 (fun (IndState) ->
	      IndState#onestate{incoming=IndState#onestate.incoming++ValidNewJobs}
	  end, State#corr_res_state.states)}.

resource_call({_,Fun,Args}) ->
  {Fun,Args}.


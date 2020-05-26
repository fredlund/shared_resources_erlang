-module(shr_corr_resource).

-behaviour(shr_corr_implementation).

-export([initial_state/2,postcondition/4,next_state/4]).

%% private exports
-export([job_new_waiting/3, executable_jobs/4, job_next_states/5]).
-export([is_deterministic/1]).
-export([resource_call/1]).

%%-define(debug,true).
-include("debug.hrl").

-include("tester.hrl").
-include("corr_resource_state.hrl").

initial_state(_,Options) ->
  ?TIMEDLOG("initial_state~n",[]),
  DataSpec = proplists:get_value(data_spec,Options),
  DataModule = shr_utils:module(DataSpec),
  WaitSpec = proplists:get_value(waiting_spec,Options),
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
	  ,swait=shr_utils:initial_state(WaitSpec,[{state_module,DataModule}|Options])
	}
       ]
     ,options = Options
     ,data_module = shr_utils:module(DataSpec)
     ,waiting_module = shr_utils:module(WaitSpec)
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

postcondition(State,_Args,Result,TS) ->
  {NewJobs,FinishedJobs} = Result,
  ?RAWLOG
    ("~n~n******************** postcondition ********************~n~n",[]),
  ?LOG
     ("postcondition: NewJobs=~n  ~p~nFinishedJobs=~p~n~n",
      [NewJobs,FinishedJobs]),
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
	  maybe_print_model_state(void,State),
	  false
      end;
    true ->
  case proplists:get_value(verbose,State#corr_res_state.options,false) of
    true ->
      io:format
	("~~nnpostcondition: new jobs=~n~p~ncompleted jobs=~n~p~nstate=~n~p~n",
	 [NewJobs,FinishedJobs,State]);
    false ->
      ok
  end,
  try
    case accept_incoming(add_new_jobs(NewJobs,State),FinishedJobs,State,TS) of
      false -> false;
      {ok,NewState} -> 
	%% Finally check whether some non-finished job is finishable in all
	%% possible model states
	case return_remaining_states(State,NewState#corr_res_state.states,TS) of
	  [] -> false;
	  _ -> true
	end
    end of
    false ->
      io:format
	("~n*** Error: postcondition false after starting new jobs:~n  ~s~n~n",
	   [shr_test_jobs:print_jobs(NewJobs,TS)]),
      false;
    true ->
      true
  catch _:Reason ->
      io:format("postcondition raises ~p~nStacktrace:~n~p~n",
		[Reason,
		 erlang:get_stacktrace()]),
      error(badresource)
  end
  end.

next_state(State,Result,_,TS) ->
  try
    {NewJobs,FinishedJobs} = Result,
    if
      NewJobs==[] ->
	State;
      true ->
	{ok,NewState} =
	  accept_incoming(add_new_jobs(NewJobs,State),FinishedJobs,State,TS),
	RemainingStates =
	  return_remaining_states(State,NewState#corr_res_state.states,TS),
	NewState#corr_res_state{states=RemainingStates}
    end
  catch _:_ ->
      io:format("~n*** Warning: next raises exception~n"),
      io:format("~p~n",[erlang:get_stacktrace()]),
      error(badresource)
  end.

is_deterministic(State) ->
  case State#corr_res_state.states of
    [_,_|_] -> false;
    _ -> true
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
accept_incoming(State,FinishedJobs,OrigState,TS) ->
  %% First always "accept" an incoming new job
  %% (since otherwise the execution would still be blocked)
  FirstStatesAndJobs = accept_one_incoming(State,FinishedJobs),
  ?LOG("after first:~n~p~n~n",[FirstStatesAndJobs]),

  %% Now finish all remaining jobs, FirstStatesAndJobs is
  %% a list of pairs (State,RemainingJobs) where State is
  %% still a viable State, and RemainingJobs is the set of finished
  %% jobs remaining to execute; once no jobs remain the state moves to
  %% the third parameter.
  case finish_jobs(State,FirstStatesAndJobs,[],OrigState,FinishedJobs,TS) of
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
finish_jobs(_,[],FinishedStates,_,_,_) ->
  ?LOG
     ("Finishing (no StatesJobs remain)... FinishedStates=~n  ~p~n~n",
      [FinishedStates]),
  {ok,lists:usort(FinishedStates)};
finish_jobs(State,StatesAndJobs,FinishedStates,OrigState,FinishedJobs,TS) ->
  ?LOG("finish_jobs: StatesJobs:~n  ~p~nFinishedStates=~n  ~p~n~n",
       [StatesAndJobs,FinishedStates]),

  %% Recurse over the list of possible states 
  %% (and finished jobs in each state)
  {NewStatesAndJobs,NewFinishedStates} =
    lists:foldl
      (fun ({IndState,FJobs},{NSJ,NF}) ->
	   case compute_transitions(IndState,FJobs,State) of
	     terminated ->
	       {NSJ,[IndState|NF]};
	     NewStatesJobs when is_list(NewStatesJobs) ->
	       {NewStatesJobs++NSJ,NF}
	   end
       end, {[],FinishedStates}, StatesAndJobs),
  if
    NewStatesAndJobs==[], NewFinishedStates==[] ->
      io:format
	("~n*** Error: there are calls among~n~s~n"++
   	   "that have been completed "++
	   "by the implementation\n"++
	   "which cannot be completed by the model\n"++
	   "(when checking that implementation return values\n"++
	   "are permitted by the specification)~n~n",
	 [shr_test_jobs:print_jobs(nonsilent_jobs(FinishedJobs,State),TS)]),
      maybe_print_model_state(void,OrigState),
      false;

    true ->
      finish_jobs
	(State,
	 merge_jobs_and_states(NewStatesAndJobs),
	 lists:usort(NewFinishedStates),
	 OrigState,FinishedJobs,TS)
  end.


compute_transitions(#onestate{incoming=Incoming,waiting=Waiting}=IndState,
		    FJobs,State) ->

  DataModule = State#corr_res_state.data_module,
  WaitingModule = State#corr_res_state.waiting_module,

  %% Is there a silent job executable
  SilentJobs =
    silent_jobs(Waiting,State),
  SilentExecutables =
    lists:filter
      (fun (Job) -> 
	   job_is_executable(Job,IndState,DataModule,WaitingModule)
       end, SilentJobs),
  ?LOG
     ("silent_jobs=~n~p~nsilent_executables=~n~p~n~n",
      [SilentJobs,SilentExecutables]),
  
  NonSilentFJobs = 
    nonsilent_jobs(FJobs,State),

  if
    %% Nothing to do when no more jobs has terminated, 
    %% incoming is empty,
    %% and no silent call is executable -->
    %% move state to finished states
    NonSilentFJobs==[], Incoming==[], SilentExecutables==[] ->
      terminated;
    
    true ->
      
      %% Calculate accepting moves
      NewAcceptStates =
	lists:map
	  (fun (Job) -> 
	       {job_new_waiting(Job,IndState,WaitingModule),FJobs} 
	   end,
	   Incoming),
      
      %% Silent moves
      NewSilentStates =
	lists:flatmap
	  (fun (Job) ->
	       ReturnValue =
		 job_returns
		   (Job,
		    IndState,
		    DataModule),
	       
	       NextStates =
		 job_next_states
		   (Job,
		    %% May be underspecified
		    ReturnValue,
		    IndState,
		    DataModule,
		    WaitingModule),
	       lists:map
		 (fun (NextState) -> {NextState,FJobs} end,
		  NextStates)
	   end,
	   SilentExecutables),
      
      %% A call finishes
      NewFinishStates =
	lists:flatmap
	  (fun (Job) ->
	       case find_job(Job,Waiting) of
		 {ok,QueueJob} ->
		   %% We have to be careful -- the job in the state
		   %% has the waiting info while the completed job has
		   %% the correct result
		   case
		     job_is_executable(QueueJob,IndState,
				       DataModule,WaitingModule)
		     andalso job_returns_correct_value(Job,IndState,
						       DataModule) of
		     true ->
		       NextStates = 
			 job_next_states
			   (QueueJob,Job#job.result,
			    IndState,
			    DataModule,
			    WaitingModule),
		       NewJobs =
			 delete_job(Job,FJobs),
		       lists:map
			 (fun (NextState) -> {NextState,NewJobs} end,
			  NextStates);
		     false -> []
		   end;
		 false -> []
	       end
	   end,
	   NonSilentFJobs),

      NewAcceptStates++NewFinishStates++NewSilentStates
  end.

maybe_print_model_state(IndStates,CorrState) ->
  {IndState,PrintStr} =
    case IndStates of
      [I] -> 
        {I,"Final model state"};
      _ -> 
        case CorrState#corr_res_state.states of
          [IS] -> {IS,"Model state"};
          _ -> {void,""}
        end
    end,
  if
    IndState =/= void ->
      ScheduleStateStr =
        if
          IndState#onestate.swait=/=void ->
            "schedule state:~n  "++
              print_schedule_state(IndState#onestate.swait,CorrState#corr_res_state.waiting_module);
          true ->
            ""
        end,
      io:format
	("~s~n  ~s~n~s~n",
	 [
          PrintStr,
          print_model_state(IndState#onestate.sdata,CorrState#corr_res_state.data_module),
          ScheduleStateStr
         ]);
    true ->
      ok
  end.

print_model_state(ModelState,ModelSpec) ->
  try ModelSpec:print_state(ModelState)
  catch 
    _:undef ->
      io_lib:format("~w",[ModelState]);
    Class:Reason -> 
      io:format
	("*** WARNING: printing model state ~p using ~p fails due to~n~p:~p~n",
	 [ModelState,ModelSpec,Class,Reason]),
      io:format
	("*** Stacktrace:~n~p~n",
	 [erlang:get_stacktrace()]),
      io_lib:format("~p",[ModelState]) 
  end.

print_schedule_state(ScheduleState,ScheduleSpec) ->
  try ScheduleSpec:print_state(ScheduleState)
  catch 
    _:undef ->
      io_lib:format("~w",[ScheduleState]);
    Class:Reason -> 
      io:format
	("*** WARNING: printing schedule state ~p using ~p fails due to~n~p:~p~n",
	 [ScheduleState,ScheduleSpec,Class,Reason]),
      io:format
	("*** Stacktrace:~n~p~n",
	 [erlang:get_stacktrace()]),
      io_lib:format("~p",[ScheduleState]) 
  end.
      
%% Returns the states which are such that a job
%% can be executed, i.e., which since the implementation did NOT
%% execute that job signals an incompatible state.
%% There should be no silent jobs still executable, since they presumably
%% were already executed, so no special care with silent jobs is needed here.
%% If all states can execute a non-silent action, an error is printed.
return_remaining_states(OrigState,FinalStates,TS) ->
  ?LOG("FinalStates=~n~p~n",[FinalStates]),
  DataModule = OrigState#corr_res_state.data_module,
  WaitingModule = OrigState#corr_res_state.waiting_module,
  {SuccessStates,JobsPerFailedState} =
    lists:foldl
      (fun (#onestate{waiting=Waiting}=IndState,{S,J}) ->
	   case executable_jobs(Waiting,IndState,DataModule,WaitingModule) of
	     [] -> {[IndState|S],J};
	     Jobs -> {S,[Jobs|J]}
	   end
       end, 
       {[],[]},
       FinalStates),
  if
    SuccessStates==[] -> 
      io:format
	("~n*** Error: at least one of the following calls can be "++
	   "completed by the model but have not been completed:~n~s~n",
	 [shr_test_jobs:print_jobs
	    (lists:usort
	       (fun (Job1,Job2) -> Job1#job.call == Job2#job.call end,
		lists:flatten(JobsPerFailedState)), TS)]),
      maybe_print_model_state(FinalStates,OrigState);
    true ->
      ok
  end,
  SuccessStates.

executable_jobs(Jobs,IndState,DataModule,WaitingModule) ->
  lists:filter
    (fun (Job) -> 
	 job_is_executable(Job,IndState,DataModule,WaitingModule) 
     end, Jobs).

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

silent_jobs(Jobs,State) ->
  SilentCalls = proplists:get_value(silent,State#corr_res_state.options,[]),
  lists:filter
    (fun (Job) ->
	 {Resource,Operation,_} = Job#job.call,
	 lists:member({Resource,Operation},SilentCalls)
     end, Jobs).

nonsilent_jobs(Jobs,State) ->
  SilentCalls = proplists:get_value(silent,State#corr_res_state.options,[]),
  lists:filter
    (fun (Job) ->
	 {Resource,Operation,_} = Job#job.call,
	 not(lists:member({Resource,Operation},SilentCalls))
     end, Jobs).

delete_job(Job,JobList) ->
  lists:filter(fun (ListJob) -> not(job_eq(ListJob,Job)) end, JobList).

merge_jobs_and_states(JobsAndStates) ->
  lists:usort(JobsAndStates).

job_is_executable(Job,IndState,DataModule,WaitingModule) ->
  job_cpre_is_true(Job,IndState,DataModule)
    andalso job_priority_enabled_is_true(Job,IndState,WaitingModule).

job_returns(Job,IndState,DataModule) ->
  Result =
    DataModule:return_value
      (IndState#onestate.sdata,resource_call(Job#job.call)),
  ?LOG
    ("Job ~p returns value ~p~n",
     [Job#job.call,Result]),
  Result.
  
job_returns_correct_value(Job,IndState,DataModule) ->
  Result =
    DataModule:return
      (IndState#onestate.sdata,resource_call(Job#job.call),Job#job.result,Job#job.symbolicResult),
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

job_next_states(Job,Result,IndState,DataModule,WaitingModule) ->
  NewDataStates =
    case job_pre_is_true(Job,IndState,DataModule) of
      false ->
	[IndState#onestate.sdata];
      true ->
	case
	  DataModule:post
	  (resource_call(Job#job.call),
	   Result,
	   IndState#onestate.sdata,
           Job#job.symbolicResult) of
	  {'$shr_nondeterministic',States} -> States;
	  State -> [State]
	end
    end,
  lists:map
    (fun (NewDataState) ->
	 NewWaitState = 
	   WaitingModule:post_waiting
	     (resource_call(Job#job.call),
	      Job#job.waitinfo,
	      IndState#onestate.swait,
	      NewDataState),
	 IndState#onestate
	   {
	   swait=NewWaitState,
	   sdata=NewDataState,
	   waiting=delete_job(Job,IndState#onestate.waiting)
	  }
     end, NewDataStates).

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


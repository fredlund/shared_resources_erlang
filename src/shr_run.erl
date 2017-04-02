-module(shr_run).

%%-define(debug,true).
-include("debug.hrl").

%% Allow to redefine it via options
-define(COMPLETION_TIME,300).

-include("tester.hrl").

-record(state,{blocked_users=[],old_commands=[],time=0}).


run(TestCase,Options) ->
  shr_utils:setup_shr(),
  run(TestCase,[],Options,#state{}).

run([],History,_Options,State) ->
  History;
run([TestItems|Rest],History,Options,State) ->
  {HistoryItem,NewState} = run_test_items(TestItems,History,Options,State),
  run(Rest,History++[HistoryItem],Options,State).

run_test_items(TestItems,History,Options,State) ->
  ParentPid =
    self(),
  Counter =
    shr_utils:get({?MODULE,counter}),
  {OldEnabled,NewState} = 
    lists:foldl
      (fun (TestItem,{Cmds,St}) ->
	   case is_enabled(TestItem,Cmds,State) of
	     true -> 
	       {[TestItem|TestItems],
		St#state
		{old_commands=lists:delete(TestItem,St#state.old_commands)}};
	     false ->
	       {Cmds,St}
	   end
       end, {[],State}, State#state.old_commands),
  NewItems =
    lists:filter
      (fun (TestItem) -> 
	   is_enabled(TestItem,OldEnabled,NewState) 
       end, TestItems),
  AllCommands =
    lists:map(fun test_item_to_command/1, OldEnabledItems++NewItems),
  run_commands(AllCommands,History,Options,NewState).

test_item_to_command(TestItem) ->
  case TestItem of
    {User,{Resource,{Operation,Args}}} when is_integer(User) ->
      {User,#command{call={Operation,Args},port=Resource}};
    {Resource,{Operation,Args}} ->
      #command{call={Operation,Args},port=Resource}
  end.

run_commands(Commands,History,Options,State) ->
  try
    NewJobs =
      lists:map
	(fun (Command) ->
	     {F,Args} = Command#command.call,
	     Info = Command#command.options,
	     Call = {Command#command.port,F,Args},
	     PreJob = #job{call=Call,info=Info},
	     try shr_supervisor:add_childfun
		   (fun () ->
			try shr_calls:call(Command#command.port,{F,Args}) of
			    Result ->
			    ParentPid!
			      {PreJob#job{pid=self(),result=Result},Counter}
			catch _Exception:Reason ->
			    StackTrace =
			      erlang:get_stacktrace(),
			    io:format("Job ~p exiting due to ~p~nStacktrace:~n~p~n",
				      [self(),Reason,StackTrace]),
			    Result = 
			      {exit,self(),Reason,StackTrace},
			    ParentPid!
			      {PreJob#job{pid=self(),result=Result},Counter}
			end
		    end) of
		 Pid -> PreJob#job{pid=Pid}
	     catch _Exception2:Reason2 ->
		 StackTrace = erlang:get_stacktrace(),
		 PreJob#job
		   {pid=spawn
			  (fun () ->
			       Result = {exit,self(),Reason2,StackTrace},
			       ParentPid!{PreJob#job{pid=self(),result=Result},Counter}
			   end)}
	     end
	 end, Commands),
    if
      NewJobs==[] ->
	{[],[]};
      true ->
	FinishedJobs = wait_for_jobs(NewJobs,WaitTime,Counter),
	?TIMEDLOG("NewJobs=~p~nFinishedJobs=~p~n",[NewJobs,FinishedJobs]),
	HistoryItem = {NewJobs,FinishedJobs},
	{HistoryItem,State#state{time=State#state.time+1}}
    end
  catch _ExceptionType:Reason ->
      io:format
	("*** Error: ~p:do_cmds(~p) raised an exception ~p~n",
	 [?MODULE,self(),Reason]),
      io:format
	("Stacktrace:~n~p~n",
	 [erlang:get_stacktrace()]),
      throw(bad)
  end.

wait_for_jobs(NewJobs,WaitTime,Counter) ->
  JobsAlive = shr_utils:get({?MODULE,jobs_alive}),
  timer:sleep(WaitTime),
  case receive_completions(Counter,[],NewJobs++JobsAlive) of
    FinishedJobs when is_list(FinishedJobs) ->
      AllJobs = 
	NewJobs++JobsAlive,
      NewJobsAlive =
	lists:filter
	  (fun (OldJob) ->
	       lists:all(fun (FinishedJob) ->
			     OldJob#job.pid=/=FinishedJob#job.pid
			 end,FinishedJobs)
	   end, AllJobs),
      shr_utils:put({?MODULE,jobs_alive},NewJobsAlive),
      FinishedJobs
  end.

receive_completions(Counter,Finished,JobsAlive) ->
  receive
    {'EXIT',Pid,Reason} ->
      handle_exit(Pid,Reason,[],Counter,Finished,JobsAlive);
    {'DOWN',_,_,Pid,Reason} ->
      handle_exit(Pid,Reason,[],Counter,Finished,JobsAlive);
    {Job,JobCounter} ->
      case Job of
	{exit,Pid,Reason,StackTrace} when JobCounter==Counter -> 
	  handle_exit(Pid,Reason,StackTrace,Counter,Finished,JobsAlive);
	{exit,Pid,_Reason,_} ->
	  io:format
	    ("*** Warning: exit for old job ~p received; consider increasing wait time.~n",[Pid]),
	  receive_completions(Counter,Finished,JobsAlive);
	_ when is_record(Job,job), JobCounter==Counter ->
	  receive_completions(Counter,[Job|Finished],JobsAlive);
	_ when is_record(Job,job) ->
	  io:format
	    ("*** Warning: old job received; consider increasing wait time.~n"),
	  receive_completions(Counter,Finished,JobsAlive)
      end;
    X ->
      io:format("~p: unknown message ~p received~n",[?MODULE,X]),
      throw(bad)
  after 0 -> lists:reverse(Finished)
  end.

handle_exit(Pid,Reason,StackTrace,Counter,Finished,JobsAlive) ->
  if
    Reason=/=normal ->
      case lists:filter(fun (Job) -> Pid==Job#job.pid end, JobsAlive) of
	[Job] ->
	  NewJob = Job#job{result={exit,Pid,Reason,StackTrace}},
	  receive_completions(Counter,[NewJob|Finished],JobsAlive);
	[] -> 
	  ?TIMEDLOG
	    ("*** Warning: got exit for process ~p which is not a current job~n",
	     [Pid]),
	  receive_completions(Counter,Finished,JobsAlive)
      end;
    true -> receive_completions(Counter,Finished,JobsAlive)
  end.

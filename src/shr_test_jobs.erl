%% Testing of jobs, where jobs may fail to complete nondeterministically
%% 
%% The module is parametric on:
%%          - a module for generating commands to start jobs
%%          - a module for deciding whether the finished (and non-finished jobs)
%%            have behaved correctly

-module(shr_test_jobs).

-export([api_spec/0,initial_state/0]).
-export([init_state/1]).
-export([start_pre/1,start_args/1,start/2,start_post/3,start_next/3]).
-export([do_cmds_pre/1,do_cmds_args/1,do_cmds_pre/2,do_cmds/4,do_cmds_post/3,do_cmds_next/3]).
-export([print_jobs/2]).
-export([job_exited/1]).

-export([command_parser/1]).
-export([prop_res/1, check_prop/1, check_prop/2, eqc_printer/2]).

-export([return_test_cases/0,print_test_cases/0,print_test_case/1,basic_test_case/1]).
-export([initial_gen_state/1,test_data_spec/1,test_waiting_spec/1,gen_module/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

%% Super fragile below
-record(eqc_statem_history,{state, args, command, features, result}).

%%-define(debug,true).
-include("debug.hrl").

%% Allow to redefine it via options
-define(COMPLETION_TIME,300).

-include("tester.hrl").
-include("corr_resource_state.hrl").

-record(state,
	{
	  started    %% Started testing?
	  ,options    %% Testing options
          ,test_data_spec
          ,test_waiting_spec
	  ,test_gen_state
	  ,test_corr_state
	  ,test_gen_module
	  ,test_corr_module
	  ,test_observers_states
	  ,completion_time
	  ,start_fun
	  ,stop_fun
	  ,jobs_alive
          ,counter
	  ,symVarsCounter=0
	}).

-record(observer,{name,module,state}).


api_spec() ->
  #api_spec{}.

initial_state() ->
  ?TIMEDLOG("initial_state~n",[]),
  #state{}.

init_state(Options) ->
  ?TIMEDLOG("init_state. Options are~n~p~n",[Options]),
  TestGenSpec = proplists:get_value(test_gen_spec,Options),
  TestCorrSpec = proplists:get_value(test_corr_spec,Options),
  TestObserverSpecs = proplists:get_value(test_observers_spec,Options,[]),
  StartFun = proplists:get_value(start_fun,Options),
  StopFun = proplists:get_value(stop_fun,Options),
  
  {ok,TestObserversState} = 
    observer_initial_states(TestObserverSpecs,Options),
  #state
    {
     started=false
    ,options=Options
    ,test_data_spec=proplists:get_value(data_spec,Options)
    ,test_waiting_spec=proplists:get_value(waiting_spec,Options)
    ,test_gen_state=shr_utils:initial_state(TestGenSpec,Options)
    ,test_corr_state=shr_utils:initial_state(TestCorrSpec,Options)
    ,test_observers_states=TestObserversState
    ,test_gen_module=shr_utils:module(TestGenSpec)
    ,test_corr_module=shr_utils:module(TestCorrSpec)
    ,start_fun=StartFun
    ,stop_fun=StopFun
    ,jobs_alive=[]
    ,completion_time=proplists:get_value(completion_time,Options,?COMPLETION_TIME)
    }.

observer_initial_states(TestObserverSpecs,Options) ->
  NameModuleInits = 
    lists:map
      (fun ({Name,Module}) ->
	   {Name,Module,[]};
	   ({Name,Module,Init}) ->
	   {Name,Module,Init}
       end, TestObserverSpecs),
  lists:foldl
    (fun ({Name,Module,Init},{RAcc,ISAcc}) ->
	 case Module:init(Init,Options) of
	   {ok,InitialState} ->
	     Observer = 
	       #observer{name=Name,module=Module,state=InitialState},
	     {RAcc,[Observer|ISAcc]};
	   Other ->
	     io:format
	       ("*** SHRT ERROR: protocol observer ~p does not return a "++
		  "valid initial state: ~p~n",
		[Name,Other]),
	     {failed,ISAcc}
	 end
     end, {ok,[]}, NameModuleInits).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_pre(State) ->
  not(State#state.started).

start_args(State) ->
  [
   State#state.options
   ,State#state.start_fun
  ].

start(Options,StartFun) ->
  Id = proplists:get_value(id,Options,unknown),
  shr_utils:put(?MODULE,{id,Id}),
  Counter =
    case shr_utils:get({?MODULE,counter}) of
      undefined -> 0;
      N when is_integer(N), N > 0 -> N
    end,
  shr_utils:put({?MODULE,counter},Counter + 1),
  shr_utils:put({?MODULE,jobs_alive},[]),
  shr_utils:setup_shr(),
  ?TIMEDLOG
    ("start_fun is ~p~n",
     [StartFun]),
  StartResult =
    if
      is_function(StartFun) ->
	try 
	  Self = self(),
	  {Pid,Ref} = 
	    spawn_monitor
	      (fun () ->
		   Result = StartFun(Options),
		   Self!{started,Result}
	       end),
	  wait_start(Pid,Ref)
	catch Class:Reason:StackTrace ->
	    io:format
	      ("*** SHRT ERROR: function ~p with options~n~p~n"++
		 "raised an exception ~p:~p~n",
	       [StartFun,Options,Class,Reason]),
	    io:format
	      ("stacktrace:~n~p~n",
	       [StackTrace]),
	    error(bad_startfun)
	end;
      true ->
	[]
    end,
  if
    StartResult==false -> false;
    true -> Counter
  end.

wait_start(Pid,Ref) ->
  receive
    {'DOWN',Ref,process,_Object,Info} ->
      io:format("~n*** SHRT WARNING: start_fun failed due to:~n~p~n",[Info]),
      wait_start(Pid,Ref);
    {started,Result} ->
	?TIMEDLOG
	   ("start_fun returned ~p~n",
	    [Result]),
	Result;
    {'DOWN',_,_,_,normal} ->
      wait_start(Pid,Ref);
    Other ->
      io:format("~n*** SHRT WARNING: got message ~p~n",[Other]),
      wait_start(Pid,Ref)
    after 10000 ->
	io:format("*** SHRT ERROR: start_fun does not return~n"),
	false
    end.

start_post(_State,_,{'EXIT',Reason}) ->
  io:format
    ("*** SHRT ERROR: start raised the exception ~p~n",
     [Reason]),
  io:format
    ("Stacktrace:~n~p~n",
     [erlang:get_stacktrace()]),
  false;
start_post(_State,_,Result) ->
  Result=/=false.

start_next(State,
	   Counter,
	   [Options,_]) ->
  State#state
    {
    started=true
    ,options = Options
    ,counter=Counter
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_cmds_pre(State) ->
  State#state.started.

do_cmds_args(State) ->
  ?TIMEDLOG("do_cmds_args~n",[]),
  try
    WaitTime =
      State#state.completion_time,
    NoEnvWait =
      proplists:get_value(no_env_wait,State#state.options,false),
    ?LET(Commands,
	 (State#state.test_gen_module):command
	   (State#state.test_gen_state,
	    State#state.test_corr_state),
	 begin
	   ?LOG("Commands are ~p~n",[Commands]),
	   [
            lists:map(fun command_parser/1,Commands),
            WaitTime,
            NoEnvWait,
            State#state.symVarsCounter]
	 end)
  catch _:Reason:StackTrace ->
      io:format
	("*** MODEL ERROR: ~p:do_cmds_args raised an exception ~p in state~n  ~p~n"
	 ++"Such errors cannot be handled by the QuickCheck dynamic state machine...~n",
	 [?MODULE,Reason,State]),
      io:format
	("Stack trace:~n~p~n",
	 [StackTrace]),
      error(bad_state_machine)
  end.

do_cmds_pre(State,[Commands|_]) ->
  try
    ?LOG("do_cmds_pre: will check pre for ~p~n",[Commands]),
    ensure_boolean
      ((State#state.test_gen_module):precondition
	 (State#state.test_gen_state,raw(Commands),State#state.test_corr_state))
  catch _:Reason:StackTrace ->
      io:format
	("*** MODEL ERROR: ~p:do_cmds_pre raised an exception ~p in state~n  ~p~n"
	 ++"with commands ~p~n"
	 ++"Such errors cannot be handled by the QuickCheck dynamic state machine...~n",
	 [?MODULE,Reason,State,Commands]),
      io:format
	("Stack trace:~n~p~n",
	 [StackTrace]),
      error(bad_state_machine)
  end.

do_cmds(Commands,WaitTime,NoEnvWait,SymVarsCounter) ->
  ?TIMEDLOG("run_commands(~p)~n",[Commands]),
  try
    ?TIMEDLOG("new round: cmds = ~p~n",[Commands]),
    ParentPid =
      self(),
    Counter =
      shr_utils:get({?MODULE,counter}),
    NewJobs =
      lists:map
	(fun ({Cnt,Command}) ->
	     {F,Args} = Command#command.call,
	     Info = Command#command.options,
	     Call = {Command#command.port,F,Args},
	     PreJob = #job{call=Call,info=Info,symbolicResult={var,Cnt+SymVarsCounter}},
	     try shr_supervisor:add_childfun
		   (fun () ->
                        ?TIMEDLOG("beginning to execute ~p~n",[PreJob]),
			try shr_calls:call(Command#command.port,{F,Args}) of
			    Result ->
                            ?TIMEDLOG("finished executing ~p~n",[PreJob]),
			    ParentPid!
			      {PreJob#job{pid=self(),result=Result},Counter}
			catch _Exception:Reason:StackTrace ->
			    io:format("Job ~p exiting due to ~p~nStacktrace:~n~p~n",
				      [self(),Reason,StackTrace]),
			    Result = 
			      {exit,self(),Reason,StackTrace},
			    ParentPid!
			      {PreJob#job{pid=self(),result=Result},Counter}
			end
		    end) of
		 Pid -> PreJob#job{pid=Pid}
	     catch _Exception2:Reason2:StackTrace ->
		 PreJob#job
		   {pid=spawn
			  (fun () ->
			       Result = {exit,self(),Reason2,StackTrace},
			       ParentPid!{PreJob#job{pid=self(),result=Result},Counter}
			   end)}
	     end
	 end, lists:zip(lists:seq(0,length(Commands)-1),Commands)),
    if
      NewJobs==[] ->
	{[],[]};
      true ->
	FinishedJobs = wait_for_jobs(NewJobs,WaitTime,Counter,NoEnvWait),
	?TIMEDLOG("NewJobs=~p~nFinishedJobs=~p~n",[NewJobs,FinishedJobs]),
	{NewJobs,FinishedJobs}
    end
  catch _ExceptionType:Reason:StackTrace ->
      io:format
	("*** SHRT ERROR: ~p:do_cmds(~p) raised an exception ~p~n",
	 [?MODULE,self(),Reason]),
      io:format
	("Stacktrace:~n~p~n",
	 [StackTrace]),
      error(bad)
  end.

raw(Commands) when is_list(Commands) ->
  lists:map(fun raw/1, Commands);
raw(Command) ->
  Command#command.raw.

call(Commands) when is_list(Commands) ->
  lists:map(fun call/1, Commands);
call(Command) ->
  Command#command.call.
  
command_parser(Cmd={Port,Call={_F,_Args}}) ->
  #command{raw=Cmd,port=Port,call=Call};
command_parser(Cmd={Port,Call={_F,_Args},Options}) ->
  #command{raw=Cmd,port=Port,call=Call,options=Options}.

job_exited(Job) ->
  case Job#job.result of
    {exit,_,_,_} -> true;
    _ -> false
  end.

do_cmds_post(State,[Commands|_],Result={NewJobs,FinishedJobs}) ->
try
  ?TIMEDLOG("after do_cmds result=~p~n",[Result]),
  case lists:filter(fun job_exited/1, FinishedJobs) of
    [Job|_] ->
      {exit,Pid,ExitReason,_} = Job#job.result,
      io:format
	("*** Test Error: job ~p terminated with exception~n  ~p~n",
	 [Pid,ExitReason]),
      false;
    [] ->
      case
	(State#state.test_corr_module):postcondition
	(State#state.test_corr_state,
	 call(filter_environment_commands(State,Commands)),
	 {filter_environment_jobs(State,NewJobs),
	  filter_environment_jobs(State,FinishedJobs)},
	 State) of
	true ->
	  case observers_next_states
	    (State#state.test_observers_states,NewJobs, FinishedJobs) of
	    {ok, _} -> true;
	    {fail, _} -> false
	  end;
	false -> false
      end
  end of PostResult when is_boolean(PostResult) -> 
    shr_utils:put(failed,not(PostResult)), PostResult
  catch _:Reason:StackTrace ->
      io:format
	("*** Model error: ~p:do_cmds_post raised an exception ~p in state~n  ~p~n"
	 ++"with commands ~p"
	 ++"~nresult is ~p~n"
	 ++"Such errors cannot be handled by the QuickCheck dynamic state machine...~n",
	 [?MODULE,Reason,State,Commands,Result]),
      io:format
	("Stack trace:~n~p~n",
	 [StackTrace]),
      error(bad_state_machine)
  end.

do_cmds_next(State,Result={NewJobs,FinishedJobs},[Commands|_]) ->
  ?TIMEDLOG("do_cmds_next, result=~p~n",[Result]),
  ?TIMEDLOG("Num states=~p~n",[length((State#state.test_corr_state)#corr_res_state.states)]),
  try
    NewTestGenState =
      (State#state.test_gen_module):next_state
	(State#state.test_gen_state,{NewJobs,FinishedJobs},
	 raw(Commands),
	 State#state.test_corr_state),
    {ok, NewTestObserversStates} =
      observers_next_states 
	(State#state.test_observers_states, NewJobs, FinishedJobs),
    NewTestCorrState =
      (State#state.test_corr_module):next_state
	(State#state.test_corr_state,
	 {filter_environment_jobs(State,NewJobs),
	  filter_environment_jobs(State,FinishedJobs)},
	 call(filter_environment_commands(State,Commands)),
	 State),
    State#state
      {
      test_gen_state=NewTestGenState
      ,test_corr_state=NewTestCorrState
      ,test_observers_states=NewTestObserversStates
      ,symVarsCounter=State#state.symVarsCounter+length(NewJobs)
     }
  catch _:Reason:StackTrace ->
      io:format
	("*** MODEL ERROR: ~p:do_cmds_next raised an exception ~p in state~n  ~p~n"
	 ++"with commands ~p"
	 ++"~nresult is ~p~n"
	 ++"Such errors cannot be handled by the QuickCheck dynamic state machine...~n",
	 [?MODULE,Reason,State,Commands,Result]),
      io:format
	("Stack trace:~n~p~n",
	 [StackTrace]),
      error(bad_state_machine)
  end.

observers_next_states(ModuleStates,NewJobs,FinishedJobs) ->
  lists:foldl
    (fun (Observer,{RAcc,SAcc}) ->
	 #observer{name=Name,module=Module,state=State} = Observer,
	 case Module:next_state(NewJobs, FinishedJobs, State) of
	   {ok,NextState} ->
	     {RAcc,[Observer#observer{state=NextState}|SAcc]};
	   Other ->
	     io:format
	       ("*** Test error: protocol observer ~p signalled a testing failure "++
		  "for the new jobs~n  ~p~nand the finished jobs~n  ~p~n"++
		  "Error was ~p~n",
		[Name,NewJobs,FinishedJobs,Other]),
	     {failed,SAcc}
	 end
     end, {ok,[]}, ModuleStates).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter_environment_commands(_State,Commands) ->
  lists:filter
    (fun (Command) ->
	 not(is_environment(Command#command.port))
     end, Commands).

%% Heuristic check for environments
is_environment(environment) ->
  true;
is_environment({environment,_}) ->
  true;
is_environment(_) ->
  false.

wait_for_jobs(NewJobs,WaitTime,Counter,NoEnvWait) ->
  JobsAlive = shr_utils:get({?MODULE,jobs_alive}),
  TimeStamp = get_millisecs_timestamp(),
  UntilTime = TimeStamp + WaitTime,
  ?TIMEDLOG("~p: waiting for ~p for ~p (=~p)~n",[TimeStamp,NewJobs,WaitTime,shr_utils:milliseconds_after()]),
  AllJobs = NewJobs++JobsAlive,
  case receive_completions(UntilTime,Counter,[],AllJobs,NoEnvWait) of
    {FinishedJobs,NewJobsAlive} when is_list(FinishedJobs) ->
      shr_utils:put({?MODULE,jobs_alive},NewJobsAlive),
      FinishedJobs
  end.

receive_completions(_UntilTime,_Counter,Finished,[],false) ->
  {Finished,[]};
receive_completions(UntilTime,Counter,Finished,JobsAlive,NoEnvWait) ->
  Timeout = max(0,UntilTime-get_millisecs_timestamp()),
  receive
    {'EXIT',Pid,Reason} ->
      handle_exit(UntilTime,Pid,Reason,[],Counter,Finished,JobsAlive,NoEnvWait);
    {'DOWN',_,_,Pid,Reason} ->
      handle_exit(UntilTime,Pid,Reason,[],Counter,Finished,JobsAlive,NoEnvWait);
    {Job,JobCounter} ->
      case Job of
	{exit,Pid,Reason,StackTrace} when JobCounter==Counter -> 
	  handle_exit(UntilTime,Pid,Reason,StackTrace,Counter,Finished,JobsAlive,NoEnvWait);
	{exit,Pid,_Reason,_} ->
	  io:format
	    ("*** ~p: SHRT WARNING: exit for old job~n~p~n received; consider increasing wait time.~n",[shr_utils:milliseconds_after(),Pid]),
	  receive_completions(UntilTime,Counter,Finished,JobsAlive,NoEnvWait);
	_ when is_record(Job,job), JobCounter==Counter ->
          ?TIMEDLOG("got job ~p~n",[Job]),
	  receive_completions(UntilTime,Counter,[Job|Finished],delete_job(Job,JobsAlive),NoEnvWait);
	_ when is_record(Job,job) ->
	  io:format
	    ("*** ~p: SHRT WARNING: old job~n~p~n received; consider increasing wait time.~n",[shr_utils:milliseconds_after(),Job]),
	  receive_completions(UntilTime,Counter,Finished,JobsAlive,NoEnvWait)
      end;
    X ->
      io:format("SHRT WARNING: ~p: unknown message ~p received~n",[?MODULE,X]),
      error(bad)
  after Timeout -> {lists:reverse(Finished),JobsAlive}
  end.

delete_job(Job,Jobs) ->
  lists:filter(fun (OldJob) -> OldJob#job.pid=/=Job#job.pid end, Jobs).

handle_exit(UntilTime,Pid,Reason,StackTrace,Counter,Finished,JobsAlive,NoEnvWait) ->
  if
    Reason=/=normal ->
      case lists:filter(fun (Job) -> Pid==Job#job.pid end, JobsAlive) of
	[Job] ->
	  NewJob = Job#job{result={exit,Pid,Reason,StackTrace}},
	  receive_completions(UntilTime,Counter,[NewJob|Finished],delete_job(NewJob,JobsAlive),NoEnvWait);
	[] -> 
	  ?TIMEDLOG
	    ("*** SHRT WARNING: got exit for process ~p which is not a current job~n",
	     [Pid]),
	  receive_completions(UntilTime,Counter,Finished,JobsAlive,NoEnvWait)
      end;
    true -> receive_completions(UntilTime,Counter,Finished,JobsAlive,NoEnvWait)
  end.

get_millisecs_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

filter_environment_jobs(_State,Jobs) ->
  lists:filter
    (fun (Job) -> 
	 {Port,_,_} = Job#job.call,
	 not(is_environment(Port))
     end,
     Jobs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_prop(Options) ->
  check_prop(fun prop_res/1,Options).

check_prop(Prop,Options) ->
  shr_utils:put(test_cases,[]),
  EQCProp =
    eqc:on_test
    (fun () -> io:format("starting...~n"), fun () -> ok end end,
     eqc:on_output(fun eqc_printer/2,Prop(Options))),
  Result = eqc:quickcheck(EQCProp),
  if
    not(Result) ->
      io:format("~n~n***FAILED~n");
    true ->
      io:format("~n~nPASSED~n",[])
  end,
  Result.

prop_res(Options) ->
  MoreCommands = 
    case proplists:get_value(more_commands,Options) of
      N when is_integer(N), N>=1 ->
        N;
      _ ->
        5
    end,
  ?FORALL
     (Cmds,
      ?LET(SCmds,
	   (more_commands
	      (100,eqc_dynamic_cluster:dynamic_commands
		   (?MODULE,init_state(Options)))),
	   SCmds),
      ?CHECK_COMMANDS
	 ({H, DS, Res},
	  ?MODULE,
	  Cmds,
	  begin
	    case proplists:get_value(stop_fun,Options) of
	      Fun when is_function(Fun) ->
		Fun(Options);
	      _ ->
		ok
	    end,
	    case proplists:get_value(print_trace,DS#state.options,false) of
	      true ->
		print_testcase(Cmds,H,DS,Res);
	      false ->
		ok
	    end,
	    TestCases = 
	      case shr_utils:get(test_cases) of
		undefined -> [];
		L when is_list(L) -> L 
	      end,
	    NewTestCase =
	      #test_case
	       {
		 test_case=Cmds,
		 test_result=(Res==ok)
	       },
	    NewTestCases = 
	      [NewTestCase|TestCases],
	    shr_utils:put(test_cases,NewTestCases),
	    if
	      (Res == ok) ->
		case proplists:get_value(print_testcase,Options,false) of
		  true ->
		    print_testcase(Cmds,H,DS,Res);
		  false ->
		    ok
		end,
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

print_counterexample(Cmds,H,FailingState,Reason) -> 
    io:format
      ("~n~nTest failed with reason ~p~n",
       [Reason]),
  print_testcase1(Cmds,H,FailingState,Reason).

print_testcase(Cmds,H,State,Result) ->
  print_testcase1(Cmds,H,State,Result).

print_testcase1(Cmds,H,State,Result) ->
  try
    CommandResultSequence = commands_and_return_values(Cmds,H,Result),
    io:format("~nCommand sequence:~n"),
    io:format("-----------------~n~n"),
    print_terminated_commands(CommandResultSequence,State),
    io:format("~n~n")
  catch _:Exception:StackTrace ->
      io:format
	("~n*** SHRT WARNING: print_counterexample raised an exception ~p~n"
	 ++"Stacktrace:~n~p~n",
	 [Exception,StackTrace]),
      ok
  end.

commands_and_return_values(Cmds,H,Result) ->
  {CommandSequence,_} =
    lists:split(length(H)+1,Cmds),
  ReturnValues = 
    case Result of
      {exception,_} ->
	(lists:map(fun result_value_from_history/1, H))++[Result];
      _ ->
	(lists:map(fun result_value_from_history/1, H))
    end,
  lists:zip(tl(CommandSequence),ReturnValues).

result_value_from_history({_,_,_,Result}) ->
  Result;
result_value_from_history({_,_,Result}) ->
  Result;
result_value_from_history(Other) ->
  if
    is_record(Other,eqc_statem_history) ->
      Other#eqc_statem_history.result;
    true ->
      io:format
	("*** SHRT WARNING: don't know how to extract the result from "++
	   "the statement history~n"),
      if 
	is_tuple(Other) ->
	  io:format
	    ("... it is a tuple of size ~p with elements~n",
	     [size(Other)]),
	  lists:foreach
	    (fun (I) ->
		 io:format("~p: ~p~n",[I,element(I,Other)])
	     end, lists:seq(1,size(Other)));
	true ->
	  io:format
	    ("... it is a value of the shape~n~p~n",
	     [Other])
      end,
      void
  end.

ensure_boolean(true) ->
  true;
ensure_boolean(false) ->
  false;
ensure_boolean(Other) ->
  io:format("*** SHRT ERROR: ensure_boolean expects a boolean -- got ~p~n",[Other]),
  io:format
    ("Stacktrace:~n~p~n",
     [try throw(trace) catch _:_:StackTrace -> StackTrace, Other end]).

print_terminated_commands([],_State) ->
  ok;
print_terminated_commands([{Call,Result}|Rest],State) ->
  ResultString =
    case Call of
      {_,_,do_cmds,_Cmds,_} ->
	{_Jobs,Finished} = Result,
	FinishedJobs =
	  lists:filter(fun (Job) -> not(job_exited(Job)) end, Finished),
	ExitedJobs = 
	  lists:filter(fun job_exited/1, Finished),
	FinishedStr =
	  if
	    FinishedJobs == [] -> "";
	    true ->
	      "-- unblocks "++
	      lists:foldl
		(fun (UnblockedJob,Acc) ->
		     JobStr = print_finished_job_info(UnblockedJob,State),
		     RetStr = 
		       case UnblockedJob#job.result of
			 void -> "";
			 Other -> " (returns "++io_lib:format("~p",[Other])++") "
		       end,
		     if
		       Acc=="" -> JobStr++RetStr;
		       true -> Acc++", "++JobStr++RetStr
		     end
		 end, "", FinishedJobs)
	  end,
	ExitedStr = 
	  if 
	    ExitedJobs == [] -> "";
	    true -> 
	      " -- exited "++
	      lists:foldl
		(fun (ExitedJob,Acc) ->
		     JobStr = print_finished_job_info(ExitedJob,State),
		     if
		       Acc=="" -> JobStr;
		       true -> Acc++", "++JobStr
		     end
		 end, "", ExitedJobs)
	  end,
	FinishedStr++ExitedStr;
      _ -> ""
    end,
  CallString = print_call_result(Call,Result,State),
  if
    CallString=/="" ->
      io:format("  ~s ~s~n",[CallString,ResultString]);
    true ->
      ok
  end,
  print_terminated_commands(Rest,State).

print_call_result(Call,Result,State) ->
    case Call of
      {_,_,do_cmds,[Commands|_],_} ->
	print_call_commands_result(Commands,Result,State);
      {_,_,do_cmds,[Commands|_],_,_} ->
	print_call_commands_result(Commands,Result,State);
      {_,_,start,_,_} ->
	"";
      {_,_,Name,Args,_} ->
	io_lib:format("~p ~p",[Name,Args])
    end.

print_call_commands_result(Commands,Result,State) ->
  {Jobs,_} = Result,
  {Prefix,Postfix} = 
    if
      length(Commands)>1 -> {"<< "," >>"};
      true -> {"",""}
    end,
  io_lib:format
    ("~s~s~s",
     [Prefix,print_jobs(fun print_started_job_info/2,"",Jobs,State),Postfix]).

print_jobs(Jobs,State) ->
  io_lib:format
    ("<< ~s >>",
     [print_jobs(fun print_started_job_info/2,"",Jobs,State)]).

print_jobs(_,Acc,[],_) -> Acc;
print_jobs(Printer,Acc,[Job|Rest],State) ->
  Comma = if Acc=="" -> Acc; true -> ",\n     " end,
  print_jobs(Printer,io_lib:format("~s~s~s",[Acc,Comma,Printer(Job,State)]),Rest,State).

print_finished_job_info(Job,#state{test_gen_module=TGM,test_gen_state=TGS}) ->
  try TGM:print_finished_job_info(Job,TGS)
  catch _:_Reason ->
      io_lib:format("~p",[Job])
  end.

print_started_job_info(Job,#state{test_gen_module=TGM,test_gen_state=TGS}) ->
  try TGM:print_started_job_info(Job,TGS)
  catch _:_Reason ->
      io_lib:format("~p",[Job])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_test_cases() ->
  shr_utils:get(test_cases).

print_test_cases() ->
  lists:foreach
    (fun (TestCase) ->
	 print_test_case(convert_to_basic_testcase(TestCase))
     end, return_test_cases()).

initial_gen_state(TestCase) ->
  State = initial_state_from_test_case(TestCase),
  State#state.test_gen_state.

test_data_spec(TestCase) ->
  State = initial_state_from_test_case(TestCase),
  State#state.test_data_spec.

test_waiting_spec(TestCase) ->
  State = initial_state_from_test_case(TestCase),
  State#state.test_waiting_spec.

gen_module(TestCase) ->
  State = initial_state_from_test_case(TestCase),
  State#state.test_gen_module.

initial_state_from_test_case(TestCase) ->
  case TestCase of
    [{init,Init}|_] ->
      {State,_} = Init,
      State
  end.
  
basic_test_case(TestCase) ->
  lists:filter
    (fun (TestCase) ->
	 case TestCase of
	   {init,_} -> false;
	   {final,_,_} -> false;
	   T when is_tuple(T) -> element(3,T)==do_cmds
	 end
     end, TestCase).

convert_to_basic_testcase(TestCase) when is_record(TestCase,test_case) ->
  convert_to_basic_testcase(TestCase#test_case.test_case);
convert_to_basic_testcase(TestCase) ->
  lists:reverse
    (lists:foldl
       (fun (Command,Acc) ->
	    if
	      is_tuple(Command), size(Command)>=4 ->
		case element(3,Command) of
		  do_cmds ->
		    case element(4,Command) of
		      [Commands|_] -> [Commands|Acc];
		      _ -> Acc
		    end;
		  _ -> Acc
		end;
	      true -> Acc
	    end
	end, [], TestCase)).

print_test_case(T) ->
  TestCase = T#test_case.test_case,
  io_lib:format
    ("~n~s~n",
     [
      combine
	("  ",
	 lists:map
	   (fun (Cmds) ->
		{Commands,Prefix,Postfix} =
		  if
		    is_list(Cmds), length(Cmds)>1 ->
		      if
			length(Cmds)>1 -> {Cmds,"<< "," >>"};
			true -> {hd(Cmds),"",""}
		      end;
		    true -> {Cmds,"",""}
		  end,
		io_lib:format
		  ("~s~s~s",
		   [Prefix,
		    print_commands(Commands),
		    Postfix])
	    end, convert_to_basic_testcase(TestCase)), "\n")
     ]).

combine(_Pre,[],_Combinator) ->
  "";
combine(Pre,[Hd],_Combinator) ->
  Pre++Hd;
combine(Pre,[Hd|Rest],Combinator) ->
  if
    Hd=="" -> combine(Pre,Rest,Combinator);
    true -> Pre++Hd++Combinator++combine(Pre,Rest,Combinator)
  end.

print_commands([]) ->
  "";
print_commands([Cmd]) ->
  {F,Args} = Cmd#command.call,
  Target = Cmd#command.port,
  io_lib:format("~s",[shr_utils:print_mfa({Target,F,Args})]);
print_commands([Cmd|Rest]) ->
  io_lib:format("~s,~s",[print_commands([Cmd]),print_commands(Rest)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




		    

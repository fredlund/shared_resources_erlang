-module(shr_run).

%%-define(debug,true).
-include("debug.hrl").

-export([run/1,run/2,runs/3,runs/4]).
-export([print_run/1]).

%% Allow to redefine it via options
-define(COMPLETION_TIME,300).

-include("tester.hrl").

-record(state,{blocked_users=[],old_items=[],time=0}).

run(TestCase) ->
  run(TestCase,[]).

run(TestCase,Options) ->
  ?TIMEDLOG("~n~n*** beginning run~n",[]),
  shr_supervisor:ensure_started(self()),
  shr_utils:put({?MODULE,jobs_alive},[]),
  shr_utils:put({?MODULE,counter},0),
  run(TestCase,[],Options,#state{}).

run([],History,_Options,_State) ->
  History;
run([TestItems|Rest],History,Options,State) ->
  {HistoryItem={NewJobs,FinishedJobs},NewState} = 
    run_test_items(TestItems,Options,State),
  NewerState =
    lists:foldl
      (fun (Job,S) ->
	   case proplists:get_value(user,Job#job.info) of
	     User when is_integer(User) ->
	       S#state{blocked_users=lists:delete(User,S#state.blocked_users)};
	     _ ->
	       S
	   end
       end, NewState, FinishedJobs),
  NewHistory =
    if
      NewJobs==[] -> History;
      true -> History++[HistoryItem]
    end,
  run(Rest,NewHistory,Options,NewerState).

runs(TestCase,SetupFun,Time) ->
  runs(TestCase,SetupFun,Time,[]).

runs(TestCase,SetupFun,Time,Options) ->
  lists:map
    (fun (Run) ->
	 lists:map(fun ({_,Item}) -> Item end, Run)
     end, runs1(TestCase,SetupFun,[],Time+get_millisecs_timestamp(),Options)).

runs1(TestCase,SetupFun,Runs,Deadline,Options) ->
  Now = get_millisecs_timestamp(),
  if
    Now>Deadline ->
      Runs;
    true ->
      shr_supervisor:restart(self()),
      SetupFun(),
      Run = normalize_run(run(TestCase,Options)),
      case member_run(Run,Runs) of
	true ->
	  runs1(TestCase,SetupFun,Runs,Deadline,Options);
	false ->
	  runs1(TestCase,SetupFun,[Run|Runs],Deadline,Options)
      end
  end.

normalize_run(History) ->
  lists:map
    (fun (Item={NewJobs,FinishedJobs}) -> 
	 {{lists:sort(lists:map(fun normalize_job/1,NewJobs)),
	   lists:sort(lists:map(fun normalize_job/1,FinishedJobs))},
	  Item}
     end, History).

member_run(Run,NormalizedRuns) ->
  lists:member(strip_run(Run),lists:map(fun strip_run/1,NormalizedRuns)).

strip_run(Run) ->
  lists:map(fun ({NI,_}) -> NI end, Run).
	   
normalize_job(Job) ->
  {Job#job.call,Job#job.result}.

run_test_items(PreTestItems,Options,State) ->
  ?TIMEDLOG("run_test_items ~p State=~p~n",[PreTestItems,State]),
  TestItems =
    if 
      is_list(PreTestItems) -> PreTestItems;
      is_tuple(PreTestItems) -> [PreTestItems]
    end,
  {OldEnabledItems,NewState} = 
    lists:foldl
      (fun (TestItem,{TItems,S}) ->
	   case is_enabled(TestItem,TItems,S) of
	     true -> 
	       {[TestItem|TItems],
		S#state{old_items=lists:delete(TestItem,S#state.old_items)}};
	     false ->
	       {TItems,S}
	   end
       end, {[],State}, State#state.old_items),
  {NewItems,NewBlockedItems} =
    lists:foldl
      (fun (TestItem,{NI,NB}) -> 
	   case is_enabled(TestItem,OldEnabledItems,NewState) of
	     true -> {[TestItem|NI],NB};
	     false -> {NI,[TestItem|NB]}
	   end
       end, {[],[]}, TestItems),
  AllItems =
    OldEnabledItems++NewItems,
  AllNewUsers =
    users(AllItems),
  NewerState =
    NewState#state
    {blocked_users=AllNewUsers++NewState#state.blocked_users,
     old_items=NewState#state.old_items++NewBlockedItems},
  ?TIMEDLOG("running ~p blocking ~p~n",[AllItems,AllNewUsers]),
  AllCommands =
    lists:map(fun test_item_to_command/1, AllItems),
  WaitTime =
    proplists:get_value(completion_time,Options,?COMPLETION_TIME),
  EnvWait =
    proplists:get_value(no_env_wait,Options,false),
  {NewJobs,FinishedJobs} = shr_test_jobs:do_cmds(AllCommands,WaitTime,EnvWait),
  HistoryItem = {NewJobs,FinishedJobs},
  NewestState =
    if
      NewJobs=/=[] ->
	NewerState#state{time=NewerState#state.time+1};
      true ->
	NewerState
    end,
  {HistoryItem,NewestState}.

is_enabled(TestItem,TestItems,State) ->
  (not(user_call(TestItem))) orelse 
    ((not(lists:member(user_id(TestItem),State#state.blocked_users)))
     andalso (not(lists:member(user_id(TestItem),users(TestItems))))).

user_call({User,{_Resource,_Operation,_Args}}) when is_integer(User) ->
  true;
user_call(_) ->
  false.

user_id({User,{_Resource,_Operation,_Args}}) when is_integer(User) ->
  User.

users(TestItems) ->
  lists:foldl
    (fun (TestItem,Acc) ->
	 case user_call(TestItem) of
	   true ->
	     [user_id(TestItem)|Acc];
	   false ->
	     Acc
	 end
     end, [], TestItems).

test_item_to_command(TestItem) ->
  case TestItem of
    {User,{Resource,Operation,Args}} when is_integer(User) ->
      #command{call={Operation,Args},port=Resource,options=[{user,User}]};
    {Resource,Operation,Args} ->
      #command{call={Operation,Args},port=Resource,options=[]}
  end.

get_millisecs_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

print_run([]) ->
  ok;
print_run([{NewJobs,Finished}|Rest]) ->
  FinishedJobs =
    lists:filter(fun (Job) -> not(shr_test_jobs:job_exited(Job)) end, Finished),
  ExitedJobs = 
    lists:filter(fun shr_test_jobs:job_exited/1, Finished),
  FinishedStr =
    if
      FinishedJobs == [] -> "";
      true ->
	"-- unblocks "++
	  lists:foldl
	    (fun (UnblockedJob,Acc) ->
		 JobStr = print_job(UnblockedJob),
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
		 JobStr = print_job(ExitedJob),
		 if
		   Acc=="" -> JobStr;
		   true -> Acc++", "++JobStr
		 end
	     end, "", ExitedJobs)
    end,
  CallString = print_new_jobs(NewJobs),
  if
    CallString=/="" ->
      io:format("  ~s ~s~n",[CallString,FinishedStr++ExitedStr]);
    true ->
      ok
  end,
  print_run(Rest).

print_new_jobs(NewJobs) ->
  {Prefix,Postfix} = 
    if
      length(NewJobs)>1 -> {"<< "," >>"};
      true -> {"",""}
    end,
  io_lib:format
    ("~s~s~s",
     [Prefix,
      print_jobs(NewJobs),
      Postfix]).

print_jobs([]) ->
  "";
print_jobs([Job]) ->
  print_job(Job);
print_jobs([Job|Rest]) ->
  io_lib:format("~s, ~s",[print_jobs([Job]),print_jobs(Rest)]).

print_job(Job) ->
  {R,F,Args} = Job#job.call,
  io_lib:format("~s",[shr_utils:print_mfa({R,F,Args})]).
  

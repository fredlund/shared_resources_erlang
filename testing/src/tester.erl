-module(tester).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-define(COMPLETION_TIME,100).

-include("tester.hrl").

api_spec() ->
  #api_spec{}.

callouts(_,_) ->
  ?EMPTY.

initial_state() ->
  io:format("~p:initial_state~n",[?MODULE]),
  #state{}.

init_state({DataSpec,DI},{WaitSpec,WI},{TestingSpec,TI}) ->
  io:format("~p:init_state~n",[?MODULE]),
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

command(State) ->
  if
    not(State#state.started) ->
      {call,?MODULE,start,[]};
    true ->
      (State#state.testingSpec):command(State#state.test_state,State)
  end.

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
      (fun (Command) ->
	   case Command of
	     {call,?MODULE,void,[],_} -> true;
	     _ -> false
	   end
       end,
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
  case Call of
    {_,_,start,_,_} ->
      not(State#state.started);
    {_,_,void,_,_} ->
      true;
    {_,_,do_job,_,_} ->
      State#state.started
	andalso (State#state.testingSpec):precondition(State,State#state.test_state,Call)
  end.

next_state(State,Result,Call) ->
  case Call of
    {_,_,start,_,_} ->
      State#state{started=true};
    {_,_,void,_,_} ->
      State;
    {_,_,do_cmds,[_],_} ->
      {NewJobs,FinishedJobs} = Result,
      NewState = calculate_next_state(add_new_jobs(NewJobs,State),FinishedJobs),
      NewTestState =
	(State#state.testingSpec):next_state
	  (NewState#state.test_state,
	   NewState,
	   Result,
	   Call),
      State#state{test_state=NewTestState}
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
	  calculate_next_state(add_new_jobs(NewJobs,State),FinishedJobs),
	  true
      end;
    _ -> true
  end.

job_finished_with_exception({_,Result}) ->
  case Result of
    {java_exception,_Exc} -> true;
    _ -> false
  end.

calculate_next_state(State,FinishedJobs) ->
  if
    FinishedJobs==[] ->
      State;
    true ->
      FirstStatesAndJobs = accept_one_incoming(State,FinishedJobs),
      FinalStates = finish_jobs(State,FirstStatesAndJobs,[]),
      check_remaining_jobs(State,FinalStates),
      State#state{states=FinalStates}
  end.

accept_one_incoming(State,FinishedJobs) ->
  NewStates =
    lists:flatmap
      (fun (IndState) ->
	   lists:map
	     (fun (Job) -> [job_new_waiting(Job,IndState,State)] end,
	      IndState#onestate.incoming)
       end, State#state.states),
  merge_jobs_and_states
    (lists:map(fun (NewState) -> {NewState,FinishedJobs} end, NewStates)).
    
finish_jobs(_,[],FinishedJobs) ->
  lists:usort(lists:map(fun ({S,_}) -> S end,FinishedJobs));
finish_jobs(State,StatesAndJobs,FinishedJobs) ->
  NewStatesAndJobs =
    lists:flatmap
      (fun ({IndState,FJobs}) ->
	   NewAcceptStates =
	     lists:map
	       (fun (Job) -> [{job_new_waiting(Job,IndState,State),FJobs}] end,
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
       end, StatesAndJobs),
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
		 (Jobs=/=[]) orelse (S#onestate.waiting=/=[])->
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

check_remaining_jobs(State,FinalStates) ->
  ok.
job_cpre_is_true(Job,IndState,State) ->
  (State#state.dataSpec):cpre(Job#job.call,IndState#onestate.sdata).

job_pre_is_true(Job,IndState,State) ->
  (State#state.dataSpec):pre(Job#job.call,IndState#onestate.sdata).

job_priority_enabled_is_true(Job,IndState,State) ->
  (State#state.dataSpec):pre(Job#job.call,IndState#onestate.swait,IndState#onestate.sdata).

job_new_waiting(Job,IndState,State) ->
  NewWaitState = 
    (State#state.waitSpec):new_waiting
      (Job#job.call,
       IndState#onestate.swait,
       IndState#onestate.sdata),
  IndState#onestate
    {
    swait=NewWaitState,
    incoming=IndState#onestate.incoming--[Job],
    waiting=lists:usort([Job|IndState#onestate.waiting])
   }.

job_next_state(Job,IndState,State) ->
  NewDataState =
    (State#state.dataSpec):post
      (Job#job.call,
       IndState#onestate.sdata),
  NewWaitState = 
    (State#state.waitSpec):new_waiting
      (Job#job.call,
       IndState#onestate.swait,
       NewDataState),
  IndState#onestate
    {
    swait=NewWaitState,
    sdata=NewDataState,
    incoming=IndState#onestate.incoming--[Job]
   }.

add_new_jobs(NewJobs,State) ->
  [OneIndState|_] =
    State#state.states,
  ValidNewJobs =
    lists:filter
      (fun (Job) -> job_pre_is_true(Job,OneIndState,State) end,
       NewJobs),
  State#state
    {states =
       lists:map
	 (fun (IndState) ->
	      IndState#onestate{incoming=IndState#onestate.incoming++ValidNewJobs}
	  end, State#state.states)}.

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
		 io_lib:format("~sunblocks ~p,",[Acc,UnblockedJob])
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
			[Acc,UnblockedJob]);
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


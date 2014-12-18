-module(tester).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

%% -define(debug,true).
-include("../../src/debug.hrl").

-define(COMPLETION_TIME,100).

-include("tester.hrl").

api_spec() ->
  #api_spec{}.

callouts(_,_) ->
  ?EMPTY.

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

-spec command(#state{}) -> any().
command(State) ->
  if
    not(State#state.started) ->
      {call,?MODULE,start,[]};
    true ->
      ?LET
	(Commands,
	 (State#state.testingSpec):command(State#state.test_state,State),
	 case filter_commands(Commands) of
	   Cmds when is_list(Cmds) ->
	     {call,?MODULE,do_cmds,[Cmds]};
	   VoidCall ->
	     VoidCall
	 end)
  end.

filter_commands(Commands) ->
  filter_commands(Commands,false,[]).

filter_commands([],_,L) when L=/=[] ->
  L;
filter_commands([],true,[]) ->
  {call,?MODULE,void,[]};
filter_commands([First|Rest],HasVoid,L) ->
  case First of
    {?MODULE,void,_} ->
      filter_commands(Rest,true,L);
    _ ->
      filter_commands(Rest,HasVoid,[First|L])
  end.

start() ->
  [{cp,CP}] =
    ets:lookup(?MODULE,cp),
  ?LOG("CP is ~p~n",[CP]),
  {ok,NodeId} =
    java:start_node
      ([{java_verbose,"SEVERE"},
	{call_timeout,infinity},
	{java_exception_as_value,true},
	{add_to_java_classpath,CP}]),
  NodeId.

store_data(Key,Value) ->
  ets:insert(?MODULE,{Key,Value}).

get_data(Key) ->
  [{Key,Value}] = ets:lookup(?MODULE,Key),
  Value.

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
    {_,_,do_cmds,_,_} ->
      State#state.started
	andalso (State#state.testingSpec):precondition(State,State#state.test_state,Call)
  end.

next_state(State,Result,Call) ->
  case Call of
    {_,_,start,_,_} ->
      (State#state.testingSpec):start(Result),
      State#state{started=true};
    {_,_,void,_,_} ->
      State;
    {_,_,do_cmds,_,_} ->
      {NewJobs,FinishedJobs} = Result,
      ?LOG
	("Next_state: result=~p~n",[Result]),
      {ok,NewState} =
	calculate_next_state
	  (add_new_jobs(NewJobs,State),
	   lists:map(fun ({Job,_}) -> Job end, FinishedJobs)),
      NewTestState =
	(State#state.testingSpec):next_state
	  (NewState#state.test_state,
	   NewState,
	   Result,
	   Call),
      NextState = NewState#state{test_state=NewTestState},
      ?LOG("next_state: ~p~n",[NextState]),
      NextState
  end.

postcondition(State,Call,Result) ->
  case Call of
    {_,_,do_cmds,_,_} ->
      {NewJobs,FinishedJobs} = 
	Result,
      ?LOG
	("Postcondition: result=~p~n",[Result]),
      case lists:any(fun job_finished_with_exception/1,FinishedJobs) of
	true ->
	  java_exception;
	false ->
	  case calculate_next_state
	    (add_new_jobs(NewJobs,State),
	     lists:map(fun ({Job,_}) -> Job end, FinishedJobs)) of
	    false -> false;
	    _ -> true
	  end
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
      {ok,State};
    true ->
      FirstStatesAndJobs = accept_one_incoming(State,FinishedJobs),
      ?LOG("after first:~n~p~n",[FirstStatesAndJobs]),
      case finish_jobs(State,FirstStatesAndJobs,[]) of
	false -> false;
	{ok,FinalStates} ->
	  case check_remaining_jobs(State,FinalStates) of
	    false -> false;
	    true -> {ok,State#state{states=FinalStates}}
	  end
      end
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
    
finish_jobs(_,[],FinishedStates) ->
  {ok,lists:usort(FinishedStates)};
finish_jobs(State,StatesAndJobs,FinishedStates) ->
  NewStatesAndJobs =
    lists:flatmap
      (fun ({IndState,FJobs}) ->
	   ?LOG("IndState is ~p~n;jobs=~p~n",[IndState,FJobs]),
	   NewAcceptStates =
	     lists:map
	       (fun (Job) -> {job_new_waiting(Job,IndState,State),FJobs} end,
		IndState#onestate.incoming),
	   NewFinishStates =
	     lists:flatmap
	       (fun (Job) ->
		    ?LOG
		      ("for job ~p ",
		       [Job]),
		    ?LOG
		      ("cpre is ~p and priority_enabled is ~p~n",
		       [job_cpre_is_true(Job,IndState,State),
			job_priority_enabled_is_true(Job,IndState,State)]),
		    case job_exists(Job,FJobs)
		      andalso job_cpre_is_true(Job,IndState,State)
		      andalso job_priority_enabled_is_true(Job,IndState,State) of
		      true ->
			[{job_next_state(Job,IndState,State),
			  delete_job(Job,FJobs)}];
		      false -> []
		    end
		end,
		IndState#onestate.waiting),
	   NewAcceptStates++NewFinishStates
       end, StatesAndJobs),
  if
    NewStatesAndJobs==[], FinishedStates==[] ->
      io:format("*** Error: remaining jobs cannot be executed by the model~n"),
      false;
    true -> 
      {ActiveStatesAndJobs,Finished} = 
	lists:foldl
	  (fun (SJ={S,Jobs},{A,F}) ->
	       if
		 (Jobs=/=[]) orelse (S#onestate.incoming=/=[]) ->
		   {[SJ|A],F};
		 true ->
		   {A,[S|F]}
	       end
	   end,
	   {[],[]},
	   NewStatesAndJobs),
      finish_jobs
	(State,
	 merge_jobs_and_states(ActiveStatesAndJobs),
	 lists:usort(Finished++FinishedStates))
  end.

job_eq(Job1,Job2) ->
  (Job1#job.pid==Job2#job.pid) andalso (Job1#job.call==Job2#job.call).

job_exists(Job,JobList) ->
  lists:any(fun (ListJob) -> job_eq(Job,ListJob) end, JobList).

delete_job(Job,JobList) ->
  lists:filter(fun (ListJob) -> not(job_eq(ListJob,Job)) end, JobList).

minus_jobs(JobList1,JobList2) ->
  lists:foldl(fun (Job2,Acc) -> delete_job(Job2,Acc) end, JobList1, JobList2).

merge_jobs_and_states(JobsAndStates) ->
  lists:usort(JobsAndStates).

check_remaining_jobs(State,FinalStates) ->
  true.
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

job_next_state(Job,IndState,State) ->
  NewDataState =
    (State#state.dataSpec):post
      (resource_call(Job#job.call),
       IndState#onestate.sdata),
  NewWaitState = 
    (State#state.waitSpec):post_waiting
      (resource_call(Job#job.call),
       Job#job.waitinfo,
       IndState#onestate.swait,
       NewDataState),
  IndState#onestate
    {
    swait=NewWaitState,
    sdata=NewDataState,
    waiting=delete_job(Job,IndState#onestate.waiting)
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
		print_counterexample(Cmds,H,DS,Res,TestingSpec),
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

print_counterexample(Cmds,H,_DS,Reason,TestingSpec) ->
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
  print_commands(lists:zip(tl(FailingCommandSequence),ReturnValues),TestingSpec),
  io:format("~n~n").

print_commands([],TestingSpec) ->
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
    void -> print_cmds(Acc,Rest);
    _ -> print_cmds(io_lib:format("~s~s~p ~p",[Acc,Comma,Fun,Args]),Rest)
  end.

print_unblocked_job(Job,{TestModule,_}) ->
  try TestModule:print_unblocked_job_info(Job)
  catch _:Reason ->
      io_lib:format("~p",[Job])
  end.

report_java_exception(Exception) ->
  io:format("*** Warning: unexpected Java exception~n"),
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

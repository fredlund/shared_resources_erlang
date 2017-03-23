-module(shr_sample_resource).

-compile(export_all).


%%-define(debug,true).
-include("debug.hrl").

-include("tester.hrl").

-record(state,{state,options,waiting_module,data_module,test_gen_state,test_gen_module,pid_counter}).
-record(onestate,{incoming,waiting,sdata,swait}).

generate(PreOptions) ->
  Options = [no_par|PreOptions],
  WaitSpec = proplists:get_value(waiting_spec,Options),
  DataSpec = proplists:get_value(data_spec,Options),
  TestGenSpec = proplists:get_value(test_gen_spec,Options),
  State =
    #state{
    state=
      #onestate{
       incoming=[]
       ,waiting=[]
       ,sdata=shr_utils:initial_state(DataSpec,Options)
       ,swait=shr_utils:initial_state(WaitSpec,Options)
      }
    ,pid_counter=0
    ,options=Options
    ,data_module = shr_utils:module(DataSpec)
    ,waiting_module = shr_utils:module(WaitSpec)
    ,test_gen_module = shr_utils:module(TestGenSpec)
    ,test_gen_state = shr_utils:initial_state(TestGenSpec,Options)
   },
  do_run(eqc_gen:pick(eqc_gen:choose(1,30)),State).

do_run(0,_State) ->
  [];
do_run(N,State) when is_integer(N), N>0 ->
  case eqc_gen:pick
    ((State#state.test_gen_module):command
       (State#state.test_gen_state,
	void)) of
    [] ->
      [];
    [PreCall] ->
      Command = shr_test_jobs:command_parser(PreCall),
      {F,Args} = Command#command.call,
      {Pid,State1} = new_pid(State),
      Call = {Command#command.port,F,Args},
      Job = #job{pid=Pid,call=Call,info=Command#command.options},
      OneState = shr_corr_resource:job_new_waiting(Job,State#state.state,State#state.waiting_module),
      ResultState = repeat_until_stable(State1#state{state=OneState}),
      FinishedJobs = job_minus(OneState#onestate.waiting,(ResultState#state.state)#onestate.waiting),
      NewTestGenState =
	(State#state.test_gen_module):next_state
	  (State#state.test_gen_state,
	   {[Job],FinishedJobs},
	   [PreCall],void),
      [Call|do_run(N-1,ResultState#state{test_gen_state=NewTestGenState})]
  end.

new_pid(State) ->
  PidCounter = State#state.pid_counter,
  {{pid,PidCounter}, State#state{pid_counter=PidCounter+1}}.

repeat_until_stable(State) ->
  DataModule = State#state.data_module,
  WaitingModule = State#state.waiting_module,
  IndState = State#state.state,
  case shr_corr_resource:executable_jobs(IndState#onestate.waiting,IndState,DataModule,WaitingModule,both) of
    [] ->
      State;
    Jobs ->
      {Job,_RestJobs} = pick(Jobs),
      Result = DataModule:return_value(Job#job.call,IndState#onestate.sdata),
      NextIndStates = 
	shr_corr_resource:job_next_states
	  (Job,Result,IndState,DataModule,WaitingModule,both),
      N = random:uniform(length(NextIndStates)),
      NextIndState = lists:nth(N,NextIndStates),
      repeat_until_stable(State#state{state=NextIndState})
  end.

pick(L) ->
  N = eqc_gen:pick(eqc_gen:choose(1,length(L))),
  {Pre,[Elem|Post]} = lists:split(N-1,L),
  {Elem,Pre++Post}.

job_minus(Jobs1,Jobs2) ->
  lists:filter
    (fun (Job) ->
	 lists:all(fun (OtherJob) -> Job#job.pid=/=OtherJob#job.pid end, Jobs2)
     end, Jobs1).






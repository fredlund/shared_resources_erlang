-module(robot_commands).

-include_lib("eqc/include/eqc.hrl").
-include("../../testing/src/tester.hrl").

%%-define(debug,true).
-include("../../src/debug.hrl").

-compile(export_all).

-record(teststate,
	{n_robots,n_naves,num_enters,blocked,corridors,warehouses,options,controller}).

-define(PESO_FACTOR,10).

-define(MAX_CONCURRENT,3).
-define(MAX_STATES,400).

init([NumRobots,NumNaves],Options) ->
  #teststate
    {
      n_robots=NumRobots,
      n_naves=NumNaves,
      options=Options,
      num_enters=0,
      blocked=[],
      corridors=lists:map(fun (I) -> {I,[]} end, lists:seq(0,NumNaves-1)),
      warehouses=lists:map(fun (I) -> {I,[]} end, lists:seq(0,NumNaves-1))
    }.

started(TS,Controller) ->
  TS#teststate{controller=Controller}.

command(TS,State) ->
  command(TS,State,0).
command(TS,State,NumConcurrent) ->
  ?LET
     (Command,
      job_cmd(TS,State),
      if
	NumConcurrent>=?MAX_CONCURRENT -> [];
	true ->
	  ?LET
	     (NextCommands,
	      eqc_gen:frequency
		([{3,[]},
		  {parfreq(State),
		   ?LAZY
		      (begin
			 TS1 = 
			   case calltype_in_call(Command) of
			     void ->
			       TS;
			     _ ->
			       TS0 =
				 case call_is_enter_in_warehouse0(Command) of
				   true -> add_enters(1,TS);
				   false -> TS
				 end,
			       add_to_blocked(robot_in_call(Command),TS0)
			   end,
			 command(TS1,State)
		       end)}]),
	      [Command|NextCommands])
      end).

parfreq(State) ->
  case proplists:get_value(no_par,State#state.options,false) of
    true -> 0;
    false ->
      case length(State#state.states)>=?MAX_STATES of
	true ->
	  io:format
	    ("*** Warning: cutting parallel test case due to too many states: ~p~n",
	     [length(State#state.states)]),
	  0;
	false ->
	  1
      end
  end.

job_cmd(TS,State) ->
  ?LOG("job_cmd: TS=~p~nState=~p~n",[TS,State]),
  Alternatives =
    [{?MODULE,enter,[num_enters(TS),0,peso()]} ||
      num_enters(TS) < TS#teststate.n_robots-1]
    ++
    [tester:make_void_call() ||
      num_enters(TS) >= TS#teststate.n_robots-1]
    ++
    [{?MODULE,enter,[R,N,peso(P)]} ||
      N <- corridors(TS),
      {R,P} <- corridor(N,TS),
      not(lists:member(R,blocked(TS)))
    ]
    ++
    [{?MODULE,exit,[R,N,P]} ||
      N <- warehouses(TS),
      {R,P} <- warehouse(N,TS),
      not(lists:member(R,blocked(TS)))
    ],
  ?LOG("Alternatives=~p~n",[Alternatives]),
  if
    Alternatives==[] ->
      io:format("No alternatives in state~n~p~n",[State]);
    true ->
      ok
  end,
  eqc_gen:oneof(Alternatives).

precondition(_State,TS,Commands) -> 
  do_preconditions(TS#teststate{blocked=[]},Commands).

do_preconditions(_TS,[]) ->
  true;
do_preconditions(TS,[Call|NextCalls]) ->
  Result =
    case Call of
      {_,enter,[R,0,_]} ->
	(not(lists:member(R,TS#teststate.blocked)))
	  andalso (num_enters(TS) < TS#teststate.n_robots)
	  andalso (R>=num_enters(TS));
      {_,enter,[R,N,P]} when N>0 ->
	(not(lists:member(R,TS#teststate.blocked)))
	  andalso case corridor(N,TS) of
		    [{R1,P1}] -> (R==R1) andalso (P>=P1);
		    _ -> false
		  end;
      {_,exit,[R,N,P]} ->
	(not(lists:member(R,TS#teststate.blocked)))
	  andalso lists:member({R,P},warehouse(N,TS))
    end,
  Result
    andalso do_preconditions(TS#teststate
			     {blocked=[robot_in_call(Call)|TS#teststate.blocked]},
			     NextCalls).

next_state(TS,_State,Result,_) ->
  {NewJobs,FinishedJobs} =
    Result,
  RemainingNewJobs = 
    tester:minus_jobs(NewJobs,FinishedJobs),
  NewJobsBlocked =
    lists:map(fun (Job) -> robot_in_call(Job#job.call) end, RemainingNewJobs),
  NewUnblocked =
    lists:map(fun (Job) -> robot_in_call(Job#job.call) end, FinishedJobs),
  NewBlocked =
    (TS#teststate.blocked ++ NewJobsBlocked) -- NewUnblocked,
  NewTS =
    lists:foldl
      (fun (Job,TSA) ->
	   Call = Job#job.call,
	   case {warehouse_in_call(Call),
		 calltype_in_call(Call)} of
	     {0,enter} ->
	       set_enters(max(num_enters(TSA),robot_in_call(Call)+1),TSA);
	     _ ->
	       TSA
	   end
       end,
       TS#teststate{blocked=NewBlocked},
       NewJobs),
  lists:foldl
    (fun (Job,TSA) ->
	 case Job#job.call of
	   {_,exit,[R,N,P]} ->
	     TSState1 = delete_from_warehouse({R,P},N,TSA),
	     NUM_NAVES = num_naves(TSA),
	     if
	       N==(NUM_NAVES-1) ->
		 TSState1;
	       true ->
		 add_to_corridor({R,P},N+1,TSState1)
	     end;
	   {_,enter,[R,N,P]} ->
	     TSState1 = add_to_warehouse({R,P},N,TSA),
	     if
	       N==0 ->
		 TSState1;
	       true ->
		 delete_from_corridor({R,P},N,TSState1)
	     end
	 end
     end, 
     NewTS,
     FinishedJobs).

num_enters(TS) ->
  TS#teststate.num_enters.

set_enters(N,TS) ->
  TS#teststate{num_enters=N}.

add_enters(N,TS) ->
  set_enters(num_enters(TS)+N,TS).

is_blocked(R,TS) ->
  lists:member(R,TS#teststate.blocked).

add_to_blocked(R,TS) ->
  OldUsed = TS#teststate.blocked,
  TS#teststate{blocked=[R|OldUsed]}.

robot_in_call(Call) ->
  case Call of
    {_,exit,[R,_,_]} -> R;
    {_,enter,[R,_,_]} -> R;
    {_,void,_} -> void
  end.

warehouse_in_call(Call) ->
  case Call of
    {_,exit,[_,N,_]} -> N;
    {_,enter,[_,N,_]} -> N
  end.
  
calltype_in_call(Call) ->
  case Call of
    {_,exit,_} -> exit;
    {_,enter,_} -> enter;
    {_,void,_} -> void
  end.

call_is_enter_in_warehouse0(Call) ->
  (calltype_in_call(Call)==enter)
    andalso (warehouse_in_call(Call)==0).

peso() ->
  ?LET(X,eqc_gen:choose(1,?PESO_FACTOR),X*100).

peso(P) ->
  Pdiv = P div 100,
  ?LET(X,eqc_gen:choose(Pdiv,?PESO_FACTOR),X*100).

corridors(TS) ->
  lists:seq(1,num_naves(TS)-1).

warehouses(TS) ->
  lists:seq(0,num_naves(TS)-1).

blocked(TS) ->
  TS#teststate.blocked.

corridor(N,TS) ->
  element(2,lists:nth(N+1,TS#teststate.corridors)).

warehouse(N,TS) ->
  element(2,lists:nth(N+1,TS#teststate.warehouses)).

delete_from_warehouse(Element,N,TS) ->
  TS#teststate
    {warehouses=
       lists:keystore
	 (N,
	  1,
	  TS#teststate.warehouses,
	  {N,lists:delete(Element,warehouse(N,TS))})}.

add_to_corridor(Element,N,TS) ->
  TS#teststate
    {corridors=lists:keystore(N,1,TS#teststate.corridors,{N,[Element]})}.
  
add_to_warehouse(Element,N,TS) ->
  TS#teststate
    {warehouses=
       lists:keystore
	 (N,
	  1,
	  TS#teststate.warehouses,
	  {N,lists:sort([Element|warehouse(N,TS)])})}.

delete_from_corridor(_Element,N,TS) ->
  TS#teststate
    {corridors=lists:keystore(N,1,TS#teststate.corridors,{N,[]})}.

num_naves(TS) ->
  TS#teststate.n_naves.

enter(_R,N,P) ->
  java:call(tester:get_data(controller),solicitarEntrar,[N,P]).

exit(_R,N,P) ->
  java:call(tester:get_data(controller),solicitarSalir,[N,P]).

start(NodeId,_TS) ->  
  case java:new(NodeId,'ControlAccesoNavesMonitor',[]) of
    Exc = {java_exception,_} -> 
      java:report_java_exception(Exc),
      throw(bad);
    Controller ->
      tester:store_data(controller,Controller),
      Controller
  end.

print_started_job_info(Job) ->
  {_,Name,[Robot,Warehouse,Weight]} = Job#job.call,
  io_lib:format("~p(~p,~p,~p)",[Name,Robot,Warehouse,Weight]).

print_finished_job_info(Job) ->
  print_job_info(Job).

print_job_info(Job) ->
  io_lib:format("~p",[robot_in_call(Job#job.call)]).

print_state(TS) ->
  lists:foldr
    (fun (I,Acc) ->  
	 Warehouse = warehouse(I,TS),
	 if
	   I == 0 ->
	     io_lib:format
	       ("w(0)={~s}; ",
		[print_warehouse(Warehouse)]);
	   true ->
	     Corridor = corridor(I,TS),
	     io_lib:format
	       ("corr(~p)={~s} w(~p)={~s}; ",
		[I,print_corridor(Corridor),I,print_warehouse(Warehouse)])
	 end++Acc
     end, "", lists:seq(0,TS#teststate.n_naves-1)).

print_corridor([]) -> "";
print_corridor([RobotPeso]) -> 
  print_robot_peso(RobotPeso).

print_warehouse([]) -> "";
print_warehouse([RobotPeso]) -> 
  print_robot_peso(RobotPeso);
print_warehouse([RobotPeso|Rest]) -> 
  print_robot_peso(RobotPeso)++","++print_warehouse(Rest).

print_robot_peso({Robot,Peso}) ->
  io_lib:format("~p:~p",[Robot,Peso]).

  
  

	 


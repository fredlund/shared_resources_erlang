-module(robot_commands).

-include_lib("eqc/include/eqc.hrl").
-include("../testing/src/tester.hrl").

%% -define(debug,true).
-include("../src/debug.hrl").

-compile(export_all).

-record(teststate,{n,num_enters,blocked,corridors,warehouses}).

-define(PESO_FACTOR,10).

init([N,NumNaves]) ->
  #teststate
    {
     n=N,
     num_enters=0,
     blocked=[],
     corridors=lists:map(fun (I) -> {I,[]} end, lists:seq(0,NumNaves-1)),
     warehouses=lists:map(fun (I) -> {I,[]} end, lists:seq(0,NumNaves-1))
    }.

command(TS,State) ->
  ?LET
     (Command,
      job_cmd(TS,State),
      ?LET
	 (NextCommands,
	  eqc_gen:frequency
	    ([{3,[]},
	      {1,
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
	  [Command|NextCommands])).

job_cmd(TS,State) ->
  Alternatives =
    [{?MODULE,enter,[num_enters(TS),0,peso()]} ||
      num_enters(TS) < TS#teststate.n-1]
    ++
    [{tester,void,[]} ||
      num_enters(TS) >= TS#teststate.n-1]
    ++
    [{?MODULE,enter,[R,N,peso(P)]} ||
      N <- corridors(State),
      {R,P} <- corridor(N,TS),
      not(lists:member(R,blocked(TS)))
    ]
    ++
    [{?MODULE,exit,[R,N,P]} ||
      N <- warehouses(State),
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

precondition(_State,TS,Call) -> 
  case Call of
    {_,do_cmds,Commands,_} ->
      Result = do_preconditions(TS#teststate{blocked=[]},Commands),
      io:format("pre(~p)=~p in ~p~n",[Call,Result,TS]),
      Result;
    _ ->
      true
  end.

do_preconditions(_TS,[]) ->
  true;
do_preconditions(TS,[Call|NextCalls]) ->
  Result =
    case Call of
      {_,enter,[R,0,_],_} ->
	(not(lists:member(R,TS#teststate.blocked)))
	  andalso (num_enters(TS) < TS#teststate.n)
	  andalso (R>=num_enters(TS));
      {_,enter,[R,N,P],_} when N>0 ->
	(not(lists:member(R,TS#teststate.blocked)))
	  andalso case corridor(N,TS) of
		    [{R1,P1}] -> (R==R1) andalso (P>=P1);
		    _ -> false
		  end;
      {_,exit,[R,N,P],_} ->
	(not(lists:member(R,TS#teststate.blocked)))
	  andalso lists:member({R,P},warehouse(N,TS))
    end,
  Result
    andalso do_preconditions(TS#teststate
			     {blocked=[robot_in_call(Call)|TS#teststate.blocked]},
			     NextCalls).

next_state(TS,State,Result,_) ->
  {NewJobs,FinishedJobsAndResults} =
    Result,
  FinishedJobs =
    lists:map(fun ({Job,_}) -> Job end,FinishedJobsAndResults),
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
	     State1 = delete_from_warehouse({R,P},N,TSA),
	     NUM_NAVES = num_naves(State),
	     if
	       N==(NUM_NAVES-1) ->
		 State1;
	       true ->
		 add_to_corridor({R,P},N+1,State1)
	     end;
	   {_,enter,[R,N,P]} ->
	     State1 = add_to_warehouse({R,P},N,TSA),
	     if
	       N==0 ->
		 State1;
	       true ->
		 delete_from_corridor({R,P},N,State1)
	     end;
	   _ -> State
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
    {_,enter,[R,_,_]} -> R
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

corridors(State) ->
  lists:seq(1,num_naves(State)-1).

warehouses(State) ->
  lists:seq(0,num_naves(State)-1).

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
	 (N+1,
	  1,
	  TS#teststate.warehouses,
	  {N+1,lists:delete(Element,warehouse(N,TS))})}.

add_to_corridor(Element,N,TS) ->
  TS#teststate
    {corridors=lists:keystore(N+1,1,TS#teststate.corridors,{N+1,[Element]})}.
  
add_to_warehouse(Element,N,TS) ->
  TS#teststate
    {warehouses=
       lists:keystore
	 (N+1,
	  1,
	  TS#teststate.warehouses,
	  {N+1,lists:sort([Element|warehouse(N,TS)])})}.

delete_from_corridor(_Element,N,TS) ->
  TS#teststate
    {corridors=lists:keystore(N+1,1,TS#teststate.corridors,[])}.

num_naves(State) ->
  OneState = hd(State#state.states),
  robots:num_naves(OneState#onestate.sdata).

enter(_R,N,P) ->
  java:call(tester:get_data(controller),solicitarEntrar,[N,P]).

exit(_R,N,P) ->
  java:call(tester:get_data(controller),solicitarSalir,[N,P]).

start(NodeId) ->  
  case java:new(NodeId,'ControlAccesoNavesMonitor',[]) of
    Exc = {java_exception,_} -> 
      java:report_java_exception(Exc),
      throw(bad);
    Controller ->
      tester:store_data(controller,Controller)
  end.

print_unblocked_job_info(Job) ->
  io_lib:format("~p",[robot_in_call(Job#job.call)]).
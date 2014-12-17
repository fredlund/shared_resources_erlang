-module(robot_commands).

-include_lib("eqc/include/eqc.hrl").
-include("../testing/src/tester.hrl").

-compile(export_all).

-record(teststate,{n,num_enters,used,corridors,warehouses}).

-define(PESO_FACTOR,10).

init([N]) ->
  #teststate
    {
     n=N,
     num_enters=0,
     used=[],
     corridors=lists:duplicate(N,[]),
     warehouses=lists:duplicate(N,[])
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
			   add_to_used(robot_in_call(Command),TS0)
		       end,
		     command(TS1,State)
		   end)}]),
	  [Command|NextCommands])).

job_cmd(TS,State) ->
  Alternatives =
    [{?MODULE,enter,[num_enters(State),0,peso()]} ||
      num_enters(State) < TS#teststate.n-1]
    ++
    [{tester,void,[]} ||
      num_enters(State) >= TS#teststate.n-1]
    ++
    [{?MODULE,enter,[R,N,peso(P)]} ||
      N <- corridors(State),
      {R,P} <- corridor(N,TS),
      not(lists:member(R,used(TS)))
    ]
    ++
    [{?MODULE,exit,[R,N,P]} ||
      N <- warehouses(State),
      {R,P} <- warehouse(N,TS),
      not(lists:member(R,used(TS)))
    ],
  io:format("Alternatives=~p~n",[Alternatives]),
  if
    Alternatives==[] ->
      io:format("No alternatives in state~n~p~n",[State]);
    true ->
      ok
  end,
  eqc_gen:oneof(Alternatives).

precondition(_State,TS,Call) -> 
  case Call of
    {_,_,do_job,[_,enter,[R,0,_]],_} ->
      (num_enters(TS) < TS#teststate.n)
	andalso (R>=num_enters(TS));
    {_,_,do_job,[_,enter,[R,N,P]],_} when N>0 ->
      case corridor(N,TS) of
	[{R1,P1}] -> (R==R1) andalso (P>=P1);
	_ -> false
      end;
    {_,_,do_job,[_,exit,[R,N,P]],_} ->
      lists:member({R,P},warehouse(N,TS));
    {_,_,void,_,_} ->
      true
  end.

next_state(TS,State,Result,_) ->
  {NewJobs,FinishedJobs} = Result,
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
       TS,
       NewJobs),
  lists:foldl
    (fun (Job,TSA) ->
	 case Job#job.call of
	   {_,exit,[R,N,P]} ->
	     State1 = delete_from_warehouse({R,P},N,TSA),
	     NUM_NAVES = robots:num_naves(State),
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

is_used(R,TS) ->
  lists:member(R,TS#teststate.used).

add_to_used(R,TS) ->
  OldUsed = TS#teststate.used,
  TS#teststate{used=[R|OldUsed]}.

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
  lists:seq(1,robots:num_naves(State)-1).

warehouses(State) ->
  lists:seq(0,robots:num_naves(State)-1).

used(TS) ->
  TS#teststate.used.

corridor(N,TS) ->
  lists:nth(N+1,TS#teststate.corridors).

warehouse(N,TS) ->
  lists:nth(N+1,TS#teststate.warehouses).

delete_from_warehouse(Element,N,TS) ->
  TS#teststate
    {warehouses=
       setelement
	 (N+1,
	  TS#teststate.warehouses,
	  lists:delete(Element,warehouse(N,TS)))}.

add_to_corridor(Element,N,TS) ->
  TS#teststate
    {corridors=setelement(N+1,TS#teststate.corridors,[Element])}.
  
add_to_warehouse(Element,N,TS) ->
  TS#teststate
    {warehouses=
       setelement
	 (N+1,
	  TS#teststate.warehouses,
	  lists:sort([Element|warehouse(N,TS)]))}.

delete_from_corridor(_Element,N,TS) ->
  TS#teststate
    {corridors=setelement(N+1,TS#teststate.corridors,[])}.



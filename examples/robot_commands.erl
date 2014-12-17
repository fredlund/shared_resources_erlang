-module(robot_commands).

-include_lib("eqc/include/eqc.hrl").
-include("../testing/src/tester.hrl").

-compile(export_all).

-record(genstate,{num_enters,used,corridors,warehouses}).

-define(PESO_FACTOR,10).

initial_state(State) ->
  IndState = one_state(State),
  N = robots:n(IndState#onestate.sdata),
  GenState = 
    #genstate
    {
    num_enters=0,
    used=[],
    corridors=lists:duplicate(N,[]),
    warehouses=lists:duplicate(N,[])
   },
  State#state{test_state=GenState}.

command(PreState) ->
  io:format("command(~p)~n",[PreState]),
  TestingState = PreState#state.test_state,
  State = PreState#state{test_state=TestingState#genstate{used=[]}},
  ?LET
     (Command,
      job_cmd(State),
      ?LET
	 (NextCommands,
	  eqc_gen:frequency
	    ([{3,[]},
	      {1,
	       ?LAZY
		  (begin
		     State1 = 
		       case calltype_in_call(Command) of
			 void -> State;
			 _ ->
			   State0 =
			     case call_is_enter_in_warehouse0(Command) of
			       true -> add_enters(1,State);
			       false -> State
			     end,
			   add_to_used(robot_in_call(Command),State0)
		       end,
		     command(State1)
		   end)}]),
	  [Command|NextCommands])).

job_cmd(State) ->
  io:format("used=~p~n",[used(State)]),
  ?LET(IndState,
       eqc_gen:oneof(State#state.states),
       begin
	 Alternatives =
	   [{?MODULE,enter,[num_enters(State),0,peso()]} ||
	     num_enters(State) < robots:n(IndState#onestate.sdata)-1]
	   ++
	   [{tester,void,[]} ||
	     num_enters(State) >= robots:n(IndState#onestate.sdata)-1]
	   ++
	   [{?MODULE,enter,[R,N,peso(P)]} ||
	     N <- corridors(IndState),
	     {R,P} <- corridor(N,IndState#onestate.sdata),
	     not(lists:member(R,used(State)))
	   ]
	   ++
	   [{?MODULE,exit,[R,N,P]} ||
	     N <- warehouses(IndState),
	     {R,P} <- warehouse(N,IndState#onestate.sdata),
	     not(lists:member(R,used(State)))
	   ],
	 io:format("Alternatives=~p~n",[Alternatives]),
	 if
	   Alternatives==[] ->
	     io:format("No alternatives in state~n~p~n",[State]);
	   true ->
	     ok
	 end,
	 eqc_gen:oneof(Alternatives)
       end).

precondition(State,Call) -> 
  lists:all
    (fun (IndState) -> precondition_ind(State,IndState,Call) end,
     State#state.states).

precondition_ind(State,IndState,Call) ->     
  case Call of
    {_,_,do_job,[_,enter,[R,0,_]],_} ->
      (num_enters(State) < (robots:n(IndState#onestate.sdata)-1))
	andalso (R>=num_enters(State));
    {_,_,do_job,[_,enter,[R,N,P]],_} when N>0 ->
      case corridor(N,IndState#onestate.sdata) of
	[{R1,P1}] -> (R==R1) andalso (P>=P1);
	_ -> false
      end;
    {_,_,do_job,[_,exit,[R,N,P]],_} ->
      lists:member({R,P},warehouse(N,IndState#onestate.sdata));
    {_,_,void,_,_} ->
      true
  end.

next_state(State,Result,_) ->
  {NewJobs,FinishedJobs} = Result,
  NewJobsTestState =
    lists:foldl
      (fun (Job,TS) ->
	   TS1 = 
	     case {warehouse_in_call(Job#job.call),
		   calltype_in_call(Job#job.call)} of
	       {0,enter} ->
		 set_enters(max(num_enters(State),robot_in_call(Call)+1),State);
	       _ ->
		 State
	     end
  end.

num_enters(State) ->
  (State#state.test_state)#genstate.num_enters.

set_enters(N,State) ->
  TestingState = State#state.test_state,
  State#state{test_state=TestingState#genstate{num_enters=N}}.

add_enters(N,State) ->
  set_enters(num_enters(State)+N,State).

is_used(R,State) ->
  lists:member
    (R,
     (State#state.test_state)#genstate.num_enters).

add_to_used(R,State) ->
  TestingState = State#state.test_state,
  OldUsed = TestingState#genstate.used,
  State#state{test_state=TestingState#genstate{used=[R|OldUsed]}}.

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

corridors(IndState) ->
  lists:seq(1,robots:num_naves(IndState#onestate.sdata)-1).

warehouses(IndState) ->
  lists:seq(0,robots:num_naves(IndState#onestate.sdata)-1).

used(State) ->
  TestState = State#state.test_state,
  TestState#genstate.used.

corridor(N,State) ->
  Result=lists:nth(N+1,State#robots.corridors),
  io:format("corridor(~p,~p) -> ~p~n",[N,State,Result]),
  Result.

warehouse(N,State) ->
  Result=lists:nth(N+1,State#robots.warehouses),
  io:format("warehouses(~p,~p) -> ~p~n",[N,State,Result]),
  Result.

one_state(State) ->
  hd(State#state.states).


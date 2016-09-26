-module(robots_physical).
-behaviour(gen_server).

-export([init/1,handle_call/3,terminate/2]). 
-export([handle_cast/2,handle_info/2,code_change/3]).
-export([start/1,start_link/1,enter/3,exit/3]).

-record(state,
	{
	  num_warehouses
	  ,weight_limit
	  ,warehouses
	  ,corridors
	  ,pending
	}).

-record(robot,
	{
	  id,
	  weight
	}).

-record(action,
	{
	  id,
	  weight,
	  limit,
	  from,
	  warehouse,
	  action_type
	}).

-define(MOVETIME,20).

%%-define(debug,true).
-include("../../src/debug.hrl").

init([Options]) ->
  NumWarehouses = proplists:get_value(num_warehouses,Options),
  {ok,
   #state
   {
     num_warehouses=NumWarehouses
     ,weight_limit=proplists:get_value(weight_limit,Options)
     ,warehouses=n_tuple(NumWarehouses,[])
     ,corridors=n_tuple(NumWarehouses,[])
     ,pending=[]
   }
  }.


handle_call({enter,[RobotId,Warehouse,Weight]},From,State)
  when is_integer(Warehouse), Warehouse>=0, Warehouse<State#state.warehouses ->
  ?TIMEDLOG
    ("handle_call(enter,~p,~p,~p)~n  state ~p~n",
     [RobotId,Warehouse,Weight,State]),
  {Robot,State1} =
    if
      Warehouse==0 ->
	not_present(RobotId,State),
	{#robot{id=RobotId,weight=Weight},State};
      true ->
	case get_robot_from_corridor(RobotId,Warehouse,State) of
	  Robot2 when is_record(Robot2,robot) ->
	    State2 = remove_from_corridor(RobotId,Warehouse,State),
	    {Robot2,State2};
	  _ ->
	    throw({robot_missing_from_corridor,RobotId,Warehouse})
	end
    end,
  assert(true, Weight >= Robot#robot.weight, decreasing_robot_weight),
  NewState = 
   add_to_pending(entering,Robot#robot{weight=Weight},Warehouse,From,State1),
  ?TIMEDLOG("handle_call => ~p~n",[NewState]),
  {noreply,NewState,?MOVETIME};

handle_call({exit,[RobotId,Warehouse,Weight]},From,State)
  when Warehouse>=0, Warehouse<State#state.warehouses ->
  ?TIMEDLOG
    ("handle_call(exit,~p,~p,~p)~n  state ~p~n",
     [RobotId,Warehouse,Weight,State]),
  case get_robot_from_warehouse(RobotId,Warehouse,State) of
    Robot when is_record(Robot,robot) ->
      assert(true, Weight >= Robot#robot.weight, decreasing_robot_weight),
      State1 = 
	remove_from_warehouse(RobotId,Warehouse,State),
      NewState = 
	add_to_pending
	  (exiting,Robot#robot{weight=Weight},Warehouse,From,State1),
      ?TIMEDLOG("handle_call => ~p~n",[NewState]),
      {noreply,NewState,?MOVETIME};
    _ ->
      exit({robot_missing_from_corridor,RobotId,Warehouse})
  end.

handle_info(timeout,State) ->
  ?TIMEDLOG
    ("handle_info(timeout)~n  state ~p~n",
     [State]),
  {EnabledActions,ResultState} = get_timeout_actions(State),
  FinalState = 
    lists:foldl
      (fun (Action,AccState) ->
	   Robot = #robot{id=Action#action.id,weight=Action#action.weight},
	   Warehouse = Action#action.warehouse,
	   NewState =
	     if
	       Action#action.action_type==entering ->
		 add_robot_to_warehouse(Robot,Warehouse,AccState);
	       true ->
		 if
		   Warehouse==State#state.num_warehouses-1 -> 
		     AccState;
		   true -> 
		     add_robot_to_corridor(Robot,Warehouse+1,AccState)
		 end
	     end,
	   gen_server:reply(Action#action.from,ok),
	   NewState
       end, ResultState, EnabledActions),
  ?TIMEDLOG("handle_info => ~p~n",[FinalState]),
  case get_new_timer(FinalState) of
    no ->
      {noreply,FinalState};
    Timer ->
      Timeout = timeStampToMilliSeconds(Timer),
      io:format("~p: set timer for ~p~n",[?MODULE,Timeout]),
      {noreply,FinalState,Timeout}
  end.

handle_cast(_,State) ->
  {noreply,State}.

code_change(_,State,_) ->
  {ok,State}.

terminate(_,_) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assert(Expected,Value,Exc) ->
  if
    Expected=/=Value ->
      Fault = {Exc,expected,Expected,got,Value},
      io:format("~n*** Error: physical fault: ~p~n",[Fault]),
      exit(Fault);
    true ->
      ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_timeout_actions(#state{}) -> {[#action{}],#state{}}.

get_timeout_actions(State) ->
  Now = os:timestamp(),
  lists:foldl
    (fun (Action,Acc={Enabled,AccState}) ->
	 case compareTimes_ge(Now, Action#action.limit) of
	   true -> 
	     NewState = remove_from_pending(Action#action.id,AccState),
	     {[Action|Enabled],NewState};
	   false -> Acc
	 end
     end, 
     {[],State},
     get_pending(State)).

-spec get_new_timer(#state{}) -> no | {non_neg_integer(),non_neg_integer(),non_neg_integer()}.

get_new_timer(State) ->
  lists:foldl
    (fun (Action,AccTimer) ->
	 case (AccTimer==no) 
	   orelse compareTimes_ge(AccTimer,Action#action.limit) of
	   true -> Action#action.limit;
	   false -> AccTimer
	 end
     end, no, get_pending(State)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_present(RobotId,State) ->
  assert(false,get_robot_from_pending(RobotId,State),robot_exists_in_pending),
  lists:foreach
    (fun (N) ->
	 assert
	   (false,get_robot_from_corridor(RobotId,N,State),
	    robot_exists_in_corridor),
	 assert
	   (false,get_robot_from_warehouse(RobotId,N,State),
	    robot_exists_in_warehouse)
     end, lists:seq(0,State#state.num_warehouses-1)).
		
get_robot_from_corridor(RobotId,Warehouse,State) ->
  lists:keyfind(RobotId,#robot.id,get_corridor(Warehouse,State)).

get_robot_from_warehouse(RobotId,Warehouse,State) ->
  lists:keyfind(RobotId,#robot.id,get_warehouse(Warehouse,State)).

get_robot_from_pending(RobotId,State) ->
  lists:keyfind(RobotId,#action.id,get_pending(State)).

remove_from_corridor(RobotId,Warehouse,State) ->
  set_corridor
    (lists:keydelete(RobotId,#robot.id,get_corridor(Warehouse,State)),
     Warehouse,
     State).

remove_from_warehouse(RobotId,Warehouse,State) ->
  set_warehouse
    (lists:keydelete(RobotId,#robot.id,get_warehouse(Warehouse,State)),
     Warehouse,
     State).

remove_from_pending(RobotId,State) ->
  set_pending
    (lists:keydelete(RobotId,#action.id,get_pending(State)),
     State).

add_robot_to_warehouse(Robot,Warehouse,State) ->
  Robots =
    get_warehouse(Warehouse,State),
  PreviousWeight =  
    lists:sum(lists:map(fun (R) -> R#robot.weight end, Robots)),
  TotalWeight =
    PreviousWeight + Robot#robot.weight,
  ?TIMEDLOG
    ("Total weight=~p Weight limit=~p~n",
     [TotalWeight,State#state.weight_limit]),
  assert(true,
	 TotalWeight =< State#state.weight_limit,
	 weight_limit_exceeded),
  set_warehouse([Robot|Robots],Warehouse,State).

add_robot_to_corridor(Robot,Warehouse,State) ->
  assert([],get_corridor(Warehouse,State),corridor_not_empty),
  set_corridor([Robot],Warehouse,State).

add_to_pending(Type,Robot,Warehouse,From,State) ->
  Pending = get_pending(State),
  
  %% Check that when exiting a warehouse there is no collision in
  %% the corridor (from another robot exiting, or from another robot
  %% entering the following warehouse).
  if
    Type==exiting, Warehouse==State#state.num_warehouses-1 ->
      true;
    Type==exiting ->
      assert([],
	     lists:filter
	       (fun (Action) ->
		    ((Action#action.action_type==exiting) 
		     andalso (Action#action.warehouse==Warehouse))
		      orelse
			((Action#action.action_type==entering) 
			 andalso (Action#action.warehouse==Warehouse+1))
		end, get_pending(State)),
	     {robot_collision,{Robot,Type,Warehouse,State#state.num_warehouses}});
    true -> 
      true
  end,
  Limit = addTimeStamps(os:timestamp(),milliSecondsToTimeStamp(?MOVETIME)),
  Action = 
    #action
    {id=Robot#robot.id,
     weight=Robot#robot.weight,
     warehouse=Warehouse,
     limit=Limit,
     from=From,
     action_type=Type},
  set_pending([Action|Pending],State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_warehouse(N,State) ->
  element(N+1,State#state.warehouses).

set_warehouse(Content,N,State) ->
  State#state{warehouses=setelement(N+1,State#state.warehouses,Content)}.

get_corridor(N,State) ->
  element(N+1,State#state.corridors).

set_corridor(Content,N,State) ->
  State#state{corridors=setelement(N+1,State#state.corridors,Content)}.

get_pending(State) ->
  State#state.pending.

set_pending(Content,State) ->
  State#state{pending=Content}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_tuple(N,Value) ->
  list_to_tuple(lists:duplicate(N,Value)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compareTimes_ge({M1, S1, Mic1}, {M2, S2, Mic2}) ->
  M1 > M2
    orelse M1 =:= M2 andalso S1 > S2
    orelse M1 =:= M2 andalso S1 =:= S2 andalso Mic1 >= Mic2.

milliSecondsToTimeStamp(MilliSeconds) ->
  Seconds = MilliSeconds div 1000,
  MegaSeconds = Seconds div 1000000,
  {MegaSeconds, Seconds rem 1000000, MilliSeconds rem 1000 * 1000}.

addTimeStamps({M1,S1,Mic1},{M2,S2,Mic2}) ->
  Mic=Mic1+Mic2,
  MicRem = Mic rem 1000000,
  MicDiv = Mic div 1000000,
  S = S1+S2+MicDiv,
  SRem = S rem 1000000,
  SDiv = S div 1000000,
  M = M1+M2+SDiv,
  {M,SRem,MicRem}.

timeStampToMilliSeconds({MegaSeconds,Seconds,MilliSeconds}) ->
  MegaSeconds * 1000000 * 1000000 + Seconds * 1000000 + MilliSeconds.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start([any()]) -> pid().

start(Options) ->
  do_start(Options,false).

-spec start_link([any()]) -> pid().

start_link(Options) ->
  do_start(Options,true).

do_start(Options,DoLink) ->
  SpawnFun = if DoLink -> start_link; true -> start end,
  gen_server:SpawnFun({local,physical},?MODULE,[Options],[]).

-spec enter(non_neg_integer(),non_neg_integer(),non_neg_integer()) -> any().

enter(RobotId,Warehouse,Weight) ->
  case whereis(physical) of
    Pid when is_pid(Pid) ->
      gen_server:call(Pid,{enter,RobotId,Warehouse,Weight})
  end.

-spec exit(non_neg_integer(),non_neg_integer(),non_neg_integer()) -> any().

exit(RobotId,Warehouse,Weight) ->
  case whereis(physical) of
    Pid when is_pid(Pid) ->
      gen_server:call(Pid,{exit,RobotId,Warehouse,Weight})
  end.




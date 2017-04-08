-module(gritter_gnr).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(shr_gnr_implementation).

%%-define(debug,true).
-include("../../src/debug.hrl").

-include("../../src/tester.hrl").

-compile(export_all).

-record(gstate,
	{
	  max_uids  %% number of uids to use
	  ,blocked   %% uids blocked (a set)
	  ,following %% which uids are followed by an uid (uid -> set(uid))
	  ,resends   %% uid accepts resends from uid ({uid,uid} -> bool)
	  ,num_msgs_to_read %% number of remaining messages to read for uid
	  ,msgs_present     %% messages remaining to read for uid
	  ,msgs_maybe_present %% messages possibly remaining to read for uid
	}).

initial_state(MaxUsers) -> 
  #gstate
    {
      max_uids=MaxUsers
      ,blocked=sets:new()
      ,following=[]
      ,resends=[]
      ,num_msgs_to_read=[]
      ,msgs_present=[]
      ,msgs_maybe_present=[]
    }.

nop_pre(State) ->
  length(alive(State))==0.
nop_args(_State) ->
  [].
nop() ->
  ok.
nop_next(State, _Var, _Args=[]) ->
  State.

seguir_pre(State) ->
  ?LOG("seguir_pre~n",[]),
  alive(State)=/=[].
seguir_args(State) ->
  ?LOG("seguir_args~n",[]),
  AliveUids = alive(State),
  ?LET(Uid1,my_oneof(AliveUids),
       ?LET(Uid2,frequency([{20,follow_uid(Uid1,State)},{1,Uid1}]),
	    [Uid1,Uid2,frequency([{7,true},{1,false}])])).
seguir_pre(State,[Uid1,_Uid2,_Resend]) ->
  ?LOG("seguir_pre(~p): blocked=~p~n",[Uid1,sets:to_list(State#gstate.blocked)]),
  lists:member(Uid1,alive(State)).
seguir(_Uid1,_Uid2,_Resend) ->
  ok.
seguir_next(State, _Var, _Args=[Uid1,Uid2,Resend]) ->
  ?LOG("seguir_next~n",[]),
  if
    Uid1==Uid2 -> State;
    true ->
      Following = sets:add_element(Uid2,following(Uid1,State)),
      FinalState =
	set_resends
	  (Uid1,Uid2,Resend,
	   set_following(Uid1,Following,State)),
      ?LOG
	 ("follow(~p) in~n~s~n==> ~s~n",
	  [_Args,print_state(State),print_state(FinalState)]),
      FinalState
  end.

follow_uid(FollowerUid,State) ->
  frequency
    (lists:map
       (fun (Uid) ->
	    case sets:is_element(Uid,following(FollowerUid,State)) of
	      true -> {1,Uid};
	      false -> {5,Uid}
	    end
	end, lists:delete(FollowerUid,uids(State)))).

dejarDeSeguir_pre(State) ->
  ?LOG("dejarDeSeguir_pre~n",[]),
  all_alive_following(State)=/=[].
dejarDeSeguir_args(State) ->
  ?LOG("dejarDeSeguir_args~n",[]),
  AllFollowing = all_alive_following(State),
  AliveUids = alive(State),
  Uids = uids(State),
  ?LET([Uid3,Uid4],
       frequency
	 ([{3,?LET({Uid1,Uid2},my_oneof(AllFollowing),[Uid1,Uid2])},
	   {1,[my_oneof(AliveUids),my_oneof(Uids)]}]),
       [Uid3,Uid4]).
dejarDeSeguir_pre(State,[Uid1,_]) ->
  ?LOG("dejarDeSeguir_pre(~p): blocked=~p~n",[Uid1,sets:to_list(State#gstate.blocked)]),
  lists:member(Uid1,alive(State)).
dejarDeSeguir(_Uid1,_Uid2) ->
  ok.
dejarDeSeguir_next(State, _Var, _Args=[Uid1,Uid2]) ->
  ?LOG("dejarDeSeguir_next~n",[]),
  Following = sets:del_element(Uid2,following(Uid1,State)),
  FinalState = set_following(Uid1,Following,State),
  ?LOG
    ("dejarDeSeguir(~p) in~n~s~n==> ~s~n",
     [_Args,print_state(State),print_state(FinalState)]),
  FinalState.

enviar_pre(State) ->
  ?LOG("enviar_pre~n",[]),
  (alive(State)=/=[]) andalso (sets:size(all_possible_sends(State))>0).
enviar_args(State) ->
  ?LOG("enviar_args~n",[]),
  ?LET(Uid, 
       frequency
	 (lists:map
	    (fun (Uid1) ->
		 case sets:size(followers(Uid1,State)) of
		   0 -> {1,Uid1};
		   _ ->
		     case lists:any
		       (fun (Uid2) -> sets:is_element(Uid2,State#gstate.blocked) end,
			sets:to_list(followers(Uid1,State))) of
		       true -> {20,Uid1};
		       false -> {10,Uid1}
		     end
		 end
	     end, alive(State))),
       ?LET(IsResending,bool(),
	    ?LET(Msg,
		 my_oneof(sets:to_list(possible_sends(State,Uid,IsResending))),
		 [Uid,Msg,IsResending]))).
enviar_pre(State,[Uid,Msg,Resending]) ->
  ?LOG("enviar_pre(~p): blocked=~p~n",[Uid,sets:to_list(State#gstate.blocked)]),
  lists:member(Uid,alive(State))
    andalso sets:is_element(Msg,possible_sends(State,Uid,Resending)).
enviar(_Uid,_Msg,_IsResending) ->
  ok.
enviar_next(State, _Var, _Args=[Uid,Msg,IsResending]) ->
  ?LOG("enviar_next~n",[]),
  FinalState =
    lists:foldl
      (fun ({Uid2,S},S1) ->
	   case sets:is_element(Uid,S) of
	     false -> S1;
	     true -> 
	       case resends(Uid2,Uid,State) of
		 false when IsResending -> S1;
		 _ ->
		   Present = msgs_present(Uid2,State),
		   case sets:is_element(Msg,msgs_maybe_present(Uid2,State)) of
		     true ->
		       io:format
			 ("*** Error: enviar(~p,~p) -- msg in maybe_present=~p"
			  ++" for following user ~p~n",
			  [Uid,Msg,Uid2,msgs_maybe_present(Uid2,State)]),
		       error(badarg);
		     false -> ok
		   end,
		   case sets:is_element(Msg,Present) of
		     true ->
		       S1;
		     false ->
		       S2 = set_msgs_present(Uid2,sets:add_element(Msg,Present),S1),
		       NumMsgsToRead = num_msgs_to_read(Uid2,S2),
		       ?LOG("NumMsgsToRead(~p) = ~p~n",[NumMsgsToRead,Uid2]),
		       S3 = set_num_msgs_to_read(Uid2,1+NumMsgsToRead,S2),
		       case sets:is_element(Uid2,blocked(S3)) of
			 true ->
			   S4 = set_blocked(sets:del_element(Uid2,blocked(S3)),S3),
			   do_read(Uid2,S4);
			 false ->
			   S3
		       end
		   end
	       end
	   end
       end, State, State#gstate.following),
  ?LOG
    ("enviar(~p) in~n~s~n==> ~s~n",
     [_Args,print_state(State),print_state(FinalState)]),
  FinalState.

possible_sends(State,Uid,Resending) ->
  PermittedMsgs =
    lists:foldl
      (fun ({Follower,S},Possible) ->
	   if
	     Uid==Follower ->
	       Possible;
	     true ->
	       case sets:is_element(Uid,S) of
		 false ->
		   Possible;
		 true ->
		   case not(Resending) orelse resends(Follower,Uid,State) of
		     true ->
		       PossiblyPresent = msgs_maybe_present(Follower,State),
		       sets:subtract(Possible,PossiblyPresent);
		     false ->
		       Possible
		   end
	       end
	   end
       end, sets:from_list(msgs()), State#gstate.following),
  PermittedMsgs.

all_possible_sends(State) ->
  lists:foldl
    (fun (Uid,S) ->
	 sets:intersection(S,possible_sends(State,Uid,false))
     end, sets:from_list(msgs()), uids(State)).

leer_pre(State) ->
  ?LOG("leer_pre~n",[]),
  alive(State)=/=[].
leer_args(State) ->
  ?LOG("leer_args~n",[]),
  ?LET(ReaderUid,
       frequency
	 (lists:map
	    (fun (Uid) ->
		 {case sets:size(following(Uid,State)) of 0 -> 1; _ -> 7 end, Uid}
	     end, alive(State))),
       [ReaderUid]).
leer_pre(State,[Uid]) ->
  ?LOG("leer_pre(~p): blocked=~p~n",[Uid,sets:to_list(State#gstate.blocked)]),
  lists:member(Uid,alive(State)).
leer(_Uid) ->
  ok.
leer_next(State, _Var, _Args=[Uid]) ->
  ?LOG
    ("leer_next(~p): num_msgs_to_read=~p blocked=~p must=~p may=~p~n",
     [Uid,num_msgs_to_read(Uid,State),sets:to_list(State#gstate.blocked),
      sets:to_list(msgs_present(Uid,State)),sets:to_list(msgs_maybe_present(Uid,State))]),
  FinalState =
    case num_msgs_to_read(Uid,State) of
      0 ->
	set_blocked(sets:add_element(Uid,State#gstate.blocked),State);
      N when N>0 -> 
	do_read(Uid,State)
    end,
  ?LOG
    ("leer(~p) in~n~s~n==> ~s~n",
     [_Args,print_state(State),print_state(FinalState)]),
  FinalState.

do_read(Uid,State) ->
  N = num_msgs_to_read(Uid,State),
  S1 = set_num_msgs_to_read(Uid,N-1,State),
  case {sets:size(msgs_present(Uid,S1)),
	sets:size(msgs_maybe_present(Uid,S1))} of
    {0,0} ->
      io:format
	("*** Error in do_read: msgs_present=0 msgs_maybe_present=0~n"++
	   "num_msgs_to_read=~p~n",[N]),
      error(badarg);
    {0,1} -> 
      set_msgs_maybe_present(Uid,sets:new(),S1);
    {0,_} ->
      S1;
    {1,0} ->
      set_msgs_present(Uid,sets:new(),S1);
    {_,_} ->
      Present = msgs_present(Uid,S1),
      MaybePresent = msgs_maybe_present(Uid,S1),
      S2 = set_msgs_present(Uid,sets:new(),S1),
      set_msgs_maybe_present(Uid,sets:union(Present,MaybePresent),S2)
  end.

weight(_State, seguir) -> 7;
weight(_State, dejarDeSeguir) -> 1;
weight(_State, enviar) -> 5;
weight(State, leer) -> 
  case lists:any(fun ({_Uid,S}) -> sets:size(S) > 0 end, State#gstate.following) of
    true -> 3;
    false -> 1
  end;
weight(_State, _) -> 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_state(MaxUsers,_) ->
  initial_state(MaxUsers).

command(State,_CorrState) ->
  ?LET(Command,command(State),
       begin
	 ?LOG("command/2 generated ~p~n",[Command]),
	 {call,_,F,Args} = Command,
	 case F of
	   nop -> [];
	   _ -> case Args of
		  [Element|_] ->
		    [{Element,{F,Args}}];
		  _ ->
		    io:format("*** Error: malformed command ~p~n",[Command]),
		    error(badarg)
		end
	 end
       end).

next_state(State,Result,Commands,_CorrState) ->
  ?LOG("Commands are ~p~n",[Commands]),
  case Commands of
    [] ->
      State;
    [{_,{F,Args}}] ->
      next_state(State,Result,{call,?MODULE,F,Args})
  end.

precondition(State,Commands,_CorrState) ->
  ?LOG("gritter_gnr:precondition: Commands=~p~n",[Commands]),
  Result =
    case Commands of
      [] ->
	true;
      [{_,{F,Args}}] ->
	Call = {call,?MODULE,F,Args},
	precondition(State,Call)
    end,
  ?LOG("gritter_gnr: precondition returns ~p~n",[Result]),
  Result.

readable(State) ->
  lists:foldl
    (fun (Uid,Acc) ->
	 case num_msgs_to_read(Uid,State)>0 of
	   true -> [Uid|Acc];
	   false -> Acc
	 end
     end, [], alive(State)).

msgs() ->
  ["m1","m2","m3","m4"].

all_alive_following(State) ->
  lists:flatmap
    (fun ({Uid,S}) ->
	 case sets:is_element(Uid,State#gstate.blocked) of
	   true -> [];
	   false -> lists:map(fun (Uid2) -> {Uid,Uid2} end,sets:to_list(S))
	 end
     end,
     State#gstate.following).

alive(State) ->
  Result =
    sets:to_list
      (sets:subtract
	 (sets:from_list(lists:seq(1,State#gstate.max_uids)),
	  State#gstate.blocked)),
  ?LOG
    ("alive(~p) returns ~p~n",
     [sets:to_list(State#gstate.blocked),Result]),
  Result.

uids(State) ->
  lists:seq(1,State#gstate.max_uids).

set_following(Uid,Following,State) ->
  State#gstate
    {following=lists:keystore(Uid, 1, State#gstate.following, {Uid,Following})}.

following(Id,State) ->
  case lists:keyfind(Id,1,State#gstate.following) of
    false -> sets:new();
    {_,Set} -> Set
  end.
  
followers(Uid1,State) ->
  lists:foldl
    (fun ({Uid2,S},Set) ->
	 case sets:is_element(Uid1,S) of
	   true ->
	     sets:add_element(Uid2,Set);
	   false ->
	     Set
	 end
     end, sets:new(), State#gstate.following).  

set_resends(Uid1,Uid2,Resends,State) ->
  Key = {Uid1,Uid2},
  State#gstate
    {resends=lists:keystore(Key, 1, State#gstate.resends, {Key,Resends})}.

resends(Uid1,Uid2,State) ->
  {_,Value} = lists:keyfind({Uid1,Uid2},1,State#gstate.resends),
  Value.

msgs_maybe_present(Uid,State) ->
  case lists:keyfind(Uid,1,State#gstate.msgs_maybe_present) of
    false -> sets:new();
    {_,Value} -> Value
  end.

set_msgs_maybe_present(Uid,Msgs,State) ->
  State#gstate
    {msgs_maybe_present=lists:keystore(Uid, 1, State#gstate.msgs_maybe_present, {Uid,Msgs})}.

msgs_present(Uid,State) ->
  case lists:keyfind(Uid,1,State#gstate.msgs_present) of
    false -> sets:new();
    {_,Value} -> Value
  end.

set_msgs_present(Uid,Msgs,State) ->
  State#gstate
    {msgs_present=lists:keystore(Uid, 1, State#gstate.msgs_present, {Uid,Msgs})}.

num_msgs_to_read(Uid,State) ->
  case lists:keyfind(Uid,1,State#gstate.num_msgs_to_read) of
    false -> 0;
    {_,Value} -> Value
  end.

set_num_msgs_to_read(Uid,N,State) ->
  State#gstate
    {num_msgs_to_read=lists:keystore(Uid, 1, State#gstate.num_msgs_to_read, {Uid,N})}.
blocked(State) ->
  State#gstate.blocked.

set_blocked(Blocked,State) ->
  State#gstate{blocked=Blocked}.

sample() ->
  eqc_gen:sample(eqc_statem:commands(?MODULE)).

print_finished_job_info(Job,_TS) ->
  {_,_,Args} = Job#job.call,
  io_lib:format("~p",[hd(Args)]).

print_started_job_info(Job,_TS) ->
  {_,F,Args} = Job#job.call,
  shr_utils:print_mfa({F,Args}).

print_state(#gstate{max_uids=MaxUids,following=Following,resends=Resends,num_msgs_to_read=NumMsgsToRead,msgs_present=MsgsPresent,msgs_maybe_present=MsgsMaybePresent,blocked=Blocked}) ->
  io_lib:format
    ("{max_uids=~p,blocked=~s,following=~s,resends=~s,num_msgs_to_read=~s,msgs_present=~s,msgs_maybe_present=~s}~n",
     [
      MaxUids,
      io_lib:format("~p",[sets:to_list(Blocked)]),
      lists:foldl
      (fun ({Uid,S},Acc) ->
	   io_lib:format("{~p,~p},~s",[Uid,sets:to_list(S),Acc])
       end, "", Following),
      lists:foldl
      (fun ({Key,Value},Acc) ->
	   io_lib:format("{~p,~p},~s",[Key,Value,Acc])
       end, "", Resends),
      lists:foldl
	(fun ({Key,Count},Acc) ->
	     io_lib:format("{~p,~p),~s",[Key,Count,Acc])
	 end, "", NumMsgsToRead),
      lists:foldl
      (fun ({Uid,S},Acc) ->
	   io_lib:format("{~p,~p},~s",[Uid,sets:to_list(S),Acc])
       end, "", MsgsPresent),
      lists:foldl
      (fun ({Uid,S},Acc) ->
	   io_lib:format("{~p,~p},~s",[Uid,sets:to_list(S),Acc])
       end, "", MsgsMaybePresent)
     ]).

my_oneof([]) ->
  io:format
    ("oneof([]) - stacktrace:~n~p~n",
     [try throw(trace) catch _:_ -> erlang:get_stacktrace() end]),
  error(badarg);
my_oneof(Other) ->
  eqc_gen:oneof(Other).


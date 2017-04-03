%% @doc This module provides a simple supervisor behaviour,
%% where a set of processes are started, and if any process
%% terminates, the supervisor itself fails. That is,
%% processes are not restarted upon failure.
%%
%% A difference to the normal supervisor behaviour is that
%% the behaviour permits adding existing processes to the supervisor
%% tree in a simple manner.
%%
%% A limitation is that currently only one shr_supervisor process
%% can be active at any time; this is quite easy to fix.

-module(shr_supervisor).

-export([init/1,handle_call/3,handle_info/2,handle_cast/2,code_change/3,terminate/2]).
-export([ensure_started/1,add_childfun/1,add_childfun/2,add_childproc/1,add_childproc/2,restart/1,is_alive/0]).

-record(state,{processes,terminated,timeout,reportTo}).
-record(process,{pid,name}).

-behaviour(gen_server).

%%-define(debug,true).
-include("debug.hrl").

-type supervisor_ret() ::  {ok,[pid()],any()} 
			 | {ok,pid(),any()}
			 | {ok,[pid()]}
			 | {ok,pid()}.

-define(GEN_SERVER,gen_server).

init(_) ->
  process_flag(trap_exit,true),
  {ok,#state{processes=[],timeout=void}}.

handle_call(Msg,From,State) ->
  ?TIMEDLOG("got ~p~n",[Msg]),
  Result = handle_call1(Msg,From,State),
  ?TIMEDLOG("responding ~p to ~p~n",[Result,Msg]),
  Result.

handle_call1({addfun,Name,Spec},_From,State) ->
  Pid = spawn_link(Spec),
  Process = #process{pid=Pid,name=Name},
  if
    Name =/= [] -> shr_register:register(Name,Pid);
    true -> ok
  end,
  {reply, Pid, State#state{processes=[Process|State#state.processes],terminated=[]}};
handle_call1({addproc,Names,Spec},_From,State) ->
  Result =
    case Spec of
      Fun when is_function(Fun) -> Fun();
      {M,F,Args} -> apply(M,F,Args)
    end,
  StrippedInfo = 
    case Result of 
      {ok,Ch,_Info} when is_list(Ch) -> {ok,Ch};
      {ok,Ch,_Info} -> {ok,[Ch]};
      {ok,Ch} when is_list(Ch) -> {ok,Ch};
      {ok,Ch} -> {ok,[Ch]};
      _ -> Result
    end,
  case StrippedInfo of
    {ok,Children} ->
      Processes = 
	lists:map
	  (fun (Child) -> 
	       #process{pid=Child}
	   end, Children),
      RegisterNames =
	if
	  is_list(Names) -> Names;
	  true -> [Names]
	end,
      if
	Names=/=[] ->
	  lists:foreach
	    (fun ({Name,Child}) ->
		 shr_register:register(Name,Child)
	     end, lists:zip(RegisterNames,Children));
	true -> 
	  ok
      end,
      {reply, Children,
       State#state{processes=Processes++State#state.processes,terminated=[]}};
    Other ->
      io:format
	("*** Error: ~p(~p) adding process from recipe ~p (with names ~p) "
	 ++"returns an incorrect value~n~p~n",
	 [?MODULE,self(),Spec,Names,Other]),
      {reply, bad, State}
  end;
handle_call1({restart,ReportTo},_From,State) ->
  NewState = do_restart(State,ReportTo),
  timer:sleep(100),
  {reply, ok, NewState};
handle_call1(is_alive,_From,State) ->
  {reply, State#state.processes =/= [], State}.

handle_info(timeout,State) ->
  ?TIMEDLOG("Got timeout~n",[]),
  {noreply, do_kill(State)};
handle_info({'EXIT',_Pid,normal},State) ->
  ?TIMEDLOG("Got normal exit for ~p~n",[_Pid]),
  {noreply, State};
handle_info(Msg={'EXIT',Pid,Reason},State) ->
  ?TIMEDLOG("Handling ~p~n",[Msg]),
  case find_process(Pid, State#state.processes) of
    [P] ->
	if 
	  Reason==noproc ->
	    io:format
	      ("*** Error: ~p(~p) got noproc ~p~n",
	       [?MODULE,self(),Msg]),
	    update_timeout(State);

	  Reason=/=killed, Reason=/=normal ->
	    io:format
	      ("~p(~p): supervised process ~p(~p) exited because of~n~p~n",
	       [?MODULE,self(),P#process.pid,P#process.name,Reason]),
	    inform_about_exit(Pid,Reason,State),
	    do_kill(State),
	    {stop,Reason,State};

	  true -> update_timeout(State)
	end;
    [] ->
      case find_process(Pid, State#state.terminated) of
	[] ->
	  ?TIMEDLOG
	     ("*** Warning. Got exit for non-supervised process ~p~n",
	      [Pid]);
	_ -> ok
      end,
      update_timeout(State)
  end.

handle_cast(_,State) ->
  {noreply,State}.

code_change(_,State,_) ->
  {ok,State}.

inform_about_exit(Pid,Reason,#state{reportTo=ReportTo}) ->
  if
    is_pid(ReportTo) ->
      ReportTo ! {'EXIT',Pid,Reason};
    is_atom(ReportTo) ->
      ReportTo ! {'EXIT',Pid,Reason};
    true ->
      ok
  end.

set_timeout(State) ->
  if
    State#state.timeout == void ->
      Timestamp = timestamp(),
      {noreply, State#state{timeout=Timestamp+10}, 10};
    true ->
      update_timeout(State)
  end.

update_timeout(State) ->
  if
    State#state.timeout =/= void ->
      Timestamp = timestamp(),
      NewTimeout = max(State#state.timeout-Timestamp,0),
      {noreply, State, NewTimeout};
    true ->
      {noreply, State}
  end.

terminate(_Reason, State) ->
  do_kill(State),
  ?TIMEDLOG("terminating due to ~p~n",[_Reason]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_process(Pid,Processes) ->
  lists:filter(fun (P) -> P#process.pid==Pid end, Processes).

start_server() ->
  ?TIMEDLOG("starting server~n",[]),
  Parent = self(),
  spawn_link
    (fun () ->
	 Result = ?GEN_SERVER:start({local,?MODULE}, ?MODULE, void, []),
	 Parent!{?MODULE,Result}
     end),
  receive
    {?MODULE,{ok,Pid}} when is_pid(Pid) ->
      ok;
    {?MODULE,Other} ->
      io:format
	("*** Error: could not start ~p due to ~p~n",
	 [?MODULE,Other]),
      exit(?MODULE)
  end. 

ensure_started(ReportTo) ->
  case whereis(?MODULE) of
    undefined ->
      start_server();
    Pid when is_pid(Pid) ->
      ok
  end.

do_restart(State,ReportTo) ->
  (do_kill(State))#state{reportTo=ReportTo}.

do_kill(State) ->
  ?TIMEDLOG("Will kill remaining processes ~p~n",[State#state.processes]),
  lists:foreach
    (fun (P) ->
	 try unlink(P#process.pid) catch _:_ -> ok end,
	 exit(P#process.pid,kill)
     end, 
     State#state.processes),
  State#state{processes=[], terminated=State#state.processes, timeout=void}.

%% @doc Spawns a process executing the argument function, 
%% and starts supervising it.
-spec add_childfun(fun(() -> any())) -> any().
add_childfun(Spec) ->
  ?GEN_SERVER:call(?MODULE,{addfun,[],Spec}).
%% @doc Spawns a process executing the argument function, 
%% with the name provided by the name argument,
%% and starts supervising it.
-spec add_childfun(atom(),fun(() -> any())) -> any().
add_childfun(Name,Spec) ->
  ?GEN_SERVER:call(?MODULE,{addfun,Name,Spec}).

%% @doc Executes the function argument (either an anynomous function, or a 
%% {Module,FunAtom,Args} triple), which should return a list of pids to 
%% supervisor (enclosed by an ok atom, and possibly with a third field).
%% The pids returned are included in the supervision tree.
-spec add_childproc(fun(() -> supervisor_ret()) | {atom(),atom(),[any()]}) -> any().
add_childproc(Spec) ->
  ?GEN_SERVER:call(?MODULE,{addproc,[],Spec}).
-spec add_childproc(atom(),fun(() -> supervisor_ret()) | {atom(),atom(),[any()]}) -> any().
%% @doc Executes the function argument (either an anynomous function, or a 
%% {Module,FunAtom,Args} triple), which should return a list of pids to 
%% supervisor (enclosed by an ok atom, and possibly with a third field).
%% The pids returned are included in the supervision tree. The name argument 
%% gives a symbolic name to the new supervised pids.
add_childproc(Name,Spec) ->
  ?GEN_SERVER:call(?MODULE,{addproc,Name,Spec}).

%% @doc Starts, or restarts if running, a shr_supervisor.
%% The ``ReportTo'' argument is a pid to which the supervisor sends
%% a failure report, if a supervisor process fails.
%%
%% Internally the supervisor is implemented as a gen_server. However,
%% note that the gen_server is started from a fresh process to ensure
%% that the gen_server is not shutdown when the parent process
%% terminates. The moment when the parent process terminates
%% is not so well-defined for e.g. QuickCheck tests.
%%
-spec restart(pid()) -> ok.
restart(ReportTo) ->  
  ?TIMEDLOG("restart(~p)~n",[ReportTo]),
  case whereis(?MODULE) of
    undefined ->
      start_server();
    _ ->
      ?GEN_SERVER:call(?MODULE,{restart,ReportTo})
  end.

%% @doc Checks if the shr_supervisor is alive.
-spec is_alive() -> boolean().
is_alive() ->
  ?GEN_SERVER:call(?MODULE,is_alive).

timestamp() ->
  {MegaSecs,Secs,MicroSecs} = os:timestamp(),
  (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

-module(shr_register).

-export([start/0,start_link/0,register/2,unregister/1,whereis/1]).

-export([init/1,handle_call/3,terminate/2]). 
-export([handle_cast/2,handle_info/2,code_change/3]).

%%-define(debug,true).
-include("debug.hrl").

init(_) -> {ok,[]}.

start() ->
  Result = gen_server:start({local,?MODULE},?MODULE,[],[]),
  case Result of
    {ok,_} -> ok;
    Other -> 
      io:format
	("*** Error: shr_register:start_link failed with ~p~n",
	 [Other])
  end,
  Result.

start_link() ->
  Result = gen_server:start_link({local,?MODULE},?MODULE,[],[]),
  case Result of
    {ok,_} -> ok;
    Other -> 
      io:format
	("*** Error: shr_register:start_link failed with ~p~n",
	 [Other])
  end,
  Result.

handle_call({register,Name,Pid},_From,State) ->
  case is_process_alive(Pid) of
    false -> 
      io:format
	 ("*** WARNING: register: pid ~p is not alive when registering name ~p~n",
	  [Pid,Name]),
      {reply, {exception,badarg}, State};
    true ->
      case lists:keyfind(Name,1,State) of
	T when is_tuple(T) -> 
	  io:format
	     ("*** WARNING: register: name ~p is already registered in ~p; new pid ~p~n",
	      [Name,T,Pid]),
	  {reply, {exception,badarg}, State};
	false ->
	  NewState = lists:keystore(Name, 1, State, {Name,Pid}),
	  monitor(process, Pid),
	  ?TIMEDLOG("registered ~p as ~p~n",[Pid,Name]),
	  {reply, true, NewState}
      end
  end;
handle_call({unregister,Name},_From,State) ->
  case lists:keyfind(Name,1,State) of
    false -> 
      io:format
	("*** WARNING: unregister: name ~p does not exist~n",
	 [Name]),
      {reply, {exception,badarg}, State};
    T when is_tuple(T) -> 
      {reply, true, lists:keydelete(Name,1,State)}
  end;
handle_call({whereis,Name},_From,State) ->
  case lists:keyfind(Name,1,State) of
    false ->
      ?TIMEDLOG("~p is not registered~n",[Name]),
      {reply, undefined, State};
    {_,Pid} ->
      ?TIMEDLOG("~p is at ~p~n",[Name,Pid]),
      {reply, Pid, State}
  end.

handle_info({'DOWN',_,_,Pid,_Reason},State) ->
  ?TIMEDLOG
    ("handle_info: process ~p died due to reason ~p (table index ~p) ~n",
     [Pid,_Reason,lists:keyfind(Pid,2,State)]),
  {noreply,lists:keydelete(Pid,2,State)};
handle_info(_,State) ->
  State.

handle_cast(_,State) ->
  {noreply,State}.

code_change(_,State,_) ->
  {ok,State}.

terminate(_,_) ->
  ok.
	      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

register(Name,Pid) when is_pid(Pid) ->	   
  interpret_result(gen_server:call(?MODULE,{register,Name,Pid}));
register(_,_) -> 
  interpret_result({exception, badarg}).

unregister(Name) ->
  interpret_result(gen_server:call(?MODULE,{unregister,Name})).

whereis(Name) ->
  interpret_result(gen_server:call(?MODULE,{whereis,Name})).

interpret_result({exception, Reason}) ->
  error(Reason);
interpret_result(Other) ->
  Other.










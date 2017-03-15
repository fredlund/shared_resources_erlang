-module(shr_register).

-export([start/0,start_link/0,register/2,unregister/1,whereis/1]).

-export([init/1,handle_call/3,terminate/2]). 
-export([handle_cast/2,handle_info/2,code_change/3]).

%%-define(debug,true).
-include("debug.hrl").

init(_) -> {ok,[]}.

start() ->
  gen_server:start({local,?MODULE},?MODULE,[],[]).

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_call({register,Name,Pid},_From,State) ->
  case is_process_alive(Pid) of
    false -> 
      ?TIMEDLOG
	 ("register: pid ~p is not alive when registering name ~p~n",
	  [Pid,Name]),
      {reply, {exception,badarg}, State};
    true ->
      case lists:keyfind(Name,1,State) of
	T when is_tuple(T) -> 
	  ?TIMEDLOG
	     ("register: name ~p is already registered in ~p; new pid ~p~n",
	      [Name,T,Pid]),
	  {reply, {exception,badarg}, State};
	false ->
	  case lists:keyfind(Pid,2,State) of
	    T when is_tuple(T) -> 
	      ?TIMEDLOG
		 ("register: pid ~p is already registered in ~p; new name ~p~n",
		  [Pid,T,Name]),
	      {reply, {exception,badarg}, State};
	    false ->
	      NewState = lists:keystore(Name, 1, State, {Name,Pid}),
	      monitor(process, Pid),
	      ?TIMEDLOG("registered ~p as ~p~n",[Pid,Name]),
	      {reply, true, NewState}
	  end
      end
  end;
handle_call({unregister,Name},_From,State) ->
  case lists:keyfind(Name,1,State) of
    false -> 
      ?TIMEDLOG("unregister: name ~p does not exist~n",[Name]),
      {reply, {exception,badarg}, State};
    T when is_tuple(T) -> 
      {reply, true, lists:keydelete(Name,1,State)}
  end;
handle_call({whereis,Name},_From,State) ->
  case lists:keyfind(Name,1,State) of
    false ->
      {reply, undefined, State};
    {_,Pid} ->
      {reply, Pid, State}
  end.

handle_info({'DOWN',_,_,Pid,_},State) ->
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










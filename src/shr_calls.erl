%% @doc This module implements remote calls without timeouts, using the same
%% calling conventions as gen_servers, so that processes can call both
%% gen_servers and processes implemented using this library without
%% changing any code.
%% @author Lars-Ake Fredlund (lfredlund@fi.upm.es)
%% @copyright 2016 Lars-Ake Fredlund
%%

-module(shr_calls).

%%-define(debug,true).
-include("debug.hrl").

-define(TAG,'$gen_call').

-export([is_call/1,call/2,msg/1,reply/2]).

-type procname() :: pid() | atom() | {atom(),atom()}.
-type message() :: {?TAG, {pid(),reference()}, any()}.

%% @doc Calls a process (either a gen_server or a process using the calling
%% discipline of this module), and waits for a reply.
-spec call(procname(),any()) -> any().
call(Pid,Msg) ->
  ?TIMEDLOG
    ("call(~p,~p)~n",
     [Pid,Msg]),
  Reference = erlang:make_ref(),
  Pid!{?TAG,{self(),Reference},Msg},
  receive
    {Reference,Value} ->
      ?TIMEDLOG
	 ("call(~p,~p) ==> ~p~n",
	  [Pid,Msg,Value]),
      Value
  end.

%% @doc Retrieves the message from a communication sent using the ``call/2''
%% function (or a gen_server call).
-spec msg(message()) -> any().
msg({?TAG,{_Pid,_Ref},Msg}) ->
  Msg.

%% @doc Sends a reply (the value in the second argument)
%% to the message first argument.
-spec reply(message(),any()) -> any().
reply({?TAG,{Pid,Ref},_Msg},Value) ->
  Pid!{Ref,Value}.

%% @doc Returns true if the argument has been sent using call/2
%% (or a gen_server call).
-spec is_call(any()) -> bool().
is_call({?TAG,{_Pid,_Ref},__Msg}) ->
  true;
is_call(_) ->
  false.


%% Remote calls without timeouts.
%% Mimics the calling conventions of gen_servers
%% in order that call/2 can be used to call a gen_server too.

-module(shr_calls).

%%-define(debug,true).
-include("debug.hrl").

-define(TAG,'$gen_call').

-export([is_call/1,call/2,msg/1,reply/2]).

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

msg({?TAG,{_Pid,_Ref},Msg}) ->
  Msg.

reply({?TAG,{Pid,Ref},_Msg},Value) ->
  Pid!{Ref,Value}.

is_call({?TAG,{_Pid,_Ref},__Msg}) ->
  true;
is_call(_) ->
  false.


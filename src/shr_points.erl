-module(shr_points).

-export([open_endpoint/1,open_endpoint/2,open_endpoint/3,connect/2]).
-export([endpoint/2,endpoint/3,endpoint/4]).

-define(debug,true).
-include("debug.hrl").

endpoint(Name,P) ->
  connect(open_endpoint(Name),P).
endpoint(Name,P,SpawnOptions) ->
  connect(open_endpoint(Name,SpawnOptions),P).
endpoint(Name,Node,P,SpawnOptions) ->
  connect(open_endpoint(Name,Node,SpawnOptions),P).

open_endpoint(Name) ->
  open_endpoint(Name,[]).
open_endpoint(Name,SpawnOptions) ->
  spawn_opt(fun () -> wait_connect_endpoint(Name) end, SpawnOptions).
open_endpoint(Name,Node,SpawnOptions) ->
  spawn_opt(Node,fun () -> wait_connect_endpoint(Name) end, SpawnOptions).

wait_connect_endpoint(Name) ->
  receive
    {endpoint_connect,P} ->
      ?LOG("connect: ~p",[P]),
      check_valid(P),
      do_endpoint(Name,[P])
  end.

do_endpoint(Name,Ps) ->
  receive
    {endpoint_connect,P} ->
      ?LOG("connect: ~p",[P]),
      check_valid(P),
      do_endpoint(Name,[P|Ps]);
    _Msg = {from_endpoint,Name1,Msg,Info} ->
      ?LOG("got ~p~n",[_Msg]),
      OldPath = proplists:get_value(path,Info,[]),
      %% Inefficient: a map or a normal key-value list is better
      NewInfo = [{path,[Name1|OldPath]}|Info],
      lists:foreach(fun (P) -> comm(Name,P,Msg,NewInfo) end, Ps),
      do_endpoint(Name,Ps);
    Msg ->
      ?LOG("got ~p~n",[Msg]),
      lists:foreach(fun (P) -> comm(Name,P,Msg) end, Ps),
      do_endpoint(Name,Ps)
  end.

connect(E,P) ->
  E!{endpoint_connect,P},
  E.

check_valid(P) ->
  case P of
    _ when is_pid(P) -> true;
    _ when is_atom(P) -> true;
    {RegName,Node} when is_atom(RegName), is_atom(Node) -> true;
    _ ->
      io:format
	("*** Error: invalid endpoint ~p~n",
	 [P]),
      error(badarg)
  end.

comm(Name,P,Msg) ->
  comm(Name,P,Msg,[]).
comm(Name,P,Msg,Info) ->
  P!{from_endpoint,Name,Msg,Info}.




      
      
      
  
  

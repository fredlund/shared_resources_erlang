-module(shr_utils).

-export([print_mfa/1,set_beginning/0,milliseconds_after/0]).
-export([put/2,get/1,ensure_open/0,open_clean_db/0]).
-export([initial_state/2,module/1]).
-export([nondeterministic/1]).
-export([setup_shr/0]).

%%-define(debug,true).
-include("debug.hrl").

print_mfa({M,F,Args}) ->
  io_lib:format
    ("~p:~p(~s)",
     [M,F,print_terms(Args,",")]);
print_mfa({F,Args}) ->
  io_lib:format
    ("~p(~s)",
     [F,print_terms(Args,",")]).

print_terms([],_Combine) ->
  "";
print_terms([T],_Combine) ->
  io_lib:format("~p",[T]);
print_terms([T|Rest],Combine) ->
  io_lib:format("~p~s~s",[T,Combine,print_terms(Rest,Combine)]).

timestamp_to_milliseconds() ->
  {MegaSeconds,Seconds,MicroSeconds} = os:timestamp(),
  (MegaSeconds*1000000+Seconds)*1000+(MicroSeconds div 1000).

set_beginning() ->
  Timestamp = timestamp_to_milliseconds(),
  ?MODULE:put({?MODULE,beginning},{timestamp,Timestamp}),
  Timestamp.

milliseconds_after() ->
  Beginning = 
    case ?MODULE:get({?MODULE,beginning}) of
      {timestamp,Timestamp} ->
	Timestamp;
      undefined -> 
	set_beginning()
    end,
  timestamp_to_milliseconds() - Beginning.


put(Key,Value) ->
  ensure_open(),
  ets:insert(?MODULE,{Key,Value}).

get(Key) ->
  ensure_open(),
  try ets:lookup(?MODULE,Key) of
    [{Key,Value}] -> Value;
    _ -> undefined
  catch _:_ -> false end.
      
ensure_open() ->
  case ets:info(?MODULE) of
    undefined ->
      open_db();
    _ ->
      ok
  end.

open_db() ->
  spawn(fun () ->
	    try ets:new(?MODULE,[named_table,public]), wait_forever()
	    catch _:_ -> ensure_open() end
	end),
  wait_until_stable().

wait_forever() ->
  receive _ -> wait_forever() end.

wait_until_stable() ->
  case ets:info(?MODULE) of
    undefined ->
      timer:sleep(10),
      wait_until_stable();
    _ ->
      ok
  end.

open_clean_db() ->
  ensure_open(),
  ets:delete_all_objects(?MODULE).

initial_state(Module,Options) when is_atom(Module) ->				 
  Module:initial_state([],Options);
initial_state({Module,Init},Options) when is_atom(Module) ->	
  Module:initial_state(Init,Options).

module(Module) when is_atom(Module) ->
  Module;
module({Module,_}) when is_atom(Module) ->
  Module.

nondeterministic(States=[_,_|_]) ->
  {'$shr_nondeterministic',States};
nondeterministic([State]) ->
  State;
nondeterministic(State) when not(is_list(State)) ->
  State.

setup_shr() ->
  shr_supervisor:restart(self()),
  ?TIMEDLOG("starting shr_register~n",[]),
  Return = 
    shr_supervisor:add_childproc
      (shr_register,
       fun () -> shr_register:start_link() end),
  ?TIMEDLOG
     ("shr_supervisor returns ~p exists: ~p~n",
      [Return,
       whereis(shr_register)]).

  


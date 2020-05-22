-module(shr_storage).

-export([read/1,write/2]).

write(Key,Value) ->
  ensure_open(),
  ets:insert(?MODULE,{Key,Value}).

read(Key) ->
  ensure_open(),
  try ets:lookup(?MODULE,Key) of
    [{Key,Value}] -> {ok,Value};
    _ -> false
  catch _:_ -> false end.
      
ensure_open() ->
  case ets:info(?MODULE) of
    undefined ->
      open_db();
    _ ->
      ok
  end.

open_clean_db() ->
  ensure_open(),
  ets:delete_all_objects(?MODULE).

open_db() ->
  spawn(fun () ->
	    ets:new(?MODULE,[named_table,public]),
	    wait_forever()
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
  

-module(gritter_sim).

-export([run/3]).

-define(debug,true).
-include("../../src/debug.hrl").

run(N,Controller,_Options) when is_integer(N), N>=2 ->
  {A,B,C} = os:timestamp(),
  random:seed(A,B,C),
  Self = self(),
  lists:foreach
    (fun (Id) ->
	 spawn_link
	   (fun () ->
		run1(Id,N,Controller),
		Self!done
	    end)
     end, lists:seq(1,N)),
  lists:foreach(fun (_) -> receive done -> ok end end, lists:seq(1,N)).

run1(Id,N,Controller) ->
  {A,B,C} = os:timestamp(),
  random:seed(A,B,C),
  Action = random:uniform(4),
  Call = 
    if
      Action==1 ->
	Regritos = rnd_bool(),
	Seguir = choose_except(1,N,Id),
	{seguir,[Id,Seguir,Regritos]};
      Action==2 ->
	Seguir = choose(1,N),
	{dejarDeSeguir,[Id,Seguir]};
      Action==3 ->
	Grito = list(fun () -> choose(32,64) end),        
	Regrito = rnd_bool(),
	{enviar,[Id,Grito,Regrito]};
      Action==4 ->
	{leer,[Id]}
    end,
  io:format("~p: will execute ~p~n",[Id,Call]),
  Result = shr_calls:call(Controller,Call),
  io:format("~p: call ~p returned ~p~n",[Id,Call,Result]),
  run1(Id,N,Controller).

choose_except(From,To,Except) ->
  oneof(lists:delete(Except,lists:seq(From,To))).
oneof(List) ->
  lists:nth(random:uniform(length(List)),List).
rnd_bool() ->
  case random:uniform(2) of 1 -> true; 2 -> false end.
choose(From,To) ->
  From+random:uniform(To-From)-1.
list(F) ->
  case random:uniform(10) of
    1 -> [];
    N -> lists:map(fun (_) -> F() end, lists:seq(1,N))
  end.

		

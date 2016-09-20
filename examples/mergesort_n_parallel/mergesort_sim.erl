-module(mergesort_sim).

-export([run/2]).

-define(debug,true).
-include("../../src/debug.hrl").

run(Config,Controllers) ->
  init_random(),
  Self = self(),
  lists:foreach
    (fun ({link,N,M,I}) ->
	 spawn_link_init_random
	   (fun () -> 
		links(controller(N,Controllers),M,controller(I,Controllers)) 
	    end)
     end, mergesort_tests:links(Config)),
  lists:foreach
    (fun ({output,N}) ->
	 spawn_link_init_random
	   (fun () -> outputs(controller(N,Controllers),Self) end)
     end, mergesort_tests:outputs(Config)),
  Rounds = choose(1,10),
  lists:foreach
    (fun ({input,N,M}) ->
	 spawn_link_init_random
	   (fun () -> inputs(controller(N,Controllers),M,Rounds) end)
     end, mergesort_tests:inputs(Config)),
  lists:foreach(fun (_) -> receive done -> ok end end, mergesort_tests:outputs(Config)).

controller(N,Controllers) ->
  case lists:keyfind(N,1,Controllers) of
    {_,Controller} -> 
      Controller
  end.

spawn_link_init_random(Fun) ->
  spawn_link
    (fun () ->
	 init_random(),
	 Fun()
     end).

inputs(N,M,0) -> 
  shr_calls:call(N,{input,[M,-1]});
inputs(N,M,Rounds) when Rounds>0 ->
  inputs(N,M,choose(0,10),choose(0,10)),
  inputs(N,M,Rounds-1).
inputs(N,M,_Min,0) ->
  io:format("~p: input(~p,~p)~n",[N,M,eod]),
  shr_calls:call(N,{input,[M,eod]});
inputs(N,M,Min,Count) ->
  NewMin = choose(Min,Min+3),
  io:format("~p: input(~p,~p)~n",[N,M,{data,NewMin}]),
  shr_calls:call(N,{input,[M,{data,NewMin}]}),
  inputs(N,M,NewMin,Count-1).

outputs(N,Self) ->
  Result = shr_calls:call(N,{output,[]}),
  io:format("~p: output(~p)~n",[N,Result]),
  case Result of
    -1 -> 
      Self!done;
    _ -> 
      outputs(N,Self)
  end.

links(N,M,I) ->
  Result = shr_calls:call(N,{output,[]}),
  shr_calls:call(M,{input,[I,Result]}),
  links(N,M,I).

choose(From,To) ->
  From+random:uniform(To-From)-1.

init_random() ->
  {A,B,C} = os:timestamp(),
  random:seed(A,B,C).

  
		

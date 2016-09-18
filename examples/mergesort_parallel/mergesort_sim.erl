-module(mergesort_sim).

-export([run/2]).

-define(debug,true).
-include("../../src/debug.hrl").

run(Config,Controllers) ->
  {A,B,C} = os:timestamp(),
  random:seed(A,B,C),
  Self = self(),
  lists:foreach
    (fun ({link,N,M,I}) ->
	 spawn_link
	   (fun () -> 
		links(controller(N,Controllers),M,controller(I,Controllers)) 
	    end)
     end, mergesort_tests:links(Config)),
  lists:foreach
    (fun ({output,N}) ->
	 spawn_link(fun () -> outputs(controller(N,Controllers),Self) end)
     end, mergesort_tests:outputs(Config)),
  lists:foreach
    (fun ({input,N,M}) ->
	 spawn_link(fun () -> inputs(controller(N,Controllers),M) end)
     end, mergesort_tests:inputs(Config)),
  lists:foreach(fun (_) -> receive done -> ok end end, mergesort_tests:outputs(Config)).

controller(N,Controllers) ->
  case lists:keyfind(N,1,Controllers) of
    {_,Controller} -> 
      Controller
  end.

inputs(N,M) ->
  inputs(N,M,choose(0,10),choose(0,10)).
inputs(N,M,_Min,0) ->
  io:format("~p: input(~p,~p)~n",[N,M,eod]),
  shr_calls:call(N,{input,[M,eod]});
inputs(N,M,Min,Count) ->
  NewMin = choose(Min,Min+3),
  io:format("~p: input(~p,~p)~n",[N,M,NewMin]),
  shr_calls:call(N,{input,[M,NewMin]}),
  inputs(N,M,NewMin,Count-1).

outputs(N,Self) ->
  Result = shr_calls:call(N,{output,[]}),
  io:format("~p: output(~p)~n",[N,Result]),
  case Result of
    eod -> 
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

		

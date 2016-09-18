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
     end, mergesorts_tests:outputs(Config)),
  lists:foreach
    (fun ({input,N,M}) ->
	 spawn_link(fun () -> inputs(controller(N,Controllers),M) end)
     end, mergesort_tests:inputs(Config)),
  lists:foreach(fun (_) -> receive done -> ok end end, mergesorts_tests:outputs(Config)).

controller(N,Controllers) ->
  case lists:keysearch(N,1,Controllers) of
    {_,Controller} -> 
      Controller
  end.

inputs(N,M) ->
  inputs(N,M,choose(0,10),choose(0,10)).
inputs(_N,_M,_Min,0) ->
  ok;
inputs(N,M,Min,Count) ->
  NewMin = choose(Min,Min+3),
  shr_calls:call(N,{input,M,NewMin}),
  inputs(N,M,NewMin,Count-1).

outputs(N,Self) ->
  case shr_calls:call(N,output) of
    eod -> Self!done;
    _ -> outputs(N,Self)
  end.

links(N,M,I) ->
  Result = shr_calls:call(N,output),
  shr_calls:call(M,{input,I,Result}),
  links(N,M,I).

choose(From,To) ->
  From+random:uniform(To-From)-1.

		

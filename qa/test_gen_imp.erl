-module(test_gen_imp).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_gen() ->
  ?LET
     (Scheduler,
      oneof([{fcfs,[]},{always,[]}]),
      begin
	Max = 7,
	Options = 
	  [{start_fun,start(Scheduler,Max)},
	   {no_par,true},
	   {needs_java,false}],
	Generator = 
	  {fsms,[{7,{multibuffer_reader_fsm,[Max,?MODULE]}},
		 {7,{multibuffer_writer_fsm,[Max,?MODULE]}}]},
	tester:test
	  (Options,
	   {multibuffer,[Max]},
	   Scheduler,
	   Generator)
      end).
	  
start(Scheduler,Max) ->
  fun (_,_) ->
      Resource = resource:start_link({multibuffer,[Max]},Scheduler,[]),
      tester:store_data(resource,Resource)
  end.
	  
test() ->
  eqc:quickcheck(prop_gen()).

get(N) ->
  resource:call(tester:get_data(resource),get,[N]).

put(L) ->	    
  resource:call(tester:get_data(resource),put,[L]).
  

      
      

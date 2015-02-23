-module(test_gen_imp).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

test_multibuffer() ->
  Configs =
    product
      ([
	[{fcfs,[]},{always,[]}],
	[{no_par,true},{no_par,false}]
       ]),
  lists:map
    (fun ([Scheduler,ParValue]) ->
	 Max =
	   7,
	 Implementation =
	   {?MODULE,[Scheduler,Max]},
	 Options = 
	   [{implementation,Implementation},
	    ParValue,
	    {needs_java,false}],
	 Resource =
	   {multibuffer,[Max]},
	 Generator = 
	   {fsms,[{7,{multibuffer_reader_fsm,[Max,?MODULE]}},
		  {7,{multibuffer_writer_fsm,[Max,?MODULE]}}]},
	 io:format
	   ("Testing resource with scheduler ~p: parallel: ~p~n",
	    [Scheduler,ParValue]),
	 eqc:quickcheck
	   (tester:test
	      (Options,
	       Resource,
	       Scheduler,
	       Generator))
     end, Configs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start([Scheduler,Max],_) ->
  Resource = resource:start_link({multibuffer,[Max]},Scheduler,[]),
  tester:store_data(resource,Resource).
	  
get(N) ->
  resource:call(tester:get_data(resource),get,[N]).

put(L) ->	    
  resource:call(tester:get_data(resource),put,[L]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

product(Sets) ->
  product(Sets,[]).

product([],Elems) ->
  [(lists:reverse(Elems))];
product([Set|RestSets],Elems) ->
  lists:flatmap
    (fun (Elem) ->
	 product(RestSets,[Elem|Elems])
     end, Set).

      
      

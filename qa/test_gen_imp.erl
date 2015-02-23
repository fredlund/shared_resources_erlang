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
	   {?MODULE,[multibuffer,Scheduler,Max]},
	 Options = 
	   [{implementation,Implementation},
	    ParValue],
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

test_robots() ->
  Configs =
    product
      ([
	[{fcfs,[]},{always,[]}],
	[{no_par,true},{no_par,false}]
       ]),
  lists:map
    (fun ([Scheduler,ParValue]) ->
	 NUM_NAVES =
	   4,
	 MAX_WEIGHT =
	   1000,
	 Implementation =
	   {?MODULE,[robots,Scheduler,NUM_NAVES,MAX_WEIGHT]},
	 Options = 
	   [{implementation,Implementation},
	    ParValue],
	 Resource =
	   {robots,[NUM_NAVES,1000]},
	 Generator = 
	   {fsms,[{10,{robot_fsm,[NUM_NAVES,?MODULE]}}]}, 
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

start([multibuffer,Scheduler,Max],_) ->
  Resource = resource:start_link({multibuffer,[Max]},Scheduler,[]),
  tester:store_data(resource,Resource);

start([robots,Scheduler,NUM_NAVES,MAX_WEIGHT],_) ->
  Resource = resource:start_link({robots,[NUM_NAVES,MAX_WEIGHT]},Scheduler,[]),
  tester:store_data(resource,Resource).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(N) ->
  resource:call(tester:get_data(resource),get,[N]).

put(L) ->	    
  resource:call(tester:get_data(resource),put,[L]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


enter(Robot,Nave,Peso) ->
  resource:call(tester:get_data(resource),enter,[Robot,Nave,Peso]).

exit(Robot,Nave,Peso) ->	    
  resource:call(tester:get_data(resource),exit,[Robot,Nave,Peso]).
  
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

      
      

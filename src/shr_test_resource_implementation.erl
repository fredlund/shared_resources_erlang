-module(shr_test_resource_implementation).

%%-define(debug,true).
-include("debug.hrl").

-include("resources.hrl").


-export([prop_tri/1,prop_tri/7]).

prop_tri(RTest) when is_record(RTest,rtest) ->
  prop_tri
    (
    not_undefined(RTest#rtest.generator,generator,rtest),
    RTest#rtest.start_implementation,
    RTest#rtest.stop_implementation,
    not_undefined(RTest#rtest.resource,resource,rtest),
    RTest#rtest.scheduler,
    RTest#rtest.test_observer,
    RTest#rtest.options
   ).

prop_tri(Generator, 
	 StartImplementation,
	 StopImplementation,
	 ResourceSpec,
	 PreSchedulerSpec,
	 TestObserversSpec,
	 PreOptions) ->
  SchedulerSpec =
    if
      (PreSchedulerSpec == void) or (PreSchedulerSpec == undefined) ->
	shr_always;
      true ->
	PreSchedulerSpec
    end,
  Options = 
    [
     {test_gen_spec,Generator}
    ,{data_spec,ResourceSpec}
    ,{waiting_spec,SchedulerSpec}
    ,{test_corr_spec,shr_corr_resource}
     |not_void([StartImplementation,StopImplementation,TestObserversSpec],
	       [start_fun,stop_fun,test_observer_spec],
	       PreOptions)
    ],
  ?TIMEDLOG("will execute shr_test_jobs:prop_res(~p)~n",[Options]),
  shr_test_jobs:prop_res(Options).

not_undefined(undefined,Name,Record) ->
  io:format
    ("*** Error: ~p is undefined in ~p~n",
     [Name,Record]),
  error(bad_test);
not_undefined(Value,_,_) ->
  Value.

not_void([],[],Options) ->
  Options;
not_void([Option|Options],[Name|Names],PreOptions) ->
  not_void(Options,Names,not_void_option(Option,Name,PreOptions)).

not_void_option(Option,Name,PreOptions) ->
  if
    Option =/= void ->
      [{Name,Option}|PreOptions];
    true ->
      PreOptions
  end.

  

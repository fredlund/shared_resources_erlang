-module(shr_test_resource_implementation).

-define(debug,true).
-include("debug.hrl").

-export([prop_tri/7]).

prop_tri(Generator, 
	 StartImplementation,
	 StopImplementation,
	 ResourceSpec,
	 PreSchedulerSpec,
	 TestObserversSpec,
	 PreOptions) ->
  SchedulerSpec =
    if
      PreSchedulerSpec == void ->
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

  

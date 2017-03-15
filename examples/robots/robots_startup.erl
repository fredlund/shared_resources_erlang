%% Starts the robot subsystem -- i.e.,
%% an environment (robots_physical),
%% a controller (implemented by a generic resource where the data specification
%% is given by the data_spec option, and the waiting specification by the
%% wait_spec option),
%% a list of protocol machines for interacting with the environment
%% and the controller.

-module(robots_startup).

-export([startup/1]).

%%-define(debug,true).
-include("../../src/debug.hrl").


startup(Options) ->
  ?TIMEDLOG("starting up -- options are~n  ~p~n",[Options]),

  %% We need an environment
  [Environment] = 
    shr_simple_supervisor:add_childproc
      (physical, fun () -> robots_physical:start_link(Options) end),

  %% We need a controller, first start the resource and then wrap it in
  %% a generic Erlang resource implementation
  DataSpec = proplists:get_value(data_spec,Options),
  WaitingSpec = proplists:get_value(waiting_spec,Options),
  [Controller] = 
    shr_simple_supervisor:add_childproc
      (DataSpec, 
       fun () ->
	   shr_gen_resource:start_link(DataSpec,WaitingSpec,Options)
       end),

  %% Next create N protocol machines.
  ProtocolImplementation = 
    proplists:get_value(protocol_implementation,Options,robots_protocol),
  lists:map
    (fun (Id) ->
	 ?TIMEDLOG("will create machine ~p~n",[Id]),
	 [ControllerPid,EnvironmentPid] = 
	   shr_simple_supervisor:add_childproc
	     ({protocol,Id},
	      fun () ->
		  shr_gen_protocol:start_link
		    (ProtocolImplementation,
		     [{controller,Controller},
		      {environment,Environment}]++Options)
	      end),
	 shr_register:register({controller,Id},ControllerPid),
	 shr_register:register({environment,Id},EnvironmentPid),
	 [{{controller,Id},ControllerPid},{{environment,Id},EnvironmentPid}]
     end, lists:seq(1,proplists:get_value(num_robots,Options))).


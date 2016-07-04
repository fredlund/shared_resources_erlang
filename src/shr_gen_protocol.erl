-module(shr_gen_protocol).

%% Probably this should be a gen_server instead.
%% However, the module is not a standard one, as
%% it returns two processes instead of one.

-export([start/2, start/3, start_link/2, start_link/3]).

-record(state,
	{
	  module
	  ,controller
	  ,environment
	  ,protocol_state
	  ,previous_controller_call
	}).

%%-define(debug,true).
-include("debug.hrl").

start(ProtocolSpec,Options) ->
  start(ProtocolSpec,none,Options).

start(ProtocolSpec,Name,Options) ->
  do_start(ProtocolSpec,Name,Options,false).

start_link(ProtocolSpec,Options) ->
  start_link(ProtocolSpec,none,Options).

start_link(ProtocolSpec,Name,Options) ->
  do_start(ProtocolSpec,Name,Options,true).

do_start(ProtocolSpec,Name,Options,DoLink) ->
  Module = shr_utils:module(ProtocolSpec),
  {ProtocolState,ProtocolOptions} =
    case shr_utils:initial_state(ProtocolSpec,Options) of
      {ok,PS,PO} -> {PS,PO};
      {ok,PS} -> {PS,[]}
    end,
  State =
    #state
    {
    module = Module
    ,controller = proplists:get_value(controller,Options)
    ,environment = proplists:get_value(environment,Options)
    ,protocol_state = ProtocolState
    ,previous_controller_call = void
   },
  SpawnFun = if DoLink -> spawn_link; true -> spawn end,
  Pid = erlang:SpawnFun(fun () -> loop(State) end),
  case Name of
    {local,LocalName} ->
      register(LocalName,Pid);
    _ ->
      ok
  end,
  ControllerProtocolPort = shr_port:new(controller,[Pid]),
  EnvironmentProtocolPort = shr_port:new(environment,[Pid]),
  {ok,[ControllerProtocolPort,EnvironmentProtocolPort]}.

loop(State) ->
  Module =
    State#state.module,
  receive
    {From,RawCall} when (From==controller) orelse (From==environment) ->
      {_Name,_Args} = Call = shr_calls:msg(RawCall),
      ?TIMEDLOG("~s~n",[shr_utils:print_mfa({From,_Name,_Args})]),
      ProtocolState = State#state.protocol_state,
      case Module:postcondition(From,Call,ProtocolState) of
	true ->
	  NextProtocolState = Module:next_state(From,Call,ProtocolState),
	  Result = type_call(Call,From,State),
	  ?TIMEDLOG("will reply ~p to ~p~n",
		    [Result,RawCall]),
	  shr_calls:reply(RawCall,Result),
	  PreviousControllerCall =
	    if
	      From==controller -> Call;
	      true -> void
	    end,
	  loop(State#state
	       {
		 protocol_state=NextProtocolState
		 ,previous_controller_call=PreviousControllerCall
	       });
	false ->
	  ?TIMEDLOG("postcondition_failed for ~p; state was~n  ~p~n",
		    [RawCall,ProtocolState]),
	  shr_calls:reply
	    (RawCall,
	     {error,{protocol_postcondition_failed,{From,Call,ProtocolState}}})
      end
  end.
	  
type_call({Fun,Args},environment,State) ->
  shr_calls:call(State#state.environment,{Fun,Args});
type_call({Fun,Args},controller,State) ->
  shr_calls:call(State#state.controller,{Fun,Args}).



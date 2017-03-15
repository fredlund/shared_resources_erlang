%% @doc This module provides an inefficient but general implementation 
%% of a shared resource, with priorities.
%% A resource is implemented as a gen_server process.

-module(shr_gen_resource).
-behaviour(gen_server).

-export([start/3,start/4,start_link/3,start_link/4,call/2,operations/1]).

-export([init/1,handle_call/3,terminate/2]). 
-export([handle_cast/2,handle_info/2,code_change/3]).

-define(debug,true).
-include("debug.hrl").

-type waitstate() :: any().
-type callstate() :: any().

-type call() :: {atom(),[any()]}.
-export_type([call/0]).

-type module_or_module_init() :: atom() | {atom(),[any()]}.

-record(call_record,
	{
	  call :: call(),
	  from :: {pid(),reference()}
	}).
-record(call_waitinginfo,
	{
	  callrec :: #call_record{},
	  waitinfo :: any()
	}).
-record(state,
	{
	  state :: callstate(),
	  waitstate :: waitstate(),
	  calls :: [#call_waitinginfo{}],
	  state_module :: atom(),
	  wait_module :: atom()
	}).

init([StateSpec,WaitSpec|Options]) ->
  StateMod = shr_utils:module(StateSpec),
  WaitMod = shr_utils:module(WaitSpec),
  State = shr_utils:initial_state(StateSpec,Options),
  WaitState = shr_utils:initial_state(WaitSpec,Options),
  {ok,
   #state
   {state_module=StateMod, wait_module=WaitMod,
    state=State, waitstate=WaitState,
    calls=[]}}.

handle_call(Command,From,State) ->
  ?TIMEDLOG("handle_call(~p) in ~p~n",[Command,State]),
  case Command of
    operations -> 
      {reply, apply(State#state.state_module,operations,[]), State};
    Call ->
      case pre(Call,State) of
	true ->
	  CallRecord = #call_record{call=Call,from=From},
	  NewState = add_callrecord(CallRecord,State),
	  {noreply,compute_new_state(NewState)};
	false ->
	  {noreply,State}
      end
  end.

compute_new_state(State) ->
  case enabled_calls(State) of
    [] ->
      State;
    EnabledCalls ->
      CallInfo =
	pick_call(EnabledCalls),
      CallRecordToExecute = 
	CallInfo#call_waitinginfo.callrec,
      CallToExecute =
	CallRecordToExecute#call_record.call,
      Result =
	case return_value(CallToExecute,State) of
	  underspecified -> void;
	  Other -> Other
	end,
      PostState =
	post(CallToExecute,Result,State),
      Info =
	CallInfo#call_waitinginfo.waitinfo,
      NewWaitState =
	post_waiting(CallToExecute,Info,PostState),
      RemainingCalls =
	NewWaitState#state.calls -- [CallInfo],
      return_to_caller
	(Result,
	 CallRecordToExecute),
      compute_new_state(NewWaitState#state{calls=RemainingCalls})
  end.

handle_info(_,State) ->
  {noreply,State}.

handle_cast(_,State) ->
  {noreply,State}.

code_change(_,State,_) ->
  {ok,State}.

terminate(_,_) ->
  ok.

-spec add_callrecord(#call_record{},#state{}) -> #state{}.
add_callrecord(CallRecord,State) ->
  Call = CallRecord#call_record.call,
  {Info,NewWaitState} = new_waiting(Call,State),
  CallInfo = #call_waitinginfo{callrec=CallRecord,waitinfo=Info},
  State#state{waitstate=NewWaitState,calls=[CallInfo|State#state.calls]}.

-spec pick_call([#call_waitinginfo{}]) -> #call_waitinginfo{}.
pick_call(Calls) ->
  N = random:uniform(length(Calls)),
  lists:nth(N,Calls).

-spec enabled_calls(#state{}) -> [#call_waitinginfo{}].
enabled_calls(State) ->
  lists:filter
    (fun (CallInfo) ->
	 Call = (CallInfo#call_waitinginfo.callrec)#call_record.call,
	 Info = CallInfo#call_waitinginfo.waitinfo,
	 cpre(Call,State) andalso priority_enabled(Call,Info,State)
     end, State#state.calls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec pre(call(),#state{}) -> boolean().
pre(Call,State) ->
  Result = apply(State#state.state_module,pre,[Call,State#state.state]),
  ?TIMEDLOG
     ("~p: pre(~s) -> ~p (~p)~n",
      [self(),print_call(Call),Result,State#state.state_module]),
  Result.
  
-spec cpre(call(),#state{}) -> boolean().
cpre(Call,State) ->
  Result = apply(State#state.state_module,cpre,[Call,State#state.state]),
  ?TIMEDLOG
     ("~p: cpre(~s) -> ~p~n",
      [self(),print_call(Call),Result]),
  Result.

-spec post(call(),any(),#state{}) -> #state{}.
post(Call,Result,State) ->
    NewDataState
    = apply(State#state.state_module,post,[Call,Result,State#state.state]),
  ?TIMEDLOG
     ("~p: post(~s,~p) -> ~p~n",
      [self(),print_call(Call),Result,NewDataState]),
  State#state{state=NewDataState}.

-spec return_value(call(),#state{}) -> any().
return_value(Call,State) ->
  ReturnValue
    = apply(State#state.state_module,return_value,[Call,State#state.state]),
  ?TIMEDLOG
     ("~p: return_value(~s) -> ~p~n",
      [self(),print_call(Call),ReturnValue]),
  ReturnValue.

-spec new_waiting(call(),#state{}) -> {any(),waitstate()}.
new_waiting(Call,State) ->
  Result
    = apply(State#state.wait_module,
	    new_waiting,
	    [Call,
	     State#state.waitstate,
	     State#state.state]),
  ?TIMEDLOG
     ("~p: new_waiting(~s) -> ~p~n",
      [self(),print_call(Call),Result]),
  Result.

-spec priority_enabled(call(),any(),#state{}) -> boolean().
priority_enabled(Call,Info,State) ->
  Result =
    apply(State#state.wait_module,priority_enabled,
	  [Call,Info,State#state.waitstate,State#state.state]),
  ?TIMEDLOG
     ("~p: priority_enabled(~s,~p) -> ~p~n",
      [self(),print_call(Call),Info,Result]),
  Result.

-spec post_waiting(call(),any(),#state{}) -> #state{}.
post_waiting(Call,Info,State) ->
  Result =
    apply(State#state.wait_module,post_waiting,
	  [Call,Info,State#state.waitstate,State#state.state]),
  ?TIMEDLOG
     ("~p: post_waiting(~s) -> ~p~n",
      [self(),print_call(Call),Result]),
  State#state{waitstate=Result}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_to_caller(Result,CallRecord) ->
  shr_gen_server:reply(CallRecord#call_record.from,Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc A call to a shared resource implemented using the 
%% shr_gen_resource module.
-spec call(atom()|pid(),{atom(),[any()]}) -> any().
call(Resource,{F,Args}) when is_atom(F), is_list(Args) ->
  shr_gen_server:call(Resource,{F,Args}).

%% @doc Returns a list with the operations the resource provides.
%% shr_gen_resource module.
-spec operations(atom()|pid()) -> [atom()].
operations(Resource) ->
  shr_gen_server:call(Resource,operations).

%% @doc Starts a shared resource.
%% The options argument provides necessary parameters for the resource
%% such as defining the implementation module, and the module
%% defining call priorities. The option list is treated as a property list.
%%
%% Concretely the option ``data_spec'' names
%% the resource implementation module,
%% and the ``waiting_spec'' option names the module defining call priorities.
-spec start(module_or_module_init(),module_or_module_init(),[any()]) -> {ok,pid()}.
start(DataSpec,WaitSpec,Options) ->
  shr_gen_server:start(?MODULE, [DataSpec,WaitSpec], []).
%% @doc Starts a shared resource, with a registered name.
%% The options argument provides necessary options for the resource
%% such as defining the implementation module, and the module
%% defining call priorities. The option list is treated as a property list.
%%
%% Concretely the option ``data_spec'' names
%% the resource implementation module,
%% and the ``waiting_spec'' option names the module defining call priorities.
-spec start(atom(),module_or_module_init(),module_or_module_init(),[any()]) -> {ok,pid()}.
start(Name, DataSpec, WaitSpec, Options) ->
  shr_gen_server:start(Name, ?MODULE, [DataSpec,WaitSpec|Options], []).
%% @doc Starts and links to a shared resource.
%% The options argument provides necessary options for the resource
%% such as defining the implementation module, and the module
%% defining call priorities. The option list is treated as a property list.
%%
%% Concretely the option ``data_spec'' names
%% the resource implementation module,
%% and the ``waiting_spec'' option names the module defining call priorities.
-spec start_link(module_or_module_init(),module_or_module_init(),[any()]) -> {ok,pid()}.
start_link(DataSpec, WaitSpec, Options) ->
  shr_gen_server:start_link(?MODULE, [DataSpec,WaitSpec|Options], []).
%% @doc Starts and links to a shared resource, with a registered name.
%% The options argument provides necessary options for the resource
%% such as defining the implementation module, and the module
%% defining call priorities. The option list is treated as a property list.
%%
%% Concretely the option ``data_spec'' names
%% the resource implementation module,
%% and the ``waiting_spec'' option names the module defining call priorities.
-spec start_link(atom(),module_or_module_init(),module_or_module_init(),[any()]) -> {ok,pid()}.
start_link(Name, DataSpec, WaitSpec, Options) ->
  shr_gen_server:start_link(Name, ?MODULE, [DataSpec,WaitSpec|Options], []).


print_call({F,Args}) ->
  io_lib:format("~p(~s)",[F,print_args(Args)]).
print_args([]) ->
  "";
print_args([Arg]) ->
  io_lib:format("~p",[Arg]);
print_args([Arg|Rest]) ->
  io_lib:format("~p,~s",[Arg,print_args(Rest)]).







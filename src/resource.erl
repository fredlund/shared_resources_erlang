%% An inefficient but general implementation of a shared resource, 
%% with priorities.
%%
%% A resource is implemented as a process.

-module(resource).

-export([start/3,start_link/3,start/4,start_link/4,init/2,call/3]).

-define(debug,true).
-include("debug.hrl").

-type waitstate() :: any().
-type callstate() :: any().

-record(call,
	{
	  function :: atom(),
	  arguments :: [any()]
	}).
-record(call_record,
	{
	  call :: #call{},
	  pid :: pid(),
	  tag :: reference()
	}).
-record(call_waitinginfo,
	{
	  callrec :: #call_record{},
	  waitinfo :: any()
	}).
-record(return_record,
	{
	  result :: any(),
	  tag :: reference()
	}).
-record(state,
	{
	  state :: callstate(),
	  waitstate :: waitstate(),
	  calls :: [#call_waitinginfo{}],
	  module :: atom(),
	  name :: atom() | pid()
	}).

-type call() :: {'call',atom(),[any()]}.

-export_type([call/0]).

start(Mod, Args, Options) ->
  spawn(?MODULE, init, [self(),{Mod,Args,Options,void}]).
start({local,Name}, Mod, Args, Options) ->
  spawn(?MODULE, init, [self(),{Mod,Args,Options,{local,Name}}]).
start_link(Mod, Args, Options) ->
  {ok, spawn_link(?MODULE, init, [self(),{Mod,Args,Options,void}])}.
start_link({local,Name}, Mod, Args, Options) ->
  spawn_link(?MODULE, init, [self(),{Mod,Args,Options,{local,Name}}]).

-spec init(pid(),{atom(),[any()],any(),{local,atom()}|void}) -> no_return().
init(ParentPid,{Mod,Args,_Options,NameSpec}) ->
  {ok,State} = init_state(Mod,Args),
  {ok,WaitState} = wait_init(Mod,Args),
  Name = 
    case NameSpec of
      {local,N} ->
	register(N,self()),
	N;
      _ ->
	self()
    end,
  ParentPid!{ok,self()},
  loop
    (#state{module=Mod,state=State,waitstate=WaitState,calls=[],name=Name},
     false).

-spec loop(#state{},boolean()) -> no_return().
loop(State,NeedUpdate) ->
  case new_calls(State) of
    [] when not(NeedUpdate) ->
      timer:sleep(1);
    CallRecords ->
      NewState =
	lists:foldl
	  (fun (CallRecord,S) -> 
	       add_callrecord(CallRecord,S)
	   end, State, CallRecords),
      case enabled_calls(NewState) of
	[] ->
	  timer:sleep(1),
	  loop(NewState,false);
	EnabledCalls ->
	  CallInfo =
	    pick_call(EnabledCalls),
	  CallRecordToExecute = 
	    CallInfo#call_waitinginfo.callrec,
	  CallToExecute =
	    CallRecordToExecute#call_record.call,
	  {Result,PostState} =
	    post(CallToExecute,NewState),
	  NewWaitState =
	    post_waiting(CallToExecute,NewState),
	  RemainingCalls =
	    NewState#state.calls -- [CallInfo],
	  return_to_caller
	    (Result,
	     CallRecordToExecute),
	  loop
	    (NewState#state
	     {state=PostState,
	      waitstate=NewWaitState,
	      calls=RemainingCalls},
	    true)
      end
  end.

-spec new_calls(#state{}) -> [#call_record{}].
new_calls(State) ->
  receive
    {call,CallRecord} when is_record(CallRecord,call_record) ->
      Call = CallRecord#call_record.call,
      ?LOG
	 ("~p: got call ~s~n",
	  [State#state.name,
	   print_call(Call#call.function,Call#call.arguments)]),
      case pre(Call,State) of
	true -> [CallRecord|new_calls(State)];
	false -> new_calls(State)
      end;
    Other ->
      io:format
	("*** Warning:unexpected message ~p resource by shared resource ~p~n",
	 [Other,State#state.name]),
      new_calls(State)
  after 0 -> [] end.

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

init_state(Module,Arguments) ->
  Result = apply(Module,init,Arguments),
  ?LOG
     ("~s -> ~p~n",
      [print_call(init,Arguments),Result]),
  Result.

wait_init(Module,Arguments) ->
  Result = apply(Module,wait_init,Arguments),
  ?LOG
     ("~s -> ~p~n",
      [print_call(wait_init,Arguments),Result]),
  Result.

-spec pre(#call{},#state{}) -> boolean().
pre(Call,State) ->
  Result = apply(State#state.module,pre,[symbolic(Call),State#state.state]),
  ?LOG
     ("~p: pre(~s) -> ~p~n",
      [State#state.name,print_call(Call),Result]),
  Result.
  
-spec cpre(#call{},#state{}) -> boolean().
cpre(Call,State) ->
  Result = apply(State#state.module,cpre,[symbolic(Call),State#state.state]),
  ?LOG
     ("~p: cpre(~s) -> ~p~n",
      [State#state.name,print_call(Call),Result]),
  Result.

-spec post(#call{},#state{}) -> {any(),any()}.
post(Call,State) ->
  Result = apply(State#state.module,post,[symbolic(Call),State#state.state]),
  ?LOG
     ("~p: post(~s) -> ~p~n",
      [State#state.name,print_call(Call),Result]),
  Result.

new_waiting(Call,State) ->
  Result =
    apply(State#state.module,
	  new_waiting,
	  [symbolic(Call),
	   State#state.waitstate,
	   State#state.state]),
  ?LOG
     ("~p: new_waiting(~s) -> ~p~n",
      [State#state.name,print_call(Call),Result]),
  Result.

-spec priority_enabled(#call{},any(),#state{}) -> boolean().
priority_enabled(Call,Info,State) ->
  Result =
    apply(State#state.module,priority_enabled,
	  [symbolic(Call),Info,State#state.waitstate,State#state.state]),
  ?LOG
     ("~p: priority_enabled(~s,~p) -> ~p~n",
      [State#state.name,print_call(Call),Info,Result]),
  Result.

-spec post_waiting(#call{},#state{}) -> waitstate().
post_waiting(Call,State) ->
  Result =
    apply(State#state.module,post_waiting,
	  [symbolic(Call),State#state.waitstate,State#state.state]),
  ?LOG
     ("~p: post_waiting(~s) -> ~p~n",
      [State#state.name,print_call(Call),Result]),
  Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

symbolic(Call) ->
  {call,Call#call.function,Call#call.arguments}.

return_to_caller(Result,CallRecord) ->
  ReturnRecord = #return_record{result=Result,tag=CallRecord#call_record.tag},
  (CallRecord#call_record.pid)!{return,ReturnRecord}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec call(atom()|pid(),atom(),[any()]) -> any().
call(Resource,F,Args) when is_atom(F), is_list(Args) ->
  true = (is_atom(Resource) orelse is_pid(Resource)),
  Call = #call{function=F,arguments=Args},
  Reference = erlang:make_ref(),
  CallRecord = #call_record{call=Call,tag=Reference,pid=self()},
  ?LOG("call ~s to ~p~n",[print_call(F,Args),Resource]),
  Resource!{call,CallRecord},
  receive
    {return,ReturnRecord=#return_record{tag=Reference}}
      when is_record(ReturnRecord,return_record) ->
      Result = ReturnRecord#return_record.result,
      ?LOG("return ~s to ~p -> ~p~n",[print_call(F,Args),Resource,Result]),
      Result
  end.

print_call(Call) when is_record(Call,call) ->
  print_call(Call#call.function,Call#call.arguments).
print_call(F,Args) ->
  io_lib:format("~p(~s)",[F,print_args(Args)]).
print_args([]) ->
  "";
print_args([Arg]) ->
  io_lib:format("~p",[Arg]);
print_args([Arg|Rest]) ->
  io_lib:format("~p,~s",[Arg,print_args(Rest)]).







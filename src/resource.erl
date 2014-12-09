%% An inefficient but general implementation of a shared resource, 
%% with priorities.
%%
%% A resource is implemented as a process.

-module(resource).

-export([start/3,start_link/3,start/4,start_link/4,init/2,call/3]).

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

start(Mod, Args, Options) ->
  spawn(?MODULE, init, [self(),{Mod,Args,Options,void}]).
start({local,Name}, Mod, Args, Options) ->
  spawn(?MODULE, init, [self(),{Mod,Args,Options,{local,Name}}]).
start_link(Mod, Args, Options) ->
  {ok, spawn_link(?MODULE, init, [self(),{Mod,Args,Options,void}])}.
start_link({local,Name}, Mod, Args, Options) ->
  spawn_link(?MODULE, init, [self(),{Mod,Args,Options,{local,Name}}]).

init(ParentPid,{Mod,Args,_Options,NameSpec}) ->
  {ok, {State,WaitState}} = Mod:init(Args),
  Name = 
    case NameSpec of
      {local,N} ->
	register(N,self()),
	N;
      _ ->
	self()
    end,
  ParentPid!{ok,self()},
  loop(#state{module=Mod,state=State,waitstate=WaitState,calls=[],name=Name}).

loop(State) ->
  CallRecords = new_calls(State),
  NewState =
    lists:foldl
      (fun (CallRecord,S) -> 
	   new_waiting(CallRecord,S)
       end, State, CallRecords),
  case enabled_calls(State) of
    [] ->
      timer:sleep(1),
      loop(State);
    EnabledCalls ->
      CallInfo =
	pick_call(EnabledCalls),
      CallRecordToExecute = 
	CallInfo#call_waitinginfo.callrec,
      CallToExecute =
	CallRecordToExecute#call_record.call,
      {Result,NewState} =
	post(CallToExecute,State),
      NewWaitState =
	post_waiting(CallToExecute,State),
      RemainingCalls =
	State#state.calls -- {CallInfo},
      return_to_caller
	(Result,
	 CallRecordToExecute),
      loop(#state{state=NewState,waitstate=NewWaitState,calls=RemainingCalls})
  end.

-spec new_calls(atom()) -> [#call_record{}].
new_calls(State) ->
  receive
    {call,CallRecord} when is_record(CallRecord,call_record) ->
      Call = CallRecord#call_record.call,
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

-spec new_waiting(#call{},#state{}) -> #state{}.
new_waiting(CallRecord,State) ->
  Call = CallRecord#call_record.call,
  {CallInfo,NewWaitState} =
    apply(State#state.module,
	  new_waiting,
	  [Call,
	   State#state.waitstate,
	   State#state.state]),
  true = is_record(CallInfo,call_waitinginfo),
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
	 cpre(Call,State) andalso enabled(Call,State)
     end, State#state.calls).

-spec pre(#call{},#state{}) -> boolean().
pre(Call,State) ->
  apply(State#state.module,pre,[Call]).
  
-spec cpre(#call{},#state{}) -> boolean().
cpre(Call,State) ->
  apply(State#state.module,cpre,[Call,State#state.state]).

-spec post(#call{},#state{}) -> {any(),any()}.
post(Call,State) ->
  apply(State#state.module,post,[Call,State#state.state]).

-spec enabled(#call{},#state{}) -> boolean().
enabled(Call,State) ->
  apply(State#state.module,enabled,[Call,State#state.state]).

-spec post_waiting(#call{},#state{}) -> boolean().
post_waiting(Call,State) ->
  apply(State#state.module,enabled,[Call,State#state.waitstate,State#state.state]).

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
  Resource!{call,CallRecord},
  receive
    {return,ReturnRecord=#return_record{tag=Reference}}
      when is_record(ReturnRecord,return_record) ->
      ReturnRecord#return_record.result
  end.







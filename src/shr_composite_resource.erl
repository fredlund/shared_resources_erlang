-module(shr_composite_resource).

-export([start/2,start/3,start_link/2,start_link/3,call/2,operations/1]).

-export([init/1,handle_call/3,terminate/2]). 
-export([handle_cast/2,handle_info/2,code_change/3]).

%%-define(debug,true).
-include("debug.hrl").

-record(state,{operations=[],resources=[],external_mapping,links=[],calls=[]}).

init([SystemSpec|Options]) ->
  check_systemspec(SystemSpec),
  {ok,start_systemspec(SystemSpec,Options)}.

start(SystemSpec, Options) ->
  gen_server:start(?MODULE, [SystemSpec|Options]).

start(Name, SystemSpec, Options) ->
  gen_server:start(Name, ?MODULE, [SystemSpec|Options]).

start_link(SystemSpec, Options) ->
  gen_server:start_link(?MODULE, [SystemSpec|Options]).

start_link(Name, SystemSpec, Options) ->
  gen_server:start_link(Name, ?MODULE, [SystemSpec|Options]).

handle_call(Command,From,State) ->
  ?TIMEDLOG("handle_call(~p) in ~p~n",[Command,State]),
  case Command of
    operations -> 
      {reply, State#state.operations, State};
    Call = {Operation,_Args} ->
      true = lists:member(Operation,State#state.operations),
      {Rid,NewCall} = (State#state.external_mapping)(Call),
      {Rid,Pid} = lists:keylookup(Rid,1,State#state.resources),
      shr_calls:forward_call(Pid,NewCall,From),
      {noreply, State}
  end.

handle_info(_,State) ->
  {noreply,State}.

handle_cast(_,State) ->
  {noreply,State}.

code_change(_,State,_) ->
  {ok,State}.

terminate(_,_) ->
  ok.

call(Resource,{F,Args}) when is_atom(F), is_list(Args) ->
  gen_server:call(Resource,{F,Args}).

operations(Resource) ->
  gen_server:call(Resource,operations).

check_systemspec(SystemSpec) ->
  {system, Operations, Resources, Processes, ExternalMapping, Linking} =
    SystemSpec,
  ok.

start_systemspec(SystemSpec,_Options) ->
  {system, Operations, Resources, Processes, ExternalMapping, Linking} =
    SystemSpec,
  ResourceMap = 
    lists:map
      (fun ({Rid,{DataSpec,WaitSpec,Args}}) -> 
	   {ok,Pid} = shr_gen_resource:start_link(DataSpec,WaitSpec,Args),
	   {Rid,Pid}
       end, Resources),
  lists:foreach
    (fun ({Module,Fun,PreArgs}) ->
	 Args = subst(ResourceMap,PreArgs),
	 spawn_link(fun () -> apply(Module,Fun,Args) end)
     end, Processes),
  lists:foreach
    (fun ({{Rid1,Op1},{Rid2,Op2}}) ->
	 {Rid1,Pid1} = lists:keylookup(Rid1,1,ResourceMap),
	 {Rid2,Pid2} = lists:keylookup(Rid2,1,ResourceMap),
	 spawn_link(fun () -> link_operations(Pid1,Op1,Pid2,Op2) end)
     end, Linking),
  #state
    {
     operations=Operations,
     resources=ResourceMap,
     external_mapping=ExternalMapping,
     links=Linking
    }.

link_operations(Pid1,Op1,Pid2,Op2) ->
  Value = shr_calls:call(Pid1,Op1,[]),
  shr_calls:call(Pid2,Op2,[Value]),
  link_operations(Pid1,Op1,Pid2,Op2).

subst(Map,T) when is_tuple(T) ->
  list_to_tuple(subst(Map,tuple_to_list(T)));
subst(Map,[Hd|Tl]) ->
  [subst(Map,Hd)|subst(Map,Tl)];
subst(Map,Atom) when is_atom(Atom) ->
  case lists:keylookup(Atom,1,Map) of
    false ->
      Atom;
    {_,Value} ->
      Value
  end;
subst(_Map,Value) ->
  Value.






-module(shr_composite_resource).

-include("resources.hrl").

-export([start/3,start/4,start_link/3,start_link/4,call/2,operations/1]).

-export([init/1,handle_call/3,terminate/2]). 
-export([handle_cast/2,handle_info/2,code_change/3]).

%%-define(debug,true).
-include("debug.hrl").

-record(state,{operations=[],resources=[],external_mapping,links=[],calls=[]}).

init([SystemSpec,Args|Options]) ->
  ?TIMEDLOG("will initialize using specification~n~p~n",[SystemSpec]),
  check_systemspec(SystemSpec),
  {ok,start_systemspec(SystemSpec,Args,Options)}.

start(SystemSpec, Args, Options) ->
  shr_gen_server:start(?MODULE, [SystemSpec,Args|Options],[]).

start(Name, SystemSpec, Args, Options) ->
  shr_gen_server:start(Name, ?MODULE, [SystemSpec,Args|Options],[]).

start_link(SystemSpec, Args, Options) ->
  shr_gen_server:start_link(?MODULE, [SystemSpec,Args|Options],[]).

start_link(Name, SystemSpec, Args, Options) ->
  shr_gen_server:start_link(Name, ?MODULE, [SystemSpec,Args|Options],[]).

handle_call(Command,From,State) ->
  ?TIMEDLOG("handle_call(~p) in ~p~n",[Command,State]),
  case Command of
    operations -> 
      {reply, State#state.operations, State};
    Call = {Operation,_Args} ->
      ?TIMEDLOG("handle_call: got ~p~n",[Call]),
      true = lists:member(Operation,State#state.operations),
      case (State#state.external_mapping)(Call) of
	{Rid,NewCall} ->
	  ?TIMEDLOG("handle_call: mapped ~p to ~p:~p~n",[Call,Rid,NewCall]),
	  case lists:keyfind(Rid,1,State#state.resources) of
	    {Rid,Pid} ->
	      shr_calls:forward_call(Pid,NewCall,From),
	      {noreply, State};
	    _ ->
	      io:format
		("*** Error: rid ~p (from call ~p) is not known in~n~p~n",
		 [Rid,Call,State#state.resources]),
	      error(systemspec)
	  end;
	_ ->
	  io:format
	    ("*** Error: no mapping for call ~p~n",
	     [Call]),
	  error(systemspec)
      end
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
  shr_gen_server:call(Resource,{F,Args}).

operations(Resource) ->
  shr_gen_server:call(Resource,operations).

check_systemspec(SystemSpec) when is_record(SystemSpec,rsystem) ->
  ok;
check_systemspec(Other) ->
  io:format
    ("*** Error: system specification is on the wrong format:~n~p~n",
     [Other]),
  throw(badarg).

start_systemspec(SystemSpec,Args,_Options) ->
  ParameterMap = 
    lists:zip(SystemSpec#rsystem.parameters,Args),
  ResourceMap = 
    lists:foldl
      (fun ({Rid,ResourceSpec},Map) -> 
	       {Module,Fun,ResourceArgs} = parse_resourceSpec(ResourceSpec),
	   {ok,Pid} = apply(Module,Fun,ResourceArgs),
	   [{Rid,Pid}|Map]
       end, ParameterMap, SystemSpec#rsystem.resources),
  lists:foreach
    (fun ({Module,Fun,PreArgs}) ->
	 Args = subst(ResourceMap,PreArgs),
	 spawn_link(fun () -> apply(Module,Fun,Args) end)
     end, SystemSpec#rsystem.processes),
  lists:foreach
    (fun ({{Rid1,Op1},{Rid2,Op2}}) ->
	 {Rid1,Pid1} = lists:keyfind(Rid1,1,ResourceMap),
	 {Rid2,Pid2} = lists:keyfind(Rid2,1,ResourceMap),
	 spawn_link(fun () -> link_operations(Pid1,Op1,Pid2,Op2) end)
     end, SystemSpec#rsystem.linking),
  #state
    {
     operations=SystemSpec#rsystem.operations,
     resources=ResourceMap,
     external_mapping=SystemSpec#rsystem.external_mapping,
     links=SystemSpec#rsystem.linking
    }.

parse_resourceSpec(ResourceSpec) ->
  case ResourceSpec of
    {shr_resource,DataModule} ->
      {shr_gen_resource,start_link,[DataModule,shr_always,[]]};
    {shr_resource,DataModule,WaitingModule} ->
      {shr_gen_resource,start_link,[DataModule,WaitingModule,[]]};
    {shr_resource,DataModule,WaitingModule,Args} ->
      {shr_gen_resource,start_link,[DataModule,WaitingModule,Args]};
    Spec={_Module,_Fun,Args} when is_list(Args) -> 
      Spec
  end.

link_operations(Pid1,Op1,Pid2,Op2) ->
  Value = shr_calls:call(Pid1,{Op1,[]}),
  ?TIMEDLOG("link(~p,~p,~p,~p) got ~p~n",[Pid1,Op1,Pid2,Op2,Value]),
  shr_calls:call(Pid2,{Op2,[Value]}),
  ?TIMEDLOG("link(~p,~p,~p,~p) sent ~p~n",[Pid1,Op1,Pid2,Op2,Value]),
  link_operations(Pid1,Op1,Pid2,Op2).

subst(Map,T) when is_tuple(T) ->
  list_to_tuple(subst(Map,tuple_to_list(T)));
subst(Map,[Hd|Tl]) ->
  [subst(Map,Hd)|subst(Map,Tl)];
subst(Map,Atom) when is_atom(Atom) ->
  case lists:keyfind(Atom,1,Map) of
    false ->
      Atom;
    {_,Value} ->
      Value
  end;
subst(_Map,Value) ->
  Value.






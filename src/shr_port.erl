-module(shr_port).

-export([new/1,new/2,new/3,new/4]).

-define(debug,true).
-include("debug.hrl").

-define(VOID_NAME,'$noname').

%% Do we permit ports which are connected to multiple processes? (for now yes)
%% But we do not yet permit dynamic adding of processes.

new(Processes) ->
  new(?VOID_NAME, Processes).
new(Name, Processes) 
  when is_list(Processes), Processes=/=[] ->
  new(Name,Processes,[]).
new(Name, Processes, SpawnOptions)
  when is_list(Processes), Processes=/=[] ->
  spawn_opt(fun () -> do_port1(Name,Processes) end, SpawnOptions).
new(Name, Node, Processes, SpawnOptions)
  when is_list(Processes), Processes=/=[] ->
  spawn_opt(Node,fun () -> do_port1(Name,Processes) end, SpawnOptions).

do_port1(Name,P) ->
  Processes = 
    if
      is_pid(P) -> [P];
      true -> P
    end,
  do_port(Name,Processes).

do_port(Name,Ps) ->
  receive
    Msg ->
      ForwardedMsg = 
	if
	  Name==?VOID_NAME -> Msg;
	  true -> {Name,Msg}
	end,
      lists:foreach(fun (Pid) -> Pid!ForwardedMsg end, Ps),
      do_port(Name,Ps)
  end.

	     




      
      
      
  
  

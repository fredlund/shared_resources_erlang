-module(multibuffer_commands).

-include_lib("eqc/include/eqc.hrl").
-include("../testing/src/tester.hrl").

%%-define(debug,true).
-include("../src/debug.hrl").

-compile(export_all).

-record(teststate,{max,max_readers,max_writers,readers,writers,implementation}).

init([MAX,NReaders,NWriters,Implementation]) ->
  #teststate
    {
     max=MAX,
     implementation=Implementation,
     max_readers=NReaders,
     max_writers=NWriters,
     readers=0,
     writers=0
    }.

command(TS,State) ->
  ?LET
     (Command,
      job_cmd(TS,State),
      ?LET
	 (NextCommands,
	  eqc_gen:frequency
	    ([{3,[]},
	      {1,
	       ?LAZY
		  (begin
		     TS1 = 
		       case calltype_in_call(Command) of
			 void ->
			   TS;
			 put ->
			   TS#teststate{writers=TS#teststate.writers+1};
			 get ->
			   TS#teststate{readers=TS#teststate.readers+1}
		       end,
		     command(TS1,State)
		   end)}]),
	  [Command|NextCommands])).

job_cmd(TS,State) ->
  ?LOG("job_cmd: TS=~p~nState=~p~n",[TS,State]),
  Alternatives =
    [{TS#teststate.implementation,put,[nats(TS#teststate.max div 2)]} ||
      TS#teststate.writers < TS#teststate.max_writers]
    ++
    [{TS#teststate.implementation,get,[choose(1,TS#teststate.max div 2)]} ||
      TS#teststate.readers < TS#teststate.max_readers]
    ++
    [tester:make_void_call() ||
      (TS#teststate.writers >= TS#teststate.max_writers)
	andalso (TS#teststate.readers >= TS#teststate.max_readers)],
  ?LOG("Alternatives=~p~n",[Alternatives]),
  if
    Alternatives==[] ->
      io:format("No alternatives in state~n~p~n",[State]);
    true ->
      ok
  end,
  eqc_gen:oneof(Alternatives).

nats(N) ->
  ?LET(Num,eqc_gen:choose(1,N),lists:duplicate(Num,nat())).

calltype_in_call({_,CallType,_}) -> CallType.

precondition(_State,TS,Commands) -> 
  do_preconditions(TS,Commands).

do_preconditions(_TS,[]) ->
  true;
do_preconditions(TS,[Call|NextCalls]) ->
  {Result,NewTS} =
    case Call of
      {_,put,_} ->
	{TS#teststate.writers < TS#teststate.max_writers,
	 TS#teststate{writers=TS#teststate.writers+1}};
      {_,get,_} ->
	{TS#teststate.readers < TS#teststate.max_readers,
	 TS#teststate{readers=TS#teststate.readers+1}};
      _ ->
	{true, TS}
    end,
  Result andalso do_preconditions(NewTS,NextCalls).

next_state(TS,_State,Result,_) ->
  {NewJobs,FinishedJobs} =
    Result,
  {NumReaders0,NumWriters0} = 
    count_readers_writers(NewJobs),
  {NumReaders1,NumWriters1} =
    count_readers_writers(FinishedJobs),
  TS#teststate
    {readers=TS#teststate.readers+NumReaders0-NumReaders1,
     writers=TS#teststate.writers+NumWriters0-NumWriters1}.

count_readers_writers(Jobs) ->
  lists:foldl
    (fun (Job,{Readers,Writers}) ->
	 case Job#job.call of
	   {_,put,_} -> {Readers,Writers+1};
	   {_,get,_} -> {Readers+1,Writers};
	   _ -> {Readers,Writers}
	 end
     end, {0,0}, Jobs).

print_unblocked_job_info(Job) ->
  {_,Command,Args} = Job#job.call,
  io_lib:format("~p(~s) -> ~p",[Command,print_terms(Args),Job#job.result]).

print_terms([]) -> "";
print_terms([T]) -> io_lib:format("~p",[T]);
print_terms([T|Rest]) -> io_lib:format("~p,~s",[T,print_terms(Rest)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_,TS) ->
  (TS#teststate.implementation):start(TS#teststate.max).

  

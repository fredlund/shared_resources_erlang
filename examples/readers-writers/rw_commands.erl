-module(rw_commands).

-include_lib("eqc/include/eqc.hrl").
-include("../../testing/src/tester.hrl").

%%-define(debug,true).
-include("../../src/debug.hrl").

-compile(export_all).

%% shared resource model inner state
-record(teststate,
  {
    max_readers,
    max_writers,
    readers, 
    writers ,
    blocked,
    options,
    controller
  }).

-define(MAX_CONCURRENT,3).
-define(MAX_STATES,400).

%% constructor 
init([NumReaders,NumWriters],Options) ->
  #teststate
    {
      max_readers=NumReaders, %% # max of readers
      max_writers=NumWriters, %% # max of writers
      options=Options,
      blocked=[],
      readers=0,
      writers=0
    }.

started(TS,Controller) ->
  TS#teststate{controller=Controller}.

%% Probably should move to the tester
command(TS,State) ->
  command(TS,State,0).
command(TS,State,NumConcurrent) ->
  ?LET
     (Command,
      job_cmd(TS,State),
      if
  NumConcurrent>=?MAX_CONCURRENT -> [];
  true ->
    ?LET
       (NextCommands,
        eqc_gen:frequency
    ([{7,[]},
      {parfreq(State),
       ?LAZY
          (begin
       TS1 = 
         case calltype_in_call(Command) of
           void ->
             TS;
           afterWrite ->
             TS;
           afterReadv ->
             TS;
           beforeWrite ->
             TS#teststate{writers=TS#teststate.writers+1};
           beforeRead ->
             TS#teststate{readers=TS#teststate.readers+1}
         end,
       command(TS1,State,NumConcurrent+1)
           end)}]),
        [Command|NextCommands])
      end).

%% TBD ???
parfreq(State) ->
  case proplists:get_value(no_par,State#state.options,false) of
    true -> 0;
    false ->
      case length(State#state.states)>=?MAX_STATES of
  true ->
    io:format
      ("*** Warning: cutting parallel test case due to too many states: ~p~n",
       [length(State#state.states)]),
    0;
  false ->
    1
      end
  end.

% jobs generation
job_cmd(TS,State) ->
  ?LOG("job_cmd: TS=~p~nState=~p~n",[TS,State]),
  Alternatives =
    [{?MODULE,beforeRead} ||
      TS#teststate.readers < TS#teststate.max_readers]
    ++
    [{?MODULE,beforeWrite} ||
      TS#teststate.writers < TS#teststate.max_writers]
    ++
    [{?MODULE,afterRead} || true ]
    ++
    [{?MODULE,afterWrite} || true ]
    ++
    [tester:make_void_call() ||
      (TS#teststate.writers >= TS#teststate.max_writers) andalso
      (TS#teststate.readers >= TS#teststate.max_readers)
    ],
  ?LOG("Alternatives=~p~n",[Alternatives]),
  if
    Alternatives==[] ->
      io:format("No alternatives in state~n~p~n",[State]);
    true ->
      ok
  end,
  eqc_gen:oneof(Alternatives).

% precondition evaluator
% counts the number of processes
precondition(_State,TS,Commands) -> 
  do_preconditions(TS#teststate{blocked=[]},Commands).

do_preconditions(_TS,[]) ->
  true;
do_preconditions(TS,[Call|NextCalls]) ->
  Result =
    case Call of
      {_,beforeRead} ->
          {TS#teststate.readers < TS#teststate.max_readers,
          TS#teststate{readers=TS#teststate.readers+1}};
      {_,beforeWrite} ->
          {TS#teststate.writers < TS#teststate.max_writers,
          TS#teststate{writers=TS#teststate.writers+1}};
      {_,afterRead} ->
        true;
      {_,afterWrite} ->
        true
    end,
    Result andalso do_preconditions(TS,NextCalls)  
  .

next_state(TS,_State,Result,_) ->
  {NewJobs,FinishedJobs} =
    Result,
  {NumeReadersNJ,NumWritersNJ} = 
    count_readers_writers(NewJobs),
  {NumReadersFJ,NumWritersNJ} =
    count_readers_writers(FinishedJobs),
  TS#teststate
    {readers=TS#teststate.readers+NumeReadersNJ-NumReadersFJ,
     writers=TS#teststate.writers+NumWritersNJ-NumWritersNJ}.

count_readers_writers(Jobs) ->
  lists:foldl
    (fun (Job,{Readers,Writers}) ->
   case Job#job.call of
     {_,afterWrite,_} -> {Readers,Writers+1};
     {_,afterRead,_} -> {Readers+1,Writers};
     _ -> {Readers,Writers}
   end
     end, {0,0}, Jobs).

% readers(TS) ->
%   TS#teststate.readers.

% writers(TS) ->
%   TS#teststate.writers.

is_blocked(R,TS) ->
  lists:member(R,TS#teststate.blocked).

add_to_blocked(R,TS) ->
  OldUsed = TS#teststate.blocked,
  TS#teststate{blocked=[R|OldUsed]}.
  
% returns the type of the call
calltype_in_call(Call) ->
  case Call of
    {_,beforeRead} -> beforeRead;
    {_,afterRead} -> afterRead;
    {_,beforeWrite} -> beforeWrite;
    {_,afterWrite} -> afterWrite;
    {_,void} -> void
  end.

blocked(TS) ->
  TS#teststate.blocked.

%% JAVA calls
beforeRead() ->
  java:call(tester:get_data(controller),beforeRead).

afterRead() ->
  java:call(tester:get_data(controller),afterRead).

beforeWrite() ->
  java:call(tester:get_data(controller),beforeWrite).

afterWrite() ->
  java:call(tester:get_data(controller),afterWrite).

%% JAVA construction
start(NodeId,_TS) ->  
  case java:new(NodeId,'ReadersWriters',[]) of
    Exc = {java_exception,_} -> 
      java:report_java_exception(Exc),
      throw(bad);
    Controller ->
      tester:store_data(controller,Controller),
      Controller
  end.

% Method invokation printer
print_started_job_info(Job) ->
  {_,CommandName} = Job#job.call,
  io_lib:format("~p:~p()",[Job#job.pid,CommandName]).

print_finished_job_info(Job) ->
  {_,CommandName} = Job#job.call,
  io_lib:format("~p:~p() -> ~p",[Job#job.pid,CommandName,Job#job.result]).


% print_job_info(Job) ->
%   print_started_job_info(Job).
%   % io_lib:format("~p",[rw_in_call(Job#job.call)]).

% Print Java State
print_state(TS) ->
  io_lib:format("readers = ~p - writers = ~p", 
    [TS#teststate.readers, TS#teststate.writers]).  
  

   


-module(rw_commands).
%% MODULE that gathers all information regarding the model of the FSM to test
%% the Java module

-include_lib("eqc/include/eqc.hrl").
-include("../../testing/src/tester.hrl").

%%-define(debug,true).
-include("../../src/debug.hrl").

-compile(export_all).

%% TEST STATE - must include the Java state
-record(teststate,
  { 
    n_readers,
    n_writers,
    readers, 
    writers,
    entered,
    blocked,
    options,
    controller
  }).

-define(MAX_CONCURRENT,3).
-define(MAX_STATES,400).

init([NumReaders,NumWriters],Options) ->
  #teststate
    {
      n_readers=NumReaders,
      n_writers=NumWriters,

      readers=0,
      writers=0,
      entered=[],

      blocked=[],
      options=Options
    }.

%% STARTING THE MODEL
started(TS,Controller) ->
  TS#teststate{controller=Controller}.

%% GENERATING the COMMANDS for the model ???
%% All the commands or on demand?
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
      ([{MAX_CONCURRENT,[]},
        {parfreq(State),
         ?LAZY
            (begin
              TS1 = 
                case calltype_in_call(Command) of
                  void ->
                    TS;
                  _ ->
                    TS0 =
                    add_reader(TS),
                  add_to_blocked(Command,TS0)
                    % case calltype_in_call(Command) of
                    %   beforeRead -> add_reader(TS),
                    %                 add_to_blocked(Command,TS0);
                    %   beforeWrite -> add_writer(TS),
                    %                  add_to_blocked(Command,TS0);
                    %   %% falta para los after !!! verificar en la lista - agregar en before
                    % end,
                end,
              command(TS1,State)
             end)}]),
          [Command|NextCommands])
    end).

%% CONCURRENT EXECUTION
%% Generates commands only and only if there less than MAX_CONCURRENT
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

%% JOB GENERATOR - Call invokations ???
%% Should be [{?MODULE,command_name,args=gen_arguments()} || 
%%                                     pre_condition(command_name, args)]
%% return [{?MODULE,command_name,args=gen_arguments()} || 
%%                                     pre_condition(command_name, args)] ???
job_cmd(TS,State) ->
  ?LOG("job_cmd: TS=~p~nState=~p~n",[TS,State]),
  Alternatives =
    [{?MODULE,beforeRead,[]} ||
      true]
    %% with reader ID
    % [{?MODULE,beforeRead,[TS#teststate.l_entered_reader]} ||
    %   TS#teststate.n_readers < length(TS#teststate.entered_readers]
    ++
    [{?MODULE,beforeRead,[]} ||
      true]
    ,
    % ++
    %% after when the reader is in l_reader_enter and as well for writers

    %% only when cannot create more writers/readers
    % [tester:make_void_call() ||
    %   TS#teststate.n_readers >= length(TS#teststate.entered_readers) andalso
    %   TS#teststate.n_writers >= length(TS#teststate.entered_writers)],
  ?LOG("Alternatives=~p~n",[Alternatives]),
  if
    Alternatives==[] ->
      io:format("No alternatives in state~n~p~n",[State]);
    true ->
      ok
  end,
  eqc_gen:oneof(Alternatives).

%% HAVE NO IDEA about this ???
%% all the preconditions?
do_preconditions(_TS,[]) ->
  true;
do_preconditions(TS,[Call|NextCalls]) ->
  % Result =
  %   case Call of
  %     {_,beforeRead,_} ->
  %       (TS#teststate.n_readers < length(TS#teststate.entered_readers));
  %     {_,beforeWrite,_} ->
  %       (TS#teststate.n_writers < length(TS#teststate.entered_writers))
  %     % for after, check list
  %   end,

  % % ???
  % Result
  %   andalso do_preconditions(TS#teststate
  %          {blocked=[robot_in_call(Call)|TS#teststate.blocked]},
  %          NextCalls)
  true.

%% GENERATE the next model state for the FSM base on the FinishedJobs
%% and checks whether a Job can be executable ???
next_state(TS,_State,Result,_) ->
  {NewJobs,FinishedJobs} =
    Result,
  % not considering after - this should be done by looking for previous calls
  % RemainingNewJobs = 
  %   tester:minus_jobs(NewJobs,FinishedJobs),
  % NewJobsBlocked =
  %   lists:map(fun (Job) -> robot_in_call(Job#job.call) end, RemainingNewJobs),
  % NewUnblocked =
  %   lists:map(fun (Job) -> robot_in_call(Job#job.call) end, FinishedJobs),
  % NewBlocked =
  %   (TS#teststate.blocked ++ NewJobsBlocked) -- NewUnblocked,
  NewTS =
    lists:foldl
      (fun (Job,TSA) ->
        case Job#job.call of
          {_,beforeRead_,[P]} ->
            OldReaders =TS#teststate.readers,
            TS#teststate{readers = OldReaders+1};
          _ ->
          TSA
        end
      end,

      % TS#teststate{blocked=NewBlocked},
      NewJobs),

  lists:foldl
    (fun (Job,TSA) ->
      case Job#job.call of
        {_,beforeWrite_,[P]} ->
          OldWriters =TS#teststate.writers,
          TS#teststate{writers = OldWriters+1}
      end
    end, 
   NewTS,
   FinishedJobs).

%% JAVA calls
beforeRead() ->
  java:call(tester:get_data(controller),beforeRead,[]).

% afterRead() ->
%   java:call(tester:get_data(controller),afterRead,[]).

beforeWrite() ->
  java:call(tester:get_data(controller),beforeWrite,[]).

% afterWrite() ->
%   java:call(tester:get_data(controller),afterWrite,[]).

%% JAVA construction
start(NodeId,_TS) ->  
  case java:new(NodeId,'es.upm.babel.ccjml.samples.readerswriters.java.ReadersWritersMonitor',[]) of
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
  io_lib:format("(readers = ~p - writers = ~p)", 
    [TS#teststate.readers, TS#teststate.writers]).  


%% AUXILIARY FUNCTIONS
% add_reader({_,_,[r]},TS) ->
%   OldUsed = TS#teststate.entered_readers,
%   TS#teststate{l_entered_reader=[r|OldUsed]},
%   TS#teststate{l_entered_reader=TS#teststate.l_entered_reader+1}.
add_reader(TS) ->
  TS.

% add_writer({_,_,[w]},TS) ->
%   OldUsed = TS#teststate.entered_writers,
%   TS#teststate{l_entered_writers=[r|OldUsed]},
%   TS#teststate{l_entered_writer=TS#teststate.l_entered_writer+1}.

writer_in_call(Call) ->
  case Call of
    {_,beforeWrite,[R]} -> R;
    {_,afterWrite,[R]} -> R;
    {_,_,_} -> void
  end.

reader_in_call(Call) ->
  case Call of
    {_,beforeRead,[R]} -> R;
    {_,afterRead,[R]} -> R;
    {_,_,_} -> void
  end.

calltype_in_call(Call) ->
  case Call of
    {_,beforeRead,_} -> beforeRead;
    % {_,afterRead,_} -> afterRead;
    {_,beforeWrite,_} -> beforeWrite
    % {_,afterWrite,_} -> afterWrite
  end.

add_to_blocked(R,TS) ->
  OldUsed = TS#teststate.blocked,
  TS#teststate{blocked=[R|OldUsed]}.
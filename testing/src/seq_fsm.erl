-module(seq_fsm).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

-include("../../testing/src/tester.hrl").

init(_Id,[Sequence]) ->
  Sequence.

precondition(_Id,Sequence,_GS,[N,_Call]) ->
  N == length(Sequence).

command(_Id,Sequence,_GlobalState) ->
  case Sequence of
    [] -> stopped;
    [First|_] -> {?MODULE,call,[length(Sequence),First]}
  end.

next_state(_Id,[_|Rest],_GS,_Job) ->
  Rest.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call(_N,{Module,Fun,Args}) ->
  apply(Module,Fun,Args).



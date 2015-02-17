-module(rw).
%% MODULE that gathers all information regarding the Java implementation state

-behaviour(resource_data_implementation).

-export([init/2,pre/2,cpre/2,post/2,return/3,return_value/2]).

-include("rw.hrl").

init([NumReaders,NumWriters],_Options) ->
  #rw
    {
      max_readers=NumReaders,
      max_writers=NumWriters,
      readers = 0,
      writers = 0
    }.

%% PRE DEFINITIONS
pre({_,_},State) ->
  true.

%% CPRE DEFINITIONS
cpre({beforeRead,_},State) ->
  State#rw.writers == 0;
cpre({beforeWrite,_},State) ->
  State#rw.writers == 0 andalso State#rw.readers == 0.

%% POST DEFINITIONS
post({beforeRead,_},State) ->
  State#rw
    {readers=State#rw.readers+1};
post({afterRead,_},State) ->
  State#rw
    {readers=State#rw.readers-1};
post({beforeWrite,_},State) ->
  State#rw
    {writers=State#rw.writers+1};
post({afterWrite,_},State) ->
  State#rw
    {writers=State#rw.writers-1}.

return(_State,_Call,_Result) ->
  true.

return_value(_,_) ->
  void.

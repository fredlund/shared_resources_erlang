-module(rw).

-behaviour(resource_data_implementation).

-export([init/2,pre/2,cpre/2,post/2,return/3]).
-export([num_readers/1,num_writers/1]).

-include("rw.hrl").

init([NumReaders,NumWriters],_Options) ->
% init(_,_Options) ->
  #rw
    {
     last_readers=NumReaders,
     last_writers=NumWriters,
     readers = 0,
     writers = 0
    }.

% externals function definition
num_readers(State) ->
  State#rw.num_naves.

num_writers(State) ->
  State#rw.max_weight.

% PRE definitions
pre({beforeRead},State) ->
  true;
pre({afterRead},State) ->
  true;

pre({beforeWrite},State) ->
  true;
pre({afterWrite},State) ->
  true;

% CPRE definition
cpre({beforeRead},State) ->
  (State#rw.writers == 0);
cpre({afterRead},State) ->
  true;

cpre({beforeWrite},State) ->
  (State#rw.readers == 0) andalso (State#rw.writers == 0);
cpre({afterWrite},State) ->
  true;

% POST definition
post({beforeRead},State) ->
    State#rw{readers=(State#rw.readers+1)};
post({afterRead},State) ->
    State#rw{readers=(State#rw.readers-1)};

post({beforeWrite},State) ->
    State#rw{writers=(State#rw.writers+1)};
post({afterRead},State) ->
    State#rw{writers=(State#rw.writers-1)};
% post({exit,[_R,N,W]},State) ->
%   NewState =
%     if
%       N==State#rw.num_naves-1 -> add_weight(-W,N,State);
%       true -> add_weight(-W,N,add_robot(N+1,State))
%     end,
%   NewState.

% returning Void
return(_State,_Call,_Result) ->
  true.

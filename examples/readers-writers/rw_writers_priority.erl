%% Priority for writers

-module(rw_writers_priority).

-behaviour(resource_wait_implementation).

-export([init/2,new_waiting/3,priority_enabled/4,post_waiting/4]).

%% TBD ???
init(_,_) ->
  {x,0}.

%% call, ss, sa ??? why two components
new_waiting({beforeWrite},WS={_x,_rWaitingWriters},_DS) ->
  {void,{_x,_rWaitingWriters+1}};
new_waiting(_Call,WS,_DS) ->
  {void,WS}.

%% como marcar protocolos ??
%%  call, cinfo, ss, sa
%% ss = {_, readers waiting}
priority_enabled({beforeRead},_cinfo,{_,_rWaitingWriters},_) ->
  _rWaitingWriters == 0;
priority_enabled(_Call,_Info,_WS,_DS) ->
  true.

post_waiting({afterWrite},_CallInfo,{_x,_rWaitingWriters},_) ->
  {_x, _rWaitingWriters-1};

post_waiting(_,_CallInfo,WS,_) ->
  WS.

%% Priority for readers

-module(rw_readers_priority).

-behaviour(resource_wait_implementation).

-export([init/1,new_waiting/3,priority_enabled/4,post_waiting/4]).

%% TBD ???
init([_,N,_MaxWeight]) ->
  {N,lists:map(fun (I) -> {I,[]} end, lists:seq(0,N-1))}.

%% call, ss, sa ??? why two components
new_waiting({beforeRead}},WS={_x,_rCounter},_DS) ->
  {void,{_x, _rCounter+1};

new_waiting(_Call,WS,_DS) ->
  {void,WS}.

%% como marcar protocolos ??
%%  call, cinfo, ss, sa
%% ss = {_, readers waiting}
priority_enabled({beforeRead},_cinfo,WS={_,_rCounter},TS) ->
  TS#teststate.writers == 0;

priority_enabled({beforeWrite},_cinfo,WS={_,_rCounter},_) ->
  _rCounter == 0;

priority_enabled(_Call,_Info,_WS,_DS) ->
  true.

post_waiting({afterRead},_CallInfo,WS={_x,_rCounter},_) ->
  {_x, _rCounter-1};

post_waiting(_,_CallInfo,WS,_) ->
  WS.

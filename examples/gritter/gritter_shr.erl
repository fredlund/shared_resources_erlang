-module(gritter_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(state,{siguiendo,regritos,porleer}).

%%-define(debug,true).
-include("../../src/debug.hrl").

initial_state(_,_) ->
  #state
    {
      siguiendo = [],
      regritos = [],
      porleer = []
    }.

pre(_Msg={seguir,[Seguidorid,Seguidoid,_Regritos]},_State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  Seguidoid =/= Seguidorid;
pre(_Msg={dejarDeSeguir,[Seguidorid,Seguidoid]},State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  sets:is_element(Seguidoid,siguiendo(Seguidorid,State));
pre(_Msg={enviar,[_Uid,_Grito,_Regrito]},_State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  true;
pre(_Msg={leer,[_Uid]},_State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  true.

cpre(_Msg={seguir,[_Seguidorid,_Seguidoid,_Regritos]},_State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  true;
cpre(_Msg={dejarDeSeguir,[_Seguidorid,_Seguidoid]},_State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  true;
cpre(_Msg={enviar,[_Uid,_Grito,_Regrito]},_State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  true;
cpre(_Msg={leer,[Uid]},State) ->
  Result = sets:size(porleer(Uid,State)) > 0,
  ?TIMEDLOG("~p state is ~s; result is ~p~n",[_Msg,print_state(State),Result]),
  Result.

post(_Msg={seguir,[Seguidorid,Seguidoid,Regritos]},_Return,State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  Siguiendo = sets:add_element(Seguidoid,siguiendo(Seguidorid,State)),
  set_regritos
    (Seguidorid,Seguidoid,Regritos,
     set_siguiendo(Seguidorid,Siguiendo,State));
post(_Msg={dejarDeSeguir,[Seguidorid,Seguidoid]},_Return,State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  Siguiendo = sets:del_element(Seguidoid,siguiendo(Seguidorid,State)),
  set_siguiendo(Seguidorid,Siguiendo,State);
post(_Msg={enviar,[Uid,Grito,Regrito]},_Return,State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  lists:foldl
    (fun ({U,Siguiendo},FoldState) ->
	 case sets:is_element(Uid,Siguiendo) of
	   true ->
	     case not(Regrito) orelse regritos(U,Uid,State) of
	       true -> 
		 Porleer = porleer(U,FoldState),
		 set_porleer(U,sets:add_element(Grito,Porleer),FoldState);
	       false ->
		 FoldState
	     end;
	   false -> FoldState
	 end
     end, State, State#state.siguiendo);
post(_Msg={leer,[Uid]},Result,State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  Porleer = sets:del_element(Result,porleer(Uid,State)),
  set_porleer(Uid,Porleer,State).

return(State,{leer,[Uid]},Result) ->
  Porleer = porleer(Uid,State),
  sets:is_element(Result,Porleer);
return(_,{seguir,[Uid,Uid,_]},Result) ->
  exception('GestorGritter$PreViolationSharedResourceException',Result);
return(State,{dejarDeSeguir,[Uid1,Uid2]},Result) ->
  case sets:is_element(Uid2,siguiendo(Uid1,State)) of
    true -> not_exception(Result);
    false -> exception('GestorGritter$PreViolationSharedResourceException',Result)
  end;
return(_,_Call,Result) ->
  not_exception(Result).

exception(Name,Result) ->
  case Result of
    {exception,ExcName} -> ExcName==Name;
    _ -> false
  end.

not_exception(Result) ->
  case Result of
    {exception,_} -> false;
    _ -> true
  end.

return_value({leer,[Uid]},State) ->
  Porleer = porleer(Uid,State),
  case sets:size(Porleer) of
    Size when Size>0 ->
      Elements = sets:to_list(Porleer),
      N = random:uniform(Size),
      lists:nth(N,Elements);
    _ ->
      underspecified
  end;
return_value(_,_) ->
  underspecified.

set_siguiendo(Uid,Siguiendo,State) ->
  State#state
    {siguiendo=lists:keystore(Uid, 1, State#state.siguiendo, {Uid,Siguiendo})}.

siguiendo(Id,State) ->
  case lists:keyfind(Id,1,State#state.siguiendo) of
    false -> sets:new();
    {_,Set} -> Set
  end.
  
set_regritos(Uid1,Uid2,Regritos,State) ->
  Key = {Uid1,Uid2},
  State#state
    {regritos=lists:keystore(Key, 1, State#state.regritos, {Key,Regritos})}.

regritos(Uid1,Uid2,State) ->
  {_,Value} = lists:keyfind({Uid1,Uid2},1,State#state.regritos),
  Value.

porleer(Id,State) ->
  case lists:keyfind(Id,1,State#state.porleer) of
    false -> sets:new();
    {_,Set} -> Set
  end.
  
set_porleer(Id,Porleer,State) ->
  State#state
    {porleer=lists:keystore(Id, 1, State#state.porleer, {Id,Porleer})}.

print_state(#state{siguiendo=Siguiendo,regritos=Regritos,porleer=Porleer}) ->
  io_lib:format
    ("{siguiendo=~s,~nregritos=~s,~nporleer=~s}~n",
     [lists:foldl
      (fun ({Uid,S},Acc) ->
	   io_lib:format("~s{~p,~p}",[Acc,Uid,sets:to_list(S)])
       end, "", Siguiendo),
      lists:foldl
      (fun ({Key,Value},Acc) ->
	   io_lib:format("~s{~p,~p}",[Acc,Key,Value])
       end, "", Regritos),
      lists:foldl
      (fun ({Uid,PL},Acc) ->
	   io_lib:format("~s{~p,~p}",[Acc,Uid,sets:to_list(PL)])
       end, "", Porleer)]).

      

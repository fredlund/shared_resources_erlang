-module(mergesort_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(state,{left,right}).

%%-define(debug,true).
-include("../../src/debug.hrl").

initial_state(_,_) ->
  new_state().

pre(_Msg,_) ->
  ?TIMEDLOG("pre: ~p~n",[_Msg]),
  true.

cpre(_Msg={left,[N]},State) ->
  ?TIMEDLOG("cpre: ~p state=~s~n",[_Msg,print_state(State)]),
  is_empty(left(State));
cpre(_Msg={right,[N]},State) ->
  ?TIMEDLOG("cpre: ~p state=~s~n",[_Msg,print_state(State)]),
  is_empty(right(State));
cpre(_Msg={output,_},State) ->
  ?TIMEDLOG("cpre: ~p state=~s~n",[_Msg,print_state(State)]),
  (not(is_empty(left(State)))) andalso (not(is_empty(right(State)))).

post(_Msg={left,[Element]},_Return,State) ->
  ?TIMEDLOG("post: ~p~n",[_Msg]),
  set_left(Element,State);
post(_Msg={right,[Element]},_Return,State) ->
  ?TIMEDLOG("post: ~p~n",[_Msg]),
  set_right(Element,State);
post(_Msg={output,_},_Return,State) ->
  ?TIMEDLOG("post: ~p~n",[_Msg]),
  case is_eod(left(State)) andalso is_eod(right(State)) of
    true ->
      set_left(empty(),set_right(empty(),State));
    false ->
      case less(left(State),right(State)) of
	true ->
	  set_left(empty(),State);
	false ->
	  set_right(empty(),State)
      end
  end.

return(State,_Msg={output,_},Result) ->
  ?TIMEDLOG("return: ~p~n",[_Msg]),
  Min = min(left(State),right(State)),
  Result == Min;
return(_,_Call,Result) ->
  not_exception(Result).

not_exception(Result) ->
  case Result of
    {exception,_} -> false;
    _ -> true
  end.

return_value(_Msg={output,_},State) ->
  ?TIMEDLOG("return_value: ~p~n",[_Msg]),
  min(left(State),right(State));
return_value(_,_) ->
  underspecified.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_state(State) ->
  io_lib:format
    ("{left=~p,right=~p}",
     [left(State),right(State)]).

empty() ->
  empty().

is_element(Element,Element) ->
  true;
is_element(_,_) ->
  false.

is_empty(Element) ->
  is_element(empty,Element).

is_eod(Element) ->
  is_element(eod,Element).

less(eod,_) ->
  false;
less(_,eod) ->
  true;
less(Element1,{Element2,_}) ->
  Element1<Element2.

min(Element,eod) ->
  Element;
min(eod,Element) ->
  Element;
min(Element1,Element2) ->
  if
    Element1<Element2 ->
      Element1;
    true ->
      Element2
  end.

new_state() ->
  #state
    {
     left=empty
    ,right=empty
    }.

left(State) ->
  State#state.left.

right(State) ->
  State#state.right.

set_left(Element,State) ->
  State#state{left=Element}.

set_right(Element,State) ->
  State#state{right=Element}.




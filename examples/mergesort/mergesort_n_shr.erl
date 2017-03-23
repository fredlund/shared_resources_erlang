-module(mergesort_n_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).


%%-define(debug,true).
-include("../../src/debug.hrl").

initial_state([N],_) ->
  new_state(N).

pre(_Msg,_) ->
  ?TIMEDLOG("pre: ~p~n",[_Msg]),
  true.

cpre(_Msg={in,[N,_Element]},State) ->
  ?TIMEDLOG("cpre: ~p state=~s~n",[_Msg,print_state(State)]),
  is_empty(nth(N,State));
cpre(_Msg={output,_},State) ->
  ?TIMEDLOG("cpre: ~p state=~s~n",[_Msg,print_state(State)]),
  lists:all(fun (Element) -> not(is_empty(Element)) end, elements(State)).

post(_Msg={in,[N,Element]},_Return,State) ->
  ?TIMEDLOG("post: ~p~n",[_Msg]),
  set_nth(Element,N,State);
post(_Msg={output,_},_Return,State) ->
  ?TIMEDLOG("post: ~p~n",[_Msg]),
  case lists:all(fun (Element) -> is_eod(Element) end, elements(State)) of
    true ->
      set_elements(lists:duplicate(size(State),eod));
    false ->
      {NMin,_Min} = find_min(State),
      set_nth(empty(),NMin,State)
  end.

return(State,_Msg={output,_},Result) ->
  ?TIMEDLOG("return: ~p~n",[_Msg]),
  {_NMin,Min} = find_min(State),
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
  {_NMin,Min} = find_min(State),
  Min;
return_value(_,_) ->
  underspecified.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_state(State) ->
  io_lib:format("~p",[State]).

empty() ->
  empty.

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
less(Element1,Element2) ->
  Element1<Element2.

find_min(State) ->
  lists:foldl
    (fun ({N,Element},{NMin,Min}) ->
	 case less(Element,Min) of
	   true ->
	     {N,Element};
	   false ->
	     {NMin,Min}
	 end
     end, {eod,1}, lists:zip(lists:seq(1,size(State)),elements(State))).

new_state(N) ->
  set_elements(lists:duplicate(N,empty)).

elements(State) ->
  tuple_to_list(State).

set_elements(Elements) ->
  list_to_tuple(Elements).

nth(N,State) ->
  element(N,State).

set_nth(Value,N,State) ->
  setelement(N,State,Value).





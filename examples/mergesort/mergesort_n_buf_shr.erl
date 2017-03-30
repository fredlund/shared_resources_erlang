-module(mergesort_n_buf_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).


%%-define(debug,true).
-include("../../src/debug.hrl").

-record(state,{inputs,output}).

initial_state([N],_) ->
  new_state(N).

pre(_Msg,_) ->
  ?TIMEDLOG("pre: ~p~n",[_Msg]),
  true.

cpre(_Msg={in,[N,_Element]},State) ->
  ?TIMEDLOG("cpre: ~p state=~s~n",[_Msg,print_state(State)]),
  true;
cpre(_Msg={output,_},State) ->
  ?TIMEDLOG("cpre: ~p state=~s~n",[_Msg,print_state(State)]),
  lists:all(fun (Element) -> not(is_empty(Element)) end, elements(State)).

post(_Msg={in,[N,Element]},_Return,State) ->
  ?TIMEDLOG("post: ~p state=~s~n",[_Msg,print_state(State)]),
  add_nth(Element,N,State);
post(_Msg={output,_},_Return,State) ->
  ?TIMEDLOG("post: ~p state=~s~n",[_Msg,print_state(State)]),
  Result = 
    case lists:all(fun (Element) -> has_eod(Element) end, elements(State)) of
      true ->
	set_elements(lists:duplicate(size(inputs(State)),[eod]),State);
      false ->
	{NMins,_Min} = find_mins(State),
	shr_utils:nondeterministic
	  (lists:map (fun (NMin) -> tl_nth(NMin,State) end, NMins))
    end,
  ?TIMEDLOG("post: ~p~nstate=~s => ~p~n",[_Msg,print_state(State),Result]),
  Result.

return(State,_Msg={output,_},Result) ->
  ?TIMEDLOG("return: ~p~n",[_Msg]),
  {_NMins,Min} = find_mins(State),
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
  {_NMins,Min} = find_mins(State),
  Min;
return_value(_,_) ->
  underspecified.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_state(State) ->
  io_lib:format("~p",[State]).

empty() ->
  [].

is_element(Element,Element) ->
  true;
is_element(_,_) ->
  false.

is_empty(Element) ->
  is_element([],Element).

has_eod(Element) ->
  case Element of
    [eod|_] -> true;
    _ -> false
  end.

less(eod,_) ->
  false;
less(_,eod) ->
  true;
less(Element1,Element2) ->
  Element1<Element2.

find_mins(State) ->
  lists:foldl
    (fun ({N,[Element|_]},{NMins,Min}) ->
	 if
	   Element==Min -> 
	     {[N|NMins],Min};
	   true ->
	     case less(Element,Min) of
	       true ->
		 {[N],Element};
	       false ->
		 {NMins,Min}
	     end
	 end
     end, 
     {[1],eod}, 
     lists:zip(lists:seq(1,size(inputs(State))),elements(State))).

new_state(N) ->
  #state{inputs=list_to_tuple(lists:duplicate(N,empty())),output=empty()}.

inputs(State) ->
  State#state.inputs.

output(State) ->
  State#state.output.

set_inputs(Inputs,State) ->
  State#state{inputs=Inputs}.

set_output(Output,State) ->
  State#state{output=Output}.

elements(State) ->
  tuple_to_list(State#state.inputs).

set_elements(Elements,State) ->
  State#state{inputs=list_to_tuple(Elements)}.

nth(N,State) ->
  element(N,inputs(State)).

set_nth(Value,N,State) ->
  set_inputs(setelement(N,inputs(State),Value),State).

add_nth(Value,N,State) ->
  set_nth(nth(N,State)++[Value],N,State).

tl_nth(N,State) ->
  set_nth(tl(nth(N,State)),N,State).




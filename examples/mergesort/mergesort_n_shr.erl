-module(mergesort_n_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(buf,{max_size,contents}).
-record(state,{input_bufs,output_buf}).

%%-define(debug,true).
-include("../../src/debug.hrl").

initial_state(_,Options) ->
  OutputBufSize = proplists:get_value(output_buf_size,Options),
  InputBufSpecs = proplists:get_value(input_buf_spec,Options),
  InputBufs =
    lists:map(fun (MaxSize) -> new_buffer(MaxSize) end, InputBufSpecs),
  OutputBuf =
    new_buffer(OutputBufSize),
  new_state(InputBufs,OutputBuf).

pre(_Msg={input,[N,_Element]},State) ->
  ?TIMEDLOG("pre: ~p~n",[_Msg]),
  N =< num_input_bufs(State);
pre(_Msg,_) ->
  ?TIMEDLOG("pre: ~p~n",[_Msg]),
  true.

cpre(_Msg={input,[N,_]},State) ->
  ?TIMEDLOG("cpre: ~p state=~s~n",[_Msg,print_state(State)]),
  Buf = input_buf(N,State),
  buf_size(Buf) < max_size(Buf);
cpre(_Msg={output,_},State) ->
  ?TIMEDLOG("cpre: ~p state=~s~n",[_Msg,print_state(State)]),
  OutputBuf = output_buf(State),
  buf_size(OutputBuf) > 0.

post(_Msg={input,[N,Element]},_Return,State) ->
  ?TIMEDLOG("post: ~p~n",[_Msg]),
  InputBuf = input_buf(N,State),
  NewState = set_input_buf(N,add_element(Element,InputBuf),State),
  OutputBuf = output_buf(NewState),
  case max_size(OutputBuf) == buf_size(OutputBuf) of
    true -> 
      NewState;
    false ->
      try_move_to_output(NewState)
  end;
post(_Msg={output,_},_Return,State) ->
  ?TIMEDLOG("post: ~p~n",[_Msg]),
  OutputBuf = output_buf(State),
  try_move_to_output(set_output_buf(strip_first_element(OutputBuf),State)).

return(State,_Msg={output,_},Result) ->
  ?TIMEDLOG("return: ~p~n",[_Msg]),
  OutputBuf = output_buf(State),
  Result == first_element(OutputBuf);
return(_,_Call,Result) ->
  not_exception(Result).

not_exception(Result) ->
  case Result of
    {exception,_} -> false;
    _ -> true
  end.

return_value(_Msg={output,_},State) ->
  ?TIMEDLOG("return_value: ~p~n",[_Msg]),
  OutputBuf = output_buf(State),
  first_element(OutputBuf);
return_value(_,_) ->
  underspecified.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_state(State) ->
  io_lib:format
    ("{inputs=~s,~noutput=~s}~n",
     [lists:foldl
      (fun (Buf,Acc) ->
	   io_lib:format("~s~s",[Acc,buf_print(Buf)])
       end, "", tuple_to_list(input_bufs(State))),
      buf_print(output_buf(State))
     ]).

buf_print(Buf) ->
  io_lib:format("<~p,~w>",[max_size(Buf),contents(Buf)]).
      
new_state(InputBufs,OutputBuf) ->
  #state
    {
     input_bufs=list_to_tuple(InputBufs)
    ,output_buf=OutputBuf
    }.

num_input_bufs(State) ->
  size(input_bufs(State)).

input_bufs(#state{input_bufs=InputBufs}) ->
  InputBufs.

input_buf(N,State) ->
  element(N,input_bufs(State)).

set_input_buf(N,Buf,State) ->
  State#state{input_bufs=setelement(N,input_bufs(State),Buf)}.

output_buf(#state{output_buf=OutputBuf}) ->
  OutputBuf.

set_output_buf(OutputBuf,State) ->
  State#state{output_buf=OutputBuf}.

new_buffer(MaxSize) ->
  #buf{max_size=MaxSize,contents=[]}.

buf_size(#buf{contents=Contents}) ->
  length(Contents).

max_size(#buf{max_size=MaxBufSize}) ->
  MaxBufSize.

add_element(Element,Buf) ->
  set_contents(contents(Buf)++[Element],Buf).

contents(#buf{contents=Contents}) ->
  Contents.

set_contents(Contents,Buf) ->
  Buf#buf{contents=Contents}.

first_element(Buf) ->
  Contents = contents(Buf),
  hd(Contents).

strip_first_element(Buf) ->
  set_contents(tl(contents(Buf)),Buf).

try_move_to_output(State) ->
  OutputBuf = output_buf(State),
  case max_size(OutputBuf) > buf_size(OutputBuf) of
    true ->
      case find_min_element(tuple_to_list(input_bufs(State)),State) of
	nope ->
	  State;
	eod ->
	  NewState =
	    lists:foldl
	      (fun (I,Acc) -> 
		   InputBuf = input_buf(I,Acc),
		   set_input_buf(I,strip_first_element(InputBuf),Acc)
	       end, State, lists:seq(1,num_input_bufs(State))),
	  set_output_buf(add_element(eod,OutputBuf),NewState);
	{Element,NewState} ->
	  set_output_buf(add_element(Element,OutputBuf),NewState)
      end;
    false ->
      State
  end.

find_min_element(InputBufs,State) ->
  lists:foldl
    (fun ({I,InputBuf},Acc) ->
	 BufSize = buf_size(InputBuf),
	 if
	   (Acc==nope) orelse (BufSize==0) -> nope;
	   true ->
	     First = first_element(InputBuf),
	     case less(First,Acc) of
	       false -> 
		 Acc;
	       true ->  
		 NewInputBuf = strip_first_element(InputBuf),
		 {First,set_input_buf(I,NewInputBuf,State)}
	     end
	 end
     end, eod, lists:zip(lists:seq(1,length(InputBufs)),InputBufs)).

less(eod,_) ->
  false;
less(_,eod) ->
  true;
less(Element1,{Element2,_}) ->
  Element1<Element2.



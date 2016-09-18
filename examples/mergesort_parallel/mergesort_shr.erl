-module(mergesort_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(buf,{max_size,contents,eod}).
-record(state,{input_bufs,output_buf}).

%%-define(debug,true).
-include("../../src/debug.hrl").

initial_state(_,Options) ->
  OutputBufSize = proplists:get_value(output_buf_size,Options),
  InputBufSpecs = proplists:get_value(input_buf_spec,Options),
  InputBufs =
    lists:map
      (fun (MaxSize) -> #buf{max_size=MaxSize,contents=[],eod=false} end,
       InputBufSpecs),
  OutputBuf =
    #buf{max_size=OutputBufSize,contents=[],eod=false},
  #state
    {
     input_bufs=list_to_tuple(InputBufs)
    ,output_buf=OutputBuf
    }.

pre(_Msg={input,[N,_]},State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  (N =< num_input_bufs(State)) andalso not((input_buf(N,State))#buf.eod);
pre(_Msg,_) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  true.

cpre(_Msg={input,[N,_]},State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  Buf = input_buf(N,State),
  buf_size(Buf) < max_size(Buf);
cpre(_Msg={output,_},#state{output_buf=OutputBuf}) ->
  buf_size(OutputBuf)>0.

post(_Msg={input,[N,Element]},_Return,State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  #state{output_buf=OutputBuf} 
    = NewState 
    = add_element_to_input_buf(N,Element,State),
  case is_full(OutputBuf) of
    true -> 
      NewState;
    false ->
      try_to_move(NewState)
  end;
post(_Msg={output,_},_Return,State=#state{output_buf=OutputBuf}) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  State#state{output_buf=tl(OutputBuf)}.

is_full(Buf) ->
  max_size(Buf) == buf_size(Buf).

add_element_to_input_buf(N,Element,State=#state{input_bufs=InputBufs}) ->
  Buf = input_buf(N,State),
  set_input_buf(N,add_element(Element,Buf),State).

add_element(Element,Buf = #buf{contents=Contents}) ->
  Buf#buf{contents=Contents++[Element]}.

num_input_bufs(#state{input_bufs=InputBufs}) ->
  size(InputBufs).

input_buf(N,#state{input_bufs=InputBufs}) ->
  element(N,InputBufs).

buf_size(#buf{contents=Contents}) ->
  length(Contents).

max_size(#buf{max_size=MaxBufSize}) ->
  MaxBufSize.

eod(#buf{eod=Eod}) ->
  Eod.

set_input_buf(N,Buf,State = #state{input_bufs=InputBufs}) ->
  State#state{input_bufs=setelement(n,Buf,input_bufs)}.

try_to_move(State=#state{output_buf=OutputBuf}) ->
  case OutputBuf#buf.max_size > length(OutputBuf#buf.contents) of
    true ->
      case find_min_element(tuple_to_list(State#state.input_bufs),State) of
	nope ->
	  State;
	no_value ->
	  State;
	{Element,NewState} ->
	  NewState#state{output_buf=add_element(Element,OutputBuf)}
      end;
    false ->
      State
  end.

first_element(Buf) ->
  Element = hd(Buf#buf.contents),
  NewBuf = tl(Buf#buf.contents),
  {Element,NewBuf}.

find_min_element(InputBufs,State) ->
  lists:foldl
    (fun ({I,InputBuf},Acc) ->
	 case Acc of
	   nope ->
	     nop;
	   _ ->
	     BufSize = buf_size(InputBuf),
	     Eod = eod(InputBuf),
	     if
	       Acc==nope ->
		 Acc;
	       BufSize==0, Eod==true -> 
		 Acc;
	       BufSize==0, Eod==false ->
		 nope;
	       true ->
		 {First,NewBuf} = first_element(InputBuf),
		 case Acc of
		   no_value ->
		     set_input_buf(I,NewBuf,State);
		   {OldValue,NewState} ->
		     if
		       First<OldValue ->
			 set_input_buf(I,NewBuf,State);
		       true ->
			 Acc
		     end
		 end
	     end
	 end
     end, no_value, InputBufs).

return(#state{output_buf=OutputBuf},{output,_},Result) ->
  if
    OutputBuf#buf.contents==[] ->
      Result==eod;
    true ->
      Result == {data,hd(OutputBuf)}
  end;
return(_,_Call,Result) ->
  not_exception(Result).

not_exception(Result) ->
  case Result of
    {exception,_} -> false;
    _ -> true
  end.

return_value({output,_},#state{output_buf=OutputBuf}) ->
  if
    OutputBuf#buf.contents==[] ->
      eod;
    true ->
      {data,hd(OutputBuf)}
  end;
return_value(_,_) ->
  underspecified.

print_state(#state{input_bufs=InputBufs,output_buf=OutputBuf}) ->
  io_lib:format
    ("{inputs=~s,~noutput=~s}~n",
     [lists:foldl
      (fun (Buf,Acc) ->
	   io_lib:format("~s~s",[Acc,buf_print(Buf)])
       end, "", InputBufs),
      buf_print(OutputBuf)
     ]).

buf_print(#buf{max_size=MaxSize,contents=Contents,eod=Eod}) ->
  io_lib:format("<~p,~p,~p>",[MaxSize,Contents,Eod]).
      

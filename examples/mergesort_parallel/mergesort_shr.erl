-module(mergesort_shr).

-behaviour(shr_data_implementation).

-export([initial_state/2,pre/2,cpre/2,post/3,return/3,return_value/2]).
-export([print_state/1]).

-record(buf,{max_size,contents,eod}).
-record(state,{input_bufs,output_buf}).

%%-define(debug,true).
-include("../../src/debug.hrl").

initial_state(Options,_) ->
  io:format("Options are ~p~n",[Options]),
  OutputBufSize = proplists:get_value(output_buf_size,Options),
  InputBufSpecs = proplists:get_value(input_buf_spec,Options),
  InputBufs =
    lists:map(fun (MaxSize) -> new_buffer(MaxSize) end, InputBufSpecs),
  OutputBuf =
    new_buffer(OutputBufSize),
  new_state(InputBufs,OutputBuf).

pre(_Msg={input,[N,_]},State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  N =< num_input_bufs(State) andalso 
    begin
      InputBuf = input_buf(N,State),
      not(eod(InputBuf))
    end;
pre(_Msg,_) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  true.

cpre(_Msg={input,[N,_]},State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  Buf = input_buf(N,State),
  buf_size(Buf) < max_size(Buf);
cpre(_Msg={output,_},State) ->
  OutputBuf = output_buf(State),
  buf_size(OutputBuf)>0.

post(_Msg={input,[N,Element]},_Return,State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  InputBuf = input_buf(N,State),
  if
    Element==eod ->
      set_input_buf(N,set_eod(InputBuf),State);
    true ->
      NewState = set_input_buf(N,add_element(Element,InputBuf),State),
      OutputBuf = output_buf(NewState),
      case is_full(OutputBuf) of
	true -> 
	  NewState;
	false ->
	  try_to_move(NewState)
      end
  end;
post(_Msg={output,_},_Return,State) ->
  ?TIMEDLOG("~p~n",[_Msg]),
  OutputBuf = output_buf(State),
  set_output_buf(strip_first(OutputBuf),State).

return(State,{output,_},Result) ->
  OutputBuf = output_buf(State),
  Contents = contents(OutputBuf),
  if
    Contents==[] ->
      Result==eod;
    true ->
      Result == {data,hd(Contents)}
  end;
return(_,_Call,Result) ->
  not_exception(Result).

not_exception(Result) ->
  case Result of
    {exception,_} -> false;
    _ -> true
  end.

return_value({output,_},State) ->
  OutputBuf = output_buf(State),
  Contents = contents(OutputBuf),
  if
    Contents==[] ->
      eod;
    true ->
      {data,hd(Contents)}
  end;
return_value(_,_) ->
  underspecified.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
      
new_state(InputBufs,OutputBuf) ->
  #state
    {
     input_bufs=list_to_tuple(InputBufs)
    ,output_buf=OutputBuf
    }.

num_input_bufs(#state{input_bufs=InputBufs}) ->
  size(InputBufs).

input_buf(N,#state{input_bufs=InputBufs}) ->
  element(N,InputBufs).

set_input_buf(N,Buf,State=#state{input_bufs=InputBufs}) ->
  State#state{input_bufs=setelement(N,Buf,InputBufs)}.

output_buf(#state{output_buf=OutputBuf}) ->
  OutputBuf.

set_output_buf(OutputBuf,State) ->
  State#state{output_buf=OutputBuf}.

new_buffer(MaxSize) ->
  #buf{max_size=MaxSize,contents=[],eod=false}.

buf_size(#buf{contents=Contents}) ->
  length(Contents).

max_size(#buf{max_size=MaxBufSize}) ->
  MaxBufSize.

is_full(Buf) ->
  max_size(Buf) == buf_size(Buf).

add_element(Element,Buf = #buf{contents=Contents}) ->
  Buf#buf{contents=Contents++[Element]}.

eod(#buf{eod=Eod}) ->
  Eod.

set_eod(Buf) ->
  Buf#buf{eod=true}.
    
contents(#buf{contents=Contents}) ->
  Contents.

set_contents(Contents,Buf) ->
  Buf#buf{contents=Contents}.

first_element(Buf) ->
  Contents = contents(Buf),
  Element = hd(Contents),
  NewBuf = set_contents(tl(Contents),Contents),
  {Element,NewBuf}.

strip_first(Buf) ->
  {_Element,NewBuf} = first_element(Buf),
  NewBuf.

try_to_move(State=#state{output_buf=OutputBuf}) ->
  case OutputBuf#buf.max_size > length(OutputBuf#buf.contents) of
    true ->
      case find_min_element(tuple_to_list(State#state.input_bufs),State) of
	nope ->
	  State;
	eod ->
	  OutputBuf = output_buf(State),
	  set_output_buf(set_eod(OutputBuf),State);
	{Element,NewState} ->
	  NewState#state{output_buf=add_element(Element,OutputBuf)}
      end;
    false ->
      State
  end.

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
		   eod ->
		     set_input_buf(I,NewBuf,State);
		   {OldValue,_} ->
		     if
		       First<OldValue ->
			 set_input_buf(I,NewBuf,State);
		       true ->
			 Acc
		     end
		 end
	     end
	 end
     end, eod, InputBufs).

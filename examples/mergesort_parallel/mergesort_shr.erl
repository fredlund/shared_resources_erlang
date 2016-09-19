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
    lists:map(fun (MaxSize) -> new_buffer(MaxSize) end, InputBufSpecs),
  OutputBuf =
    new_buffer(OutputBufSize),
  new_state(InputBufs,OutputBuf).

pre(_Msg={input,[N,Element]},State) ->
  ?TIMEDLOG("pre: ~p~n",[_Msg]),
  case Element of
    eod -> true;
    {data,_} -> true;
    _ -> false
  end 
    andalso N =< num_input_bufs(State) 
    andalso 
    begin
      InputBuf = input_buf(N,State),
      not(eod(InputBuf))
    end;
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
  BufSize = buf_size(OutputBuf),
  Eod = eod(OutputBuf),
  (BufSize > 0) orelse ((BufSize == 0) andalso Eod).

post(_Msg={input,[N,Element]},_Return,State) ->
  ?TIMEDLOG("post: ~p~n",[_Msg]),
  InputBuf = input_buf(N,State),
  NewState =
    case Element of
      eod ->
	set_input_buf(N,set_eod(InputBuf,true),State);
      {data,Data} ->
	set_input_buf(N,add_element(Data,InputBuf),State)
    end,
  OutputBuf = output_buf(NewState),
  case max_size(OutputBuf) == buf_size(OutputBuf) of
    true -> 
      NewState;
    false ->
      try_to_move(NewState)
  end;
post(_Msg={output,_},_Return,State) ->
  ?TIMEDLOG("post: ~p~n",[_Msg]),
  OutputBuf = output_buf(State),
  BufSize = buf_size(OutputBuf),
  if
    BufSize == 0 -> 
      set_output_buf(set_eod(OutputBuf,false),State);
    true ->
      try_to_move(set_output_buf(strip_first(OutputBuf),State))
  end.

return(State,_Msg={output,_},Result) ->
  ?TIMEDLOG("return: ~p~n",[_Msg]),
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

return_value(_Msg={output,_},State) ->
  ?TIMEDLOG("return_value: ~p~n",[_Msg]),
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
  io_lib:format("<~p,~w,~p>",[max_size(Buf),contents(Buf),eod(Buf)]).
      
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
  #buf{max_size=MaxSize,contents=[],eod=false}.

buf_size(#buf{contents=Contents}) ->
  length(Contents).

max_size(#buf{max_size=MaxBufSize}) ->
  MaxBufSize.

add_element(Element,Buf = #buf{contents=Contents}) ->
  Buf#buf{contents=Contents++[Element]}.

eod(#buf{eod=Eod}) ->
  Eod.

set_eod(Buf,Value) ->
  Buf#buf{eod=Value}.
    
contents(#buf{contents=Contents}) ->
  Contents.

set_contents(Contents,Buf) ->
  Buf#buf{contents=Contents}.

first_element(Buf) ->
  Contents = contents(Buf),
  Element = hd(Contents),
  NewBuf = set_contents(tl(Contents),Buf),
  {Element,NewBuf}.

strip_first(Buf) ->
  {_Element,NewBuf} = first_element(Buf),
  NewBuf.

try_to_move(State) ->
  OutputBuf = output_buf(State),
  case max_size(OutputBuf) > buf_size(OutputBuf) of
    true ->
      MinElement = find_min_element(tuple_to_list(input_bufs(State)),State),
      case MinElement of
	nope ->
	  State;
	eod ->
	  set_output_buf(set_eod(OutputBuf,true),State);
	{Element,NewState} ->
	  set_output_buf(add_element(Element,OutputBuf),NewState)
      end;
    false ->
      State
  end.

find_min_element(InputBufs,State) ->
  lists:foldl
    (fun ({I,InputBuf},Acc) ->
	 case Acc of
	   nope -> Acc;
	   _ ->
	     BufSize = buf_size(InputBuf),
	     Eod = eod(InputBuf),
	     if
	       BufSize==0, Eod==true -> Acc;
	       BufSize==0, Eod==false -> nope;
	       true ->
		 {First,NewBuf} = first_element(InputBuf),
		 case Acc of
		   {OldValue,_} when OldValue=<First -> Acc;
		   _ -> {First,set_input_buf(I,NewBuf,State)}
		 end
	     end
	 end
     end, eod, lists:zip(lists:seq(1,length(InputBufs)),InputBufs)).

-module(shr_java_controller).

%%-define(debug,true).
-include("debug.hrl").

-export([start/2,start_link/2]).
-export([init/1,handle_call/3,terminate/2]). 
-export([handle_cast/2,handle_info/2,code_change/3]).

-export([call/2]).

-record(state,{value_converter,free_pids,controller}).


start(Controller,Options) ->
  start(Controller,false,Options).

start_link(Controller,Options) ->
  start(Controller,true,Options).

start(Controller,Link,Options) ->
  ValueConverter =
    case proplists:get_value(result_converter,Options) of
      ValueConv when is_function(ValueConv) -> ValueConv;
      _ -> fun std_converter/1
    end,
  Fun = 
    if
      Link -> start_link;
      true -> start
    end,
  Result = 
    gen_server:Fun({local,?MODULE},?MODULE,[Controller,ValueConverter],[]),
  case Result of
    {ok,_} -> ok;
    Other -> 
      io:format
	("*** Error: ~p:start_link failed with ~p~n",
	 [?MODULE,Other])
  end,
  Result.

init([Controller,ValueConverter]) ->
  {ok,#state{value_converter=ValueConverter,free_pids=[],controller=Controller}}.

handle_call(Msg={_Method,_Args},From,State) ->
  {Pid,NewState} = find_free_pid(State),
  Pid!{call,Msg,From,self()},
  {noreply,NewState}.
  
handle_info({reply,Value,From,Pid},State) ->
  gen_server:reply(From,Value),
  {noreply,State#state{free_pids=[Pid|State#state.free_pids]}};
handle_info(_,State) ->
  {noreply,State}.

find_free_pid(State) ->
  case State#state.free_pids of
    [Pid|Rest] ->
      {Pid,State#state{free_pids=Rest}};
    _ ->
      Pid = spawn_link(fun () -> java_caller(State) end),
      {Pid,State}
  end.
      
java_caller(State) ->
  receive
    {call,{Method,Args},From,Parent} ->
      ?TIMEDLOG("will call java:call(~p,~p,~p)~n",[Controller,Method,Args]),
      try 
	convert
	  (java:call(State#state.controller,Method,Args),
	   State#state.value_converter) of
	Result ->
	  ?TIMEDLOG("result of ~p is ~p~n",[Call,Result]),
	  Parent!{reply,Result,From,self()},
	  java_caller(State)
      catch throw:{java_exception,Exc} ->
	  Parent!{reply,convert(Exc,State#state.value_converter),From,self()},
	  java_caller(State)
      end
  end.

handle_cast(_,State) ->
  {noreply,State}.

code_change(_,State,_) ->
  {ok,State}.

terminate(_,_) ->
  ok.

convert(Result,void) ->
  Result;
convert(Result,F) ->
  ?TIMEDLOG("will convert ~p~n",[Result]),
  try F(Result) of
      ConvertedValue ->
      ?TIMEDLOG("converted value is ~p~n",[ConvertedValue]),
      ConvertedValue
  catch Exception:Reason ->
      io:format("convert ~p failed with ~p:~pn",[Result,Exception,Reason]),
      throw(bad)
  end.

std_converter(Result) ->
  case java:is_object_ref(Result) of
    true ->
      ?TIMEDLOG("before calling instanceof on ~p~n",[Result]),
      case java:instanceof(Result,'java.lang.String') of
	true ->
	  ?TIMEDLOG("before calling string_to_list on ~p~n",[Result]),
	  ResultValue = java:string_to_list(Result),
	  ?TIMEDLOG("Result value is ~p~n",[ResultValue]),
	  ResultValue;
	false ->
	  case java:instanceof(Result,'java.lang.Throwable') of
	    true -> {exception,java:getClassName(Result)};
	    false -> Result
	  end
      end;
    false -> Result
  end.

call(Msg,Args) ->
  gen_server:call(?MODULE,{Msg,Args}).

      
		  

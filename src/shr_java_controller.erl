-module(shr_java_controller).

%%-define(debug,true).
-include("debug.hrl").

-export([start_link_client/2]).
-export([null_converter/1,std_converter/1]).

start_link_client(Controller,Options) ->
  ValueConverter =
    case proplists:get_value(result_converter,Options) of
      Fun when is_function(Fun) -> Fun;
      _ -> fun std_converter/1
    end,
  Pid = spawn_link(fun () -> java_caller1(Controller,ValueConverter) end),
  {ok,[Pid]}.

java_caller1(Controller,ValueConverter) ->
  receive
    Call ->
      ?TIMEDLOG("got call ~p~n",[Call]),
      {Method,Args} = shr_calls:msg(Call),
      ?TIMEDLOG("will call java:call(~p,~p,~p)~n",[Controller,Method,Args]),
      try convert(java:call(Controller,Method,Args),ValueConverter) of
	  Result ->
	  ?TIMEDLOG("result of ~p is ~p~n",[Call,Result]),
	  shr_calls:reply(Call,Result),
	  java_caller1(Controller,ValueConverter)
      catch throw:{java_exception,Exc} ->
	  shr_calls:reply(Call,convert(Exc,ValueConverter)),
	  java_caller1(Controller,ValueConverter)
      end
  end.

null_converter(Result) ->
  Result.

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

      
		  

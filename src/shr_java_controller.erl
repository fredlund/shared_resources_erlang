-module(shr_java_controller).

%%-define(debug,true).
-include("debug.hrl").

-export([start/2,start_link/2]).
-export([init/1,handle_call/3,terminate/2]). 
-export([handle_cast/2,handle_info/2,code_change/3]).

-export([call/2]).

-record(state,{value_converter,arg_converter,free_pids,controller}).


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
  ArgConverter =
    case proplists:get_value(arg_converter,Options) of
      ArgConv when is_function(ArgConv) -> ArgConv;
      _ -> fun std_arg_converter/2
    end,
  Fun = 
    if
      Link -> start_link;
      true -> start
    end,
  Result = 
    gen_server:Fun({local,?MODULE},?MODULE,[Controller,ValueConverter,ArgConverter],[]),
  case Result of
    {ok,_} -> ok;
    Other -> 
      io:format
	("*** Error: ~p:start_link failed with ~p~n",
	 [?MODULE,Other])
  end,
  Result.

init([Controller,ValueConverter,ArgConverter]) ->
  {ok,#state{value_converter=ValueConverter,arg_converter=ArgConverter,free_pids=[],controller=Controller}}.

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
  Node = java:node_id(State#state.controller),
  receive
    {call,_Call={Method,Args},From,Parent} ->
      ?TIMEDLOG
         ("will call java:call(~p,~p,~p)~n",
          [State#state.controller,Method,Args]),
      ConvertedArgs = convert_args(Node,Args,State#state.arg_converter),
      try 
	convert
	  (java:call(State#state.controller,Method,ConvertedArgs),
	   State#state.value_converter) of
	Result ->
	  ?TIMEDLOG("result of ~p is ~p~n",[_Call,Result]),
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

convert_args(Node,Args,F) when is_function(F) ->
  ?TIMEDLOG("will convert args ~p~n",[Args]),
  try lists:map(fun (Arg) -> F(Node,Arg) end, Args) of
      ConvertedArgs ->
      ?TIMEDLOG("converted args ~p~n",[ConvertedArgs]),
      ConvertedArgs
  catch Exception:Reason ->
      io:format("convert_args ~p failed with ~p:~p~nn",[Args,Exception,Reason]),
      io:format("Stacktrace:~n~p~n",[erlang:get_stacktrace()]),
      error(badarg)
  end.

convert(Result,void) ->
  Result;
convert(Result,F) ->
  ?TIMEDLOG("will convert ~p~n",[Result]),
  try F(Result) of
      ConvertedValue ->
      ?TIMEDLOG("converted value is ~p~n",[ConvertedValue]),
      ConvertedValue
  catch Exception:Reason ->
      io:format("convert ~p failed with ~p:~p~nn",[Result,Exception,Reason]),
      io:format("Stacktrace:~n~p~n",[erlang:get_stacktrace()]),
      error(badarg)
  end.

std_arg_converter(Node,Arg) ->
  case Arg of
    _ when is_list(Arg) ->
      java:list_to_string(Node,Arg);
    _ ->
      Arg
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
	    true -> 
              {exception,java:getClassName(Result)};
	    false -> 
              ?TIMEDLOG
                 ("not converting ~p~n",
                  [java:string_to_list(java:call(Result,toString,[]))]),
              ?TIMEDLOG
                 ("Classname=~p~n",[java:getClassName(Result)]),
              Result
	  end
      end;
    false -> Result
  end.

call(Msg,Args) ->
  gen_server:call(?MODULE,{Msg,Args}).

      
		  

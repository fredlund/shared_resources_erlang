-module(shr_debug).

%%-define(debug,true).
-include("debug.hrl").

-export([debug/1]).

debug(StartFun) when is_function(StartFun) ->
  shr_utils:setup_shr(),
  case shr_simple_supervisor:add_childproc(debugged,StartFun) of
    [Pid|_] -> debug1(Pid);
    Other -> debug1(Other)
  end;
debug(Resource) ->
  shr_utils:setup_shr(),
  debug1(Resource).

debug1(Resource) ->
  Pid =
    case Resource of
      P when is_pid(P) -> 
	P;
      A when is_atom(A) ->
	case shr_register:whereis(A) of
	  P when is_pid(P) ->
	    P;
	  undefined -> 
	    io:format("*** Error: ~p is not alive~n",[A]),
	    error(badarg)
	end
    end,
  monitor(process,Pid),
  debug(1,Pid,Resource,[]).

debug(Time,ResourcePid,Resource,OngoingCalls) ->
  timer:sleep(100),
  Self = self(),
  case check_finished_calls(ResourcePid,OngoingCalls) of
    {ok,RemainingOngoingCalls} ->
      case io:parse_erl_exprs(list_to_atom(integer_to_list(Time)++"> ")) of
	{ok,[{call,_,{atom,_,halt},[]}],_} ->
	  ok;
	{ok,[Form],_} ->
	  case parse_command(Form) of
	    {error,Reason} ->
	      io:format
		("The form ~p cannot be parsed due to ~p~n",
		 [Form,Reason]),
	      debug(Time,ResourcePid,Resource,RemainingOngoingCalls);
	    Call={_Operation,Args} when is_list(Args) ->
	      Pid = 
		spawn
		  (fun () ->
		       Result = shr_calls:call(Resource,Call),
		       Self!{self(),Call,Time,Result}
		   end),
	      monitor(process,Pid),
	      debug(Time+1,ResourcePid,Resource,[{Pid,Time,Call}|RemainingOngoingCalls])
	  end;
	_ -> 
	  io:format("did not understand that command~n",[]),
	  debug(Time,ResourcePid,Resource,RemainingOngoingCalls)
      end;
    _ -> 
      io:format("the resource is no longer alive; terminating...~n")
  end.

parse_command(Command) ->
  case Command of
    {call,_,{atom,_,Operation},Args} when is_list(Args) ->
      case parse_terms(Args) of
	Terms when is_list(Terms) ->
	  {Operation,Terms};
	Other ->
	  Other
      end;
    _ -> 
      {error,parse_call}
  end.

parse_terms([]) ->       
  [];
parse_terms([Hd|Rest]) -> 
  try erl_eval:expr(Hd,[]) of {value,Value,_} ->
      case parse_terms(Rest) of
	RestResult when is_list(RestResult) ->
	  [Value|RestResult];
	RestResult ->
	  RestResult
      end
  catch _:_ ->
      {error,{parse_argument,Hd}}
  end.

check_finished_calls(ResourcePid,OngoingCalls) ->
  receive
    {Pid,{Operation,Args},Time,Result} ->
      io:format
	("the call ~p(~s) at time ~p returned ~p~n",
	 [Operation,print_args(Args),Time,Result]),
      RemainingCalls = lists:keydelete(Pid,1,OngoingCalls),
      check_finished_calls(ResourcePid,RemainingCalls);
    {'DOWN',_,_,ResourcePid,Info} ->
      io:format
	("the resource terminated due to ~p~n",
	 [Info]);
    {'DOWN',_,_,Pid,Info} ->
      case lists:keyfind(Pid,1,OngoingCalls) of
	{_,Time,{Operation,Args}} ->
	  io:format
	    ("the call ~p(~s) at time ~p raised an exception due to ~p~n",
	     [Operation,print_args(Args),Time,Info]),
	  RemainingCalls = lists:keydelete(Pid,1,OngoingCalls),
	  check_finished_calls(ResourcePid,RemainingCalls);
	false ->
	  check_finished_calls(ResourcePid,OngoingCalls)
      end;
    Other ->
      io:format("strange message ~p received~n",[Other]),
      check_finished_calls(ResourcePid,OngoingCalls)
  after 0 -> {ok,OngoingCalls}
end.

print_args([]) ->
  "";
print_args([Arg]) ->
  io_lib:format("~p",[Arg]);
print_args([Arg|Rest]) ->
  io_lib:format("~p,~s",[Arg,print_args(Rest)]).

      


	 


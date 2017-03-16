-module(shr_debug).

%%-define(debug,true).
-include("debug.hrl").

-export([debug/1]).

debug(Resource) ->
  debug(Resource,[]).

debug(Resource,OngoingCalls) ->
  timer:sleep(100),
  Self = self(),
  RemainingOngoingCalls = check_finished_calls(OngoingCalls),
  case io:parse_erl_exprs('> ') of
    {ok,[{call,_,{atom,_,halt},[]}],_} ->
      ok;
    {ok,[Form],_} ->
      case parse_command(Form) of
	{error,Reason} ->
	  io:format("The form ~p cannot be parsed due to ~p~n",[Form,Reason]),
	  debug(Resource,RemainingOngoingCalls);
	Call={_Operation,Args} when is_list(Args) ->
	  Pid = 
	    spawn
	      (fun () ->
		   Result = shr_calls:call(Resource,Call),
		   Self!{self(),Call,Result}
	       end),
	  monitor(process,Pid),
	  debug(Resource,[{Pid,Call}|RemainingOngoingCalls])
      end;
    _ -> 
      io:format("did not understand that command~n",[]),
      debug(Resource,RemainingOngoingCalls)
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

check_finished_calls(OngoingCalls) ->
  receive
    {Pid,{Operation,Args},Result} ->
      io:format
	("the call ~p(~s) returned ~p~n",
	 [Operation,print_args(Args),Result]),
      RemainingCalls = lists:keydelete(Pid,1,OngoingCalls),
      check_finished_calls(RemainingCalls);
    {'DOWN',_,_,Pid,Info} ->
      {_,{Operation,Args}} = lists:keyfind(Pid,1,OngoingCalls),
      io:format
	("the call ~p(~s) raised an exception due to ~p~n",
	 [Operation,print_args(Args),Info]),
      RemainingCalls = lists:keydelete(Pid,1,OngoingCalls),
      check_finished_calls(RemainingCalls);
    Other ->
      io:format("strange message ~p received~n",[Other]),
      check_finished_calls(OngoingCalls)
  after 0 -> ok
end.

print_args([]) ->
  "";
print_args([Arg]) ->
  io_lib:format("~p",[Arg]);
print_args([Arg|Rest]) ->
  io_lib:format("~p,~s",[Arg,print_args(Rest)]).

      


	 


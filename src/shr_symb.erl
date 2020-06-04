-module(shr_symb).

-compile(export_all).

sfun(AppFun,CompFun,A) ->
  {'$sfun',AppFun,CompFun,A}.

is_sfun({'$sfun',_,_,_}) ->
  true;
is_sfun(_) ->
  false.

is_symb_var({var,N}) when is_integer(N) ->
  true;
is_symb_var(_) ->
  false.

exec_sfun({'$sfun',AppFun,_,Args}) ->
  case AppFun of
    _ when is_function(AppFun) -> apply(AppFun,Args);
    {M,F} when is_atom(M), is_atom(F) -> apply(M,F,Args)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_sfun(T) ->
  check_term(fun is_sfun/1,T).

is_symbolic(T) ->
  check_term(fun is_symb_var/1,T).

check_term(F,T) ->
  F(T) orelse check_term1(F,T).

check_term1(F,T) ->
  case T of
    _ when is_tuple(T) ->
      check_term1(F,tuple_to_list(T));
    [Hd|Tl] ->
      check_term(F,Hd) orelse check_term(F,Tl);
    _ when is_map(T) ->
      check_term1(F,maps:values(T));
    _ ->
      false
  end.

subst(VarMap,T) ->
  case T of
    {var,_} ->
      case lists:keyfind(T,1,VarMap) of
        false ->
          io:format
            ("~n*** ERROR: map ~p~ndoes not contain variable ~p~n",
             [VarMap,T]),
          error(bad);
        {_,Value} ->
          Value
      end;
    _ when is_tuple(T) ->
      list_to_tuple(subst(VarMap,tuple_to_list(T)));
    [Hd|Tl] ->
      [subst(VarMap,Hd)|subst(VarMap,Tl)];
    _ when is_map(T) ->
      maps:from_list(subst(VarMap,maps:to_list(T)));
    _ ->
      T
  end.

eval(PreT) ->
  T = subexpr_eval(PreT),
  case T of
    {'$sfun',_,_,Args} ->
      case (not(has_sfun(Args))) andalso (not(is_symbolic(Args))) of
        true ->
          exec_sfun(T);
        false ->
          T
      end;
    _ -> T
  end.

subexpr_eval(T) ->
  case T of
    _ when is_tuple(T) ->
      list_to_tuple(eval(tuple_to_list(T)));
    [Hd|Tl] ->
      [eval(Hd)|eval(Tl)];
    _ when is_map(T) ->
      maps:from_list(eval(maps:to_list(T)));
    _ ->
      T
  end.
                   
output_sfun(S) ->
  case S of 
    {'$sfun',_,OutputFun,Args} ->
      SFUnArgs = lists:map(fun (A) -> output_sfun_or_value(A) end, Args),
      case OutputFun of
        _ when is_function(OutputFun) ->
          apply(OutputFun,SFUnArgs);
        {M,F} ->
          apply(M,F,SFUnArgs);
        _ ->
          io:format("~n*** Error: not a function ~p~n",[S]),
          error(bad)
      end
  end.
                       
output_sfun_or_value(T) ->
  case is_sfun(T) of
    true ->
      output_sfun(T);
    false ->
      case T of
        null -> 
          "null";
        {var,N} when is_integer(N) ->
          "Call.v(\""++shr_test_cases_to_junit:symbVar(N)++"\")";
        _ -> 
          io_lib:format("~p",[T])
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert(T,Type) ->
  sfun(fun (T) -> T end,
       fun (T) -> "(("++Type++") "++T++")" end,
       [T]).
           
equalp(T1,T2) ->
  sfun(fun (E1,E2) -> E1==E2 end,
       fun (E1,E2) -> "("++E1++").equals("++E2++")" end,
       [T1,T2]).

refequalp(T1,T2) ->
  sfun(fun (E1,E2) -> E1==E2 end,
       fun (E1,E2) -> "(("++E1++") == ("++E2++"))" end,
       [T1,T2]).

andp(T1,T2) ->
  sfun(fun (E1,E2) -> E1 and E2 end,
       fun (E1,E2) -> "("++E1++") && ("++E2++")" end,
       [T1,T2]).
  
orp(T1,T2) ->
  sfun(fun (E1,E2) -> E1 or E2 end,
       fun (E1,E2) -> "("++E1++") || ("++E2++")" end,
       [T1,T2]).

leqp(T1,T2) ->
  sfun(fun (E1,E2) -> E1=<E2 end,
       fun (E1,E2) -> "("++E1++") <= ("++E2++")" end,
       [T1,T2]).

ltp(T1,T2) ->
  sfun(fun (E1,E2) -> E1<E2 end,
       fun (E1,E2) -> "("++E1++") < ("++E2++")" end,
       [T1,T2]).

geqp(T1,T2) ->
  sfun(fun (E1,E2) -> E1>=E2 end,
       fun (E1,E2) -> "("++E1++") => ("++E2++")" end,
       [T1,T2]).

gtp(T1,T2) ->
  sfun(fun (E1,E2) -> E1>E2 end,
       fun (E1,E2) -> "("++E1++") > ("++E2++")" end,
       [T1,T2]).

notp(T) ->
  sfun(fun (E) -> not(E) end,
       fun (E) -> "!("++E++")" end,
       [T]).

andp([]) ->
  true;
andp([T]) ->
  T;
andp([T1,T2]) ->
  andp(T1,T2);
andp([T|Rest]) ->
  andp(T,andp(Rest)).

orp([]) ->
  true;
orp([T]) ->
  T;
orp([T1,T2]) ->
  orp(T1,T2);
orp([T|Rest]) ->
  orp(T,orp(Rest)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


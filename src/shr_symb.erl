-module(shr_symb).

-compile(export_all).

sfun(AppFun,CompFun,A) ->
  {'$apply',AppFun,CompFun,A}.

is_sfun({'$apply',_,_,_}) ->
  true;
is_sfun(_) ->
  false.

is_symb_var({var,N}) when is_integer(N) ->
  true;
is_symb_var(_) ->
  false.

exec_sfun(Expr={'$apply',AppFun,_,Args}) ->
  Arity = length(Args),
  case AppFun of
    _ when is_function(AppFun,Arity) -> 
      apply(AppFun,Args);
    {M,F} when is_atom(M), is_atom(F) -> 
      apply(M,F,Args);
    _ ->
      io:format
        ("~n*** Error: expression ~p is malformed~n",[Expr]),
      error(bad)
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

eval(T,Map) ->
  try eval1(T,Map)
  catch Class:Reason ->
      io:format
        ("Could not evaluate term~n~p~nwith map~n~p~nStacktrace:~n~p~n",
         [T,Map,erlang:get_stacktrace()]),
      error(bad)
  end.

eval1(T,Map) ->
  case T of
    {'$apply',F1,F2,Args} ->
      exec_sfun({'$apply',F1,F2,eval1(Args,Map)});
    {var,_} ->
      case lists:keyfind(T,1,Map) of
        false ->
          io:format
            ("~n*** ERROR: map ~p~ndoes not contain variable ~p~n",
             [Map,T]),
          error(bad);
        {_,Value} ->
          Value
      end;
    _ when is_tuple(T) ->
      list_to_tuple(eval1(tuple_to_list(T),Map));
    [Hd|Tl] ->
      [eval1(Hd,Map)|eval1(Tl,Map)];
    _ when is_map(T) ->
      maps:from_list(eval1(maps:to_list(T),Map));
    _ ->
      T
  end.
                   
printSeqExpr(T) ->
  print1
    (T,
     fun ({var,N}) -> "Call.v(\""++shr_test_cases_to_junit:symbVar(N)++"\")" end).

print(T) ->
  print1(T,fun (X) -> io_lib:format("~p",[X]) end).
               
print1(T,VarPrinter) ->
  case T of
    {'$apply',_,OutputFun,Args} ->
      MyPrintFun = fun (X) -> print1(X,VarPrinter) end,
      case OutputFun of
        _ when is_function(OutputFun) ->
          apply(OutputFun,[MyPrintFun|Args]);
        {M,F} ->
          apply(M,F,[MyPrintFun|Args]);
        _ ->
          io:format("~n*** Error: not a function ~p~n",[T]),
          error(bad)
      end;
    {var,N} when is_integer(N) ->
      VarPrinter(T);
    _ when is_tuple(T) ->
      "{" ++ print_comma_list(tuple_to_list(T),VarPrinter) ++ "}";
    [Hd|Tl] ->
      "[" ++ print1(Hd,VarPrinter) ++ "|" ++ print1(Tl,VarPrinter) ++ "]";
    _ when is_map(T) ->
      "#{" ++ print_keyvalue_list(maps:to_list(T),VarPrinter) ++ "}";
    _ ->
      io_lib:format("~p",[T])
  end.

print_comma_list([],_) ->
  "";
print_comma_list([T],VarPrinter) ->
  print1(T,VarPrinter);
print_comma_list([T|Rest],VarPrinter) ->
  print1(T,VarPrinter)++","++print_comma_list(Rest,VarPrinter).

print_keyvalue_list([],_) ->
  "";
print_keyvalue_list([{Key,Value}],VarPrinter) ->
  print1(Key,VarPrinter) ++ "=>" ++ print1(Value,VarPrinter);
print_keyvalue_list([{Key,Value}|Rest],VarPrinter) ->
  print1(Key,VarPrinter) ++ "=>" ++ print1(Value,VarPrinter) ++ "," ++ print_keyvalue_list(Rest,VarPrinter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert(T,Type) ->
  sfun({?MODULE,jconv},
       fun (P,T) -> "(("++Type++") "++P(T)++")" end,
       [T]).
           
jconv(T) -> T.

equalp(T1,T2) ->
  sfun({?MODULE,jequal},
       fun (P,E1,E2) -> "("++P(E1)++").equals("++P(E2)++")" end,
       [T1,T2]).

jequal(E1,E2) -> E1==E2.

refequalp(T1,T2) ->
  sfun({?MODULE,jrefequalp},
       fun (P,E1,E2) -> "(("++P(E1)++") == ("++P(E2)++"))" end,
       [T1,T2]).

jrefequalp(E1,E2) -> E1==E2.

andp(T1,T2) ->
  sfun({?MODULE,jandp},
       fun (P,E1,E2) -> "("++P(E1)++") && ("++P(E2)++")" end,
       [T1,T2]).
  
jandp(E1,E2) -> E1 and E2.

orp(T1,T2) ->
  sfun({?MODULE,jorp},
       fun (P,E1,E2) -> "("++P(E1)++") || ("++P(E2)++")" end,
       [T1,T2]).

jorp(E1,E2) -> E1 and E2.

leqp(T1,T2) ->
  sfun({?MODULE,jleqp},
       fun (P,E1,E2) -> "("++P(E1)++") <= ("++P(E2)++")" end,
       [T1,T2]).

jleqp(E1,E2) -> E1=<E2.

ltp(T1,T2) ->
  sfun({?MODULE,jltp},
       fun (P,E1,E2) -> "("++P(E1)++") < ("++P(E2)++")" end,
       [T1,T2]).

jltp(E1,E2) -> E1<E2.

geqp(T1,T2) ->
  sfun({?MODULE,jgeqp},
       fun (P,E1,E2) -> "("++P(E1)++") => ("++P(E2)++")" end,
       [T1,T2]).

jgeqp(E1,E2) -> E1>=E2.

gtp(T1,T2) ->
  sfun({?MODULE,jgtp},
       fun (P,E1,E2) -> "("++P(E1)++") > ("++P(E2)++")" end,
       [T1,T2]).

jgtp(E1,E2) -> E1>E2.

notp(T) ->
  sfun({?MODULE,jnotp},
       fun (P,E) -> "!("++P(E)++")" end,
       [T]).

jnotp(E) -> not(E).

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


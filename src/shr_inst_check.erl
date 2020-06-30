-module(shr_inst_check).

-compile(export_all).

inst(S1,S2) ->
  case same_rounds(S1,S2) of
    true ->
      subst(S1,S2);
    false ->
      false
  end.

inst_check(S1,S2) ->
  same_rounds(S1,S2) andalso is_subst(subst_rounds(S1,S2)).

same_rounds([],[]) ->
  true;
same_rounds([],_) ->
  false;
same_rounds(_,[]) ->
  false;
same_rounds([First1|Rest1],[First2|Rest2]) ->
  structurally_same(First1,First2) andalso same_rounds(Rest1,Rest2).

structurally_same([],[]) ->
  true;
structurally_same([{Fun,_}|Rest1],[{Fun,_}|Rest2]) ->
  structurally_same(Rest1,Rest2);
structurally_same(_,_) ->
  false.

subst_rounds(Rounds1,Rounds2) ->
  subst_rounds(Rounds1,Rounds2,[]).
subst_rounds([],[],Subst) ->
  Subst;
subst_rounds([R1|Rest1],[R2|Rest2],Subst) ->
  NewSubst = subst(R1,R2,Subst),
  case is_subst(NewSubst) of
    true -> subst_rounds(Rest1,Rest2,NewSubst);
    _ -> false
  end.

subst(S1,S2) ->
  subst(S1,S2,[]).
subst([],[],Subst) ->
  Subst;
subst([{_,Args1}|Rest1],[{_,Args2}|Rest2],Subst) ->
  NewSubst = find_subst(Args1,Args2,Subst),
  case is_subst(NewSubst) of
    true -> subst(Rest1,Rest2,NewSubst);
    _ -> false
  end.

find_subst([],[],Subst) ->
  Subst;
find_subst([V1|Rest1],[V2|Rest2],Subst) ->
  case map_term(V1,Subst) of
    {ok,V} ->
      if
        V==V2 -> find_subst(Rest1,Rest2,Subst);
        true -> false
      end;
    false ->
      find_subst(Rest1,Rest2,[{V1,V2}|Subst])
  end.

map_term(V,Subst) ->
  case lists:keyfind(V,1,Subst) of
    {_,Value} -> {ok,Value};
    false -> false
  end.

is_subst(L) when is_list(L) ->
  true;
is_subst(_) ->
  false.


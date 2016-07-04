-module(test_to_junit).

-export([translate/5,dynamic_test_to_test/2]).

translate(Test,Module,TranslatorModule,Init,InitState) ->
  {ok,TSI} = 
    TranslatorModule:initial_translator_state(Init),
  {Includes,NewTS} = 
    build(Test,1,Module,InitState,TranslatorModule,TSI),
  io:format("State after bulding is~n~p~n",[NewTS]),
  output(Test,1,TranslatorModule,Includes,NewTS).

build(Test,N,Module,State,TranslatorModule,TranslatorState) ->
  build(Test,N,Module,State,TranslatorModule,TranslatorState,[]).

build([],_,_,_,_,TS,Includes) ->
  {lists:reverse(Includes),TS};
build([[Call={_Actor,{Command,Args}}]|Rest],N,Module,State,TranslatorModule,TranslatorState,Includes) ->
  NewState = Module:next_state(State, void, [Call], void),
  case TranslatorModule:junit_build_state
    ({Command,Args},N,State,NewState,TranslatorState) of
    {ok,NewTranslateState} ->
      build
	(Rest,N+1,Module,NewState,TranslatorModule,NewTranslateState,[true|Includes]);
    false ->
      build
	(Rest,N,Module,NewState,TranslatorModule,TranslatorState,[false|Includes])
  end;
build([{set,Var,Call={call,_,Command,Args}}|Rest],N,Module,State,TranslatorModule,TranslatorState,Includes) ->
  NewState = Module:next_state(State, Var, Call),
  io:format("Call ~p~n",[{Command,Args}]),
  case TranslatorModule:junit_build_state
    ({Command,Args},N,State,NewState,TranslatorState) of
    {ok,NewTranslateState} ->
      build
	(Rest,N+1,Module,NewState,TranslatorModule,NewTranslateState,[true|Includes]);
    false ->
      build
	(Rest,N,Module,NewState,TranslatorModule,TranslatorState,[false|Includes])
  end.

output([],_,_,_,_) ->
  "";
output([[{_Actor,{Command,Args}}]|Rest],N,TranslatorModule,[Include|Includes],TS) ->
  Call = {Command,Args},
  {String,N1} = 
    if
      Include -> {TranslatorModule:junit_output(Call,N,TS),N+1};
      true -> {"",N}
    end,
  RestString = output(Rest,N1,TranslatorModule,Includes,TS),
  if
    RestString=="" -> String;
    true -> String++"\n,"++RestString
  end;
output([{set,_,{call,_,Command,Args}}|Rest],N,TranslatorModule,[Include|Includes],TS) ->
  Call = {Command,Args},
  {String,N1} = 
    if
      Include -> {TranslatorModule:junit_output(Call,N,TS),N+1};
      true -> {"",N}
    end,
  RestString = output(Rest,N1,TranslatorModule,Includes,TS),
  if
    RestString=="" -> String;
    true -> String++"\n,"++RestString
  end.

dynamic_test_to_test(Test,GetInit) ->
  lists:foldr
    (fun (Call,{Calls,Init}) ->
	 case Call of
	   {init,{InitValue,_}} ->
	     {Calls,GetInit(InitValue)};
	   {final,_,_} ->
	     {Calls,Init};
	   {call,Module,F,[Args|_],_} ->
	     {[{call,Module,F,Args}|Calls],Init};
	   _ ->
	     io:format("*** Error: strange call ~p~n",[Call]),
	     throw(bad)
	 end
     end, {[],void}, Test).

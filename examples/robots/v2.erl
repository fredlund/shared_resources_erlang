-module(v2).

-compile(export_all).

-include("../../testing/src/fstate.hrl").

start(NodeId,_TS) ->  
  case java:new(NodeId,'ControlAccesoNavesMonitor',[]) of
    Exc = {java_exception,_} -> 
      java:report_java_exception(Exc),
      throw(bad);
    Controller ->
      tester:store_data(controller,Controller),
      Controller
  end.

started(TS,Controller) ->
  TS#fstate{global_state=Controller}.

test() ->
  Id = "test",
  CP =  ["/home/fred/gits/src/cctester/test/classes/",
%%	 "/home/fred/practica_1_complete/G-4F1M/110175/20140605-170257",
	 "/home/fred/practica_1_complete/G-4F1M/000999/20140522-143819/",
	 "/home/fred/gits/src/cctester/test/cclib.jar"],
  DataSpec =
    {robots,[4,1000]},
  WaitSpec =
    {fcfs,[]},
  TestingSpec = 
    {fsms,[{10,{robot_fsm,[4]}}]},  %% 10 robots for a system of 4 warehouses
  Options =
    [{needs_java,true},{cp,CP},{max_par,0},{id,Id},
     {start_fun,fun start/2},
     {global_state,void},      %% We could leave this out...
     {started_fun,fun started/2}],
  tester:test(Options,DataSpec,WaitSpec,TestingSpec).

test2() ->
  Id = "test",
  CP =  ["/home/fred/gits/src/cctester/test/classes/",
%%	 "/home/fred/practica_1_complete/G-4F1M/110175/20140605-170257",
	 "/home/fred/practica_1_complete/G-4F1M/000999/20140522-143819/",
	 "/home/fred/gits/src/cctester/test/cclib.jar"],
  DataSpec = {robots,[4,1000]},
  WaitSpec = {always,[]},
  TestingSpec = {fsms,{void,10,{robot_fsm,[4]},fun () -> ok end,fun () -> ok end}},
  Options = [{needs_java,true},{no_par,true},{cp,CP},{id,Id}],
  tester:test(Options,DataSpec,WaitSpec,TestingSpec).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Run all tests on a set of exercises
runtests() ->
  runtests("ControlAccesoNavesMonitor.class","/home/fred/cc_prac1_final").

runtests(TargetFile,EntregasDir) ->
  Entregas = find_entregas(TargetFile,EntregasDir),
  CP =
    ["/home/fred/gits/src/cctester/test/classes/",
     "/usr/share/java/junit4.jar",
     "/home/fred/lib/net-datastructures-5-0.jar",
     "/home/fred/gits/src/cctester/test/cclib.jar"],
  runtests_for_entregas(Entregas,CP).
  
runtests_for_entregas(Entregas,CP) ->
  lists:foreach
    (fun ({User,Group,Dir,_TimeStr,_Time}) ->
	 io:format
	   ("~n===============================================================~n"),
	 io:format
	   ("~nWill test user ~s in group ~s in ~s~n",
	    [User,Group,Dir]),
	 CPDir = [Dir|CP],

	 io:format("~n~nno progress; no parallel~n"),
	 io:format("---------------------------~n"),
	 PreOptions1 = [{enforce_progress,false},{no_par,true}],
	 Result1 =
	   run(User,Dir,PreOptions1,CPDir),

	 Result2 =
	   begin
	     io:format("~n~nprogress; no parallel~n"),
	     io:format("---------------------------~n"),
	     PreOptions2 = [{enforce_progress,true},{no_par,true}],
	     run(User,Dir,PreOptions2,CPDir)
	   end,

	 Result3 =
	   if
	     Result1, Result2 ->
	       io:format("~n~nprogress; parallel~n"),
	       io:format("---------------------------~n"),
	       PreOptions3 = [{enforce_progress,true},{no_par,false}],
	       run(User,Dir,PreOptions3,CPDir);
	     true ->
	       false
	   end,
	 
	 if
	   Result1, Result2, Result3 ->
	     io:format
	       ("No errors found for user ~p~n",
		[User]);
	   true ->
	     io:format
	       ("User ~p has errors: test1: ~p test2: ~p test3: ~p~n",
		[User,Result1,Result2,Result3])
	 end,
	 Result3
     end, Entregas).

run(User,Dir,PreOptions,CP) ->
  DataSpec = {robots,[4,1000]},
  WaitSpec = {always,[]},
  TestingSpec = {robot_commands,[10,4]},
  Options = [{needs_java,true},{cp,CP},{id,User}|PreOptions],
  tester:test(Options,DataSpec,WaitSpec,TestingSpec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Should return a list of <group,matricula,time> entries identifying
%% the last successful entrega
find_entregas(LFile,Target) ->
  WildCard = Target++"/*",
  io:format
    ("find_entregas: wildcard=~s~n",
     [WildCard]),
  lists:foldl
    (fun (Dir,Files) ->
	 Group = filename:basename(Dir),
	 %%io:format
	   %%("group is ~p for ~p: recursing over ~p~n",[Group,Dir,Target]),
	 find_group_entregas(LFile,Group,Dir)++Files
     end,[],filelib:wildcard(WildCard)).

copy_entregas(Entregas,ToDir) ->
  ok = filelib:ensure_dir(ToDir),
  lists:foreach
    (fun (Entrega={Student,Group,FromPath,Best,Time}) ->
	 ToPath = ToDir++"/"++Group++"/"++Student++"/"++Best,
	 ok = filelib:ensure_dir(ToPath),
	 Cmd = "cp -r -p -n "++FromPath++" "++ToPath,
	 os:cmd(Cmd)
     end, Entregas).

find_group_entregas(LFile,Group,GroupTarget) ->
  lists:foldl
    (fun (Dir,Files) ->
	 User = filename:basename(Dir),
	 case has_valid_entrega(LFile,Group,User,Dir) of
	   {ok,Entrega} ->
	     [Entrega|Files];
	   _ ->
	     io:format
	       ("~n*** Warning: no valid entrega for user ~p in group ~p~n",
		[User,Group]),
	     Files
	 end
     end,[],filelib:wildcard(GroupTarget++"/*")).

has_valid_entrega(LFile,Group,User,Dir) ->
  case lists:foldl
    (fun (EntregaDir,Best) ->
	 TimeStr = filename:basename(EntregaDir),
	 Time = to_dtime(TimeStr),
	 case filelib:wildcard(EntregaDir++"/"++LFile) of
	   [] -> Best;
	   _ -> update_best(Time,filename:absname(EntregaDir),User,Group,TimeStr,Best)
	 end
     end, void, filelib:wildcard(Dir++"/*")) of
    void -> false;
    Result -> {ok,Result}
  end.

update_best(Time,Dir,User,Group,TimeStr,void) ->
  {User,Group,Dir,TimeStr,Time};
update_best(Time,Dir,User,Group,TimeStr,{User2,Group2,Dir2,TimeStr2,Time2}) ->
  case cmp_times(Time,Time2) of
    greater -> {User,Group,Dir,TimeStr,Time};
    _ -> {User2,Group2,Dir2,TimeStr2,Time2}
  end.
      
cmp_times({Y1,M1,D1,H1,Min1,S1},{Y2,M2,D2,H2,Min2,S2}) ->
  if
    Y1>Y2 -> greater;
    Y2>Y1 -> lesser;
    M1>M2 -> greater;
    M2>M1 -> lesser;
    D1>D2 -> greater;
    D2>D1 -> lesser;
    H1>H2 -> greater;
    H2>H1 -> lesser;
    Min1>Min2 -> greater;
    Min2>Min1 -> lesser;
    S1>S2 -> greater;
    S2>S1 -> lesser;
    true -> equal
  end.

to_dtime([Y1,Y2,Y3,Y4,M1,M2,D1,D2,_,H1,H2,Min1,Min2,S1,S2]) ->
  {Y,_}=string:to_integer([Y1,Y2,Y3,Y4]),
  {M,_}=string:to_integer([M1,M2]),
  {D,_}=string:to_integer([D1,D2]),
  {H,_}=string:to_integer([H1,H2]),
  {Min,_}=string:to_integer([Min1,Min2]),
  {S,_}=string:to_integer([S1,S2]),
  {Y,M,D,H,Min,S}.

find_late_entregas(Dir,Description) ->
  lists:foreach
    (fun ({LabNo,File,CutOffTime}) ->
	 LabDir = io_lib:format("~s/Lab~p",[Dir,LabNo]),
	 io:format("Checking Lab ~p in directory ~s~n",[LabNo,LabDir]),
	 Entregas = find_entregas(File,LabDir),
	 io:format("Number of entregas is ~p~n",[length(Entregas)]),
	 LateEntregas =
	   lists:filter
	     (fun ({User,Group,Dir,TimeStr,Time}) ->
		  case cmp_times(Time,CutOffTime) of
		    lesser -> false;
		    Other -> true
		  end
	      end, Entregas),
	 io:format("Number of late entregas is ~p~n",[length(LateEntregas)]),
	 case length(LateEntregas) > 0 of
	   true ->
	     io:format("Cutoff time: ~p~n",[CutOffTime]),
	     lists:foreach
	       (fun ({User,Group,Dir,TimeStr,Time}) ->
		    io:format
		      ("*** User ~p from Group ~p has late entrega "++
			 "of lab ~p~n        at time ~p~n",
		       [User,Group,LabNo,Time])
		end, LateEntregas);
	   false -> ok
	 end
     end,
     Description).



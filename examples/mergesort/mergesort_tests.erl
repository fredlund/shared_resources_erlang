-module(mergesort_tests).

-include_lib("eqc/include/eqc.hrl").

-export([test/0]).
-export([debug/0]).

buffers_2() ->
  {system,
   [],
   [in,out],
   [{r1,shr_utils:start_link_resource(mergesort_2_shr)},
    {r2,shr_utils:start_link_resource(mergesort_2_shr)},
    {r3,shr_utils:start_link_resource(mergesort_2_shr)}],
   [],
   fun ({in,[1,Msg]}) -> {r1,left,[Msg]};
       ({in,[2,Msg]}) -> {r1,right,[Msg]};
       ({in,[3,Msg]}) -> {r2,left,[Msg]};
       ({in,[4,Msg]}) -> {r2,right,[Msg]};
       ({out,[]}) -> {r3,out,[]}
   end,
   [{{r1,out},{r3,left}}, {{r2,out},{r3,right}}]
  }.

       
    
   
   
  

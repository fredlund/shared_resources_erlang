-module(shr_fsm).

-include("../tester.hrl").

-callback initial_state(any(),any(),[any()]) -> any().
-callback precondition(any(),any(),any(),{atom(),any(),[any()]}) -> boolean().
-callback command(any(),any(),any()) -> any().
-callback next_state(any(),any(),any(),{any(),atom(),[any()]}) -> any().
  

  

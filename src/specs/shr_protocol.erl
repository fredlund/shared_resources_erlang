-module(shr_protocol).

-callback initial_state(any(),[any()]) -> {ok,any()}.
-callback postcondition(any(),any(),any()) -> any().
-callback next_state(any(),any(),any()) -> any().

  

  

-module(shr_corr_implementation).

-callback initial_state([any()],any()) -> any().
-callback postcondition(any(),any(),any(),any()) -> any().
-callback next_state(any(),any(),any(),any()) -> any().

  

-module(shr_gnr_implementation).

-callback initial_state([any()],any()) -> any().
-callback start(any(),any()) -> any().
-callback started(any(),any()) -> any().   
-callback precondition(any(),any()) -> any().
-callback command(any(),any()) -> any().
-callback next_state(any(),any(),any()) -> any().

  

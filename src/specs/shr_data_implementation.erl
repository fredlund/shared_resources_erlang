-module(shr_data_implementation).

-callback initial_state([any()],any()) -> {ok,any()} | any().
-callback pre(resource:call(),any()) -> boolean().
-callback cpre(resource:call(),any()) -> boolean().
-callback post(resource:call(),any(),any()) -> any().

  

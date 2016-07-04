-module(shr_wait_implementation).

-callback initial_state([any()],any()) -> {ok,any()} | any().
-callback new_waiting(resource:call(), any(), any()) -> {any(), any()}.
-callback priority_enabled(resource:call(), any(), any(), any()) -> boolean().
-callback post_waiting(resource:call(), any(), any(), any()) -> any().

  

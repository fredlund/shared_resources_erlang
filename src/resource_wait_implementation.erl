-module(resource_wait_implementation).

-callback init([any()]) -> {ok,any()} | any().
-callback new_waiting(resource:call(), any(), any()) -> {any(), any()}.
-callback priority_enabled(resource:call(), any(), any(), any()) -> boolean().
-callback post_waiting(resource:call(), any(), any()) -> {any(), any()}.

  

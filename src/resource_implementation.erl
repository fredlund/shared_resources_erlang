-module(resource_implementation).

-callback pre(resource:call(),any()) -> boolean().
-callback cpre(resource:call(),any()) -> boolean().
-callback post(resource:call(),any()) -> {any(),any()}.
-callback new_waiting(resource:call(), any(), any()) -> {any(), any()}.
-callback priority_enabled(resource:call(), any(), any(), any()) -> boolean().
-callback post_waiting(resource:call(), any(), any()) -> {any(), any()}.

  

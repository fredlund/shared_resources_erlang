-module(resource_data_implementation).

-callback init([any()],any()) -> {ok,any()} | any().
-callback pre(resource:call(),any()) -> boolean().
-callback cpre(resource:call(),any()) -> boolean().
-callback post(resource:call(),any()) -> any().

  

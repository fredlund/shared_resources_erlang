-module(control).

-include("tester.hrl").
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

debug() ->
  shr_debug:debug
    (fun () ->
	 shr_gen_resource:start({control_shr,[]},shr_always,[])
     end).


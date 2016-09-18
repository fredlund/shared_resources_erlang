-module(mergesort_tests).

-include_lib("eqc/include/eqc.hrl").

-export([sim/1]).
-export([test/0]).

-export([inputs/1]).
-export([outputs/1]).
-export([links/1]).

%% A configuration is a description of the inputs, outputs and
%% links to components:
%% {component,N,[InputSize],OutputSize} --
%% specifies component N with a list of input components (with buffer sizes
%% specified), and an output buffer of specified size;
%% {input,N,I} -- specifies an input process for input M of component N
%% {output,N} -- specifies an output process for the output of component N
%% {link,N,M,I} -- specifies a link process between the 
%% output of component N and input I of component M

sim(Config) ->
  valid_config(Config),

  %% Start the supervisor
  shr_simple_supervisor:restart(self()),

  Components = components(Config),

  ControllerOpts = 
    [
     {data_spec,mergesort_shr}
     ,{waiting_spec,shr_always}
    ],

  %% We need a set of controllers, first start a resource and then wrap it in
  %% a generic Erlang resource implementation
  Controllers = 
    lists:map
      (fun ({component,N,InputSizes,OutputSize}) ->
	   Opts = [{output_buf_size=OutputSize},{input_buf_spec=InputSizes}],
	   [Controller] = 
	      shr_simple_supervisor:add_childproc
		(mergesort_shr, 
		 fun () ->
		     shr_gen_resource:start_link(ControllerOpts,Opts)
		 end),
	   {N,Controller}
       end, Components),

  %% Finally start the simulation
  mergesort_sim:run(Config,Controllers).
  
components(Config) ->
  lists:filter
    (fun (Element) ->
	 case Element of
	   {component,_,_,_} -> true;
	   _ -> false
	 end
     end, Config).

outputs(Config) ->
  lists:filter
    (fun (Element) ->
	 case Element of
	   {output,_} -> true;
	   _ -> false
	 end
     end, Config).

inputs(Config) ->
  lists:filter
    (fun (Element) ->
	 case Element of
	   {input,_,_} -> true;
	   _ -> false
	 end
     end, Config).

links(Config) ->
  lists:filter
    (fun (Element) ->
	 case Element of
	   {link,_,_,_} -> true;
	   _ -> false
	 end
     end, Config).

inputs({component,N,_,_},Config) ->
  lists:filter
    (fun (Element) ->
	 case Element of
	   {input,N,_} -> true;
	   {link,_,N,_} -> true;
	   _ -> false
	 end
     end, Config).

output({component,N,_,_},Config) ->
  lists:filter
    (fun (Element) ->
	 case Element of
	   {output,N} -> true;
	   {link,N,_,_} -> true;
	   _ -> false
	 end
     end, Config).

valid_config(Config) ->
  Components = components(Config),
  ComponentNumbers = 
    lists:foldl
      (fun ({component,N,_,_},Acc) -> sets:add_element(N,Acc) end,
       sets:new(), Components),
  ComponentsLength = length(Components),
  ComponentsLength = sets:size(ComponentNumbers),
  lists:foreach
    (fun (Component) ->
	 Inputs = inputs(Component,Config),
	 true = covers_inputs(Inputs,Component),
	 [_] = output(Component,Config)
     end, Components),
  Outputs = outputs(Config),
  RemainingOutputs =
    lists:foldl
      (fun (Output,Acc) ->
	   ComponentNum = output_component(Output),
	   sets:delete_element(ComponentNum,Acc)
       end, Outputs),
  0 = sets:size(RemainingOutputs),
  Inputs = inputs(Config),
  lists:foreach
    (fun (Input) ->
	 InputComponent = input_component(Input),
	 true = sets:is_element(InputComponent)
     end, Inputs).

covers_inputs(Inputs,{component,_,Inputs,_}) ->
  NumInputs = length(Inputs),
  [] = 
    lists:foldl
      (fun (Input,Acc) ->
	   InputNum = input_num(Input),
	   lists:delete(InputNum,Acc)
       end, lists:seq(1,NumInputs), Inputs).

input_num({input,_,I}) -> I;
input_num({link,_,_,I}) -> I.

input_component({input,N,_}) -> N;
input_component({link,_,N,_}) -> N.

output_component({output,N}) -> N;
output_component({link,N,_,_}) -> N.

%% A simple test

test() ->  
  sim
    (
    [
     {component,1,[1,2],1}
    ,{input,1,1},{input,1,2}
    ,{output,1}
    ]
    ).

     
     
     
				
  

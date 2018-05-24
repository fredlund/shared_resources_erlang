-record(fstate,
        {
          machines,
          global_state,
          options,
          start,
          started,
          stop,
          global_constraint,
          blocked
        }).

-record(machine,
	{
	  id,
	  module,
	  weight,
	  state,
	  options
	}).
	

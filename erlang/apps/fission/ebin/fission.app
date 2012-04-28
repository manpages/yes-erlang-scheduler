{application,fission,

	[{description,"VeryPositive Fission K/V storage"},
	
	{vsn,"0.1"},
	
	{modules,[
	
		fission,
		fission_table,
		
		fission_util,
		
		fission_syn,
		fission_asyn
		
	]},
	
	{registered, [fission]},
	
	{mod,{fission,[]}},
	
	{env, [
	
		{data_dir, "data"}
		
	]},
	
	{applications,[
		kernel,
		stdlib
	]}]}.

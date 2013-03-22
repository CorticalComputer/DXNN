%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.
-define(BEHAVIORAL_TRACE,false).
-define(INTERACTIVE_SELECTION,false).
-record(af,{name,parameters,neural_constraints}).
-record(lt,{name,parameters,neural_constraints}).
-record(schema,{sensors,actuators}).
-record(stat,{morphology,specie_id,avg_subcores,subcores_std,avg_neurons,neurons_std,avg_fitness,fitness_std,max_fitness,min_fitness,avg_diversity,validation_fitness,evaluations,time_stamp}).
-record(fingerprint,{cf,ct,constraint,history,tot_subcores,tot_substrates,tot_neurons,subcore_pattern,neuro_patterns}).
-record(trace,{stats=[],tot_evaluations=0,step_size=500,next_step=500}).
%-record(stats,{morphology,avg_subcores=[],avg_neurons=[],avg_fitness=[],max_fitness=[],min_fitness=[],avg_diversity=[],evaluations=[],tot_evaluations=0,step_size=500,next_step=500}).
-record(champion,{hof_fingerprint,id,fitness,main_fitness,tot_n,evolvability,robustness,brittleness,generation,behavioral_differences}).
-record(population,{id,type,trace=#trace{},specie_ids=[],topspecie_ids,polis_id,evo_strat,seed_agent_ids=[],seed_specie_ids=[],objectives=[fitness],phylogenetic_tree_depth=inf}). %hof:[{FitnessVector,Id}...]
-record(specie,{id,morphology,constraint,fingerprint,trace=#trace{},cur_stat,avg_fitness,stagnation_factor,dx_ids=[],dead_pool=[],population_id,seed_agent_ids=[],hof_distinguishers=[tot_n],specie_distinguishers=[tot_n],hall_of_fame=[]}).
-record(dx,{id,cx_id,n_ids,specie_id,constraint,morphology,heredity_type,neural_type,generation,fitness,main_fitness,profile,summary,evo_hist,mode,evo_strat,offspring_ids=[],parent_ids=[],champion_flag=[false],evolvability=0,brittleness=0,robustness=0, evolutionary_capacitance=0,behavioral_trace}).
-record(cortex,{id,sensors,actuators,cf,ct,type,plasticity,pattern,cids,su_id,link_form,substrate_link_form,dimensions,densities,categories=[],generation}). %%%id = {{LayerIndex,NumId},subcore}
-record(subcore,{id,i,o,cf,ct,type,plasticity,pattern,cids,su_id,link_form,dimensions,densities,generation}).
-record(neuron,{id,ivl,i,ovl,o,ro,type,dwp,su_id,generation,parameters=[],preprocessor,signal_integrator,activation_function,postprocessor,plasticity,heredity_type,mlffnn_module}). %%%id = {{LayerIndex,NumId},neuron}
-record(neurode,{id,weights,af,bias,parameters=[]}).
%-record(hall_of_fame,{id,identifier,fitness}).%id={population_id,agent_id}
%-record(citizen,{id,dx_id,fitness,fitness_profile,specie_id}).
-record(summary,{type,tot_neurons,tot_n_ils,tot_n_ols,tot_n_ros,af_distribution,fitness}).
-record(sensor,{name,id,format,tot_vl,parameters,objects=[],vis=[]}).
-record(actuator,{name,id,format,tot_vl,parameters,objects=[],vis=[]}).
-record(sCT,{name,id,format,tot_vl,parameters}).
-record(sCF,{name,id,format,tot_vl,parameters}).
-record(tag,{name,id,format,tot_vl,parameters}).

-record(polis,{id,scape_ids=[],population_ids=[],specie_ids=[],dx_ids=[],parameters=[]}).
-record(scape,{id,type,physics,metabolics,sector2avatars,avatars=[],plants=[],walls=[],pillars=[],laws=[],anomolies=[],artifacts=[],objects=[],elements=[],atoms=[],scheduler=0}).
-record(sector,{id,type,scape_pid,sector_size,physics,metabolics,sector2avatars,avatars=[],plants=[],walls=[],pillars=[],laws=[],anomolies=[],artifacts=[],objects=[],elements=[],atoms=[]}).
-record(avatar,{id,sector,morphology,type,specie,energy=0,health=0,food=0,sound,gestalt,spear,age=0,kills=0,loc,direction,r,mass,objects,vis=[],state,stats,actuators,sensors}).

-record(object,{id,sector,type,color,loc,pivot,elements=[],parameters=[]}).
-record(circle,{id,sector,color,loc,pivot,r}).
-record(square,{id,sector,color,loc,pivot,r}).
-record(line,{id,sector,color,loc,pivot,coords}).
-record(e,{id,sector,v_id,type,loc,pivot}).%pivot
-record(a,{id,sector,v_id,type,loc,pivot,mass,properties}).

-record(constraint,{
	morphology = pole2_balancing3, %[pole2_balancing3|flatlander...]
	heredity_types = [darwinian],
	actuators = [],
	sensors = [],
	cx_types, %[hypercube,neural]
	cx_plasticity, %[iterative,none]
	cx_linkform = recursive, %[recursive,feedforward,self_recursive]
	sc_types = [hypercube,neural,aart,layered,modular], %[hypercube,neural]
	sc_neural_plasticity = [none], %[none,modulated]
	sc_hypercube_plasticity  = [none], %[none,abc,iterative]
	sc_neural_linkform = recursive, %[recursive,feedforward,self_recursive,jordan_recursive,sj_recursive]
	sc_hypercube_linkform = [feedforward],
	neural_types = [standard], %[standard]
	neural_pfs = [none],%[none,modulated]
	neural_afs = [tanh,gaussian,sin,absolute,sgn,linear,log,sqrt], %[tanh,gaussian,sin,linear,absolute,sgn,log,sqrt]
	neural_signal_integrators = [dot],
	neural_preprocessors = [none],
	neural_postprocessors = [none],
	specie_distinguishers=[tot_n],%[tot_n,tot_inlinks,tot_outlinks,tot_sensors,tot_actuators,pattern,tot_tanh,tot_sin,tot_cos,tot_gaus,tot_lin...]
	hof_distinguishers=[tot_n],%[tot_n,tot_inlinks,tot_outlinks,tot_sensors,tot_actuators,pattern,tot_tanh,tot_sin,tot_cos,tot_gaus,tot_lin...]
	objectives = [main_fitness,inverse_tot_n] %[main_fitness,problem_specific_fitness,other_optimization_factors...]
}).

-record(hall_of_fame,{fitness,agents}).

-record(agent_evo_strat,{
	functional = true,
	strategies_mutation_prob = 0,%Probability of mutating an evolutionary strategies parameter, increments and decrements based on how far from the edge: 0-100%.
	mutation_operators = modular_mutator:mos(),
	tuning_mutation_range = math:pi(),%From -2Pi to Pi, starts of with a range of -Pi to Pi.
	tuning_annealing_flag = false,%true|false
	annealing_parameter = 0.5,%Standard: math:pi()*math:pow(0.5,Age) tuning_mutation_range*math:pow(anealing_parameter,Age), ranges from 0 to 1, where 1 is no anealing, and 0 is stop evolving.
	topological_mutation_prob = 1,%Standard: Range from 1 to 1/sqrt(Tot_Neurons), Multiplier range: 1 to sqrt(Tot_Neurons).
	topological_annealing_flag = false,%true|false
	neuron_perturbation_prob = 1,%Standard: 1/sqrt(TotNeurons) Probability of choosing a neuron for perturbation, multiplier: 1 to sqrt(TotNeurons)
	weight_perturbation_prob = 1,%Standard: 1/sqrt(TotWeights) Probability of choosing a weight for perturbation, multiplier: 1 to sqrt(TotWeights)
	active_neuron_selection_type = dynamic_random,%What type to use to select active neurons: %[dynamic|active|recent|current|all|dynamic_random|active_random|recent_random|current_random|all_random]
	active_neuron_selection_parameter = undefined %Augment the parameter of selection, dependent on type
}).

-record(specie_evo_strat,{
	strategies_mutation_prob = 0,%Probability of mutating an evolutionary strategies parameter: 0-100%
	evolution_type = memetic,%memetic|genetic
	selection_type = competition,%competition|top3
	selection_threshold = 0.5,%Top 10% - 90% survival.
	diversity_importance = 0,%Weight of the fingerprint difference, importance for genetic diversity to be part of the fitness function.
	revenent_prob = 0.1 %probability that a deadpool NN will reavaluate, 10% - 90%
}).

-record(experiment,{
	id,
	backup_flag = true,
	pm_parameters,
	init_constraints,
	progress_flag=in_progress,
	trace_acc=[],
	run_index=1,
	tot_runs=10,
	notes,
	started={date(),time()},
	completed,
	interruptions=[]
}).

-record(pmp,{
	op_mode=gt,
	population_id=test,
	survival_percentage=0.5,
	specie_size_limit=10,
	init_specie_size=10,
	polis_id = mathema,
	generation_limit = 100,
	evaluations_limit = 100000,
	fitness_goal = inf,
	benchmarker_pid,
	evolution_type,
	selection_type,
	specie_constraint
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LEGEND %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%======================================POLIS======================================
%scape_id:[Id1...Idn]
%population_ids: [Id1...Idn]
%specie_ids:[Id1...Idn]
%dx_ids:[Id1...Idn]
%parameters:???

%======================================SCAPE======================================
%sector_ids:[Id1...Id2]
%type:[Dimensions,Sim_Level]
%championdx_ids: [{OriginalDX_Id1,NewDX_Id1},{OrignalDX_Id2,NewDX_Id2}...]

%======================================SECTOR======================================
%scape_id: Id
%type:[Dimensions,Sim_Level]
%atom_ids:[Id1...Idn]
%element_ids:[Id1...Idn]
%object_ids:[Id1...Idn]
%avatar_ids:[Id1...Idn]

%======================================POPULATION======================================
%id: UniqueId
%Type:test,dx,cyber_warfare,forex...
%specie_ids: [{UniqueId1,Morphology}...{UniqueIdn,Morphology}]
%top_species: [{Fitness1,Id1},{Fitness2,Id2}...{Fitnessn,Idn}]
%polis_id: Id 
%scpecie_trace: avg_fitness,avg_diversity...
%avg_diversity: [[{Specie_Id1,Diversity1}...{Specie_Idn,Diversity2}]....]

%======================================SPECIE======================================
%id: UniqueId
%dx_ids: [Id1,Id2...Idn]
%topdx_ids: [Id1,Id2...Idn]
%championdx_ids: [Id1,Id2...Idn]
%population_id: Id
%last_improved: Number of generations ago an improvement occured. Over 3 is considered stagnant and the population if it is not in top 3, dies out.
%hall_of_fame:[{Fitness:List,Agent_Id}...]
%stagnation_factor|last_improved:Last time the hall_of_fame was improved, with a new agent entrance.
%dead_pool:[{Fitness:List,Agent_Id}...]
%======================================DX======================================
%id = UniqueId
%citizenship_id = citizen_id
%fitness = Float
%fitness_profile = List
%specie_id = Id of the specie the DX belongs to.
%summary = [tot_substrates,tot_subcores,tot_neurons,tot_sclinks,tot_nlinks,tot_sigmoids,tot_sines,tot_guassians,tot_linears,tot_hebbians]
%evo_hist: Evolutionary History in a list: [{Generation,TypeOfMutagen,Applied_On,From,To}...]
%mode: [online,offline]. Active when summoned, inactive when not summoned.
%parent_id: agent_Id of the parent agent (or parents?).
%======================================CORTEX======================================
%id: {{0,0},core,UniqueId}
%type:[neural|hypercube|aart]
%link_form:[feedforward|recursive]
%Sensory_Pool: [Sensor1...SensorN] Sensor: {sensor,Name,Id,TotVL,Parameters}
%Actuator_Pool:[Actuator1...ActuatorN] Actuator: {actuator,Name,Id,TotVL,Parameters}
%dimensions:N, densities:[D1,D2,D3...DN]
%CF:
%	neural:		[{Actuator1,[N_Id1...N_Idn]},{Actuator2,[N_Id1...N_Idn]}...]
%	hypercube:	[{sCF,[N_Id1...N_Idn]}...]
%CT:
%	neural:		[{Sensor1,[{N_Id1,FilterTag1},{N_Id2,FilterTag2}...]}...] FilterTag:{single,Index} | {block,VL}
%	hypercube:	[{sCT,[{N_Id1,FilterTag1},{N_Id2,FilterTag2}...]}...] FilterTag:{single,Index} | {block,VL}

%neural: [{NeuronId1,VL,Tag}] Tag: [{I_Id,Index}...]
%neuromodulator:[yes|no]
%pattern:[{Layer_Id1,[Id1...Idn]}...{Layer_Idn,[Id1...Idn]}]
%Substrate encoding: X density = n, Y density = k, Z density = p, T density = l
%D1 = [{X1,[W1,W2...Wn]}...{Xm,[W1,W2...Wn]}]
%D2 = [{Y1,D1}...{Yk,D1}]
%D3 = [{Z1,D2}...{Zp,D2}]
%D4 = [{T1,D3}...{Tl,D3}]
%[{D3,[{D2,[{D1,{o,[W1,W2...Wn]}},{D1,{o,[W1,W2...Wn]}}]},{D2,[{D1,{o,[W1,W2...Wn]}},{D1,{o,[W1,W2...Wn]}}]}]}
% {D3,[{D2,[{D1,{o,[W1,W2...Wn]}},{D1,{o,[W1,W2...Wn]}}]},{D2,[{D1,{o,[W1,W2...Wn]}},{D1,{o,[W1,W2...Wn]}}]}]}]

%[{[D3,D2,D1],[{o,[W1,W2,W3]},{o,W1,W2,W3}]},
% {[D3,D2,D1],[{o,[W1,W2,W3]},{o,W1,W2,W3}]}]
%SubCore/Core|Cortex inputs/output formats:
%	{From_PId,forward,[Val1,Val2...Valn]}
%Sensor/Actuator: 
%Formats:
%	undefined, simple vector with no geometric information
%	no_geo
%	{symetric,[R1,R2...Rk],[Val1...Valn]} where n == R1*R2*...Dk and k = dimension
%	{asymetric,[[R1..Rp],[R1..Rt]],[Val1...Valn]} where lists:sum(lists:flatten([[R1...Rp],[R1..Rt]])) == n, and depth = Dimension.
%	coorded, every val comes with its own coord tuple: {Coord,Val}. The coord is a list, thus specifying the dimensionality.
%Combining:
%	Determine highest dimension input
%	Every input gets its own dimension
%	If input has lower dimension than SHyperCube -1, then it is put together with
%	Perhaps it is better to... give each input its own hypercube, merging 2d or 1d outputs in a unification hypercube, and outputing to output hypercubes

%======================================NEURON======================================
%id: {{LI,PI},neuron,UniqueId} :: -1 < LI < 1, -1 < PI < 1
%lt:{[hebbian,none],[tanh,cos,quadratic,guassian,mhat,step]}, exp: [hebbian,tanh], [Adaptor,ActivationFunction]
%type:[standard|bst|neuromodulated]
%DWP:[{Id1,WPC1},{Id2,WPC2},{Id3,WPC3}...{IdN,[{W1,DW1,LP2}...{Wk,DWk,LPk}]},{threshold,[{W,DW,LP}]}]
%DWP:[{Id1,WPC1},{Id2,WPC2},{Id3,WPC3}...{threshold,[{W,{Max,Min,Sample_Max,Sample_Min,Sample_Size},LP}]}]??????????????????????????????????????/
%I:[{Id1,IVL1},{Id2,IVL2}...]
%O:[Id1,Id2...]
%RO:[Id1,Id2...]
%modulation:[none,static,self,external]
%PI:[Id]
%I:[{Id1,WPC1,IV2},{Id2,WPC2,IV2}...{IdN,[{W1,DW1,LP2}...{Wk,DWk,LPk}],[IV1...IVk]},{threshold,[{W,DW,LP}],[1]}]

%======================================Neural_Setups======================================
%Architecture_Type: neural
%Neural_Types: [standard]
%PreProcessors: [none]
%SignalIntegrator: [dot]
%ActivationFunctions: [tanh]
%PostProcessors: [none]

%Architecture_Type: neural
%Neural_Types: [general]
%PreProcessors: [none]
%SignalIntegrator: [dot]
%ActivationFunctions: [tanh,cos,guassian]
%PostProcessors: [none]

%Architecture_Type: neural
%Neural_Types: [universal]
%PreProcessors: [none,normalizer]
%SignalIntegrator: [dot,product,unit,spheroid,cuboid,elipsoid,gaussianoid]
%ActivationFunctions: [tanh,cos,guassian]
%PostProcessors: [none,threshold]

%Architecture_Type: neural
%Neural_Types: [grossberg]
%PreProcessors: [none,normalizer]
%SignalIntegrator: [spheroid,cuboid,elipsoid,gaussianoid]
%ActivationFunctions: [none]
%PostProcessors: [threshold]

%Architecture_Type: neural
%Neural_Types: [layered]
%PreProcessors: [none,normalizer] if LI < 0.5 else [none]
%SignalIntegrator: [spheroid,cuboid,elipsoid,gaussianoid] if LI < 0.5 else [dot,product,unit]
%ActivationFunctions: [none] if LI < 0.5 else [tanh,cos,guassian]
%PostProcessors: [threshold] if LI < 0.5 else [none,threshold]

%Architecture_Type: hypercube
%Neural_Types: [standard]
%PreProcessors: [none]
%SignalIntegrator: [dot]
%ActivationFunctions: [tanh]
%PostProcessors: [none]

%This means that neural_types are independent of architecture_type...  link_form is though, feedforward if hypercube and aart, but neural can be anything.

%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(technome_constructor).
-compile(export_all).
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Technome_Constructor Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TEST_CONSTRAINT,#constraint{morphology=xorandxor,sc_types=[neural],sc_hypercube_plasticity=[none], sc_neural_linkform=feedforward, neural_afs = [cplx],neural_signal_integrators=[dot],neural_types = [standard]}).%pole2_balancing3}).
-define(INIT_CONSTRAINTS,[#constraint{morphology=Morphology,sc_types=SC_Types, sc_hypercube_plasticity=[none], sc_neural_linkform=LinkForm, neural_afs = [tanh],neural_signal_integrators=[complex_dot]}|| Morphology<-[epiwalker],LinkForm<-[recursive], SC_Types<-[[aart]]]).
%%%NEURON PARAMETERS
%-define(NEURO_TYPES,[standard]).%[standard,circuit],
%-define(NEURO_ADAPTERS,[none]). %[none,modulated]
%-define(ACTIVATION_FUNCTIONS,[tanh,gaussian,sin,absolute,sgn,linear,log,sqrt]). %[tanh,gaussian,sin,linear,absolute,sgn,log,sqrt]
%%%DX PARAMETERS
-define(INIT_POPULATION_ID,population_test).
-define(INIT_SPECIE_ID,specie_test).
-define(INIT_DX_ID,dx_test).
%-define(INIT_ARCHITECTURE_TYPE,modular).
%-define(INIT_CX_LINKFORM,recursive). %[recursive,feedforward,self_recursive]
%-define(MORPHOLOGY,pole2_balancing3). %pole2_balancing3, flatlander
-define(OPMODE,gt).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%============================================================Technome Construction Functions============================================================		
create_agent()->
	{A,B,C} = now(),
	random:seed(A,B,C),
	Specie_Id = ?INIT_SPECIE_ID,
	DX_Id = ?INIT_DX_ID,
	SpecCon = ?TEST_CONSTRAINT,
	F = fun()->
		case mnesia:read({dx,dx_test}) of
			[] ->
				done;
			_ ->
				delete_dx(dx_test)
				%io:format("Result:~p~n",[Result])
		end,
		modular_constructor:construct_DX(Specie_Id,DX_Id,SpecCon)
	end,
	mnesia:transaction(F).

test()->
	Init_PopulationId = ?INIT_POPULATION_ID,
	Init_SpecieId = ?INIT_SPECIE_ID,
	Init_DXId = ?INIT_DX_ID,
	OpMode = ?OPMODE,	
	SpecCon = ?TEST_CONSTRAINT,
	%io:format("SpecCon:~p~n",[SpecCon]),
	test({Init_PopulationId,Init_SpecieId,Init_DXId,OpMode,SpecCon}).
test({Population_Id,Specie_Id,DX_Id,OpMode,SpecCon})->
	{A,B,C} = now(),
	random:seed(A,B,C),
	F = fun()->
		case mnesia:read({dx,dx_test}) of
			[] ->
				done;
			_ ->
				delete_dx(dx_test)
				%io:format("Result:~p~n",[Result])
		end,
		modular_constructor:construct_DX(Specie_Id,DX_Id,SpecCon)
	end,
	case mnesia:transaction(F) of
		{atomic,_} ->
			exoself:test(OpMode,dx_test);
		ERROR ->
			io:format("********ERROR:technome_constructor:text(): ~p~n",[ERROR])
	end.

print_PopulationIds(Population_Id)->
	[P] = mnesia:dirty_read({population,Population_Id}),
	_ = [print_SpecieAgentIds(Specie_Id)|| Specie_Id<-P#population.specie_ids],
	ok.
	
	print_SpecieAgentIds(Specie_Id)->
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		io:format("****Specie_Id:~p****~n",[Specie_Id]),
		[print_GeneticLine(Agent_Id,1)||Agent_Id<-S#specie.seed_agent_ids].
		
		print_GeneticLine(Agent_Id,Generation)->
			[A] = mnesia:dirty_read({dx,Agent_Id}),
			io:format("Generation:~p Agent Id:~p~n",[Generation,Agent_Id]),
			[print_GeneticLine(Id,Generation+1) || Id <- A#dx.offspring_ids].

print_AllAgentIds()->
	Agent_Ids=mnesia:dirty_all_keys(dx),
	[io:format("Agent_Id:~p~n",[Agent_Id])||Agent_Id<-Agent_Ids],
	io:format("Total agents:~p~n",[length(Agent_Ids)]).

print_agent(DX_Id)->
	F = fun()->
		[DX] = mnesia:read({dx,DX_Id}),
		Cx_Id = DX#dx.cx_id,
		view_Cortex(Cx_Id),
		print_profile(DX#dx.profile),
		print_summary(DX#dx.summary)
	end,
	mnesia:transaction(F).
	
	view_Cortex(Cx_Id)->
		[Cx] = mnesia:read({cortex,Cx_Id}),
		io:format("Cortex:~n~p~n",[Cx]),
		CIds = Cx#cortex.cids,
		[view_Neuron(N_Id) || N_Id <- CIds].
		
		view_Neuron(N_Id)->
			[N] = mnesia:read({neuron,N_Id}),
			io:format("***Neuron:~n~p~n",[N]).
	
	print_profile(undefined)->
		io:format("Profile:~p~n",[undefined]);
	print_profile(Profile)->
		io:format("Profile:~p~n",[Profile]),
		[TotCores,TotSubCores,TotNeurons] = Profile,
		io:format("TotCores:~p TotSubCores:~p TotNeurons:~p~n",[TotCores,TotSubCores,TotNeurons]).
		
	print_summary(S)->
		%-record(summary,{type,tot_neurons,tot_n_ils,tot_n_ols,tot_n_ros,af_distribution,fitness}).
		io:format("Type:~p~n Total Neurons:~p~n Total Neural Inputs:~p~n Tot Neural Outputs:~p~n Tot Neural Recursives:~p~n {TotTanh,TotSin,TotCos,TotGaussian,TotAbsolute,TotSgn,TotLog,TotSqrt,TotLin}:~p~n Fitness:~p~n",
		[S#summary.type,S#summary.tot_neurons,S#summary.tot_n_ils,S#summary.tot_n_ols,S#summary.tot_n_ros,S#summary.af_distribution,S#summary.fitness]).

delete_dx(DX_Id)->
	[DX] = mnesia:read({dx,DX_Id}),
	[mnesia:delete({neuron,N_Id}) || N_Id <- DX#dx.n_ids],
	mnesia:delete({cortex,DX#dx.cx_id}),
	mnesia:delete({dx,DX_Id}).	
	
clone_dx(DX_Id,CloneDX_Id)->
	F = fun()->
		[DX] = mnesia:read({dx,DX_Id}),
		IdsNCloneIds = ets:new(idsNcloneids,[set,private]),
		ets:insert(IdsNCloneIds,{threshold,threshold}),
		CloneN_Ids = map_ids(IdsNCloneIds,DX#dx.n_ids,[]),
		[CloneCx_Id] = map_ids(IdsNCloneIds,[DX#dx.cx_id],[]),
		ets:insert(IdsNCloneIds,{DX_Id,CloneDX_Id}),
		clone_neurons(IdsNCloneIds,DX#dx.n_ids),
		clone_cx(IdsNCloneIds,DX#dx.cx_id),
		mnesia:write(DX#dx{
			id = CloneDX_Id,
			cx_id = CloneCx_Id,
			n_ids = CloneN_Ids,
			parent_ids=[DX_Id],
			offspring_ids=[]}),
		ets:delete(IdsNCloneIds)
	end,
	mnesia:transaction(F).
	
	map_ids(TableName,[Id|Ids],Acc)->
		%io:format("Id:~p~n",[Id]),
		{{LayerIndex,_NumId},Type} = Id,
		CloneId = {{LayerIndex,technome_constructor:generate_UniqueId()},Type},
		ets:insert(TableName,{Id,CloneId}),
		%ets:insert(TableName,{CloneId,Id}),
		map_ids(TableName,Ids,[CloneId|Acc]);
	map_ids(_TableName,[],Acc)->
		Acc.
	
	clone_cx(TableName,Cx_Id)->
		[Cx] = mnesia:read({cortex,Cx_Id}),
		CloneCx_Id = ets:lookup_element(TableName,Cx_Id,2),
 		CloneCF = clone_Cxlinks(TableName,Cx#cortex.cf,[]),
		CloneCT = clone_Cxlinks(TableName,Cx#cortex.ct,[]),
		CloneCIds = [ets:lookup_element(TableName,Id,2) || Id <- Cx#cortex.cids],
		CloneSU_Id = ets:lookup_element(TableName,Cx#cortex.su_id,2),
		mnesia:write(Cx#cortex{
			id = CloneCx_Id,
			cf = CloneCF,
			ct = CloneCT,
			cids = CloneCIds,
			su_id = CloneSU_Id}).	

		clone_Cxlinks(TableName,[{R,Ids}|LinkList],Acc) when is_record(R,actuator)->%Neural: CF 
			clone_Cxlinks(TableName,LinkList,[{R,[ets:lookup_element(TableName,Id,2)|| Id<-Ids]}|Acc]);
		clone_Cxlinks(TableName,[{R,IdPs}|LinkList],Acc) when is_record(R,sensor)->%Neural: CT
			clone_Cxlinks(TableName,LinkList,[{R,[{ets:lookup_element(TableName,Id,2),Tag}|| {Id,Tag}<-IdPs]}|Acc]);
		clone_Cxlinks(TableName,[{R,Ids}|LinkList],Acc) when is_record(R,sCF)->%Hypercube: CF 
			clone_Cxlinks(TableName,LinkList,[{R,[ets:lookup_element(TableName,Id,2)|| Id<-Ids]}|Acc]);
		clone_Cxlinks(TableName,[{R,IdPs}|LinkList],Acc) when is_record(R,sCT)->%Hypercube: CT
			clone_Cxlinks(TableName,LinkList,[{R,[{ets:lookup_element(TableName,Id,2),Tag}|| {Id,Tag}<-IdPs]}|Acc]);
		clone_Cxlinks(_TableName,[],Acc)->
			lists:reverse(Acc).
		
	clone_neurons(TableName,[N_Id|N_Ids])->
		[N] = mnesia:read({neuron,N_Id}),
		CloneN_Id = ets:lookup_element(TableName,N_Id,2),
		CloneI =  [{ets:lookup_element(TableName,I_Id,2),IVL}|| {I_Id,IVL} <- N#neuron.i],
		CloneO = [ets:lookup_element(TableName,O_Id,2)|| O_Id <- N#neuron.o],
		CloneRO =[ets:lookup_element(TableName,RO_Id,2)|| RO_Id <- N#neuron.ro],
		CloneDWP = case N#neuron.type of
			standard ->
				[{ets:lookup_element(TableName,Id,2),WPC} || {Id,WPC} <- N#neuron.dwp];
			circuit ->
				N#neuron.dwp
		end,
		CloneSU_Id = ets:lookup_element(TableName,N#neuron.su_id,2),
		mnesia:write(N#neuron{
			id = CloneN_Id,
			i = CloneI,
			o = CloneO,
			ro = CloneRO,
			dwp = CloneDWP,
			su_id = CloneSU_Id}),
		clone_neurons(TableName,N_Ids);
	clone_neurons(_TableName,[])->
		done.	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SHARED NEURO CONSTRUCTION MODULES%%%%%%%%%%%%%%%%%
generate_UniqueId()->
	{MegaSeconds,Seconds,MicroSeconds} = now(),
	1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).

construct_Neuron(SU_Id,Generation,N_Id,{N_TotIVL,N_I},{N_TotOVL,N_O},SpeCon,Neural_Type,Heredity_Type)->
	AF=generate_NeuralAF(SpeCon#constraint.neural_afs,Neural_Type,N_Id),
	DWP=case Neural_Type of
		circuit ->
			create_circuit(N_TotIVL,[2,1],AF);
			%create_circuit(N_TotIVL,[1+random:uniform(round(math:sqrt(N_TotIVL))),1]);
		standard ->
			create_NWP(N_I,[])
	end,
	Neuron = #neuron{
		id = N_Id,
		type = Neural_Type,
		heredity_type = Heredity_Type,
		ivl = N_TotIVL,
		i = N_I,
		ovl = N_TotOVL,
		o = N_O,
		%lt = LearningType,
		preprocessor = generate_NeuralPreprocessor(SpeCon#constraint.neural_preprocessors,Neural_Type,N_Id),
		signal_integrator = generate_NeuralSignalIntegrator(SpeCon#constraint.neural_signal_integrators,Neural_Type,N_Id),
		activation_function=AF,
		postprocessor = generate_NeuralPostprocessor(SpeCon#constraint.neural_postprocessors,Neural_Type,N_Id),
		plasticity=generate_NeuralPF(SpeCon#constraint.neural_pfs,Neural_Type,N_Id),
		ro = calculate_RO(SU_Id,N_Id,N_O,[]),
		dwp = DWP,
		su_id = SU_Id,
		generation = Generation
	},
%	io:format("~p~n",[Neuron]),
	mnesia:write(Neuron).
	
	create_circuit(IVL,Densities,AF)->create_circuit(IVL,Densities,AF,[]).
	create_circuit(IVL,[VL|Densities],AF,Acc)->
		{Weights,Parameters} = case AF of
			rbf ->
				{[random:uniform()-0.5|| _<-lists:seq(1,IVL)],[random:uniform()]};
			_ ->
				{[random:uniform()-0.5|| _<-lists:seq(1,IVL)],[]}
		end,
		Layer=[#neurode{id=technome_constructor:generate_UniqueId(),af=AF,weights=Weights} || _<-lists:seq(1,VL)],
		create_circuit(length(Layer),Densities,AF,[Layer|Acc]);
	create_circuit(_IVL,[],_AF,Acc)->
		lists:reverse(Acc).
	
	generate_NeuralPF(Available_PFs,NeuralType,N_Id) -> generate_NeuralPF(Available_PFs,NeuralType,N_Id,[]).
	generate_NeuralPF(Available_PlasticityFunctions,NeuralType,N_Id,Not_PFs)->
		Available_PFs=case NeuralType of
			standard ->
				Available_PlasticityFunctions;
			layered ->
				{{LI,UId},neuron} = N_Id,
				case LI < 0 of
					true ->
						[perceptron];
					false ->
						[none]
				end;
			single_grossberg_layer ->
				{{LI,UId},neuron} = N_Id,
				case LI == 0 of
					true ->
						[art];
					false ->
						[none]
				end;
			single_perceptron_layer ->
				{{LI,UId},neuron} = N_Id,
				case LI == 0 of
					true ->
						[perceptron];
					false ->
						[none]
				end;
			higher_order ->
				Available_PlasticityFunctions;
			_ ->
				[none]
			
		end,
		case Available_PFs -- Not_PFs of
			[] ->
				none;
			Other ->
				lists:nth(random:uniform(length(Other)),Other)
		end.
	
	generate_NeuralAF(Available_AFs,NeuralType,N_Id)-> generate_NeuralAF(Available_AFs,NeuralType,N_Id,[]).
	generate_NeuralAF(Available_ActivationFunctions,NeuralType,N_Id,Not_AFs)->
		Available_AFs=case NeuralType of
			standard ->
				Available_ActivationFunctions;
			layered ->
				{{LI,UId},neuron} = N_Id,
				case LI < 0 of
					true ->
						[sgn];
					false ->
						[tanh]
				end;
			single_grossberg_layer ->
				{{LI,UId},neuron} = N_Id,
				case LI == 0 of
					true ->
						[sgn];
					false ->
						[tanh]
				end;
			single_perceptron_layer ->
				{{LI,UId},neuron} = N_Id,
				case LI == 0 of
					true ->
						[sgn];
					false ->
						[tanh]
				end;
			higher_order ->
				[tanh];
			circuit ->
				Available_ActivationFunctions;
			_ ->
				[tanh]
			
		end,
		case Available_AFs -- Not_AFs of
			[] ->
				tanh;
			Other ->
				lists:nth(random:uniform(length(Other)),Other)
		end.
		
	generate_NeuralPreprocessor(Available_Preprocessors,NeuralType,N_Id)-> generate_NeuralPreprocessor(Available_Preprocessors,NeuralType,N_Id,[]).
	generate_NeuralPreprocessor(Available_Preprocessors,NeuralType,N_Id,Not_Preprocessors)->
		case Available_Preprocessors -- Not_Preprocessors of
			[] ->
				none;
			Other ->
				lists:nth(random:uniform(length(Other)),Other)
		end.
			
	generate_NeuralSignalIntegrator(Available_SignalIntegrators,NeuralType,N_Id)->generate_NeuralSignalIntegrator(Available_SignalIntegrators,NeuralType,N_Id,[]).
	generate_NeuralSignalIntegrator(Available_SignalIntegrators,NeuralType,N_Id,Not_SignalIntegrators)->
		SigInts=case NeuralType of
			standard ->
				Available_SignalIntegrators;
			layered ->
				{{LI,UId},neuron} = N_Id,
				case LI < 0 of
					true ->
						[vector_distance];
					false ->
						[dot]
				end;
			single_grossberg_layer ->
				{{LI,UId},neuron} = N_Id,
				case LI == 0 of
					true ->
						[within_hypersphere,within_hypercube];
					false ->
						[dot]
				end;
			single_perceptron_layer ->
				{{LI,UId},neuron} = N_Id,
				case LI == 0 of
					true ->
						[dot];
					false ->
						[dot]
				end;
			higher_order ->
				[pun,higher_order];
			_ ->
				[dot]
			
		end,
		case SigInts -- Not_SignalIntegrators of
			[] ->
				dot;
			Other ->
				lists:nth(random:uniform(length(Other)),Other)
		end.
						
	generate_NeuralPostprocessor(Available_Postprocessors,Type,N_Id)-> generate_NeuralPostprocessor(Available_Postprocessors,Type,N_Id,[]).
	generate_NeuralPostprocessor(Available_Postprocessors,Type,N_Id,Not_Postprocessors)->
		case Available_Postprocessors -- Not_Postprocessors of
			[] ->
				none;
			Other ->
				lists:nth(random:uniform(length(Other)),Other)
		end.			
			
	%{threshold,[{random:uniform()/2 - 0.5,{1,-1,-1000,1000,100},random:uniform()/2 - 0.5}
	create_NWP([{Id,IVL}|I],NWPAcc)->
		NWP = {Id,create_Weights(IVL,[])},
		create_NWP(I,[NWP|NWPAcc]);
	create_NWP([],NWPAcc)->
		lists:reverse(NWPAcc).%++[{threshold,[weight_tuple()]}].
		
		create_Weights(0,WeightsAcc)->
			WeightsAcc;
		create_Weights(IVL,WeightsAcc)->
			create_Weights(IVL-1,[weight_tuple()|WeightsAcc]).
			
			weight_tuple()->
				{random:uniform(),random:uniform(),random:uniform()}.

			null_wt()->
				{1,1,0}.

%			weight_tuple()->%%%For self adjusting scaling system
%				{random:uniform()/2 - 0.5,{1,-1,-1000,1000,100},random:uniform()/2 - 0.5}.
				
%			null_wt()->%%%For self adjusting scaling system
%				{1,{1,-1,-1000,1000,100},random:uniform()/2 - 0.5}.

	calculate_RO(SU_Id,Self_Id,[Id|Ids],Acc)->
		case Id of
			SU_Id ->
				calculate_RO(SU_Id,Self_Id,Ids,Acc);
			Id ->
				{{TLI,_},_NodeType} = Self_Id,
				{{LI,_},_} = Id,
				case LI =< TLI of
					true ->
						calculate_RO(SU_Id,Self_Id,Ids,[Id|Acc]);
					false ->
						calculate_RO(SU_Id,Self_Id,Ids,Acc)
				end
		end;
	calculate_RO(_SU_Id,_Self_Id,[],Acc)->
		lists:reverse(Acc).
%-record(profile,{tot_subcores,tot_substrates,tot_subcore_ils,tot_subcore_ols,tot_neurons,tot_neuron_ils,tot_neuron_ols,function_distribution,fitness}).
update_Stats(DX_Id)->
	[DX] = mnesia:read({dx,DX_Id}),
	Cx_Id = DX#dx.cx_id,
	[Cortex] = mnesia:read({cortex,Cx_Id}),
	N_Ids = Cortex#cortex.cids,
	{Tot_Neuron_ILs,Tot_Neuron_OLs,Tot_Neuron_ROs,Function_Distribution} = find_NN_Summary(N_Ids),
	Type = Cortex#cortex.type,
	Summary = #summary{
		type = Type,
		tot_neurons = length(N_Ids),
		tot_n_ils = Tot_Neuron_ILs,
		tot_n_ols = Tot_Neuron_OLs,
		tot_n_ros = Tot_Neuron_ROs,
		af_distribution = Function_Distribution},
%	Summary = [
%		{tot_substrates,find_TotSubstrates(SC_Ids,0)},
%		{tot_subcores,length(SC_Ids)},
%		{tot_neurons,length(N_Ids)}
%	],
	Updated_DX = DX#dx{
		n_ids = N_Ids,
		summary = Summary
	},
	mnesia:write(Updated_DX).
		
	find_NN_Summary(N_Ids)->
		find_NN_Summary(N_Ids,0,0,0,{0,0,0,0,0,0,0,0,0,0}).
	find_NN_Summary([N_Id|N_Ids],ILAcc,OLAcc,ROAcc,FunctionDistribution)->
		[N] = mnesia:read({neuron,N_Id}),
		IL_Count = length(N#neuron.i),
		OL_Count = length(N#neuron.o),
		RO_Count = length(N#neuron.ro),
		%{_Adaptor,AF} = N#neuron.lt,
		AF = N#neuron.activation_function,
		%%[tanh,gaussian,sin,linear,absolute,sgn,log,sqrt]
		{TotTanh,TotSin,TotCos,TotGaussian,TotAbsolute,TotSgn,TotLog,TotSqrt,TotLin,TotOther} = FunctionDistribution,
		U_FunctionDistribution= case AF of
			tanh ->{TotTanh+1,TotSin,TotCos,TotGaussian,TotAbsolute,TotSgn,TotLog,TotSqrt,TotLin,TotOther};
			sin ->{TotTanh,TotSin+1,TotCos,TotGaussian,TotAbsolute,TotSgn,TotLog,TotSqrt,TotLin,TotOther};
			cos ->{TotTanh,TotSin,TotCos+1,TotGaussian,TotAbsolute,TotSgn,TotLog,TotSqrt,TotLin,TotOther};
			gaussian->{TotTanh,TotSin,TotCos,TotGaussian+1,TotAbsolute,TotSgn,TotLog,TotSqrt,TotLin,TotOther};
			absolute->{TotTanh,TotSin,TotCos,TotGaussian,TotAbsolute+1,TotSgn,TotLog,TotSqrt,TotLin,TotOther};
			sgn ->{TotTanh,TotSin,TotCos,TotGaussian,TotAbsolute,TotSgn+1,TotLog,TotSqrt,TotLin,TotOther};
			log ->{TotTanh,TotSin,TotCos,TotGaussian,TotAbsolute,TotSgn,TotLog+1,TotSqrt,TotLin,TotOther};
			sqrt ->{TotTanh,TotSin,TotCos,TotGaussian,TotAbsolute,TotSgn,TotLog,TotSqrt+1,TotLin,TotOther};
			linear ->{TotTanh,TotSin,TotCos,TotGaussian,TotAbsolute,TotSgn,TotLog,TotSqrt,TotLin+1,TotOther};
			Other ->{TotTanh,TotSin,TotCos,TotGaussian,TotAbsolute,TotSgn,TotLog,TotSqrt,TotLin+1,TotOther+1}
		end,
		find_NN_Summary(N_Ids,IL_Count+ILAcc,OL_Count+OLAcc,RO_Count+ROAcc,U_FunctionDistribution);
	find_NN_Summary([],ILAcc,OLAcc,ROAcc,FunctionDistribution)->
		{ILAcc,OLAcc,ROAcc,FunctionDistribution}.

reset_fitness(DX_Id)->
	[DX] = mnesia:read({dx,DX_Id}),
	mnesia:write(DX#dx{fitness=undefined,main_fitness=undefined}).
	
construct_FitnessProfile(DX,Fitness)->
	%[DX] = mnesia:dirty_read({dx,DX_Id}),
	Summary = DX#dx.summary,
	Tot_Neurons = Summary#summary.tot_neurons,
	%TN_Penalty = abs(Fitness*0.01*Tot_Neurons),
	Tot_Cores = 1,
	Tot_SubCores = 0,
	Profile = [Tot_Cores,Tot_SubCores,Tot_Neurons],
	%Tot_Penalty = TN_Penalty,
	TrueFitness = Fitness,% - Tot_Penalty,
	io:format("Fitness:~p~n",[Fitness]),
	{Fitness,Profile}.
	
construct_profile(DX_Id)->
	[DX] = mnesia:read({dx,DX_Id}),
	%io:format("DX:~p~n",[DX]),
	Summary = DX#dx.summary,
	Tot_Neurons = Summary#summary.tot_neurons,
	[0,0,Tot_Neurons].
read(TnK)->
	case mnesia:read(TnK) of
		[] ->
			undefined;
		[R] ->
			R
	end.
	
dirty_read(TnK)->
	case mnesia:dirty_read(TnK) of
		[] ->
			undefined;
		[R] ->
			R
	end.

write(R)->
	F = fun()->
		mnesia:write(R)
	end,
	mnesia:transaction(F). 

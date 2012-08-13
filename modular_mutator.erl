%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(modular_mutator).
-compile(export_all).
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Mutagens %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(MUTATION_OPERATORS,[{add_Neuron,0.2},{neurolink_OutputSplice,0.2},{add_ONLink,0.2},{add_INLink,0.2},{add_Threshold,0.2}]). %change_ActivationFunction
mos()->?MUTATION_OPERATORS.
-define(SUBCORE_MUTAGENS,[
%	add_SubCore,
%	remove_SubCore,
%	subCoreLink_Splice,
%	subCoreLink_DeSplice,
%	add_SubCoreLink,
%	change_SubCorePlasticity,
%	add_SubCoreModulator,
%	remove_SubCoreModulator,

	add_Neuron, %Can also add a link to a new sensor or actuator
%	remove_Neuron,
	neurolink_OutputSplice,
%	neurolink_DeSplice,
	%neurolink_InputSplice,
%	neurolink_DeInSplice,
	add_ONLink, %Can also add a link to a new actuator
%	remove_ONLink,
	add_INLink, %Can also add a link to a new sensor
%	remove_INLink,
%	change_PlasticityFunction,
%	change_ActivationFunction,
%	reset_DWP,
%	reset_Neuron,
%	add_NeuroModulation,
%	remove_NeuroModulation,

%	increase_SubstrateResolution,
%	decrease_SubstrateResolution,
%	increase_SubstrateDepth,
%	decrease_SubstrateDepth,
	add_SensorLink,
	add_ActuatorLink,
%	add_Sensor,
%	add_Actuator,
%	remove_Threshold,
	add_Threshold
	]).%change_Adapter,change_ActivationFunction,reset_DWP,reset_Neuron]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modular_Mutator Parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SCCTTypes,[single,block,block]).%[single,block,all],
-define(HYPERCUBE_CFTAGS,morphology:get_HCF(Cx#cortex.dimensions,Cx#cortex.plasticity)).
-define(HYPERCUBE_CTTAGS,morphology:get_HCT(Cx#cortex.dimensions,Cx#cortex.plasticity)).%TODO: Add standard coordinates
-define(ACTUATOR_TAGS,morphology:get_Actuators(DX#dx.morphology)).
-define(SENSOR_TAGS,morphology:get_Sensors(DX#dx.morphology)).
-define(EVO_STRAT,static). %static|evolving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test()->
	F = fun()->
		mutate(dx_test)
	end,
	mnesia:transaction(F).
	
mutate(DX_Id)->
	{A,B,C} = now(),
	random:seed(A,B,C),
	[DX] = mnesia:read({dx,DX_Id}),
	OldGeneration = DX#dx.generation,
	NewGeneration = OldGeneration+1,
	mnesia:write(DX#dx{generation = NewGeneration,evo_strat=agent_evo_strat:mutate(DX#dx.evo_strat)}),
	[Cx] = mnesia:read({cortex,DX#dx.cx_id}),
	mnesia:write(Cx#cortex{generation = NewGeneration}),
	apply_Mutagens(DX_Id,NewGeneration),
	verify_network(DX_Id),%%%TODO:Should be Cx or Cx_Id rather than DX_Id
	technome_constructor:update_Stats(DX_Id),
	technome_constructor:reset_fitness(DX_Id),
	find_AbroptEdges(DX_Id), %What is a NeuroEdge?
	ok.
	
test(DX_Id,MutationOperator)->
	F = fun()->
		[DX] = mnesia:dirty_read({dx,DX_Id}),
		Summary = DX#dx.summary,
		Cx_Id = DX#dx.cx_id,
		modular_mutator:MutationOperator(DX_Id,Cx_Id)
	end,
	mnesia:transaction(F).

%-record(agent_evo_strat,{
%	strategies_mutation_prob = 0,%Probability of mutating an evolutionary strategies parameter, increments and decrements based on how far from the edge: 0-100%.
%	tuning_mutation_range = math:pi(),%From -2Pi to Pi, starts of with a range of -Pi to Pi.
%	tuning_annealing_flag = false,%true|false
%	annealing_parameter = 0.5,%Standard: math:pi()*math:pow(0.5,Age) tuning_mutation_range*math:pow(anealing_parameter,Age), ranges from 0 to 1, where 1 is no anealing, and 0 is stop evolving.
%	topological_mutation_prob = 1,%Standard: Range from 1 to 1/sqrt(Tot_Neurons), Multiplier range: 1 to sqrt(Tot_Neurons).
%	topological_annealing_flag = false,%true|false
%	neuron_perturbation_prob = 1,%Standard: 1/sqrt(TotNeurons) Probability of choosing a neuron for perturbation, multiplier: 1 to sqrt(TotNeurons)
%	weight_perturbation_prob = 1,%Standard: 1/sqrt(TotWeights) Probability of choosing a weight for perturbation, multiplier: 1 to sqrt(TotWeights)
%	active_neuron_selection_type = dynamic_random,%What type to use to select active neurons: %[dynamic|active|recent|current|all|dynamic_random|active_random|recent_random|current_random|all_random]
%	active_neuron_selection_parameter = undefined %Augment the parameter of selection, dependent on type
%}).
	apply_Mutagens(DX_Id,NewGeneration)->
		[DX] = mnesia:read({dx,DX_Id}),
		Summary = DX#dx.summary,
		Cx_Id = DX#dx.cx_id,
		Tot_Neurons = Summary#summary.tot_neurons,
		case ?EVO_STRAT of
			static ->
				TotMutations = random:uniform(round(math:pow(Tot_Neurons,1/2))),
				Mutation_Operators = ?SUBCORE_MUTAGENS;
			evolving ->
				EvoStrat = DX#dx.evo_strat,
				%io:format("EvoStrat:~p~n",[EvoStrat]),
				TotMutations = round(Tot_Neurons*EvoStrat#agent_evo_strat.topological_mutation_prob),
				Mutation_Operators = EvoStrat#agent_evo_strat.mutation_operators,
				io:format("TotMutations:~p~n",[TotMutations]);
			dynamic ->
				TotMutations = void,
				Mutation_Operators = void
		end,
		apply_Mutagens(DX_Id,NewGeneration,Cx_Id,TotMutations,Mutation_Operators).
		
		apply_Mutagens(_DX_Id,_NewGeneration,_Cx_Id,0,_Mutation_Operators)->
			done;
		apply_Mutagens(DX_Id,NewGeneration,Cx_Id,MutationIndex,Mutation_Operators)->
			Result = apply_SCLevelMutagen(DX_Id,NewGeneration,Cx_Id,Mutation_Operators),
			case Result of
				{atomic,_} ->
					apply_Mutagens(DX_Id,NewGeneration,Cx_Id,MutationIndex-1,Mutation_Operators);
				Error ->
					io:format("******** Error:~p~nRetrying with new Mutagen...~n",[Error]),
					apply_Mutagens(DX_Id,NewGeneration,Cx_Id,MutationIndex,Mutation_Operators)
			end.	
	
			apply_SCLevelMutagen(DX_Id,NewGeneration,Cx_Id,Mutagens)->
				F = fun()->
					Mutagen = get_RandomMutagen(Mutagens),
					io:format("Mutagen:~p~n",[Mutagen]),
					modular_mutator:Mutagen(DX_Id,Cx_Id)
				end,
				mnesia:transaction(F).	
			
			get_RandomMutagen(Mutagens)->
				case Mutagens of
					[{Mutagen,_PercentageSector}|_] ->
						get_RandomMutagen(0,Mutagens,random:uniform());
					_ ->
						Tot_Mutagens = length(Mutagens),
						lists:nth(random:uniform(Tot_Mutagens),Mutagens)
				end.
				
				get_RandomMutagen(Range_From,[{Mutagen,Prob}|Mutagens],Choice)->
					Range_To = Range_From+Prob,
					case (Choice >= Range_From) and (Choice =< Range_To) of
						true ->
							Mutagen;
						false ->
							get_RandomMutagen(Range_To,Mutagens,Choice)
					end;
				get_RandomMutagen(_Rage_From,[],_Choice)->
					exit("********ERROR:get_RandomMutagen:: in get_RandomMutagen(Mutagens,Choice), Mutagens reached []").
			
	find_AbroptEdges(DX_Id)->
		[DX] = mnesia:read({dx,DX_Id}),
		N_Ids = DX#dx.n_ids,
		find_NeuroEdges(N_Ids,[],[]).
		
		find_NeuroEdges([N_Id|N_Ids],ZeroIAcc,ZeroOAcc)->
			[N] = mnesia:read({neuron,N_Id}),
			I = N#neuron.i,
			O = N#neuron.o,
			case {length(I),length(O)} of
				{0,0} -> find_NeuroEdges(N_Ids,[N_Id|ZeroIAcc],[N_Id|ZeroOAcc]);
				{0,_} -> find_NeuroEdges(N_Ids,[N_Id|ZeroIAcc],ZeroOAcc);
				{_,0} -> find_NeuroEdges(N_Ids,ZeroIAcc,[N_Id|ZeroOAcc]);
				{_,_} -> find_NeuroEdges(N_Ids,ZeroIAcc,ZeroOAcc)
			end;
		find_NeuroEdges([],ZeroIAcc,ZeroOAcc)->
			case (length(ZeroIAcc) =/= 0) or (length(ZeroOAcc) =/= 0) of
				true ->
					exit("********ERROR:find_NeuroEdges:: ZeroEdge Error");
				false ->
					done
			end.
		
%%%================================================================SubCore Mutagens================================================================%%%
%%evo_hist: Evolutionary History in a list: [{Generation,TypeOfMutagen,Applied_On,From,To}...]
update_EvoHist(DX_Id,Mutagen,New_Id,Applied_On,From,To,Parameter)->
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	EvoHist = DX#dx.evo_hist,
	U_EvoHist = [{Generation,Mutagen,New_Id,Applied_On,From,To,Parameter}|EvoHist],
	%io:format("EvoHist:~p U_EvoHist:~p~n",[EvoHist,U_EvoHist]),
	mnesia:write(DX#dx{
		evo_hist = U_EvoHist
	}).

%--------------------------------insert_NewLayer--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output:
%%%MsgComunication:
insert_NewLayer(SU_Id,NewLI)->
%	io:format("SU_Id:~p NewLI:~p~n",[SU_Id,NewLI]),
	case SU_Id of
		{_,cortex} ->
			[Cx] = mnesia:read({cortex,SU_Id}),
			Pattern = Cx#cortex.pattern,
			U_Pattern = add_LayerToPattern(NewLI,Pattern),
			mnesia:write(Cx#cortex{pattern = U_Pattern})
	end.

	add_LayerToPattern(NewLI,Pattern)->
%		io:format("add_LayerToPattern, NewLI:~p Pattern:~p~n",[NewLI,Pattern]),
		[{FirstLI,FirstLL}|PTail] = Pattern,
		[{LastLI,_}|_] = lists:reverse(Pattern),
		case (NewLI > LastLI) or (NewLI < FirstLI) of
			true ->
				case NewLI > LastLI of
					true ->
						Pattern ++ [{NewLI,0}];
					false ->
						[{NewLI,0}] ++ Pattern
				end;
			false ->
				insert_NewLayer(NewLI,PTail,[{FirstLI,FirstLL}])
		end.
	
		insert_NewLayer(NewLI,[{LI2,LL2}|Pattern],UpdatedPatternAcc)->
			[{LI1,LL1}|_] = UpdatedPatternAcc,
%			io:format("NewLI:~p LI2:~p LI1:~p~n",[NewLI,LI2,LI1]),
			case (NewLI > LI1) and (NewLI < LI2) of
				true ->
					insert_NewLayer(NewLI,Pattern,[{LI2,LL2},{NewLI,0}|UpdatedPatternAcc]);
				false ->
					insert_NewLayer(NewLI,Pattern,[{LI2,LL2}|UpdatedPatternAcc])
			end;
		insert_NewLayer(_NewLI,[],UpdatedPatternAcc)->
			lists:reverse(UpdatedPatternAcc).		

add_UnitToPattern([{LI,LL}|Pattern],TL,U_Pattern)->
	case LI == TL of
		true ->
			add_UnitToPattern(Pattern,TL,[{LI,LL+1}|U_Pattern]);
		false ->
			add_UnitToPattern(Pattern,TL,[{LI,LL}|U_Pattern])
	end;
add_UnitToPattern([],_TL,U_Pattern)->
	lists:reverse(U_Pattern).

get_NewLI(LI,LI,Direction,Pattern)->
	exit("******** ERROR: get_NewLI FromLI == ToLI");
get_NewLI(FromLI,ToLI,Direction,Pattern)->
%	io:format("FromLI:~p ToLI:~p Direction:~p Pattern:~p~n",[FromLI,ToLI,Direction,Pattern]),
	NewLI = case Direction of
		next ->
			get_NextLI(Pattern,FromLI,ToLI);
		prev ->
			get_PrevLI(lists:reverse(Pattern),FromLI,ToLI)
	end,
%	io:format("FromLI:~p ToLI:~p Direction:~p Pattern:~p NewLI:~p~n",[FromLI,ToLI,Direction,Pattern,NewLI]),
	NewLI.
	
	get_NextLI([{LI,_LastLayerDensity}],LI,su)->
		(LI+1)/2;
	get_NextLI([{LI,_LayerDensity}|Pattern],FromLI,ToLI)->
		case LI == FromLI of
			true ->
				[{NextLI,_NextLayerDensity}|_] = Pattern,
				case NextLI == ToLI of
					true ->
						(FromLI + ToLI)/2;
					false ->
						NextLI
				end;

			false ->
				get_NextLI(Pattern,FromLI,ToLI)
		end.

	get_PrevLI([{LI,_FirstLayerDensity}],LI,su)->
		(-1+LI)/2;
	get_PrevLI([{LI,_LayerDensity}|Pattern],FromLI,ToLI)->
		case LI == FromLI of
			true ->
				[{PrevLI,_PrevLayerDensity}|_] = Pattern,
				case PrevLI == ToLI of
					true ->
						(FromLI + ToLI)/2;
					false ->
						PrevLI
				end;
			false ->
				get_PrevLI(Pattern,FromLI,ToLI)
		end.

%--------------------------------get_TargetLayerIds--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output:
%%%MsgComunication:	
	get_TargetLayerIds(SU_Id,0,_Ids,_TargetLayerIdsAcc)->
		[SU_Id];
	get_TargetLayerIds(SU_Id,TLI,[{{LI,UId},TL}|Ids],TargetLayerIdsAcc)->
		case TLI =:= LI of
			true ->
				get_TargetLayerIds(SU_Id,TLI,Ids,[{{LI,UId},TL}|TargetLayerIdsAcc]);
			false ->
				get_TargetLayerIds(SU_Id,TLI,Ids,TargetLayerIdsAcc)
		end;
	get_TargetLayerIds(_SU_Id,_TLI,[],TargetLayerIdsAcc)->
		lists:reverse(TargetLayerIdsAcc).
			
%--------------------------------remove_LayerFromPattern--------------------------------				
remove_LayerFromPattern(LI,TLI,[LL|Pattern],UpdatedPatternAcc)->
	case LI == TLI of
		true -> 
			case LL == 0 of
				true ->
					remove_LayerFromPattern(LI+1,TLI,Pattern,UpdatedPatternAcc);
				false ->
					exit("********ERROR:remove_LayerFromPattern:: in remove_LayerFromPattern(), LL =/= 0 ~n")
			end;
		false ->
			remove_LayerFromPattern(LI+1,TLI,Pattern,[LL|UpdatedPatternAcc])
	end;
remove_LayerFromPattern(_LI,_TLI,[],UpdatedPatternAcc)->
	lists:reverse(UpdatedPatternAcc).
%update_EvoHist(DX_Id,Mutagen,New_Id,Applied_On,From,To,Parameter)
%--------------------------------increase_SubstrateResolution--------------------------------
increase_SubstrateResolution(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	case Cx#cortex.type of
		neural ->
			exit("******** increase_SubstrateResolution not applicable to Cx#cortex.type == neural~n");
		hypercube ->
			Old_Densities = Cx#cortex.densities,
			[Old_Depth|Old_SubDensities] = Old_Densities,
			%New_SubDensities = [Density+random:uniform(round(math:sqrt(Density))) || Density <- Old_SubDensities],
			New_SubDensities = [Density+(random:uniform(5)-1) || Density <- Old_SubDensities],
			New_Densities = [Old_Depth|New_SubDensities],
			mnesia:write(Cx#cortex{
				densities = New_Densities
			}),
			update_EvoHist(DX_Id,increase_SubstrateResolution,void,Cx_Id,void,void,void)
	end.
	
%--------------------------------decrease_SubstrateResolution--------------------------------	
decrease_SubstrateResolution(_DX_Id,_Cx_Id)->
	done.%TODO

%--------------------------------increase_SubstrateDepth--------------------------------
increase_SubstrateDepth(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	case Cx#cortex.type of
		neural ->
			exit("******** increase_SubstrateDepth not applicable to Cx#cortex.type == neural~n");
		hypercube ->
			Old_Densities = Cx#cortex.densities,
			[Old_Depth|Old_SubDensities] = Old_Densities,
			New_Depth = Old_Depth+1,
			New_Densities = [New_Depth|Old_SubDensities],
			mnesia:write(Cx#cortex{
				densities = New_Densities
			}),
			update_EvoHist(DX_Id,increase_SubstrateDepth,void,Cx_Id,void,void,void)
	end.
	
%--------------------------------decrease_SubstrateDepth--------------------------------	
decrease_SubstrateDepth(_DX_Id,_Cx_Id)->
	done.%TODO

%--------------------------------add_Sensor--------------------------------
%update_EvoHist(DX_Id,Mutagen,New_Id,Applied_On,From,To,Parameter)
add_SensorLink(DX_Id,Cx_Id)->%TODO We need to eventually make sure that sensor holds the most recent list of sensors, same for actuators.
	[DX] = mnesia:read({dx,DX_Id}),
	[Cx] = mnesia:read({cortex,Cx_Id}),
	io:format("Inside add_Sensor()~n"),
	case Cx#cortex.type of
		neural ->
			case Cx#cortex.cids--[N_Id|| {N_Id,_Filter_Tag}<-lists:flatten([NIdPs|| {_Sensor,NIdPs} <- Cx#cortex.ct])] of
				[] ->
					exit("******** ERROR: No unconnected Neurons in add_SensorLink(DX_Id,Cx_Id)~n");
				NId_Pool ->
					io:format("****************add_SensorLink(DX_Id,Cx_Id) NId_Pool:~p~n",[NId_Pool]),
					IId_Pool = [Cx_Id],
					N_Id = lists:nth(random:uniform(length(NId_Pool)),NId_Pool),
					%add_INeuroLinks(DX_Id,N_Id,IId_Pool),
					[link_FromElementToElement(DX_Id,From_Id,N_Id) || From_Id <-IId_Pool],
					update_EvoHist(DX_Id,add_SensorLink,N_Id,Cx_Id,void,void,void)
			end;
			%exit("******** ERROR: No Neural type add_Sensor(DX_Id,Cx_Id) exists yet~n");
		hypercube ->
			CurrentSensors = Cx#cortex.sensors,
			case ?SENSOR_TAGS--CurrentSensors of
				[] ->
					exit("******** ERROR: No new Sensors to add in add_SensorLink(DX_Id,Cx_Id)~n");
				AvailableNew_Sensors ->
					NewSensor = lists:nth(random:uniform(length(AvailableNew_Sensors)),AvailableNew_Sensors),
					mnesia:write(Cx#cortex{
						sensors = [NewSensor|CurrentSensors]
					}),
				update_EvoHist(DX_Id,add_SensorLink,NewSensor#sensor.name,Cx_Id,void,void,void)
			end
	end.
	
%--------------------------------add_Actuator--------------------------------	
add_ActuatorLink(DX_Id,Cx_Id)->%TODO in the case of a Neural system, it links to an Actuator, sometimes new, sometimes the same.
	[DX] = mnesia:read({dx,DX_Id}),
	[Cx] = mnesia:read({cortex,Cx_Id}),
	io:format("Inside add_Actuator()~n"),
	case Cx#cortex.type of
		neural ->
			case Cx#cortex.cids--lists:flatten([NIds||{_Actuator,NIds} <-Cx#cortex.cf]) of
				[] ->
					exit("******** ERROR: No unconnected Neurons in add_ActuatorLink(DX_Id,Cx_Id)~n");
				NId_Pool ->
					io:format("****************add_ActuatorLink(DX_Id,Cx_Id) NId_Pool:~p~n",[NId_Pool]),
					OId_Pool = [Cx_Id],
					N_Id = lists:nth(random:uniform(length(NId_Pool)),NId_Pool),
					%add_ONeuroLinks(DX_Id,N_Id,OId_Pool),
					[link_FromElementToElement(DX_Id,N_Id,To_Id) || To_Id <- OId_Pool],
					update_EvoHist(DX_Id,add_ActuatorLink,N_Id,Cx_Id,void,void,void)
			end;
			%exit("******** ERROR: No Neural type add_Actuator(DX_Id,Cx_Id) exists yet~n");
		hypercube ->
			CurrentActuators = Cx#cortex.actuators,
			case ?ACTUATOR_TAGS--CurrentActuators of
				[] ->
					exit("******** ERROR: No new Actuators to add in add_ActuatorLink(DX_Id,Cx_Id)~n");
				AvailableNew_Actuators ->
					NewActuator = lists:nth(random:uniform(length(AvailableNew_Actuators)),AvailableNew_Actuators),
					mnesia:write(Cx#cortex{
						actuators = [NewActuator|CurrentActuators]
					}),
					update_EvoHist(DX_Id,add_ActuatorLink,NewActuator#actuator.name,Cx_Id,void,void,void)
			end
	end.

%--------------------------------NeuroSplice--------------------------------
%%%Notes:
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
neurolink_OutputSplice(DX_Id,Cx_Id)-> %TODO: Currently does only forward based splicing.
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	[BeforeNL_Cx] = mnesia:read({cortex,Cx_Id}),
	BeforeNL_Pattern = BeforeNL_Cx#cortex.pattern,
	BeforeNL_CIds = BeforeNL_Cx#cortex.cids,
	Tot_Neurons = length(BeforeNL_CIds),
	N_Id = lists:nth(random:uniform(Tot_Neurons),BeforeNL_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	%O_Ids = N#neuron.o,
	O_IdPool = case above_LimitedCIds(Cx_Id,N_Id,N#neuron.o,[]) of
		[] ->
			exit("********ERROR:neurolink_OutputSplice:: NeuroLink_OutputSplice O_IdPool == []");
		Ids ->
			Ids
	end,
%O_IdPool = N#neuron.o,
%O_IdPool = case different_LimitedCIds(SC_Id,N_Id,N#neuron.o,[]) of
%	[] ->
%		exit("********ERROR:neurolink_OutputSplice:: NeuroLink_OutputSplice O_IdPool == []");	
%	Ids ->
%		Ids
%end,
io:format("neurolink_OutputSplice(DX_Id,Cx_Id)::N_Id~p, O_IdPool:~p~n",[N_Id,O_IdPool]),	
		
	Tot_OLinks = length(O_IdPool),
	O_Id = lists:nth(random:uniform(Tot_OLinks),O_IdPool),
	{{TLI,_},neuron} = N_Id,
	{NewN_Id,NewLI} = case O_Id of
		Cx_Id ->
			NewLI = get_NewLI(TLI,su,next,BeforeNL_Pattern),
			case lists:keymember(NewLI,1,BeforeNL_Pattern) of %To make sure that the NewLI is either the last layer or below, otherwise add new layer.
				true ->
					%insert_NewLayer(SC_Id,NewLI),
					{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI};
				false ->
					insert_NewLayer(Cx_Id,NewLI),
					{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI}
			end;
		{{OLI,NumId},neuron} ->
			case OLI =< TLI of %In the case that recursive neurolink_OutputSplices are allowed, which is not in the current version.
				true ->
					NewLI = TLI,
					{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI};
				false ->
					NewLI = get_NewLI(TLI,OLI,next,BeforeNL_Pattern),
					case lists:keymember(NewLI,1,BeforeNL_Pattern) of %If NewLI member, then np, if not add a new layer to pattern.
						true->
							{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI};
						false ->
							insert_NewLayer(Cx_Id,NewLI),
							{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI}
					end
			end
	end,
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Pattern = Cx#cortex.pattern,
	CIds = Cx#cortex.cids,
	technome_constructor:construct_Neuron(Cx_Id,Generation,NewN_Id,{0,[]},{1,[]},get_SpeCon(specie_id,DX#dx.specie_id)),
	U_Pattern = add_UnitToPattern(Pattern,NewLI,[]),
	mnesia:write(Cx#cortex{
		pattern = U_Pattern,
		cids = [NewN_Id|CIds]}),

	[NewN] = mnesia:read({neuron,NewN_Id}),
	mnesia:write(NewN#neuron{
		ivl = NewN#neuron.ivl + 1,
		i = [{N_Id,1}|NewN#neuron.i],
		dwp = case NewN#neuron.type of
			standard ->
				[{N_Id,[technome_constructor:null_wt()]}|NewN#neuron.dwp];
			bst ->
				NewN#neuron.dwp
		end,
		o = [O_Id|NewN#neuron.o]
	}),
	[UN] = mnesia:read({neuron,N_Id}),
	mnesia:write(UN#neuron{
		o = [NewN_Id|UN#neuron.o] -- [O_Id]}),
	
	case O_Id of
		Cx_Id ->
			[Cortex] = mnesia:read({cortex,Cx_Id}),
			U_CxCF = update_CxCF_NId(N_Id,NewN_Id,Cortex#cortex.cf,[]),
			io:format("neurolink_OutputSplice(DX_Id,Cx_Id)::CF:~p U_CF:~p~n",[Cortex#cortex.cf,U_CxCF]),
			mnesia:write(Cortex#cortex{cf = U_CxCF});
		_ ->
			[ON] = mnesia:read({neuron,O_Id}),
			mnesia:write(ON#neuron{
				i = lists:keyreplace(N_Id,1,ON#neuron.i,{NewN_Id,1}),
				dwp = case ON#neuron.type of
					standard ->
						{value,{N_Id,WeightsP}} = lists:keysearch(N_Id,1,ON#neuron.dwp),
						lists:keyreplace(N_Id,1,ON#neuron.dwp,{NewN_Id,WeightsP});
					bst ->
						ON#neuron.dwp
				end})
	end,
	update_EvoHist(DX_Id,neurolink_OutputSplice,NewN_Id,N_Id,N_Id,O_Id,void).

	update_CxCF_NId(N_Id,NewN_Id,[{Tag,NIds}|CxCF],Acc)->
		case lists:member(N_Id,NIds) of
			true ->
				U_NIds = replace(N_Id,NewN_Id,NIds,[]),
				update_CxCF_NId(N_Id,NewN_Id,CxCF,[{Tag,U_NIds}|Acc]);
			false ->
				update_CxCF_NId(N_Id,NewN_Id,CxCF,[{Tag,NIds}|Acc])
		end;
	update_CxCF_NId(_N_Id,NewN_Id,[],Acc)->
		lists:reverse(Acc).
		
		replace(OldN_Id,NewN_Id,[N_Id|N_Ids],Acc)->
			case OldN_Id == N_Id of
				true ->
					replace(OldN_Id,NewN_Id,N_Ids,[NewN_Id|Acc]);
				false ->
					replace(OldN_Id,NewN_Id,N_Ids,[N_Id|Acc])
			end;
		replace(_OldN_Id,_NewN_Id,[],Acc)->
			lists:reverse(Acc).

neurolink_InputSplice(DX_Id,Cx_Id)->%CIds = ContainedWithinIds, the Ids contained within some module (Cx in this case).
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	[BeforeNL_Cx] = mnesia:read({cortex,Cx_Id}),
	BeforeNL_Pattern = BeforeNL_Cx#cortex.pattern,
	BeforeNL_CIds = BeforeNL_Cx#cortex.cids,
	Tot_Neurons = length(BeforeNL_CIds),
	N_Id = lists:nth(random:uniform(Tot_Neurons),BeforeNL_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	%I_Ids = [I_Id||{I_Id,_IVL}<-N#neuron.i],
	I_IdPool = case below_LimitedCIds(Cx_Id,N_Id,[I_Id||{I_Id,_IVL}<-N#neuron.i],[]) of
		[] ->
			exit("********ERROR:neurolink_InputSplice:: NeuroLink_InputSplice I_IdPool == []");
		Ids ->
			Ids
	end,

io:format("neurlink_InputSplice(DX_Id,Cx_Id)::N_Id~p, I_IdPool:~p~n",[N_Id,I_IdPool]),	
		
	Tot_ILinks = length(I_IdPool),
	I_Id = lists:nth(random:uniform(Tot_ILinks),I_IdPool),
	{{TLI,_},neuron} = N_Id,
	io:format("neurlink_InputSplice(DX_Id,Cx_Id)::I_Id:~p~n",[I_Id]),
	{NewN_Id,NewLI} = case I_Id of
		Cx_Id ->
			NewLI = get_NewLI(TLI,su,prev,BeforeNL_Pattern),
			io:format("neurlink_InputSplice(DX_Id,Cx_Id)::NewLI:~p~n",[{NewLI,lists:keymember(NewLI,1,BeforeNL_Pattern)}]),
			case lists:keymember(NewLI,1,BeforeNL_Pattern) of %To make sure that the NewLI is either the first layer or above, otherwise add new layer.
				true ->
					%insert_NewLayer(SC_Id,NewLI),
					{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI};
				false ->
					insert_NewLayer(Cx_Id,NewLI),
					{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI}
			end;
		{{ILI,NumId},neuron} ->
			case ILI >= TLI of %In the case that recursive neurolink_InputSplices are allowed, which is not in the current version.
				true ->
					NewLI = TLI,
					{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI};
				false ->
					NewLI = get_NewLI(TLI,ILI,prev,BeforeNL_Pattern),
					case lists:keymember(NewLI,1,BeforeNL_Pattern) of %If NewLI member, then np, if not add a new layer to pattern.
						true->
							{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI};
						false ->
							insert_NewLayer(Cx_Id,NewLI),
							{{{NewLI,technome_constructor:generate_UniqueId()},neuron},NewLI}
					end
			end
	end,
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Pattern = Cx#cortex.pattern,
	CIds = Cx#cortex.cids,
	technome_constructor:construct_Neuron(Cx_Id,Generation,NewN_Id,{0,[]},{1,[]},get_SpeCon(specie_id,DX#dx.specie_id)),
	U_Pattern = add_UnitToPattern(Pattern,NewLI,[]),
	mnesia:write(Cx#cortex{
		pattern = U_Pattern,
		cids = [NewN_Id|CIds]}),

	[NewN] = mnesia:read({neuron,NewN_Id}),
	{I_Id,I_VL} = lists:keyfind(I_Id, 1, N#neuron.i),
io:format("NewN#neuron.type:~p~n",[NewN#neuron.type]),
	mnesia:write(NewN#neuron{
		ivl = NewN#neuron.ivl + I_VL,
		i = [{I_Id,I_VL}|NewN#neuron.i],
		dwp = case NewN#neuron.type of
			standard ->
				[{I_Id,[technome_constructor:null_wt()||_<-lists:seq(1,I_VL)]}|NewN#neuron.dwp];
			bst ->
				NewN#neuron.dwp
		end,
		o = [N_Id|NewN#neuron.o]
	}),
	
	[UN] = mnesia:read({neuron,N_Id}),
	mnesia:write(UN#neuron{
		i = [{NewN_Id,1}|UN#neuron.i] -- [{I_Id,I_VL}],
		dwp = [{NewN_Id,[technome_constructor:null_wt()]}|lists:keydelete(I_Id, 1,UN#neuron.dwp)]
	}),

	case I_Id of
		Cx_Id ->
			[Cortex] = mnesia:read({cortex,Cx_Id}),
			U_CxCT = update_CxCT_NId(N_Id,NewN_Id,Cortex#cortex.ct,[]),
			io:format("CT:~p U_CT:~p~n",[Cortex#cortex.ct,U_CxCT]),
			mnesia:write(Cortex#cortex{ct = U_CxCT});
		_ ->
			[IN] = mnesia:read({neuron,I_Id}),
			mnesia:write(IN#neuron{
				o = replace(N_Id,NewN_Id,IN#neuron.o,[])
			})
	end,
	update_EvoHist(DX_Id,neurolink_InputSplice,NewN_Id,N_Id,N_Id,I_Id,void).

	update_CxCT_NId(N_Id,NewN_Id,[{CT_Tag,NIdPs}|CxCT],Acc)->%CT_Tag= Sensor|Geotag Filter=Link tag
		case lists:keyfind(N_Id,1,NIdPs) of
			{N_Id,Filter} ->
				U_NIdPs = lists:keyreplace(N_Id,1,NIdPs,{NewN_Id,Filter}),
			%	U_NIds = replace(N_Id,NewN_Id,NIds,[]),
				update_CxCT_NId(N_Id,NewN_Id,CxCT,[{CT_Tag,U_NIdPs}|Acc]);
			false ->
				update_CxCT_NId(N_Id,NewN_Id,CxCT,[{CT_Tag,NIdPs}|Acc])
		end;
	update_CxCT_NId(_N_Id,NewN_Id,[],Acc)->
		lists:reverse(Acc).

	filter_LinkType(LinkType,FilterType,SU_Id,TId,Id_Pool)->
%		io:format("LinkType,FilterType,SU_Id,TId,Id_Pool:~p~n",[{LinkType,FilterType,SU_Id,TId,Id_Pool}]),
		{{TLI,_UniqueId},_NodeType} = TId,
		case LinkType of
			recursive ->
				Id_Pool;
			feedforward ->
				FF_IdPool = case FilterType of
					above ->
						filter_LT_above(TLI,SU_Id,Id_Pool,[]);
					below ->
						filter_LT_below(TLI,SU_Id,Id_Pool,[])
				end,
				FF_IdPool
		end.
		
		filter_LT_above(TLI,SU_Id,[Id|Id_Pool],Acc)->
			case Id of
				SU_Id ->
					filter_LT_above(TLI,SU_Id,Id_Pool,[Id|Acc]);
				{{LI,_UniqueId},NodeType} ->
					case LI > TLI of
						true -> 
							filter_LT_above(TLI,SU_Id,Id_Pool,[Id|Acc]);
						false ->
							filter_LT_above(TLI,SU_Id,Id_Pool,Acc)
					end
			end;
		filter_LT_above(_TLI,_SU_Id,[],Acc)->
			lists:reverse(Acc).

		filter_LT_below(TLI,SU_Id,[Id|Id_Pool],Acc)->
			case Id of
				SU_Id ->
					filter_LT_below(TLI,SU_Id,Id_Pool,[Id|Acc]);
				{{LI,_UniqueId},NodeType}->
					case LI < TLI of
						true ->
							filter_LT_below(TLI,SU_Id,Id_Pool,[Id|Acc]);
						false ->

							filter_LT_below(TLI,SU_Id,Id_Pool,Acc)
					end
			end;
		filter_LT_below(_TLI,_SU_Id,[],Acc)->
			lists:reverse(Acc).

%--------------------------------different_LimitedCIds--------------------------------
%Filters Ids such that all ids are those that do not blong to the same layer.
	different_LimitedCIds(SU_Id,Target_Id,[Id|Ids],Limited_CIdsAcc)->
		case Id of
			SU_Id ->
				different_LimitedCIds(SU_Id,Target_Id,Ids,[Id|Limited_CIdsAcc]);
			_ ->
				{{TLI,_},neuron} = Target_Id,
				{{LI,_},neuron} = Id,
				case LI =/= TLI of
					true ->
						different_LimitedCIds(SU_Id,Target_Id,Ids,[Id|Limited_CIdsAcc]);
					false ->
						different_LimitedCIds(SU_Id,Target_Id,Ids,Limited_CIdsAcc)
				end
		end;
	different_LimitedCIds(_SU_Id,_Target_Id,[],Acc)->
		Acc.

%--------------------------------Add Threshold--------------------------------
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
add_Threshold(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_CIds = Cx#cortex.cids,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	add_Threshold(DX_Id,Cx_Id,N_Id).
	
	add_Threshold(DX_Id,Cx_Id,N_Id)->
		[DX] = mnesia:read({dx,DX_Id}),
		Generation = DX#dx.generation,
		[N] = mnesia:read({neuron,N_Id}),
		DWP = N#neuron.dwp,
		case lists:keymember(threshold, 1, DWP) of
			true ->
				exit("********ERROR:add_Threshold:: This Neuron already has a threshold part.");
			false ->
				U_DWP = lists:append(DWP,[{threshold,[technome_constructor:weight_tuple()]}]),
				mnesia:write(N#neuron{
					dwp = U_DWP,
					generation = Generation})
		end,
		update_EvoHist(DX_Id,add_Threshold,void,N_Id,void,void,void).

%--------------------------------remove Thresholds--------------------------------
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
remove_Threshold(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_CIds = Cx#cortex.cids,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	remove_Threshold(DX_Id,Cx_Id,N_Id).
	
	remove_Threshold(DX_Id,Cx_Id,N_Id)->
		[DX] = mnesia:read({dx,DX_Id}),
		Generation = DX#dx.generation,
		[N] = mnesia:read({neuron,N_Id}),
		DWP = N#neuron.dwp,
		case lists:keymember(threshold, 1, DWP) of
			true ->
				U_DWP = lists:keydelete(threshold,1,DWP),
				mnesia:write(N#neuron{
					dwp = U_DWP,
					generation = Generation});
			false->
				exit("********ERROR:remove_Threshold:: This Neuron does not have a threshold part.")
		end,
		update_EvoHist(DX_Id,remove_Threshold,void,N_Id,void,void,void).

%--------------------------------Add Neurons--------------------------------
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
add_Neuron(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_Pattern = Cx#cortex.pattern,
	{TargetLayer,_} = lists:nth(random:uniform(length(Cx_Pattern)),Cx_Pattern),
	add_Neurons(DX_Id,Cx_Id,TargetLayer,1).

%--------------------------------Add Neurons--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output:
%%%MsgComunication:					
	add_Neurons(_DX_Id,_Cx_Id,_TLI,0)->
		done;
	add_Neurons(DX_Id,Cx_Id,TLI,NeuroIndex)->
		add_Neuron(DX_Id,Cx_Id,TLI),
		add_Neurons(DX_Id,Cx_Id,TLI,NeuroIndex-1).
	
proper_OIds(SU_Id,Id_Pool,PresentIds,Elements_Requested)->
	case get_OIdPool(SU_Id,Id_Pool,PresentIds) of
		[] ->
			exit("********ERROR:proper_OIds:: Id_Pool is empty");
		OId_Pool ->
			get_UniqueIds(OId_Pool,Elements_Requested,[])
	end.
	
	get_OIdPool(SU_Id,Id_Pool,PresentIds)->
		%io:format("get_OIdPool:: SU_Id:~p Id_Pool:~p PresentIds:~p~n",[SU_Id,Id_Pool,PresentIds]),
		[Cx] = mnesia:read({cortex,SU_Id}),
		[DX] = mnesia:read({dx,Cx#cortex.su_id}),
		CF = Cx#cortex.cf,
		Flag =case Cx#cortex.type of
			neural ->
				InvalidActuators = [Actuator || {Actuator,NIds}<-CF, Actuator#actuator.tot_vl == length(NIds)],
				?ACTUATOR_TAGS -- InvalidActuators;
			hypercube ->
				InvalidTags = [SCF || {SCF,NIds}<-CF, SCF#sCF.tot_vl == length(NIds)],
				?HYPERCUBE_CFTAGS -- InvalidTags
		end,
		case Flag of
			[] ->
				%io:format("***** Tag_Pool -- CF_Tags == []~n"),
				Id_Pool -- PresentIds;
			_ ->
				[SU_Id|Id_Pool] -- PresentIds
		end.
	
	get_UniqueIds(Id_Pool,Elements_Requested)-> 
		get_UniqueIds(Id_Pool,Elements_Requested,[]).
	get_UniqueIds(_Id_Pool,0,IdAcc)->
		IdAcc;
	get_UniqueIds([],_Id_Index,[])->
		exit("********ERROR:get_UniqueIds:: Id_Pool == IdAcc == []");
	get_UniqueIds([],_Id_Index,IdAcc)->
		IdAcc;
	get_UniqueIds(Id_Pool,Id_Index,IdAcc)->
		UniqueId = lists:nth(random:uniform(length(Id_Pool)),Id_Pool),
%		io:format("UniqueId:~p~n",[UniqueId]),
		get_UniqueIds(Id_Pool--[UniqueId],Id_Index-1,[UniqueId|IdAcc]).
	
add_Neuron(DX_Id,Cx_Id,TargetLayer)-> %TODO: Crash/End if in last layer, feed forward, and Cx already fully connected to.
	%Extract cointained Neuro Ids from SC and Pattern,
	%Create new Neuron(s?) technome.
	%Update Ids/I/O/RO for the Neuron and the Neurons/SubCore he is connected to.
	%Cortex:{{Id,core},TotCFVL,CF:[{Id1,VL1},{Id2,VL2}...],TotCTVL,CT:[Id1,Id2...],Type,Version,Pattern,CIds,Mutagens}
	%construct_NeuronTechnome(Technome,Cx_Id,N_Id,{N_TotIVL,N_I},{N_TotOVL,N_O})
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_Pattern = Cx#cortex.pattern,
	Cx_CIds = Cx#cortex.cids,
	N_Id = {{TargetLayer,technome_constructor:generate_UniqueId()},neuron},
	technome_constructor:construct_Neuron(Cx_Id,Generation,N_Id,{0,[]},{1,[]},get_SpeCon(specie_id,DX#dx.specie_id)),
	U_Cx_Pattern = add_UnitToPattern(Cx_Pattern,TargetLayer,[]),
	mnesia:write(Cx#cortex{
		pattern = U_Cx_Pattern,
		cids = [N_Id|Cx_CIds]}),
	%%%%NEWMETHOD:
	Link_Form = Cx#cortex.link_form,
	Partial_IIdPool = [Cx_Id|filter_LinkType(Link_Form,below,Cx_Id,N_Id,Cx_CIds)],
	ILinks_Requested = 1,%random:uniform(round(math:sqrt(length(Partial_IIdPool),1/3))),
	IId_Pool = get_UniqueIds(Partial_IIdPool,ILinks_Requested),
	Partial_OIdPool = filter_LinkType(Link_Form,above,Cx_Id,N_Id,Cx_CIds),
	OLinks_Requested = 1,%random:uniform(round(math:sqrt(length(Partial_OIdPool),1/3))),
	OId_Pool = proper_OIds(Cx_Id,Partial_OIdPool,[],OLinks_Requested),
%	add_INeuroLinks(DX_Id,N_Id,IId_Pool),
%	add_ONeuroLinks(DX_Id,N_Id,OId_Pool),
	[link_FromElementToElement(DX_Id,From_Id,N_Id) || From_Id <- IId_Pool],
	[link_FromElementToElement(DX_Id,N_Id,To_Id) || To_Id <- OId_Pool],
	update_EvoHist(DX_Id,add_Neuron,void,N_Id,IId_Pool,OId_Pool,void).

link_FromElementToElement(DX_Id,From_ElementId,To_ElementId)->
	io:format("link_FromElementToElement(~p,~p,~p)~n",[DX_Id,From_ElementId,To_ElementId]),
	case {From_ElementId,To_ElementId} of
		{{_FromSId,neuron},{_ToSId,neuron}} ->
			link_FromNeuronToNeuron(DX_Id,From_ElementId,To_ElementId);
		{{_FromSId,cortex},{_ToSId,neuron}} ->
			link_FromCortexToNeuron(DX_Id,From_ElementId,To_ElementId);
		{{_FromSId,neuron},{_ToSId,cortex}} ->
			link_FromNeuronToCortex(DX_Id,From_ElementId,To_ElementId)
	end.

link_FromNeuronToNeuron(DX_Id,From_NeuronId,To_NeuronId)->
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	[FromN] = mnesia:read({neuron,From_NeuronId}),
%From Part
	{{ToLI,_},_} = To_NeuronId,
	U_FromN = link_FromNeuron(FromN,To_NeuronId,ToLI,Generation),
%	io:format("FromN:~p~n~n U_FromN:~p~n",[FromN,U_FromN]),
	mnesia:write(U_FromN),
%To Part
	[ToN] = mnesia:read({neuron,To_NeuronId}),%We read it afterwards, in the case that it's the same Element. Thus we do not overwrite the earlier changes.
	FromOVL = FromN#neuron.ovl,
	U_ToN = link_ToNeuron(From_NeuronId,FromOVL,ToN,Generation),
	mnesia:write(U_ToN).

	link_FromNeuron(FromN,ToId,ToLI,Generation)->
		{{FromLI,_},_} = FromN#neuron.id,
%		{{ToLI,_},_} = To_NeuronId,
%io:format("FromLI:~p ToLI:~p~n",[FromLI,ToLI]),
		FromO = FromN#neuron.o,
		FromRO = FromN#neuron.ro,
		case lists:member(ToId, FromO) of
			true ->
				exit("******** ERROR:add_NeuronO[can not add O_Id to Neuron]: ~p already a member of ~p~n",[ToId,FromN#neuron.id]);
			false ->
				{U_FromO,U_FromRO} = case FromLI >= ToLI of
					true -> %io:format("Recurisve Addition O~p~n",[erlang:get_stacktrace()]), %exit("crashed"),
						{[ToId|FromO],[ToId|FromRO]};
					false ->
						{[ToId|FromO],FromRO}
				end,
%				io:format("FromO:~p U_FromO:~p FromRO:~p U_FromRO:~p~n",[FromO,U_FromO,FromRO,U_FromRO]),
				FromN#neuron{
					o = U_FromO,
					ro = U_FromRO,
					generation = Generation}
		end.

	link_ToNeuron(FromId,FromOVL,ToN,Generation)->
		ToIVL = ToN#neuron.ivl,
		ToI = ToN#neuron.i,
		ToDWP = ToN#neuron.dwp,
		case lists:keymember(FromId, 1, ToI) of
			true ->
				exit("ERROR:add_NeuronI::[can not add I_Id]: ~p already a member of ~p~n",[FromId,ToN#neuron.id]);
			false ->
				U_ToIVL = ToIVL+FromOVL,
				U_ToI = [{FromId,FromOVL}|ToI],
				U_ToDWP = case ToN#neuron.type of
					standard -> 
						[{FromId, [technome_constructor:weight_tuple()||_ <-lists:seq(1,FromOVL)]}|ToDWP];
					bst -> 
						ToDWP
				end,
				ToN#neuron{
					ivl = U_ToIVL,
					i = U_ToI,
					dwp = U_ToDWP,
					generation = Generation}
		end.

link_FromCortexToNeuron(DX_Id,From_CortexId,To_NeuronId)->
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	[FromCx] = mnesia:read({cortex,From_CortexId}),
%From Part
	CT_TagP=get_CxCT(DX,FromCx),
	CT = FromCx#cortex.ct,
	{_CT_Tags,NIdPs} = lists:unzip(CT),
	{NIds,_Filters} = lists:unzip(lists:flatten(NIdPs)),
	case lists:member(To_NeuronId,NIds) of
		true ->
			exit("ERROR:add_CortexCT[can not add CT_Id]: ~p already a member of ~p~n",[To_NeuronId,From_CortexId]);
		false ->
			{CT_Tag,Filter} = CT_TagP,
			U_CT=case lists:keymember(CT_Tag,1,CT) of
				true ->
					{value,{CT_Tag,N_IdPs}} = lists:keysearch(CT_Tag, 1, CT),
					lists:keyreplace(CT_Tag,1,CT,{CT_Tag,[{To_NeuronId,Filter}|N_IdPs]});
				false ->
					[{CT_Tag,[{To_NeuronId,Filter}]}|CT]
			end,
			mnesia:write(FromCx#cortex{ct=U_CT})
	end,
%To Part
	[ToN] = mnesia:read({neuron,To_NeuronId}),
	FromOVL=case CT_TagP of%CT_Tag=GeoTag|Sensor
		{_CT_Tag,{single,_Index}}->
			1;
		{_CT_Tag,{block,VL}}->
			VL
	end,
	U_ToN = link_ToNeuron(From_CortexId,FromOVL,ToN,Generation),
	mnesia:write(U_ToN).
			
	get_CxCT(DX,Cx)->
%		[DX] = mnesia:read({dx,DX_Id}),
%		[Cx] = mnesia:read({cortex,Cx_Id}),
		case Cx#cortex.type of
			hypercube ->
				CTTAGsP_Pool = [{SCT,{block,SCT#sCT.tot_vl}} || SCT <-?HYPERCUBE_CTTAGS],
				PoolSize = length(CTTAGsP_Pool),
				CT_TagP = lists:nth(random:uniform(PoolSize),CTTAGsP_Pool);
			neural ->
				CTTAGsP_Pool = case lists:nth(random:uniform(length(?SCCTTypes)),?SCCTTypes) of
					single ->
						CT= lists:flatten([[{Sensor,{single,I_Index}}||I_Index<-lists:seq(1,Sensor#sensor.tot_vl)]||Sensor<-?SENSOR_TAGS]),
						%io:format("CT:~p I:~p Id:~p ~n",[CT,I,Id]),
						CT;
					block ->
						CT = [{Sensor,{block,Sensor#sensor.tot_vl}} || Sensor <-?SENSOR_TAGS],
						CT;
					all ->
						Tot_SensorVL=lists:sum([Sensor#sensor.tot_vl||Sensor<-?SENSOR_TAGS]),
						CT = [{?SENSOR_TAGS,{all,Tot_SensorVL}}],
						CT
				end,
				PoolSize = length(CTTAGsP_Pool),
				CT_TagP = lists:nth(random:uniform(PoolSize),CTTAGsP_Pool)
		end,
		CT_TagP.
		
link_FromNeuronToCortex(DX_Id,From_NeuronId,To_CortexId)->
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	[FromN] = mnesia:read({neuron,From_NeuronId}),
%From Part
	ToLI = inf,
	U_FromN = link_FromNeuron(FromN,To_CortexId,ToLI,Generation),
	mnesia:write(U_FromN),
%To Part
	[ToCx] = mnesia:read({cortex,To_CortexId}),
	CF_Tag = get_CxCF(DX,ToCx),
	CF = ToCx#cortex.cf,
	{_CF_Tags,NIdPs} = lists:unzip(CF),
	NIds = lists:flatten(NIdPs),
	case lists:member(From_NeuronId,NIds) of
		true ->
			exit("ERROR:add_CortexCF[can not add CF_Id]: ~p already a member of ~p~n",[From_NeuronId,To_CortexId]);
		false ->
			U_CF=case lists:keymember(CF_Tag,1,CF) of
				true ->
					{value,{CF_Tag,N_Ids}} = lists:keysearch(CF_Tag, 1, CF),
					lists:keyreplace(CF_Tag,1,CF,{CF_Tag,[From_NeuronId|N_Ids]});
				false ->
					[{CF_Tag,[From_NeuronId]}|CF]
			end,
			mnesia:write(ToCx#cortex{cf=U_CF})
	end.

	get_CxCF(DX,Cx)->
%		[Cx] = mnesia:read({cortex,Cx_Id}),
%		[DX] = mnesia:read({dx,Cx#cortex.su_id}),
		CxCF_TagPs=case Cx#cortex.type of
			neural ->
				InvalidActuators=[Actuator||{Actuator,NIds} <-Cx#cortex.cf,Actuator#actuator.tot_vl=<length(NIds)], 
				?ACTUATOR_TAGS -- InvalidActuators;
			hypercube ->
				InvalidTags=[CF_Tag||{CF_Tag,NIds} <-Cx#cortex.cf,CF_Tag#sCF.tot_vl=<length(NIds)], 
				?HYPERCUBE_CFTAGS -- InvalidTags
		end,
		case CxCF_TagPs of
			[] ->
				exit("********ERROR:get_CxCF:: no more unique CFTAGS left");
			CFTAGS ->
				io:format("CFTAGS:~p~n",[CxCF_TagPs]),
				lists:nth(random:uniform(length(CFTAGS)),CFTAGS)
		end.

cutlink_FromElementToElement(DX_Id,From_ElementId,To_ElementId)->
	case {From_ElementId,To_ElementId} of
		{{_FromSId,neuron},{_ToSId,neuron}} ->
			cutlink_FromNeuronToNeuron(DX_Id,From_ElementId,To_ElementId);
		{{_FromSId,cortex},{_ToSId,neuron}} ->
			cutlink_FromCortexToNeuron(DX_Id,From_ElementId,To_ElementId);
		{{_FromSId,neuron},{_ToSId,cortex}} ->
			cutlink_FromNeuronToCortex(DX_Id,From_ElementId,To_ElementId)
	end.
				
cutlink_FromNeuronToNeuron(DX_Id,From_NeuronId,To_NeuronId)->
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	[FromN] = mnesia:read({neuron,From_NeuronId}),
%From Part
	U_FromN = cutlink_FromNeuron(FromN,To_NeuronId,Generation),
	mnesia:write(U_FromN),
%To Part
	[ToN] = mnesia:read({neuron,To_NeuronId}),
	U_ToN = cutlink_ToNeuron(From_NeuronId,ToN,Generation),
	mnesia:write(U_ToN).
	
	cutlink_FromNeuron(FromN,ToId,Generation)->
		FromO = FromN#neuron.o,
		FromRO = FromN#neuron.ro,
		%SU_Id = FromN#neuron.su_id,
		%io:format("O_Id:~p O:~p~n",[O_Id,O]),
		case lists:member(ToId, FromO) of
			true ->
				U_FromO = FromO--[ToId],
				U_FromRO = FromRO--[ToId],%Not necessary if not recursive...
				FromN#neuron{
					o = U_FromO,
					ro = U_FromRO};
			false ->
				exit("ERROR[can not remove O_Id]: ~p not a member of ~p~n",[ToId,FromN#neuron.id])
		end.
		
	cutlink_ToNeuron(FromId,ToN,Generation)->
		%[N] = mnesia:read({neuron,N_Id}),
		ToIVL = ToN#neuron.ivl,
		ToI = ToN#neuron.i,
		ToDWP = ToN#neuron.dwp,
		%io:format("I_Id:~p I:~p~n",[I_Id,I]),
		case lists:keymember(FromId, 1, ToI) of
			true ->
				{value,{FromId,ToWPC}}=lists:keysearch(FromId,1,ToDWP),
				U_ToIVL = ToIVL-length(ToWPC),
				U_ToI = lists:keydelete(FromId,1,ToI),
				U_ToDWP = case ToN#neuron.type of
					standard ->
						lists:keydelete(FromId,1,ToDWP);
					bst ->
						ToDWP
				end,
				ToN#neuron{
					ivl = U_ToIVL,
					i = U_ToI,
					dwp = U_ToDWP,
					generation = Generation};
			false ->
				exit("ERROR[can not remove I_Id]: ~p not a member of ~p~n",[FromId,ToN#neuron.id])
		end.
%CT:
%	neural:		[{Sensor1,[{N_Id1,FilterTag1},{N_Id2,FilterTag2}...]}...] FilterTag:{single,Index} | {block,VL}
%	hypercube:	[{sCT,[{N_Id1,FilterTag1},{N_Id2,FilterTag2}...]}...] FilterTag:{single,Index} | {block,VL}	
cutlink_FromCortexToNeuron(DX_Id,From_CortexId,To_NeuronId)->
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	[FromCx] = mnesia:read({cortex,From_CortexId}),
%From Part %TODO: Here we must remove the CT_VL from the TotCTVL that is associated with some tag.
	%TotCTVL = Cx#cortex.ctvl,
	CT = FromCx#cortex.ct,
	{_CT_Tags,NIdPs} = lists:unzip(CT),
	{NIds,_Filters} = lists:unzip(lists:flatten(NIdPs)),
	case lists:member(To_NeuronId,NIds) of
		true ->
			%U_TotCTVL = TotCTVL-CT_VL,
			U_CT = [{CT_Tag,[{N_Id,FilterTag} || {N_Id,FilterTag} <- N_IdPs, N_Id =/= To_NeuronId]} || {CT_Tag,N_IdPs} <- CT],
			mnesia:write(FromCx#cortex{
%				ctvl = U_TotCTVL,
				ct = U_CT});
		false ->
			exit("ERROR[can not remove CT_Id]: ~p not a member of ~p~n",[To_NeuronId,From_CortexId])
	end,
%To Part
	[ToN] = mnesia:read({neuron,To_NeuronId}),
	U_ToN = cutlink_ToNeuron(From_CortexId,ToN,Generation),
	mnesia:write(U_ToN).
	
%CF:
%	neural:		[{Actuator1,[N_Id1...N_Idn]},{Actuator2,[N_Id1...N_Idn]}...]
%	hypercube:	[{sCF1,[N_Id1...N_Idn]},{sCF2,[N_Id2...N_Idn]}...]
cutlink_FromNeuronToCortex(DX_Id,From_NeuronId,To_CortexId)->
	[DX] = mnesia:read({dx,DX_Id}),
	Generation = DX#dx.generation,
	[FromN] = mnesia:read({neuron,From_NeuronId}),
%From Part
	U_FromN = cutlink_FromNeuron(FromN,To_CortexId,Generation),
	mnesia:write(U_FromN),
%To Part %TODO: here we must remove O_VL that is associated with the appropriate Tag.
	[ToCx] = mnesia:read({cortex,To_CortexId}),
	%TotCFVL = Cx#cortex.cfvl,
	CF = ToCx#cortex.cf,
	{_CF_Tags,NId_Lists} = lists:unzip(CF),
	NIds = lists:flatten(NId_Lists),
	case lists:member(From_NeuronId,NIds) of
		true ->
	%		U_TotCFVL = TotCFVL-CF_VL,
			U_CF = [{CF_Tag,lists:delete(From_NeuronId,N_Ids)} || {CF_Tag,N_Ids}<-CF], %TODO: Here it is assumed that there is only one connection fromN toCX
			mnesia:write(ToCx#cortex{
				%cfvl = U_TotCFVL,
				cf = U_CF});
		false ->
			exit("ERROR[can not remove CF_Id]: ~p not a member of ~p~n",[From_NeuronId,To_CortexId])
	end.

%--------------------------------Remove Neuron--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
remove_Neuron(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_CIds = Cx#cortex.cids,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	remove_Neurons(DX_Id,Cx_Id,[N_Id],on).

	remove_Neurons(DX_Id,Cx_Id,[Id|Ids],ProtectionFlag)->
		remove_Neuron(DX_Id,Cx_Id,Id,ProtectionFlag),
		remove_Neurons(DX_Id,Cx_Id,Ids,ProtectionFlag);
	remove_Neurons(_DX_Id,_Cx_Id,[],_ProtectionFlag)->
		done.
		 
remove_Neuron(DX_Id,Cx_Id,N_Id,ProtectionFlag) ->
	case ProtectionFlag of	
		on->
			[Cx] = mnesia:read({cortex,Cx_Id}),
			Cx_CIds = Cx#cortex.cids,
			case length(Cx_CIds) == 1 of
				true -> 
					%modular_mutator:remove_SubCore(DX_Id,SC_Id,on);
			      		exit("********ERROR:remove_Neuron:: no function: remove_SubCore(DX_Id,Cx_Id,on)");
			      	false ->
					remove_Neuron(DX_Id,Cx_Id,N_Id)
			end;
		off ->
			remove_Neuron(DX_Id,Cx_Id,N_Id)
	end.
	
remove_Neuron(DX_Id,Cx_Id,N_Id={{TL,_},_})->
	%Disconnect the Neuron
	%Update Ids/I/O/RO for the remaining Neurons, Neurons to whom and from whom this Neuron was connected.
	%Remove the Neuron
	%Cortex:{{Id,core},TotCFVL,CF:[{Id1,VL1},{Id2,VL2}...],TotCTVL,CT:[Id1,Id2...],Type,Version,Pattern,CIds,Mutagens}
	[N] = mnesia:read({neuron,N_Id}),
	%io:format("N:~p~n",[N]),
	I = N#neuron.i,
	O = N#neuron.o,
	{I_Ids,_IVLs} = lists:unzip(I),
	[cutlink_FromElementToElement(DX_Id,N_Id,To_Id) || To_Id <- O],
	[cutlink_FromElementToElement(DX_Id,From_Id,N_Id) || From_Id<-I_Ids],
%	remove_I(DX_Id,O,[N_Id]),
%	remove_O(DX_Id,I_Ids,[N_Id]),
	mnesia:delete({neuron,N_Id}),
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_Pattern = Cx#cortex.pattern,
	Cx_CIds = Cx#cortex.cids,
	U_Cx_Pattern = remove_UnitFromPattern(Cx_Pattern,TL,1,[]),
	U_Cx_CIds = Cx_CIds--[N_Id],
	mnesia:write(Cx#cortex{
		pattern = U_Cx_Pattern,
		cids = U_Cx_CIds}),
	verify_network(DX_Id),
	update_EvoHist(DX_Id,remove_Neuron,void,N_Id,void,void,void).
	
	remove_UnitFromPattern([{LI,LL}|Pattern],TL,ExtraUnits,U_Pattern)->
		case LI =:= TL of
			true ->
				remove_UnitFromPattern(Pattern,TL,ExtraUnits,[{LI,LL-ExtraUnits}|U_Pattern]);
			false ->
				remove_UnitFromPattern(Pattern,TL,ExtraUnits,[{LI,LL}|U_Pattern])
		end;
	remove_UnitFromPattern([],_TL,_ExtraUnits,U_Pattern)->
		lists:reverse(U_Pattern).

%%%=============================================================== Neuro Mutagens ===============================================================%%%
%--------------------------------Add INeuronLinks--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
add_INLink(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_CIds = Cx#cortex.cids,
	Link_Form = Cx#cortex.link_form,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	Partial_IIdPool = [Cx_Id|filter_LinkType(Link_Form,below,Cx_Id,N_Id,Cx_CIds)],
	I = N#neuron.i,
	{I_Ids,_IVLs} = lists:unzip(I),
	Id_Pool = Partial_IIdPool -- I_Ids,
	case length(Id_Pool)of
		0 ->
			exit("********ERROR:add_INLink:: Neuron already_connected_from_all_ids");
		TotPossibleILinks ->
			IId_Pool = get_UniqueIds(Id_Pool,1),
			%add_INeuroLinks(DX_Id,N_Id,IId_Pool),
			[link_FromElementToElement(DX_Id,From_Id,N_Id) || From_Id <- IId_Pool],
			update_EvoHist(DX_Id,add_INLink,void,N_Id,IId_Pool,void,void)
	end.

	below_LimitedCIds(SU_Id,Target_Id,[Id|Ids],Limited_CIdsAcc)->
		case Id of
			SU_Id ->
				below_LimitedCIds(SU_Id,Target_Id,Ids,[Id|Limited_CIdsAcc]);
			_ ->
				{{TLI,_},NodeType} = Target_Id,
				{{LI,_},NodeType} = Id,
				
				case TLI > LI of
					true ->
						below_LimitedCIds(SU_Id,Target_Id,Ids,[Id|Limited_CIdsAcc]);
					false ->
						below_LimitedCIds(SU_Id,Target_Id,Ids,Limited_CIdsAcc)
				end
		end;
	below_LimitedCIds(_SU_Id,_Target_Id,[],Limited_CIdsAcc)->
		Limited_CIdsAcc.
		
%--------------------------------Remove INeuronLinks--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
remove_INLink(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_CIds = Cx#cortex.cids,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	I = N#neuron.i,
	{I_Ids,_IVLs} = lists:unzip(I),
	RemoveILink = lists:nth(random:uniform(length(I_Ids)),I_Ids),
	remove_INeuroLinks(DX_Id,N_Id,[RemoveILink],on),
	verify_network(DX_Id).
		
remove_INeuroLinks(DX_Id,N_Id,RemoveILinks,ProtectionFlag)->
	case ProtectionFlag of
		on ->
			[N] = mnesia:read({neuron,N_Id}),
			I = N#neuron.i,
			case length(I) =:= length(RemoveILinks) of
				true ->
					SC_Id = N#neuron.su_id,
					remove_Neuron(DX_Id,SC_Id,N_Id,on);
					%io:format("[Function does not yet exist]Remove Neuron in remove_INeuroLinks~n");
				false ->
					[cutlink_FromElementToElement(DX_Id,From_Id,N_Id) || From_Id <- RemoveILinks]
					%remove_INeuroLinks(DX_Id,N_Id,RemoveILinks)
			end;
		off ->
			[cutlink_FromElementToElement(DX_Id,From_Id,N_Id) || From_Id <- RemoveILinks]
			%remove_INeuroLinks(DX_Id,N_Id,RemoveILinks)
	end.
			
%--------------------------------add_ONLink--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
add_ONLink(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_CIds = Cx#cortex.cids,
	Link_Form = Cx#cortex.link_form,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	N_O = N#neuron.o,
	Partial_OIdPool = filter_LinkType(Link_Form,above,Cx_Id,N_Id,Cx_CIds),
	case length([Cx_Id|Partial_OIdPool] -- N_O) of
		0 ->
			exit("********ERROR:add_ONLink:: Neuron already_connected_to_all_ids");
		TotPossibleOLinks ->
%			io:format("TPO:~p~n",[TotPossibleOLinks]),
			OId_Pool = proper_OIds(Cx_Id,Partial_OIdPool,N_O,1), %TODO: This makes sure that the Neuron does not connect to the same element twice
			%add_ONeuroLinks(DX_Id,N_Id,OId_Pool),
			[link_FromElementToElement(DX_Id,N_Id,To_Id) || To_Id <- OId_Pool],
			update_EvoHist(DX_Id,add_ONLink,void,N_Id,void,OId_Pool,void)
	end.

	above_LimitedCIds(SU_Id,Target_Id,[Id|Ids],Limited_CIdsAcc)->
		case Id of
			SU_Id ->
				above_LimitedCIds(SU_Id,Target_Id,Ids,[Id|Limited_CIdsAcc]);
			_ ->
				{{TLI,_},NodeType} = Target_Id,
				{{LI,_},NodeType} = Id,
				
				case TLI < LI of
					true ->
						above_LimitedCIds(SU_Id,Target_Id,Ids,[Id|Limited_CIdsAcc]);
					false ->
						above_LimitedCIds(SU_Id,Target_Id,Ids,Limited_CIdsAcc)
				end
		end;
	above_LimitedCIds(_SU_Id,_Target_Id,[],Limited_CIdsAcc)->
		Limited_CIdsAcc.
		
%--------------------------------remove_ONLink--------------------------------
%%%Notes: Situations can occur when RemoveINeuroLinks disconnects all the links, and thus the length(N_O) of some neuron ends up being 0.
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
remove_ONLink(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Cx_CIds = Cx#cortex.cids,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	N_O = N#neuron.o,
	RemoveOLink = lists:nth(random:uniform(length(N_O)),N_O),
	remove_ONeuroLinks(DX_Id,N_Id,[RemoveOLink],on).
		
remove_ONeuroLinks(DX_Id,N_Id,RemoveOLinks,ProtectionFlag)->
	case ProtectionFlag of
		on ->
			[N] = mnesia:read({neuron,N_Id}),
			N_O = N#neuron.o,
			case length(N_O) == length(RemoveOLinks) of
				true ->
					SC_Id = N#neuron.su_id,
					remove_Neuron(DX_Id,SC_Id,N_Id,on);
					%io:format("[Function does not yet exist]Remove Neuron in remove_ONeurLinks~n");
				false ->
					[cutlink_FromElementToElement(DX_Id,N_Id,To_Id) || To_Id <- RemoveOLinks]
					%remove_ONeuroLinks(DX_Id,N_Id,RemoveOLinks)
			end;
		off ->
			[cutlink_FromElementToElement(DX_Id,N_Id,To_Id) || To_Id <- RemoveOLinks]
			%remove_ONeuroLinks(DX_Id,N_Id,RemoveOLinks)
	end.

%--------------------------------Change Adapter--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
change_PlasticityFunction(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Generation = Cx#cortex.generation,
	Cx_CIds = Cx#cortex.cids,
	Link_Form = Cx#cortex.link_form,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	{Old_PF,AF} = N#neuron.lt,
	SpeCon = get_SpeCon(dx_id,DX_Id),
	New_PF = technome_constructor:generate_NeuronPF(SpeCon#constraint.neural_pfs,[Old_PF]),
	mnesia:write(N#neuron{lt = {New_PF,AF},generation=Generation}),
	update_EvoHist(DX_Id,change_PlasticityFunction,void,N_Id,void,void,New_PF).
	
%--------------------------------Change ActivationFunction--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
change_ActivationFunction(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Generation = Cx#cortex.generation,
	Cx_CIds = Cx#cortex.cids,
	Link_Form = Cx#cortex.link_form,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	{Adapter,Old_AF} = N#neuron.lt,
	SpeCon = get_SpeCon(dx_id,DX_Id),
	New_AF = technome_constructor:generate_NeuronAF(SpeCon#constraint.neural_afs,[Old_AF]),
	mnesia:write(N#neuron{lt = {Adapter,New_AF},generation=Generation}),
	update_EvoHist(DX_Id,change_ActivationFunction,void,N_Id,void,void,New_AF).

%--------------------------------Reset DWP--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
reset_DWP(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Generation = Cx#cortex.generation,
	Cx_CIds = Cx#cortex.cids,
	Link_Form = Cx#cortex.link_form,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	N_I = N#neuron.i,
	New_DWP = technome_constructor:create_NWP(N_I,[]),
	mnesia:write(N#neuron{dwp = New_DWP,generation=Generation}),
	update_EvoHist(DX_Id,reset_DWP,void,N_Id,void,void,New_DWP).

%--------------------------------Reset Neuron--------------------------------
%%%Notes: 
%%%Function: 
%%%Interface:Input: Output: 
%%%MsgComunication:
reset_Neuron(DX_Id,Cx_Id)->
	[Cx] = mnesia:read({cortex,Cx_Id}),
	Generation = Cx#cortex.generation,
	Cx_CIds = Cx#cortex.cids,
	Link_Form = Cx#cortex.link_form,
	N_Id = lists:nth(random:uniform(length(Cx_CIds)),Cx_CIds),
	[N] = mnesia:read({neuron,N_Id}),
	{Old_PF,Old_AF} = N#neuron.lt,
	SpeCon = get_SpeCon(dx_id,DX_Id),
	New_PF = technome_constructor:generate_NeuronPF(SpeCon#constraint.neural_pfs,[Old_PF]),
	New_AF = technome_constructor:generate_NeuronAF(SpeCon#constraint.neural_afs,[Old_AF]),
	N_I = N#neuron.i,
	New_DWP = technome_constructor:create_NWP(N_I,[]),
	mnesia:write(N#neuron{lt = {New_PF,New_AF},dwp=New_DWP,generation=Generation}),
	update_EvoHist(DX_Id,reset_Neuron,void,N_Id,void,void,{New_PF,New_AF,New_DWP}).


%1. Take Neurons connected to from SC.
%2. Take the new elements connected to from the current neurons, 
%3. Continue this until, no new neurons are in the connected to list, or SC is reached.
%4. If subcore is not reached, declare this individual braindead, if SC is reached, remove any Neuron within the SC_CIds but not listed.
	verify_network(DX_Id)->
		[DX] = mnesia:read({dx,DX_Id}),
		test_NeuroLevelThroughput(DX_Id,DX#dx.cx_id),
		done.

		test_NeuroLevelThroughput(DX_Id,Cx_Id)->
			[Cx] = mnesia:read({cortex,Cx_Id}),
			Cx_CIds = Cx#cortex.cids,
			Cx_CT = Cx#cortex.ct,
			{_CT_Tags,NIdPs} = lists:unzip(Cx_CT),
			{CT_Ids,_Filters} = lists:unzip(lists:flatten(NIdPs)),
			case test_NeuroLevelThroughput(Cx_Id,CT_Ids,CT_Ids) of
				{true, TotUniqueIds} ->
					case Cx_CIds -- TotUniqueIds of
						[] ->
							done;
						IsolatedIds -> %This happens when a Neuron is only connected to himself through a recurrent link.
							remove_Neurons(DX_Id,Cx_Id,IsolatedIds,on)
					end;
				{false,_}->
					exit("******** NeuroLevelThroughput test failure")
			end.
			
			test_NeuroLevelThroughput(Cx_Id,[Id|CT],TotIdAcc)->

				UniqueO = case Id == Cx_Id of
					false ->
						[N] = mnesia:read({neuron,Id}),
						O_Ids = N#neuron.o,
						O_Ids--TotIdAcc;
					true ->
						[]
				end,
				test_NeuroLevelThroughput(Cx_Id,lists:append(CT,UniqueO),lists:append(UniqueO,TotIdAcc));
			test_NeuroLevelThroughput(Cx_Id,[],TotIdAcc)->
				{lists:member(Cx_Id,TotIdAcc), TotIdAcc}.

get_SpeCon(specie_id,Specie_Id)->
	SpeCon=case mnesia:dirty_read({specie,Specie_Id}) of
		[S]->
			S#specie.constraint;
		[] ->%Default, no specie exists, dx_test
			#constraint{}
	end,
%	io:format("Specie_Id:~p SpeCon:~p~n",[Specie_Id,SpeCon]),
	SpeCon;
get_SpeCon(dx_id,DX_Id)->
	io:format("here1~n"),
	[DX] = mnesia:dirty_read({dx,DX_Id}),
	Specie_Id = DX#dx.specie_id,
	get_SpeCon(specie_id,Specie_Id);
get_SpeCon(cx_id,Cx_Id)->
	io:format("here2~n"),
	[Cx] = mnesia:dirty_read({cx,Cx_Id}),
	DX_Id = Cx#cortex.su_id,
	get_SpeCon(dx_id,DX_Id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This source code and work is provided and developed by Gene I. Sher & DXNN Research Group WWW.DXNNResearch.COM
%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%

-module(agent_evo_strat).
-compile(export_all).
-include("records.hrl").

init()->
	#agent_evo_strat{
		strategies_mutation_prob = random:uniform(),
		mutation_operators = normalize([{MO,random:uniform()} ||{MO,_Prob} <-  modular_mutator:mos()]),
		tuning_annealing_flag = lists:nth(random:uniform(2),[true,false]),
		tuning_mutation_range = random:uniform()*2,
		annealing_parameter = random:uniform(),
		topological_mutation_prob = random:uniform(),
		topological_annealing_flag = lists:nth(random:uniform(2),[true,false]),
		neuron_perturbation_prob = random:uniform(),
		weight_perturbation_prob = random:uniform(),
		active_neuron_selection_type = lists:nth(random:uniform(8),[dynamic,active,current,all,dynamic_random,active_random,current_random,all_random]),
		active_neuron_selection_parameter = undefined}.
		
normalize(MOs)->
	L = [Prob|| {_MO,Prob} <- MOs],
	Sum=lists:sum(L),
	[{MO,Prob/Sum} || {MO,Prob} <- MOs].
	
mutate(AES)->
	SMP = AES#agent_evo_strat.strategies_mutation_prob,
	U_SMP = dynamic_perturbation(SMP,0.5,0,1),
	
	MOs = AES#agent_evo_strat.mutation_operators,
	U_MOs = normalize([{MO,dynamic_perturbation(Prob,1/math:sqrt(length(MOs)),0,1)} || {MO,Prob} <- MOs]),
	
	TuningAF = AES#agent_evo_strat.tuning_annealing_flag,
	U_TuningAF = case random:uniform() < SMP of
			true ->
				lists:nth(random:uniform(2),[true,false]);
			false ->
				TuningAF
		end,
	
	TMR = AES#agent_evo_strat.tuning_mutation_range,
	U_TMR = dynamic_perturbation(TMR,SMP,0,2),
	
	AP = AES#agent_evo_strat.annealing_parameter,
	U_AP = dynamic_perturbation(AP,SMP,0,1),
	
	TMP = AES#agent_evo_strat.topological_mutation_prob,
	U_TMP = dynamic_perturbation(TMP,SMP,0,1),
	
	TopologicalAF = AES#agent_evo_strat.topological_annealing_flag,
	U_TopologicalAF = case random:uniform() < SMP of
			true ->
				lists:nth(random:uniform(2),[true,false]);
			false ->
				TopologicalAF
		end,
	
	NPP = AES#agent_evo_strat.neuron_perturbation_prob,
	U_NPP = dynamic_perturbation(NPP,SMP,0,1),
	
	WPP = AES#agent_evo_strat.weight_perturbation_prob,
	U_WPP = dynamic_perturbation(WPP,SMP,0,1),
	
	ANST = AES#agent_evo_strat.active_neuron_selection_type,
	U_ANST = case random:uniform() < SMP of
		true ->
			lists:nth(random:uniform(8),[dynamic,active,current,all,dynamic_random,active_random,current_random,all_random]);
		false ->
			ANST
	end,
	
	ANSP = AES#agent_evo_strat.active_neuron_selection_parameter,
	U_ANSP = ANSP,
	
	AES#agent_evo_strat{
		strategies_mutation_prob = U_SMP,
		tuning_annealing_flag = U_TuningAF,
		tuning_mutation_range = U_TMR,
		annealing_parameter = U_AP,
		topological_mutation_prob = U_TMP,
		topological_annealing_flag = U_TopologicalAF,
		neuron_perturbation_prob = U_NPP,
		weight_perturbation_prob = U_WPP,
		active_neuron_selection_type = U_ANST,
		active_neuron_selection_parameter = U_ANSP}.
	
	dynamic_perturbation(Val,MP,Min,Max)->
		case random:uniform() < MP of
			true ->
				case random:uniform() < 0.5 of
					true ->
						Val - (Val-Min)*random:uniform()*0.1;
					false ->
						Val + (Max-Val)*random:uniform()*0.1
				end;
			false ->
				Val
		end.

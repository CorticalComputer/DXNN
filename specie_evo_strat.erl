%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(specie_evo_strat).
-compile(export_all).
-include("records.hrl").

%-record(specie_evo_strat,{
%	strategies_mutation_prob = 0,%Probability of mutating an evolutionary strategies parameter: 0-100%
%	evolution_type = memetic,%memetic|genetic
%	selection_type = competition,%competition|top3
%	selection_threshold = 0.5,%Top 10% - 90% survival.
%	diversity_factor = 0,%Weight of the fingerprint difference, importance for genetic diversity to be part of the fitness function.
%	revenent_prob = 0.1 %probability that a deadpool NN will reavaluate, 10% - 90%
%}).

init()->
	#specie_evo_strat{
		strategies_mutation_prob = random:uniform(),
		evolution_type = lists:nth(random:uniform(2),[memetic,genetic]),
		selection_type = lists:nth(random:uniform(2),[competition,top3]),
		selection_threshold = 0.1 + random:uniform(80)/100,
		diversity_importance = random:uniform(),
		revenent_prob = random:uniform()}.
		
mutate(PES)->
	SMP = PES#specie_evo_strat.strategies_mutation_prob,
	U_SMP = dynamic_perturbation(SMP,0.5,0,1),
	
	ET = PES#specie_evo_strat.evolution_type,
	U_ET = case random:uniform() < SMP of
		true ->
			lists:nth(random:uniform(2),[memetic,genetic]);
		false ->
			ET
	end,
	
	ST = PES#specie_evo_strat.selection_type,
	U_ST = case random:uniform() < SMP of
		true ->
			lists:nth(random:uniform(2),[competition,top3]);
		false ->
			ST
	end,
	
	SThreshold = PES#specie_evo_strat.selection_threshold,
	U_SThreshold = dynamic_perturbation(SThreshold,SMP,0.1,0.9),
	
	DI = PES#specie_evo_strat.diversity_importance,
	U_DI = dynamic_perturbation(DI,SMP,0,1),
	
	RP = PES#specie_evo_strat.revenent_prob,
	U_RP = dynamic_perturbation(RP,SMP,0,1),
	
	PES#specie_evo_strat{
		strategies_mutation_prob=U_SMP,
		evolution_type=U_ET,
		selection_type=U_ST,
		selection_threshold=U_SThreshold,
		diversity_importance=U_DI,
		revenent_prob=U_RP}.
		
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

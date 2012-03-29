%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(logger).
-compile(export_all).
-include("forex_db.hrl").
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Logger Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SURVIVAL_TYPE,competition). %competition,pl_competition,simple_ranking,speciated_ranking,forced_speciation
%competition: The survival and replication is purely based on the fitness, which determines the energy left for node creation. If it's above the creator's total nodes,
%	a new mutated copy can be made, if it is less than 50%, then the entity dies, if it is >50% but <100%, 50% of the energy requirenment for creation is subtracted.
%	The cost of each Neuron is based on the average fitness from the first generation from the population of all the single Neuron individuals.
%pl_competition: Same as competition but the cost for each neuron is based on the population limit and the average number of Neurons, weighted with the fitness.
%simple_ranking: The fitnesses are ranked from greatest to smallest, top 3 are chosen, and then based on their ration of fitness to to fitness  creation is allowed.
%speciated_ranking: First we choose 3 of top, topologically different individuals, and then produce a specific number of mutants for each individual.
%forced_speciation: We diffirintiate everyone into their own topological group. Then within each group the individuals are ranked, and some number of top individuals are 
%	allowed to create mutant coppies, if the specie has not produced any improvements in some number of generations, it is removed.
-define(INIT_POPULATION_SIZE,30).
-define(INIT_POPULATION_ID,test).
-define(INIT_ARCHITECTURE_TYPE,modular).
-define(INIT_LINK_FORM,recursive).
-define(INIT_PROBLEM_TYPE,{pole2_balancing,3}).
-define(OPMODE,gt).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start()->
	PId = spawn(logger,server,[void]),
	register(logger,PId).

stop()->
	logger ! {self(),stop}.

server(void)->
	receive 
		{_From, start_logger}->
			logger:server(high);
		{_From, start_logger,Log_Type}->
			logger:server(Log_Type);
		{_From, stop}->
			io:format("Logger is shutting down normally.~n");
		MSG ->
			io:format("MSG:~p~n",[MSG]),
			logger:server(void)
	end;
server(Log_Type)->
	receive 
		{log_info,Info}->
			display_info(Log_Type,Info),
			logger:server(Log_Type);
		{new_LT,New_LogType}->
			logger:server(New_LogType)
	end.

	display_info(Log_Type,Info)->
		case Log_Type of
			all ->
				io:format("***** Loging Type:~p *****~n",[Log_Type]),
				io:format("Logger Info:~p~n",[Info]);
			none ->
				done
		end.

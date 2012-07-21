%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(modular_constructor).
-compile(export_all).
-include("forex_db.hrl").
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modular_Constructor Parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
-define(CT_TYPES,[block]).%[single,block,all],
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%============================================================Technome Construction Functions============================================================
construct_DX(Specie_Id,DX_Id,SpecCon)->
%	io:format("~p ~p ~p ~p~n",[Specie_Id,DX_Id,LinkForm,Morphology]),
	{A,B,C} = now(),
	random:seed(A,B,C),
	Generation = 0,
	Cx_Id = construct_CortexTechnome(DX_Id,Generation,SpecCon),
	DX = #dx{
		id = DX_Id,
		cx_id = Cx_Id,
		specie_id = Specie_Id,
		morphology = SpecCon#constraint.morphology,
		generation = Generation,
		evo_hist = [],
		evo_strat = agent_evo_strat:init()
	},
	mnesia:write(DX),
	technome_constructor:update_Stats(DX_Id),
	%io:format("New Technome with DX_Id:~p ~nDX_STATS: ~p~n",[DX_Id,mnesia:read(dx,DX_Id)]),
	done.

%-record(core,{id,ivl,i,ovl,o,cfvl,cf,ctvl,ct,ro,type,pattern,cids,su_id,link_form,generation}). %%%id = {{LayerIndex,NumId},subcore}
construct_CortexTechnome(DX_Id,Generation,SpecCon)->
	Cx_Id = {{origin,technome_constructor:generate_UniqueId()},cortex},
	SC_Types = SpecCon#constraint.sc_types,
	Morphology = SpecCon#constraint.morphology,
	Type = lists:nth(random:uniform(length(SC_Types)),SC_Types),
	io:format("SC_Types:~p Type:~p~n",[SC_Types,Type]),
	case Type of
		hypercube->
%			I_VL = lists:sum([Sensor#sensor.tot_vl || Sensor <- Sensors]),
%			O_VL = lists:sum([Actuator#actuator.tot_vl || Actuator <- Actuators]),
			Substrate_Link_Form = lists:nth(random:uniform(length(SpecCon#constraint.sc_hypercube_linkform)),SpecCon#constraint.sc_hypercube_linkform),
			Neural_Link_Form = SpecCon#constraint.sc_neural_linkform,
			SC_Plasticity = SpecCon#constraint.sc_hypercube_plasticity,
			[Plasticity]=SC_Plasticity,
			Sensors = morphology:get_Sensors(Morphology),
			Actuators = morphology:get_InitActuators(Morphology),
			Density = 5,
			Dimensions = calculate_OptimalHypercubeDimension(Morphology),
			Depth = 2,
			Densities = [Depth,1|lists:duplicate(Dimensions-2,Density)], %[X,Y,Z,T...]
			
			SubstrateCT = morphology:get_InitHCT(Dimensions,Plasticity),
			SubstrateCF = morphology:get_InitHCF(Dimensions,Plasticity),
			io:format("SubstrateCT:~p~n SubstrateCF:~p Densities:~p~n",[SubstrateCT,SubstrateCF,Densities]),
			CT_VL = lists:sum([SCT#sCT.tot_vl || SCT <- SubstrateCT]),
			CF_VL = lists:sum([SCF#sCF.tot_vl || SCF <- SubstrateCF]),
			
			CT_Type = lists:nth(random:uniform(length(?CT_TYPES)),?CT_TYPES),
			CT = case CT_Type of
				block ->
					[{SCT#sCT.tot_vl,{block,SCT#sCT.tot_vl}} || SCT <-SubstrateCT]
			end,
			{FL_IVLs,CT_Tags} = lists:unzip(CT),
			
			case (length(SubstrateCT) == 1) and (CF_VL == 1) of
				true ->
					N_Id = {{0,technome_constructor:generate_UniqueId()},neuron},
					FLIds = [N_Id],
					ClusteredLLIds = [[N_Id]],
					LLIds = lists:flatten(ClusteredLLIds),
					construct_FirstNeuroLayerTechnome(Cx_Id,Generation,[Cx_Id],FL_IVLs,[Cx_Id],FLIds,SpecCon);
				false ->
					FLIds = [{{-0.33,technome_constructor:generate_UniqueId()},neuron} || _ <- CT],%
					ClusteredLLIds=[[{{0.33,technome_constructor:generate_UniqueId()},neuron}|| _ <-lists:seq(1,SCF#sCF.tot_vl)] || SCF <- SubstrateCF],
					LLIds = lists:flatten(ClusteredLLIds),
					construct_FirstNeuroLayerTechnome(Cx_Id,Generation,LLIds,FL_IVLs,lists:duplicate(length(FLIds),Cx_Id),FLIds,SpecCon),
					construct_LastNeuroLayerTechnome(Cx_Id,Generation,FLIds,LLIds,SpecCon)%
			end,
			%io:format("C_CF:~p~nC_CT:~p~n",[{Actuators,ClusteredLLIds},{Sensors,FLIds,CT_Tags}]),
			C_CF = [{SCF,SCF_Ids} || {SCF,SCF_Ids}<-lists:zip(SubstrateCF,ClusteredLLIds)],
			C_CT = [{SCT,[{FLId,CT_Tag}]} || {SCT,FLId,CT_Tag}<-lists:zip3(SubstrateCT,FLIds,CT_Tags)];
		neural ->
			Substrate_Link_Form = void,
			Neural_Link_Form = SpecCon#constraint.sc_neural_linkform,
			SC_Plasticity = SpecCon#constraint.sc_neural_plasticity,
			[Plasticity]=SC_Plasticity,
			Sensors = morphology:get_InitSensors(Morphology),
			Actuators = morphology:get_InitActuators(Morphology),
			Dimensions = void,
			Densities = void,
			
			io:format("Morphology:~p Sensors:~p Actuators:~p LinkForm:~p SC_Plasticity:~p~n",[Morphology,Sensors,Actuators,Neural_Link_Form,SC_Plasticity]),
			I_VL = lists:sum([Sensor#sensor.tot_vl || Sensor <- Sensors]),
			O_VL = lists:sum([Actuator#actuator.tot_vl || Actuator <- Actuators]),
			CT_VL = I_VL,
			CF_VL = O_VL,
			
			CT_Type = lists:nth(random:uniform(length(?CT_TYPES)),?CT_TYPES),
			CT = case CT_Type of
				block ->
					[{S#sensor.tot_vl,{block,S#sensor.tot_vl}} || S <-Sensors]
			end,
			{FL_IVLs,CT_Tags} = lists:unzip(CT),
			
			case (length(Sensors) == 1) and (O_VL == 1) of
				true ->
					N_Id = {{0,technome_constructor:generate_UniqueId()},neuron},
					FLIds = [N_Id],
					ClusteredLLIds = [[N_Id]],
					LLIds = lists:flatten(ClusteredLLIds),
					construct_FirstNeuroLayerTechnome(Cx_Id,Generation,[Cx_Id],FL_IVLs,[Cx_Id],FLIds,SpecCon);
				false ->
					FLIds = [{{-0.33,technome_constructor:generate_UniqueId()},neuron} || _ <- CT],%
					ClusteredLLIds=[[{{0.33,technome_constructor:generate_UniqueId()},neuron}|| _ <- lists:seq(1,A#actuator.tot_vl)] || A <- Actuators],
					LLIds = lists:flatten(ClusteredLLIds),
					construct_FirstNeuroLayerTechnome(Cx_Id,Generation,LLIds,FL_IVLs,lists:duplicate(length(FLIds),Cx_Id),FLIds,SpecCon),
					construct_LastNeuroLayerTechnome(Cx_Id,Generation,FLIds,LLIds,SpecCon)%
			end,
			%io:format("C_CF:~p~nC_CT:~p~n",[{Actuators,ClusteredLLIds},{Sensors,FLIds,CT_Tags}]),
			C_CF = [{Actuator,Actuator_Ids} || {Actuator,Actuator_Ids}<-lists:zip(Actuators,ClusteredLLIds)],
			C_CT = [{Sensor,[{FLId,CT_Tag}]} || {Sensor,FLId,CT_Tag}<-lists:zip3(Sensors,FLIds,CT_Tags)]
	end,

	%io:format("Inside modular_constructor:~n C_CT:~p C_CF:~p~n",[C_CT,C_CF]),
	case (FLIds == LLIds) of
		true ->
			Pattern = [{0,length(FLIds)}],
			C_CIds = FLIds;
		false ->
			Pattern = [{-0.33,length(FLIds)},{0.33,length(LLIds)}],
			C_CIds = lists:append(FLIds,LLIds)
	end,

	Cortex = #cortex{
		id = Cx_Id,
		sensors = Sensors,
		actuators = Actuators,
		cf = C_CF,
		ct = C_CT,
		type = Type,
		plasticity = Plasticity,
		pattern = Pattern,
		cids = C_CIds,
		su_id = DX_Id,
		link_form = Neural_Link_Form,
		substrate_link_form = Substrate_Link_Form,
		dimensions = Dimensions,
		densities = Densities,
		generation = Generation
	},
	mnesia:write(Cortex),
	Cx_Id.
	
	calculate_OptimalHypercubeDimension(Morphology)->
		Sensors=morphology:get_Sensors(Morphology),
		Actuators=morphology:get_Actuators(Morphology),
		S_Formats = [S#sensor.format || S<-Sensors],
		A_Formats = [A#actuator.format || A<-Actuators],
		extract_maxdim(S_Formats++A_Formats,[]) + 2.
		
		extract_maxdim([F|Formats],Acc)->
			DS=case F of
				{symetric,Dims}->
					length(Dims);
				no_geo ->
					1
			end,
			extract_maxdim(Formats,[DS|Acc]);
		extract_maxdim([],Acc)->
			lists:max(Acc).
	
construct_FirstNeuroLayerTechnome(SU_Id,Generation,O_Ids,[IVL|IVLs],[I_Id|I_Ids],[N_Id|FLIds],SpecCon)->
	N_TotIVL = IVL,
	N_I = [{I_Id,IVL}],
	N_TotOVL = 1,
	N_O = O_Ids,
	technome_constructor:construct_Neuron(SU_Id,Generation,N_Id,{N_TotIVL,N_I},{N_TotOVL,N_O},SpecCon),
	construct_FirstNeuroLayerTechnome(SU_Id,Generation,O_Ids,IVLs,I_Ids,FLIds,SpecCon);
construct_FirstNeuroLayerTechnome(_SU_Id,_Generation,_O_Ids,[],[],[],_SpecCon)->
	done.
	
construct_LastNeuroLayerTechnome(SU_Id,Generation,I_Ids,[N_Id|LLIds],SpecCon)->
	N_TotIVL = length(I_Ids),
	N_I = [{I_Id,1}|| I_Id <- I_Ids],
	N_TotOVL = 1,
	N_O = [SU_Id],
	technome_constructor:construct_Neuron(SU_Id,Generation,N_Id,{N_TotIVL,N_I},{N_TotOVL,N_O},SpecCon),
	construct_LastNeuroLayerTechnome(SU_Id,Generation,I_Ids,LLIds,SpecCon);
construct_LastNeuroLayerTechnome(_SU_Id,_Generation,_I_Ids,[],_SpecCon)->
	done.

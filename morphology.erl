%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(morphology).
-compile(export_all).
-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HYPERCUBE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CF:
%	neural:		[{Actuator1,[N_Id1...N_Idn]},{Actuator2,[N_Id1...N_Idn]}...]
%	hypercube:	[{GeoTag1,[N_Id1...]},{GeoTag2,[N_Id2...]}...] HyperTag:[{weight,1}...]
%CT:
%	neural:		[{Sensor1,[{N_Id1,FilterTag1},{N_Id2,FilterTag2}...]}...] FilterTag:{single,Index} | {block,VL}
%	hypercube:	[{GeoTag1,[{N_Id1,FilterTag1},{N_Id2,FilterTag2}...]}...] GeoTag:[{cartesian,VL}...], FilterTag:{single,Index} | {block,VL}
%-record(sCT,{name,tot_vl,parameters}).
%-record(sCF,{name,tot_vl,parameters}).
get_InitHCF(Dimensions,Plasticity,Type)->
	[lists:nth(1,get_HCF(Dimensions,Plasticity,Type))].
	
get_InitHCT(Dimensions,Plasticity)->%Dimensions*2 +3 due to 1 set of To coordinates and 1 set of From coordinates, plus 3 values: InputVal,OutputVal,CurrentWeight
	[lists:nth(1,get_HCT(Dimensions,Plasticity))].

get_HCF(Dimensions,Plasticity,SystemType)->
	case SystemType of
		hypercube ->
			HCF = case Plasticity of
				iterative ->
					[#sCF{name=delta_weight,tot_vl=1}]; %[{delta_weight,1}];
				abcn ->
					[#sCF{name=abcn,tot_vl=4}]; %[{abcn,4}];
				none ->
					[#sCF{name=weight,tot_vl=1}]; %[{weight,1}]
				modular_none ->
					[#sCF{name=weight_expression,tot_vl=2}] %[{weight_conexpr,2}]
			end;
		aart ->
			HCF = case Plasticity of
				none ->
					%[#sCF{name=template_update,tot_vl=1}]
					%[#sCF{name=distance_lp,tot_vl=2}]
					[#sCF{name=distance,tot_vl=1}]
			end
	end.		
	
get_HCT(Dimensions,Plasticity)->
	io:format("Dimensions:~p, Plasticity:~p~n",[Dimensions,Plasticity]),
	HCT = if
		(Plasticity == iterative) ->
			Std=[
				#sCT{name=cartesian,tot_vl=Dimensions*2+3},%{cartesian,Dimensions*2+3},
				#sCT{name=centripital_distances,tot_vl=2+3},%{centripital_distances,2+3},
				#sCT{name=cartesian_distance,tot_vl=1+3},%{cartesian_distance,1+3},
				#sCT{name=cartesian_CoordDiffs,tot_vl=Dimensions+3},%{cartesian_CoordDiffs,Dimensions+3}
				#sCT{name=cartesian_GaussedCoordDiffs,tot_vl=Dimensions+3},%{cartesian_GaussedCoordDiffs,Dimensions+3}
				#sCT{name=iow,tot_vl=3}%{iow,3}
			],
			Adt=case Dimensions of
				2 ->
					[#sCT{name=polar,tot_vl=Dimensions*2+3}];%[{polar,Dimensions*2+3}];
				3 ->
					[#sCT{name=spherical,tot_vl=Dimensions*2+3}];%[{spherical,Dimensions*2+3}]
				_ -> 
					[]
			end,
			lists:append(Std,Adt);
		(Plasticity == none) or (Plasticity == abcn) or (Plasticity == modular_none)->
			Std=[
				%#sCT{name=cartesian_distance,tot_vl=1},%{cartesian_distance,1},
				%#sCT{name=cartesian_CoordDiffs,tot_vl=Dimensions},%{cartesian_CoordDiffs,Dimensions+3}
				#sCT{name=cartesian,tot_vl=Dimensions*2},%{cartesian,Dimensions*2},
				#sCT{name=centripital_distances,tot_vl=2},%{centripital_distances,2},
				#sCT{name=cartesian_distance,tot_vl=1},%{cartesian_distance,1},
				#sCT{name=cartesian_CoordDiffs,tot_vl=Dimensions},%{cartesian_CoordDiffs,Dimensions+3}
				#sCT{name=cartesian_GaussedCoordDiffs,tot_vl=Dimensions}%{cartesian_GaussedCoordDiffs,Dimensions+3}
			],
			Adt=case Dimensions of
				2 ->
					[#sCT{name=polar,tot_vl=Dimensions*2}];%[{polar,Dimensions*2}];
				3 ->
					[#sCT{name=spherical,tot_vl=Dimensions*2}];%[{spherical,Dimensions*2}]
				_ -> 
					[]
			end,
			lists:append(Std,Adt)
	end,
	HCT.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NEURAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GLOBAL Sensors/Actuators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Init Actuators/Sensors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_RandomInitSensors(Morphology)->
	Sensors = morphology:Morphology(sensors),
	[lists:nth(random:uniform(length(Sensors)),Sensors)].
	%[lists:nth(1,Sensors)].
	
get_RandomInitActyatirs(Morphology)->
	Actuators = morphology:Morphology(actuators),
	[lists:nth(random:uniform(length(Actuators)),Actuators)].
	%[lists:nth(1,Sensors)].

get_InitActuators(Morphology)->
	[lists:nth(1,morphology:Morphology(actuators))].
	
get_InitSensors(Morphology)->
	Sensors = morphology:Morphology(sensors),
	%[lists:nth(random:uniform(length(CT)),CT)].
	[lists:nth(1,Sensors)].

get_Actuators(Morphology)->
	morphology:Morphology(actuators).
	
get_Sensors(Morphology)->
	morphology:Morphology(sensors).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% XOR_MIMIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xor_mimic(actuators)->
	[
		#actuator{name=xor_output,id=test,format=no_geo,tot_vl=1,parameters=[3]}
	];
xor_mimic(sensors)->
	[
		#sensor{name=xor_input,id=test,format=no_geo,tot_vl=2,parameters=[2]}
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% POLE2_BALANCING3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
pole2_balancing3(actuators)->
	[
		#actuator{name=pole2_balancing,id=dp_id3,format=no_geo,tot_vl=1,parameters=[3]}
	];
pole2_balancing3(sensors)->
	[
		#sensor{name=pole2_balancing,id=dp_id3,format=no_geo,tot_vl=3,parameters=[3]}
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% POLE2_BALANCING6 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
pole2_balancing6(actuators)->
	[
		#actuator{name=pole2_balancing,id=dp_id6,format=no_geo,tot_vl=1,parameters=[6]}
	];
pole2_balancing6(sensors)->
	[
		#sensor{name=pole2_balancing,id=dp_id6,format=no_geo,tot_vl=6,parameters=[6]}
	].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% vowel_recognition %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vowel_recognition(actuators)->
	[
		#actuator{name=vowel_recognition,id=test,format=no_geo,tot_vl=11,parameters=[11]}
	];
vowel_recognition(sensors)->
	[
		#sensor{name=vowel_recognition,id=test,format=no_geo,tot_vl=10,parameters=[10]}
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% mines_vs_rocks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mines_vs_rocks(actuators)->
	[
		#actuator{name=mines_vs_rocks,id=test,format=no_geo,tot_vl=2,parameters=[2]}
	];
mines_vs_rocks(sensors)->
	[
		#sensor{name=mines_vs_rocks,id=test,format=no_geo,tot_vl=60,parameters=[60]}
	].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FOREX_TRADER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
forex_trader(actuators)->
	[
		#actuator{name=fx_Trade,id=fx_id,format=no_geo,tot_vl=1,parameters=[]}
	];
forex_trader(sensors)->
	LinearSensors=[#sensor{name=fx_ListSensor,id=fx_id,format=no_geo,tot_vl=HRes,parameters=[HRes,close]} || HRes<-[2]],
	GraphSensors = [#sensor{name=fx_GraphSensor,id=fx_id,format={symetric,[HRes,VRes]},tot_vl=HRes*VRes,parameters=[HRes,VRes]} || HRes <-[100], VRes<-[20]],
	InternalSensors = [#sensor{name=fx_Internals,id=fx_id,format=no_geo,tot_vl=3,parameters=[3]}],%[Long|Short|Void,Value]
	GraphSensors.%++InternalSensors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Flatlander %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%{2,{actuator,move_2d,cell_id,[2]}},
%{2,{actuator,translate_2d,cell_id,[2]}}
%Movement2d = {2,{actuator,rotate_and_move,cell_id,[2]}}
flatlander(actuators)->
	prey(actuators);
flatlander(sensors)->
	prey(sensors).

%-record(sensor,{name,id,format,tot_vl,parameters,objects=[],vis=[]}).
%-record(actuator,{name,id,format,tot_vl,parameters,objects=[],vis=[]}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Prey %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
prey(actuators)->
	Movement = [#actuator{name=two_wheels,id=cell_id,format=no_geo,tot_vl=2,parameters=[2]}],
	Cloning = [#actuator{name=create_offspring,id=cell_id,format=no_geo,tot_vl=1,parameters=[1]}],
	Weapons = [#actuator{name=spear,id=cell_id,format=no_geo,tot_vl=1,parameters=[1]}],
	Communications = [#actuator{name=speak,id=cell_id,format=no_geo,tot_vl=1,parameters=[1]}],
	%Movement = [{actuator,two_wheels,cell_id,no_geo,2,[2]}],
	%Cloning = [{actuator,create_offspring,cell_id,no_geo,1,[1]}],
	%Weapons = [{actuator,spear,cell_id,no_geo,1,[1]}],
	%Communications = [{actuator,speak,cell_id,no_geo,1,[1]}],
	Movement;%++Weapons;%++Communications;
prey(sensors)->
	Pi = math:pi(),
	Coned_Scanners = [#sensor{name=Name,id=cell_id,format=no_geo,tot_vl=Density,parameters=[Density]} || 
		{Name,Density} <- [{coned_plant_sensor,4}]],
	%Coned_Scanners =[{sensor,ScannerType,cell_id,no_geo,Density,[Density]}||Density<-[4],ScannerType<-[coned_plant_sensor]],
%	Coned_Scanners =[{sensor,ScannerType,cell_id,no_geo,Density,[Density]}||Density<-[4,10],ScannerType<-[coned_plant_sensor,coned_prey_sensor,coned_poison_sensor]],
%	Coned_Scanners =[{sensor,ScannerType,cell_id,no_geo,Density,[Density]}||Density<-[4],ScannerType<-[coned_energy_sensor]],
	Distance_Scanners = [#sensor{name=distance_scanner,id=cell_id,format=no_geo,tot_vl=Density,parameters=[Spread,Density,ROffset]} || 
		Spread<-[Pi/2],Density<-[5], ROffset<-[Pi*0/2]],
	%Distance_Scanners =[{sensor,distance_scaner,cell_id,no_geo,Density,[Spread,Density,ROffset]} || Spread <-[Pi/2], Density <-[10], ROffset<-[Pi*0/2]],
	Color_Scanners = [#sensor{name=color_scanner,id=cell_id,format=no_geo,tot_vl=Density,parameters=[Spread,Density,ROffset]} ||
		Spread <-[Pi/2], Density <-[5], ROffset<-[Pi*0/2]],
	%Color_Scanners =[{sensor,color_scaner,cell_id,no_geo,Density,[Spread,Density,ROffset]} || Spread <-[Pi/2], Density <-[10], ROffset<-[Pi*0/2]],
	Energy_Scanners = [#sensor{name=energy_scanner,id=cell_id,format=no_geo,tot_vl=Density,parameters=[Spread,Density,ROffset]} ||
		Spread <-[Pi/2], Density <-[5], ROffset<-[Pi*0/2]],
	%Energy_Scanners =[{sensor,energy_scanner,cell_id,no_geo,Density,[Spread,Density,ROffset]} || Spread <-[Pi/2], Density <-[10], ROffset<-[Pi*0/2]],
	Stat_Readers = [#sensor{name=Name,id=cell_id,format=no_geo,tot_vl=Density,parameters=Param} ||
		{Name,Density,Param} <- [{energy_reader,1,[]}]],
	%Stat_Readers = [{sensor,energy_reader,cell_id,no_geo,1,[]}],
	Commmunications = [#sensor{name=Name,id=cell_id,format=no_geo,tot_vl=Density,parameters=[Spread,Density,ROffset]} ||
		Name <- [sound_scanner], Spread <-[Pi/2], Density <-[10], ROffset<-[Pi*0/2]],
	Orders = [#sensor{name=Name,id=cell_id,format=no_geo,tot_vl=3,parameters=[3]} || Name <- [order]],
	Beacons = [#sensor{name=Name,id=cell_id,format=no_geo,tot_vl=4,parameters=[4]} || Name <- [guard]],
	%Communications = [{sensor,hear,cell_id,no_geo,Density,[Spread,Density,ROffset]} || Spread <-[Pi/2], Density <-[10], ROffset<-[Pi*0/2]],
	%lists:append([Coned_Scanners,Distance_Scanners,Color_Scanners,Stat_Readers]).
	Color_Scanners++Distance_Scanners++Orders++Beacons.%++Communications.%++Energy_Scanners++Stat_Readers.

epitopes(actuators)->
	SequenceLength=336,
	Parameters=[
		TableName=abc_pred16,
		StartIndex=561,
		EndIndex=1400,
		StartBenchIndex=1,
		EndBenchIndex=280,
		StartTestIndex=281,
		EndTestIndex=560
	],
	[#actuator{name=abc_pred,id=cell_id,format=no_geo,tot_vl=1,parameters=Parameters}];
epitopes(sensors)->
	SequenceLength=336,
	Parameters=[
		TableName=abc_pred16,
		StartIndex=561,
		EndIndex=1400,
		StartBenchIndex=1,
		EndBenchIndex=280,
		StartTestIndex=281,
		EndTestIndex=560
	],
	[#sensor{name=abc_pred,id=cell_id,format=no_geo,tot_vl=336,parameters=Parameters}].

epiwalker(actuators)->
	Parameters=[
	],
	[
		#actuator{name=epiwalker_MarkAART,id=primary,format=no_geo,tot_vl=1,parameters=Parameters}
		%#actuator{name=epiwalker_Move,id=primary,format=no_geo,tot_vl=1,parameters=Parameters}
	];
epiwalker(sensors)->
	ParameterList=[
	pcc
%	"PrimSeqDec",
%	"SideChainPolarity",
%	"SideChainCharge",
%	"Hydropathy",
%	"Polarity_Grantham_1974",
%	"Flexibility_KarplusSchulz_1985",
%	"Antigenicity_KolaskarTongaonkar_1990",
%	"Hydrophilicity_Parker_1986"
%	"Polarity_Ponnuswamy_1980"
	],
	SeqLen = 51,
	[#sensor{name=epiwalker_PrimSeqAART,format=no_geo,tot_vl=SeqLen,parameters=Parameters} || Parameters<-ParameterList].


aart_classifier(actuators)->
	Parameters=[
		glass
	],
	[#actuator{name=aart_classifier,id=cell_id,format=no_geo,tot_vl=1,parameters=Parameters}];
aart_classifier(sensors)->
	SeqLen=9,
	Parameters=[
		glass
	],
	[#sensor{name=aart_classifier,id=cell_id,format=no_geo,tot_vl=SeqLen,parameters=Parameters}].

create_format(Type,Precurser)->
	case Type of
		no_geo ->
			[Precurser];
		symetric ->
			[XRes,YRes] = Precurser,
			[XRes || _ <- lists:seq(YRes)];
		asymetric ->
			Precurser
	end.

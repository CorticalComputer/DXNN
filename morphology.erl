%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This source code and work is provided and developed by Gene I. Sher & DXNN Research Group WWW.DXNNResearch.COM
%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.
%
%The original release of this source code and the DXNN MK2 system was introduced and explained (architecture and the logic behind it) in my book: Handbook of Neuroevolution Through Erlang. Springer 2012, print ISBN: 978-1-4614-4462-6 ebook ISBN: 978-1-4614-4463-6. 
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%

-module(morphology).
-compile(export_all).
-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Init Standard Actuators/Sensors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_InitSensors(Morphology)->
	Sensors = morphology:Morphology(sensors),
	[lists:nth(1,Sensors)].

get_InitActuators(Morphology)->
	Actuators = morphology:Morphology(actuators),
	[lists:nth(1,Actuators)].

get_Sensors(Morphology)->
	morphology:Morphology(sensors).

get_Actuators(Morphology)->
	morphology:Morphology(actuators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Init Substrate_CPPs/Substrate_CEPs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_InitSubstrateCPPs(Dimensions,Plasticity)->
	Substrate_CPPs = get_SubstrateCPPs(Dimensions,Plasticity),
	[lists:nth(1,Substrate_CPPs)].

get_InitSubstrateCEPs(Dimensions,Plasticity)->
	Substrate_CEPs = get_SubstrateCEPs(Dimensions,Plasticity),
	[lists:nth(1,Substrate_CEPs)].

get_SubstrateCPPs(Dimensions,Plasticity)->
	io:format("Dimensions:~p, Plasticity:~p~n",[Dimensions,Plasticity]),
	if
		(Plasticity == iterative) or (Plasticity == abcn) ->
			Std=[
				#sensor{name=cartesian,type=substrate,vl=Dimensions*2+3},%{cartesian,Dimensions*2+3},
				#sensor{name=centripital_distances,type=substrate,vl=2+3},%{centripital_distances,2+3},
				#sensor{name=cartesian_distance,type=substrate,vl=1+3},%{cartesian_distance,1+3},
				#sensor{name=cartesian_CoordDiffs,type=substrate,vl=Dimensions+3},%{cartesian_CoordDiffs,Dimensions+3}
				#sensor{name=cartesian_GaussedCoordDiffs,type=substrate,vl=Dimensions+3},%{cartesian_GaussedCoordDiffs,Dimensions+3}
				#sensor{name=iow,type=substrate,vl=3}%{iow,3}
			],
			Adt=case Dimensions of
				2 ->
					[#sensor{name=polar,type=substrate,vl=Dimensions*2+3}];%[{polar,Dimensions*2+3}];
				3 ->
					[#sensor{name=spherical,type=substrate,vl=Dimensions*2+3}];%[{spherical,Dimensions*2+3}]
				_ -> 
					[]
			end,
			lists:append(Std,Adt);
		(Plasticity == none) or (Plasticity == modular_none)->
			Std=[
				#sensor{name=cartesian,type=substrate,vl=Dimensions*2},%{cartesian,Dimensions*2},
				#sensor{name=centripital_distances,type=substrate,vl=2},%{centripital_distances,2},
				#sensor{name=cartesian_distance,type=substrate,vl=1},%{cartesian_distance,1},
				#sensor{name=cartesian_CoordDiffs,type=substrate,vl=Dimensions},%{cartesian_CoordDiffs,Dimensions+3}
				#sensor{name=cartesian_GaussedCoordDiffs,type=substrate,vl=Dimensions}%{cartesian_GaussedCoordDiffs,Dimensions+3}
			],
			Adt=case Dimensions of
				2 ->
					[#sensor{name=polar,type=substrate,vl=Dimensions*2}];%[{polar,Dimensions*2}];
				3 ->
					[#sensor{name=spherical,type=substrate,vl=Dimensions*2}];%[{spherical,Dimensions*2}]
				_ -> 
					[]
			end,
			lists:append(Std,Adt)
	end.

get_SubstrateCEPs(Dimensions,Plasticity)->
	case Plasticity of
		iterative ->
			[#actuator{name=delta_weight,type=substrate,vl=1}]; %[{delta_weight,1}];
		abcn ->
			[#actuator{name=set_abcn,type=substrate,vl=5}]; %[{abcn,4}];
		none ->
			[#actuator{name=set_weight,type=substrate,vl=1}]; %[{weight,1}]
		modular_none ->
			[#actuator{name=weight_expression,type=substrate,vl=2}] %[{weight_conexpr,2}]
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MORPHOLOGIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xor_mimic(sensors)->
	[
		#sensor{name=xor_GetInput,type=standard,scape={private,xor_sim},vl=2}
	];
xor_mimic(actuators)->
	[
		#actuator{name=xor_SendOutput,type=standard,scape={private,xor_sim},vl=1}
	].
%*Every sensor and actuator uses some kind of function associated with it. A function that either polls the environment for sensory signals (in the case of a sensor) or acts upon the environment (in the case of an actuator). It is a function that we need to define and program before it is used, and the name of the function is the same as the name of the sensor or actuator it self. For example, the create_Sensor/1 has specified only the rng sensor, because that is the only sensor function we've finished developing. The rng function has its own vl specification, which will determine the number of weights that a neuron will need to allocate if it is to accept this sensor's output vector. The same principles apply to the create_Actuator function. Both, create_Sensor and create_Actuator function, given the name of the sensor or actuator, will return a record with all the specifications of that element, each with its own unique Id.

pole_balancing(sensors)->
	[
		#sensor{name=pb_GetInput,type=standard,scape={private,pb_sim},vl=3,parameters=[3]}
	];
pole_balancing(actuators)->
	[
		#actuator{name=pb_SendOutput,type=standard,scape={private,pb_sim},vl=1,parameters=[with_damping,1]}
	].
%Both, the pole balancing sensor and actuator, interface with the pole balancing simulation, a private scape. The type of problem the pole balancing simulation is used as depends on the sensor and acutuator parameters. The sensor's vl and parameters specify that the sensor will request the private scape for the cart's and pole's position and angular position respectively. The actuator's parameters specify that the scape should use without_damping type of fitness, and that since only a single pole is being used, that the termination condition associated with the second pole will be zeroed out, by being multiplied by the specified 0 value. When instead of using 0, we use 1, the private scape would use the angular position of the second pole as an element in calculating the fitness score of the interfacing agent, and using that angular position for the purpose of calculating whether termination condition has been reached by the problem.

discrete_tmaze(sensors)->
	[
		#sensor{name=dtm_GetInput,type=standard,scape={private,dtm_sim},vl=4,parameters=[all]}
	];
discrete_tmaze(actuators)->
	[
		#actuator{name=dtm_SendOutput,type=standard,scape={private,dtm_sim},vl=1,parameters=[]}
	].

predator(actuators)->
	prey(actuators);
predator(sensors)->
	prey(sensors).

prey(actuators)->
	Movement = [#actuator{name=differential_drive,type=standard,scape={public,flatland}, vl=2, parameters=[2]}],
	Movement;
prey(sensors)->
	Pi = math:pi(),
	Color_Scanners = [#sensor{name=color_scanner,type=standard,scape={public,flatland},vl=Density, parameters=[Spread,Density,ROffset]} || Spread <-[Pi/2], Density <-[5], ROffset<-[Pi*0/2]],
	Range_Scanners = [#sensor{name=range_scanner,type=standard,scape={public,flatland},vl=Density, parameters=[Spread,Density,ROffset]} || Spread <-[Pi/2], Density <-[5], ROffset<-[Pi*0/2]],
	Color_Scanners++Range_Scanners.
	
forex_trader(actuators)->
	[
		#actuator{name=fx_Trade,type=standard,scape={private,fx_sim},format=no_geo,vl=1,parameters=[]}
	];
forex_trader(sensors)->
	PLI_Sensors=[#sensor{name=fx_PLI,type=standard,scape={private,fx_sim},format=no_geo,vl=HRes,parameters=[HRes,close]} || HRes<-[10]],
	PCI_Sensors = [#sensor{name=fx_PCI,type=standard,scape={private,fx_sim},format={symmetric,[HRes,VRes]},vl=HRes*VRes,parameters=[HRes,VRes]} || HRes <-[10], VRes<-[10]],
	InternalSensors = [#sensor{name=fx_Internals,type=standard,scape={private,fx_sim},format=no_geo,vl=3,parameters=[3]}],%[Long|Short|Void],Value
	PCI_Sensors.%++InternalSensors.

epitopes(actuators)->
	TableName=abc_pred16,
	SequenceLength=336,
	StartIndex=1121,
	EndIndex=560,
	StartBenchIndex=561,
	EndBenchIndex=840,
	[#actuator{name=abc_pred,type=standard,scape={private,epitopes},format=no_geo,vl=1,parameters=[TableName,StartIndex,EndIndex,StartBenchIndex,EndBenchIndex]}];
epitopes(sensors)->
	TableName=abc_pred16,
	SequenceLength=336,
	StartIndex=1121,
	EndIndex=560,
	StartBenchIndex=561,
	EndBenchIndex=840,
	[#sensor{name=abc_pred,type=standard,scape={private,epitopes},format=no_geo,vl=SequenceLength,parameters=[TableName,StartIndex,EndIndex,StartBenchIndex,EndBenchIndex]}].


generate_id() ->
	{MegaSeconds,Seconds,MicroSeconds} = now(), 
	1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).

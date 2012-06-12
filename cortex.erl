%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This source code and work is provided and developed by Gene I. Sher & DXNN Research Group WWW.DXNNResearch.COM
%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%

-module(cortex).
-compile(export_all).
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Cortex Parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-define(MAX_ATTEMPTS,50).
-define(LIVES,1).
-define(MIN_PIMPROVEMENT,0.0000).
-define(SAT_LIMIT,math:pi()).
-record(state,{type,plasticity,morphology,specie_id,sensors,actuators,cf,ct,complexity,op_mode,max_attempts,dimensions,densities}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen(ExoSelf,Node)->
	PId = spawn(Node,cortex,prep,[ExoSelf]),
	{ok, PId}.
	
prep(ExoSelf)->
	{A,B,C} = now(),
	random:seed(A,B,C),
	receive
		{ExoSelf,init,InitState}->
%			io:format("Cortex:Prep::~p~n",[InitState]),
			{ExoSelf,Id,Sensors,Actuators,CF,CT,Max_Attempts,Smoothness,OpMode,Type,Plasticity,Morphology,Specie_Id,TotNeurons,Dimensions,Densities,Link_Form}=InitState,
			put(link_form,Link_Form),
			State = #state{
				type=Type,
				plasticity=Plasticity,
				morphology=Morphology,
				specie_id=Specie_Id,
				sensors=Sensors,
				actuators=Actuators,
				cf=CF,
				ct=CT,
				complexity=TotNeurons,
				op_mode=OpMode,
				max_attempts=Max_Attempts,
				dimensions = Dimensions,
				densities = Densities
			},
			enter_scape(State),
			put(state,State),
			put(type,Type),
			put(morphology,Morphology),
			put(instance,1),
			put(fitness_list,[]),
			put(max_attempts,Max_Attempts),
			case OpMode of
				dx ->
					dx;
				tpt ->
					put(rounds,1000),
					tpt;
				cm ->
					committee ! {self(),my_pid,ExoSelf},
					cm;
				_ ->
					put(fitness,{-1000000,1}),
					put(opmode,OpMode),
					OpMode
			end,
%			io:format("CortexInit:~p~n",[{ExoSelf,Id,CF,CT,OpMode}]),
			case Type of
				neural ->
					[sense(ExoSelf,CT) || _<- lists:seq(1,Smoothness)],
					cortex:cortex(ExoSelf,Id,State,CT,CF,[],[],OpMode);
				hypercube ->
					%[sense(ExoSelf,CT) || _<- lists:seq(1,Smoothness)],
					self() ! {self(),tik},
					Substrate = init,
					cortex:cortex(ExoSelf,self(),Id,State,Sensors,Actuators,CT,CF,Densities,{Substrate,reset},OpMode)
			end
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NEURAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cortex(ExoSelf,Id,State,CT,[{Actuator,[CF_PId|N_Ids]}|CF],CFAcc,OAcc,OpMode)->
	receive
		{CF_PId,forward,Output}->
%			io:format("Cortex:~p CxCF:~p~n",[Id,{CF_PId,Output}]),
			cortex:cortex(ExoSelf,Id,State,CT,[{Actuator,N_Ids}|CF],[Output|CFAcc],OAcc,OpMode);
		{ExoSelf,reset_IProfile} ->
			io:format("Cortex is reseting IProfile:~n"),
			ExoSelf ! {self(),ready},
			cortex:cortex(ExoSelf,Id,State,CT,[{Actuator,[CF_PId|N_Ids]}|CF],CFAcc,OAcc,OpMode);
		{ExoSelf,revert_IProfile} ->
			%io:format("reverting:~n"),
			ExoSelf ! {self(),ready},
			cortex:cortex(ExoSelf,Id,State,CT,[{Actuator,[CF_PId|N_Ids]}|CF],CFAcc,OAcc,OpMode);
		{fitness,Fitness} ->
			cortex:cortex(ExoSelf,Id,State,CT,[{Actuator,[CF_PId|N_Ids]}|CF],CFAcc,OAcc,OpMode);
		{ExoSelf,terminate}->
			void
		after 20000 ->
			io:format("********ERROR: Neural_Cortex Crashed:~p~n",[{ExoSelf,Id,State,CT,[{Actuator,[CF_PId|N_Ids]}|CF],CFAcc,OAcc,OpMode}])
	end;
cortex(ExoSelf,Id,State,CT,[{Actuator,[]}|CF],CFAcc,OAcc,OpMode)->
	Output = case Actuator#actuator.tot_vl - length(CFAcc) of
		0 ->
			lists:reverse(lists:flatten(CFAcc));
		Val ->
			lists:append(lists:duplicate(Val,0),lists:reverse(lists:flatten(CFAcc)))
	end,
	cortex(ExoSelf,Id,State,CT,CF,[],[{Actuator,Output}|OAcc],OpMode);
cortex(ExoSelf,Id,State,CT,[],[],OAcc,OpMode)->
%	io:format("Cortex:~p~n",[CFAcc]),
	%OutputP = lists:reverse(CFAcc),
	case cortex:OpMode(ExoSelf,State#state.specie_id,OAcc,0,0) of
		end_training ->
			case State#state.morphology of
				forex_trader ->
					get(fx_pid) ! terminate;
				_ ->
					ok
			end,
			%leave_scape(),
			done;
		_ ->
%			io:format("Cortex:~p CxCT:~p~n",[Id,CT]),
			sense(ExoSelf,CT)
	end,
	cortex:cortex(ExoSelf,Id,State,CT,State#state.cf,[],[],OpMode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HYPERCUBE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cortex(ExoSelf,Self,Id,State,I,O,CT,CF,Densities,{Substrate,SMode},OpMode)->		
	receive
		{Self,tik}->
			{U_Substrate,U_SMode,OAcc} = reason(I,O,CT,CF,Densities,{Substrate,SMode},State#state.plasticity),
%			io:format("id:~p OAcc:~p~n",[self(),OAcc]),
			Formated_OAcc = format_OAcc(OAcc,O,[]), %not a list of lists, and it should be.
%			io:format("id:~p Formated_OAcc:~p OAcc:~p~n",[self(),Formated_OAcc,OAcc]),
			case cortex:OpMode(ExoSelf,State#state.specie_id,Formated_OAcc,0,0) of
				end_training ->
					%leave_scape(),
					cortex:cortex(ExoSelf,Self,Id,State,I,O,CT,CF,Densities,{U_Substrate,U_SMode},OpMode);
				reset_IProfile ->
					put(substrate,Substrate),
					Self ! {Self,tik},
					cortex:cortex(ExoSelf,Self,Id,State,I,O,CT,CF,Densities,{U_Substrate,reset},OpMode);
				revert_IProfile ->
					Self ! {Self,tik},
					cortex:cortex(ExoSelf,Self,Id,State,I,O,CT,CF,Densities,{U_Substrate,U_SMode},OpMode);
				_ ->
%					io:format("Cortex:~p CxCT:~p~n",[Id,CT]),
					Self ! {Self,tik},
					cortex:cortex(ExoSelf,Self,Id,State,I,O,CT,CF,Densities,{U_Substrate,U_SMode},OpMode)
			end; 
		{ExoSelf,reset_IProfile} ->
%			io:format("reseting:~n"),
			put(substrate,Substrate),
			ExoSelf ! {self(),ready},
			cortex:cortex(ExoSelf,Self,Id,State,I,O,CT,CF,Densities,{Substrate,reset},OpMode);
		{ExoSelf,revert_IProfile} ->
%			io:format("reverting:~n"),
			Old_Substrate = get(substrate),
			ExoSelf ! {self(),ready},
			cortex:cortex(ExoSelf,Self,Id,State,I,O,CT,CF,Densities,{Old_Substrate,reset},OpMode);
		{ExoSelf,terminate}->
%			io:format("Resulting substrate:~p~n",[Substrate]),
			void;
		Msg ->
			io:format("Unknown Msg:~p~n",[Msg]),
			cortex:cortex(ExoSelf,Self,Id,State,I,O,CT,CF,Densities,{Substrate,SMode},OpMode)
		after 20000 ->
			io:format("********ERROR: Hypercube_Cortex Crashed:~p~n",[{ExoSelf,Self,Id,State,I,O,CT,CF,Densities,{Substrate,SMode},OpMode}])
	end.

%Sensory_Pool: [Sensor1...] Sensor: {sensor,Name,Id,VL,Parameters}
%Actuator_Pool:[Actuator1...] Actuator: {actuator,Name,Id,VL,Parameters}
reason(Sensors,Actuators,CT,CF,Densities,{Substrate,SMode},Plasticity)->
	Input = [sensors:Name(VL,SensorId,Parameters) || {sensor,Name,SensorId,Format,VL,Parameters,_Obj,_Vis}<- Sensors],
%	io:format("Sensors:~p Actuators:~p Input:~p~n",[Sensors,Actuators,Input]),
%	Input = lists:reverse(IAcc),
	case SMode of
		reset ->%io:format("reset~n"),
			New_Substrate = create_substrate2(Sensors,Densities,Actuators),
%			io:format("IResolutions:~p OResolutions:~p New_Substrate:~p~n",[IResolutions,OResolutions,New_Substrate]),
%			io:format("Densities:~p~n New_Substrate:~p~n Input:~p~n CT:~p~n CF:~p~n",[Densities,New_Substrate,Input,CT,CF]),
			{Output,Populated_Substrate} = calculate_ResetOutput(Densities,New_Substrate,Input,CT,CF,Plasticity),
%			io:format("New_Substrate:~p~n Output:~p~n Populated_Substrate:~p~n",[New_Substrate,Output,Populated_Substrate]),
			U_SMode=case Plasticity of
				iterative ->
					iterative;
				none ->
					hold;
				abcn ->
					hold;
				modular_none ->
					hold
			end,
			{Populated_Substrate,U_SMode,Output};
		iterative ->%io:format("Iterative~n"),
			{Output,U_Substrate} = calculate_IterativeOutput(Densities,Substrate,Input,CT,CF),
%			io:format("Output:~p~n Densities:~p~n Substrate:~p~n U_Substrate:~p~n CT:~p~n CF:~p~n",[Output,Densities,Substrate,U_Substrate,CT,CF]),
			{U_Substrate,SMode,Output};
		hold ->%io:format("hold~n"),
			{Output,U_Substrate} = calculate_HoldOutput(Densities,Substrate,Input),
			%io:format("Output1:~p Output:~p~n",[Output,Output]),
			{U_Substrate,SMode,Output}
	end.

format_OAcc(OAcc,[Actuator|Actuators],Acc)->%io:format("Here?:~p~n",[{[Actuator|Actuators],OAcc}]),
	{Output,OAccRem}=lists:split(Actuator#actuator.tot_vl,OAcc),
	format_OAcc(OAccRem,Actuators,[{Actuator,Output}|Acc]);
format_OAcc([],[],Acc)->
	Acc.
%%==================================================================== Internal Functions
fanout([Pid|Pids],Msg)->
	Pid ! Msg,
	fanout(Pids,Msg);
fanout([],_Msg)->
	true.

flush_buffer()->
	receive 
		ANY -> %io:format("ANY:~p~n",[ANY]),
		flush_buffer()
	after 0 ->
		done
end.

%	no_geo
%	{symetric,[R1,R2...Rk],[Val1...Valn]} where n == R1*R2*...Dk and k = dimension
%	{asymetric,[[R1..Rp],[R1..Rt]],[Val1...Valn]} where lists:sum(lists:flatten([[R1...Rp],[R1..Rt]])) == n, and depth = Dimension.
%	coorded, every val comes with its own coord tuple: {Coord,Val}. The coord is a list, thus specifying the dimensionality.
test_IS(SubstrateDimension)->
	Sensors = [
		#sensor{format=no_geo,tot_vl=10},
		#sensor{format={symetric,lists:reverse([3,4])},tot_vl=[
		1,-1,-1,-1,
		1,-1,-1,-1,
		1,1,1,1]}
	],
	compose_ISubstrate(Sensors,SubstrateDimension).

test_OS(SubstrateDimension)->
	Actuators = [
		#actuator{format=no_geo,tot_vl=10},
		#actuator{format={symetric,lists:reverse([3,4])},tot_vl=[
		1,-1,-1,-1,
		1,-1,-1,-1,
		1,1,1,1]}
	],
	compose_OSubstrate(Actuators,SubstrateDimension,[w1,w2,w3]).


compose_ISubstrate(Sensors,SubstrateDimension)->
	compose_ISubstrate(Sensors,[],1,SubstrateDimension-2).
compose_ISubstrate([S|Sensors],Acc,Max_Dim,Required_Dim)->
	case S#sensor.format of
		no_geo ->
			Dim=1,
			CoordLists = create_CoordLists([S#sensor.tot_vl]),
			ISubstrate_Part=[{Coord,0,void}|| Coord<-CoordLists],
			{Dim,ISubstrate_Part};
		{symetric,Resolutions}->
			Dim = length(Resolutions),
			Signal_Length = cortex:mult(Resolutions),
			CoordLists = create_CoordLists(Resolutions),
			ISubstrate_Part=[{Coord,0,void}|| Coord<-CoordLists],
			{Dim,ISubstrate_Part};
%		{asymetric,Resolutions}->
%			Dim = find_depth(Resolutions),
%			ISubstrate_Part = void,
%			{Dim,ISubstrate_Part};
		{coorded,Dim,Resolutions,ISubstrate_Part} ->
			{Dim,ISubstrate_Part}
	end,
	U_Dim = case Max_Dim > Dim of
		true ->
			Max_Dim;
		false ->
			Dim
	end,
	compose_ISubstrate(Sensors,[ISubstrate_Part|Acc],U_Dim,Required_Dim);
compose_ISubstrate([],Acc,ISubstratePart_MaxDim,Required_Dim)->
	case Required_Dim >= ISubstratePart_MaxDim of
		true ->
			ISubstrate_Depth = length(Acc),
			ISubstrate_DepthCoords = build_CoordList(ISubstrate_Depth),
			adv_extrude(Acc,Required_Dim,lists:reverse(ISubstrate_DepthCoords),-1,[]);%Passed in inverted,reversed inside adv_extrude, same for depth coords.
		false ->
			exit("Error in adv_extrude, Required_Depth < ISubstratePart_MaxDepth~n")
	end.

	adv_extrude([ISubstrate_Part|ISubstrate],Required_Dim,[IDepthCoord|ISubstrate_DepthCoords],LeadCoord,Acc)->
		Extruded_ISP = [{[LeadCoord,IDepthCoord|lists:append(lists:duplicate(Required_Dim - length(Coord),0),Coord)],O,W} || {Coord,O,W}<-ISubstrate_Part],
		extrude(ISubstrate_Part,Required_Dim,IDepthCoord,[]),
		adv_extrude(ISubstrate,Required_Dim,ISubstrate_DepthCoords,LeadCoord,lists:append(Extruded_ISP,Acc));
	adv_extrude([],_Required_Dim,[],_LeadCoord,Acc)->
		Acc.
		
		extrude([{Coord,O,W}|ISubstrate_Part],Required_Dim,DepthCoord,Acc)->
			Dim_Dif = Required_Dim - length(Coord),
			U_Coord= [1,DepthCoord|lists:append(lists:duplicate(Dim_Dif,0),Coord)],
			extrude(ISubstrate_Part,Required_Dim,DepthCoord,[{U_Coord,O,W}|Acc]);
		extrude([],_Required_Dim,_DepthCoord,Acc)->
			Acc.

compose_OSubstrate(Actuators,SubstrateDimension,Weights)->
	compose_OSubstrate(Actuators,[],1,SubstrateDimension-2,Weights).
compose_OSubstrate([A|Actuators],Acc,Max_Dim,Required_Dim,Weights)->
	case A#actuator.format of
		no_geo ->%Dim=void,OSubstrate_Part=void,
			Dim=1,
			CoordLists = create_CoordLists([A#actuator.tot_vl]),
			OSubstrate_Part=[{Coord,0,Weights}|| Coord<-CoordLists],
			{Dim,OSubstrate_Part};
		{symetric,Resolutions}->%Dim=void,OSubstrate_Part=void,
			Dim = length(Resolutions),
			Signal_Length = cortex:mult(Resolutions),
			CoordLists = create_CoordLists(Resolutions),
			OSubstrate_Part=[{Coord,0,Weights}|| Coord<-CoordLists],
			{Dim,OSubstrate_Part};
%		{asymetric,Resolutions}->%Dim=void,OSubstrate_Part=void,
%			Dim = find_depth(Resolutions),
%			OSubstrate_Part = void,
%			{Dim,OSubstrate_Part};
		{coorded,Dim,Resolutions,Unadjusted_OSubstrate_Part} ->
			OSubstrate_Part=[{Coord,O,Weights}|| {Coord,O,_}<-Unadjusted_OSubstrate_Part],
			{Dim,OSubstrate_Part}
	end,
	U_Dim = case Max_Dim > Dim of
		true ->
			Max_Dim;
		false ->
			Dim
	end,
	compose_OSubstrate(Actuators,[OSubstrate_Part|Acc],U_Dim,Required_Dim,Weights);
compose_OSubstrate([],Acc,OSubstratePart_MaxDim,Required_Dim,_Weights)->
	case Required_Dim >= OSubstratePart_MaxDim of
		true ->%done;
			ISubstrate_Depth = length(Acc),
			ISubstrate_DepthCoords = build_CoordList(ISubstrate_Depth),
			adv_extrude(Acc,Required_Dim,lists:reverse(ISubstrate_DepthCoords),1,[]);%Passed in inverted,reversed inside adv_extrude, same for depth coord
		false ->
			exit("Error in adv_extrude, Required_Depth < OSubstratePart_MaxDepth~n")
	end.

	find_depth(Resolutions)->find_depth(Resolutions,0).
	find_depth(Resolutions,Acc)->
		case is_list(Resolutions) of
			true ->
				[_Head|Tail] = Resolutions,
				find_depth(Tail,Acc+1);
			false ->
				Acc
		end.

%Substrate encoding: X density = n, Y density = k, Z density = p, T density = l
%Weights = [W1,W2...WI],
%[[{[Z1,Y,X],o,[W1...Wn]}...{[Z1,Yn,Xk],o,[W1...Wn]}]...[{[Zs,Y,X],o,[W1...Wn]}...]],
		build_CoordList(Density)->
			case Density == 1 of
				true ->
					[0.0];
				false ->
					DensityDividers = Density - 1,
					Resolution = 2/DensityDividers,
					build_CoordList(Resolution,DensityDividers,1,[])
			end.

			extend(I,DI,D,Substrate)->
				void.
				
			mult(List)->
				mult(List,1).
			mult([Val|List],Acc)->
				mult(List,Val*Acc);
			mult([],Acc)->
				Acc.

test_CS2()->
	Sensors = [
		#sensor{format=no_geo,tot_vl=3},
		#sensor{format={symetric,lists:reverse([2,3])},tot_vl=6}
	],
	Actuators = [
		#actuator{format=no_geo,tot_vl=2},
		#actuator{format={symetric,lists:reverse([3,2])},tot_vl=6}
	],
	create_substrate2(Sensors,[2,3,2,2],Actuators).
	
create_substrate2(Sensors,Densities,Actuators)->
	[Depth|SubDensities] = Densities,
	Substrate_I = compose_ISubstrate(Sensors,length(Densities)),
	I_VL = length(Substrate_I),
%	io:format("I_VL:~p~n",[I_VL]),
	case get(link_form) of
		feedforward ->
			Weight = 0,
			H = mult(SubDensities),
			IWeights = lists:duplicate(I_VL,Weight),
			HWeights = lists:duplicate(H,Weight);
		%ff_fully_interconnected->%TODO: Variable number of weights for each layer.
		%	;
		fully_interconnected ->
			Output_Neurodes = tot_ONeurodes(Actuators,0),
			Weight = 0,
			Tot_HiddenNeurodes = mult([Depth-1|SubDensities]),
			Tot_Weights = Tot_HiddenNeurodes + I_VL + Output_Neurodes,
			IWeights = lists:duplicate(Tot_Weights,Weight),
			HWeights = lists:duplicate(Tot_Weights,Weight);
		jordan_recurrent ->
			Output_Neurodes = tot_ONeurodes(Actuators,0),
			Weight = 0,
			H = mult(SubDensities),
			IWeights = lists:duplicate(I_VL+Output_Neurodes,Weight),
			HWeights = lists:duplicate(H,Weight);
		neuronself_recurrent ->
			Weight = 0,
			H = mult(SubDensities),
			IWeights = lists:duplicate(I_VL+1,Weight),
			HWeights = lists:duplicate(H+1,Weight)
		%planeself_recurrent ->%TODO: Variable number of weights for each layer
		%	;
		%olb_recurrent ->%TODO: One Layer Back recurrency, variable number of weights for each layer.
		%	void
	end,	
	case Depth of
		1 ->
			Substrate_O=compose_OSubstrate(Actuators,length(Densities),IWeights),
			[Substrate_I,Substrate_O];
		2 ->
			Substrate_R = cs(SubDensities,IWeights),
			Substrate_O=compose_OSubstrate(Actuators,length(Densities),HWeights),
%			io:format("Substrate_I:~n~p~n Substrate_R:~n~p~n Substrate_O:~n~p~n",[Substrate_I,Substrate_R,Substrate_O]),
			[Substrate_I,extrude(0,Substrate_R),Substrate_O];
		_ ->
			Substrate_R = cs(SubDensities,IWeights),
			Substrate_H = cs(SubDensities,HWeights),
			Substrate_O=compose_OSubstrate(Actuators,length(Densities),HWeights),
			%io:format("OResolutions:~p Substrate_O:~p~n",[OResolutions,Substrate_O]),
			[_,RCoord|C1] = build_CoordList(Depth+1),
			[_|C2] = lists:reverse(C1),
			HCoords = lists:reverse(C2),
			%io:format("RCoord:~p HCoord:~p~n",[RCoord,HCoords]),
			ESubstrate_R = extrude(RCoord,Substrate_R),
			ESubstrates_H = [extrude(HCoord,Substrate_H) || HCoord<-HCoords],
			%io:format("ESubstrate_R:~p ESubstrates_H:~p~n",[ESubstrate_R,ESubstrates_H]),
			lists:append([[Substrate_I,ESubstrate_R],ESubstrates_H,[Substrate_O]])
	end.

tot_ONeurodes([A|Actuators],Acc)->
	Tot_ANeurodes=case A#actuator.format of
		no_geo ->%Dim=void,OSubstrate_Part=void,
			A#actuator.tot_vl;
		{symetric,Resolutions}->%Dim=void,OSubstrate_Part=void,
			cortex:mult(Resolutions);
		{coorded,Dim,Resolutions,Unadjusted_OSubstrate_Part} ->
			length(Unadjusted_OSubstrate_Part)
	end,
	tot_ONeurodes(Actuators,Tot_ANeurodes+Acc);
tot_ONeurodes([],Acc)->
	Acc.


%[{[D3,D2,D1],o,[W1,W2,W3...]}...]
	cs(Densities,Weights)->
		RDensities = lists:reverse(Densities),
		Substrate = create_CoordLists(RDensities,[]),
		attach(Substrate,0,Weights).
	
		create_CoordLists(Densities)->create_CoordLists(Densities,[]).	
		create_CoordLists([Density|RDensities],[])->
			CoordList = build_CoordList(Density),
			XtendedCoordList = [[Coord]||Coord <- CoordList],
			create_CoordLists(RDensities,XtendedCoordList);
		create_CoordLists([Density|RDensities],Acc)->
			CoordList = build_CoordList(Density),
			XtendedCoordList = [[Coord|Sub_Coord]||Coord <- CoordList,Sub_Coord <- Acc],
			create_CoordLists(RDensities,XtendedCoordList);
		create_CoordLists([],Acc)->
			Acc.
			
			build_CoordList(Resolution,0,Coord,Acc)->
				[-1|Acc];
			build_CoordList(Resolution,DensityDividers,Coord,Acc)->
				build_CoordList(Resolution,DensityDividers-1,Coord-Resolution,[Coord|Acc]).
		
attach(List,E1,E2)->
	attach(List,E1,E2,[]).
attach([Val|List],E1,E2,Acc)->
	attach(List,E1,E2,[{Val,E1,E2}|Acc]);
attach([],_E1,_E2,Acc)->
	lists:reverse(Acc).
	
extrude(NewDimension_Coord,Substrate)->
	extrude(NewDimension_Coord,Substrate,[]).
extrude(NewDimension_Coord,[{Coord,O,W}|Substrate],Acc)->
	%io:format("NewDimension_Coord:~p~n",[NewDimension_Coord]),
	extrude(NewDimension_Coord,Substrate,[{[NewDimension_Coord|Coord],O,W}|Acc]);
extrude(_Coord,[],Acc)->
	lists:reverse(Acc).
	
calculate_IterativeOutput(Densities,Substrate,Input,CT,CF)->
%	Input = [I|| {I_PId,I} <- InputP],
	[SHead|STail] = Substrate,
	Populated_SHead = populate_SHead2(SHead,lists:flatten(Input),[]),
	{Output,U_STail} = update_STail(Populated_SHead,STail,CT,CF),
	%{calculate_output(Populated_SHead,U_STail),[Populated_SHead|U_STail]}.
	{Output,[Populated_SHead|U_STail]}.
	
	update_STail(Populated_SHead,U_STail,CT,CF)->
		[CurHypercube|RemSubstrate] = U_STail,%TODO: UPdate to also support other than feed forward link_form.
		update_STail(Populated_SHead,CurHypercube,RemSubstrate,CT,CF,[],[]).
	
		update_STail(PrevHypercube,[{Coord,PrevO,PrevWeights}|CurHypercube],Substrate,CT,CF,Acc1,Acc2)->
%			io:format("PrevHypercube:~p~n Coord:~p~n CT:~p~n CF:~p~n",[PrevHypercube,Coord,CT,CF]),
			U_O=calculate_substrate_output(PrevHypercube,{Coord,PrevO,PrevWeights},0),
			U_Weights = get_weights(PrevHypercube,Coord,CT,CF,[],PrevWeights,U_O),
%			io:format("PrevWeights:~p~n U_Weights:~p~n",[PrevWeights,U_Weights]),
			update_STail(PrevHypercube,CurHypercube,Substrate,CT,CF,[{Coord,U_O,U_Weights}|Acc1],Acc2);
		update_STail(_PrevHypercube,[],[CurHypercube|Substrate],CT,CF,Acc1,Acc2)->
			PrevHypercube = lists:reverse(Acc1),
			update_STail(PrevHypercube,CurHypercube,Substrate,CT,CF,[],[PrevHypercube|Acc2]);
		update_STail(_PrevHypercube,[],[],CT,CF,Acc1,Acc2)->
			OutputHypercube = lists:reverse(Acc1),
			{[O||{_Coord,O,_Weights}<-OutputHypercube],lists:reverse([OutputHypercube|Acc2])}.

			get_weights([{I_Coord,I,_I_Weights}|PrevHypercube],Coord,CT,CF,Acc,[W|Weights],O)->
				advanced_fanout2(CT,I_Coord,Coord,[I,O,W]),
				DeltaWeight=receive
					{I_PId,forward,[Val]}->
						%Threshold = 0.33,
						%Processed_Weight = if 
						%	Weight > Threshold ->
						%		(functions:scale(Weight,1,Threshold)+1)/2;
						%	Weight < -Threshold ->
						%		(functions:scale(Weight,-Threshold,-1)-1)/2;
						%	true ->
						%		0
						%end
						Val;
					{I_PId,forward,[Weight,Expresion]}->
						Weight*Expresion
				end,
%				io:format("Weight:~p Processed_Weight:~p~n",[Weight,Processed_Weight]),
%				io:format("W:~p DW:~p~n",[W,DeltaWeight]),
				get_weights(PrevHypercube,Coord,CT,CF,[functions:sat(W+DeltaWeight,3.1415,-3.1415)|Acc],Weights,O);
			get_weights([],_Coord,_CT,_CF,Acc,[],_O)->
				lists:reverse(Acc).

				advanced_fanout2([{SCT,To_PIdPs}|CT],I_Coord,Coord,IOW)->
					Function = SCT#sCT.name,
					Vector = geometry:Function(I_Coord,Coord,IOW),
					[To_PId ! {self(),forward,Vector} || {To_PId,_FilterTag}<-To_PIdPs],
					advanced_fanout2(CT,I_Coord,Coord,IOW);
				advanced_fanout2([],_I_Coord,_Coord,_IOW)->
					done.

calculate_HoldOutput(Densities,Substrate,Input)->
%	Input = [I|| {I_PId,I} <- InputP],
	[SHead|Populated_STail] = Substrate,
%	io:format("calculate_HoldOutput:~n Densities:~p Substrate:~p Input:~p~n",[Densities,Substrate,Input]),
	Populated_SHead = populate_SHead2(SHead,lists:flatten(Input),[]),
	%Populated_Substrate = lists:append([Populated_SHead],Populated_THead),
	%io:format("Populated_SHead:~p~n InputP:~p~n",[Populated_SHead,InputP]),
	{Output,U_Substrate} = calculate_output(Populated_SHead,Populated_STail),
	{Output,[Populated_SHead|U_Substrate]}.

calculate_ResetOutput(Densities,Substrate,Input,CT,CF,Plasticity)->
%	Input = [I|| {I_PId,I} <- InputP],
	[ISubstrate|STail] = Substrate,
%	io:format("calculate_ResetOutput:~n Densities:~p~n {I_PId,Input}:~p~n CT:~p~n CF:~p~n SHead:~p~n",[Densities,Input,CT,CF,SHead]),
%	io:format("ISubstrate:~p lists:flatten(Input):~p~n",[ISubstrate,lists:flatten(Input)]),
	Populated_ISubstrate = populate_SHead2(ISubstrate,lists:flatten(Input),[]),
	case Plasticity of
		iterative ->
			Populated_Substrate = [Populated_ISubstrate|STail],
		%	io:format("Populated_SHead:~p~n Populated_THead:~p~n Populated_Substrate:~p~n",[Populated_SHead,STail,Populated_Substrate]),
			{Output,U_Substrate} = calculate_output(Populated_ISubstrate,STail),
			{Output,[Populated_ISubstrate|U_Substrate]};
		_ ->%none, modular_none
			Populated_STail = populate_STail2(Substrate,CT,CF),
			Populated_Substrate = lists:append([Populated_ISubstrate],Populated_STail),
			%io:format("Populated_ISubstrate:~p~n Populated_STail:~p~n Populated_Substrate:~p~n",[Populated_ISubstrate,Populated_STail,Populated_Substrate]),
			{Output,U_Substrate} =calculate_output(Populated_ISubstrate,Populated_STail),
			{Output,[Populated_ISubstrate|U_Substrate]}
	end.

	populate_SHead2([{Coord,PrevO,void}|Substrate],[I|Input],Acc)->
		populate_SHead2(Substrate,Input,[{Coord,I,void}|Acc]);
	populate_SHead2([],[],Acc)->
		lists:reverse(Acc).
		
	populate_STail2(Substrate,CT,CF)->
		case get(link_form) of
			feedforward ->
				[PrevHypercube,CurHypercube|RemSubstrate] = Substrate,
				populate_STail2(PrevHypercube,CurHypercube,RemSubstrate,CT,CF,[],[]);
			%ff_fully_interconnected->%TODO: Variable number of weights for each layer.
			%	;
			fully_interconnected ->
				[_InputHypercube,CurHypercube|RemSubstrate] = Substrate,
				populate_STail_fi(lists:flatten(Substrate),CurHypercube,RemSubstrate,CT,CF,[],[]);
			jordan_recurrent ->
				[IHypercube,CurHypercube|RemSubstrate] = Substrate,
				[OSubstrate|_]=lists:reverse(Substrate),
				populate_STail2(lists:flatten([IHypercube,OSubstrate]),CurHypercube,RemSubstrate,CT,CF,[],[]);
			neuronself_recurrent ->
				[PrevHypercube,CurHypercube|RemSubstrate] = Substrate,
				populate_STail_nsr(PrevHypercube,CurHypercube,RemSubstrate,CT,CF,[],[])
			%planeself_recurrent ->%TODO: Variable number of weights for each layer
			%	;
			%olb_recurrent ->%TODO: One Layer Back recurrency, variable number of weights for each layer.
			%	void
		end.
	
		populate_STail2(PrevHypercube,[{Coord,PrevO,PrevWeights}|CurHypercube],Substrate,CT,CF,Acc1,Acc2)->
%			io:format("PrevHypercube:~p~n Coord:~p~n CT:~p~n CF:~p~n",[PrevHypercube,Coord,CT,CF]),
			NewWeights = get_weights(PrevHypercube,Coord,CT,CF,[]),
			populate_STail2(PrevHypercube,CurHypercube,Substrate,CT,CF,[{Coord,PrevO,NewWeights}|Acc1],Acc2);
		populate_STail2(_PrevHypercube,[],[CurHypercube|Substrate],CT,CF,Acc1,Acc2)->
			PrevHypercube = lists:reverse(Acc1),
			populate_STail2(PrevHypercube,CurHypercube,Substrate,CT,CF,[],[PrevHypercube|Acc2]);
		populate_STail2(_PrevHypercube,[],[],CT,CF,Acc1,Acc2)->
			lists:reverse([lists:reverse(Acc1)|Acc2]).

		populate_STail_fi(FlatSubstrate,[{Coord,PrevO,_PrevWeights}|CurHypercube],Substrate,CT,CF,Acc1,Acc2)->
			NewWeights = get_weights(FlatSubstrate,Coord,CT,CF,[]),
			populate_STail_fi(FlatSubstrate,CurHypercube,Substrate,CT,CF,[{Coord,PrevO,NewWeights}|Acc1],Acc2);
		populate_STail_fi(FlatSubstrate,[],[CurHypercube|Substrate],CT,CF,Acc1,Acc2)->
			populate_STail_fi(FlatSubstrate,CurHypercube,Substrate,CT,CF,[],[lists:reverse(Acc1)|Acc2]);
		populate_STail_fi(_FlatSubstrate,[],[],CT,CF,Acc1,Acc2)->
			lists:reverse([lists:reverse(Acc1)|Acc2]).

		populate_STail_nsr(PrevHypercube,[{Coord,PrevO,_PrevWeights}|CurHypercube],Substrate,CT,CF,Acc1,Acc2)->
			NewWeights = get_weights([{Coord,PrevO,_PrevWeights}|PrevHypercube],Coord,CT,CF,[]),
			populate_STail_nsr(PrevHypercube,CurHypercube,Substrate,CT,CF,[{Coord,PrevO,NewWeights}|Acc1],Acc2);
		populate_STail_nsr(_PrevHypercube,[],[CurHypercube|Substrate],CT,CF,Acc1,Acc2)->
			PrevHypercube = lists:reverse(Acc1),
			populate_STail_nsr(PrevHypercube,CurHypercube,Substrate,CT,CF,[],[PrevHypercube|Acc2]);
		populate_STail_nsr(_PrevHypercube,[],[],CT,CF,Acc1,Acc2)->
			lists:reverse([lists:reverse(Acc1)|Acc2]).

			get_weights([{I_Coord,_O,_Weights}|PrevHypercube],Coord,CT,CF,Acc)->
				advanced_fanout2(CT,I_Coord,Coord),
				[{SCCF,N_Ids}] = CF,
				case SCCF#sCF.name of
					weight_expression ->
						[Weight,Expression]=neuro_FanIn(N_Ids,[]),
						%io:format("Weight:~p Expression:~p~n",[Weight,Expression]),
						case Expression > 0 of
							true ->
								get_weights(PrevHypercube,Coord,CT,CF,[Weight|Acc]);
							false ->
								get_weights(PrevHypercube,Coord,CT,CF,[0|Acc])
						end;
					_ ->
						receive
							{I_PId,forward,[Weight]}->
								Threshold = 0.33,
								Processed_Weight = if 
									Weight > Threshold ->
										(functions:scale(Weight,1,Threshold)+1)/2;
									Weight < -Threshold ->
										(functions:scale(Weight,-Threshold,-1)-1)/2;
									true ->
										0
								end		
						end,
%						io:format("Weight:~p Processed_Weight:~p~n",[Weight,Processed_Weight]),
						get_weights(PrevHypercube,Coord,CT,CF,[Processed_Weight|Acc])
				end;
			get_weights([],_Coord,_CT,_CF,Acc)->
				lists:reverse(Acc).
				
				neuro_FanIn(N_Ids)->
					neuro_FanIn(N_Ids,[]).
				neuro_FanIn([N_Id|N_Ids],Acc)->
					receive
						{I_PId,forward,[Val]}->
							neuro_FanIn(N_Ids,[Val|Acc])
					end;
				neuro_FanIn([],Acc)->
					lists:reverse(Acc).
				
				advanced_fanout2([{SCT,To_PIdPs}|CT],I_Coord,Coord)->
					Function=SCT#sCT.name,
					Vector = geometry:Function(I_Coord,Coord),
					[To_PId ! {self(),forward,Vector} || {To_PId,_FilterTag}<-To_PIdPs],
					advanced_fanout2(CT,I_Coord,Coord);
				advanced_fanout2([],_I_Coord,_Coord)->
					done.
						
		calculate_output(ISubstrate,Substrate)->
			case get(link_form) of
				feedforward ->
					calculate_output_ff(ISubstrate,Substrate,[]);
				%ff_fully_interconnected->%TODO: Variable number of weights for each layer.
				%	;
				fully_interconnected ->
					calculate_output_fi(ISubstrate,Substrate,[]);
				jordan_recurrent ->
					[OSubstrate|_] = lists:reverse(Substrate),
					calculate_output_ff(lists:flatten([ISubstrate|OSubstrate]),Substrate,[]);
				neuronself_recurrent ->
					calculate_output_nsr(ISubstrate,Substrate,[])
				%planeself_recurrent ->%TODO: Variable number of weights for each layer
				%	;
				%olb_recurrent ->%TODO: One Layer Back recurrency, variable number of weights for each layer.
				%	void
			end.
			
		calculate_output_ff(Prev_Hypercube,[Cur_Hypercube|Substrate],Acc)->
%			io:format("Prev_Hypercube:~p~n",[Prev_Hypercube]),
%			Val=length(Prev_Hypercube),
%			Normalizer = case Val == 0 of
%				true ->
%					1;
%				false ->
%					Val
%			end,
%			U_PH=[{Coord,O,Weights}  || {Coord,O,Weights} <- Prev_Hypercube],
			%U_PH=[{Coord,O+(random:uniform()-0.5)*O*0.5,Weights}  || {Coord,O,Weights} <- Prev_Hypercube],	
			Updated_CurHypercube = [{Coord,calculate_substrate_output(Prev_Hypercube,{Coord,Prev_O,Weights},0),Weights} || {Coord,Prev_O,Weights} <- Cur_Hypercube],
%			io:format("Updated_CurHypercube:~p~n",[Updated_CurHypercube]),
			calculate_output_ff(Updated_CurHypercube,Substrate,[Updated_CurHypercube|Acc]);
		calculate_output_ff(Prev_Hypercube,[],Acc)->
%			io:format("id:~p Prev_Hypercube:~p~n",[self(),Prev_Hypercube]),
			{[Output || {_Coord,Output,_Weights} <- Prev_Hypercube],lists:reverse(Acc)}.
			
			calculate_substrate_output([{_I_Coord,O,_I_Weights}|I_Hypercube],{Coord,Prev_O,[Weight|Weights]},Acc)->
				calculate_substrate_output(I_Hypercube,{Coord,Prev_O,Weights},O*Weight+Acc);
			calculate_substrate_output([],{Coord,Prev_O,[]},Acc)->
				functions:tanh(Acc).
			%calculate_substrate_output(A,B,C)->
			%	io:format("{A,B,C}:~p~n",[{A,B,C}]).
			
		calculate_output_fi(Input_Substrate,[Cur_Hypercube|Substrate],Acc)->
			Updated_CurHypercube = [{Coord,calculate_substrate_output(lists:flatten([Input_Substrate,Cur_Hypercube|Substrate]),{Coord,Prev_O,Weights},0),Weights} || {Coord,Prev_O,Weights} <- Cur_Hypercube],
			calculate_output_fi([Input_Substrate|Updated_CurHypercube],Substrate,[Updated_CurHypercube|Acc]);
		calculate_output_fi(Output_Hypercube,[],Acc)->
			{[Output || {_Coord,Output,_Weights} <- Output_Hypercube],lists:reverse(Acc)}.
			
		calculate_output_nsr(Prev_Hypercube,[Cur_Hypercube|Substrate],Acc)->
			Updated_CurHypercube = [{Coord,calculate_substrate_output([{Coord,Prev_O,Weights}|Prev_Hypercube],{Coord,Prev_O,Weights},0),Weights} || {Coord,Prev_O,Weights} <- Cur_Hypercube],
			calculate_output_nsr(Updated_CurHypercube,Substrate,[Updated_CurHypercube|Acc]);
		calculate_output_nsr(Prev_Hypercube,[],Acc)->
			{[Output || {_Coord,Output,_Weights} <- Prev_Hypercube],lists:reverse(Acc)}.
		
test(Weight)->
	Processed_Weight = if 
		Weight > 0.33 ->
			(functions:scale(Weight,1,0.33)+1)/2;
		Weight < -0.33 ->
			(functions:scale(Weight,-0.33,-1)-1)/2;
		true ->
			0
	end.
	
t(Val)->
	complex_scale(Val,-4,4,-2,2,1).
		
complex_scale(Val,Min,Max,DZMin,DZMax,Range)->
	if 
		Val > DZMax ->
			(functions:scale(Val,Max,DZMax)+1)*Range/2;
		Val < DZMin ->
			(functions:scale(Val,DZMin,Min)-1)*Range/2;
		true ->
			0
	end.






















%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
enter_scape(S)->
	Morphology=S#state.morphology,
	Specie_Id=S#state.specie_id,
	Actuators=S#state.actuators,
	Sensors=S#state.sensors,
	TotNeurons = S#state.complexity,
	case Morphology of
		flatlander ->
			{Scape_PId,Scape_Type} = gen_server:call(polis,{get_scape,flatland}),
			put(scape,Scape_PId),
%			io:format("Inside Cortex:: Scape_PId:~p~n",[Scape_PId]),
			done = gen_server:call(Scape_PId,{enter,Morphology,Specie_Id,Actuators,Sensors,TotNeurons});
		prey ->
			{Scape_PId,Scape_Type} = gen_server:call(polis,{get_scape,flatland}),
			put(scape,Scape_PId),
%			io:format("Inside Cortex:: Scape_PId:~p~n",[Scape_PId]),
			done = gen_server:call(Scape_PId,{enter,Morphology,Specie_Id,Actuators,Sensors,TotNeurons});
		_ ->
			done
	end.
	
leave_scape()->
	case get(morphology) of
		flatlander ->
			done = gen_server:call(get(scape),leave);
		prey -> 
			done = gen_server:call(get(scape),leave);
		_ ->
			done
	end.
	
reset_avatar()->
	case get(morphology) of
		flatlander ->
			gen_server:call(get(scape),reset);
		prey ->
			gen_server:call(get(scape),reset);
		_ ->
			done
	end.
	
reenter_scape()->
	S = get(state),
	Morphology=S#state.morphology,
	Specie_Id=S#state.specie_id,
	Actuators=S#state.actuators,
	Sensors=S#state.sensors,
	TotNeurons = S#state.complexity,
	case get(morphology) of
		flatlander ->
			done=gen_server:call(get(scape),{enter,Morphology,Specie_Id,Actuators,Sensors,TotNeurons});
		prey ->%io:format("Cortex reenter_scape(), organism:~p~n",[self()]),
			done=gen_server:call(get(scape),{enter,Morphology,Specie_Id,Actuators,Sensors,TotNeurons});
		_ ->
			done
	end.

sense(ExoSelf,[{S,N_PIdPs}|CT])->
	Name = S#sensor.name,
	VL = S#sensor.tot_vl,
	SensorId = S#sensor.id,
	Parameters=S#sensor.parameters,
	Input = sensors:Name(VL,SensorId,Parameters),
	advanced_fanout(N_PIdPs,Input),
	sense(ExoSelf,CT);
sense(_ExoSelf,[])->
	done.

	advanced_fanout([{N_PId,Tag}|N_PIdPs],Input)->
		%io:format("N_PId Tag:~p~n",[{N_PId,Tag}]),
		case Tag of
			{single,Index}->
				%io:format("Input:~p~n",[Input]),
				Val = lists:nth(Index,Input),
				%io:format("Val:~p Input:~p~n",[Val,Input]),
				N_PId ! {self(),forward,[Val]};
			{block,VL}->
				%{value,{PId,Input}} = lists:keysearch(PId,1,IAcc),
				N_PId ! {self(),forward,Input}
		end,
		advanced_fanout(N_PIdPs,Input);
	advanced_fanout([],_Input)->
		done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPMODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gt(ExoSelf,Specie_Id,[{A,Output}|OAcc],FitnessAcc,ProgressAcc)->
	%{actuator,Name,ActuatorId,Format,VL,Parameters} = Actuator,
	Name = A#actuator.name,
	ActuatorId = A#actuator.id,
	Parameters = A#actuator.parameters,
	{Progress,Fitness} = actuators:Name(ExoSelf,Output,ActuatorId,Parameters),
	case Fitness of
		goal_reached-> put(goal,reached);
		_ -> done
	end,
	gt(ExoSelf,Specie_Id,OAcc,FitnessAcc+Fitness,ProgressAcc+Progress);
gt(ExoSelf,Specie_Id,[],AffectFitness,AffectProgress)->
	FitnessType = void,
	FitnessAcc = case get(fitness_acc) of
		undefined ->
			0;
		FA ->
			FA
	end,
	U_FitnessAcc = FitnessAcc+AffectFitness,
	put(fitness_acc,U_FitnessAcc),
	MaxMissedAttempts = get(max_attempts),
	{HighestFitness,AttemptIndex} = get(fitness),
	Goal = get(goal),
%	io:format("Fitness:~p HighestFitness:~p,AtemptIndex:~p~n",[U_FitnessAcc,HighestFitness,AttemptIndex]),
	case AffectProgress >= 1 of
		false ->
			put(fitness,{HighestFitness,AttemptIndex});
		true ->
			done = gen_server:call(ExoSelf,memory_reset),
			put(fitness_acc,0),
			gen_server:cast(monitor,{self(),evaluations,Specie_Id,1}),
			FitnessList= get(fitness_list),
			U_FitnessList = [U_FitnessAcc|FitnessList],
			Instance = get(instance),
			case Instance >= ?LIVES of
				true->
					put(fitness_list,[]),
					put(instance,1),
					Avg_Fitness = functions:avg(U_FitnessList),
%					io:format("U_FitnessList:~p Avg_Fitness:~p~n",[U_FitnessList,Avg_Fitness]),
					case Avg_Fitness > (HighestFitness + abs(HighestFitness*?MIN_PIMPROVEMENT)) of
						true ->
							done = gen_server:call(ExoSelf,{weight_save}),
							done = gen_server:call(ExoSelf,{backup_request}),
							case Goal of
								undefined ->
									done;
								reached ->
									io:format("Id:~p, reached goal.~n",[self()]),
									gen_server:cast(monitor,{self(),goal_reached,get(morphology)})
							end,
							put(fitness,{Avg_Fitness,1}),
							reenter_scape(),
							done = gen_server:call(ExoSelf,{weight_mutate}),
							reset_IProfile;
						false ->
							done = gen_server:call(ExoSelf,{weight_revert}),
							%io:format("Pid:~p HighestFitness:~p Fitness:~p~n",[self(),HighestFitness,U_FitnessAcc]),
							case AttemptIndex >= MaxMissedAttempts of
								true ->
									gen_server:cast(ExoSelf,{self(),fitness,{FitnessType,HighestFitness,Goal}}),
									io:format("Pid:~p HighestFitness:~p AttemptIndex:~p~n",[self(),HighestFitness,AttemptIndex]),
									case Goal of
										undefined ->
											done;
										reached ->
											io:format("Id:~p, reached goal.~n",[self()]),
											gen_server:cast(monitor,{self(),goal_reached,get(morphology)})
									end,
									end_training;
								false ->
									%io:format("Pid:~p AttemptIndex:~p MaxMissedAttempts~p~n",[self(),AttemptIndex,MaxMissedAttempts]),
									put(fitness,{HighestFitness,AttemptIndex+1}),
									reenter_scape(),
									done = gen_server:call(ExoSelf,{weight_mutate}),
									reset_IProfile
							end
					end;
				false->
%					io:format("Instance:~p Cortex:~p~n",[Instance,self()]),
					put(fitness_list,U_FitnessList),
					put(instance,Instance+1),
					put(fitness,{HighestFitness,AttemptIndex+1}), %TODO We need to reset the memory, because it just lets it back in, hence the diff 
					reset_Memory,
					reenter_scape(),
					reset_IProfile	%We saw last time, where one of the values is different (the first?)
			end
	end.

benchmark(ExoSelf,Specie_Id,[{A,Output}|OAcc],FitnessAcc,ProgressAcc)->
	Name = A#actuator.name,
	ActuatorId = A#actuator.id,
	Parameters = A#actuator.parameters,
	{Progress,Fitness} = actuators:Name(ExoSelf,Output,ActuatorId,Parameters),
	case Fitness of
		goal_reached-> put(goal,reached);
		_ -> done
	end,
	benchmark(ExoSelf,Specie_Id,OAcc,FitnessAcc+Fitness,ProgressAcc+Progress);
benchmark(ExoSelf,Specie_Id,[],AffectFitness,AffectProgress)->
	FitnessType = void,
	FitnessAcc = case get(fitness_acc) of
		undefined ->
			0;
		FA ->
			FA
	end,
	U_FitnessAcc = FitnessAcc+AffectFitness,
	put(fitness_acc,U_FitnessAcc),
	case AffectProgress >= 1 of
		false ->
			void;
		true ->
			done = gen_server:call(ExoSelf,memory_reset),
			gen_server:cast(ExoSelf,{self(),benchmark_fitness,{FitnessType,U_FitnessAcc,get(goal)}}),
			io:format("Pid:~p Fitness:~p~n",[self(),U_FitnessAcc]),
			end_training
	end.
	
test(ExoSelf,Specie_Id,[{A,Output}|OAcc],FitnessAcc,ProgressAcc)->
	Name = A#actuator.name,
	ActuatorId = A#actuator.id,
	Parameters = A#actuator.parameters,
	{Progress,Fitness} = actuators:Name(ExoSelf,Output,ActuatorId,Parameters),
	case Fitness of
		goal_reached-> put(goal,reached);
		_ -> done
	end,
	test(ExoSelf,Specie_Id,OAcc,vector_add(Fitness,FitnessAcc,[]),ProgressAcc+Progress);
test(ExoSelf,Specie_Id,[],FitnessAcc,AffectProgress)->
	FitnessType = void,
	U_FitnessAcc = case get(fitness_acc) of
		undefined ->
			FitnessAcc;
		FA ->
			%io:format("lists:reverse(FitnessAcc),FA:~p~n",[{lists:reverse(FitnessAcc),FA}]),
			vector_add(FitnessAcc,FA,[])
	end,
	%U_FitnessAcc = FitnessAcc+AffectFitness,
	put(fitness_acc,U_FitnessAcc),
	case AffectProgress >= 1 of
		false ->
			void;
		true ->
			done = gen_server:call(ExoSelf,memory_reset),
			gen_server:cast(ExoSelf,{self(),test_fitness,{FitnessType,U_FitnessAcc,get(goal)}}),
			io:format("Pid:~p Fitness:~p~n",[self(),U_FitnessAcc]),
			end_training
	end.
	
	vector_add(LA,0,[])->
		LA;
	vector_add([A|LA],[B|LB],Acc)->
		vector_add(LA,LB,[A+B|Acc]);
	vector_add([],[],Acc)->
		lists:reverse(Acc).
			
championship(ExoSelf,Specie_Id,[{A,Output}|OAcc],FitnessAcc,ProgressAcc)->
	Name = A#actuator.name,
	ActuatorId = A#actuator.id,
	Parameters = A#actuator.parameters,
	{Progress,Fitness} = actuators:Name(ExoSelf,Output,ActuatorId,Parameters),
	case Fitness of
		goal_reached-> put(goal,reached);
		_ -> done
	end,
	championship(ExoSelf,Specie_Id,OAcc,FitnessAcc+Fitness,ProgressAcc+Progress);
championship(ExoSelf,Specie_Id,[],AffectFitness,AffectProgress)->
	FitnessType = void,
	FitnessAcc = case get(fitness_acc) of
		undefined ->
			0;
		FA ->
			FA
	end,
	U_FitnessAcc = FitnessAcc+AffectFitness,
	put(fitness_acc,U_FitnessAcc),
	MaxMissedAttempts = get(max_attempts),
	{HighestFitness,AttemptIndex} = get(fitness),
	Goal = get(goal),
%	io:format("Fitness:~p HighestFitness:~p,AtemptIndex:~p~n",[U_FitnessAcc,HighestFitness,AttemptIndex]),
	case AffectProgress >= 1 of
		false ->
			put(fitness,{HighestFitness,AttemptIndex});
		true ->
			done = gen_server:call(ExoSelf,memory_reset),
			put(fitness_acc,0),
			case U_FitnessAcc > (HighestFitness + abs(HighestFitness*?MIN_PIMPROVEMENT)) of
				true ->
					put(fitness,{U_FitnessAcc,1}),
					reenter_scape();
				false ->
%					io:format("Pid:~p HighestFitness:~p Fitness:~p~n",[self(),HighestFitness,U_FitnessAcc]),
					case AttemptIndex >= MaxMissedAttempts of
						true ->
							gen_server:cast(ExoSelf,{self(),championship_fitness,{FitnessType,HighestFitness,Goal}}),
							io:format("Pid:~p HighestFitness:~p AttemptIndex:~p~n",[self(),HighestFitness,AttemptIndex]),
							end_training;
						false ->
							put(fitness,{HighestFitness,AttemptIndex+1}),
							reenter_scape()
					end
			end
	end.

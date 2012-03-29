%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(sensors).
-compile(export_all).
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Transducers Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Input channel: A structure within Konishi citizens which receive data from other software.
%Input navigator: A structure within Konishi citizens which issues requests to the polis operating system for data to be provided to the citizen's input channels from a particular address.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Pole balancing sensor %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Input:
%Output: Depending on input, outputs [CartPosition,Pole1Angle,Pole2Angle]
pole2_balancing(CTVL,SensorId,[Parameter])->
%	Input = case get(scape_PId) of
%		undefined ->
%			Scape_PId = gen_server:call(polis,{get_scape,pole_balancing}),
%			put(scape_PId,Scape_PId),
%			gen_server:call(Scape_PId,{control,pole2_balancing,{sensor,SensorId,Parameter}});
%		Scape_PId ->
%			gen_server:call(Scape_PId,{control,pole2_balancing,{sensor,SensorId,Parameter}})
%	end,
%	Input.
	simulations:pole2_balancing(SensorId,Parameter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% vowel_Recognition %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vowel_recognition(CTVL,TableName,Parameters)->
	%read from database
	%return list
	Index = case get(vowel_recognition) of
		undefined -> 
			put(vowel_recognition,1),
			1;
		Val -> 
			Val
	end,
	ets:lookup_element(vowel_recognition,Index,3).

mines_vs_rocks(CTVL,TableName,Parameters)->
	Index = case get(mines_vs_rocks) of
		undefined -> 
			put(mines_vs_rocks,1),
			1;
		Val -> 
			Val
	end,
	ets:lookup_element(mines_vs_rocks,Index,3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Database Read Sensor %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xor_input(_CTVL,_TableName,[Feature])->
	Index=case get(index) of
		undefined ->
			Key=ets:first(xor_table),
			put(index,{Key,0}),
			Key;
		{Key,FitnessAcc} ->
			Key
	end,
	ets:lookup_element(xor_table,Index,Feature).

db_read(CTVL,TableName,[Feature])->
	case get(TableName) of
		undefined -> 
			MDC = db:metadata_category([TableName]),
			IndexStart = db:lookup_element(metadata,MDC,start),
			IndexEnd = db:lookup_element(metadata,MDC,stop),
			put(TableName,{IndexStart,IndexEnd,IndexStart,0}),
			transducers:advanced_read(CTVL,TableName,Feature,IndexStart);
		{_IndexStart,_IndexEnd,Index,_Fitness}->
			transducers:advanced_read(CTVL,TableName,Feature,Index)
	end.
		
	advanced_read(CTVL,TableName,Feature,Index)->%TODO:Create a function s.t. when CTVL is larger than supplied VL from db such that more data is provided
		{Max,Min} = get_scalers(TableName,Feature),
		QVector = case ((TableName == fx5) or (TableName == fx15) or (TableName == fx60)) and (modular == get(type)) of
				false ->
					db:lookup_element(TableName,Index,Feature);
				true ->
					%io:format("CTVL:~p~n",[CTVL]),
					db:lookup_longvector(TableName,Index,Feature,CTVL)
		end,
		%io:format("QVector:~p~n",[QVector]),
		case is_list(QVector) of
			true ->
				[functions:scale(Val,Max,Min) || Val <-QVector];
			false ->
				ScaledVal = functions:scale(QVector,Max,Min),%TODO:Everything is scaled between 1000/-1000
				%ScaledVal = QVector*1000, %TODO: Simple ammplifier by 1000fold. Diff dealing with small numbers.
				%io:format("Feature:~p Max:~p Min:~p Val:~p ScaledVal:~p~n",[Feature,Max,Min,QVector,ScaledVal]),
				[ScaledVal]
		end.
		
		get_scalers(TableName,Feature)->
			case get({scale,Feature}) of
				{Max,Min} ->
					{Max,Min};
				undefined->
					case db:is_forex(TableName) of
						true ->
							{CPair,FeatureName} = db:rmap(TableName,Feature),
							case db:lookup(forex_metadata,{TableName,CPair,FeatureName}) of 
								[] ->
									io:format("******** Scalers not found:~p~n",[{self(),TableName,CPair,FeatureName}]),
									Max = 1,
									Min = -1,
									put({scale,Feature},{Max,Min}),
									{Max,Min};
								_ ->
									%io:format("Scalers found:~p~n",[{self(),TableName,CPair,FeatureName}]),
									Max = db:lookup_element(forex_metadata,{TableName,CPair,FeatureName},max),
									Min = db:lookup_element(forex_metadata,{TableName,CPair,FeatureName},min),
									%Max = 0.0001,
									%Min = -0.0001,
									put({scale,Feature},{Max,Min}),
									{Max,Min}
							end;
						false ->
							%io:format("Using default scaling for:~p~n",[{self(),TableName,Feature}]),
							Max = 1,
							Min = -1,
							put({scale,Feature},{Max,Min}),
							{Max,Min}
					end
			end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Shared Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fanout([Pid|Pids],Msg)->
	gen_server:cast(Pid,Msg),
	fanout(Pids,Msg);
fanout([],_Msg)->
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FLATLANDER SENSORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
energy_reader(CTVL,SensorId,_Parameters)->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			[Self#avatar.energy/10]
	end.
	
distance_scanner(CTVL,SensorId,[Spread,Density,RadialOffset])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			distance_scaner(silent,{1,0,0},Density,Spread,Loc,Direction,lists:keydelete(self(), 2, Avatars))
	end.

color_scanner(CTVL,SensorId,[Spread,Density,RadialOffset])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			color_scaner(silent,{1,0,0},Density,Spread,Loc,Direction,lists:keydelete(self(), 2, Avatars))
	end.

energy_scanner(CTVL,SensorId,[Spread,Density,RadialOffset])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			energy_scaner(silent,{1,0,0},Density,Spread,Loc,Direction,lists:keydelete(self(), 2, Avatars))
	end.

speak_scanner()->
	done.
	
gestalt_scanner()->
	start.

order(CTVL,SensorId,_Parameters)->
	%case gen_server:call(get(scape),{get_all,avatars}) of
	%	destroyed->
	%		ok;
	%	Avatars ->
	%		Self = lists:keyfind(self(),2,Avatars),
	%	%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
	%		Loc = Self#avatar.loc,
	%		Direction = Self#avatar.direction,
	%		energy_scaner(silent,{1,0,0},Density,Spread,Loc,Direction,lists:keydelete(self(), 2, Avatars))
	%end,
	%calculate obedience based on trajectory and agent's original loc.
	case get(order) of
		undefined ->
			Cooloff = 200,
			Command = [random:uniform()*2-1,random:uniform()*2-1],
			put(order,{Cooloff,CommandHold=50,Command,-1}),
			[-1,0,0];
		{Cooloff,CommandHold,Command,_ATag} ->
			%io:format("Order:~p~n",[{Cooloff,CommandHold,Command}]),
			if
				(Cooloff >= 0) ->
					put(order,{Cooloff-1,CommandHold,Command,-1}),
					[-1|Command];
				(Cooloff =< 0) and (CommandHold >= 0) ->
					%Coolfoff finished, hold command and do commanhold countdown
					%io:format("Order:~p~n",[Command]),
					put(order,{Cooloff,CommandHold-1,Command,1}),
					[1|Command];
				(Cooloff =< 0) and (CommandHold =< 0) ->
					erase(order),
					[-1,0,0]
			end
	end.
	
guard(CTVL,SensorId,_Parameters)->
	BeaconCoords=case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			[0,0];
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Beacon=lists:keyfind(beacon,2,Avatars),
			{AX,AY} = Self#avatar.loc,
			Direction = Self#avatar.direction,
			{BX,BY} = Beacon#avatar.loc,
			CoordDif={AX-BX,AY-BY},
			{R,Theta}=geometry:cartesian2polar(CoordDif),
			{_,DirTheta}=geometry:cartesian2polar(Direction),
			[R,Theta-DirTheta]
			%energy_scaner(silent,{1,0,0},Density,Spread,Loc,Direction,lists:keydelete(self(), 2, Avatars))
	end,
	%calculate distance to beacon, based on beacon's loc
	case get(guard) of
		undefined ->
			Cooloff = 100,
			put(guard,{Cooloff,CommandHold=100,Range=200+random:uniform(200),BeaconCoords,-1}),
			[-1,Range|BeaconCoords];
		{Cooloff,CommandHold,Range,_OldBC,_ATag} ->
			%io:format("Guard:~p~n",[{Cooloff,CommandHold,Range}]),
			if
				(Cooloff >= 0) ->
					put(guard,{Cooloff-1,CommandHold,Range,BeaconCoords,-1}),
					[-1,Range|BeaconCoords];
				(Cooloff =< 0) and (CommandHold >= 0) ->
					%Coolfoff finished, hold command and do commanhold countdown
					%io:format("Beacon:~p~n",[{Range,BeaconCoords}]),
					put(guard,{Cooloff,CommandHold-1,Range,BeaconCoords,1}),
					[1,Range|BeaconCoords];
				(Cooloff =< 0) and (CommandHold =< 0) ->
					erase(guard),
					[-1,0,0,0]
			end
	end.

%What should it be?
%Input: ViewAngle= Radian, Density= n, Gaze direction= {SensorLoc,Direction}.
%Output: List of ranges 1/Distance no intersection = -1, with angle starting with Gaze + (ViewAngle/2), and ending with (Gaze - ViewAngle/2), [Dist1...DistDensity].
	distance_scaner(Op,{Zoom,PanX,PanY},Density,Spread,Loc,Direction,Avatars)->
		case is_even(Density) of
			true ->
				Resolution = Spread/Density,
				SAngle = (Density/2)*Resolution,
				StartAngle = -SAngle+Resolution/2;
				%{angle_list((-StartAngle+Resolution/2),Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,(-StartAngle+Resolution/2),[])};
			false ->
				Resolution = Spread/Density,
				SAngle=trunc(Density/2)*Resolution,
				StartAngle = -SAngle
				%io:format("Resolution:~p StartAngle:~p Density:~p Spread:~p~n",[Resolution,StartAngle,Density,Spread]),
				%{angle_list(-StartAngle,Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,-StartAngle,[])}
		end,
		UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
		RangeScanList = compose_RangeScanList(Loc,UnitRays,Avatars,[]),
		%io:format("RangeScanList:~p~n",[RangeScanList]),
		case {Op,get(canvas)} of
			{silent,_} ->
				done;
			{draw,undefined} ->
				Canvas = gen_server:call(get(scape),get_canvas),
				put(canvas,Canvas);
			{draw,Canvas}->
				{X,Y} = Loc,
				FLoc = {X*Zoom+PanX,Y*Zoom+PanY},
				ScanListP=lists:zip(UnitRays,RangeScanList),
				Ids = [gs:create(line,Canvas,[{coords,[FLoc,{(X+Xr*Scale)*Zoom+PanX,(Y+Yr*Scale)*Zoom+PanY}]}])||{{Xr,Yr},Scale}<-ScanListP, Scale =/= -1],
				timer:sleep(2),
				[gs:destroy(Id) || Id<- Ids]
		end,
		RangeScanList.
		
		compose_RangeScanList(Loc,[Ray|UnitRays],Avatars,Acc)->
			{Distance,_Color}=shortest_intrLine({Loc,Ray},Avatars,{inf,void}),
			compose_RangeScanList(Loc,UnitRays,Avatars,[Distance|Acc]);
		compose_RangeScanList(_Loc,[],_Avatars,Acc)->
			lists:reverse(Acc).

	color_scaner(Op,{Zoom,PanX,PanY},Density,Spread,Loc,Direction,Avatars)->
		case is_even(Density) of
			true ->
				Resolution = Spread/Density,
				SAngle = (Density/2)*Resolution,
				StartAngle = -SAngle+Resolution/2;
				%{angle_list((-StartAngle+Resolution/2),Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,(-StartAngle+Resolution/2),[])};
			false ->
				Resolution = Spread/Density,
				SAngle=trunc(Density/2)*Resolution,
				StartAngle = -SAngle
				%io:format("Resolution:~p StartAngle:~p Density:~p Spread:~p~n",[Resolution,StartAngle,Density,Spread]),
				%{angle_list(-StartAngle,Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,-StartAngle,[])}
		end,
		UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
		ColorScanList = compose_ColorScanList(Loc,UnitRays,Avatars,[]),
		%io:format("ColorScanList:~p~n",[ColorScanList]),
		case {Op,get(canvas)} of
			{silent,_} ->
				done;
			{draw,undefined} ->
				Canvas = gen_server:call(get(scape),get_canvas),
				put(canvas,Canvas);
			{draw,Canvas}->
				{X,Y} = Loc,
				FLoc = {X*Zoom+PanX,Y*Zoom+PanY},
				ScanListP=lists:zip(UnitRays,ColorScanList),
				Ids = [gs:create(line,Canvas,[{coords,[FLoc,{(X+Xr*25)*Zoom+PanX,(Y+Yr*25)*Zoom+PanY}]},{fg,val2clr(Color)}])||{{Xr,Yr},Color}<-ScanListP],
				timer:sleep(2),
				[gs:destroy(Id) || Id<- Ids]
		end,
		ColorScanList.
		
		compose_ColorScanList(Loc,[Ray|UnitRays],Avatars,Acc)->
			{_Distance,Color}=shortest_intrLine({Loc,Ray},Avatars,{inf,void}),
			compose_ColorScanList(Loc,UnitRays,Avatars,[Color|Acc]);
		compose_ColorScanList(_Loc,[],_Avatars,Acc)->
			lists:reverse(Acc).

	energy_scaner(Op,{Zoom,PanX,PanY},Density,Spread,Loc,Direction,Avatars)->
		case is_even(Density) of
			true ->
				Resolution = Spread/Density,
				SAngle = (Density/2)*Resolution,
				StartAngle = -SAngle+Resolution/2;
				%{angle_list((-StartAngle+Resolution/2),Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,(-StartAngle+Resolution/2),[])};
			false ->
				Resolution = Spread/Density,
				SAngle=trunc(Density/2)*Resolution,
				StartAngle = -SAngle
				%io:format("Resolution:~p StartAngle:~p Density:~p Spread:~p~n",[Resolution,StartAngle,Density,Spread]),
				%{angle_list(-StartAngle,Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,-StartAngle,[])}
		end,
		UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
		EnergyScanList = compose_EnergyScanList(Loc,UnitRays,Avatars,[]),
		%io:format("RangeScanList:~p~n",[RangeScanList]),
		case Op of
			silent ->
				done;
			draw ->
				io:format("EnergyScanList:~p~n",[EnergyScanList])
		end,
		EnergyScanList.
		
		compose_EnergyScanList(Loc,[Ray|UnitRays],Avatars,Acc)->
			{_Distance,_Color,Energy}=shortest_intrLine2({Loc,Ray},Avatars,{inf,void},0),
			%io:format("compose_EnergyScanList:~p~n",[Energy]),
			compose_EnergyScanList(Loc,UnitRays,Avatars,[Energy/100|Acc]);
		compose_EnergyScanList(_Loc,[],_Avatars,Acc)->
			lists:reverse(Acc).

		shortest_intrLine2(Gaze,[Avatar|Avatars],Val,Energy)->
			{D,_} = Val,
			{U_D,U_C} = intr(Gaze,Avatar#avatar.objects,Val),
			U_Energy = case D == U_D of
				true ->
					Energy;
				false ->
					Avatar#avatar.energy
			end,
			shortest_intrLine2(Gaze,Avatars,{U_D,U_C},U_Energy);
		shortest_intrLine2(_Gaze,[],{Distance,Color},Energy)->
			case Distance of
				inf ->%TODO, perhaps absence of color should be -1, not 1.
					{-1,1,Energy};
				0.0 ->
					{-1,1,Energy};
				_ ->
					{Distance,clr2val(Color),Energy}
			end.

		create_UnitRays(_,0,_,_,Acc)->
			Acc;
		create_UnitRays({X,Y},Density,Resolution,Angle,Acc)->
			%io:format("Angle:~p~n",[Angle*180/math:pi()]),
			UnitRay = {X*math:cos(Angle) - Y*math:sin(Angle), X*math:sin(Angle) + Y*math:cos(Angle)},
			create_UnitRays({X,Y},Density-1,Resolution,Angle+Resolution,[UnitRay|Acc]).

		shortest_intrLine(Gaze,[Avatar|Avatars],Val)->
			shortest_intrLine(Gaze,Avatars,intr(Gaze,Avatar#avatar.objects,Val));
		shortest_intrLine(_Gaze,[],{Distance,Color})->
			case Distance of
				inf ->%TODO, perhaps absence of color should be -1, not 1.
					{-1,1};
				0.0 ->
					{-1,1};
				_ ->
					{Distance,clr2val(Color)}
			end.

		intr(Gaze,[{circle,_Id,Color,_Pivot,C,R}|Objects],{Min,MinColor})->
			{S,D} = Gaze,
			[{Xc,Yc}] = C,
			{Xs,Ys} = S,
			{Xd,Yd} = D,
			{Xv,Yv} = {Xs-Xc,Ys-Yc},
			VdotD = Xv*Xd + Yv*Yd,
			Dis = math:pow(VdotD,2) - (Xv*Xv + Yv*Yv - R*R),
			%io:format("S:~p D:~p C:~p V:~p R:~p VdotD:~p Dis:~p~n",[S,D,C,{Xv,Yv},R,VdotD,Dis]),
			Result=case Dis > 0 of
				false ->
					inf;
				true ->
					SqrtDis = math:sqrt(Dis),
					I1 = -VdotD - SqrtDis,
					I2 = -VdotD + SqrtDis,
					case (I1 > 0) and (I2 >0) of
						true ->
							erlang:min(I1,I2);
						false ->
							inf
					end
			end,
			{UMin,UMinColor}=case Result < Min of
				true ->
					{Result,Color};
				false ->
					{Min,MinColor}
			end,
			intr(Gaze,Objects,{UMin,UMinColor});
		intr(Gaze,[{line,_Id,Color,_Pivot,[{X3,Y3},{X4,Y4}],_Parameter}|Objects],{Min,MinColor})->
			{S,D} = Gaze,
			{X1,Y1} = S,
			{XD0,YD0} = D,
			PerpXD1 = Y4-Y3,
			PerpYD1 = -(X4-X3),
			PerpXD0 = YD0,
			PerpYD0 = -XD0,
			Result=case PerpXD1*XD0 + PerpYD1*YD0 of
				0.0 ->
					inf;
				Denom ->
					RayLength = ((PerpXD1*(X3-X1)) + (PerpYD1*(Y3-Y1)))/Denom,
					T = ((PerpXD0*(X3-X1)) + (PerpYD0*(Y3-Y1)))/Denom,			
					case (RayLength >= 0) and (T >= 0) and (T =< 1) of
						true ->
							RayLength;
						false ->
							inf
					end
			end,
			{UMin,UMinColor}=case Result < Min of
				true ->
					{Result,Color};
				false ->
					{Min,MinColor}
			end,
			intr(Gaze,Objects,{UMin,UMinColor});
		intr(_Gaze,[],{Min,MinColor})->
			{Min,MinColor}.

	shortest_distance(OperatorAvatar,Avatars)->
		Loc = OperatorAvatar#avatar.loc,
		shortest_distance(Loc,Avatars,inf).
		
		shortest_distance({X,Y},[Avatar|Avatars],SD)->
			{LX,LY} = Avatar#avatar.loc,
			Distance = math:sqrt(math:pow(X-LX,2)+math:pow(Y-LY,2)),
			shortest_distance({X,Y},Avatars,erlang:min(SD,Distance));
		shortest_distance({_X,_Y},[],SD)->
			case SD of
				inf ->
					-1;
				_ ->
					SD
			end.

coned_avatar_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
			%io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			TargetAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type =/= borders],
			coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,TargetAvatars)
	end.

coned_color_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			TargetAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type =/= borders],
			coned_color_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,TargetAvatars)
	end.	

coned_energy_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			TargetAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), (A#avatar.type == flatlander) or (A#avatar.type == prey) or (A#avatar.type == plant)],
%			io:format("TargetAvatars:~p~n",[length(TargetAvatars)]),
			coned_energy_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,TargetAvatars)
	end.
	
coned_sound_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			TargetAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), (A#avatar.type == flatlander) or (A#avatar.type == prey)],
%s			io:format("TargetAvatars:~p~n",[length(TargetAvatars)]),
			coned_sound_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,TargetAvatars)
	end.

coned_flatlander_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			TargetAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type == flatlander],
			coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,TargetAvatars)
	end.

coned_plant_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			TargetAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), (A#avatar.type == plant)],
			coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,TargetAvatars)
	end.
	

coned_poison_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			TargetAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type == poison],
			coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,TargetAvatars)
	end.

coned_plantandpoison_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			PlantAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type == plant],
			PoisonAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type == poison],
			PlantVector=coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,PlantAvatars),
			PoisonVector=coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,PoisonAvatars),
			PlantVector++PoisonVector
	end.

coned_PlantPredatorPrey_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			PlantAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type == plant],
			FlatlanderAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type == flatlander],
			PreyAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type == prey],
			PlantVector=coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,PlantAvatars),
			FlatlanderVector=coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,FlatlanderAvatars),
			PreyVector=coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,PreyAvatars),
			PlantVector++FlatlanderVector++PreyVector
	end.

coned_prey_sensor(CTVL,SensorId,[Density])->
	case gen_server:call(get(scape),{get_all,avatars}) of
		destroyed->
			lists:duplicate(CTVL,-1);
		Avatars ->
			Self = lists:keyfind(self(),2,Avatars),
		%	io:format("Avatars:~p Self:~p~n",[Avatars,Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			R = Self#avatar.r,
			TargetAvatars = [A || A<-lists:keydelete(self(), 2, Avatars), A#avatar.type == prey],
			coned_object_sensor(silent,{1,0,0},R,Density,2*math:pi(),Loc,Direction,TargetAvatars)
	end.

	coned_object_sensor(Op,{Zoom,PanX,PanY},R,Density,Spread,{X,Y},Direction,Avatars)->
		case is_even(Density) of
			true ->
				Resolution = Spread/Density,
				SAngle = (Density/2)*Resolution,
				StartAngle = -SAngle+Resolution/2;
				%{angle_list((-StartAngle+Resolution/2),Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,(-StartAngle+Resolution/2),[])};
			false ->
				Resolution = Spread/Density,
				SAngle=trunc(Density/2)*Resolution,
				StartAngle = -SAngle
				%io:format("Resolution:~p StartAngle:~p Density:~p Spread:~p~n",[Resolution,StartAngle,Density,Spread]),
				%{angle_list(-StartAngle,Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,-StartAngle,[])}
		end,
		UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
		RangeList = [is_in_cone(X,Y,X+Xd,Y+Yd,Resolution/2,Avatars,inf) || {Xd,Yd} <- UnitRays],
		case {Op,get(canvas)} of
			{silent,_} ->
				done;
			{draw,undefined} ->
				Canvas = gen_server:call(get(scape),get_canvas),
				put(canvas,Canvas);
			{draw,Canvas}->
				RangeListP=lists:zip(UnitRays,RangeList),
			Ids = [gs:create(line,Canvas,[{coords,[{X*Zoom+PanX,Y*Zoom+PanY},{(X+Xd*Sc)*Zoom+PanX,(Y+Yd*Sc)*Zoom+PanY}]}])||{{Xd,Yd},Sc}<-RangeListP,Sc =/= -1],
				timer:sleep(2),
				[gs:destroy(Id) || Id<- Ids]
		end,
%		io:format("RangeList~p~n",[RangeList]),
		RangeList.

		is_in_cone(X1,Y1,X2,Y2,HalfCone,[Avatar|Avatars],Val)->
			{X3,Y3} = Avatar#avatar.loc,
			Angle = try angle([{X1,Y1},{X2,Y2}],[{X1,Y1},{X3,Y3}]) of
				Result ->
					Result
				catch
					E->
						io:format("***********************************************************IS_IN_CONE STACKTRACE~p~n",[erlang:get_stacktrace()]),
						Msg = [{X1,Y1},{X2,Y2},{X3,Y3},erlang:get_stacktrace()],
						logger ! {self(),store,Msg},
						io:format("~p~n",[{{X1,Y1},{X2,Y2},{X3,Y3},erlang:get_stacktrace()}]),
						ets:insert(test1,{self(),[{{X1,Y1},{X2,Y2},{X3,Y3},E}]}),
						0
			end,
			U_Val=case abs(Angle) < HalfCone of
				true ->
					Distance = math:sqrt(math:pow(X1-X3,2)+math:pow(Y1-Y3,2)),%TODO - Avatar#avatar.r,
					erlang:min(Distance,Val);
				false ->
					Val
			end,
			is_in_cone(X1,Y1,X2,Y2,HalfCone,Avatars,U_Val);
		is_in_cone(_X1,_Y1,_X2,_Y2,_HalfCone,[],Distance)->
			case Distance of
				inf ->
					-1;
				_ ->
					Distance
			end.

	coned_color_sensor(Op,{Zoom,PanX,PanY},R,Density,Spread,{X,Y},Direction,Avatars)->
		case is_even(Density) of
			true ->
				Resolution = Spread/Density,
				SAngle = (Density/2)*Resolution,
				StartAngle = -SAngle+Resolution/2;
				%{angle_list((-StartAngle+Resolution/2),Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,(-StartAngle+Resolution/2),[])};
			false ->
				Resolution = Spread/Density,
				SAngle=trunc(Density/2)*Resolution,
				StartAngle = -SAngle
				%io:format("Resolution:~p StartAngle:~p Density:~p Spread:~p~n",[Resolution,StartAngle,Density,Spread]),
				%{angle_list(-StartAngle,Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,-StartAngle,[])}
		end,
		UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
		RangeList = [is_in_cone_color(X,Y,X+Xd,Y+Yd,Resolution/2,Avatars,inf) || {Xd,Yd} <- UnitRays],
		%io:format("RangeList:~p~n",[RangeList]),
		case {Op,get(canvas)} of
			{silent,_} ->
				done;
			{draw,undefined} ->
				Canvas = gen_server:call(get(scape),get_canvas),
				put(canvas,Canvas);
			{draw,Canvas}->
				RangeListP=lists:zip(UnitRays,RangeList),
				Ids = [gs:create(line,Canvas,[{coords,[{X*Zoom+PanX,Y*Zoom+PanY},{(X+Xd*Sc)*Zoom+PanX,(Y+Yd*Sc)*Zoom+PanY}]}])||{{Xd,Yd},Sc}<-RangeListP],
				timer:sleep(2),
				[gs:destroy(Id) || Id<- Ids]
		end,
		RangeList.

		is_in_cone_color(X1,Y1,X2,Y2,HalfCone,[Avatar|Avatars],Val)->
			{X3,Y3} = Avatar#avatar.loc,
			Angle = angle([{X1,Y1},{X2,Y2}],[{X1,Y1},{X3,Y3}]),
			U_Val=case abs(Angle) < HalfCone of
				true ->
					Distance = math:sqrt(math:pow(X1-X3,2)+math:pow(Y1-Y3,2)),
					case Distance < Val of
						true ->
							type2color(Avatar#avatar.type);
							%Avatar#avatar.team;
						false ->
							Val
					end;
				false ->
					Val
			end,
			is_in_cone_color(X1,Y1,X2,Y2,HalfCone,Avatars,U_Val);
		is_in_cone_color(_X1,_Y1,_X2,_Y2,_HalfCone,[],Color)->
			case Color of
				inf ->
					-1;
				_ ->
					Color
			end.

			type2color(Type)->
				case Type of
					prey -> 1;
					predator -> 0;
					plant -> -1
				end.

coned_energy_sensor(Op,{Zoom,PanX,PanY},R,Density,Spread,{X,Y},Direction,Avatars)->
	case is_even(Density) of
		true ->
			Resolution = Spread/Density,
			SAngle = (Density/2)*Resolution,
			StartAngle = -SAngle+Resolution/2;
			%{angle_list((-StartAngle+Resolution/2),Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,(-StartAngle+Resolution/2),[])};
		false ->
			Resolution = Spread/Density,
			SAngle=trunc(Density/2)*Resolution,
			StartAngle = -SAngle
			%io:format("Resolution:~p StartAngle:~p Density:~p Spread:~p~n",[Resolution,StartAngle,Density,Spread]),
			%{angle_list(-StartAngle,Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,-StartAngle,[])}
	end,
	UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
%	io:format("UnitRays:~p~n",[UnitRays]),
	RangeList = [is_in_cone_energy(X,Y,X+Xd,Y+Yd,Resolution/2,Avatars,inf) || {Xd,Yd} <- UnitRays],
%	io:format("RangeList:~p~n",[RangeList]),
	case {Op,get(canvas)} of
		{silent,_} ->
			done;
		{draw,undefined} ->
			Canvas = gen_server:call(get(scape),get_canvas),
			put(canvas,Canvas);
		{draw,Canvas}->
			RangeListP=lists:zip(UnitRays,RangeList),
			Ids = [gs:create(line,Canvas,[{coords,[{X*Zoom+PanX,Y*Zoom+PanY},{(X+Xd*Sc)*Zoom+PanX,(Y+Yd*Sc)*Zoom+PanY}]}])||{{Xd,Yd},Sc}<-RangeListP],
			timer:sleep(2),
			[gs:destroy(Id) || Id<- Ids]
	end,
	RangeList.

	is_in_cone_energy(X1,Y1,X2,Y2,HalfCone,[Avatar|Avatars],Val)->
		{X3,Y3} = Avatar#avatar.loc,
		Angle = angle([{X1,Y1},{X2,Y2}],[{X1,Y1},{X3,Y3}]),
		U_Val=case abs(Angle) < HalfCone of
			true ->
				Distance = math:sqrt(math:pow(X1-X3,2)+math:pow(Y1-Y3,2)),
				case Distance < Val of
					true ->
						E_Resolution = 100,
						AEnergy = Avatar#avatar.energy/E_Resolution,
%						io:format("AEnergy:~p~n",[AEnergy]),
						if 
							(AEnergy >= 1) -> 
								1 - 1/AEnergy;
							(AEnergy =< -1) ->
								-1 - 1/AEnergy;
							true ->
								0
						end;
					false ->
						Val
				end;
			false ->
				Val
		end,
		is_in_cone_energy(X1,Y1,X2,Y2,HalfCone,Avatars,U_Val);
	is_in_cone_energy(_X1,_Y1,_X2,_Y2,_HalfCone,[],Energy)->
		case Energy of
			inf ->
				-1;
			_ ->
				Energy
		end.

coned_sound_sensor(Op,{Zoom,PanX,PanY},R,Density,Spread,{X,Y},Direction,Avatars)->
	case is_even(Density) of
		true ->
			Resolution = Spread/Density,
			SAngle = (Density/2)*Resolution,
			StartAngle = -SAngle+Resolution/2;
			%{angle_list((-StartAngle+Resolution/2),Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,(-StartAngle+Resolution/2),[])};
		false ->
			Resolution = Spread/Density,
			SAngle=trunc(Density/2)*Resolution,
			StartAngle = -SAngle
			%io:format("Resolution:~p StartAngle:~p Density:~p Spread:~p~n",[Resolution,StartAngle,Density,Spread]),
			%{angle_list(-StartAngle,Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,-StartAngle,[])}
	end,
	UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
%	io:format("UnitRays:~p~n",[UnitRays]),
	RangeList = [is_in_cone_sound(X,Y,X+Xd,Y+Yd,Resolution/2,Avatars,inf) || {Xd,Yd} <- UnitRays],
%	io:format("RangeList:~p~n",[RangeList]),
	case {Op,get(canvas)} of
		{silent,_} ->
			done;
		{draw,undefined} ->
			Canvas = gen_server:call(get(scape),get_canvas),
			put(canvas,Canvas);
		{draw,Canvas}->
			RangeListP=lists:zip(UnitRays,RangeList),
			Ids = [gs:create(line,Canvas,[{coords,[{X*Zoom+PanX,Y*Zoom+PanY},{(X+Xd*Sc)*Zoom+PanX,(Y+Yd*Sc)*Zoom+PanY}]}])||{{Xd,Yd},Sc}<-RangeListP],
			timer:sleep(2),
			[gs:destroy(Id) || Id<- Ids]
	end,
	RangeList.

	is_in_cone_sound(X1,Y1,X2,Y2,HalfCone,[Avatar|Avatars],Val)->
		{X3,Y3} = Avatar#avatar.loc,
		Angle = angle([{X1,Y1},{X2,Y2}],[{X1,Y1},{X3,Y3}]),
		U_Val=case abs(Angle) < HalfCone of
			true ->
				Distance = math:sqrt(math:pow(X1-X3,2)+math:pow(Y1-Y3,2)),
				case Distance < Val of
					true ->
						Sound = Avatar#avatar.sound,
						io:format("Sound:~p~n",[Sound]),
						Sound;
					false ->
						Val
				end;
			false ->
				Val
		end,
		is_in_cone_sound(X1,Y1,X2,Y2,HalfCone,Avatars,U_Val);
	is_in_cone_sound(_X1,_Y1,_X2,_Y2,_HalfCone,[],Sound)->
		case Sound of
			inf ->
				0;
			_ ->
				Sound
		end.

angle([{X1,Y1},{X2,Y2}],[{X3,Y3},{X4,Y4}])->%Non directional, only magnitude of the angle between the 2 lines.
	{Ux,Uy} = {X2-X1,Y2-Y1},
	{Vx,Vy} = {X4-X3,Y4-Y3},
	Ulength = math:sqrt(Ux*Ux+Uy*Uy),
	Vlength = math:sqrt(Vx*Vx+Vy*Vy),
	Denomenator = (Ulength*Vlength),
	Val = case Denomenator == 0 of
		true ->
			(Ux*Vx+Uy*Vy)/0.00001;
		false ->
 			(Ux*Vx+Uy*Vy)/(Ulength*Vlength)
	end,
	case (Val >= -1) and (Val =< 1) of
		true ->
			math:acos(Val);
		false ->
			case Val > 1 of
				true ->
					0;
				false ->
					math:pi()
			end
	end.
	
clr2val(Color)->
	case Color of
		black -> -1; %poison
		cyan -> -0.75;
		green -> -0.5; %plant
		yellow -> -0.25;
		blue -> 0; %prey
		gret -> 0.25;
		red -> 0.5; %predator
		brown -> 0.75; % wall
		_ -> 1%io:format("transducers:clr2val(Color): Color = ~p~n",[Color]), 1 %emptiness
	end.
	
val2clr(Val)->
	case Val of
		-1 -> black;
		-0.75 -> cyan;
		-0.5 -> green;
		-0.25 -> yellow;
		0 -> blue;
		0.25 -> grey;
		0.5 ->	red;
		0.75 -> brown;
		_ -> white
	end.

is_even(Val)->
	case (Val rem 2) of
		0 ->
			true;
		_ ->
			false
	end.

cone(Density,Spread)->
	case is_even(Density) of
		true ->
			Resolution = Spread/Density,
			StartAngle = (Density/2)*Resolution,
			{angle_list((-StartAngle+Resolution/2),Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,(-StartAngle+Resolution/2),[])};
		false ->
			Resolution = Spread/Density,
			StartAngle=trunc(Density/2)*Resolution,
			io:format("Resolution:~p StartAngle:~p Density:~p Spread:~p~n",[Resolution,StartAngle,Density,Spread]),
			{angle_list(-StartAngle,Density,Resolution,[]),create_UnitRays({0,1},Density,Resolution,-StartAngle,[])}
	end.

	angle_list(_Angle,0,_Resolution,Acc)->
		Acc;
	angle_list(Angle,Index,Resolution,Acc)->
		angle_list(Angle+Resolution,Index-1,Resolution,[Angle|Acc]).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FOREX SENSORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fx_GraphSensor(CTVL,_SensorId,Parameters)->
	case get(fx_pid) of
		undefined ->
			PId = fx:sim(self()),
			put(fx_pid,PId);
		PId ->
			PId
	end,
	[HRes,VRes] = Parameters,
	case get(opmode) of
		gt	->
			%Normal, assuming we have 10000 rows, we start from 1000 to 6000
			PId ! {self(),sense,'EURUSD15',close,[HRes,VRes,graph_sensor],1000,200};
		benchmark ->
			PId ! {self(),sense,'EURUSD15',close,[HRes,VRes,graph_sensor],200,last}
	end,
	receive 
		{_From,Result}->
			%io:format("Result:~p~n",[Result]),
			Result
	end.

fx_ListSensor(CTVL,_SensorId,Parameters)->
	case get(fx_pid) of
		undefined ->
			PId = fx:sim(self()),
			put(fx_pid,PId);
		PId ->
			PId
	end,
	[HRes,Type] = Parameters,%Type=open|close|high|low
	%PId ! {self(),sense,'EURUSD15',Type,[HRes,list_sensor]},
	case get(opmode) of
		gt	->
			%Normal, assuming we have 10000 rows, we start from 1000 to 6000
			PId ! {self(),sense,'EURUSD15',close,[HRes,list_sensor],5000,1000};
		benchmark ->
			PId ! {self(),sense,'EURUSD15',close,[HRes,list_sensor],1000,last}
	end,
	receive 
		{_From,Result}->
%			io:format("Result:~p~n",[Result]),
			normalize(Result)
	end.
	
	normalize(Vector)->
		Normalizer=math:sqrt(lists:sum([Val*Val||Val<-Vector])),
		[Val/Normalizer || Val <- Vector].
	
fx_Internals(CTVL,_SensorId,Parameters)->
	%io:format("CTVL:~p Parameters:~p~n",[CTVL,Parameters]),
	case get(fx_pid) of
		undefined ->
			PId = fx:sim(self()),
			put(fx_pid,PId);
		PId ->
			PId
	end,
	PId ! {self(),sense,internals,Parameters},
	receive
		{PId,Result}->
			Result
	end.

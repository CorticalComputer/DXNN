%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(world).
-compile(export_all).
-include("records.hrl").
-define(COLLISIONS,on).

init(World_Type,Physics,Metabolics)->
	XMin = -5000,
	XMax = 5000,
	YMin = -5000,
	YMax = 5000,
	WorldPivot = {(XMin+XMax)/2,(YMin+YMax)/2},
	World_Border = [{XMin,XMax},{YMin,YMax}],

	case World_Type of
		duel ->	
			Id=technome_constructor:generate_UniqueId(),
			Plants=[scape:create_avatar(plant,plant,Id,{undefined,undefined},respawn,Metabolics)|| _<-lists:duplicate(10,1)],
			Walls = lists:append(create_walls(),create_pillars()),
			Scape_Physics = [],
			Plants++Walls;
		hunt ->
			[];
		dangerous_hunt ->
			[];
		flatland ->
			%Plants=[scape:create_avatar(plant,plant,gen_id(),{undefined,undefined},respawn,Metabolics)||_<-lists:duplicate(10,1)],
			%Poisons=[scape:create_avatar(poison,poison,gen_id(),{undefined,undefined},respawn,Metabolics)||_<-lists:duplicate(10,1)],
			Walls = lists:append(create_walls(),create_pillars()),
			Rocks = create_rocks(),
			FirePits = create_firepits(),
			Beacons = create_beacons(),
			Scape_Physics = [],
			%io:format("Plants:~p~n",[Plants]),
			Plants=[scape:create_avatar(plant,plant,gen_id(),{undefined,scape:return_valid(Rocks++FirePits)},respawn,Metabolics)||_<-lists:duplicate(10,1)],
			Poisons=[scape:create_avatar(poison,poison,gen_id(),{undefined,scape:return_valid(Rocks++FirePits)},respawn,Metabolics)||_<-lists:duplicate(10,1)],
			Plants;%++Beacons;%++Rocks++Walls++Poisons++FirePits++Beacons;%++Poisons;%++Walls;
		dynamic ->
			[];
		baator ->
			Id=technome_constructor:generate_UniqueId(),
			Plants=[scape:create_avatar(plant,plant,Id,{undefined,undefined},respawn,Metabolics)|| _<-lists:duplicate(10,1)],
			Poisons=[scape:create_avatar(poison,poison,Id,{undefined,undefined},respawn,Metabolics)|| _<-lists:duplicate(10,1)],
			Walls = lists:append(create_walls(),create_pillars()),
			Scape_Physics = [],
			Plants++Walls;
		multi_agent ->
			[]
	end.

behavior(Collision,Penetration,OAvatar,Avatar)->
	OAType = OAvatar#avatar.type,
	AType = Avatar#avatar.type,
	if
		(OAType == prey) and (AType == plant) and (OAvatar#avatar.spear =/= undefined)->%OAvatar is a predator
			%io:format("Prey: ~p ate a plant: ~p gained energy:~p~n",[OAvatar#avatar.id,Avatar#avatar.id,Avatar#avatar.energy*0.2]),
			{Avatar#avatar.energy*0.2,plant_eaten,OAvatar,Avatar};
		(OAType == prey) and (AType == plant) ->
			%io:format("Prey: ~p ate a plant: ~p~n",[OAvatar#avatar.id,Avatar#avatar.id]),
			{Avatar#avatar.energy,plant_eaten,OAvatar,Avatar};
		(OAType == prey) and (AType == poison) ->
			%io:format("Prey: ~p ate a plant: ~p~n",[OAvatar#avatar.id,Avatar#avatar.id]),
			{Avatar#avatar.energy,poison_eaten,OAvatar,Avatar};
			%{500,plant_eaten};
		(OAType == prey) and (AType == prey) and (Penetration == true) and (Avatar#avatar.spear == undefined)->%OAvatar spears Avatar who is a prey
			{500,destroy,OAvatar,Avatar};
		(OAType == prey) and (AType == prey) and (Penetration == true) and (Avatar#avatar.spear =/= undefined)->%OAvatar/predator spears another predator
			io:format("########Predator killed another predator~n"),
			{100,destroy,OAvatar,Avatar};
		(OAType == prey) and (AType == prey) and (Collision == true)->
			U_Avatar=case ?COLLISIONS of
				on ->
					PushStrength = 0.1,%Push
					push(OAvatar,Avatar,PushStrength);
				off ->
					Avatar
			end,
			{0,void,OAvatar,U_Avatar};
		(OAType == flatlander) and (AType == prey) and (Penetration == true)->
%			io:format("Hunter: ~p ate a Prey: ~p~n",[OAvatar#avatar.id,Avatar#avatar.id]),
			{500,destroy,OAvatar,Avatar};
		(OAType == flatlander) and (AType == prey) and (Collision == true)->
			U_Avatar=case ?COLLISIONS of
				on ->
					PushStrength = 1,%Push_Hard
					push(OAvatar,Avatar,PushStrength);
				off ->
					Avatar
			end,
			{0,void,OAvatar,U_Avatar};
		(OAType == flatlander) and (AType == flatlander)  and (Penetration == true)->
			U_Avatar=case ?COLLISIONS of
				on ->
					PushStrength = 1,%Push_Very_Hard %TODO, pushing must be done from the tip of the sword.
					push(OAvatar,Avatar,PushStrength);
				off ->
					Avatar
			end,
			{0,void,OAvatar,U_Avatar};
		(OAType == flatlander) and (AType == flatlander)  and (Collision == true)->
			U_Avatar=case ?COLLISIONS of
				on ->
					PushStrength = 0.1,%Push_Hard
					push(OAvatar,Avatar,PushStrength);
				off ->
					Avatar
			end,
			{0,void,OAvatar,U_Avatar};
		(OAType == flatlander) and ((AType == plant) or (AType == poison)) and (Collision == true)->
			U_Avatar=case ?COLLISIONS of
				on ->
					PushStrength = 0,%Push_asside
					push(OAvatar,Avatar,PushStrength);
				off ->
					Avatar
			end,
			{0,void,OAvatar,U_Avatar};
		(AType == rock) ->
			case ?COLLISIONS of
				on ->
					case OAvatar#avatar.energy > Avatar#avatar.energy of
						true ->%TODO
							PushStrength = 1,%Push
							U_Avatar = push(OAvatar,Avatar,PushStrength),
							{-1,void,OAvatar,U_Avatar};
						false ->
							PushStrength = 0,
							U_OAvatar=push(Avatar,OAvatar,PushStrength),
							{-1,void,U_OAvatar,Avatar}
					end;
				off ->
					{0,void,OAvatar,Avatar}
			end;
		(AType == fire_pit) ->
			case ?COLLISIONS of
				on ->
					case OAvatar#avatar.energy > Avatar#avatar.energy of
						true ->%TODO
							PushStrength = 1,%Push
							U_Avatar = push(OAvatar,Avatar,PushStrength),
							{-100,void,OAvatar,U_Avatar};
						false ->
							PushStrength = 0,
							U_OAvatar=push(Avatar,OAvatar,PushStrength),
							{-100,void,U_OAvatar,Avatar}
					end;
				off ->
					{0,void,OAvatar,Avatar}
			end;
		(AType == beacon) ->
			case ?COLLISIONS of
				on ->
					case OAvatar#avatar.energy > Avatar#avatar.energy of
						true ->%TODO
							PushStrength = 1,%Push
							U_Avatar = push(OAvatar,Avatar,PushStrength),
							{0,void,OAvatar,U_Avatar};
						false ->
							PushStrength = 0,
							U_OAvatar=push(Avatar,OAvatar,PushStrength),
							{0,void,U_OAvatar,Avatar}
					end;
				off ->
					{0,void,OAvatar,Avatar}
			end;
		true ->
			{0,void,OAvatar,Avatar}
	end.

push(OAvatar,Avatar,PushStrength)->
	OAEnergy = OAvatar#avatar.energy,
	AEnergy = Avatar#avatar.energy,
	case OAEnergy > AEnergy of
		true ->
			{OX,OY} = OAvatar#avatar.loc,
			{X,Y} = Avatar#avatar.loc,
			DX = X-OX,
			DY = Y-OY,
			Distance = math:sqrt(math:pow(OX-X,2)+math:pow(OY-Y,2)),
%	io:format("OAvatar Type:~p Avatar Type:~p OX:~p OY:~p X:~p Y:~p DX:~p DY:~p Distance:~p~n",[OAvatar#avatar.type,Avatar#avatar.type,OX,OY,X,Y,DX,DY,Distance]),
			Min_Distance = OAvatar#avatar.r + Avatar#avatar.r,
			case Distance == 0 of
				true ->
					MinPushX = -DX,
					MinPushY = -DY;
				false ->
					MinPushX = (Min_Distance/Distance)*DX - DX,
					MinPushY = (Min_Distance/Distance)*DY - DY
			end,
			%PushX = MinPushX+(DX/abs(DX))*PushStrength,
			%PushY = MinPushY+(DY/abs(DY))*PushStrength,
			PushX = MinPushX + case DX == 0 of
				true -> 0;
				false -> (DX/abs(DX))*PushStrength
			end,
			PushY = MinPushY + case DY == 0 of
				true -> 0;
				false -> (DY/abs(DY))*PushStrength
			end,
%			NewLoc = {NewX,NewY}={X+PushX,Y+PushY},
%			NewDistance = math:sqrt(math:pow(NewX-OX,2)+math:pow(NewY-OY,2)),
%			io:format("MinPushX:~p MinPushY:~p~n",[MinPushX,MinPushY]),
%			io:format("Pusher:~p Pushee:~p~n Push:~p~n NewLoc:~p~n Distance:~p NewDistance:~p~n",[{OX,OY},{X,Y},{PushX,PushY},NewLoc,Distance,NewDistance]),
			U_Loc = {X+PushX,Y+PushY},
		U_Objects=[{ObjName,Id,Color,{PX+PushX,PY+PushY},[{CX+PushX,CY+PushY}||{CX,CY}<-Coords],P}||{ObjName,Id,Color,{PX,PY},Coords,P}<-Avatar#avatar.objects],
			Avatar#avatar{loc=U_Loc,energy=AEnergy-10*PushStrength,objects=U_Objects};
		false ->
			Avatar
	end.

resist({OX,OY},Avatar)->
	PushStrength = 0,
%	{OX,OY} = OAvatar#avatar.loc,
	{X,Y} = Avatar#avatar.loc,
	DX = X-OX,
	DY = Y-OY,
	Distance = math:sqrt(math:pow(OX-X,2)+math:pow(OY-Y,2)),
	Min_Distance = Avatar#avatar.r,
	MinPushX = (Min_Distance/Distance)*DX - DX,
	MinPushY = (Min_Distance/Distance)*DY - DY,
	PushX = MinPushX,%+(DX/abs(DX))*PushStrength,
	PushY = MinPushY,%+(DY/abs(DY))*PushStrength,
	%PushX = MinPushX*PushStrength,
	%PushY = MinPushY*PushStrength,
			
%	NewLoc = {NewX,NewY}={X+PushX,Y+PushY},
%	NewDistance = math:sqrt(math:pow(NewX-OX,2)+math:pow(NewY-OY,2)),
%	io:format("MinPushX:~p MinPushY:~p~n",[MinPushX,MinPushY]),
%	io:format("Pusher:~p Pushee:~p~n Push:~p~n NewLoc:~p~n Distance:~p NewDistance:~p~n",[{OX,OY},{X,Y},{PushX,PushY},NewLoc,Distance,NewDistance]),
	U_Loc = {X+PushX,Y+PushY},
	U_Objects=[{ObjName,Id,Color,{PX+PushX,PY+PushY},[{CX+PushX,CY+PushY}||{CX,CY}<-Coords],P}||{ObjName,Id,Color,{PX,PY},Coords,P}<-Avatar#avatar.objects],
	Avatar#avatar{loc=U_Loc,objects=U_Objects}.

wall_collision(OperatorAvatar,Avatar)->
	%-record(avatar,{id,morphology,type,team,energy,sound,gestalt,spear,age=0,kills=0,loc,direction,r,objects,state,stats,actuators,sensors}).
	{X,Y} = OperatorAvatar#avatar.loc,
	R = OperatorAvatar#avatar.r,
	{WallType,WallParam} = Avatar#avatar.state,
	case WallType of
		x_wall ->
			{WY,WXMin,WXMax} = WallParam,
			case (WY =< (Y+R)) and (WY >= (Y-R)) of
				true ->
					case (WXMin =< X) and (WXMax >= X) of
						true ->
							case Y > WY of
								true ->
									DY = R-(Y-WY),
									U_Loc = {X,Y+DY},
									U_Objects = update_objects(OperatorAvatar#avatar.objects,0,DY),
									OperatorAvatar#avatar{loc = U_Loc,objects=U_Objects};
								false ->
									DY = -R-(Y-WY),
									U_Loc = {X,Y+DY},
									U_Objects = update_objects(OperatorAvatar#avatar.objects,0,DY),
									OperatorAvatar#avatar{loc = U_Loc,objects=U_Objects}
							end;
						false ->
							case X < WXMin of
								true ->
									Distance = math:sqrt(math:pow(X-WXMin,2)+math:pow(Y-WY,2)),
									case Distance < R of
										true ->
											resist({WXMin,WY},OperatorAvatar);
										false ->
											OperatorAvatar
									end;
								false ->
									Distance = math:sqrt(math:pow(X-WXMax,2)+math:pow(Y-WY,2)),
									case Distance < R of
										true ->
											resist({WXMax,WY},OperatorAvatar);
										false ->
											OperatorAvatar
									end
							end
					end;
				false ->
					OperatorAvatar
			end;
		y_wall ->
			{WX,WYMin,WYMax} = WallParam,
			case (WX =< (X+R)) and (WX >= (X-R)) of
				true ->
					case (WYMin =< Y) and (WYMax >= Y) of
						true ->
							case X > WX of
								true ->
									DX = R-(X-WX),
									U_Loc = {X+DX,Y},
									U_Objects = update_objects(OperatorAvatar#avatar.objects,DX,0),
									OperatorAvatar#avatar{loc = U_Loc,objects=U_Objects};
								false ->
									DX = -R-(X-WX),
									U_Loc = {X+DX,Y},
									U_Objects = update_objects(OperatorAvatar#avatar.objects,DX,0),
									OperatorAvatar#avatar{loc = U_Loc,objects=U_Objects}
							end;
						false ->
							case Y < WYMin of
								true ->
									Distance = math:sqrt(math:pow(Y-WYMin,2)+math:pow(X-WX,2)),
									case Distance < R of
										true ->
											resist({WX,WYMin},OperatorAvatar);
										false ->
											OperatorAvatar
									end;
								false ->
									Distance = math:sqrt(math:pow(Y-WYMax,2)+math:pow(X-WX,2)),
									case Distance < R of
										true ->
											resist({WX,WYMax},OperatorAvatar);
										false ->
											OperatorAvatar
									end
							end
					end;
				false ->
					OperatorAvatar
			end
	end.
	
	update_objects(Objects,DX,DY)->
		[{ObjName,Id,Color,{PX+DX,PY+DY},[{CX+DX,CY+DY}||{CX,CY}<-Coords],P}||{ObjName,Id,Color,{PX,PY},Coords,P}<-Objects].
		
wc({X,Y},R,WallType,WallParam)->
	case WallType of
		x_wall ->
			{WY,WXMin,WXMax} = WallParam,
			case (WY =< (Y+R)) and (WY >= (Y-R)) and ((WXMin-R) =< X) and ((WXMax+R)>= X) of
				true ->
					case Y > WY of
						true ->
							io:format("1:~p~n",[{X,WY+R}]);
						false ->
							io:format("2:~p~n",[{X,WY-R}])
					end;
				false ->
					io:format("3:~p~n",[{X,Y}])
			end;
		y_wall ->
			{WX,WYMin,WYMax} = WallParam,
			case (WX =< (X+R)) and (WX >= (X-R)) and ((WYMin-R) =< Y) and ((WYMax+R)>= Y) of
				true ->
					case X > WX of
						true ->
							io:format("4:~p~n",[{WX+R,Y}]);
						false ->
							io:format("5:~p~n",[{WX-R,Y}])
					end;
				false ->
					io:format("6:~p~n",[{X,Y}])
			end
	end.

pt({OX,OY},{X,Y})->
	DX = X-OX,
	DY = Y-OY,
	Distance = math:sqrt(math:pow(DX,2)+math:pow(DY,2)),
	Min_Distance = 1 + 1,
	%Min_Dif=Min_Distance-Distance,
	MinPushX = (Min_Distance/Distance)*DX - DX,
	MinPushY = (Min_Distance/Distance)*DY - DY,
	io:format("MinPushX:~p MinPushY:~p~n",[MinPushX,MinPushY]),
	PushStrength = 1.05,
	PushX = MinPushX*PushStrength,
	PushY = MinPushY*PushStrength,

	NewLoc = {NewX,NewY}={X+PushX,Y+PushY},
	NewDistance = math:sqrt(math:pow(NewX-OX,2)+math:pow(NewY-OY,2)),
	io:format("Pusher:~p Pushee:~p~n Push:~p~n NewLoc:~p~n Distance:~p NewDistance:~p~n",[{OX,OY},{X,Y},{PushX,PushY},NewLoc,Distance,NewDistance]).

gen_id()->
	technome_constructor:generate_UniqueId().
	
create_seeds()->
	[].
	
create_rocks()->
	Rock_Locs = [
		{100,100,40,inf},
		{200,400,20,inf},
		{300,500,20,inf},
		{200,300,60,inf},
		{200,450,15,inf},
		{300,100,50,inf},
	%	{400,500,20,inf},
	%	{450,400,20,inf},
		{1000,400,20,inf}
		%{400,200,100,inf}
	],
	[scape:create_avatar(rock,rock,gen_id(),undefined,Rock_Loc,undefined) || Rock_Loc <- Rock_Locs].

create_pillars()->
	[].

create_walls()->
	Sections =[
		{x_wall,{300,100,200}},
		{y_wall,{250,100,500}},
		{x_wall,{200,400,450}},
		{y_wall,{400,200,300}},
		%{x_wall,{300,400,450}},
		%{x_wall,{250,450,600}},
		%{y_wall,{700,200,400}},
		%{x_wall,{400,450,500}},
		{y_wall,{500,400,500}},
		{x_wall,{500,450,500}}
	],	
	[scape:create_avatar(wall,wall,gen_id(),undefined,Section_Loc,undefined) || Section_Loc <- Sections].
	
create_firepits()->
	FirePits=[
		%{100,100,50,inf},
		%{400,400,50,inf},
		{600,100,50,inf},
		{900,300,50,inf},
		{800,200,50,inf},
		{150,800,50,inf},
		{50,500,50,inf},
		{600,800,50,inf},
		{500,300,50,inf}
	],
	[scape:create_avatar(fire_pit,fire_pit,gen_id(),undefined,FirePit,undefined) || FirePit <- FirePits].
	
create_water()->
	Waters=[
	],
	[scape:create_avatar(water,water,gen_id(),undefined,Water,undefined) || Water <- Waters].
	
create_beacons()->
	Beacons = [
		{500,500,3,inf}
	],
	[scape:create_avatar(beacon,beacon,gen_id(),undefined,Beacon,undefined) || Beacon <- Beacons].


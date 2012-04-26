%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(scape).
-compile(export_all).
%% API
%-export([start_link/1,start_link/0,start/1,start/0,init/2]).
%% gen_server callbacks
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%-record(state, {}).
-behaviour(gen_server).
-include("records.hrl").
-define(NEURAL_COST,100).
-define(PLANT_AGELIMIT,2000).
-define(FL_AGELIMIT,10000).
-define(PLANT_GROWTH,off).
-define(SPAWN_LOC,[{1,[0,0]},{2,[2500,0]},{3,[5000,0]},{4,[5000,2500]},{5,[5000,5000]},{6,[2500,5000]},{7,[0,5000]},{8,[0,2500]}]).
-define(SECTOR_SIZE,10).
-define(SLAVE_ZEROxxx,1).
%%==================================================================== API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Start_Parameters) ->
	gen_server:start_link(?MODULE, Start_Parameters, []).

start(Start_Parameters) -> 
	gen_server:start(?MODULE, Start_Parameters, []).
	
start_link() ->
	gen_server:start_link(?MODULE, [], []).
    
start() -> 
	gen_server:start(?MODULE, [], []).
	
init(Pid,InitState)->
	gen_server:cast(Pid,{init,InitState}).

%%==================================================================== gen_server callbacks
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
%-record(polis,{id,scape_ids=[],population_ids=[],specie_ids=[],dx_ids=[],parameters=[]}).
%-record(scape,{id,scape_id,type,physics,metabolics,avatars=[],plants=[],walls=[],laws=[],anomolies=[],artifacts=[],objects=[],elements=[],atoms=[],borders=[]}).
%-record(avatar,{id,morphology,type,team,energy,sound,gestalt,age=0,kills=0,loc,direction,r,objects,state,stats,actuators,sensors}).
init(Parameters) ->
	{A,B,C} = now(),
	random:seed(A,B,C),
	process_flag(trap_exit,true),
%	io:format("Scape Parameters:~p~n",[Parameters]),
	spawn(scape,heartbeat,[self()]),
	InitState = case Parameters of
		{Polis_PId,Scape_Type,Physics,Metabolics} ->
			Init_Avatars = world:init(Scape_Type,Physics,Metabolics),
%			io:format("InitAvatars:~p~n",[InitAvatars]),
			#scape{id=self(),
				type = Scape_Type,
				avatars = Init_Avatars,
				physics = Physics,
				metabolics = Metabolics
			}
	end,
	{ok, InitState}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({actuator,AvatarType,Command,Output},{From_PId,_Ref},State)->
	%timer:sleep(10),
%	io:format("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX: ~p~n",[now()]),
	{FitnessP,U_State}=case get(From_PId) of
		undefined ->
			io:format("Unregistered Citizen:~p~n",[From_PId]),
			{{0,0},State};
		destroyed ->
			erase(From_PId),
			io:format("Avatar:~p destroyed.~n",[From_PId]),
			{{1,0},State};%TODO:Death Penelty
		_ ->
			Avatars = State#scape.avatars,
			Avatar = lists:keyfind(From_PId, 2, Avatars),
			U_Avatar = scape:Command(Avatar#avatar{kills=0},Output),
		%	io:format("Avatar_Id:~p Energy:~p~n",[U_Avatar#avatar.id,U_Avatar#avatar.energy]),
			case (U_Avatar#avatar.energy > 0) and (U_Avatar#avatar.age < 20000) of
				true ->
					Age = U_Avatar#avatar.age,
					Fitness = case Age > 1000 of
						true ->
							0.001+Avatar#avatar.kills*1;
						false ->
							0.001+Avatar#avatar.kills*1
					end,
				%	io:format("Fitness:~p~n",[Fitness]),
					%io:format("Avatar:~p has age:~p energy:~p and kills:~p~n",[U_Avatar#avatar.id,Age,U_Avatar#avatar.energy,U_Avatar#avatar.kills]),
					{{0,Fitness},State#scape{avatars = collision_detection(U_Avatar,lists:keyreplace(From_PId, 2, Avatars, U_Avatar))}};
				false ->
%					io:format("Avatar:~p Destroyed:~n",[From_PId]),
					io:format("Avatar:~p died at age:~p~n",[U_Avatar#avatar.id,U_Avatar#avatar.age]),
					%io:format("Process info:~p~n",[process_info(self(),[message_queue_len,messages])]),
					{{1,0},destroy_avatar(From_PId,State)}
			end
	end,
	{reply,FitnessP,U_State};
handle_call({multi_agent,update_agents,U_Avatars},{From_PId,_Ref},State)->
	{reply,ok,State#scape{avatars=U_Avatars}};
handle_call({get_all,avatars},{From_PId,_Ref},State)->
	Reply =case get(From_PId) of
		destroyed ->
			destroyed;
		_ ->
			State#scape.avatars
	end,
	{reply,Reply,State};
handle_call(tick,{From_PId,_Ref},State)->
	Avatars = State#scape.avatars,
	U_Avatars = scape:metabolics(Avatars,[]),
	{reply,done,State#scape{avatars = U_Avatars}};
handle_call({enter,Morphology,Specie_Id,CF,CT,TotNeurons},{From_PId,_Ref},State)->
	{Reply,U_State}=case get(From_PId) of
		entered ->
			io:format("Already Registered Citizen:~p~n",[From_PId]),
			{undefined,State};
		undefined ->
			Stats = {CF,CT,TotNeurons},
			Avatars = State#scape.avatars,
			put(From_PId,entered),
%			io:format("Avatar:~p entered~n",[From_PId]),
			Avatar=case get(visor) of
				undefined ->
					scape:create_avatar(Morphology,Specie_Id,From_PId,Stats,void);
				{Visor_PId,Canvas} ->
					visor:draw_avatar(Canvas,scape:create_avatar(Morphology,Specie_Id,From_PId,Stats,void))
			end,
%			io:format("Avatar:~p~n",[Avatar]),
			{done,State#scape{avatars = [Avatar|Avatars]}}
	end,
	{reply,Reply,U_State};
handle_call(leave,{From_PId,_Ref},State)->
	U_State=destroy_avatar(From_PId,State),
%	io:format("Destroyed:~p~n",[From_PId]),
	{reply,done,U_State};
handle_call(get_canvas,{From_PId,_Ref},State)->
	Reply=case get(visor) of
		{_Visor_PId,Canvas}->
			Canvas;
		undefined ->
			undefined
	end,
	{reply,Reply,State};
handle_call({Visor_PId,unsubscribe},{From_PId,_Ref},State)->
	erase(visor),
	{reply,done,State};
handle_call({stop,normal},_From, State)->
	{stop, normal, State};
handle_call({stop,shutdown},_From,State)->
	{stop, shutdown, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({Visor_PId,subscribe,Canvas},State)->
	put(visor,{Visor_PId,Canvas}),
	U_Avatars = visor:draw_avatars(Canvas,State#scape.avatars,[]),
	U_Objects = visor:draw_objects(Canvas,State#scape.objects,[]),
	io:format("Visor:~p subscribed with canvas:~p~n",[Visor_PId,Canvas]),
	{noreply,State#scape{avatars = U_Avatars,objects = U_Objects}};
handle_cast({Visor_PId,redraw,Filter},State)->
	%io:format("~p~n",[now()]),
	case get(visor) of
		undefined ->
			io:format("Scape:~p can't redraw, Visor:~p is not subscribed.~n",[State#scape.type,Visor_PId]);
		{Visor_PId,_Canvas}->
			visor:redraw_avatars(Filter,State#scape.avatars),
			visor:redraw_objects(Filter,State#scape.objects)
	end,
	{noreply,State};
handle_cast({Visor_PId,unsubscribe},State)->
	erase(visor),
	{noreply,State};
handle_cast(tick,State)->
%	Avatars = State#scape.avatars,
%	Scheduler = State#scape.scheduler,
%	io:format("Scape Type:~p Scheduler:~p~n",[State#scape.type,Scheduler]),
%	{U_Avatars,U_Scheduler}=case Scheduler of
%		10 ->
%			%Plant=create_avatar(plant,plant,technome_constructor:generate_UniqueId(),void,no_respawn),
%			Plant=case get(visor) of
%				undefined ->
%					create_avatar(plant,plant,technome_constructor:generate_UniqueId(),void,no_respawn);
%				{Visor_PId,Canvas} ->
%					visor:draw_avatar(Canvas,create_avatar(plant,plant,technome_constructor:generate_UniqueId(),void,no_respawn))
%			end,
%			io:format("Plant:~p~n",[Plant]),
%			{[Plant|scape:metabolics(Avatars,[])],0};
%		_ ->
%			{scape:metabolics(Avatars,[]),Scheduler+1}
%	end,

	Avatars = State#scape.avatars,
	U_Avatars = scape:metabolics(Avatars,[]),
	{noreply,State#scape{avatars = U_Avatars}};%#scape{avatars = U_Avatars,scheduler=U_Scheduler}};
handle_cast({init,InitState},_State)->
	{noreply,InitState};
handle_cast({stop,normal},State)->
	{stop, normal,State};
handle_cast({stop,shutdown},State)->
	{stop, shutdown, State}.
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	io:format("Scape:~p~n Terminating with reason:~p~n",[self(),Reason]),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
heartbeat(Scape_PId)->
	heartbeat(Scape_PId,100,0).
heartbeat(Scape_PId,Tau,Time)->
	receive 
		{update_tau,NewTau}->
			scape:heartbeat(Scape_PId,Tau,Time)
	after Tau ->
		gen_server:cast(Scape_PId,tick),
		scape:heartbeat(Scape_PId,Tau,Time+Tau)
	end.

metabolics([Avatar|Avatars],Acc)->
%	io:format("Avatar:~p~n",[Avatar]),
	case Avatar#avatar.type of
		plant ->
			case Avatar#avatar.state of
				no_respawn->
					case Avatar#avatar.age < 1000 of
						true ->
							U_Avatar=ripen(Avatar),
							metabolics(Avatars,[U_Avatar|Acc]);
						false ->
							case get(visor) of
								undefined->
									done;
								{_Visor_PId,_Canvas} ->
									[gs:destroy(Id) || {_ObjType,Id,_Color,_Pivot,_Coords,_Parameter} <- Avatar#avatar.objects]
							end,
							metabolics(Avatars,Acc)
					end;
				respawn ->
					%RespawnedAvatar = respawn_avatar(Avatar),
					metabolics(Avatars,[Avatar|Acc])
			end;	
		flatlander ->
			Energy = Avatar#avatar.energy,
			%io:format("Flatlander:~p~n",[Energy]),
			U_Avatar=Avatar#avatar{energy = Energy -0.01},
			metabolics(Avatars,[U_Avatar|Acc]);
		prey ->
			Energy = Avatar#avatar.energy,
			%io:format("Prey:~p~n",[Energy]),
			U_Avatar=Avatar#avatar{energy = Energy -0.01},
			metabolics(Avatars,[U_Avatar|Acc]);
		_ ->
			metabolics(Avatars,[Avatar|Acc])
	end;
metabolics([],Acc)->
%	io:format("Time_Marches on, Avatar_Count:~p~n",[length(Acc)]),
	Acc.
	
	ripen(Avatar)->
		%io:format("Here~n"),
		Energy = Avatar#avatar.energy,
		Age = Avatar#avatar.age,
		New_Color=case Energy of
			-2000 -> black;
			-500 -> cyan;
			0 -> grey;
			500 -> green;
			1300 -> yellow;
			1500 -> white;
			_ -> no_change
		end,
		%io:format("Plant:~p~n",[Energy]),
		case New_Color of
			no_change ->
				%io:format("not ripe:~n"),
				Avatar#avatar{energy = functions:saturation(Energy+2,1000),age = Age+1};
			_ ->
				%io:format("ripe~n"),
				U_Energy = functions:saturation(Energy+2,1000),
			     Avatar#avatar{energy=U_Energy,age = Age+1,objects=[{circle,Id,New_Color,Loc,Coords,R}||{circle,Id,_Color,Loc,Coords,R}<-Avatar#avatar.objects]}
		end.
	
new_loc()->
	X = random:uniform(5000),
	Y = random:uniform(5000),
	{X,Y}.
	
new_loc(XMin,XMax,YMin,YMax)->
	X = random:uniform(XMax-XMin)+XMin,
	Y = random:uniform(YMax-YMin)+YMin,
	{X,Y}.
	
remove(Avatar_PId,Avatar_PIdsP,Acc)->
	void.

check_borders(Avatar,[{XMin,XMax},{YMin,YMax}])->
	{X,Y} = Avatar#avatar.loc,
	R = Avatar#avatar.r,
	DX = if
		(X-R) < XMin -> XMin-(X-R);
		(X+R) > XMax -> XMax-(X+R);
		true -> 0
	end,
	DY = if
		(Y-R) < YMin -> YMin-(Y-R);
		(Y+R) > YMax -> YMax-(Y+R);
		true -> 0
	end,
	case {DX,DY} of
		{0,0} ->
			Avatar;
		{DX,DY} ->
			U_Objects=[{ObjName,Id,Color,{PX+DX,PY+DY},[{X+DX,Y+DY}||{X,Y}<-Coords],P}||{ObjName,Id,Color,{PX,PY},Coords,P}<-Avatar#avatar.objects],
			Avatar#avatar{loc = {X+DX,Y+DY},objects = U_Objects}
	end.

collision_detection(OperatorAvatar,Avatars)->
	collision_detection(OperatorAvatar,0,0,Avatars,[]).
collision_detection(OperatorAvatar,EnergyAcc,Kills,[Avatar|Avatars],Acc)->
	%io:format("OAvatar:~p Avatar:~p~n",[OperatorAvatar,Avatar]),
	if
		(Avatar#avatar.id == OperatorAvatar#avatar.id)->
			collision_detection(OperatorAvatar,EnergyAcc,Kills,Avatars,[Avatar|Acc]);
		(Avatar#avatar.type == wall) ->
			U_OperatorAvatar = world:wall_collision(OperatorAvatar,Avatar),
			%See if the avatar pushes against the wall, if so, make sure avatar does not go through the wall.
			%Because the avatar can move at most 1m per step, and the avatar himself is greater than  1m, he will always not have completely passed through the
			%wall, and thus can be moved back 1m or so to prevent from passing through.
			collision_detection(U_OperatorAvatar,EnergyAcc,Kills,Avatars,[Avatar|Acc]);
		%(Avatar#avatar.type == rock1)-> %These are circles, some can be moved, others can not. The amount of energy needed to move the rock is based on color, size.
		%	{U_OperatorAvatar,U_Avatar} = world:rock_collision(OperatorAvatar,Avatar),
		%	collision_detection(U_OperatorAvatar,EnergyAcc,Kills,Avatars,[U_Avatar|Acc]);
		true ->
			{X,Y}= OperatorAvatar#avatar.loc,
			{DX,DY} = OperatorAvatar#avatar.direction,
			{Xav,Yav}=Avatar#avatar.loc,
			Penetration=case (OperatorAvatar#avatar.type == flatlander) or (OperatorAvatar#avatar.spear == true) of
				true ->
					Spear_UnitRay = {DX*math:cos(0) - DY*math:sin(0), DX*math:sin(0) + DY*math:cos(0)},
					{InterDist,_Color}=sensors:shortest_intrLine({{X,Y},Spear_UnitRay},[Avatar],{inf,void}),
					(InterDist =/= -1) and (InterDist < (2 + OperatorAvatar#avatar.r));%TODO, spear length should be avatar defined, not 2.
				false ->
					false
			end,
			Distance = math:sqrt(math:pow(X-Xav,2)+math:pow(Y-Yav,2)),
			Collision = (Distance < (OperatorAvatar#avatar.r+Avatar#avatar.r)),
			{Energy,Order,U_OperatorAvatar,U_Avatar}= case Collision or Penetration of
				true ->
					world:behavior(Collision,Penetration,OperatorAvatar,Avatar);
				false ->
					{0,void,OperatorAvatar,Avatar}
			end,
			case Order of
				destroy ->
					case get(visor) of
						undefined->
							done;
						{_Visor_PId,_Canvas} ->
							[gs:destroy(Id) || {_ObjType,Id,_Color,_Pivot,_Coords,_Parameter} <- U_Avatar#avatar.objects]
					end,
					put(U_Avatar#avatar.id,destroyed),
					collision_detection(U_OperatorAvatar,EnergyAcc+Energy,Kills+1,Avatars,Acc);
				plant_eaten->
					Kill_Score = case Energy > 0 of
						true ->
							1;
						false ->
							0
					end,
					case U_Avatar#avatar.state of
						no_respawn->
							case get(visor) of
								undefined->
									done;
								{_Visor_PId,_Canvas} ->
									[gs:destroy(Id) || {_ObjType,Id,_Color,_Pivot,_Coords,_Parameter} <- U_Avatar#avatar.objects]
							end,
							collision_detection(U_OperatorAvatar,EnergyAcc+Energy,Kills+Kill_Score,Avatars,Acc);
						respawn ->
							RespawnedAvatar = respawn_avatar([Avatar|Avatars]++Acc,U_Avatar),
							collision_detection(U_OperatorAvatar,EnergyAcc+Energy,Kills+Kill_Score,Avatars,[RespawnedAvatar|Acc])
					end;
				poison_eaten->
					case U_Avatar#avatar.state of
						no_respawn->
							case get(visor) of
								undefined->
									done;
								{_Visor_PId,_Canvas} ->
									[gs:destroy(Id) || {_ObjType,Id,_Color,_Pivot,_Coords,_Parameter} <- U_Avatar#avatar.objects]
							end,
							collision_detection(U_OperatorAvatar,EnergyAcc+Energy,Kills,Avatars,Acc);
						respawn ->
							RespawnedAvatar = respawn_avatar([Avatar|Avatars]++Acc,U_Avatar),
							collision_detection(U_OperatorAvatar,EnergyAcc+Energy,Kills,Avatars,[RespawnedAvatar|Acc])
					end;
				void ->
					%io:format("here~n"),
					collision_detection(U_OperatorAvatar,EnergyAcc+Energy,Kills,Avatars,[U_Avatar|Acc])
			end
	end;
collision_detection(OperatorAvatar,EnergyAcc,KillsAcc,[],Acc)->
	case EnergyAcc =/= 0 of
		true ->
			Energy = OperatorAvatar#avatar.energy,
			Kills = OperatorAvatar#avatar.kills,
			U_OperatorAvatar = OperatorAvatar#avatar{energy=functions:saturation(Energy+EnergyAcc,10000), kills = KillsAcc+Kills},
			lists:keyreplace(U_OperatorAvatar#avatar.id, 2, Acc, U_OperatorAvatar);
		false ->
			lists:keyreplace(OperatorAvatar#avatar.id, 2, Acc, OperatorAvatar)
	end.

create_avatar(Morphology)->
	create_avatar(Morphology,Morphology,technome_constructor:generate_UniqueId(),{cf,ct,-1},respawn,undefined).
create_avatar(Morphology,Specie_Id)->
	Stats = {cf,ct,-1},
	create_avatar(Morphology,Specie_Id,technome_constructor:generate_UniqueId(),Stats,respawn,undefined).
create_avatar(Morphology,Specie_Id,Id,Stats,Parameters)->
	create_avatar(Morphology,Specie_Id,Id,Stats,Parameters,undefined).
create_avatar(Morphology,Specie_Id,Id,{CF,CT,TotNeurons},void,InitEnergy) when (Morphology == flatlander)  or (Morphology == prey) or (Morphology == automaton)->
	case Morphology of
		flatlander->
			io:format("Creating Flatlander~n"),
			%{CF,CT,TotNeurons} = Stats,
			Color = red,
			%Color=visor:ct2color(CT),
			Loc = {X,Y} = {random:uniform(500)+900,random:uniform(500)},
			Direction ={DX,DY} = {-1/math:sqrt(2),-1/math:sqrt(2)},
			%Loc = {X,Y} = {random:uniform(round(XMax/2)) + XMax/2 -R,random:uniform(round(YMax/2))+YMax/2 -R},
			Energy =case InitEnergy of
				undefined -> 
					1000;
				InitEnergy ->
					InitEnergy
			end,
			Metabolic_Package = static,
			case Metabolic_Package of
				static ->
					Mass = 6,
					R = 6;
				_ ->
					Mass = 6+Energy/1000,
					R = math:sqrt(Mass*10)
			end,
			Objects = [{circle,undefined,Color,{X,Y},[{X,Y}],R},{line,undefined,red,{X,Y},[{X,Y},{X+DX*R*2,Y+DY*R*2}],void}],
			#avatar{
				id = Id,
				type = Morphology,
				energy = Energy,
				loc = Loc,
				direction = Direction,
				r = R,
				objects = Objects,
				actuators = CF,
				sensors =CT,
				stats = TotNeurons
			};
		prey ->
			%io:format("Creating Prey:~n CF:~p~n CT:~p~n",[CF,CT]),
			%{CF,CT,TotNeurons} = Stats,
			Direction = {DX,DY} = {1/math:sqrt(2),1/math:sqrt(2)},
			{X,Y} = {random:uniform(800),random:uniform(500)},
			Energy =case InitEnergy of
				undefined -> 
					1000;
				InitEnergy ->
					InitEnergy
			end,
			Metabolic_Package = static,
			case Metabolic_Package of
				static ->
					Mass = 10,
					R = 10;
				_ ->
					Mass = 10+Energy/1000,
					R = math:sqrt(Mass*10)
			end,
			case lists:keymember(spear,2,CF) of
				true ->
					Color = red,
					Objects = [{circle,undefined,Color,{X,Y},[{X,Y}],R},{line,undefined,Color,{X,Y},[{X,Y},{X+DX*R*2,Y+DY*R*2}],void}];
				false ->
					Color = blue,
					%Objects = [{circle,undefined,Color,{X+R,Y+R},[{X+R,Y+R}],R}]
					Objects = [{circle,undefined,Color,{X,Y},[{X,Y}],R}]
			end,
			%visor:ct2color(CT),
			#avatar{
				id = Id,
				type = Morphology,
				energy = Energy,
				loc = {X,Y},
				direction = Direction,
				r = R,
				mass = Mass,
				objects = Objects,
				actuators = CF,
				sensors =CT,
				stats = TotNeurons
			};
		automaton ->
			Angle = random:uniform()*2*math:pi(),
			Direction = {DX,DY}={(1/math:sqrt(2))*math:cos(Angle) - (1/math:sqrt(2))*math:sin(Angle),(1/math:sqrt(2))*math:sin(Angle) + (1/math:sqrt(2))*math:cos(Angle)},
			%Direction = {DX,DY} = {1/math:sqrt(2),1/math:sqrt(2)},
			{X,Y} = {400+random:uniform(1120),200+random:uniform(580)},
			Mass = 10,
			R = 10,
			Color = blue,
			Objects = [{circle,undefined,Color,{X,Y},[{X,Y}],R}],
			#avatar{
				id = Id,
				type = Morphology,
				loc = {X,Y},
				direction = Direction,
				r = R,
				mass = Mass,
				objects = Objects
			}
	end;
create_avatar(Morphology,Specie_Id,Avatar_Id,{InitEnergy,InitLoc},RespawnFlag,Metabolics) when (Morphology == plant) or (Morphology == poison) ->
	case Morphology of
		plant ->
			io:format("Creating Plant~n"),
			Direction={1/math:sqrt(2),1/math:sqrt(2)},
			%{X,Y} = Loc = {random:uniform(5000),random:uniform(5000)},
			case InitLoc of
				undefined ->
					{X,Y} = Loc = {random:uniform(800),random:uniform(500)};
				Val ->
					{X,Y} = Loc = Val
			end,
			Energy =case InitEnergy of
				undefined -> 
					500;
				InitEnergy ->
					InitEnergy
			end,
			case Metabolics of
				static ->
					Mass = 3,
					R = 3;
				_ ->
					Mass = 3+Energy/1000,
					R = math:sqrt(Mass*3)
			end,
			%Objects = [{line,undefined,green,{0+X,0+Y},[{-R+X,-R+Y},{R+X,R+Y}],void},{line,undefined,green,{0+X,0+Y},[{-R+X,R+Y},{R+X,-R+Y}],void}],
			Objects = [{circle,undefined,green,{X,Y},[{X,Y}],R}],
			#avatar{
				id = Avatar_Id,
				type = Morphology,
				energy = Energy,
				food = 0,
				health = 0,
				mass = Mass,
				loc = Loc,
				direction = Direction,
				r = R,
				objects = Objects,
				state = RespawnFlag %[respawn| no_respawn]
			};
		poison ->
			io:format("Creating Poison~n"),
			Direction={1/math:sqrt(2),1/math:sqrt(2)},
			case InitLoc of
				undefined ->
					{X,Y} = Loc = {random:uniform(800),random:uniform(500)};
				Val ->
					{X,Y} = Loc = Val
			end,
			Energy =case InitEnergy of
				undefined -> 
					-2000;
				InitEnergy ->
					InitEnergy
			end,
			case Metabolics of
				static ->
					Mass = 3,
					R = 3;
				_ ->
					Mass = 3+abs(Energy)/1000,
					R = math:sqrt(Mass*3)
			end,
			%Objects = [{line,undefined,green,{0+X,0+Y},[{-R+X,-R+Y},{R+X,R+Y}],void},{line,undefined,green,{0+X,0+Y},[{-R+X,R+Y},{R+X,-R+Y}],void}],
			Objects = [{circle,undefined,black,{X,Y},[{X,Y}],R}],
			#avatar{
				id = Avatar_Id,
				type = Morphology,
				energy = Energy,
				loc = Loc,
				direction = Direction,
				r = R,
				objects = Objects,
				state = RespawnFlag
			}
	end;
create_avatar(Morphology,Specie_Id,Id,undefined,Parameters,undefined) when (Morphology == rock) or (Morphology == wall) or (Morphology == fire_pit) or (Morphology == beacon)->
	case Morphology of
		rock ->
			io:format("Creating Rock~n"),
			Direction={1/math:sqrt(2),1/math:sqrt(2)},
			{X,Y,R,Energy} = Parameters,
			Objects = [{circle,undefined,brown,{X,Y},[{X,Y}],R}],
			#avatar{
				id = Id,
				type = Morphology,
				energy = Energy,
				loc = {X,Y},
				direction = Direction,
				r = R,
				objects = Objects
			};
		wall ->
			io:format("Creating Wall~n"),
			case Parameters of
				{x_wall,{Y,XMin,XMax}}->
					YMin = YMax = Y;
				{y_wall,{X,YMin,YMax}}->
					XMin = XMax = X
			end,
			Pivot = {(XMin+XMax)/2,(YMin+YMax)/2},
			Objects = [{line,undefined,brown,Pivot,[{XMin,YMin},{XMax,YMax}],void}],
			#avatar{
				id = Id,
				type = Morphology,
				energy = 10000,
				loc = void,
				direction = void,
				r = void,
				objects = Objects,
				state = Parameters
			};
		pilar ->
			{Loc,R} = Parameters,
			Objects = [{circle,undefined,green,Loc,[Loc],R}],
			#avatar{
				id = Id,
				type = Morphology,
				loc = Loc,
				direction = void,
				r = R,
				objects = Objects,
				state = Parameters
			};
		fire_pit ->
			io:format("Creating FirePit~n"),
			Direction={1/math:sqrt(2),1/math:sqrt(2)},
			{X,Y,R,Energy} = Parameters,
			Objects = [{circle,undefined,red,{X,Y},[{X,Y}],R}],
			#avatar{
				id = Id,
				type = Morphology,
				energy = Energy,
				loc = {X,Y},
				direction = Direction,
				r = R,
				objects = Objects
			};
		beacon ->
			io:format("Creating Beacon~n"),
			Direction={1/math:sqrt(2),1/math:sqrt(2)},
			{X,Y,R,Energy} = Parameters,
			Objects = [{circle,undefined,white,{X,Y},[{X,Y}],R}],
			#avatar{
				id = beacon,
				type = Morphology,
				energy = Energy,
				loc = {X,Y},
				direction = Direction,
				r = R,
				objects = Objects
			}
	end.

destroy_avatar(From_PId,State)->
	Avatars = State#scape.avatars,
	case get(From_PId) of
		undefined ->
			io:format("Destroy Avatar in Scape:: Undefined:~p~n",[From_PId]);
		entered->
			Avatar = lists:keyfind(From_PId, 2, Avatars),
			erase(From_PId),
			case get(visor) of
				undefined->
					done;
				{Visor_PId,_Canvas} ->
					[gs:destroy(Id) || {_ObjType,Id,_Color,_Pivot,_Coords,_Parameter} <- Avatar#avatar.objects]
			end
	end,
	State#scape{avatars = lists:keydelete(From_PId, 2, Avatars)}.

respawn_avatar(A)->
%	io:format("Respawning:~p~n",[A]),
	{X,Y} = {random:uniform(800),random:uniform(500)},
	case A#avatar.type of
		plant ->
			A#avatar{
				loc = {X,Y},
				energy = 500,
				objects = [{circle,Id,green,{X,Y},[{X,Y}],R} ||{circle,Id,_Color,{OldX,OldY},[{OldX,OldY}],R}<-A#avatar.objects]
			};
		poison ->
			A#avatar{
				loc = {X,Y},
				energy = -2000,
				objects = [{circle,Id,black,{X,Y},[{X,Y}],R} ||{circle,Id,_Color,{OldX,OldY},[{OldX,OldY}],R}<-A#avatar.objects]
			}
	end.

respawn_avatar(Avatars,A)->
%	io:format("Respawning:~p~n",[A]),
	%{X,Y} = {random:uniform(800),random:uniform(800)},
	OAvatars = [A || A <- Avatars, (A#avatar.type==rock) or (A#avatar.type==pillar) or (A#avatar.type==fire_pit)],
	{X,Y} = return_valid(OAvatars),
	case A#avatar.type of
		plant ->
			A#avatar{
				loc = {X,Y},
				energy = 500,
				objects = [{circle,Id,green,{X,Y},[{X,Y}],R} ||{circle,Id,_Color,{OldX,OldY},[{OldX,OldY}],R}<-A#avatar.objects]
			};
		poison ->
			A#avatar{
				loc = {X,Y},
				energy = -2000,
				objects = [{circle,Id,black,{X,Y},[{X,Y}],R} ||{circle,Id,_Color,{OldX,OldY},[{OldX,OldY}],R}<-A#avatar.objects]
			}
	end.

	return_valid(OAvatars)->
		case return_valid(OAvatars,{random:uniform(800),random:uniform(500)}) of
			undefined ->
				return_valid(OAvatars,{random:uniform(800),random:uniform(500)});
			Loc ->
				Loc
		end.
	
		return_valid([OA|OAvatars],{X,Y})->
			{Xav,Yav} = OA#avatar.loc,
			Distance = math:sqrt(math:pow(X-Xav,2)+math:pow(Y-Yav,2)),
			Collision = (Distance < (OA#avatar.r+2)),
			case Collision of
				true ->
					undefined;
				false ->
					return_valid(OAvatars,{X,Y})
			end;
		return_valid([],{X,Y})->
			{X,Y}.

move(Avatar,S)->
	{LX,LY} = Avatar#avatar.loc,
	{DX,DY} = Avatar#avatar.direction,
	Speed=case Avatar#avatar.type of
		prey ->
			S;
		_ ->
			S*0.9
	end,
	Energy = Avatar#avatar.energy,
	TotNeurons = Avatar#avatar.stats,
	U_Energy = Energy - 0.1*(math:sqrt(math:pow(DX*Speed,2)+math:pow(DY*Speed,2)))-0.1,%TODO
	%io:format("self():~p Energy burn:~p~n",[self(),abs(Speed)]),
	U_Loc = {LX+(DX*Speed),LY+(DY*Speed)},
	U_Objects=[{ObjName,Id,C,{PX+(DX*Speed),PY+(DY*Speed)},[{X+(DX*Speed),Y+(DY*Speed)}||{X,Y}<-Coords],P}||{ObjName,Id,C,{PX,PY},Coords,P}<-Avatar#avatar.objects],
	Avatar#avatar{energy = U_Energy,loc = U_Loc,objects=U_Objects}.
	
translate(Avatar,{DX,DY})->
	{LX,LY} = Avatar#avatar.loc,
	Energy = Avatar#avatar.energy,
	TotNeurons = Avatar#avatar.stats,
	U_Energy = Energy - 0.1*(math:sqrt(math:pow(DX,2),math:pow(DY,2))) - 0.1,%TODO
	U_Loc = {LX+DX,LY+DY},
	U_Objects=[{ObjName,Id,Color,{PX+DX,PY+DY},[{X+DX,Y+DY}||{X,Y}<-Coords],P}||{ObjName,Id,Color,{PX,PY},Coords,P}<-Avatar#avatar.objects],
	Avatar#avatar{loc = U_Loc,energy = U_Energy,objects=U_Objects}.
	
rotate(Avatar,A)->
	Ratio=math:pi()/4,
	Angle = A*Ratio,%(math:pi()/2),
	{DX,DY} =Avatar#avatar.direction,
	Energy = Avatar#avatar.energy,
	TotNeurons = Avatar#avatar.stats,
	U_Energy = Energy  - 0.1*(abs(Angle))-0.1,%TODO
	U_Direction = {DX*math:cos(Angle) - DY*math:sin(Angle),DX*math:sin(Angle) + DY*math:cos(Angle)},
	U_Objects = rotation(Avatar#avatar.objects,Angle,[]),
	%U_Direction = [DZ,DX*math:sin(Angle) + DY*math:cos(Angle),DX*math:cos(Angle) - DY*math:sin(Angle)],
	Avatar#avatar{energy = U_Energy,direction=U_Direction,objects=U_Objects}.
	
	rotation([Object|Objects],Angle,Acc)->
		{ObjName,Id,Color,{PX,PY},Coords,Parameter} = Object,
		U_Coords = [{(X-PX)*math:cos(Angle) - (Y-PY)*math:sin(Angle) + PX, (X-PX)*math:sin(Angle) + (Y-PY)*math:cos(Angle) +PY} || {X,Y} <- Coords],
		U_Object = {ObjName,Id,Color,{PX,PY},U_Coords,Parameter},
		rotation(Objects,Angle,[U_Object|Acc]);
	rotation([],_Angle,Acc)->
		Acc.

	move_and_rotate(Avatar,[Speed,Angle])->
		Moved_Avatar = move(Avatar,Speed),
		Rotated_Avatar = rotate(Moved_Avatar,Angle).
		
	rotate_and_move(Avatar,[Speed,Angle])->
		Rotated_Avatar = rotate(Avatar,Angle),
		Moved_Avatar = move(Rotated_Avatar,Speed).
		
	rotate_and_translate(Avatar,[Translation,Angle])->
		Rotated_Avatar=rotate(Avatar,Angle),
	Translated_Avatar = translate(Rotated_Avatar,Translation).
	
two_wheels(Avatar,[SWheel1,SWheel2])->
	{Speed,Angle}=twowheel_to_moverotate(SWheel1,SWheel2),
	Rotated_Avatar = rotate(Avatar,Angle),
	Moved_Avatar = move(Rotated_Avatar,Speed),
	AgeAcc = Moved_Avatar#avatar.age,
	Moved_Avatar#avatar{age = AgeAcc+1}.
	
	twowheel_to_moverotate(Wr,Wl)->
		Speed = (Wr + Wl)/2,
		Angle = Wr - Wl,
		{Speed,Angle}.
		
	differential_drive(Wr,Wl)->
		R = 1,
		L = 1,
		differential_drive(R,L,Wr,Wl).
	differential_drive(R,L,Wr,Wl)->
		Uw = (Wr+wl)/2,
		Ua = (Wr-Wl),
		DTheta = (R/L)*Ua,
		Theta = DTheta*1,
		DX = R*Uw*math:cos(Theta),
		DY = R*Uw*math:sin(Theta),
		{DX,DY}.
		
speak(Avatar,[Val])->
%	io:format("Avatar:~p Speak:~p~n",[Avatar#avatar.id,Val]),
	%Energy = Avatar#avatar.energy,
	Avatar#avatar{sound=Val}.

gestalt_output(Avatar,Gestalt)->
	Avatar#avatar{gestalt=Gestalt}.

create_offspring(Avatar,[ExoSelf_PId,CreateVal])->%TODO:incomplete
	case CreateVal > 0 of
		true ->
			OffspringCost = Avatar#avatar.stats*?NEURAL_COST,
			Energy = Avatar#avatar.energy,
			case Energy > (OffspringCost+1000) of
				true ->io:format("Avatar Energy:~p OffspringCost:~p~n",[Energy,OffspringCost]),
					gen_server:cast(ExoSelf_PId,{self(),mutant_clone,granted,Avatar#avatar.id}),
					Avatar#avatar{energy=Energy-(OffspringCost+1000)};
				false ->
					Avatar#avatar{energy=Energy-50}
			end;
		false ->
			Avatar
	end.
	
spear(Avatar,[Val])->%TODO:Incomplete
	case Val > 0 of
		true ->
			%io:format("Spearing now:~p~n",[Val]),
			Energy = Avatar#avatar.energy,
			case Energy > 100 of
				true ->
					Avatar#avatar{energy=Energy-10,spear=true};
				false ->
					Avatar#avatar{energy=Energy-1,spear=false}
			end;
		false ->
			Avatar#avatar{spear=false}
	end.
	
shoot(Avatar,[Val])->%TODO:Incomplete
	case Val > 0 of
		true ->
			Energy = Avatar#avatar.energy,
			case Energy > 100 of
				true ->
					shoot_50,%draw when shot is fired, different color from spear
					Avatar#avatar{energy=Energy-20};
				false ->
					Avatar#avatar{energy=Energy-1}
			end;
		false ->
			Avatar
	end.

%Sectors are 10 by 10.
%[{3,3}] XSec = X div 10, YSec = Y div 10...
put_sector(Coord)->
	void.
get_sector(Coord)->
	void.
loc2sector({X,Y})->
	{trunc(X/?SECTOR_SIZE),trunc(Y/?SECTOR_SIZE)}.
	
command_generator()->
	%quiet time in steps (100-1000)
	%A sequence of commands is generated
	%generated commands are set to sensors, and calcualted fitness sent to actuators
	void.
	
obedience_fitness(Command,Beacon)->
	%Fitness = ImportanceA*CommandObedience - ImportanceB*CollisionAvoidenceFirePit - ImportanceC*CollisionAvoidencePillar - ImportanceC*CollisionAvoidenceWall;
	%1/abs(Command-DrivingDecision)=CommandObedience, CollisionAvoidence = FirePitIntersectionCount, PIllarIntersectionCount, WallIntersectionCount
	%BEACON:
	%distance to beacon, angle to beacon, 1/abs(DistanceFromBeacon-RequestedDistance)
	void.
	
epitopes()->
	spawn(scape,db,[]).
db()->
	ets:file2tab(abc_pred10),
	ets:file2tab(abc_pred12),
	ets:file2tab(abc_pred14),
	ets:file2tab(abc_pred16),
	ets:file2tab(abc_pred18),
	ets:file2tab(abc_pred20),
	receive
		terminate ->
			ok
	end.

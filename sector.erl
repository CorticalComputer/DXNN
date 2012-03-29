%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(sector).
-behaviour(gen_server).
-include("records.hrl").
-define(NEURAL_COST,100).
-define(PLANT_AGELIMIT,2000).
-define(FL_AGELIMIT,10000).
-define(PLANT_GROWTH,off).
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
init({Scape_PId,Sector_Id,Sector_Size,Scape_Type,Physics,Metabolics}) ->
	{A,B,C} = now(),
	random:seed(A,B,C),
	InitState = #sector{
		id=Sector_Id,
		scape_pid = Scape_PId,
		sector_size=Sector_Size,
		type = Scape_Type,
		physics = Physics,
		metabolics = Metabolics},
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
%			U_Avatar=check_borders(scape:Command(Avatar,Output),State#scape.borders),
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
					{{0,Fitness},State#scape{avatars = scape:collision_detection(U_Avatar,lists:keyreplace(From_PId, 2, Avatars, U_Avatar))}};
				false ->
%					io:format("Avatar:~p Destroyed:~n",[From_PId]),
					%Kills = U_Avatar#avatar.kills,
					%Fitness = Kills,
					io:format("Avatar:~p died at age:~p~n",[U_Avatar#avatar.id,U_Avatar#avatar.age]),
					%io:format("Process info:~p~n",[process_info(self(),[message_queue_len,messages])]),
					{{1,0},scape:destroy_avatar(From_PId,State)}
			end
	end,
	{reply,FitnessP,U_State};
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
	U_State=scape:destroy_avatar(From_PId,State),
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

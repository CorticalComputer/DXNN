%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(polis).
%-compile(export_all).
%% API
-export([start_link/1,start_link/0,start/1,start/0,stop/0,init/2,create/0,reset/0,sync/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%-record(state, {}).
-behaviour(gen_server).
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Polis Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(scape_summary,{id,type,name,physics,metabolics}).
-record(state,{active_mods=[],public_scapes=[],private_scapes=[]}).

-define(PRIVATE_SCAPES,[
	#scape_summary{type=pole_balancing,metabolics=static} %Solipsis
]).
-define(PUBLIC_SCAPES,[
	#scape_summary{type=flatland,metabolics=static} %Public Forum
]).

sync()->
	make:all([load]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	case whereis(polis) of
		undefined ->
			gen_server:start(?MODULE, [], []);
		Polis_PId ->
			io:format("Polis is already started, PId:~p~n",[Polis_PId])
	end.
	
stop()->
	case whereis(polis) of
		undefined ->
			io:format("Polis cannot be stopped, it is not online~n");
		Polis_PId ->
			Result = gen_server:cast(Polis_PId,{stop,normal}),
			io:format("Polis stopped, result:~p~n",[Result])
	end.
	
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
init(Active_Mods) ->
	process_flag(trap_exit,true),
	register(polis,self()),
	io:format("Polis Parameters:~p~n",[Active_Mods]),
	{A,B,C} = now(),
	random:seed(A,B,C),
	Public_Scapes = start_scapes(?PUBLIC_SCAPES,[]),
	Private_Scapes =[],
	mnesia:start(),
	start_databases(),
	start_supmods(Active_Mods),
	io:format("******** Polis: ##MATHEMA## is now online.~n"),
	InitState = #state{active_mods=Active_Mods,public_scapes=Public_Scapes,private_scapes=Private_Scapes}, %Scape_PIdsP = [{Scape_PId,Scape_Type}...]
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
handle_call({get_scape,random},{Cx_PId,_Ref},S)->
	Public_Scapes=S#state.public_scapes,
	PS=lists:nth(random:uniform(length(Public_Scapes)),Public_Scapes),
	{reply,{PS#scape_summary.id,PS#scape_summary.type},S};
handle_call({get_scape,ScapeType},{Cx_PId,_Ref},S)->%TODO: Needs to be based on Name and Type(public/private)
	Public_Scapes = S#state.public_scapes,
	io:format("Public_Scapes:~p ScapeType:~p~n",[Public_Scapes,ScapeType]),
	io:format("KeyFind:~p~n",[lists:keyfind(ScapeType,3,Public_Scapes)]),
	Scape_PIdP = case lists:keyfind(ScapeType,3,Public_Scapes) of
		false ->
			undefined;
		PS ->
			{PS#scape_summary.id,PS#scape_summary.type}
	end,
	{reply,Scape_PIdP,S};
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
terminate(Reason, S) ->
	Active_Mods = S#state.active_mods,
	stop_supmods(Active_Mods),
	stop_databases(),
	io:format("******** Polis: ##MATHEMA## is now offline, terminated with reason:~p~n",[Reason]),
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
create()->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(dx,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,dx)}]),
	mnesia:create_table(cortex,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,cortex)}]),
	mnesia:create_table(neuron,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,neuron)}]),
	mnesia:create_table(polis,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,polis)}]),
	mnesia:create_table(population,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,population)}]),
	mnesia:create_table(specie,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,specie)}]),
	%mnesia:create_table(citizen,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,citizen)}]).
	mnesia:create_table(scape,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,scape)}]),
	mnesia:create_table(avatar,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,avatar)}]),
	mnesia:create_table(object,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,object)}]),
	mnesia:create_table(e,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,e)}]),
	mnesia:create_table(a,[{disc_copies, [node()]},{type,set},{attributes, record_info(fields,a)}]).

reset()->
	mnesia:stop(),
	ok = mnesia:delete_schema([node()]),
	polis:create().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Start/Stop %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Start/Stop enviromental modules: DBs, Environments, Network Access systems and tools...
start_supmods([ModName|ActiveMods])->
	ModName:start(),
	start_supmods(ActiveMods);
start_supmods([])->
	benchmark:start(),
	logger:start(),
	done.
		
start_databases()->
%	forex_db:start(),
%	db:heartbeat().
	done.

stop_supmods([ModName|ActiveMods])->
	ModName:stop(),
	stop_supmods(ActiveMods);
stop_supmods([])->
	benchmark:stop(),
	logger:stop(),
	done.
	
stop_databases()->
%	forex_db:stop(),
	case whereis(heartbeat) of
		undefined ->
			done;
		PId ->
			PId ! terminate
	end.
	
start_scapes([S|Scapes],Acc)->
	Type = S#scape_summary.type,
	Physics = S#scape_summary.physics,
	Metabolics = S#scape_summary.metabolics,
	{ok,PId} = scape:start_link({self(),Type,Physics,Metabolics}),
	start_scapes(Scapes,[S#scape_summary{id=PId}|Acc]);
start_scapes([],Acc)->
	lists:reverse(Acc).
	
create_PrivateScape(Scape)->
	{ok,Scape_PId} = scape:start_link({self(),Scape}),
	{Scape_PId,Scape}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Operators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

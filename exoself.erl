%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(exoself).
-include("records.hrl").
%-compile(export_all).
%% API
-export([test/2,start_link/0,start/0,start_link/1,start/1,init/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, g/1]).
%% gen_server support_functions
-export([extract_CurGenNIds/4, extract_NWeightCount/2]).
-record(state, {op_mode,dx,active_nids,ids_n_pids,cx_id,start_time,generation}).
-behaviour(gen_server).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Exoself Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(MAX_TRIALS_TYPE,individual). %[const,individual,population]
-define(ANNEALING,false).
-define(DELTA_MULTIPLIER,math:pi()).
-define(PID_SELECTION_METHOD,dynamic_random). %[dynamic|active|recent|current|all|dynamic_random|active_random|recent_random|current_random|all_random]
%dynamic: Age = 1/random:uniform()
%dynamic_random: Probability of choosing the neuron in the list is 1/math:sqrt(Tot_Neurons_In_The_List).
%active: Last 3 generations.
%active_random:
%recent: CurGen_Neurons + math:sqrt(Tot_Neurons) of latest neurons.
%recent_random: Probability of choosing the neuron in the list is 1/math:sqrt(Tot_Neurons_In_The_List).
%current: CurGen_Neurons
%current_random: Probability of choosing the neuron in the list is 1/math:sqrt(Tot_Neurons_In_The_List).
%all: All neurons in the NN
%all_random:
-define(EVO_STRAT,static). %static|evolving|dynamic
-define(TUNING_MUTATION_RANGE,1).
-define(ANNEALING_PARAMETER,0.5).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==================================================================== API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
test(OpMode,DX_Id) ->
	start_link({OpMode,DX_Id,20}).

start_link(InitOptions) ->
	gen_server:start_link(?MODULE, InitOptions, []).

start(InitOptions) -> 
	gen_server:start(?MODULE, InitOptions, []).
	
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
init([])->
	{ok,[]};
init({OpMode,DX_Id,MT}) ->
	{A,B,C} = now(),
	random:seed(A,B,C),
	[DX] = mnesia:dirty_read({dx,DX_Id}),
	mnesia:dirty_write(DX#dx{mode=online}),
	N_Ids = DX#dx.n_ids,
	Generation = DX#dx.generation,
	Active_NIds = extract_CurGenNIds(N_Ids,Generation,3,[]),
	Tot_ActiveNeuron_Weights = extract_NWeightCount(Active_NIds,0),
	Max_Trials = case ?MAX_TRIALS_TYPE of
		const ->
			10;
		individual ->
			20 + functions:sat(round(math:sqrt(Tot_ActiveNeuron_Weights)),100,0);
		population ->
			MT;
		dx ->
			dx
	end,
	put(max_attempts,Max_Trials),
%	io:format("Max_Trials:~p Active_NIds:~p~n",[Max_Trials,Active_NIds]),
	
	IdsNPids = ets:new(idsNpids,[set,private]),
	spawn_CerebralUnits(DX,IdsNPids),
	link_CerebralUnits(DX,IdsNPids,OpMode,1),
	%activate_CerebralUnits(DX,IdsNPids),
	Cx_Id = DX#dx.cx_id,
	Cx_Pid = ets:lookup_element(IdsNPids,Cx_Id,2),

	%io:format("DX:~p~n",[DX]),
	StartTime = now(),
	InitState = #state{op_mode=OpMode,dx=DX,active_nids=Active_NIds,ids_n_pids=IdsNPids,cx_id=Cx_Id,start_time=StartTime,generation = Generation},
%	io:format("******** ExoSelf:~p started.~n",[self()]),
	put(tot_mutations,0), %Just to keep track of statistics
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
handle_call({weight_mutate},{Cx_PId,_Ref}, S)->
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
	N_Ids = DX#dx.n_ids,
	{PId_Selection_Method,Annealing,TMR,AP} = case ?EVO_STRAT of
		static ->
			{?PID_SELECTION_METHOD,?ANNEALING,?TUNING_MUTATION_RANGE,?ANNEALING_PARAMETER};
		evolving ->
			ANST = (DX#dx.evo_strat)#agent_evo_strat.active_neuron_selection_type,
			TAF = (DX#dx.evo_strat)#agent_evo_strat.tuning_annealing_flag,
			TuningMR = (DX#dx.evo_strat)#agent_evo_strat.tuning_mutation_range,
			AnnealingP = (DX#dx.evo_strat)#agent_evo_strat.annealing_parameter,
			{ANST,TAF,TuningMR,AnnealingP}
	end,
	NIdPs = case PId_Selection_Method of
		dynamic ->
			ChosenN_IdPs = case extract_CurGenNIdPs(N_Ids,S#state.generation,math:sqrt(1/random:uniform()),TMR,AP,[]) of
				[] ->
					[N_Id|_] = N_Ids,
					[{N_Id,?DELTA_MULTIPLIER}];
				ExtractedN_IdPs->
					ExtractedN_IdPs
			end,
			%io:format("ChosenN_IdPs:~p~n",[ChosenN_IdPs]),
			ChosenN_IdPs;
		dynamic_random ->
			ChosenN_IdPs = case extract_CurGenNIdPs(N_Ids,S#state.generation,math:sqrt(1/random:uniform()),TMR,AP,[]) of
				[] ->
					[N_Id|_] = N_Ids,
					[{N_Id,?DELTA_MULTIPLIER}];
				ExtractedN_IdPs->
					ExtractedN_IdPs
			end,
			%io:format("ChosenN_IdPs:~p~n",[ChosenN_IdPs]),
			Tot_Neurons = length(ChosenN_IdPs),
			MutationP = 1/math:sqrt(Tot_Neurons),
			choose_randomNIdPs(MutationP,ChosenN_IdPs);
		active ->
			extract_CurGenNIdPs(N_Ids,S#state.generation,3,TMR,AP,[]);
		active_random ->
			ChosenN_IdPs = case extract_CurGenNIdPs(N_Ids,S#state.generation,3,TMR,AP,[]) of
				[] ->
					[N_Id|_] = N_Ids,
					[{N_Id,?DELTA_MULTIPLIER}];
				ExtractedN_IdPs->
					ExtractedN_IdPs
			end,
			Tot_Neurons = length(ChosenN_IdPs),
			MutationP = 1/math:sqrt(Tot_Neurons),
			choose_randomNIdPs(MutationP,ChosenN_IdPs);
		current ->
			case extract_CurGenNIdPs(N_Ids,S#state.generation,0,TMR,AP,[]) of
				[] ->
					[N_Id|_] = N_Ids,
					[{N_Id,?DELTA_MULTIPLIER}];
				IdPs ->
					IdPs
			end;
		current_random ->
			ChosenN_IdPs = case extract_CurGenNIdPs(N_Ids,S#state.generation,0,TMR,AP,[]) of
				[] ->
					[N_Id|_] = N_Ids,
					[{N_Id,?DELTA_MULTIPLIER}];
				IdPs ->
					IdPs
			end,
			Tot_Neurons = length(ChosenN_IdPs),
			MutationP = 1/math:sqrt(Tot_Neurons),
			choose_randomNIdPs(MutationP,ChosenN_IdPs);
		all ->
			case extract_CurGenNIdPs(N_Ids,S#state.generation,S#state.generation,TMR,AP,[]) of
				[] ->
					[N_Id|_] = N_Ids,
					[{N_Id,?DELTA_MULTIPLIER}];
				IdPs ->
					IdPs
			end;
		all_random ->
			ChosenN_IdPs = case extract_CurGenNIdPs(N_Ids,S#state.generation,S#state.generation,TMR,AP,[]) of
				[] ->
					[N_Id|_] = N_Ids,
					[{N_Id,?DELTA_MULTIPLIER}];
				IdPs ->
					IdPs
			end,
			Tot_Neurons = length(ChosenN_IdPs),
			MutationP = 1/math:sqrt(Tot_Neurons),
			choose_randomNIdPs(MutationP,ChosenN_IdPs)
	end,
	%io:format("NIdPs:~p~n",[NIdPs]),
	%NPIdPs = [{ets:lookup_element(IdsNPids,NId,2),Spread} || {NId,Spread} <- NIdPs],
	case Annealing of
		true ->		
			[ets:lookup_element(IdsNPids,NId,2) ! {self(),gt,weight_mutate,Spread} || {NId,Spread} <- NIdPs];
		false ->
			[ets:lookup_element(IdsNPids,NId,2) ! {self(),gt,weight_mutate,?DELTA_MULTIPLIER} || {NId,_Spread} <- NIdPs]
	end,
	%io:format("N_Ids:~p NPIdPs:~p~n",[N_Ids,NPIdPs]),
	%put(prev_RandPIds,NPIdPs),
	Tot_Mutations = get(tot_mutations),
	put(tot_mutations,Tot_Mutations+1),
	{reply, done,S};

handle_call({weight_save},{Cx_PId,_Ref},S)->
	%case SCType of
	DX=S#state.dx,
	N_Ids = DX#dx.n_ids,
	IdsNPids = S#state.ids_n_pids,
	[ets:lookup_element(IdsNPids,NId,2) ! {self(),gt,weight_save} || NId <- DX#dx.n_ids],
	{reply,done,S};
	
handle_call({weight_revert},{Cx_PId,_Ref}, S)->
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
%	io:format("DX:~p~n",[DX]),
	Summary = DX#dx.summary,
	[ets:lookup_element(IdsNPids,NId,2) ! {self(),gt,weight_revert} || NId <- DX#dx.n_ids],
%	io:format("Weight reverting.~n"),
%	Cx_Id = DX#dx.cx_id,
%	Cx_PId = ets:lookup_element(IdsNPids,Cx_Id,2),
%	Cx_PId ! {self(), revert_IProfile},
%	receive
%		{Cx_PId,ready}->
%			done
%	end,
	{reply, done, S};

handle_call({backup_request},{Cx_PId,_Ref},S)->
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
	%CurGenN_Ids = S#state.active_nids,
	
	backup_DX(DX,IdsNPids),
%	io:format("Backup of DX: ~p completed.~n",[DX#dx.id]),
	{reply, done, S};

handle_call(memory_reset,{Cx_PId,_Ref},S)->
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
	NeuroIds = DX#dx.n_ids,
	Cx_Id = DX#dx.cx_id,
	[ets:lookup_element(IdsNPids,N_Id,2) ! {self(), memory_reset} || N_Id <- NeuroIds],
	gather_acks(length(NeuroIds)),
	[ets:lookup_element(IdsNPids,N_Id,2) ! {self(), reset} || N_Id <- NeuroIds],
%	io:format("Reseting Memory~n"),
	{reply, done, S};

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
handle_cast({Cx_Pid,tpt,success},S)->
	OpMode = S#state.op_mode,
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
	StartTime = S#state.start_time,
	io:format("Throughput Test completed in: ~p us.~n",[integer_to_list(timer:now_diff(now(),StartTime))]),
	{stop,normal,{OpMode,DX,IdsNPids}};

handle_cast({Cx_Pid,fitness,{FitnessType,Fitness,Goal_Status}},S)->
	OpMode = S#state.op_mode,
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
	StartTime = S#state.start_time,
	{Fitness,Profile} = technome_constructor:construct_FitnessProfile(DX,Fitness),
	%[DX] = mnesia:dirty_read({dx,DX_Id}),
	mnesia:dirty_write(DX#dx{
		fitness = Fitness,
		profile = Profile
	}),
	TimeElapsed = integer_to_list(timer:now_diff(now(),StartTime)),
	io:format("DX_Id:~p::~n OpMode:~p~n TimeElapsed:~pus~n FType:~p~n Fitness:~p~n FProfile:~p~n",[DX#dx.id,OpMode,TimeElapsed,FitnessType,Fitness,Profile]),
	%io:format("Id:~p Fitness:~p Fitness_Profile:~p~n",[DX_Id,Fitness,Fitness_Profile]),	
	{stop,normal,{OpMode,DX,IdsNPids,{FitnessType,Fitness,Profile}}};

handle_cast({Cx_Pid,championship_fitness,{FitnessType,Fitness,Goal_Status}},S)->
	OpMode = S#state.op_mode,
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
	StartTime = S#state.start_time,
	{TrueFitness,FitnessProfile} = technome_constructor:construct_FitnessProfile(DX,Fitness),
	TimeElapsed = integer_to_list(timer:now_diff(now(),StartTime)),
	io:format("DX_Id:~p::~n OpMode:~p~n TimeElapsed:~pus~n FType:~p~n Fitness:~p~n FProfile:~p~n",[DX#dx.id,OpMode,TimeElapsed,FitnessType,TrueFitness,FitnessProfile]),
	{stop,normal,{OpMode,DX,IdsNPids,{FitnessType,TrueFitness,FitnessProfile}}};

handle_cast({Cx_Pid,benchmark_fitness,{FitnessType,Fitness,Goal_Status}},S)->
	OpMode = S#state.op_mode,
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
	StartTime = S#state.start_time,
	{TrueFitness,FitnessProfile} = technome_constructor:construct_FitnessProfile(DX,Fitness),
	TimeElapsed = integer_to_list(timer:now_diff(now(),StartTime)),
	io:format("DX_Id:~p::~n OpMode:~p~n TimeElapsed:~pus~n FType:~p~n Fitness:~p~n FProfile:~p~n",[DX#dx.id,OpMode,TimeElapsed,FitnessType,TrueFitness,FitnessProfile]),
	benchmark ! {DX#dx.id,TrueFitness,FitnessProfile},
	{stop,normal,{OpMode,DX,IdsNPids,{FitnessType,TrueFitness,FitnessProfile}}};
	
handle_cast({_Scape_PId,mutant_clone,granted,_DX_PId},S)->
	DX=S#state.dx,
	DX_Id = DX#dx.id,
	gen_server:cast(monitor,{request,offspring,DX_Id}),
	{noreply, S};
handle_cast({_From,terminate},S)->
	DX=S#state.dx,
	IdsNPIds = S#state.ids_n_pids,
	Cx_PId = ets:lookup_element(IdsNPIds,S#state.cx_id,2),
	Cx_PId ! {self(),terminate},
	{stop,normal,{terminate,DX,IdsNPIds}};
	
handle_cast({stop,shutdown},S)->
	OpMode = S#state.op_mode, 
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
%	io:format("~p Shut_Down after: ~p us.~n",[OpMode,integer_to_list(timer:now_diff(now(),StartTime))]),
	{stop,shutdown,{OpMode,DX,IdsNPids}};
	
handle_cast({stop,normal},S)->
	OpMode = S#state.op_mode, 
	DX=S#state.dx,
	IdsNPids = S#state.ids_n_pids,
%	io:format("~p Normal_End after: ~p us.~n",[OpMode,integer_to_list(timer:now_diff(now(),StartTime))]),
	{stop, normal, {OpMode,DX,IdsNPids}}.
	
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
%	io:format("State:~p~n",[State]),
	case State of
		{tpt,DX,IdsNPids}->
			deactivate_DX(DX,IdsNPids),
			ets:delete(IdsNPids);
		{OpMode,DX,IdsNPids,{FitnessType,Fitness,Fitness_Profile}} -> 
			deactivate_DX(DX,IdsNPids),
			ets:delete(IdsNPids),
			%timer:sleep(1000),
			DX_Id = DX#dx.id,
			case OpMode of
				championship ->
					case DX#dx.morphology of
						flatlander ->
							gen_server:cast(monitor,{DX_Id,champion_terminated,Fitness,Fitness_Profile});
						prey ->
							gen_server:cast(monitor,{DX_Id,champion_terminated,Fitness,Fitness_Profile});
						_ ->
							gen_server:cast(monitor,{DX_Id,champion_terminated,Fitness,Fitness_Profile})
							%io:format("******** ExoSelf: ~p terminated with reason: ~p::OpMode:~p DX_Id:~p::~n",[self(),Reason,OpMode,DX_Id]),
					end;
				benchmark ->
					void;
				_ ->
					case DX#dx.morphology of
						flatlander ->
							gen_server:cast(monitor,{DX_Id,terminated,Fitness,Fitness_Profile});
						prey ->
							gen_server:cast(monitor,{DX_Id,terminated,Fitness,Fitness_Profile});
						_ ->
							gen_server:cast(monitor,{DX_Id,terminated,Fitness,Fitness_Profile})
							%io:format("******** ExoSelf: ~p terminated with reason: ~p::OpMode:~p DX_Id:~p::~n",[self(),Reason,OpMode,DX_Id]),
					end
			end,
			[Current_DX] = mnesia:dirty_read({dx,DX_Id}),
			mnesia:dirty_write(Current_DX#dx{mode=offline});
		{terminate,DX,IdsNPIds} ->
			deactivate_DX(DX,IdsNPIds),
			ets:delete(IdsNPIds);
		{_OpMode,DX,IdsNPids}->
			deactivate_DX(DX,IdsNPids),
			ets:delete(IdsNPids);
		OTHER ->
			io:format("Severe crash with state:~p~n",[OTHER])
	end,
	io:format("******** ExoSelf: ~p terminated with reason: ~p~n",[self(),Reason]),
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
%==================================================================Summoning Functions==================================================================
spawn_CerebralUnits(DX,IdsNPids)->
	Cx_Id = DX#dx.cx_id,
	[Cx] = mnesia:dirty_read({cortex,Cx_Id}),
	spawn_CerebralUnit(IdsNPids,neuron,DX#dx.n_ids),
	spawn_CerebralUnit(IdsNPids,cortex,[DX#dx.cx_id]),
	ets:insert(IdsNPids,{threshold,threshold}).
	
	spawn_CerebralUnit(IdsNPids,CerebralUnitType,[Id|Ids])->
		{ok,Pid} = CerebralUnitType:gen(self(),node()),
		ets:insert(IdsNPids,{Id,Pid}),
		ets:insert(IdsNPids,{Pid,Id}),
		%io:format("Spawn_Neurons::N_Id:~p, N_PId:~p~n",[N_Id,N_Pid]),
		spawn_CerebralUnit(IdsNPids,CerebralUnitType,Ids);
	spawn_CerebralUnit(_IdsNPids,_CerebralUnitType,[])->
		true.
	
link_CerebralUnits(DX,IdsNPids,OpMode,Smoothness)->
	Cx_Id = DX#dx.cx_id,
	[Cx] = mnesia:dirty_read({cortex,Cx_Id}),
	link_Neurons(IdsNPids,DX#dx.n_ids),
	link_Cortex(IdsNPids,DX#dx.cx_id,OpMode,Smoothness).

	%-record(neuron,{id,ivl,i,ovl,o,lt,ro,type,dwp,su_id,generation}). %%%id = {{LayerIndex,NumId},neuron}
	link_Neurons(IdsNPids,[N_Id|N_Ids])->
		[N] = mnesia:dirty_read({neuron,N_Id}),
		%io:format("~p~n",[N]),
		N_PId = ets:lookup_element(IdsNPids,N_Id,2),
		I_PIds = [{ets:lookup_element(IdsNPids,I_Id,2),IVL}|| {I_Id,IVL} <- N#neuron.i], %I from Cortex xor Subcore or Neurons
		O_PIds = [ets:lookup_element(IdsNPids,O_Id,2)|| O_Id <- N#neuron.o], %O to Cortex xor Subcore or Neurons
		RO_PIds =[ets:lookup_element(IdsNPids,RO_Id,2)|| RO_Id <- N#neuron.ro],
		NDWP = case N#neuron.type of
			standard ->
				[{ets:lookup_element(IdsNPids,Id,2),WPC} || {Id,WPC} <- N#neuron.dwp];
			bst ->
				N#neuron.dwp
		end,
		%io:format("I_PIds:~p~n NDWP:~p~n",[I_PIds,NDWP]),
		TotIVL = N#neuron.ivl,
		TotOVL = N#neuron.ovl,
		LearningType = N#neuron.lt,
		SU_Id = N#neuron.su_id,
		State = {self(),N_Id,SU_Id,TotIVL,I_PIds,TotOVL,O_PIds,LearningType,RO_PIds,NDWP},
		N_PId ! {self(),init,State},
		link_Neurons(IdsNPids,N_Ids);
	link_Neurons(_IdsNPids,[])->
		true.

	link_Cortex(IdsNPids,Cx_Id,OpMode,Smoothness)->
		[Cx] = mnesia:dirty_read({cortex,Cx_Id}),
		Cx_Pid = ets:lookup_element(IdsNPids,Cx_Id,2),
		Cx_Type = Cx#cortex.type,
		Cx_Plascity = Cx#cortex.plasticity,
		case Cx#cortex.type of
			neural ->
				Sensors = [Sensor|| {Sensor,_NIdPs} <- Cx#cortex.ct],
				Actuators= [Actuator||{Actuator,_NIds} <-Cx#cortex.cf];
			hypercube ->
				Sensors = Cx#cortex.sensors,
				Actuators = Cx#cortex.actuators
		end,
		Cx_CF = convert_Ids2PIds(IdsNPids,Cx#cortex.cf,[]),
		Cx_CT = convert_Ids2PIds(IdsNPids,Cx#cortex.ct,[]),
		Dimensions = Cx#cortex.dimensions,
		Densities = Cx#cortex.densities,
%		io:format("~p~n",[Cx]),
		[DX] = mnesia:dirty_read({dx,Cx#cortex.su_id}),
		Morphology = DX#dx.morphology,
		Specie_Id = DX#dx.specie_id,
		State={self(),Cx_Id,Sensors,Actuators,Cx_CF,Cx_CT,get(max_attempts),Smoothness,OpMode,Cx_Type,Cx_Plascity,Morphology,Specie_Id,length(Cx#cortex.cids),Dimensions,Densities,Cx#cortex.substrate_link_form},
		Cx_Pid ! {self(),init,State},
		true.

		convert_Ids2PIds(TableName,[{R,Ids}|LinkList],Acc) when is_record(R,actuator) -> %Neural: CF
			convert_Ids2PIds(TableName,LinkList,[{R,[ets:lookup_element(TableName,Id,2)|| Id<-Ids]}|Acc]);
		convert_Ids2PIds(TableName,[{R,IdPs}|LinkList],Acc) when is_record(R,sensor) ->%Neural: CT 
			convert_Ids2PIds(TableName,LinkList,[{R,[{ets:lookup_element(TableName,Id,2),Tag}|| {Id,Tag}<-IdPs]}|Acc]);
		convert_Ids2PIds(TableName,[{R,Ids}|LinkList],Acc) when is_record(R,sCF)->%Hypercube: CF
			convert_Ids2PIds(TableName,LinkList,[{R,[ets:lookup_element(TableName,Id,2)|| Id<-Ids]}|Acc]);
		convert_Ids2PIds(TableName,[{R,IdPs}|LinkList],Acc) when is_record(R,sCT)->%Hypercube: CT 
			convert_Ids2PIds(TableName,LinkList,[{R,[{ets:lookup_element(TableName,Id,2),Tag}|| {Id,Tag}<-IdPs]}|Acc]);
		convert_Ids2PIds(_TableName,[],Acc)->
			lists:reverse(Acc).

deactivate_DX(DX,IdsNPids)->
	%[DX] = mnesia:dirty_read({dx,DX_Id}),
	Cx_Id = DX#dx.cx_id,
	[Cx] = mnesia:dirty_read({cortex,Cx_Id}),
	[ets:lookup_element(IdsNPids,Id,2) ! {self(),terminate} || Id <- DX#dx.n_ids],
	[ets:lookup_element(IdsNPids,Id,2) ! {self(),terminate} || Id <- [DX#dx.cx_id]].
	
backup_DX(DX,IdsNPids) ->
%	io:format("Backing up DX~n"),
	backup_Neurons(IdsNPids,DX#dx.n_ids),
	done.
		
	backup_Neurons(IdsNPids,[N_Id|N_Ids])->
		N_PId = ets:lookup_element(IdsNPids,N_Id,2),
		N_PId ! {self(),get_backup},
		receive
			{N_PId,backup,{IList,O_PIds,RO_PIds,LT,NDWP}} ->
				[N] = mnesia:dirty_read({neuron,N_Id}),
				I = [{ets:lookup_element(IdsNPids,I_PId,2),IVL}|| {I_PId,IVL} <- IList], %I from Cortex xor Subcore or Neurons
				O = [ets:lookup_element(IdsNPids,O_PId,2)|| O_PId <- O_PIds], %O to Cortex xor Subcore or Neurons
				RO =[ets:lookup_element(IdsNPids,RO_PId,2)|| RO_PId <- RO_PIds],
				DWP = case N#neuron.type of
					standard ->
						[{ets:lookup_element(IdsNPids,PId,2),WPC} || {PId,WPC} <- NDWP];
					bst ->
						NDWP
				end,
				mnesia:dirty_write(neuron,N#neuron{
					i = I,
					o = O,
					ro = RO,
					lt = LT,
					dwp = DWP
				})
		end,
		%io:format("Backing up: N_Id:~p DWP:~p~n",[N_Id,DWP]),
		backup_Neurons(IdsNPids,N_Ids);
	backup_Neurons(_IdsNPids,[])->
		done.

choose_randomNIdPs(MutationP,N_IdPs)->
	case choose_randomNIdPs(N_IdPs,MutationP,[]) of
		[] ->
			{NId,Spread} = lists:nth(random:uniform(length(N_IdPs)),N_IdPs),
			[{NId,Spread}];
		Acc ->
			Acc
	end.

	choose_randomNIdPs([{NId,Spread}|N_IdPs],MutationP,Acc)->
		U_Acc = case random:uniform() < MutationP of
			true ->
				[{NId,Spread}|Acc];
			false ->
				Acc
		end,
		choose_randomNIdPs(N_IdPs,MutationP,U_Acc);
	choose_randomNIdPs([],_MutationP,Acc)->
		Acc.
	
gather_acks(0)->
	done;	
gather_acks(PId_Index)->
	receive
		{_From,ready}->
			%io:format("From:~p~n",[_From]),
			gather_acks(PId_Index-1)
		after 100000 ->
			io:format("******** Not all acks received:~p~n",[PId_Index])
	end.
	
	count_weights({_,_},Acc)->
		count_weights([],1);
	count_weights([{_Id,WPC}|DWP],Acc)->
		count_weights(DWP,length(WPC)+Acc);
	count_weights([],Acc)->
		Acc.
		
get_MaxTrials()->
	{reply, Max_Trials} = gen_server:call(monitor,{request,max_trials}),
	Max_Trials.
	
extract_CurGenNIds([N_Id|N_Ids],Generation,Acc)->
	[N] = mnesia:dirty_read({neuron,N_Id}),
	NeuronGen = N#neuron.generation,
	case NeuronGen== Generation of
		true ->
			extract_CurGenNIds(N_Ids,Generation,[N_Id|Acc]);
		false ->
			extract_CurGenNIds(N_Ids,Generation,Acc)
	end;
extract_CurGenNIds([],_Generation,Acc)->
	Acc.

extract_CurGenNIds([N_Id|N_Ids],Generation,AgeLimit,Acc)->
	[N] = mnesia:dirty_read({neuron,N_Id}),
	NeuronGen = N#neuron.generation,
	case NeuronGen >= (Generation-AgeLimit) of
		true ->
			extract_CurGenNIds(N_Ids,Generation,AgeLimit,[N_Id|Acc]);
		false ->
			extract_CurGenNIds(N_Ids,Generation,AgeLimit,Acc)
	end;
extract_CurGenNIds([],_Generation,_AgeLimit,Acc)->
	Acc.
	
extract_NWeightCount([N_Id|CurGenN_Ids],Acc)->
	[N] = mnesia:dirty_read({neuron,N_Id}),
	DWP = N#neuron.dwp,
	TotWeights = count_weights(DWP,0),
	extract_NWeightCount(CurGenN_Ids,TotWeights+Acc);
extract_NWeightCount([],Acc)->
	Acc.
	
extract_CurGenNIdPs([N_Id|N_Ids],Generation,TMR,AP,Acc)->
	[N] = mnesia:dirty_read({neuron,N_Id}),
	NeuronGen = N#neuron.generation,
	case NeuronGen == Generation of
		true ->
			Age = Generation-NeuronGen,
			Spread = TMR*math:pi()*math:pow(AP,Age),%math:pi()*math:pow(0.5,Age),
			extract_CurGenNIdPs(N_Ids,Generation,TMR,AP,[{N_Id,Spread}|Acc]);
		false ->
			extract_CurGenNIdPs(N_Ids,Generation,TMR,AP,Acc)
	end;
extract_CurGenNIdPs([],_Generation,_TMR,_AP,Acc)->
	Acc.

extract_CurGenNIdPs([N_Id|N_Ids],Generation,AgeLimit,TMR,AP,Acc)->
	[N] = mnesia:dirty_read({neuron,N_Id}),
	NeuronGen = N#neuron.generation,
	case NeuronGen >= (Generation-AgeLimit) of
		true ->
			Age = Generation-NeuronGen,
			Spread = TMR*math:pi()*math:pow(AP,Age),%math:pi()*math:pow(0.5,Age),
			extract_CurGenNIdPs(N_Ids,Generation,AgeLimit,TMR,AP,[{N_Id,Spread}|Acc]);
		false ->
			extract_CurGenNIdPs(N_Ids,Generation,AgeLimit,TMR,AP,Acc)
	end;
extract_CurGenNIdPs([],_Generation,_AgeLimit,_TMR,_AP,Acc)->
	Acc.

g(Text)->gather_urls(Text,[]).
gather_urls("arc" ++ T, L) ->
	{Url, T1} = collect_url_body(T, lists:reverse("arc" )),
	gather_urls(T1, [Url|L]);
gather_urls([_|T], L) ->
	gather_urls(T, L);
gather_urls([], L) ->
	L.
	
	collect_url_body("." ++ T, L) -> 
		{lists:reverse(L, "." ), T};
	collect_url_body([H|T], L)-> 
		collect_url_body(T, [H|L]);
	collect_url_body([], _)-> 
		{[],[]}.

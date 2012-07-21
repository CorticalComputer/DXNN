%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(population_monitor).
-include("forex_db.hrl").
-include("records.hrl").
%-compile(export_all).
%% API
-export([start_link/1,start_link/0,start/1,start/0,stop/0,init/2,init_population/1,init_population/2,continue/2,extract_DXIds/2,delete_population/1,championship/0,champion/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,create_MutantDXCopy/1,test/0,new/0,reset/0,backup/0,info/1]).

%-record(state, {}).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Population Monitor Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SURVIVAL_TYPE,competition). %competition,pl_competition,simple_ranking,speciated_ranking,forced_speciation
%competition: The survival and replication is purely based on the fitness, which determines the energy left for node creation. If it's above the creator's total nodes,
%	a new mutated copy can be made, if it is less than 50%, then the entity dies, if it is >50% but <100%, 50% of the energy requirenment for creation is subtracted.
%	The cost of each Neuron is based on the average fitness from the first generation from the population of all the single Neuron individuals.
%pl_competition: Same as competition but the cost for each neuron is based on the population limit and the average number of Neurons, weighted with the fitness.
%simple_ranking: The fitnesses are ranked from greatest to smallest, top 3 are chosen, and then based on their ration of fitness to to fitness  creation is allowed.
%speciated_ranking: First we choose 3 of top, topologically different individuals, and then produce a specific number of mutants for each individual.
%forced_speciation: We diffirintiate everyone into their own topological group. Then within each group the individuals are ranked, and some number of top individuals are 
%	allowed to create mutant coppies, if the specie has not produced any improvements in some number of generations, it is removed.
%-define(TOT_INIT_SPECIES,1).
%Morphologies:pole2_balancing3,prey,forex_trader, xor_mimic
-define(EFF,0.05). %Efficiency., TODO: this should further be changed from absolute number of neurons, to diff in lowest or avg, and the highest number of neurons
-define(INIT_CONSTRAINTS,[#constraint{morphology=Morphology,sc_types=SC_Types, sc_neural_plasticity=[none], sc_hypercube_plasticity=[none],sc_hypercube_linkform = Substrate_LinkForm,sc_neural_linkform=LinkForm}|| Morphology<-[epiwalker],Substrate_LinkForm <- [[feedforward]], LinkForm<-[recursive],SC_Types<-[[neural]]]).
-define(SURVIVAL_PERCENTAGE,0.5).
-define(SPECIE_SIZE_LIMIT,10).
-define(INIT_SPECIE_SIZE,10).
%-define(POPULATION_LIMIT,?SPECIE_SIZE_LIMIT*length(?INIT_MORPHOLOGIES)).
-define(INIT_POPULATION_ID,test).
%-define(INIT_ARCHITECTURE_TYPE,modular).
%-define(INIT_LINK_FORM,recursive).
-define(OP_MODES,[gt,benchmark]).
-define(INIT_POLIS,mathema).
-define(GENERATION_LIMIT,1000).
-define(EVALUATIONS_LIMIT,10000).
-define(DIVERSITY_COUNT_STEP,500).
-define(GEN_UID,technome_constructor:generate_UniqueId()).
-define(CHAMPION_COUNT_STEP,500).
-record(state,{op_mode,population_id,activeDX_IdPs,inactiveDX_Ids,dx_ids,tot_individuals,individuals_left,op_tag,dx_summaries=[],attempt=0,evaluations_acc=0,step_size,next_step,goal_status,survival_type}).
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

stop() ->
	gen_server:cast(monitor,{stop,normal}).
	
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
%-record(population,{id,avg_fitness,type,specie_ids,topspecie_ids,polis_id}).
%-record(specie,{id,avg_fitness,stagnation_factor,dx_ids,topdx_ids,championdx_ids,population_id}).
init(Parameters) ->
	process_flag(trap_exit,true),
	register(monitor,self()),
	io:format("******** Population monitor started with parameters:~p~n",[Parameters]),
	State = case Parameters of
		{championship,Population_Id}->
			ChampionDX_Ids = extract_DXIds(Population_Id,champions),
			put(max_trials,100),
			ActiveDX_IdPs = summon_dxs(championship,ChampionDX_Ids),
			io:format("ChampDXIds:~p~n",[ChampionDX_Ids]),
			#state{op_mode=championship,
				population_id = Population_Id,
				activeDX_IdPs = ActiveDX_IdPs,
				tot_individuals = length(ChampionDX_Ids),
				individuals_left = length(ChampionDX_Ids),
				op_tag = continue};
		{champion,Population_Id}->
			[TopChampionDX_Id|_ChampionDX_Ids] = extract_DXIds(Population_Id,champions),
			put(max_trials,100),
			ActiveDX_IdPs = summon_dxs(championship,[TopChampionDX_Id]),
			io:format("TopChampionDXIds:~p~n",[TopChampionDX_Id]),
			#state{op_mode=championship,
				population_id = Population_Id,
				activeDX_IdPs = ActiveDX_IdPs,
				tot_individuals = length([TopChampionDX_Id]),
				individuals_left = length([TopChampionDX_Id]),
				op_tag = continue};
		{OpModes,Population_Id,Survival_Type}->
			DX_Ids = extract_DXIds(Population_Id,all),
			[P] = mnesia:dirty_read({population,Population_Id}),
			T=P#population.trace,
			[put({evaluations,Specie_Id},0) || Specie_Id<-P#population.specie_ids],
			[put({active,Specie_Id},extract_SpecieDXIds(Specie_Id))|| Specie_Id<-P#population.specie_ids],
			calculate_MaxTrials(DX_Ids),
			OpMode = case lists:member(gt,OpModes) of
				true ->
					gt;
				false ->
					exit("ERROR in population_monitor. OpModes does not contain gt in init(Parameters)~n")
			end,
			ActiveDX_IdPs = summon_dxs(OpMode,DX_Ids),
			#state{op_mode=OpModes,
				population_id = Population_Id,
				activeDX_IdPs = ActiveDX_IdPs,
				tot_individuals = length(DX_Ids),
				individuals_left = length(DX_Ids),
				evaluations_acc = 0,
				step_size = T#trace.step_size,
				op_tag = continue,
				survival_type = Survival_Type}
	end,
	{ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({request,topDX_Ids}, _From, S)->
	Population_Id = S#state.population_id,
	OpTag = S#state.op_tag,
	IndividualsLeft = S#state.individuals_left,
	TopDX_Ids = extract_DXIds(Population_Id,top),
	{reply, TopDX_Ids,S};
handle_call({request,offspring,DX_Id},_From,S)->
	[DX] = mnesia:dirty_read({dx,DX_Id}),
	DXClone_Id = create_MutantDXCopy(DX_Id,safe),
%	io:format("WinnerDX:~p DXClone:~p~n",[mnesia:dirty_read({dx,WinnerDX_Id}),mnesia:dirty_read({dx,DXClone_Id})]),
%	io:format("Newborn:~p::~p~n",[DXClone_Id,{WinnerFitness,WinnerProfile,WinnerDX_Id}]),
%	{ok,_PId} = exoself:start_link({S#state.op_mode,DXClone_Id,void_MaxTrials}),
	Specie_Id = DX#dx.specie_id,
	ActiveDXIds = get({active,Specie_Id}),
	put({active,Specie_Id},[DXClone_Id|ActiveDXIds]),
	{reply,DXClone_Id,S};
handle_call(get_TRACE,_From,S)->
	Population_Id = S#state.population_id,
	[P] = mnesia:dirty_read({population,Population_Id}),
	{reply,P#population.trace,S};
handle_call(get_evaluations,_From,S)->
	Population_Id = S#state.population_id,
	[P] = mnesia:dirty_read({population,Population_Id}),
	T = P#population.trace,
	{reply,T#trace.tot_evaluations,S};
handle_call({request,max_trials}, From, S)->
	Max_Trials = get(max_trials),
	{reply, {reply,Max_Trials},S};
handle_call(terminate,From,S)->
	ActiveDX_IdPs = S#state.activeDX_IdPs,
	[gen_server:cast(DX_PId,{self(),terminate}) || {_DX_Id,DX_PId}<-ActiveDX_IdPs],
	%From ! {self(),ok},
	{stop,normal,ok,S};
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
handle_cast({DX_Id,terminated,Fitness,Fitness_Profile},S) when S#state.survival_type == competition ->
	Population_Id = S#state.population_id,
	OpTag = S#state.op_tag,
	IndividualsLeft = S#state.individuals_left,
	OpMode = case lists:member(gt,S#state.op_mode) of
		true ->
			gt;
		false ->
			exit("ERROR in population_monitor. OpModes does not contain gt in training stage~n")
	end,
	case (IndividualsLeft-1) =< 0 of
		true ->
			mutate_population(Population_Id,?SPECIE_SIZE_LIMIT,S#state.survival_type),
			%benchmark ! {self(), tunning_phase,done},
			U_Attempt = S#state.attempt+1,
			io:format("Attempt:~p Ended.~n~n~n",[U_Attempt]),
			[P] = mnesia:dirty_read({population,Population_Id}),
			T = P#population.trace,
			TotEvaluations=T#trace.tot_evaluations,
			case OpTag of
				continue ->
					case (U_Attempt >= ?GENERATION_LIMIT) or (TotEvaluations >= ?EVALUATIONS_LIMIT) or (S#state.goal_status==reached) of
						true ->%FAILED
							%benchmark ! {self(),goal_failed,S#state.evaluations_acc,U_Attempt},
							DX_Ids = extract_DXIds(Population_Id,all),
							U_S = S#state{dx_ids = DX_Ids,tot_individuals =length(DX_Ids),individuals_left = length(DX_Ids),attempt=U_Attempt},
							{stop,normal,U_S};
						false ->%IN_PROGRESS
							DX_Ids = extract_DXIds(Population_Id,all),
							calculate_MaxTrials(DX_Ids),
							summon_dxs(OpMode,DX_Ids),
							U_S = S#state{dx_ids = DX_Ids,tot_individuals =length(DX_Ids),individuals_left = length(DX_Ids),attempt=U_Attempt},
							{noreply,U_S}
					end;
				bypass ->
					DX_Ids = extract_DXIds(Population_Id,all),
					calculate_MaxTrials(DX_Ids),
					summon_dxs(OpMode,DX_Ids),
					U_S = S#state{dx_ids = DX_Ids,tot_individuals = length(DX_Ids),individuals_left = length(DX_Ids),attempt=U_Attempt},
					{noreply,U_S};
				done ->
					io:format("Shutting down Population Monitor and all associated Individuals~n"),
					U_S = S#state{individuals_left = 0,attempt=U_Attempt},
					{stop,normal,U_S};
				pause ->
					io:format("Population Monitor has paused.~n"),
					benchmark ! {self(), monitor, paused},
					U_S = S#state{individuals_left = 0,attempt=U_Attempt},
					{noreply,U_S}
			end;
		false ->
			io:format("Individuals Left:~p~n ",[IndividualsLeft-1]),
			ActiveDX_IdPs = S#state.activeDX_IdPs,
			U_ActiveDX_Ids = lists:keydelete(DX_Id,1,ActiveDX_IdPs),
			U_S = S#state{dx_ids = U_ActiveDX_Ids,individuals_left = IndividualsLeft-1},
			{noreply,U_S}
	end;
handle_cast({DX_Id,terminated,Fitness,Fitness_Profile},State) when State#state.survival_type == polis ->
%	IndividualsLeft = S#state.individuals_left,

	io:format("Tot Evaluations:~p~n",[State#state.evaluations_acc]),
	[DX] = mnesia:dirty_read({dx,DX_Id}),
	Morphology= DX#dx.morphology,
	Specie_Id = DX#dx.specie_id,
	[Specie] = mnesia:dirty_read({specie,Specie_Id}),
	Old_DeadPool_DXSummaries = Specie#specie.dead_pool,
	Old_DX_Ids = Specie#specie.dx_ids,
	io:format("Old_DeadPool:~p~n Old_DX_Ids:~p~n",[Old_DeadPool_DXSummaries,Old_DX_Ids]),
	DeadPool_DXSummaries = [{Fitness,Fitness_Profile,DX_Id}|Old_DeadPool_DXSummaries],
	%io:format("DX:~p Morphology:~p~n",[DX,Morphology]),
	
	SDX=lists:reverse(lists:sort([{Fitness_/math:pow(TotN,?EFF),{Fitness_,[A,B,TotN],DX_Id_}}||{Fitness_,[A,B,TotN],DX_Id_}<-lists:reverse(lists:sort(DeadPool_DXSummaries))])),
	ProperlySorted_DXSummaries = [Val || {_,Val}<-SDX],
	%ProperlySorted_DXSummaries = lists:reverse(lists:sort(DXSummaries)),

	Top_DXSummaries = lists:sublist(ProperlySorted_DXSummaries,round(?SPECIE_SIZE_LIMIT*?SURVIVAL_PERCENTAGE)),
	
	TotEnergy = lists:sum([extract_DXFitness(DXId) || {_Fitness,_Fitness_Profile,DXId}<-DeadPool_DXSummaries]),
	TotNeurons = lists:sum([extract_DXTotNeurons(DXId) || {_Fitness,_Fitness_Profile,DXId} <- DeadPool_DXSummaries]),
	NeuralEnergyCost = TotEnergy/TotNeurons,
	%io:format("TotEnergy:~p TotNeurons:~p NeuralEnergyCost:~p~n",[TotEnergy,TotNeurons,NeuralEnergyCost]),
	{AlotmentsP,NextGenSize_Estimate} = calculate_alotments(Top_DXSummaries,NeuralEnergyCost,[],0),
	io:format("Morphology:~p DXSummaries:~p~n Top_DXSummaries:~p~n NeuralEnergyCost:~p~n NextGenSize_Estimate:~p~n AllotmentsP:~p~n",
		[Morphology,DeadPool_DXSummaries,Top_DXSummaries,NeuralEnergyCost,NextGenSize_Estimate,AlotmentsP]),
	{WinnerFitness,WinnerProfile,WinnerDX_Id}=choose_CompetitionWinner(AlotmentsP,random:uniform(round(100*NextGenSize_Estimate))/100,0),
	Valid_DXSummaries = case length(ProperlySorted_DXSummaries) >= ?SPECIE_SIZE_LIMIT of
		true ->
			[{InvalidFitness,InvalidProfile,InvalidDX_Id}|Remaining_DXSummaries] = lists:reverse(ProperlySorted_DXSummaries),
%			io:format("Informationtheoretic Death:~p::~p~n",[InvalidDX_Id,{InvalidFitness,InvalidProfile,InvalidDX_Id}]),
			delete_dx(InvalidDX_Id,safe),
			Remaining_DXSummaries;
		false ->
			ProperlySorted_DXSummaries
	end,
	ActiveDX_IdP = case random:uniform() < 0.1 of%Update dx_ids, and dead_pool
		true ->
			U_DeadPool_DXSummaries = lists:delete({WinnerFitness,WinnerProfile,WinnerDX_Id},Valid_DXSummaries),
			{ok,WinnerDX_PId} = exoself:start_link({State#state.op_mode,WinnerDX_Id,void_MaxTrials,self()}),
			{WinnerDX_Id,WinnerDX_PId};
		false ->
			U_DeadPool_DXSummaries = Valid_DXSummaries,
			DXClone_Id = create_MutantDXCopy(WinnerDX_Id,safe),
%			io:format("WinnerDX:~p DXClone:~p~n",[mnesia:dirty_read({dx,WinnerDX_Id}),mnesia:dirty_read({dx,DXClone_Id})]),
%			io:format("Newborn:~p::~p~n",[DXClone_Id,{WinnerFitness,WinnerProfile,WinnerDX_Id}]),
			{ok,DXClone_PId} = exoself:start_link({State#state.op_mode,DXClone_Id,void_MaxTrials,self()}),
			{DXClone_Id,DXClone_PId}
	end,
	{_,_,TopDX_Ids} = unzip3(lists:sublist(Top_DXSummaries,3)),
	io:format("TopDX_Ids:~p~n",[TopDX_Ids]),
%	update_TopDXs(Specie_Id,TopDX_Ids),
	[USpecie]=mnesia:dirty_read({specie,Specie_Id}),
	mnesia:dirty_write(USpecie#specie{dead_pool = U_DeadPool_DXSummaries,topdx_ids = TopDX_Ids}),

	ActiveDX_IdPs = State#state.activeDX_IdPs,
	U_ActiveDX_IdPs = [ActiveDX_IdP|lists:keydelete(DX_Id,1,ActiveDX_IdPs)],
	U_State = State#state{dx_ids=U_ActiveDX_IdPs},
	{noreply,U_State};
handle_cast({DX_Id,champion_terminated,Fitness,Fitness_Profile},S)->
	IndividualsLeft = S#state.individuals_left,
	ActiveDX_IdPs = S#state.activeDX_IdPs,
	U_ActiveDX_IdPs = lists:keydelete(DX_Id,1,ActiveDX_IdPs),
	io:format("Champion Terminated:~p Fitness:~p FitnessProfile:~p~n",[DX_Id,Fitness,Fitness_Profile]),
	case IndividualsLeft-1 == 0 of
		true->
			{stop, normal,S#state{activeDX_IdPs=U_ActiveDX_IdPs,individuals_left=IndividualsLeft-1}};
		false ->
			{noreply,S#state{activeDX_IdPs=U_ActiveDX_IdPs,individuals_left=IndividualsLeft-1}}
	end;
handle_cast({request,offspring,DX_Id},S)->
	[DX] = mnesia:dirty_read({dx,DX_Id}),
	DXClone_Id = create_MutantDXCopy(DX_Id,safe),
%	io:format("WinnerDX:~p DXClone:~p~n",[mnesia:dirty_read({dx,WinnerDX_Id}),mnesia:dirty_read({dx,DXClone_Id})]),
%	io:format("Newborn:~p::~p~n",[DXClone_Id,{WinnerFitness,WinnerProfile,WinnerDX_Id}]),
	{ok,_PId} = exoself:start_link({S#state.op_mode,DXClone_Id,void_MaxTrials,self()}),
	Specie_Id = DX#dx.specie_id,
	ActiveDXIds = get({active,Specie_Id}),
	put({active,Specie_Id},[DXClone_Id|ActiveDXIds]),
	{noreply,S};
handle_cast({From,evaluations,Specie_Id,AEA},S)->
	Evaluations = case S#state.goal_status of
		reached ->
			0;
		_ ->
			AEA
	end,
	
	Evaluations_Acc = S#state.evaluations_acc,
	SEval_Acc=get({evaluations,Specie_Id}),
	put({evaluations,Specie_Id},SEval_Acc+Evaluations),
%	io:format("+Evaluations:~p From:~p Tot_Evaluations:~p~n",[Evaluations,From,Evaluations_Acc]),
	case Evaluations_Acc rem 50 of
		0 ->
			io:format("Tot_Evaluations:~p~n",[Evaluations_Acc]);
		_ ->
			done
	end,
	U_EvaluationsAcc = Evaluations_Acc+Evaluations,
	U_S=case S#state.survival_type of
		_ ->%TODO:::TODO:::TODO:::TODO:::TODO:::
			case U_EvaluationsAcc >= S#state.step_size of
				true ->
					gather_STATS(S#state.population_id,U_EvaluationsAcc,S#state.op_mode),
					update_PopulationChampions(S#state.population_id),
					Population_Id = S#state.population_id,
					[P] = mnesia:dirty_read({population,Population_Id}),
					T = P#population.trace,
					TotEvaluations=T#trace.tot_evaluations,
					io:format("Evaluations:~p~n",[TotEvaluations]),
					S#state{evaluations_acc=0};
				false ->
					S#state{evaluations_acc=U_EvaluationsAcc}
			end;
		_ ->
			case S#state.goal_status of
				undefined ->
					S#state{evaluations_acc=U_EvaluationsAcc};
				reached ->
					S
			end
	end,
	{noreply,U_S};
handle_cast({_From,print_TRACE},S)->
%-record(trace,{stats=[],tot_evaluations=0,step_size=500,next_step=500}).
%-record(stats,{avg_subcores=[],avg_neurons=[],avg_fitness=[],max_fitness=[],min_fitness=[],avg_diversity=[],evaluations=[],tot_evaluations=0,step_size=500,next_step=500}).
	Population_Id = S#state.population_id,
	[P] = mnesia:dirty_read({population,Population_Id}),
	io:format("******** TRACE ********:~n~p~n",[P#population.trace]),
	{noreply,S};
handle_cast({From,goal_reached,Morphology},S)->
	case S#state.goal_status of
		undefined ->
			%benchmark ! {self(),goal_reached,S#state.evaluations_acc,S#state.attempt+1,get(diversity)},
			{noreply,S#state{goal_status=reached}};
		reached ->
			{noreply,S}
	end;
handle_cast({op_tag,continue},S) ->
	Population_Id = S#state.population_id,
	OpMode = case lists:member(gt,S#state.op_mode) of
		true ->
			gt;
		false ->
			exit("ERROR in population_monitor. handle_cast({op_tag,continue},S) does not have gt in OpModes~n")
	end,
	DX_Ids = extract_DXIds(Population_Id,all),
	summon_dxs(OpMode,DX_Ids),
	U_S = S#state{individuals_left = length(DX_Ids), op_tag = continue},
	{noreply,U_S};
handle_cast({init,InitState},_State)->
	{noreply,InitState};
handle_cast({op_tag,New_OpTag},S)->
	io:format("Received new Op_Tag:~p~n",[New_OpTag]),
	U_S = S#state{op_tag = New_OpTag},
	{noreply,U_S};
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
	case S of
		[] ->
			io:format("******** Population_Monitor shut down with Reason:~p, with State: []~n",[Reason]);
		_ ->
			Population_Id = S#state.population_id,
			OpTag = S#state.op_tag,
			OpModes = S#state.op_mode,
			[P] = mnesia:dirty_read({population,Population_Id}),
			T = P#population.trace,
			TotEvaluations=T#trace.tot_evaluations,
			U_T = T#trace{tot_evaluations = TotEvaluations+S#state.evaluations_acc},
			update_PopulationChampions(S#state.population_id),
			case whereis(benchmark) of
				undefined ->	
					 ok;
				PId ->
					PId ! {Population_Id,completed,U_T},
					io:format("******** Population_Monitor:~p shut down with Reason:~p OpTag:~p, while in OpModes:~p~n",[Population_Id,Reason,OpTag,OpModes])
			end
	end.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
extract_DXIds(Population_Id,CitizenType)->
	[P] = mnesia:dirty_read({population,Population_Id}),
	Specie_Ids = P#population.specie_ids,
%	io:format("Specie_Ids:~p~n",[Specie_Ids]),
	case CitizenType of
		top ->
			extract_TopDXIds(Specie_Ids,[]);
		champions ->
			extract_ChampionDXIds(Specie_Ids,[]);
		all ->
			extract_AllDXIds(Specie_Ids,[])
	end.
	
	extract_AllDXIds([Specie_Id|Specie_Ids],Acc)->
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		DX_Ids = S#specie.dx_ids,
		extract_AllDXIds(Specie_Ids,lists:append(DX_Ids,Acc));
	extract_AllDXIds([],Acc)->
%		io:format("DX_Ids:~p~n",[Acc]),
		Acc.

	extract_TopDXIds([Specie_Id|Specie_Ids],Acc)->
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		TopDX_Ids = S#specie.topdx_ids,
		extract_TopDXIds(Specie_Ids,lists:append(TopDX_Ids,Acc));
	extract_TopDXIds([],Acc)->
		Acc.
	
	extract_ChampionDXIds([Specie_Id|Specie_Ids],Acc)->
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		ChampionDX_Idps = S#specie.championdx_ids,
		ChampionDX_Ids=[ChampionDX_Id || {_AncestorDX_Id,ChampionDX_Id}<-ChampionDX_Idps],
		extract_ChampionDXIds(Specie_Ids,lists:append(ChampionDX_Ids,Acc));
	extract_ChampionDXIds([],Acc)->
		Acc.
	
extract_SpecieDXIds(Specie_Id)->
	[S] = mnesia:dirty_read({specie,Specie_Id}),
	S#specie.dx_ids.

summon_specie(OpMode,Specie_Id)->
	[S] = mnesia:dirty_read({specie,Specie_Id}),
	DX_Ids = S#specie.dx_ids,
	Active_DXPs = summon_dxs(OpMode,DX_Ids,[]),
%	mnesia:dirty_write(S#specie{active=Active_DXs}).
	DX_Ids.

summon_dxs(OpMode,DX_Ids)->
	summon_dxs(OpMode,DX_Ids,[]).
	
summon_dxs(OpMode,[DX_Id|DX_Ids],Acc)->
%	io:format("DX_Id:~p~n",[DX_Id]),
	{ok,DX_PId} = exoself:start_link({OpMode,DX_Id,get(max_trials),self()}),
	summon_dxs(OpMode,DX_Ids,[{DX_Id,DX_PId}|Acc]);
summon_dxs(_OpMode,[],Acc)->
	Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
info(list_ids)->
	lists:map(fun(Key)-> io:format("Population Id:~p~n",[Key]) end, mnesia:dirty_all_keys(population)).
new()->
	done.
reset()->
	done.
backup()->
	done.
	
championship()->
	population_monitor:start({championship,?INIT_POPULATION_ID}).
champion()->
	population_monitor:start({champion,?INIT_POPULATION_ID}).
%--------------------------------Generate Initial Population--------------------------------
%%%Notes: Generates a population of a specific size, utilizing random number of output vector lengths, number of output vectors, and the same for inputs
%%%Function: Generates population of specified size of diverse Entities. Generates a predifined number of Individuals, marking every file with the DX_Id 
% of the individual, and saving the list of DX_DIds in the "Info" file
%%%Interface:Input:(PopSize:S) Output:[DX_Id1,DX_Id2...DX_Idn]
%%%MsgComunication: N/A
test()->
	init_population({?INIT_POPULATION_ID,?INIT_CONSTRAINTS,?OP_MODES,?SURVIVAL_TYPE}).
	
init_population(PMP,SpecCon)->
	init_population({PMP#pmp.population_id,SpecCon,PMP#pmp.op_mode,PMP#pmp.survival_type}).
	
init_population({Population_Id,Specie_Constraints,OpMode,Survival_Type})->
	{A,B,C} = now(),
	random:seed(A,B,C),
	F = fun()->
		case mnesia:read({population,Population_Id}) of
			[] ->
				create_Population(Population_Id,Specie_Constraints);
			_ ->
				delete_population(Population_Id),
				create_Population(Population_Id,Specie_Constraints)
		end
	end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic,_} ->
			population_monitor:start({OpMode,Population_Id,Survival_Type});
		Error ->
			io:format("******** ERROR in PopulationMonitor:~p~n",[Error])
	end.

	create_Population(Population_Id,Specie_Constraints)->%Link_Form,Morphologies)->
		%TotSpecies = ?TOT_INIT_SPECIES,
		SpecieSize = ?INIT_SPECIE_SIZE,
		Polis_Id = ?INIT_POLIS,
		Specie_Ids = [create_Specie(Population_Id,technome_constructor:generate_UniqueId(),SpecieSize,[],SpecCon) || SpecCon <- Specie_Constraints],
		Population = #population{
			id = Population_Id,
			specie_ids = Specie_Ids,
			polis_id = Polis_Id,
			evo_strat = specie_evo_strat:init()},
		mnesia:write(Population).
			
		create_Specie(Population_Id,Specie_Id,0,IdAcc,SpeCon)->
			io:format("Specie_Id:~p Morphology:~p~n",[Specie_Id,SpeCon#constraint.morphology]),
			Specie = #specie{
				id = Specie_Id,
				morphology = SpeCon#constraint.morphology,
				constraint = SpeCon,
				dx_ids = IdAcc,
				population_id = Population_Id
			},
%			io:format("Specie:~p~n",[Specie]),
			mnesia:write(Specie),
			Specie_Id;
		create_Specie(Population_Id,SpecieId,Citizen_Index,IdAcc,SpeCon)->
			DX_Id = float_to_list(technome_constructor:generate_UniqueId()),
			modular_constructor:construct_DX(SpecieId,DX_Id,SpeCon),
			create_Specie(Population_Id,SpecieId,Citizen_Index-1,[DX_Id|IdAcc],SpeCon).

continue(OpMode,Survival_Type)->
	Population_Id = test,
	conform_SpecieSize(Population_Id),
	population_monitor:start({OpMode,Population_Id,Survival_Type}).

%--------------------------------Evolve Top Technomes--------------------------------
%%%Notes: Species
%%%Function: 
%%%Interface:Input:() Output:
%%%MsgComunication: N/A
mutate_population(Population_Id,KeepTot,Survival_Type)->
	NeuralEnergyCost = calculate_EnergyCost(Population_Id),
	F = fun()->
		[P] = mnesia:read({population,Population_Id}),
		Specie_Ids = P#population.specie_ids,
		[mutate_Specie(Specie_Id,KeepTot,NeuralEnergyCost,Survival_Type) || Specie_Id <- Specie_Ids]
	end,
	{atomic,_} = mnesia:transaction(F).
	
	mutate_Specie(Specie_Id,PopulationLimit,NeuralEnergyCost,Survival_Type)->
		[S] = mnesia:read({specie,Specie_Id}),
		DX_Ids = S#specie.dx_ids,
		Sorted_DXSummaries = lists:reverse(lists:sort(extract_DXSummaries(DX_Ids,[]))),
		io:format("Survival_Type:~p~n",[Survival_Type]),
		case Survival_Type of
			competition ->
				TotSurvivors = round(length(Sorted_DXSummaries)*?SURVIVAL_PERCENTAGE),
				SDX=lists:reverse(lists:sort([{Fitness/math:pow(TotN,?EFF),{Fitness,[A,B,TotN],DX_Id}}||{Fitness,[A,B,TotN],DX_Id}<-Sorted_DXSummaries])),
				ProperlySorted_DXSummaries = [Val || {_,Val}<-SDX],
				
%				ProperlySorted_DXSummaries = Sorted_DXSummaries,
				Valid_DXSummaries=uniquify(ProperlySorted_DXSummaries,TotSurvivors),
				%Valid_DXSummaries = lists:sublist(ProperlySorted_DXSummaries,TotSurvivors),
				Invalid_DXSummaries = Sorted_DXSummaries -- Valid_DXSummaries,
				{_,_,Invalid_DXIds} = unzip3(Invalid_DXSummaries),
				[delete_dx(DX_Id) || DX_Id <- Invalid_DXIds],
				io:format("Valid_DXSummaries:~p~n",[Valid_DXSummaries]),
				io:format("Invalid_DXSummaries:~p~n",[Invalid_DXSummaries]),
				
				TopDXSummaries = lists:sublist(Valid_DXSummaries,3),
				{_,_,TopDX_Ids} = unzip3(TopDXSummaries),
				io:format("NeuralEnergyCost:~p~n",[NeuralEnergyCost]),
				NewGenDX_Ids = competition(Valid_DXSummaries,PopulationLimit,NeuralEnergyCost);
			ranked ->
				TotSurvivors = round(length(Sorted_DXSummaries)*?SURVIVAL_PERCENTAGE),
				SDX=lists:reverse(lists:sort([{Fitness/math:pow(TotN,?EFF),{Fitness,[A,B,TotN],DX_Id}}||{Fitness,[A,B,TotN],DX_Id}<-Sorted_DXSummaries])),
				ProperlySorted_DXSummaries = [Val || {_,Val}<-SDX],
				%Diversify based on fitness score
				%dynamic_annealing
				Valid_DXSummaries = uniquify(ProperlySorted_DXSummaries,TotSurvivors),
				Temperature = calculate_temperature(ProperlySorted_DXSummaries),
				TopDXSummaries = lists:sublist(Valid_DXSummaries,3),
				{_,_,TopDX_Ids} = unzip3(TopDXSummaries),
				io:format("NeuralEnergyCost:~p~n",[NeuralEnergyCost]),
				NewGenDX_Ids = competition(Valid_DXSummaries,PopulationLimit,NeuralEnergyCost);%,Temperature);
			top3 ->
				TotSurvivors = 3,
				ProperlySorted_DXSummaries = Sorted_DXSummaries,
				Valid_DXSummaries = lists:sublist(ProperlySorted_DXSummaries,TotSurvivors),
				Invalid_DXSummaries = Sorted_DXSummaries -- Valid_DXSummaries,
				{_,_,Invalid_DXIds} = unzip3(Invalid_DXSummaries),
				{_,_,Valid_DXIds} = unzip3(Valid_DXSummaries),
				[delete_dx(DX_Id) || DX_Id <- Invalid_DXIds],
				io:format("Valid_DXSummaries:~p~n",[Valid_DXSummaries]),
				io:format("Invalid_DXSummaries:~p~n",[Invalid_DXSummaries]),
				TopDXSummaries = lists:sublist(Valid_DXSummaries,3),
				{_,_,TopDX_Ids} = unzip3(TopDXSummaries),
				io:format("NeuralEnergyCost:~p~n",[NeuralEnergyCost]),
				NewGenDX_Ids = top3(Valid_DXIds,PopulationLimit-TotSurvivors,[])
		end,
%		io:format("Top Id:~p Top Ids:~p Sorted_DXSummaries:~p~n",[TopDX_Id,TopDX_Ids,Sorted_DXSummaries]),
		Diversity = calculate_diversity(NewGenDX_Ids),
		case get(diversity) of
			undefined->
				put(diversity,[Diversity]);
			PrevGenDiversity ->
				put(diversity,[Diversity|PrevGenDiversity])
		end,
		[TopDX_Id|_] = TopDX_Ids,
		ChampionDX_Idps = S#specie.championdx_ids,
		U_ChampionDX_Idps=case lists:keymember(TopDX_Id,1,ChampionDX_Idps) of
			true -> 
				ChampionDX_Idps;
			false->
				ChampionDX_Id = clone_dx(TopDX_Id),
				%case length(ChampionDX_Idps) >= 10 of
				%	true ->
				%		Valid_ChampionDX_Idps = lists:sublist([{TopDX_Id,ChampionDX_Id}|ChampionDX_Idps],10),
				%		Invalid_ChampionDX_Idps =  [{TopDX_Id,ChampionDX_Id}|ChampionDX_Idps] -- Valid_ChampionDX_Idps,
				%		[delete_dx(Invalid_ChampionDX_Id)|| {_OriginalDX_Id,Invalid_ChampionDX_Id}<-Invalid_ChampionDX_Idps],
				%		Valid_ChampionDX_Idps;
				%	false ->
				%		[{TopDX_Id,ChampionDX_Id}|ChampionDX_Idps]
				%end
				[{TopDX_Id,ChampionDX_Id}|ChampionDX_Idps]
		end,
		mnesia:write(S#specie{
			dx_ids = NewGenDX_Ids,
			topdx_ids = TopDX_Ids,
			championdx_ids = U_ChampionDX_Idps}).
		
		extract_DXSummaries([DX_Id|DX_Ids],Acc)->
			[DX] = mnesia:read({dx,DX_Id}),
			Fitness = DX#dx.fitness,
			Profile = DX#dx.profile,
			Summary = {Fitness,Profile,DX_Id},
			extract_DXSummaries(DX_Ids,[Summary|Acc]);
		extract_DXSummaries([],Acc)->
			Acc.
				
		extract_TopDX([DX_Summary|Sorted_DXSummaries],KeepTot)->
			{Fitness,Profile,TopDX_Id} = DX_Summary,
			io:format("Top Fitness:~p Top Profile:~p TopDX_Id:~p~n",[Fitness,Profile,TopDX_Id]),
			{FitnessAcc,Diversified_TopIdsP} = diversify([Profile],Sorted_DXSummaries,KeepTot-1,[{Fitness,TopDX_Id}],Fitness,[]),
			{FitnessAcc,Diversified_TopIdsP}.

			diversify(_TopProfiles,_Sorted_DXSummaries,0,TopDIdsAcc,FitnessAcc,_Acc)->
				{FitnessAcc,lists:reverse(TopDIdsAcc)};
			diversify(TopProfiles,[{Fitness,Profile,DX_Id}|Sorted_DXSummaries],KeepTot,TopIdsAcc,FitnessAcc,Acc)->
				case compare_profiles(TopProfiles,Profile) of
					true->
						diversify([Profile|TopProfiles],Sorted_DXSummaries,KeepTot-1,[{Fitness,DX_Id}|TopIdsAcc],FitnessAcc+Fitness,Acc);
					false ->
						diversify(TopProfiles,Sorted_DXSummaries,KeepTot,TopIdsAcc,FitnessAcc,[DX_Id|Acc])
				end;
			diversify(_TopProfiles,[],KeepTot,TopIdsAcc,FitnessAcc,Acc)->
				Remaining_SortedIds = lists:reverse(Acc),
				Remaining_TopIds = lists:sublist(Remaining_SortedIds,KeepTot),
				{FitnessAcc,lists:append(lists:reverse(TopIdsAcc),[])}.
			
				compare_profiles([TopProfile|TopProfiles],Profile)->
					case TopProfile == Profile of
						true ->
							false;
						false ->
							compare_profiles(TopProfiles,Profile)
					end;
				compare_profiles([],_Profile)->
					true.

		uniquify(ProperlySorted_DXSummaries,TotSurvivors)->
			%[DX_Summary|Sorted_DXSummaries] = ProperlySorted_DXSummaries,%lists:sublist(ProperlySorted_DXSummaries,TotSurvivors),
			[DX_Summary|Sorted_DXSummaries] = lists:sublist(ProperlySorted_DXSummaries,TotSurvivors),
			{Fitness,Profile,_DX_Id} = DX_Summary,
			Diversified_DXSummaries = diversify([{Fitness,Profile}],Sorted_DXSummaries,TotSurvivors-1,[DX_Summary]).
			
			diversify(_TopProfiles,_Sorted_DXSummaries,0,Acc)->
				lists:reverse(Acc);
			diversify(Profiles,[DX_Summary|Sorted_DXSummaries],KeepTot,Acc)->
				{Fitness,Profile,DX_Id} = DX_Summary,
				%case compare_profiles(Profiles,Profile) of
				case compare_profilesf(Profiles,{Fitness,Profile}) of
					true->
						diversify([{Fitness,Profile}|Profiles],Sorted_DXSummaries,KeepTot-1,[DX_Summary|Acc]);
					false ->
						diversify(Profiles,Sorted_DXSummaries,KeepTot,Acc)
				end;
			diversify(_TopProfiles,[],KeepTot,Acc)->
				lists:reverse(Acc).
				
				compare_profilesf([{TopFitness,TopProfile}|TopProfiles],{Fitness,Profile})->%Better make Fitnes part of profile
					case (TopProfile == Profile) of%and (TopFitness == Fitness) of
						true ->
							false;
						false ->
							compare_profilesf(TopProfiles,{Fitness,Profile})
					end;
				compare_profilesf([],_ProfileP)->
					true.
				
			calculate_temperature(ProperlySorted_DXSummaries)->
				%Calculate age between population, and agent.
				%The higher the age, the longer has this agent stayedi in the population without its offspring overtaking.
				%Increase temperate in proportion to the age, thus increasing the diversity of the offspring.
				ok.

	competition(Sorted_DXSummaries,PopulationLimit,NeuralEnergyCost)->
		{AlotmentsP,NextGenSize_Estimate} = calculate_alotments(Sorted_DXSummaries,NeuralEnergyCost,[],0),
		Normalizer = NextGenSize_Estimate/PopulationLimit,
		io:format("Normalizer:~p~n",[Normalizer]),
		gather_survivors(AlotmentsP,Normalizer,[]).
		
		calculate_alotments([{Fitness,Profile,DX_Id}|Sorted_DXSummaries],NeuralEnergyCost,Acc,NewPopAcc)->
			[_TotSubstrates,_TotSubCores,TotNeurons] = Profile,
			NeuralAlotment = Fitness/NeuralEnergyCost,
			MutantAlotment = NeuralAlotment/TotNeurons,
			U_NewPopAcc = NewPopAcc+MutantAlotment,%+case round(MutantAlotment) >= 1 of
								%	true -> 1;
								%	false -> 0
								%end,
			calculate_alotments(Sorted_DXSummaries,NeuralEnergyCost,[{MutantAlotment,Fitness,Profile,DX_Id}|Acc],U_NewPopAcc);
		calculate_alotments([],_NeuralEnergyCost,Acc,NewPopAcc)->
			io:format("NewPopAcc:~p~n",[NewPopAcc]),
			{Acc,NewPopAcc}.

		gather_survivors([{MutantAlotment,Fitness,Profile,DX_Id}|AlotmentsP],Normalizer,Acc)->
			Normalized_MutantAlotment = round(MutantAlotment/Normalizer),
			io:format("DX_Id:~p Normalized_MutantAlotment:~p~n",[DX_Id,Normalized_MutantAlotment]),
			SurvivingDX_Ids = case Normalized_MutantAlotment >= 1 of
				true ->
					MutantDX_Ids = case Normalized_MutantAlotment >= 2 of
						true ->
							[create_MutantDXCopy(DX_Id)|| _ <-lists:seq(1,Normalized_MutantAlotment-1)];
						false ->
							[]
					end,
					[DX_Id|MutantDX_Ids];
				false ->
					io:format("deleting DX:~p~n",[DX_Id]),
					delete_dx(DX_Id),
					[]
			end,
			gather_survivors(AlotmentsP,Normalizer,lists:append(SurvivingDX_Ids,Acc));
		gather_survivors([],_Normalizer,Acc)->
			io:format("New Population:~p PopSize:~p~n",[Acc,length(Acc)]),
			Acc.

		create_MutantDXCopy(DX_Id)->
			CloneDX_Id = clone_dx(DX_Id),
			io:format("CloneDX_Id:~p~n",[CloneDX_Id]),
			technome_mutator:mutate(CloneDX_Id),
			CloneDX_Id.
			
			clone_dx(DX_Id)->
				CloneDX_Id = float_to_list(technome_constructor:generate_UniqueId()),
				technome_constructor:clone_dx(DX_Id,CloneDX_Id),
				CloneDX_Id.
				
		create_MutantDXCopy(DX_Id,safe)->%TODO
			[DX] = mnesia:dirty_read({dx,DX_Id}),
			[Specie] = mnesia:dirty_read({specie,DX#dx.specie_id}),
			CloneDX_Id = clone_dx(DX_Id),
			DX_Ids = Specie#specie.dx_ids,
			mnesia:dirty_write(Specie#specie{dx_ids = [CloneDX_Id|DX_Ids]}),
			io:format("CloneDX_Id:~p~n",[CloneDX_Id]),
			technome_mutator:mutate(CloneDX_Id),
			CloneDX_Id.

top3(Valid_DXIds,0,Acc)->
	Acc;
top3(Valid_DXIds,OffspringIndex,Acc)->%TODO
	Parent_DXId = lists:nth(random:uniform(length(Valid_DXIds)),Valid_DXIds),
	MutantDX_Id = create_MutantDXCopy(Parent_DXId),
	top3(Valid_DXIds,OffspringIndex-1,[MutantDX_Id|Acc]).

choose_CompetitionWinner([{_MutantAlotment,Fitness,Profile,DX_Id}],_Index,_Acc)->%TODO: Does this really work?
	{Fitness,Profile,DX_Id};
choose_CompetitionWinner([{MutantAlotment,Fitness,Profile,DX_Id}|AlotmentsP],Index,Acc)->
	case (Index > Acc) and (Index =< Acc+MutantAlotment) of
		true ->
			{Fitness,Profile,DX_Id};
		false ->
			choose_CompetitionWinner(AlotmentsP,Index,Acc+MutantAlotment)
	end.
	
delete_population(Population_Id)->
	[P] = mnesia:read({population,Population_Id}),
	Specie_Ids = P#population.specie_ids,
	[delete_specie(Specie_Id) || Specie_Id <- Specie_Ids],
	mnesia:delete({population,Population_Id}).
	
	delete_specie(Specie_Id)->
		[S] = mnesia:read({specie,Specie_Id}),
		DX_Ids = S#specie.dx_ids,
		ChampionDX_Ids = S#specie.championdx_ids,
		[delete_dx(DX_Id) || DX_Id <- DX_Ids],
		%[delete_dx(ChampionDX_Id) || {_AncestorDX_Id,ChampionDX_Id} <- ChampionDX_Ids],%TODO: Now we keep the champions...
		mnesia:delete({specie,Specie_Id}).
		
		delete_dx(DX_Id)->
			io:format("Delete_DX(DX_Id):~p~n",[DX_Id]),
			technome_constructor:delete_dx(DX_Id).
			
		delete_dx(DX_Id,safe)->
			F = fun()->
				[DX] = mnesia:read({dx,DX_Id}),
				[Specie] = mnesia:read({specie,DX#dx.specie_id}),
				DX_Ids = Specie#specie.dx_ids,
				DeadPool = Specie#specie.dead_pool,
				mnesia:write(Specie#specie{dx_ids = lists:delete(DX_Id,DX_Ids),dead_pool=lists:keydelete(DX_Id,3,DeadPool)}),
				technome_constructor:delete_dx(DX_Id)
			end,
			Result=mnesia:transaction(F),
			io:format("Delete_DX(DX_Id,safe):~p Result:~p~n",[DX_Id,Result]).
			
unzip3(List)->
	unzip3(List,[],[],[]).
unzip3([{Val1,Val2,Val3}|List],Acc1,Acc2,Acc3)->
	unzip3(List,[Val1|Acc1],[Val2|Acc2],[Val3|Acc3]);
unzip3([],Acc1,Acc2,Acc3)->
	{lists:reverse(Acc1),lists:reverse(Acc2),lists:reverse(Acc3)}.
			
calculate_EnergyCost(Population_Id)->
	DX_Ids = extract_DXIds(Population_Id,all),
	TotEnergy = lists:sum([extract_DXFitness(DX_Id) || DX_Id<-DX_Ids]),
	TotNeurons = lists:sum([extract_DXTotNeurons(DX_Id) || DX_Id <- DX_Ids]),
	EnergyCost = TotEnergy/TotNeurons,
	EnergyCost.
	
	extract_DXTotNeurons(DX_Id)->
		[DX] = mnesia:dirty_read({dx,DX_Id}),
		Summary = DX#dx.summary,
		%{value,{tot_neurons,Tot_Neurons}} = lists:keysearch(tot_neurons, 1, Summary),
		Tot_Neurons = Summary#summary.tot_neurons,
%		io:format("Tot_Neurons:~p~n",[Tot_Neurons]),
		Tot_Neurons.
	
	extract_DXFitness(DX_Id)->
		[DX] = mnesia:dirty_read({dx,DX_Id}),
%		io:format("Fitness:~p~n",[DX#dx.fitness]),
		DX#dx.fitness.
		
calculate_MaxTrials(DX_Ids)->
	Tot_DXs = length(DX_Ids),
	CurGen_PopNWeights = calculate_CurGenPopNWeights(DX_Ids,0),
	Avg_CurGenPopNWeights = round(CurGen_PopNWeights/Tot_DXs),
	Max_Trials = 10 + functions:sat(round(math:sqrt(Avg_CurGenPopNWeights)),100,0),
%	Max_Trials = Avg_CurGenPopNWeights*2,
	put(max_trials,Max_Trials).
	
	calculate_CurGenPopNWeights([DX_Id|DX_Ids],Acc)->
		[DX] = mnesia:dirty_read({dx,DX_Id}),
		N_Ids = DX#dx.n_ids,
		Generation = DX#dx.generation,
		%io:format("N_Ids:~p Generation:~p DX:~p~n",[N_Ids,Generation,DX]),
		CurGenN_Ids = exoself:extract_CurGenNIds(N_Ids,Generation,5,[]),
		Tot_CG_Weights = exoself:extract_NWeightCount(CurGenN_Ids,0),
		calculate_CurGenPopNWeights(DX_Ids,Tot_CG_Weights+Acc);
	calculate_CurGenPopNWeights([],Acc)->
		Acc.

%-record(stat,{avg_subcores=[],avg_neurons=[],avg_fitness,max_fitness,min_fitness,avg_diversity=[],evaluations=[],time_stamp}).
%-record(trace,{stats=[],tot_evaluations=0,step_size=500,next_step=500}).
%-record(stats,{avg_subcores=[],avg_neurons=[],avg_fitness=[],max_fitness=[],min_fitness=[],avg_diversity=[],tot_evaluations=0,step_size=500,next_step=0}).
%-record(population,{id,type,specie_ids=[],topspecie_ids,polis_id}).
%-record(specie,{id,morphology,stats=#stats{},avg_fitness,stagnation_factor,dx_ids=[],topdx_ids=[],championdx_ids=[],population_id}).
%-record(summary,{tot_subcores,tot_substrates,tot_sc_ils,tot_sc_ols,tot_sc_ros,tot_neurons,tot_n_ils,tot_n_ols,tot_n_ros,af_distribution,fitness}).
gather_STATS(Population_Id,EvaluationsAcc,OpModes)->
	io:format("Gathering Species STATS in progress~n"),
	TimeStamp = technome_constructor:generate_UniqueId(),
	F = fun() ->
		[P] = mnesia:read({population,Population_Id}),
		T = P#population.trace,
		
		SpecieSTATS = [update_SpecieSTAT(Specie_Id,TimeStamp,OpModes) || Specie_Id<-P#population.specie_ids],
		PopulationSTATS = T#trace.stats,
		U_PopulationSTATS = [SpecieSTATS|PopulationSTATS],
		U_TotEvaluations = T#trace.tot_evaluations+EvaluationsAcc,
		U_Trace = T#trace{
			stats = U_PopulationSTATS,
			tot_evaluations=U_TotEvaluations
		},
		io:format("Population Trace:~p~n",[U_Trace]),
		mnesia:write(P#population{trace=U_Trace})
	end,
	Result=mnesia:transaction(F),
	io:format("Result:~p~n",[Result]).
	
	update_SpecieSTAT(Specie_Id,TimeStamp,OpModes)->
		%-record(stat,{avg_subcores=[],avg_neurons=[],avg_fitness,max_fitness,min_fitness,avg_diversity=[],evaluations=[],time_stamp}).
		%-record(trace,{stats=[],tot_evaluations=0,step_size=500,next_step=500}).
		%-record(specie,{id,morphology,constraint,trace=#trace{},cur_stat,avg_fitness,stagnation_factor,dx_ids=[],topdx_ids=[],championdx_ids=[],population_id}).
		Specie_Evaluations = get({evaluations,Specie_Id}),
		put({evaluations,Specie_Id},0),
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		{Avg_SubCores,SubCores_Std,Avg_Neurons,Neurons_Std} = calculate_SpecieAvgNodes({specie,S}),
		{AvgFitness,Fitness_Std,MaxFitness,MinFitness} = calculate_SpecieFitness({specie,S}),
		SpecieDiversity = calculate_SpecieDiversity({specie,S}),
		GenTest_FitnessP=gt(Specie_Id,OpModes),
		STAT = #stat{
			morphology = S#specie.morphology,
			specie_id = Specie_Id,
			avg_subcores=Avg_SubCores,
			subcores_std = SubCores_Std,
			avg_neurons=Avg_Neurons,
			neurons_std = Neurons_Std,
			avg_fitness=AvgFitness,
			fitness_std = Fitness_Std,
			max_fitness=MaxFitness,
			min_fitness=MinFitness,
			gentest_fitness = GenTest_FitnessP,
			avg_diversity=SpecieDiversity,
			evaluations = Specie_Evaluations,
			time_stamp=TimeStamp
		},
		T = S#specie.trace,
		%io:format("T:~p~n",[T]),
		STATS = T#trace.stats,
		U_STATS = [STAT|STATS],
		U_Trace = T#trace{stats=U_STATS,
			tot_evaluations=T#trace.tot_evaluations+Specie_Evaluations
		},
		mnesia:dirty_write(S#specie{trace=U_Trace}),
		STAT.

calculate_SpecieAvgNodes({specie,S})->
	%io:format("SpecieAvgNodes:~p~n",[S]),
	DX_Ids = S#specie.dx_ids,
	calculate_AvgNodes(DX_Ids,[],[]);
calculate_SpecieAvgNodes(Specie_Id)->
	io:format("calculate_SpecieAvgNodes(Specie_Id):~p~n",[Specie_Id]),
	[S] = mnesia:dirty_read({specie,Specie_Id}),
	calculate_SpecieAvgNodes({specie,S}).
	
	calculate_AvgNodes([DX_Id|DX_Ids],SCAcc,NAcc)->
		io:format("calculate_AvgNodes(...), DX_Id:~p~n",[DX_Id]),
		[DX] = mnesia:read({dx,DX_Id}),
		Summary = DX#dx.summary,
		Tot_Subcores = 0,%Summary#summary.tot_subcores,
		Tot_Neurons = Summary#summary.tot_neurons,
		calculate_AvgNodes(DX_Ids,[Tot_Subcores|SCAcc],[Tot_Neurons|NAcc]);
	calculate_AvgNodes([],SCAcc,NAcc)->
		{functions:avg(SCAcc),functions:std(SCAcc),functions:avg(NAcc),functions:std(NAcc)}.

calculate_PopulationDiversity(Population_Id,[Specie_Id|Specie_Ids],Acc)->
	Diversity = calculate_SpecieDiversity(Specie_Id),
	case get({diversity,Specie_Id}) of
		undefined->
			put({diversity,Specie_Id},[Diversity]);
		PrevGenDiversity ->
			put({diversity,Specie_Id},[Diversity|PrevGenDiversity])
	end,
	calculate_PopulationDiversity(Population_Id,Specie_Ids,[{Specie_Id,Diversity}|Acc]);
calculate_PopulationDiversity(_Tot_Population_Id,[],Acc)->
	Acc.

	calculate_SpecieDiversity({specie,S})->
		DX_Ids = S#specie.dx_ids,
		Diversity = calculate_diversity(DX_Ids);
	calculate_SpecieDiversity(Specie_Id)->
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		calculate_SpecieDiversity({specie,S}).
		
		calculate_diversity(DX_Ids)->
			calculate_diversity(DX_Ids,[]).
		calculate_diversity([DX_Id|DX_Ids],DiversityAcc)->
			%DiversityFactors:DXNN Size, AF, LT, #Conenctions, ConnectionTypes
			[DX] = mnesia:read({dx,DX_Id}),
			Summary = DX#dx.summary,
			NeuroLinkSummary = {Summary#summary.tot_n_ils,Summary#summary.tot_n_ols,Summary#summary.tot_n_ros},
			QuickSummary = [{Summary#summary.tot_neurons,Summary#summary.af_distribution,NeuroLinkSummary}],
			U_DiversityAcc = (DiversityAcc -- QuickSummary)++QuickSummary,%TODO, there is a better way to do this.
			calculate_diversity(DX_Ids,U_DiversityAcc);
		calculate_diversity([],DiversityAcc)->
			length(DiversityAcc).

print_SpecieDiversity([Specie_Id|Specie_Ids])->
	[S] = mnesia:dirty_read({specie,Specie_Id}),
	io:format("Specie id:~p~n Specie morphology:~p~n Diversity:~p~n",[Specie_Id,S#specie.morphology,get({diversity,Specie_Id})]),
	print_SpecieDiversity(Specie_Ids);
print_SpecieDiversity([])->
	done.
		
calculate_PopulationFitness(Population_Id,[Specie_Id|Specie_Ids],AvgFAcc,MaxFAcc,MinFAcc)->
	{AvgFitness,Std,MaxF,MinF}=calculate_SpecieFitness(Specie_Id),
	case get({fitness,Specie_Id}) of
		undefined ->
			put({fitness,Specie_Id},[{AvgFitness,Std}]);
		PrevGenFitness->
			put({fitness,Specie_Id},[{AvgFitness,Std}|PrevGenFitness])
	end,
	calculate_PopulationFitness(Population_Id,Specie_Ids,[{Specie_Id,AvgFitness}|AvgFAcc],[{Specie_Id,MaxF}|MaxFAcc],[{Specie_Id,MinF}|MinFAcc]);
calculate_PopulationFitness(_Population_Id,[],AvgFAcc,MaxFAcc,MinFAcc)->
	{AvgFAcc,MaxFAcc,MinFAcc}.

	calculate_SpecieFitness({specie,S})->
		DX_Ids = S#specie.dx_ids,
		FitnessAcc = calculate_fitness(DX_Ids),
		Sorted_FitnessAcc=lists:sort(FitnessAcc),
		case Sorted_FitnessAcc of
			[] ->
				MinFitness = 0,
				MaxFitness = 0,
				AvgFitness = 0,
				Std = inf;
			[MinFitness] ->
				MaxFitness = MinFitness,
				AvgFitness = MinFitness,
				Std = inf;
			_ ->
				[MinFitness|_] = Sorted_FitnessAcc,
				[MaxFitness|_] = lists:reverse(Sorted_FitnessAcc),
				AvgFitness = functions:avg(FitnessAcc),
				Std = functions:std(FitnessAcc)
		end,
		{AvgFitness,Std,MaxFitness,MinFitness};
	calculate_SpecieFitness(Specie_Id)->
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		calculate_SpecieFitness({specie,S}).
		
		calculate_fitness(DX_Ids)->
			calculate_fitness(DX_Ids,[]).
		calculate_fitness([DX_Id|DX_Ids],FitnessAcc)->
			[DX] = mnesia:read({dx,DX_Id}),
			case DX#dx.fitness of
				undefined ->
					calculate_fitness(DX_Ids,FitnessAcc);
				Fitness ->
					calculate_fitness(DX_Ids,[Fitness|FitnessAcc])
			end;
		calculate_fitness([],FitnessAcc)->
			FitnessAcc.

gt(Specie_Id,OpModes)->
	case lists:member(benchmark,OpModes) of
		true ->
			ChampionDX_Id = case extract_ChampionDXIds([Specie_Id],[]) of
				[DX_Id] ->
					DX_Id;
				[DX_Id|_] ->
					DX_Id;
				[]->
					void
			end,
			case ChampionDX_Id of
				void ->
					{0,void};
				_ ->
					{ok,ChampionDX_PId}= exoself:start_link({benchmark,ChampionDX_Id,1,self()}),
					receive
						{ChampionDX_Id,Fitness,FitnessProfile}->
							{Fitness,ChampionDX_Id}
					end
			end;
		false ->
			{0,void}
	end.

conform_SpecieSize(Population_Id)->
	F = fun()->
		[P] = mnesia:read({population,Population_Id}),
		Specie_Ids = P#population.specie_ids,
		conform_SpecieSize(Population_Id,Specie_Ids)
	end,
	Result=mnesia:transaction(F),
	io:format("conform_SpecieSize(Population_Id) result:~p~n",[Result]).
	
conform_SpecieSize(Population_Id,[Specie_Id|Specie_Ids])->
	[S] = mnesia:read({specie,Specie_Id}),
	DX_Ids = S#specie.dx_ids -- [DX_Id||{_,_,DX_Id}<-S#specie.dead_pool],
	case length(DX_Ids) > ?SPECIE_SIZE_LIMIT of
		true ->
			Sorted_DXSummaries = lists:reverse(lists:sort(extract_DXSummaries(DX_Ids,[]))),
			TotSurvivors = ?SPECIE_SIZE_LIMIT,
			SDX=lists:reverse(lists:sort([{Fitness_/math:pow(TotN,?EFF),{Fitness_,[A,B,TotN],DX_Id_}}||{Fitness_,[A,B,TotN],DX_Id_}<-lists:reverse(lists:sort(Sorted_DXSummaries))])),
			ProperlySorted_DXSummaries = [Val || {_,Val}<-SDX],
			Valid_DXSummaries = lists:sublist(ProperlySorted_DXSummaries,TotSurvivors),
			Invalid_DXSummaries = Sorted_DXSummaries -- Valid_DXSummaries,
			{_,_,Invalid_DXIds} = unzip3(Invalid_DXSummaries),
			[delete_dx(DX_Id,safe) || DX_Id <- Invalid_DXIds],
			io:format("Conforming_SpecieSize-Valid_DXSummaries:~p~n",[Valid_DXSummaries]),
			io:format("Conforming_SpecieSize-Invalid_DXSummaries:~p~n",[Invalid_DXSummaries]);
		false ->
			done
	end,
	conform_SpecieSize(Population_Id,Specie_Ids);
conform_SpecieSize(_Population_Id,[])->
	done.
	
update_PopulationChampions(Population_Id)->
	F = fun()->
		[P] = mnesia:read({population,Population_Id}),
		Specie_Ids = P#population.specie_ids,
		update_SpecieChampions(Specie_Ids)
	end,
	Result = mnesia:transaction(F),
	io:format("Update_Champions result:~p~n",[Result]).

	update_SpecieChampions([Specie_Id|Specie_Ids])->
		[S] = mnesia:read({specie,Specie_Id}),
		case S#specie.topdx_ids of
			[] ->
				done;
			TopDX_Ids->
				[TopDX_Id|_] = TopDX_Ids,
				ChampionDX_Idps = S#specie.championdx_ids,
				U_ChampionDX_Idps=case lists:keymember(TopDX_Id,1,ChampionDX_Idps) of
					true -> 
						ChampionDX_Idps;
					false->
						ChampionDX_Id = clone_dx(TopDX_Id),
						case length(ChampionDX_Idps) >= 10 of
							true ->
								Valid_ChampionDX_Idps = lists:sublist([{TopDX_Id,ChampionDX_Id}|ChampionDX_Idps],10),
								Invalid_ChampionDX_Idps =  [{TopDX_Id,ChampionDX_Id}|ChampionDX_Idps] -- Valid_ChampionDX_Idps,
								[delete_dx(Invalid_ChampionDX_Id)|| {_OriginalDX_Id,Invalid_ChampionDX_Id}<-Invalid_ChampionDX_Idps],
								Valid_ChampionDX_Idps;
							false ->
								[{TopDX_Id,ChampionDX_Id}|ChampionDX_Idps]
						end
				end,
				io:format("U_ChampionDX_Idps:~p~n",[U_ChampionDX_Idps]),
				mnesia:write(S#specie{championdx_ids = U_ChampionDX_Idps})
		end,
		update_SpecieChampions(Specie_Ids);
	update_SpecieChampions([])->
		done.

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
-export([start_link/1,start_link/0,start/1,start/0,stop/0,init/2,init_population/1,continue/0,extract_DXIds/2,delete_population/1,championship/0,champion/0,hof_competition/2,hof_rank/2,hof_top3/2,hof_efficiency/2,hof_random/2,list_append/2,la/2,choose_Winners/6,vecdiff/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,create_MutantDXCopy/1,test/0,new/0,reset/0,backup/0,info/1]).

%-record(state, {}).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Population Monitor Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %steady_state,generational
%Morphologies:pole2_balancing3,prey,forex_trader, xor_mimic
-define(EFF,1). %Efficiency., TODO: this should further be changed from absolute number of neurons, to diff in lowest or avg, and the highest number of neurons
-define(SURVIVAL_PERCENTAGE,0.5).
-define(SPECIE_SIZE_LIMIT,10).
-define(INIT_SPECIE_SIZE,10).
-define(REENTRY_PROBABILITY,0.0).
-define(SHOF_RATIO,1).
-define(ACTIVE_SR,0.5).
-define(SHOF_SR,0.5).
-define(MIN_ACCEPTABLE_FITNESS_RATIO,1).
-define(SELECTION_TYPE,hof_competition).
-define(EVOLUTION_TYPE,generational).%steady_state
%-define(POPULATION_LIMIT,?SPECIE_SIZE_LIMIT*length(?INIT_MORPHOLOGIES)).
-define(INIT_POPULATION_ID,test).
%-define(INIT_ARCHITECTURE_TYPE,modular).
%-define(INIT_LINK_FORM,recursive).
-define(OP_MODES,[gt,validation]).
-define(INIT_POLIS,mathema).
-define(GENERATION_LIMIT,1000).
-define(EVALUATIONS_LIMIT,100000).
-define(DIVERSITY_COUNT_STEP,500).
-define(GEN_UID,technome_constructor:generate_UniqueId()).
-define(CHAMPION_COUNT_STEP,500).
-record(state,{op_mode,population_id,activeDX_IdPs,inactiveDX_Ids,dx_ids,tot_individuals,individuals_left,op_tag,dx_summaries=[],attempt=0,evaluations_acc=0,step_size,next_step,goal_status,evolution_type,selection_type}).
-define(INIT_CONSTRAINTS,[
	#constraint{
		morphology=Morphology,
		sc_types=SC_Types,
		neural_afs =[cplx],
		sc_neural_plasticity=[none],
		sc_hypercube_plasticity=[none],
		sc_hypercube_linkform = Substrate_LinkForm,
		sc_neural_linkform=LinkForm,
		%neural_types=[circuit]
		neural_types=[standard]
	} || 
			Morphology<-[prey],
			Substrate_LinkForm <- [[feedforward]],
			LinkForm<-[recursive],
			SC_Types<-[[neural]]
	]
).
-define(INIT_PMP,#pmp{
		op_mode=[gt,validation,test],
		population_id=test,
		survival_percentage=0.5,
		evolution_type = ?EVOLUTION_TYPE,
		selection_type = ?SELECTION_TYPE,
		specie_constraint = ?INIT_CONSTRAINTS,
		specie_size_limit=10,
		init_specie_size=10,
		polis_id = mathema,
		generation_limit = inf,
		evaluations_limit = inf,
		fitness_goal = inf
	}).
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
		{OpModes,Population_Id,Evolution_Type,Selection_Type}->
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
				evolution_type = Evolution_Type,
				selection_type = Selection_Type}
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
handle_cast({DX_Id,terminated,Fitness,Fitness_Profile},S) when S#state.evolution_type == generational ->
	Population_Id = S#state.population_id,
	OpTag = S#state.op_tag,
	IndividualsLeft = S#state.individuals_left,
	OpMode = case lists:member(gt,S#state.op_mode) of
		true ->
			gt;
		false ->
			exit("ERROR in population_monitor. OpModes does not contain gt in training stage~n")
	end,
	%io:format("IndividualsLeft:~p~n",[IndividualsLeft]),
	case (IndividualsLeft-1) =< 0 of
		true ->
%			selection(Population_Id,?SPECIE_SIZE_LIMIT,S#state.evolution_type),
			%selection(Population_Id,?SELECTION_TYPE),
			selection(Population_Id,S#state.selection_type),
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
							gather_STATS(S#state.population_id,S#state.evaluations_acc,S#state.op_mode),
							{stop,normal,U_S};
						false ->%IN_PROGRESS
							DX_Ids = extract_DXIds(Population_Id,all),
							calculate_MaxTrials(DX_Ids),
							ActiveAgent_IdPs = summon_dxs(OpMode,DX_Ids),
							U_S = S#state{dx_ids = DX_Ids,activeDX_IdPs = ActiveAgent_IdPs,tot_individuals =length(DX_Ids),individuals_left = length(DX_Ids),attempt=U_Attempt},
							{noreply,U_S}
					end;
				bypass ->
					DX_Ids = extract_DXIds(Population_Id,all),
					calculate_MaxTrials(DX_Ids),
					ActiveAgent_IdPs = summon_dxs(OpMode,DX_Ids),
					U_S = S#state{dx_ids = DX_Ids,activeDX_IdPs = ActiveAgent_IdPs,tot_individuals = length(DX_Ids),individuals_left = length(DX_Ids),attempt=U_Attempt},
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
handle_cast({Agent_Id,terminated,Fitness,Fitness_Profile},State) when State#state.evolution_type == steady_state ->
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO:UPDATE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%BELOW
	F = fun()->
		%io:format("Tot Evaluations:~p~n",[State#state.evaluations_acc]),
		[A] = mnesia:read({dx,Agent_Id}),
		Specie_Id = A#dx.specie_id,
		[S] = mnesia:read({specie,Specie_Id}),
		U_ActiveAgent_Ids = S#specie.dx_ids -- [Agent_Id],
		Distinguishers = S#specie.hof_distinguishers,
		SHOF = S#specie.hall_of_fame,
		{U_SHOF,Losers}=update_SHOF(SHOF,[Agent_Id],Distinguishers,[]),
		io:format("SHOF:~p~nU_SHOF:~p~n",[SHOF,U_SHOF]),
		U_S = S#specie{hall_of_fame=U_SHOF,dx_ids=U_ActiveAgent_Ids},
		mnesia:write(U_S),
		U_S
	end,
	{atomic,U_S}=mnesia:transaction(F),
	ActiveAgent_IdPs = lists:keydelete(Agent_Id,1,State#state.activeDX_IdPs),
	case (State#state.evaluations_acc >= ?EVALUATIONS_LIMIT) or (State#state.goal_status==reached) of
		true ->%DONE
			gather_STATS(State#state.population_id,State#state.evaluations_acc,State#state.op_mode),
			[gen_server:cast(PId,{stop,normal}) || {_Id,PId}<-ActiveAgent_IdPs],
			{stop,normal,State};
		false ->%CONTINUE
			F2 = fun()->
				U_SHOF = U_S#specie.hall_of_fame,
				Specie_Id = U_S#specie.id,
				FitnessScaled=[{Champ#champion.main_fitness/math:pow(Champ#champion.tot_n,?EFF),Champ#champion.id}||Champ<-U_SHOF],
				TotFitness = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-FitnessScaled]),
				[Offspring_Id]=choose_Winners(Specie_Id,FitnessScaled,TotFitness,[],[],1),
				[U2_S] = mnesia:read({specie,Specie_Id}),
				mnesia:write(U2_S#specie{dx_ids=[Offspring_Id|U2_S#specie.dx_ids]}),
				Offspring_Id
			end,
			{atomic,Offspring_Id}=mnesia:transaction(F2),
			OpMode = case lists:member(gt,State#state.op_mode) of
				true ->
					gt;
				false ->
					exit("ERROR in population_monitor. OpModes does not contain gt in training stage~n")
			end,
			[{Offspring_Id,Offspring_PId}] = summon_dxs(OpMode,[Offspring_Id]),%TODO: Uses a static gt opmode.
			{noreply,State#state{activeDX_IdPs=[{Offspring_Id,Offspring_PId}|ActiveAgent_IdPs]}}
	end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO:UPDATE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ABOVE
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
	{ok,_PId} = exoself:start_link({gt,DXClone_Id,void_MaxTrials,self()}),
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
	U_S=case S#state.evolution_type of
		_ ->%TODO:::TODO:::TODO:::TODO:::TODO:::
			case U_EvaluationsAcc >= S#state.step_size of
				true ->
					gather_STATS(S#state.population_id,U_EvaluationsAcc,S#state.op_mode),
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
			%gather_STATS(S#state.population_id,S#state.evaluations_acc,S#state.op_mode),
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
			case whereis(benchmark) of
				undefined ->	
					 ok;
				PId ->
					PId ! {Population_Id,completed,T},
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
	F = fun()->
		case CitizenType of
			top ->
				extract_TopDXIds(Specie_Ids,[]);
			champions ->
				extract_ChampionDXIds(Specie_Ids,[]);
			all ->
				extract_AllDXIds(Specie_Ids,[])
		end
	end,
	{atomic,Results}=mnesia:transaction(F),
	Results.
	
	extract_AllDXIds([Specie_Id|Specie_Ids],Acc)->
		io:format("Specie_Id:~p~n",[Specie_Id]),
		[S] = mnesia:read({specie,Specie_Id}),
		DX_Ids = S#specie.dx_ids,
		io:format("DX_Ids:~p~n",[DX_Ids]),
		extract_AllDXIds(Specie_Ids,lists:append(DX_Ids,Acc));
	extract_AllDXIds([],Acc)->
%		io:format("DX_Ids:~p~n",[Acc]),
		Acc.

	extract_TopDXIds([Specie_Id|Specie_Ids],Acc)->
		[S] = mnesia:read({specie,Specie_Id}),
		SHOF = S#specie.hall_of_fame,
		SortedChampions=lists:reverse(lists:sort([{C#champion.main_fitness,C#champion.id} || C <- SHOF])),
		TopDX_Ids = lists:sublist(SortedChampions,3),
		extract_TopDXIds(Specie_Ids,[{TopDX_Ids,Specie_Id}|Acc]);
	extract_TopDXIds([],Acc)->
		Acc.
	
	extract_ChampionDXIds([Specie_Id|Specie_Ids],Acc)->
		[S] = mnesia:read({specie,Specie_Id}),
		SHOF = S#specie.hall_of_fame,
		SortedChampions=lists:reverse(lists:sort([{C#champion.main_fitness,C#champion.id} || C <- SHOF])),
		extract_ChampionDXIds(Specie_Ids,[{SortedChampions,Specie_Id}|Acc]);
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
	init_population(?INIT_PMP).

init_population(PMP)->
	{A,B,C} = now(),
	random:seed(A,B,C),
	F = fun()->
		case mnesia:read({population,PMP#pmp.population_id}) of
			[] ->
				create_Population(PMP#pmp.population_id,PMP#pmp.specie_constraint);
			_ ->
				delete_population(PMP#pmp.population_id),
				create_Population(PMP#pmp.population_id,PMP#pmp.specie_constraint)
		end
	end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic,_} ->
			population_monitor:start({PMP#pmp.op_mode,PMP#pmp.population_id,PMP#pmp.evolution_type,PMP#pmp.selection_type});
		Error ->
			io:format("******** ERROR in PopulationMonitor:~p~n",[Error])
	end.

	create_Population(Population_Id,Specie_Constraints)->%Link_Form,Morphologies)->
		%TotSpecies = ?TOT_INIT_SPECIES,
		SpecieSize = ?INIT_SPECIE_SIZE,
		Polis_Id = ?INIT_POLIS,
		Specie_Ids = [create_Specie(Population_Id,technome_constructor:generate_UniqueId(),SpecieSize,[],SpecCon) || SpecCon <- Specie_Constraints],
		Seed_Agent_Ids = extract_AllDXIds(Specie_Ids,[]),
		io:format("SeedIds:~p~n",[Seed_Agent_Ids]),
		Population = #population{
			id = Population_Id,
			specie_ids = Specie_Ids,
			polis_id = Polis_Id,
			evo_strat = specie_evo_strat:init(),%TODO: Not even used, needs to be removed or completed
			seed_specie_ids=Specie_Ids,
			seed_agent_ids= Seed_Agent_Ids},
		mnesia:write(Population).
			
		create_Specie(Population_Id,Specie_Id,0,IdAcc,SpeCon)->
			io:format("Specie_Id:~p Morphology:~p~n",[Specie_Id,SpeCon#constraint.morphology]),
			Specie = #specie{
				id = Specie_Id,
				morphology = SpeCon#constraint.morphology,
				constraint = SpeCon,
				dx_ids = IdAcc,
				population_id = Population_Id,
				seed_agent_ids=IdAcc
			},
%			io:format("Specie:~p~n",[Specie]),
			mnesia:write(Specie),
			Specie_Id;
		create_Specie(Population_Id,SpecieId,Citizen_Index,IdAcc,SpeCon)->
			DX_Id = float_to_list(technome_constructor:generate_UniqueId()),
			modular_constructor:construct_DX(SpecieId,DX_Id,SpeCon),
			create_Specie(Population_Id,SpecieId,Citizen_Index-1,[DX_Id|IdAcc],SpeCon).

continue()->
	population_monitor:start({?INIT_PMP#pmp.op_mode,?INIT_PMP#pmp.population_id,?INIT_PMP#pmp.evolution_type,?INIT_PMP#pmp.selection_type}).
continue(OpMode,Evolution_Type,Selection_Type)->
	Population_Id = test,
	population_monitor:start({OpMode,Population_Id,Evolution_Type,Selection_Type}).

%--------------------------------Evolve Top Technomes--------------------------------
%%%Notes: Species
%%%Function: 
%%%Interface:Input:() Output:
%%%MsgComunication: N/A
selection(Population_Id,SelectionType)->
	F = fun()->
		[P] = mnesia:read({population,Population_Id}),
		%Distinguishers = P#population.distinguishers,
		%Objectives = P#population.objectives,
		Specie_Ids = P#population.specie_ids,
		%update_HallOfFame_Population(Population_Id),
		NewGen_Population=lists:flatten([updateHOF_and_Select(Specie_Id,SelectionType) || Specie_Id <- Specie_Ids])
	end,
	{atomic,_} = mnesia:transaction(F).

	updateHOF_and_Select(Specie_Id,SelectionType)->
		[S] = mnesia:read({specie,Specie_Id}),
		%Objectives = S#specie.objectives,
		Distinguishers = S#specie.hof_distinguishers,
		Agent_Ids = S#specie.dx_ids,
		SHOF = S#specie.hall_of_fame,
		{U_SHOF,Losers}=update_SHOF(SHOF,Agent_Ids,Distinguishers,[]),
		%io:format("SHOF:~p~nU_SHOF:~p~n",[SHOF,U_SHOF]),
		U_S = S#specie{hall_of_fame=U_SHOF},
		mnesia:write(U_S),
		
		NewGen_SpecieAgents=case ?INTERACTIVE_SELECTION of
			false ->
				?MODULE:SelectionType(Specie_Id,Losers);
			true ->%io:format("U_SHOF:~p~n",[U_SHOF]),
				interactive_evolution:select(Specie_Id,Losers)
		end,
		%NewGen_SpecieAgents=?MODULE:evolvability_research(Specie_Id,RemainingChampionDesignators),%USE THIS TO TAKE into account the evolvability or robustness.
		%io:format("NewGen_SpecieAgents:~p~n",[NewGen_SpecieAgents]),
		NewGen_SpecieAgents.
		
		update_SHOF(SHOF,[Agent_Id|Agent_Ids],Distinguishers,Acc)->
			case update_SHOF(SHOF,Agent_Id,Distinguishers) of
				{U_SHOF,undefined} ->
					update_SHOF(U_SHOF,Agent_Ids,Distinguishers,Acc);
				{U_SHOF,Loser} ->
					update_SHOF(U_SHOF,Agent_Ids,Distinguishers,[Loser|Acc])
			end;
		update_SHOF(SHOF,[],_Distinguishers,Acc)->
			{SHOF,Acc}.

			update_SHOF(SHOF,Agent_Id,Distinguishers)->%Will return {U_SHOF,Champion|undefined}
				Agent = to_champion_form(SHOF,Agent_Id,Distinguishers),
				case [C|| C<-SHOF, Agent#champion.hof_fingerprint==C#champion.hof_fingerprint] of %lists:keyfind(Agent#champion.hof_fingerprint, 2, SHOF) of
						[] ->%Champion with such fingerprint does not exist, thus it is entered, as a stepping stone, into the HOF
							[A] = mnesia:read({dx,Agent#champion.id}),
							U_A = A#dx{champion_flag=[true|A#dx.champion_flag]},
							mnesia:write(U_A),
							%retrograde_update(A#dx.parent_ids,A#dx.main_fitness,A#dx.fitness,0),
							{[Agent|SHOF],undefined};
						Champs ->%Agent exists, and is either entered or not into HOF based on fitness dominance... or behavioral minimal difference.							
							SHOF_Remainder = SHOF -- Champs,
							case fitness_domination(Agent,Champs) of
								false ->
									case on_pareto_front(Agent,Champs) of
										false ->
											case novel_behavior(Agent,Champs) of
												false ->
													%io:format("Agent not added:~p~n",[{Agent}]),
													{SHOF,Agent};
												U_Champs ->
													{SHOF_Remainder++U_Champs,undefined}
											end;
										U_Champs ->
											{SHOF_Remainder++U_Champs,undefined}
									end;
								U_Champs ->
									{SHOF_Remainder++U_Champs,undefined}
							end									
					end.
			
					fitness_domination(Agent,SHOF)->
						case fitness_domination(Agent,SHOF,[],[]) of
							{[],_} ->
								%io:format("NOT ADDING, fitness_domination:~p~n",[{Agent}]),
								false;
							{LoserAcc,RemainingChamps}->
								%io:format("ADDING, fitness_domination:~p~n",[{Agent}]),
								[A] = mnesia:read({dx,Agent#champion.id}),
								U_A = A#dx{champion_flag=[true|A#dx.champion_flag]},
								mnesia:write(U_A),
								[Agent|RemainingChamps]
						end.
						fitness_domination(Agent,[Champ|Champs],LoserAcc,Acc)->
							case Agent#champion.hof_fingerprint == Champ#champion.hof_fingerprint of
								true ->
									case length([1||Val<-vecdiff(Agent#champion.fitness,Champ#champion.fitness,[]), Val > 0]) == length(Champ#champion.fitness) of
										true ->
											%We should still keep it though, if it's within some range of the dominating agent, and the system does not use genetic exploration.
											case (Champ#champion.main_fitness > Agent#champion.main_fitness*?MIN_ACCEPTABLE_FITNESS_RATIO) of
												true ->
													fitness_domination(Agent,Champs,LoserAcc,[Champ|Acc]);
												false ->
													[ChampA] = mnesia:read({dx,Champ#champion.id}),
													U_ChampA = ChampA#dx{champion_flag=[lost|ChampA#dx.champion_flag]},%true, false, lost, rentered
													mnesia:write(U_ChampA),
													fitness_domination(Agent,Champs,[Champ|LoserAcc],Acc)
											end;
										false ->
											{[],void}
									end;
								false ->
									fitness_domination(Agent,Champs,LoserAcc,[Champ|Acc])
							end;
						fitness_domination(_Agent,[],LoserAcc,Acc)->
							{LoserAcc,Acc}.
					
					on_pareto_front(Agent,SHOF)->
						case opf(Agent,SHOF) of
							false ->
								%io:format("NOT ADDING, on_pareto_front:~p~n",[{Agent}]),
								false;
							true ->
								%io:format("ADDING, on_pareto_front:~p~n",[{Agent}]),
								[A] = mnesia:read({dx,Agent#champion.id}),
								U_A = A#dx{champion_flag=[true|A#dx.champion_flag]},
								mnesia:write(U_A),
								[Agent|SHOF]
						end.
							
						opf(Agent,[Champ|Champs])->
							case length([1||Val<-vecdiff(Agent#champion.fitness,Champ#champion.fitness,[]), Val > 0]) > 0 of
								true ->
									opf(Agent,Champs);
								false ->
									false
							end;
						opf(_Agent,[])->
							true.
							
					novel_behavior(Agent,SHOF)->
						Minimal_Novelty=case get(minimal_novelty) of
							undefined ->
								put(minimal_novelty,2),
								2;
							Val ->
								Val
						end,
						MainFitness_List = [C#champion.main_fitness || C<-SHOF],
						SHOF_AvgFitness = functions:avg(MainFitness_List),
						SHOF_STD = functions:std(MainFitness_List,SHOF_AvgFitness,[]),
						%Minimal_Novelty = 2,
						Minimal_Fitness = SHOF_AvgFitness-0.1*abs(SHOF_AvgFitness),
						case (lists:min(Agent#champion.behavioral_differences) > Minimal_Novelty) and (Agent#champion.main_fitness > Minimal_Fitness) of
							true ->
								%io:format("ADDING, and the min different is:~p~n",[{lists:min(Agent#champion.behavioral_differences),Minimal_Novelty,Agent#champion.behavioral_differences}]),
								put(minimal_novelty,Minimal_Novelty+Minimal_Novelty*0.5+0.05),
								[A] = mnesia:read({dx,Agent#champion.id}),
								U_A = A#dx{champion_flag=[true|A#dx.champion_flag]},
								mnesia:write(U_A),
								[Agent|SHOF];
							false ->
								%io:format("NOT Adding, and the min different is:~p~n",[{lists:min(Agent#champion.behavioral_differences),Minimal_Novelty,Agent#champion.behavioral_differences}]),
								put(minimal_novelty,Minimal_Novelty-Minimal_Novelty*0.05+0.05),
								false
						end.
					
					to_champion_form(SHOF,Agent_Id,Distinguishers)->
						[A]=mnesia:read({dx,Agent_Id}),
						Behavioral_Differences=case ?BEHAVIORAL_TRACE of
							true ->%io:format("behavioral trace:~p~n",[A#dx.behavioral_trace]),
								case phenotypic_diversity:compare_behavior(SHOF,A#dx.behavioral_trace) of
									[]->
										[0];
									Comparisons ->
										Comparisons
								end;
							false ->
								[0]
						end,
						#champion{
							hof_fingerprint=[specie_identifier:Distinguisher(Agent_Id)|| Distinguisher <- Distinguishers],
							fitness = A#dx.fitness,%A list of multi objective fitnesses and stuff.
							main_fitness = A#dx.main_fitness, % A single fitness value that we can use to decide on probability of offspring creation.
							tot_n = length(A#dx.n_ids),
							id = Agent_Id,
							evolvability=A#dx.evolvability,
							robustness=A#dx.robustness,
							brittleness=A#dx.brittleness,
							generation=A#dx.generation,
							behavioral_differences = Behavioral_Differences
						}.
				
				vecdiff([A|V1],[B|V2],Acc)->
					vecdiff(V1,V2,[A-B|Acc]);
				vecdiff([],[],Acc)->
					Acc.
				
				retrograde_update(_AncestorIds,_Main_Fitness,_Fitness,10)->
					ok;
				retrograde_update([AncestorId|AncestorIds],Main_Fitness,Fitness,DepthIndex)->
					[A] = mnesia:read({dx,AncestorId}),
					Ancestor_MainFitness = A#dx.main_fitness,
					Evolvability = A#dx.evolvability,
					Robustness = A#dx.robustness,
					Brittleness = A#dx.brittleness,
					case Main_Fitness > (Ancestor_MainFitness + Ancestor_MainFitness*0.05) of
						true ->%Update evolvability & robustness
							%AncestralDepthAmplifier = (math:pow(1.1,DepthIndex)),%TODO: This defines how the fitness is updated retroactively
							%PercentageBasedAddition = 0.1, 
							%U_Ancestor_MainFitness = AncestralDepthAmplifier*PercentageBasedAddition,
							U_Evolvability = Evolvability + 1,
							U_Robustness = Robustness,
							U_Brittleness = Brittleness;
						false ->%If within 10%, still update robustness
							case Main_Fitness > (Ancestor_MainFitness - Ancestor_MainFitness*0.05) of
								true -> 
									U_Evolvability = Evolvability,
									U_Robustness = Robustness+1,
									U_Brittleness = Brittleness;
								false ->
									U_Evolvability = Evolvability,
									U_Robustness = Robustness,
									U_Brittleness = Brittleness+1
							end
					end,
					U_A = A#dx{
						evolvability = U_Evolvability,
						robustness = U_Robustness,
						brittleness = U_Brittleness
					},
					mnesia:write(U_A),
					retrograde_update(A#dx.parent_ids,Main_Fitness,Fitness,DepthIndex+1);
				retrograde_update([],_Main_Fitness,_Fitness,_Depth_Index)->
					ok.
	
		evolvability_research(Specie_Id)->
			[S] = mnesia:read({specie,Specie_Id}),
			io:format("S:~p~n",[S]),
			mnesia:write(S#specie{dx_ids=[]}),
			Distinguishers = S#specie.hof_distinguishers,
			Agent_Ids = get_SpecieAgentIds(Specie_Id),
			Champions=[to_champion_form(S#specie.hall_of_fame,Agent_Id,Distinguishers) || Agent_Id <-Agent_Ids],
			FitnessScaled=[{Champ#champion.main_fitness*math:pow(1.1,Champ#champion.evolvability),Champ#champion.id}||Champ<-Champions,Champ#champion.generation>0],
			TotFitness = lists:sum([Fitness || {Fitness,_Id}<-FitnessScaled]),
			NewGen_Ids=choose_Winners(Specie_Id,FitnessScaled,TotFitness,[],[],?SPECIE_SIZE_LIMIT),
			io:format("NewGen_Ids:~p~n",[NewGen_Ids]),
			mnesia:write(S#specie{dx_ids=NewGen_Ids}),
			NewGen_Ids.
			
			get_SpecieAgentIds(Specie_Id)->
				[S] = mnesia:dirty_read({specie,Specie_Id}),
				%io:format("****Specie_Id:~p****~n",[Specie_Id]),
				lists:append(S#specie.seed_agent_ids,[get_GeneticLineIds(Agent_Id,1)||Agent_Id<-S#specie.seed_agent_ids]).
		
				get_GeneticLineIds(Agent_Id,Generation)->
					[A] = mnesia:dirty_read({dx,Agent_Id}),
					%io:format("Generation:~p Agent Id:~p~n",[Generation,Agent_Id]),
					lists:append(A#dx.offspring_ids,[get_GeneticLineIds(Id,Generation+1) || Id <- A#dx.offspring_ids]).

		hof_competition(Specie_Id,RemainingChampionDesignators)->%returns a list of new generation of agents for a single specie
			[S] = mnesia:read({specie,Specie_Id}),
			%io:format("S:~p~n",[S]),
			mnesia:write(S#specie{dx_ids=[]}),
			SHOF = S#specie.hall_of_fame,
			NewGen_Ids=case ?SHOF_RATIO < 1 of
				true ->
					Agent_Ids = S#specie.dx_ids,
					Distinguishers = S#specie.hof_distinguishers,
					%Actives = to_champion_form(Agent_Ids,Distinguishers,[]) -- SHOF,
					Actives = RemainingChampionDesignators,
					%io:format("SHOF:~p~n",[SHOF]),
					%io:format("Actives:~p~n",[Actives]),
					SHOF_FitnessScaled=[{Champ#champion.main_fitness/math:pow(Champ#champion.tot_n,?EFF),Champ#champion.id}||Champ<-SHOF],
					Active_FitnessScaled=[{Ac#champion.main_fitness/math:pow(Ac#champion.tot_n,?EFF),Ac#champion.id}||Ac<-Actives],
					TotFitnessActives = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-Active_FitnessScaled]),
					TotFitnessSHOFs = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-SHOF_FitnessScaled]),
					%io:format("TotFitnessActives:~p~nTotFitnessSHOFs:~p~n",[TotFitnessActives,TotFitnessSHOFs]),
					choose_Winners(Specie_Id,Active_FitnessScaled,TotFitnessActives,[],[],round((1-?SHOF_RATIO)*?SPECIE_SIZE_LIMIT))++
					choose_Winners(Specie_Id,SHOF_FitnessScaled,TotFitnessSHOFs,[],[],round(?SHOF_RATIO*?SPECIE_SIZE_LIMIT));
				false ->
					%io:format("SHOF:~p~n",[SHOF]),
					Allotments=[{Champ#champion.main_fitness/math:pow(Champ#champion.tot_n,?EFF),Champ#champion.id}||Champ<-SHOF],
					%io:format("Allotments:~p~n",[Allotments]),
					Tot = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-Allotments]),
					%io:format("Tot:~p~n",[Tot]),
					choose_Winners(Specie_Id,Allotments,Tot,[],[],?SPECIE_SIZE_LIMIT)
			end,
			io:format("NewGen_Ids:~p~n",[NewGen_Ids]),
			[U_S] = mnesia:read({specie,Specie_Id}),
			mnesia:write(U_S#specie{dx_ids=NewGen_Ids}),
			NewGen_Ids.
		
		hof_rank(Specie_Id,RemainingChampionDesignators)->
			[S] = mnesia:read({specie,Specie_Id}),
			io:format("S:~p~n",[S]),
			mnesia:write(S#specie{dx_ids=[]}),
			SHOF = S#specie.hall_of_fame,
			NewGen_Ids=case ?SHOF_RATIO < 1 of
				true ->
					Agent_Ids = S#specie.dx_ids,
					Distinguishers = S#specie.hof_distinguishers,
					%Actives = to_champion_form(Agent_Ids,Distinguishers,[]) -- SHOF,
					Actives = RemainingChampionDesignators,
					io:format("SHOF:~p~n",[SHOF]),
					io:format("Actives:~p~n",[Actives]),
					Actives_Ranked=assign_rank(lists:sort([{Ac#champion.main_fitness,Ac#champion.id}||Ac<-Actives]), lists:seq(1,length(Actives)),[]),
					SHOF_Ranked=assign_rank(lists:sort([{Champ#champion.main_fitness,Champ#champion.id}||Champ<-SHOF]), lists:seq(1,length(SHOF)),[]),
					TotFitnessActives = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-Actives_Ranked]),
					TotFitnessSHOFs = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-SHOF_Ranked]),
					io:format("Actives_Ranked:~p~nSHOF_Ranked:~p~n",[Actives_Ranked,SHOF_Ranked]),
					choose_Winners(Specie_Id,Actives_Ranked,TotFitnessActives,[],[],round((1-?SHOF_RATIO)*?SPECIE_SIZE_LIMIT))++
					choose_Winners(Specie_Id,SHOF_Ranked,TotFitnessSHOFs,[],[],round(?SHOF_RATIO*?SPECIE_SIZE_LIMIT));
					
				false ->
					SHOF = S#specie.hall_of_fame,
					Allotments=assign_rank(lists:sort([{Champ#champion.main_fitness,Champ#champion.id}||Champ<-SHOF]),lists:seq(1,length(SHOF)),[]),
					Tot = lists:sum([Val || {Val,_Id}<-Allotments]),
					choose_Winners(Specie_Id,Allotments,Tot,[],[],?SPECIE_SIZE_LIMIT)
			end,
			io:format("NewGen_Ids:~p~n",[NewGen_Ids]),
			[U_S] = mnesia:read({specie,Specie_Id}),
			mnesia:write(U_S#specie{dx_ids=NewGen_Ids}),
			NewGen_Ids.
			
			assign_rank([{_MainFitness,Agent_Id}|Champions],[Rank|RankList],Acc)->
				assign_rank(Champions,RankList,[{Rank,Agent_Id}|Acc]);
			assign_rank([],[],Acc)->
				io:format("Rank:~p~n",[Acc]),
				Acc.
			
		hof_top3(Specie_Id,_RemainingChampionDesignators)->
			[S] = mnesia:read({specie,Specie_Id}),
			mnesia:write(S#specie{dx_ids=[]}),
			SHOF = S#specie.hall_of_fame,
			Allotments = lists:sublist(lists:reverse(lists:sort([{Champ#champion.main_fitness,Champ#champion.id}||Champ<-SHOF])),3),
			Tot = lists:sum([Val || {Val,_Id}<-Allotments]),
			io:format("SHOF:~p~n",[SHOF]),
			io:format("Allotments:~p~n",[Allotments]),
			NewGen_Ids=choose_Winners(Specie_Id,Allotments,Tot,[],[],?SPECIE_SIZE_LIMIT),
			io:format("NewGen_Ids:~p~n",[NewGen_Ids]),
			[U_S] = mnesia:read({specie,Specie_Id}),
			mnesia:write(U_S#specie{dx_ids=NewGen_Ids}),
			NewGen_Ids.
		
		hof_efficiency(Specie_Id,RemainingChampionDesignators)->
			[S] = mnesia:read({specie,Specie_Id}),
			mnesia:write(S#specie{dx_ids=[]}),
			SHOF = S#specie.hall_of_fame,
			NewGen_Ids=case ?SHOF_RATIO < 1 of
				true ->
					Agent_Ids = S#specie.dx_ids,
					Distinguishers = S#specie.hof_distinguishers,
					%Actives = to_champion_form(Agent_Ids,Distinguishers,[]),
					Actives = RemainingChampionDesignators,
					io:format("SHOF:~p~n",[SHOF]),
					io:format("Actives:~p~n",[Actives]),
					Active_NeuralEfficiencyScaled=[{Ac#champion.main_fitness/Ac#champion.tot_n,Ac#champion.id}||Ac<-Actives],
					SHOF_NeuralEfficiencyScaled=[{Champ#champion.main_fitness/Champ#champion.tot_n,Champ#champion.id}||Champ<-SHOF],
					TotFitnessActives = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-Active_NeuralEfficiencyScaled]),
					TotFitnessSHOFs = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-SHOF_NeuralEfficiencyScaled]),
					io:format("TotFitnessActives:~p~nTotFitnessSHOFs:~p~n",[TotFitnessActives,TotFitnessSHOFs]),
					choose_Winners(Specie_Id,Active_NeuralEfficiencyScaled,TotFitnessActives,[],[],round((1-?SHOF_RATIO)*?SPECIE_SIZE_LIMIT))++
					choose_Winners(Specie_Id,SHOF_NeuralEfficiencyScaled,TotFitnessSHOFs,[],[],round(?SHOF_RATIO*?SPECIE_SIZE_LIMIT));
				false ->
					io:format("SHOF:~p~n",[SHOF]),
					SHOF_NeuralEfficiencyScaled=[{Champ#champion.main_fitness/Champ#champion.tot_n,Champ#champion.id}||Champ<-SHOF],
					io:format("SHOF_NeuralEfficiencyScaled:~p~n",[SHOF_NeuralEfficiencyScaled]),
					TotFitnessSHOFs = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-SHOF_NeuralEfficiencyScaled]),
					io:format("TotFitnessSHOFs:~p~n",[TotFitnessSHOFs]),
					choose_Winners(Specie_Id,SHOF_NeuralEfficiencyScaled,TotFitnessSHOFs,[],[],?SPECIE_SIZE_LIMIT)
			end,
			io:format("NewGen_Ids:~p~n",[NewGen_Ids]),
			[U_S] = mnesia:read({specie,Specie_Id}),
			mnesia:write(U_S#specie{dx_ids=NewGen_Ids}),
			NewGen_Ids.
			
		hof_random(Specie_Id,RemainingChampionDesignators)->
			[S] = mnesia:read({specie,Specie_Id}),
			mnesia:write(S#specie{dx_ids=[]}),
			SHOF = S#specie.hall_of_fame,
			NewGen_Ids=case ?SHOF_RATIO < 1 of
				true ->
					Agent_Ids = S#specie.dx_ids,
					Distinguishers = S#specie.hof_distinguishers,
					%Actives = to_champion_form(Agent_Ids,Distinguishers,[]),
					Actives = RemainingChampionDesignators,
					io:format("SHOF:~p~n",[SHOF]),
					io:format("Actives:~p~n",[Actives]),
					Active_RandomScaled=[{1,Ac#champion.id}||Ac<-Actives],
					SHOF_RandomScaled=[{1,Champ#champion.id}||Champ<-SHOF],
					TotFitnessActives = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-Active_RandomScaled]),
					TotFitnessSHOFs = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-SHOF_RandomScaled]),
					io:format("TotFitnessActives:~p~nTotFitnessSHOFs:~p~n",[TotFitnessActives,TotFitnessSHOFs]),
					choose_Winners(Specie_Id,Active_RandomScaled,TotFitnessActives,[],[],round((1-?SHOF_RATIO)*?SPECIE_SIZE_LIMIT))++
					choose_Winners(Specie_Id,SHOF_RandomScaled,TotFitnessSHOFs,[],[],round(?SHOF_RATIO*?SPECIE_SIZE_LIMIT));
				false ->
					SHOF = S#specie.hall_of_fame,
					io:format("SHOF:~p~n",[SHOF]),
					SHOF_RandomScaled=[{1,Champ#champion.id}||Champ<-SHOF],
					TotFitnessSHOFs = lists:sum([Main_Fitness || {Main_Fitness,_Id}<-SHOF_RandomScaled]),
					io:format("TotalOffspring:~p~n",[round(?SHOF_RATIO*?SPECIE_SIZE_LIMIT)]),
					choose_Winners(Specie_Id,SHOF_RandomScaled,TotFitnessSHOFs,[],[],?SPECIE_SIZE_LIMIT)
			end,
			io:format("NewGen_Ids:~p~n",[NewGen_Ids]),
			[U_S] = mnesia:read({specie,Specie_Id}),
			mnesia:write(U_S#specie{dx_ids=NewGen_Ids}),
			NewGen_Ids.

			choose_Winners(Specie_Id,Agents,TotalFitness,OffspringAcc,ReentryAcc,0)->
				reenter(ReentryAcc,Specie_Id),
				OffspringAcc++ReentryAcc;
			choose_Winners(Specie_Id,Agents,TotalFitness,OffspringAcc,ReentryAcc,AgentIndex)->
				%io:format("1~n"),
				case choose_Winner(Specie_Id,Agents,(random:uniform(100)/100)*TotalFitness,0) of
					{OffspringId,offspring}->%io:format("2~n"),
						choose_Winners(Specie_Id,Agents,TotalFitness,[OffspringId|OffspringAcc],ReentryAcc,AgentIndex-1);
					{Agent_Id,reentry}->%io:format("3~n"),
						case lists:member(Agent_Id,ReentryAcc) of
							true ->%io:format("4~n"),
								choose_Winners(Specie_Id,Agents,TotalFitness,OffspringAcc,ReentryAcc,AgentIndex);
							false ->%io:format("5~n"),
								choose_Winners(Specie_Id,Agents,TotalFitness,OffspringAcc,[Agent_Id|ReentryAcc],AgentIndex-1)
						end
						
				end.
				
				reenter([Agent_Id|ReentryIds],Specie_Id)->
					io:format("REENTERING:~p~n",[Agent_Id]),
					[S] = mnesia:read({specie,Specie_Id}),
					SHOF = S#specie.hall_of_fame,
					U_SHOF = lists:keydelete(Agent_Id, 3, SHOF),
					U_S = S#specie{hall_of_fame=U_SHOF},
					%U_S = S#specie{hall_of_fame=U_SHOF,dx_ids=[Agent_Id|S#specie.dx_ids]},
					[A] = mnesia:read({dx,Agent_Id}),
					U_A = A#dx{champion_flag=[rentered|A#dx.champion_flag]},%true, false, lost, rentered
					mnesia:write(U_S),
					mnesia:write(U_A),
					reenter(ReentryIds,Specie_Id);
					%Remove agent from phof and shof, tag re-entry (not lost)
				reenter([],_Specie_Id)->
					ok.
				
			choose_Winner(Specie_Id,[{_PortionSize,Agent_Id}],_Index,_Acc)->
				case random:uniform() =< ?REENTRY_PROBABILITY of
					true ->
						{Agent_Id,reentry};
					false ->
						[A] = mnesia:read({dx,Agent_Id}),
						OffspringAgent_Id = create_MutantDXCopy(Agent_Id),
						U_A = A#dx{offspring_ids=[OffspringAgent_Id|A#dx.offspring_ids]},%true, false, lost, rentered
						mnesia:write(U_A),
						[OffspringA] = mnesia:read({dx,OffspringAgent_Id}),
						U_OffspringA = OffspringA#dx{champion_flag=[false|OffspringA#dx.champion_flag]},
						mnesia:write(U_OffspringA),
						{OffspringAgent_Id,offspring}
						%choose agent as parent
						%create clone, mutate clone, return offspring
				end;
			choose_Winner(Specie_Id,[{PortionSize,Agent_Id}|Allotments],Index,Acc)->
				%io:format("Index:~p~n",[Index]),
				case (Index > Acc) and (Index =< Acc+PortionSize) of
					true ->%io:format("WIndex:~p~n",[Index]),
						case random:uniform() =< ?REENTRY_PROBABILITY of
							true ->
								{Agent_Id,reentry};
							false ->
								[A] = mnesia:read({dx,Agent_Id}),
								OffspringAgent_Id = create_MutantDXCopy(Agent_Id),
								U_A = A#dx{offspring_ids=[OffspringAgent_Id|A#dx.offspring_ids]},%true, false, lost, rentered
								mnesia:write(U_A),
								[OffspringA] = mnesia:read({dx,OffspringAgent_Id}),
								U_OffspringA = OffspringA#dx{champion_flag=[false|OffspringA#dx.champion_flag]},
								mnesia:write(U_OffspringA),
								{OffspringAgent_Id,offspring}
								%choose agent as parent
								%create clone, mutate clone, return offspring
						end;
					false ->
						choose_Winner(Specie_Id,Allotments,Index,Acc+PortionSize)
				end.
		
		extract_DXSummaries([DX_Id|DX_Ids],Acc)->
			[DX] = mnesia:read({dx,DX_Id}),
			Fitness = DX#dx.fitness,
			Profile = DX#dx.profile,
			Summary = {Fitness,Profile,DX_Id},
			extract_DXSummaries(DX_Ids,[Summary|Acc]);
		extract_DXSummaries([],Acc)->
			Acc.
		
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
	io:format("delete_population::specie_ids:~p~n",[Specie_Ids]),
	[delete_specie(Specie_Id) || Specie_Id <- Specie_Ids],
	mnesia:delete({population,Population_Id}).
	
	delete_specie(Specie_Id)->
		[S] = mnesia:read({specie,Specie_Id}),
		%Every agent is an offspring of someone in the seed population
		%because there is only a single parent to an agent, starting from seed agents, it is possible to delete everyone, if we delete all offspring
		Seed_Agent_Ids = S#specie.seed_agent_ids,
		io:format("delete_specie::seed_agent_ids:~p~n",[Seed_Agent_Ids]),
		[delete_genetic_line(Agent_Id) || Agent_Id <- Seed_Agent_Ids],
		mnesia:delete({specie,Specie_Id}).
		
		delete_genetic_line(Agent_Id)->
			io:format("delete_genetic_line(~p)~n",[Agent_Id]),
			[A] = mnesia:read({dx,Agent_Id}),
			delete_dx(A#dx.id),
			Offspring_Ids = A#dx.offspring_ids,
			io:format("Offspring Ids:~p~n",[Offspring_Ids]),
			[delete_genetic_line(Id) || Id <- Offspring_Ids].
			
		delete_dx(Agent_Id)->
			io:format("Delete_DX(DX_Id):~p~n",[Agent_Id]),
			technome_constructor:delete_dx(Agent_Id).
			
		delete_dx(Agent_Id,safe)->
			F = fun()->
				[A] = mnesia:read({dx,Agent_Id}),
				[Specie] = mnesia:read({specie,A#dx.specie_id}),
				Agent_Ids = Specie#specie.dx_ids,
				DeadPool = Specie#specie.dead_pool,
				mnesia:write(Specie#specie{dx_ids = lists:delete(Agent_Id,Agent_Id),dead_pool=lists:keydelete(Agent_Id,3,DeadPool)}),
				technome_constructor:delete_dx(Agent_Id)
			end,
			Result=mnesia:transaction(F),
			io:format("Delete_DX(Agent_Id,safe):~p Result:~p~n",[Agent_Id,Result]).
			
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
		Specie_Evaluations = get({evaluations,Specie_Id}),
		put({evaluations,Specie_Id},0),
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		{Avg_SubCores,SubCores_Std,Avg_Neurons,Neurons_Std} = calculate_SpecieAvgNodes({specie,S}),
		{MaxFitness,MinFitness,AvgFitness,Fitness_Std} = calculate_SpecieFitness({specie,S}),
		io:format("STAT result:~p~n",[{MaxFitness,MinFitness,AvgFitness,Fitness_Std}]),
		SpecieDiversity = calculate_SpecieDiversity({specie,S}),
		Validation_FitnessP=validation_testing(Specie_Id,OpModes),
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
			validation_fitness = Validation_FitnessP,
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
		mnesia:write(S#specie{trace=U_Trace}),
		STAT.

calculate_SpecieAvgNodes({specie,S})->
	io:format("SpecieAvgNodes:~p~n",[S]),
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
		length(S#specie.hall_of_fame);
		%Diversity = calculate_diversity(DX_Ids);
	calculate_SpecieDiversity(Specie_Id)->
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		length(S#specie.hall_of_fame).
		%calculate_SpecieDiversity({specie,S}).
		
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
	{MaxF,MinF,AvgFitness,Std}=calculate_SpecieFitness(Specie_Id),
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
%		DX_Ids = S#specie.dx_ids,%TODO Should it be just the currently active agents. dx_ids, or also those in the hall of fame? NO IT SHOULD BE ONLY THOSE IN THE HALL OF FAME!!! must fix this before updating. 
%		io:format("DX_Ids:~p~n",[DX_Ids]),
		SHOF = S#specie.hall_of_fame,
		FitnessAcc=la([C#champion.fitness || C <- SHOF],[]),
%		FitnessAcc = calculate_fitness(DX_Ids),
		case FitnessAcc of
			[] ->
				{[],[],[],[]};
			FitnessAcc ->
				{
					[lists:max(L)||L<-FitnessAcc],
					[lists:min(L)||L<-FitnessAcc],
					[functions:avg(L)||L<-FitnessAcc],
					[functions:std(L)||L<-FitnessAcc]
				}
		end;
	calculate_SpecieFitness(Specie_Id)->
		[S] = mnesia:dirty_read({specie,Specie_Id}),
		calculate_SpecieFitness({specie,S}).
		
			la([List|ListOfLists],Acc)->
				la(ListOfLists,list_append(List,Acc));
			la([],Acc)->
				lists:reverse(Acc).
				
			list_append([],[])->
				[];
			list_append(ListA,[])->
				[[Val]||Val<-ListA];
			list_append(ListA,ListB)->
				list_append(ListA,ListB,[]).
			list_append([Val|ListA],[AccB|ListB],Acc)->
				list_append(ListA,ListB,[[Val|AccB]|Acc]);
			list_append([],[],Acc)->
				lists:reverse([lists:reverse(List) || List<-Acc]).
		
		validation_testing(Specie_Id,OpModes)->
			[S] = mnesia:read({specie,Specie_Id}),
			SHOF = S#specie.hall_of_fame,
			SortedChampions=lists:reverse(lists:sort([{C#champion.main_fitness,C#champion.id} || C <- SHOF])),
			io:format("Sorted champions:~p~n",[SortedChampions]),
			case lists:member(validation,OpModes) of
				true ->
					Champion_Id = case SortedChampions of
						[{Champ_TrnFitness,Champ_Id}] ->
							Champ_Id;
						[{Champ_TrnFitness,Champ_Id}|_] ->
							Champ_Id;
						[]->
							void
					end,
					case Champion_Id of
						void ->
							{[],void};
						_ ->
							{ok,Champion_PId}= exoself:start_link({validation,Champion_Id,1,self()}),
							receive
								{Champion_Id,ValFitness,FitnessProfile}->
									{ValFitness,Champion_Id}
							end
					end;
				false ->
					{0,void}
			end.

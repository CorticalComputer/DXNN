%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.
-module(benchmark).
-compile(export_all).
-include("forex_db.hrl").
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Benchmark Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DIR,"benchmarks/").
-define(TOT_RUNS,5).
-define(DEFAULT_POPULATION_ID,test).
-define(BENCHMARK_MORPHOLOGIES,[forex_trader]).
-define(DEFAULT_OPMODE,gt).
-define(DEFAULT_ST,competition).
-define(CONSTRAINTS,[#constraint{morphology=Morphology,sc_types=SC_Types, sc_hypercube_plasticity=[none],sc_neural_linkform=LinkForm}|| Morphology<-[forex_trader],LinkForm<-[feedforward], SC_Types<-[[hypercube]]]).
-record(state,{pm_parameters,table_name,run_index=1,tot_evaluations=0,tot_generations=0,goal_status,tunning_status,success_acc=[],failure_acc=[],diversity_acc=[]}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Starts and ends Neural Networks with various preset parameters and options, and polls the logger for information about each run.
r()-> spawn(benchmark,run,[]).
run()->
	case whereis(benchmark) of
		undefined ->
			io:format("Benchmark server was not started, can not run.~n");
		PId ->
			PId ! {self(),start_benchmark},
			receive 
				{PId,Results}->
					io:format("Benchmark Results:~p~n",[Results]);
				Msg ->
					io:format("Unkown Msg:~p~n",[Msg])
			end
	end.
	
s()->
	benchmark ! {undefined,start_benchmark}.
	
start()->
	PId = spawn(benchmark,server,[void]),
	register(benchmark,PId).

stop()->
	benchmark ! {self(),stop}.
	
server(void)->
	receive
		{From, start_benchmark}->
			put(benchmark_requester,From),
			Population_Id = test,
			OpMode = ?DEFAULT_OPMODE,
			Survival_Type = ?DEFAULT_ST,
			SpeCons=?CONSTRAINTS,
			PM_Parameters = {Population_Id,SpeCons,OpMode,Survival_Type},
			TableName = ets:new(benchmark,[private,set]),
			%State = {PM_Parameters,TableName,1,{0,0},{undefined,undefined},[]},
			population_monitor:init_population(PM_Parameters),
			Init_State = #state{
				pm_parameters=PM_Parameters,
				table_name=TableName
			},
			benchmark:server(Init_State);
		{_From,start_benchmark,PM_Parameters}->
			TableName = ets:new(benchmark,[private,set]),
			%State = {PM_Parameters,TableName,1,{0,0},{undefined,undefined},[]},
			Init_State = #state{
				pm_parameters=PM_Parameters,
				table_name=TableName
			},
			population_monitor:init_population(PM_Parameters),
			benchmark:server(Init_State);
		{_From,stop}->
			io:format("Benchmark is shutting down normally.~n");
		MSG ->
			io:format("MSG:~p~n",[MSG]),
			server(void)
	end;
server(S) when (S#state.run_index >= ?TOT_RUNS) and (S#state.goal_status == done) and (S#state.tunning_status == done)->
	%-record(state,{pm_parameters,table_name,run_index=0,tot_evaluations=0,tot_generations=0,goal_status,tunning_status,success_acc=[],failure_acc=[]}).
	SuccessList = S#state.success_acc,
	FailureList = S#state.failure_acc,
	FailureRate = length(FailureList)/?TOT_RUNS,
	{TE_List,TG_List} = lists:unzip(SuccessList),
	TE_Avg = lists:sum(TE_List)/length(TE_List),
	TG_Avg = lists:sum(TG_List)/length(TG_List),
	Sorted_TE = lists:sort(TE_List),
	Sorted_TG = lists:sort(TG_List),
	[TE_Min|_] = Sorted_TE,
	[TE_Max|_] = lists:reverse(Sorted_TE),
	[TG_Min|_] = Sorted_TG,
	[TG_Max|_] = lists:reverse(Sorted_TG),
	MTN_List = get(mtn_list),
	case get(gs_list) of
		undefined ->
			Avg_GS = void;
		GS_List ->
			Avg_GS = lists:sum(GS_List)/length(GS_List),
			put(gs_list,undefined)
	end,
	put(mtn_list,undefined),
	Avg_MTN = lists:sum(MTN_List)/length(MTN_List),
	Evaluations_Avg_STD = functions:std(TE_List),
	Generations_Avg_STD = functions:std(TG_List),
	Neurons_Avg_STD = functions:std(MTN_List),
	Neurons_Min = lists:min(MTN_List),
	Neurons_Max = lists:max(MTN_List),
	%io:format("DData:~p~n",[S#state.diversity_acc]),
	DData = functions:avg_diversity(S#state.diversity_acc),
	%DData = void,
	io:format("________Benchmark Results Start________ ~n 
	SuccessList:~p
	FailureList:~p
	Diversity {List,Avg,Std}:~p
	Evaluations Avg:~p 
	Evaluations StD:~p
	Generations Avg:~p 
	Generations StD:~p
	Evaluations Min:~p 
	Evaluations Max:~p 
	Generations Min:~p 
	Generations Mag:~p 
	Neurons Avg:~p
	Neurons StD:~p
	Neurons Min:~p
	Neurons Max:~p
	Goals Avg:~p 
	FailurRate:~p 
	________Benchmark Results End________ ~n",[SuccessList,FailureList,DData,TE_Avg,Evaluations_Avg_STD,TG_Avg,Generations_Avg_STD,TE_Min,TE_Max,TG_Min,TG_Max,Avg_MTN,Neurons_Avg_STD,Neurons_Min, Neurons_Max,Avg_GS,FailureRate]),
	case get(generalisation) of
		undefined ->
			void;
		GenList ->
			GenAvgFitness = functions:avg(GenList),
			GenAvgFitness_StD = functions:std(GenList),
			GenMaxFitness = lists:max(GenList),
			GenMinFitness = lists:min(GenList),
			io:format("Generalization results:~n GenList:~p~n GenTest:~p~n Avg Fitness:~p Std:~p Max Fitness:~p Min Fitness:~p~n Trace_Acc:~p~n",[GenList,get(gen_test), GenAvgFitness, GenAvgFitness_StD, GenMaxFitness, GenMinFitness, get(trace_acc)])
	end,
	Trace_Acc = get(trace_acc),
	{ok, File} = file:open(?DIR++"generational_benchmark", write),
	lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, Trace_Acc),
	file:close(File),
	io:format("********Generational Benchmarker: Generational Benchmark complete:~p~n",[Trace_Acc]),
	Graphs = prepare_Graphs(Trace_Acc),
	write_Graphs(Graphs,"generational_benchmark"),
	case get(benchmark_requester) of
		undefined->
			done;
		From ->
			From ! {self(),{SuccessList,TE_Avg,TG_Avg,TE_Min,TE_Max,TG_Min,TG_Max,Avg_MTN,Avg_GS,FailureList,FailureRate}}
	end,
	ets:delete(S#state.table_name),
	benchmark:server(void);
server(S) when (S#state.goal_status == goal_reached) and (S#state.tunning_status == tunning_complete)->
	%-record(state,{pm_parameters,table_name,run_index=0,tot_evaluations=0,tot_generations=0,goal_status,tunning_status,success_acc=[],failure_acc=[]}).
	io:format("Starting GenTest.~p~n",[{S#state.success_acc,S#state.failure_acc}]),
	Run_Index = S#state.run_index,
	case gen_test() of
		{passed, BestScore,TotNeurons} ->
			case get(gen_test) of
				undefined ->
					put(gen_test,[get(test)]),
					erase(test);
				Gen_Test->
					put(gen_test,[get(test)|Gen_Test]),
					erase(test)
			end,
			Trace = gen_server:call(monitor,get_TRACE),
			case get(trace_acc) of
				undefined ->
					put(trace_acc,[Trace]);
				Trace_Acc ->
					put(trace_acc,[Trace|Trace_Acc])
			end,
			TAcc = get(trace_acc),
			{ok, File} = file:open(?DIR++"generational_benchmark_partial", write),
			lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, TAcc),
			file:close(File),
			gen_server:cast(monitor,{stop,normal}),timer:sleep(1000),
			case Run_Index >= ?TOT_RUNS of
				false ->
					io:format("******** Benchmark: New population index:~p started.~n",[Run_Index+1]),
					population_monitor:init_population(S#state.pm_parameters),
					benchmark:server(S#state{run_index = Run_Index+1,goal_status = undefined,tunning_status = undefined});
				true ->
					benchmark:server(S#state{tot_evaluations=0,tot_generations=0,goal_status=done,tunning_status=done})
			end;
		failed ->
			gen_server:cast(monitor,{op_tag,continue}),
			benchmark:server(S#state{goal_status = undefined,tunning_status = undefined})
	end;
server(S)->
	receive	
		{_From,goal_reached,TotEvaluations,TotGenerations,SimDiv}->
			gen_server:cast(monitor,{op_tag,pause}),
			io:format("Benchmark recieved 'goal_reached' msg.~n"),
			io:format("DiveristyAcc:~p~n",[SimDiv]),
			%SimDiv = [length(GenerationalDiversity) || GenerationalDiversity <- DiversityAcc],
			DivAcc = S#state.diversity_acc,
			server(S#state{goal_status = goal_reached,success_acc=[{TotEvaluations,TotGenerations}|S#state.success_acc],diversity_acc=[SimDiv|DivAcc]});
		{_From,tunning_phase,done}->
			gt(),
			benchmark:server(S);
		{_From,goal_failed,TotEvaluations,TotGenerations}->
			io:format("Goal failed~n"),
		       benchmark:server(S#state{goal_status=goal_reached,tunning_status=tunning_complete,failure_acc=[{TotEvaluations,TotGenerations}|S#state.failure_acc]});
		{_From,monitor,paused}->
			io:format("Benchmark received 'monitor, paused' msg.~n"),
			benchmark:server(S#state{tunning_status=tunning_complete});
		{_From,print,gen_list}->
			case get(generalisation) of
				undefined ->
					io:format("No generalization list available.~n");
				GenList ->
					GenAvgFitness = functions:avg(GenList),
					GenAvgFitness_StD = functions:std(GenList),
					GenMaxFitness = lists:max(GenList),
					GenMinFitness = lists:min(GenList),
					io:format("Generalization results:~n GenList:~p~n GenTest:~p~n Avg Fitness:~p Std:~p Max Fitness:~p Min Fitness:~p~n Trace_Acc:~p~n",[GenList,get(gen_test), GenAvgFitness, GenAvgFitness_StD, GenMaxFitness, GenMinFitness, get(trace_acc)])
			end,
			benchmark:server(S)
	end.

gt()->
	TopDX_Id = case gen_server:call(monitor,{request,topDX_Ids}) of
		[DX_Id] ->
			DX_Id;
		[DX_Id|_] ->
			DX_Id;
		[]->
			void
	end,
	case TopDX_Id of
		void ->
			case get(test) of
				undefined ->
					put(test,[0]);
				GenList ->
					put(test,[0|GenList])
			end;
		_ ->
			{ok,TopDX_PId}=exoself:test(benchmark,TopDX_Id),
			receive
				{TopDX_Id,Fitness,FitnessProfile}->
					case get(test) of
						undefined ->
							put(test,[Fitness]);
						GenList ->
							put(test,[Fitness|GenList])
					end
			end
	end.
	
gen_test()->
	TopDX_Id = case gen_server:call(monitor,{request,topDX_Ids}) of
		[DX_Id] ->
			DX_Id;
		[DX_Id|_] ->
			DX_Id;
		[]->
			void
	end,
	case TopDX_Id of
		void ->
			case get(generalisation) of
				undefined ->
					put(generalisation,[0]);
				GenList ->
					put(generalisation,[0|GenList])
			end,
			case get(mtn_list) of
				undefined ->
					put(mtn_list,[0]);
				MTN_List ->
					put(mtn_list,[0|MTN_List])
			end,
			{passed,void,0};
		_ ->
			{ok,DX_PId}=exoself:test(benchmark,TopDX_Id),
			receive
				{TopDX_Id,Fitness,FitnessProfile}->
					case get(generalisation) of
						undefined ->
							put(generalisation,[Fitness]);
						GenList ->
							put(generalisation,[Fitness|GenList])
					end
			end,
			technome_constructor:view_dx(TopDX_Id),
			[DX] = mnesia:dirty_read({dx,TopDX_Id}),
			Summary = DX#dx.summary,
			%{value,{tot_neurons,TotNeurons}} = lists:keysearch(tot_neurons, 1, Summary),
			TotNeurons = Summary#summary.tot_neurons,
			case get(mtn_list) of
				undefined ->
					put(mtn_list,[TotNeurons]);
				MTN_List ->
					put(mtn_list,[TotNeurons|MTN_List])
			end,
			{passed,void,TotNeurons}
	end.

gen_test1()->
	TopDX_Id = case gen_server:call(monitor,{request,topDX_Ids}) of
		[DX_Id] ->
			DX_Id;
		[DX_Id|_] ->
			DX_Id
	end,
	case ?BENCHMARK_MORPHOLOGIES of
		void -> %{pole2_balancing,3} ->
			case p2b_LongRun([TopDX_Id],[]) of
				{passed,SurvivingTopDX_Ids} ->
					case p2b_GenRun(SurvivingTopDX_Ids) of
						{passed,GenScore,TotNeurons} ->
							{passed,GenScore,TotNeurons};
						failed ->
							failed
					end;
				failed ->
					failed
			end;
		_ ->
			technome_constructor:view_dx(TopDX_Id),
			[DX] = mnesia:dirty_read({dx,TopDX_Id}),
			Summary = DX#dx.summary,
			%{value,{tot_neurons,TotNeurons}} = lists:keysearch(tot_neurons, 1, Summary),
			TotNeurons = Summary#summary.tot_neurons,
			case get(mtn_list) of
				undefined ->
					put(mtn_list,[TotNeurons]);
				MTN_List ->
					put(mtn_list,[TotNeurons|MTN_List])
			end,
			{passed,void,TotNeurons}
	end.
	
	p2b_LongRun([DX_Id|DX_Ids],Acc)->
		io:format("LongRun started, DX_Id:~p~n",[DX_Id]),
		Angle1 = 1*(2*math:pi()/360),
		Angle2 = 0,
		GoalTimeSteps = 100000,
		MaxTimeSteps = 100000,
		States = [{0,0,Angle1,0,Angle2,0,1,GoalTimeSteps,MaxTimeSteps,0}],
		Score = dpb_GenTest(DX_Id,States,0),
		case Score == 1 of
			true ->
				p2b_LongRun(DX_Ids,[DX_Id|Acc]);
			false ->
				p2b_LongRun(DX_Ids,Acc)
		end;
	p2b_LongRun([],Acc)->
		case Acc of
			[] ->
				failed;
			SurvivingDX_Ids ->
				{passed,SurvivingDX_Ids}
		end.
	
	p2b_GenRun(DX_Ids)->				
		io:format("GenRun started, DX_Ids:~p~n",[DX_Ids]),
		{Top_GenScore,TotNeurons,TopDX_Id} = test_winners(DX_Ids,[]),
		case Top_GenScore > 200 of
			true ->
				io:format("GenTest passed with:~p~n",[Top_GenScore]),
				case get(mtn_list) of
					undefined ->
						put(mtn_list,[TotNeurons]);
					MTN_List ->
						put(mtn_list,[TotNeurons|MTN_List])
				end,
				case get(gs_list) of
					undefined ->
						put(gs_list,[Top_GenScore]);
					GS_List ->
						put(gs_list,[Top_GenScore|GS_List])
				end,
				{passed,Top_GenScore,TotNeurons};
			false ->
				io:format("GenTest failed with:~p~n",[Top_GenScore]),
				failed
		end.


		test_winners([DX_Id|Winners],Acc)->
			Rad2Angle = 2*math:pi()/360,

			PAngleLimit = Rad2Angle*3.6, %0.06283152 = 3.6 degrees
			PAngleRange = PAngleLimit*2,
			PVelLimit = Rad2Angle*8.6,   %0.15009752 = 8.6 degrees
			PVelRange = PVelLimit*2,
			CPosRange = 2.16*2,
			CVelRange = 1.35*2,
			Set = [0.05,0.25,0.5,0.75,0.95],
			States = [{-2.16+CPs*CPosRange,-1.35+CVs*CVelRange,-PAngleLimit+ PA1s*PAngleRange,-PVelLimit+ PV1s*PVelRange,0,0,1,1000,1000,0}||CPs<-Set,CVs<-Set,PA1s<-Set,PV1s<-Set],
			GenScore  = dpb_GenTest(DX_Id,States,0),
			io:format("GenScore:~p~n",[GenScore]),
			test_winners(Winners,[{GenScore,DX_Id}|Acc]);
		test_winners([],Acc)->
			[{GenScore, DX_Id}|_] = lists:reverse(lists:sort(Acc)),
			[DX] = mnesia:dirty_read({dx,DX_Id}),
			Summary = DX#dx.summary,
			%{value,{tot_neurons,Tot_Neurons}} = lists:keysearch(tot_neurons, 1, Summary),
			Tot_Neurons = Summary#summary.tot_neurons,
			{GenScore,Tot_Neurons,DX_Id}.
	
	dpb_GenTest(DX_Id,[State|States],Acc)->
		exoself:test(benchmark,DX_Id),
		receive
			{From,get_dpb_state} ->
				From ! {dpb_state,State},
				receive
					{From,goal_status,Goal} ->
						%io:format("Goal:~p~n",[Goal]),
						Score = case Goal of
							reached ->
								1;
							undefined ->
								0
						end,
						%timer:sleep(100),
						dpb_GenTest(DX_Id,States,Acc+Score)
				end
		end;
	dpb_GenTest(DX_Id,[],Acc)->
		Acc.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ALife Benchmarker %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alife_benchmark(TotRuns)->
	alife_benchmark(void,TotRuns,5000).

alife_benchmark(Config,TotRuns,RunLength)->
	HeartBeat = 10000,
	register(alife_benchmarker,spawn(benchmark,alife_benchmark,[Config,TotRuns,RunLength,done,HeartBeat,[]])).

alife_benchmark(Config,Index,RunLength,done,HeartBeat,Acc)->
	case Index of
		0 ->
			{ok, File} = file:open(?DIR++"alife_benchmark", write),
			lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, Acc),
			file:close(File),
			io:format("********ALife Benchmarker: ALife Benchmark complete:~p~n",[Acc]),
			Graphs = prepare_Graphs(Acc),
			write_Graphs(Graphs,"alife_benchmark");
		_ ->
			{ok, File} = file:open(?DIR++"alife_benchmark_partial", write),
			lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, Acc),
			file:close(File),
			io:format("********ALife Benchmarker: ALife simulation benchmarks left:~p~n",[Index-1]),
			population_monitor:test(),
			benchmark:alife_benchmark(Config,Index-1,RunLength,continue,HeartBeat,Acc)
	end;
alife_benchmark(Config,Index,RunLength,State,Heartbeat,Acc)->
	receive
		terminate ->
			FilePath = ?DIR++"alife_benchmark_terminated",
			{ok, File} = file:open(FilePath, write),
			lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, Acc),
			file:close(File),
			io:format("********ALife Benchmarker Terminated, Traces written to:~p~n",[FilePath]);
		{change_HeartBeat,New_Heartbeat}->
			benchmark:alife_benchmark(Config,Index,RunLength,State,New_Heartbeat,Acc)
	after Heartbeat ->
		Evaluations = gen_server:call(monitor,get_evaluations),
		case Evaluations >= RunLength of
			true ->
				Trace = gen_server:call(monitor,get_TRACE),
				ok = gen_server:call(monitor,terminate),
				timer:sleep(10000),
				io:format("********ALife Benchmarker: Population Monitor Terminated~n"),
				polis:stop(),
				io:format("********ALife Benchmarker: Polis Stopped~n"),
				timer:sleep(10000),
				polis:start(),
				io:format("********ALife Benchmarker: Polis Started~n"),
				timer:sleep(10000),
				benchmark:alife_benchmark(Config,Index,RunLength,done,Heartbeat,[Trace|Acc]);
			false ->
				benchmark:alife_benchmark(Config,Index,RunLength,State,Heartbeat,Acc)
		end
	end.
	
-record(graph,{morphology,avg_neurons=[],neurons_std=[],avg_fitness=[],fitness_std=[],max_fitness=[],min_fitness=[],avg_diversity=[],diversity_std=[],evaluations=[],evaluation_Index=[]}).
-record(avg,{avg_neurons=[],neurons_std=[],avg_fitness=[],fitness_std=[],max_fitness=[],min_fitness=[],avg_diversity=[],diversity_std=[],evaluations=[]}).
%-record(stat,{avg_subcores,subcores_std,avg_neurons,neurons_std,avg_fitness,fitness_std,max_fitness,min_fitness,avg_diversity,evaluations,time_stamp}).
pg(FileName)->
	pg(FileName,FileName).
pg(FileName,Graph_Postfix)->
	{ok,Traces} = file:consult(FileName),
	io:format("Traces:~p~n",[Traces]),
	Graphs = prepare_Graphs(Traces),
	write_Graphs(Graphs,Graph_Postfix).

prepare_Graphs(Traces)->
%Each trace is composed of a list of lists of stats. Length of list of stats determines the number of species.... we need to graph that so that we can graph the features against evaluations.
%1. seperate into Traces
%2. Seperate Traces into stats
%3. Extract from each stats the various features against evaluations
%4. Combine the whatever from all stats from all traces into the averages.
		[T|_] = Traces,
%		io:format("T:~p~n",[T]),
		[Stats_List|_] = T#trace.stats,
%		io:format("Stats_List:~p~n",[Stats_List]),
		TotMorphologies = length(Stats_List),
		Morphologies = [S#stat.morphology || S<-Stats_List],
%		io:format("Morphologies:~p~n",[Morphologies]),
		Morphology_Graphs = [prep_Traces(Traces,Morphology,[])|| Morphology <- Morphologies],
		[io:format("Graph:~p~n",[Graph])|| Graph<-Morphology_Graphs],
		Morphology_Graphs.
		
	prep_Traces([T|Traces],Morphology,Acc)->
%		io:format("Morphology:~p~n",[Morphology]),
		Morphology_Trace = lists:flatten([[S||S<-Stats,S#stat.morphology == Morphology]||Stats<-T#trace.stats]),
%		io:format("T:~p~nMorphology_Traces:~p~n",[T,Morphology_Trace]),
		prep_Traces(Traces,Morphology,[Morphology_Trace|Acc]);
	prep_Traces([],Morphology,Acc)->
		Graph = avg_MorphologicalTraces(lists:reverse(Acc),[],[],[]),
		Graph#graph{morphology=Morphology}.
		
		avg_MorphologicalTraces([S_List|S_Lists],Acc1,Acc2,Acc3)->
			case S_List of
				[S|STail] ->			
					avg_MorphologicalTraces(S_Lists,[STail|Acc1],[S|Acc2],Acc3);
				[] ->
					Graph = avg_statslists(Acc3,#graph{}),
					Graph
			end;
		avg_MorphologicalTraces([],Acc1,Acc2,Acc3)->
			avg_MorphologicalTraces(lists:reverse(Acc1),[],[],[lists:reverse(Acc2)|Acc3]).
		
			avg_statslists([S_List|S_Lists],Graph)->
				Avg = avg_stats(S_List,#avg{}),
				U_Graph = Graph#graph{
						avg_neurons = [Avg#avg.avg_neurons|Graph#graph.avg_neurons],
						neurons_std = [Avg#avg.neurons_std|Graph#graph.neurons_std],
						avg_fitness = [Avg#avg.avg_fitness|Graph#graph.avg_fitness],
						fitness_std = [Avg#avg.fitness_std|Graph#graph.fitness_std],
						max_fitness = [Avg#avg.max_fitness|Graph#graph.max_fitness],
						min_fitness = [Avg#avg.min_fitness|Graph#graph.min_fitness],
						evaluations = [Avg#avg.evaluations|Graph#graph.evaluations],
						avg_diversity = [Avg#avg.avg_diversity|Graph#graph.avg_diversity],
						diversity_std = [Avg#avg.diversity_std|Graph#graph.diversity_std]
					},
				avg_statslists(S_Lists,U_Graph);
			avg_statslists([],Graph)->
				Graph#graph{
						avg_neurons = lists:reverse(Graph#graph.avg_neurons),
						neurons_std = lists:reverse(Graph#graph.neurons_std),
						avg_fitness = lists:reverse(Graph#graph.avg_fitness),
						fitness_std = lists:reverse(Graph#graph.fitness_std),
						max_fitness = lists:reverse(Graph#graph.max_fitness),
						min_fitness = lists:reverse(Graph#graph.min_fitness),
						evaluations = lists:reverse(Graph#graph.evaluations),
						avg_diversity = lists:reverse(Graph#graph.avg_diversity),
						diversity_std = lists:reverse(Graph#graph.diversity_std)
					}.
				
				avg_stats([S|STail],Avg)->
					U_Avg = Avg#avg{
						avg_neurons = [S#stat.avg_neurons|Avg#avg.avg_neurons],
						%neurons_std = [S#stat.neurons_std|Avg#avg.neurons_std],
						avg_fitness = [S#stat.avg_fitness|Avg#avg.avg_fitness],
						%fitness_std = [S#stat.fitness_std|Avg#avg.fitness_std],
						max_fitness = [S#stat.max_fitness|Avg#avg.max_fitness],
						min_fitness = [S#stat.min_fitness|Avg#avg.min_fitness],
						evaluations = [S#stat.evaluations|Avg#avg.evaluations],
						avg_diversity = [S#stat.avg_diversity|Avg#avg.avg_diversity]
					},
					avg_stats(STail,U_Avg);
				avg_stats([],Avg)->
					Avg#avg{
						avg_neurons=functions:avg(Avg#avg.avg_neurons),
						neurons_std=functions:std(Avg#avg.avg_neurons),
						avg_fitness=functions:avg(Avg#avg.avg_fitness),
						fitness_std=functions:std(Avg#avg.avg_fitness),
						max_fitness=functions:avg(Avg#avg.max_fitness),
						min_fitness=functions:avg(Avg#avg.min_fitness),
						evaluations=functions:avg(Avg#avg.evaluations),
						avg_diversity=functions:avg(Avg#avg.avg_diversity),
						diversity_std=functions:std(Avg#avg.avg_diversity)
					}.

write_Graphs([G|Graphs],Graph_Postfix)->
	Morphology = G#graph.morphology,
	U_G = G#graph{evaluation_Index=[500*Index || Index <-lists:seq(1,length(G#graph.avg_fitness))]},
	{ok, File} = file:open(?DIR++"graph_"++atom_to_list(Morphology)++"_"++Graph_Postfix, write),
	io:format(File,"#Avg Fitness Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y,Std}) -> io:format(File, "~p ~p ~p~n",[X,Y,Std]) end, lists:zip3(U_G#graph.evaluation_Index,U_G#graph.avg_fitness,U_G#graph.fitness_std)),
	io:format(File,"~n~n#Avg Neurons Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y,Std}) -> io:format(File, "~p ~p ~p~n",[X,Y,Std]) end, lists:zip3(U_G#graph.evaluation_Index,U_G#graph.avg_neurons,U_G#graph.neurons_std)),
	io:format(File,"~n~n#Avg Diversity Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y,Std}) -> io:format(File, "~p ~p ~p~n",[X,Y,Std]) end, lists:zip3(U_G#graph.evaluation_Index,U_G#graph.avg_diversity,U_G#graph.diversity_std)),
	io:format(File,"~n~n#Avg. Max Fitness Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.max_fitness)),
	io:format(File,"~n~n#Avg. Min Fitness Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.min_fitness)),
	io:format(File,"~n~n#Specie-Population Turnover Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.evaluations)),
	file:close(File),
	write_Graphs(Graphs,Graph_Postfix);
write_Graphs([],Graph_Postfix)->
	ok.
		
unconsult(List)->
	{ok, File} = file:open(?DIR++"alife_benchmark", write),
	lists:foreach(fun(X) -> io:format(File, "~p~n",[X]) end, List),
	file:close(File).
	
genplot_avg(Lists)->
	genplot_avg(Lists,[],[],[]).

genplot_avg([List|Lists],Acc1,Acc2,Acc3)->
	case List of
		[Val|Rem] ->
			genplot_avg(Lists,[Rem|Acc1],[Val|Acc2],Acc3);
		[] ->
			StepSize=25000/length(Acc3),
			print_plot(StepSize,StepSize,Acc3)
	end;
genplot_avg([],Acc1,Acc2,Acc3)->
	genplot_avg(Acc1,[],[],[{functions:avg(Acc2),functions:std(Acc2)}|Acc3]).

genplot_max(Lists)->
	genplot_max(Lists,[],[],[]).
genplot_max([List|Lists],Acc1,Acc2,Acc3)->
	case List of
		[Val|Rem] ->
			genplot_max(Lists,[Rem|Acc1],[Val|Acc2],Acc3);
		[] ->
			StepSize=25000/length(Acc3),
			print_plot(StepSize,StepSize,Acc3)
	end;
genplot_max([],Acc1,Acc2,Acc3)->
	genplot_max(Acc1,[],[],[{lists:max(Acc2),0}|Acc3]).



genplot_all(Lists)->
	genplot_all(Lists,[],[],[]).
genplot_all([List|Lists],Acc1,Acc2,Acc3)->
	case List of
		[Val|Rem] ->
			genplot_all(Lists,[Rem|Acc1],[Val|Acc2],Acc3);
		[] ->
			StepSize=25000/length(Acc3),
			print_plot(StepSize,StepSize,Acc3)
	end;
genplot_all([],Acc1,Acc2,Acc3)->
	genplot_all(Acc1,[],[],[{functions:avg(Acc2),lists:min(Acc2),lists:max(Acc2)}|Acc3]).

%genplot_max(Lists)->
%	genplot_max(Lists,[]).
%	genplot_max([L|Lists],Acc)->
%		genplot_max(Lists,[{lists:max(L),0}|Acc]);
%	genplot_max([],Acc)->
%		StepSize=25000/length(Acc),
%		print_plot(StepSize,StepSize,Acc).
		
	print_plot(Index,[{Val,Std}|List])->
		io:format("~p  ~p  ~p~n",[Index,Val,Std]),
		print_plot(Index+500,List);
	print_plot(Index,[])->
		void.

		
	print_plot(Index,StepSize,[{Val,Std}|List])->
		io:format("~p  ~p  ~p~n",[Index,Val,Std]),
		print_plot(Index+StepSize,StepSize,List);
	print_plot(Index,StepSize,[{Val,YLow,YHigh}|List])->
		io:format("~p  ~p  ~p ~p~n",[Index,Val,YLow,YHigh]),
		print_plot(Index+StepSize,StepSize,List);
	print_plot(_Index,_SteoSize,[])->
		ok.

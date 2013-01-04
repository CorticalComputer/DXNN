%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This source code and work is provided and developed by Gene I. Sher & DXNN Research Group WWW.DXNNResearch.COM
%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%

-module(benchmark).
-compile(export_all).
-include("forex_db.hrl").
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Benchmark Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DIR,"benchmarks/").
-define(DEFAULT_POPULATION_ID,test).
-define(BENCHMARK_MORPHOLOGIES,[forex_trader]).
-define(DEFAULT_OPMODE,gt).
-define(EVOLUTION_TYPE,generational).
-define(SELECTION_TYPE,hof_competition).
%-define(INIT_CONSTRAINTS,[#constraint{morphology=Morphology,sc_types=SC_Types, sc_hypercube_plasticity=[none], sc_neural_linkform=LinkForm, neural_afs = [tanh]}|| Morphology<-[pole2_balancing3],LinkForm<-[recursive], SC_Types<-[[neural]]]).
-define(INIT_CONSTRAINTS,[#constraint{morphology=Morphology,sc_types=SC_Types, sc_hypercube_plasticity=[none], sc_neural_linkform=LinkForm, neural_afs =[tanh], neural_signal_integrators = [dot], neural_types=[standard], sc_neural_plasticity=[none], sc_hypercube_linkform = Substrate_LinkForm} || Morphology<-[pole2_balancing3],Substrate_LinkForm <- [[feedforward]], LinkForm<-[recursive],SC_Types<-[[neural]]]).
%-record(state,{pm_parameters,table_name,run_index=1,tot_evaluations=0,tot_generations=0,goal_status,tunning_status,success_acc=[],failure_acc=[],diversity_acc=[],trace_acc=[]}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Starts and ends Neural Networks with various preset parameters and options, and polls the logger for information about each run.
print_experiment(Experiment_Id)->
	io:format("********~n~p~n*******",[mnesia:dirty_read({experiment,Experiment_Id})]).

get_ekeys()->
	io:format("--- Currently Stored Experiments ---~n"),
	get_ekeys(mnesia:dirty_first(experiment)).
	
	get_ekeys('$end_of_table')->
		ok;
	get_ekeys(Key)->
		io:format("~p~n",[Key]),
		get_ekeys(mnesia:dirty_next(experiment,Key)).

start(Id)->
	PMP = #pmp{
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
	},
	E=#experiment{
		id = Id,
		backup_flag = true,
		pm_parameters=PMP,
%		init_constraints=?INIT_CONSTRAINTS,
		progress_flag=in_progress,
		run_index=1,
		tot_runs=100,
		started={date(),time()},
		interruptions=[]
	},
	mnesia:dirty_write(E),
	PId = spawn(benchmark,prep,[E]),
	register(benchmark,PId).

continue(Id)->
	case mnesia:dirty_read({experiment,Id}) of
		undefined ->
			io:format("Can't continue experiment:~p, it's not present in the database.~n",[Id]);
		[E] ->
			case E#experiment.progress_flag of
				completed ->
					io:format("Experiment:~p already completed:~p~n",[Id,E#experiment.trace_acc]);
				in_progress ->
					Interruptions = E#experiment.interruptions,
					U_Interruptions = [now()|Interruptions],
					U_E = E#experiment{
						interruptions = U_Interruptions
					},
					mnesia:dirty_write(U_E),
					register(benchmark,spawn(benchmark,prep,[U_E]))
			end
	end.

stop()->
	benchmark ! {self(),stop}.
	
prep(E)->
	PMP = E#experiment.pm_parameters,
	U_PMP = PMP#pmp{benchmarker_pid=self()},
	Population_Id = PMP#pmp.population_id,
	population_monitor:init_population(U_PMP),
	benchmark:loop(E,Population_Id).
	
loop(E,P_Id)->
	receive	
		{P_Id,completed,Trace}->
			U_TraceAcc = [Trace|E#experiment.trace_acc],
			U_RunIndex = E#experiment.run_index+1,
			case U_RunIndex > E#experiment.tot_runs of
				true ->
					U_E = E#experiment{
						trace_acc = U_TraceAcc,
						run_index = U_RunIndex,
						completed = {date(),time()},
						progress_flag = completed
					},
					mnesia:dirty_write(U_E),
					report(U_E#experiment.id,"report"),
					case lists:member(test,(U_E#experiment.pm_parameters)#pmp.op_mode) of
						true ->
							io:format("E:~p~n",[U_E]),
							Traces = U_E#experiment.trace_acc,
							BestGen_Champions = [get_best(Trace) || Trace <- Traces],
							io:format("Best validation champions per evolutionary run:~p~n",[BestGen_Champions]),
							[{BOTB_F,BOTB_Id}|_] = lists:reverse(lists:sort(BestGen_Champions)),
							io:format("BOTB:~p~n",[{BOTB_F,BOTB_Id}]),
							BestGen_PIdPs=[{exoself:start_link({test,ExoselfId,1,self()}),ExoselfId} || {GenFitness,ExoselfId} <- BestGen_Champions],
							%io:format("BestGen_PIdPs:~p~n",[BestGen_PIdPs]),
							BestGen_Results=receive_TestAcks(BestGen_PIdPs,[]),
							io:format("Test results of the best validation champions:~p~n",[BestGen_Results]),
							BestGen_Avg = get_avg(BestGen_Results,[]),
							io:format("BOTB TEST RESULTS:~p~n",[lists:keyfind(BOTB_Id,1,BestGen_Results)]),
							io:format("************************~n");
						false ->
							ok
					end;
				false ->
					U_E = E#experiment{
						trace_acc = U_TraceAcc,
						run_index = U_RunIndex
					},
					mnesia:dirty_write(U_E),
					PMP = U_E#experiment.pm_parameters,
					%Constraints = U_E#experiment.init_constraints,
					%population_monitor:prep_PopState(PMP,Constraints),
					U_PMP = PMP#pmp{benchmarker_pid=self()},
					Constraints = E#experiment.init_constraints,
					Population_Id = PMP#pmp.population_id,
					%population_monitor:prep_PopState(U_PMP,Constraints),
					population_monitor:init_population(U_PMP),
					
					io:format("****Experiment:~p/~p completed.****~n",[E#experiment.run_index,E#experiment.tot_runs]),
					loop(U_E,P_Id)
			end;
		terminate ->
			ok
	end.

final_steps(ExperimentName)->
	[U_E]=mnesia:dirty_read({experiment,ExperimentName}),
	io:format("E:~p~n",[U_E]),
	Traces = U_E#experiment.trace_acc,
	BestGen_Champions = [get_best(Trace) || Trace <- Traces],
	io:format("Best validation champions per evolutionary run:~p~n",[BestGen_Champions]),
	[{BOTB_F,BOTB_Id}|_] = lists:reverse(lists:sort(BestGen_Champions)),
	io:format("BOTB:~p~n",[{BOTB_F,BOTB_Id}]),
	BestGen_PIdPs=[{exoself:start_link({test,ExoselfId,1,self()}),ExoselfId} || {GenFitness,ExoselfId} <- BestGen_Champions],
	%io:format("BestGen_PIdPs:~p~n",[BestGen_PIdPs]),
	BestGen_Results=receive_TestAcks(BestGen_PIdPs,[]),
	io:format("Test results of the best validation champions:~p~n",[BestGen_Results]),
	BestGen_Avg = get_avg(BestGen_Results,[]),
	io:format("BOTB TEST RESULTS:~p~n",[lists:keyfind(BOTB_Id,1,BestGen_Results)]),
	io:format("************************~n").

	receive_TestAcks([{{ok,PId},Id}|PIdPs],Acc)->
		receive
			{PId,test_complete,Id,Fitness,Time} ->
				receive_TestAcks(PIdPs,[{Id,Fitness}|Acc])
		end;
	receive_TestAcks([],Acc)->
		Acc.
		
	
	get_best(T)->
		Stats = T#trace.stats,
		Validation_Champions=[Stat#stat.validation_fitness || [Stat] <- Stats],
		[Best|_]=lists:reverse(lists:sort(Validation_Champions)),
		Best.
	
	get_avg([{Id,FitnessP}|IdPs],Acc)->
		get_avg(IdPs,[FitnessP|Acc]);
	get_avg([],Acc)->
		get_avg(Acc,[],[],[]).
		
		get_avg([Fitness|FitnessPs],Acc1,Acc2,Acc3)->
			%io:format("Fitness:~p~n",[{Fitness,FitnessPs}]),
			case Fitness of
				[Score|Scores] ->
			%		io:format("Score/Scores:~p~n",[{Score,Scores}]),
					get_avg(FitnessPs,[Score|Acc1],[Scores|Acc2],Acc3);
				[] ->
					get_avg(FitnessPs,Acc1,Acc2,Acc3)
			end;
		get_avg([],[],[],Acc3)->
			io:format("Test restuls of best validation agents, with each fitness objective showing: {Avg,Std,Max,Min}:~n"),
			[io:format("~p~n",[{functions:avg(Score),functions:std(Score),lists:max(Score),lists:min(Score)}]) || Score <- lists:reverse(Acc3)];
		get_avg([],Acc1,Acc2,Acc3)->
			%io:format("Acc1:~p~n",[Acc1]),
			get_avg(Acc2,[],[],[Acc1|Acc3]).
		
	

report(Experiment_Id,FileName)->
	[E] = mnesia:dirty_read({experiment,Experiment_Id}),
	Traces = E#experiment.trace_acc,
	{ok, File} = file:open(?DIR++FileName++"_Trace_Acc", write),
	lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, Traces),
	file:close(File),
	io:format("******** Traces_Acc written to file:~p~n",[?DIR++FileName++"_Trace_Acc"]),
	Graphs = prepare_Graphs(Traces),
	write_Graphs(Graphs,FileName++"_Graphs"),
	Eval_List = [T#trace.tot_evaluations||T<-Traces],
	io:format("Tot Evaluations Avg:~p Std:~p~n",[functions:avg(Eval_List),functions:std(Eval_List)]).

-record(graph,{morphology,avg_neurons=[],neurons_std=[],avg_fitness=[],fitness_std=[],max_fitness=[],min_fitness=[],maxavg_fitness=[],minavg_fitness=[],avg_diversity=[],diversity_std=[],evaluations=[],validation_fitness=[],validationmax_fitness=[],validationmin_fitness=[],evaluation_Index=[]}).
-record(avg,{avg_neurons=[],neurons_std=[],avg_fitness=[],fitness_std=[],max_fitness=[],min_fitness=[],maxavg_fitness,minavg_fitness,avg_diversity=[],diversity_std=[],evaluations=[],validation_fitness=[],validationmax_fitness=[],validationmin_fitness=[]}).
%-record(stat,{avg_subcores,subcores_std,avg_neurons,neurons_std,avg_fitness,fitness_std,max_fitness,min_fitness,avg_diversity,evaluations,time_stamp}).

prepare_Graphs(Traces)->
	%Each trace is composed of a list of lists of stats. Length of list of stats determines the number of species.... we need to graph that so that we can graph the features against evaluations.
	%1. seperate into Traces
	%2. Seperate Traces into stats
	%3. Extract from each stats the various features against evaluations
	%4. Combine the whatever from all stats from all traces into the averages.
	[T|_] = Traces,
	[Stats_List|_] = T#trace.stats,
	Morphologies = [S#stat.morphology || S<-Stats_List],
	Morphology_Graphs = [prep_Traces(Traces,Morphology,[])|| Morphology <- Morphologies],
	[io:format("Graph:~p~n",[Graph])|| Graph<-Morphology_Graphs],
	Morphology_Graphs.
		
	prep_Traces([T|Traces],Morphology,Acc)->
		Morphology_Trace = lists:flatten([[S||S<-Stats,S#stat.morphology == Morphology]||Stats<-T#trace.stats]),
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
					maxavg_fitness = [Avg#avg.maxavg_fitness|Graph#graph.maxavg_fitness],
					minavg_fitness = [Avg#avg.minavg_fitness|Graph#graph.minavg_fitness],
					evaluations = [Avg#avg.evaluations|Graph#graph.evaluations],
					validation_fitness = [Avg#avg.validation_fitness|Graph#graph.validation_fitness],
					validationmax_fitness = [Avg#avg.validationmax_fitness|Graph#graph.validationmax_fitness],
					validationmin_fitness = [Avg#avg.validationmin_fitness|Graph#graph.validationmin_fitness],
					avg_diversity = [Avg#avg.avg_diversity|Graph#graph.avg_diversity],
					diversity_std = [Avg#avg.diversity_std|Graph#graph.diversity_std]
				},
				avg_statslists(S_Lists,U_Graph);
			avg_statslists([],Graph)->
				io:format("Validation Fitness:~p~n",[lists:reverse(Graph#graph.validation_fitness)]),
				Graph#graph{
					avg_neurons = lists:reverse(Graph#graph.avg_neurons),
					neurons_std = lists:reverse(Graph#graph.neurons_std),
					avg_fitness = lists:reverse(Graph#graph.avg_fitness),
					fitness_std = lists:reverse(Graph#graph.fitness_std),
					max_fitness = lists:reverse(Graph#graph.max_fitness),
					min_fitness = lists:reverse(Graph#graph.min_fitness),
					evaluations = lists:reverse(Graph#graph.evaluations),
					validation_fitness = lists:reverse(Graph#graph.validation_fitness),
					validationmax_fitness = lists:reverse(Graph#graph.validationmax_fitness),
					validationmin_fitness = lists:reverse(Graph#graph.validationmin_fitness),
					avg_diversity = lists:reverse(Graph#graph.avg_diversity),
					diversity_std = lists:reverse(Graph#graph.diversity_std)
				}.

				avg_stats([S|STail],Avg)->
					%io:format("S:~p~n",[S]),
					{Validation_Fitness,ChampionId} = S#stat.validation_fitness,
					%io:format("Here1:~p~n",[{Validation_Fitness,Avg#avg.validation_fitness}]),
					%io:format("Here2:~p~n",[list_append(Validation_Fitness,Avg#avg.validation_fitness)]),
					U_Avg = Avg#avg{
						avg_neurons = [S#stat.avg_neurons|Avg#avg.avg_neurons],
						%neurons_std = [S#stat.neurons_std|Avg#avg.neurons_std],
						avg_fitness = list_append(S#stat.avg_fitness,Avg#avg.avg_fitness),
						%fitness_std = list_append(S#stat.fitness_std,Avg#avg.fitness_std),
						max_fitness = list_append(S#stat.max_fitness,Avg#avg.max_fitness),
						min_fitness = list_append(S#stat.min_fitness,Avg#avg.min_fitness),
						evaluations = [S#stat.evaluations|Avg#avg.evaluations],
						validation_fitness = list_append(Validation_Fitness,Avg#avg.validation_fitness),
						avg_diversity = [S#stat.avg_diversity|Avg#avg.avg_diversity]
					},
					avg_stats(STail,U_Avg);
				avg_stats([],Avg)->
					Avg#avg{
						avg_neurons=functions:avg(Avg#avg.avg_neurons),
						neurons_std=functions:std(Avg#avg.avg_neurons),
						avg_fitness=[functions:avg(Val)||Val<-Avg#avg.avg_fitness],
						fitness_std=[functions:std(Val)||Val<-Avg#avg.avg_fitness],
						max_fitness=[lists:max(Val)||Val<-Avg#avg.max_fitness],
						min_fitness=[lists:min(Val)||Val<-Avg#avg.min_fitness],
						maxavg_fitness=[functions:avg(Val)||Val<-Avg#avg.max_fitness],
						minavg_fitness=[functions:avg(Val)||Val<-Avg#avg.min_fitness],
						evaluations=functions:avg(Avg#avg.evaluations),
						validation_fitness=[functions:avg(Val)||Val<-Avg#avg.validation_fitness],
						validationmax_fitness=[lists:max(Val)||Val<-Avg#avg.validation_fitness],
						validationmin_fitness=[lists:min(Val)||Val<-Avg#avg.validation_fitness],
						avg_diversity=functions:avg(Avg#avg.avg_diversity),
						diversity_std=functions:std(Avg#avg.avg_diversity)
					}.

			list_append([],[])->
				[];
%			list_append(0,[])->
%				[];
%			list_append(0,ListB)->
%				ListB;
			list_append(ListA,[])->
				[[Val]||Val<-ListA];
			list_append([],ListB)->
				ListB;
			list_append(ListA,ListB)->
				list_append(ListA,ListB,[]).
			list_append([Val|ListA],[AccB|ListB],Acc)->
				list_append(ListA,ListB,[[Val|AccB]|Acc]);
			list_append([],[],Acc)->
				%io:format("Acc:~p~n",[Acc]),
				lists:reverse(Acc).

write_Graphs([G|Graphs],Graph_Postfix)->
	Morphology = G#graph.morphology,
	U_G = G#graph{evaluation_Index=[500*Index || Index <-lists:seq(1,length(G#graph.avg_fitness))]},
	{ok, File} = file:open(?DIR++"graph_"++atom_to_list(Morphology)++"_"++Graph_Postfix, write),
	%io:format(File,"#Avg Fitness Vs Evaluations, Morphology:~p~n",[Morphology]),
	%lists:foreach(fun({X,Y,Std}) -> io:format(File, "~p ~p ~p~n",[X,Y,Std]) end, lists:zip3(U_G#graph.evaluation_Index,U_G#graph.avg_fitness,U_G#graph.fitness_std)),
	io:format(File,"#Avg Fitness Vs Evaluations, Morphology:~p",[Morphology]),
	print_MultiObjectiveFitness(File,U_G#graph.evaluation_Index,U_G#graph.avg_fitness,U_G#graph.fitness_std),
	
	io:format(File,"~n~n~n#Avg Neurons Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y,Std}) -> io:format(File, "~p ~p ~p~n",[X,Y,Std]) end, lists:zip3(U_G#graph.evaluation_Index,U_G#graph.avg_neurons,U_G#graph.neurons_std)),
	
	io:format(File,"~n~n#Avg Diversity Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y,Std}) -> io:format(File, "~p ~p ~p~n",[X,Y,Std]) end, lists:zip3(U_G#graph.evaluation_Index,U_G#graph.avg_diversity,U_G#graph.diversity_std)),
	
	io:format(File,"~n~n#Avg. Max Fitness Vs Evaluations, Morphology:~p",[Morphology]),
	%lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.max_fitness)),
	print_MultiObjectiveFitness(File,U_G#graph.evaluation_Index,U_G#graph.max_fitness),
	
	io:format(File,"~n~n~n#Avg. Min Fitness Vs Evaluations, Morphology:~p",[Morphology]),
	%lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.min_fitness)),
	print_MultiObjectiveFitness(File,U_G#graph.evaluation_Index,U_G#graph.min_fitness),
	
	io:format(File,"~n~n~n#Specie-Population Turnover Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.evaluations)),
	
	io:format(File,"~n~n#Validation Avg Fitness Vs Evaluations, Morphology:~p",[Morphology]),
	%lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.validation_fitness)),
	print_MultiObjectiveFitness(File,U_G#graph.evaluation_Index,U_G#graph.validation_fitness),
	
	io:format(File,"~n~n~n#Validation Max Fitness Vs Evaluations, Morphology:~p",[Morphology]),
	%lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.validationmax_fitness)),
	print_MultiObjectiveFitness(File,U_G#graph.evaluation_Index,U_G#graph.validationmax_fitness),
	
	io:format(File,"~n~n~n#Validation Min Fitness Vs Evaluations, Morphology:~p",[Morphology]),
	%lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.validationmin_fitness)),
	print_MultiObjectiveFitness(File,U_G#graph.evaluation_Index,U_G#graph.validationmin_fitness),
	
	file:close(File),
	write_Graphs(Graphs,Graph_Postfix);
write_Graphs([],_Graph_Postfix)->
	ok.
	
	print_MultiObjectiveFitness(File,[I|Index],[F|Fitness],[Std|StandardDiviation])->
		io:format(File,"~n~p",[I]),
		print_FitnessAndStd(File,F,Std),
		print_MultiObjectiveFitness(File,Index,Fitness,StandardDiviation);
	print_MultiObjectiveFitness(_File,[],[],[])->
		ok.
		
		print_FitnessAndStd(File,[FE|FitnessElements],[SE|StdElements])->
			io:format(File," ~p ~p",[FE,SE]),
			print_FitnessAndStd(File,FitnessElements,StdElements);
		print_FitnessAndStd(_File,[],[])->
			ok.
		
	print_MultiObjectiveFitness(File,[I|Index],[F|Fitness])->
		io:format(File,"~n~p",[I]),
		[io:format(File," ~p",[FE])||FE<-F],
		print_MultiObjectiveFitness(File,Index,Fitness);
	print_MultiObjectiveFitness(_File,[],[])->
		ok.
	
unconsult(List)->
	{ok, File} = file:open(?DIR++"alife_benchmark", write),
	lists:foreach(fun(X) -> io:format(File, "~p~n",[X]) end, List),
	file:close(File).
	
gen_plot(Lists)->gen_plot(Lists,[],[],[]).

gen_plot([List|Lists],Acc1,Acc2,Acc3)->
	case List of
		[Val|Rem] ->
			gen_plot(Lists,[Rem|Acc1],[Val|Acc2],Acc3);
		[] ->
			print_plot(500,Acc3)
	end;
gen_plot([],Acc1,Acc2,Acc3)->
	gen_plot(Acc1,[],[],[functions:avg(Acc2)|Acc3]).

	genplot(Lists)->genplot(Lists,[]).
	genplot([L|Lists],Acc)->
		genplot(Lists,[lists:max(L)|Acc]);
	genplot([],Acc)->
		print_plot(0,lists:reverse(Acc)).
		
	print_plot(Index,[Val|List])->
		io:format("~p  ~p~n",[Index,Val]),
		print_plot(Index+500,List);
	print_plot(_Index,[])->
		void.
		
trace2graph(TraceFileName)->
	{ok,Traces} = file:consult(TraceFileName),
	io:format("Traces:~p~n",[Traces]),
	Graphs = prepare_Graphs(Traces),
	write_Graphs(Graphs,TraceFileName++"_Graph").

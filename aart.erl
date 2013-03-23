%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(aart).
-compile(export_all).
-include("records.hrl").
-define(NEURO_ASSISTED,false).
%%%%%%%%%%%%%%
-record(template,{index,weights,threshold=1,q}).
mart(Inputs)->
	Templates = [],
	mart_train(Inputs,Templates).

	mart_train([X|Input],Templates)->
		U_Templates=case mart_best_match(X,Templates) of
			false ->
				New_T = init_Template(X,length(Templates)+1),
				[New_T|Templates];
			{T,MDist}->%io:format("here~n"),
				U_T=update_Template(T,X,MDist),
				%io:format("U_T:~p~n",[U_T]),
				lists:keyreplace(U_T#template.index,2,Templates,U_T)
		end,
		mart_train(Input,U_Templates);
	mart_train([],Templates)->
		Templates.

%calcualte distances to each
%get shortest distance
%check if best distance is smaller than vigilance: match_check(Vigilance,BestMDist)
%return either true and updated list, or false and old list.
	mart_best_match(_X,[])->false;
	mart_best_match(X,Templates)->mart_best_match(X,Templates,{void,void}).
	mart_best_match(X,[T|Templates],{BestT,Best})->
		D=mahalanobis_dist(T#template.weights,X,T#template.q),
		case D < Best of
			true ->
				mart_best_match(X,Templates,{T,D});
			false ->
				mart_best_match(X,Templates,{BestT,Best})
		end;
	mart_best_match(_X,[],{BestT,Best})->
		%io:format("BestT:~p Best:~p~n",[BestT,Best]),
		case match_check(vigilance(Best),Best) of
			true ->
				{BestT,Best};
			false ->
				false
		end.

%Ch4 4.16
mahalanobis_dist(W,X,Q)->
	VDist = vector_subtract(X,W),
	MDist = dot(mv_mult(Q,VDist),VDist).

%Ch4 4.18
vigilance(Mahalanobis_Dist)->
	math:sqrt(Mahalanobis_Dist).

%Ch4 4.19
match_check(Vigilance,MD)->
	MD < Vigilance.

%Ch4 4.12-4.15
init_Template(X,Index)->
	R_DiameterInitialSphere = 0.5,
	N = 1,
	W = X,
	Q = sm_mult(1/math:pow(R_DiameterInitialSphere,2),create_identity(length(X))),
	%TemplateLabel = Index,
	#template{weights=W,q=Q,index=Index}.

invQ(X,W,Q,MDist)->
	B = 0.1,
	B1 = (1-B)/(1-2*B),
	B2 = 1/(B*B1),
	G = mv_mult(Q,vector_subtract(X,W)),
	U_Q = sm_mult(B1,sm_add(-(1/(B2+MDist))*dot(G,G),Q)).
	
	
%4.20-4.24 Ch4
update_Template(T,X,MDist)->
	W=T#template.weights,
	Q=T#template.q,
%	U_N = N+1,
	G = mv_mult(Q,vector_subtract(X,W)),
	B = 0.1,
	B1 = (1-B)/(1-2*B),
	B2 = 1/(B*B1),
	io:format("G:~p B:~p B1:~p B2:~p~n",[G,B,B1,B2]),
	U_Q = sm_mult(B1,sm_add(-(1/(B2+MDist))*dot(G,G),Q)),
	io:format("U_Q:~p~n",[U_Q]),
	U_W = vector_add([(1-B)*Val||Val<-W],[B*Val|| Val<-X]), %U_W = (1-B)*W + B*X
	T#template{weights=U_W,q=U_Q}.

	vector_add(V1,V2)->vector_add(V1,V2,[]).
	vector_add([A|V1],[B|V2],Acc)->
		vector_add(V1,V2,[A+B|Acc]);
	vector_add([],[],Acc)->
		lists:reverse(Acc).
	
	vector_subtract(V1,V2)->vector_subtract(V1,V2,[]).
	vector_subtract([A|V1],[B|V2],Acc)->
		vector_subtract(V1,V2,[A-B|Acc]);
	vector_subtract([],[],Acc)->
		lists:reverse(Acc).
	
	dot(V1,V2)->dot(V1,V2,0).
	dot([A|V1],[B|V2],Acc)->
		dot(V1,V2,A*B+Acc);
	dot([],[],Acc)->
		Acc.
	
	mm_mult(M1,M2)->mm_mult(M1,M2,[]).
	mm_mult(M1,[V|M2],Acc)->
		void.
		
	mv_mult(M,V)->
		[dot(V1,V) || V1<-M].
	
	sm_mult(S,M)->
		[[S*Val||Val<-V]||V<-M].
		
	sm_add(S,M)->
		[[S+Val||Val<-V]||V<-M].
	
	vector_distance(V1,V2)->vector_distance(V1,V2,0).
	vector_distance([A|V1],[B|V2],Acc)->
		vector_distance(V1,V2,Acc+math:pow(A-B,2));
	vector_distance([],[],Acc)->
		math:sqrt(Acc).
		
	create_identity(H)->create_identity(H,H,[]).
	create_identity(0,_Length,Acc)->
		Acc;
	create_identity(Index,Length,Acc)->
		V=cv(Index,Length,[]),
		create_identity(Index-1,Length,[V|Acc]).
		
		cv(_Target,0,Acc)->
			Acc;
		cv(Target,Target,Acc)->
			cv(Target,Target-1,[1|Acc]);
		cv(Target,Index,Acc)->
			cv(Target,Index-1,[0|Acc]).


%%%%%%%%%%%%%%%%%%%
%The updating of the neural weights simply specifies the centroid of the field.
%The distance, the outline of the data, is specified by the activation function which is based on the synaptic weights.
%Thus, if for example we take the difference of the two vectors and calculate the euclidian distance, and specify that if it is less than some
%value K then it passes, then we specifying the hypersphere, and to whichever element it is closes...
%But we're still choosing simply whatever element it is closest too, so it is not the radious, it's simply the distance function that defines the shape. In this case, it is the math:sqrt(E(W-X)^2). But what if the distance function is evolved? What is the general form of this? Well K(F(W,X),Threshold) So the minimum of this. Example: Postprocessor(ActivationFunction(SignalIntegrator(Preprocessor(Input),Weights))) -> Threshold(None(Distance(None))) is the case for the euclidian distance based clustering neuron. The standard neuron would be: None(Tanh(Dot(None(Input),Weights))

dxnn_art(Input)->
	%1. get input
	%2. check if it belongs to any of the current templates
	%3. if false, new_Template(X,Index),
	%4. if true, update_Template(Template,X),
	%5. goto 1
	Templates=[],
	train(Input,Templates).
	
	train([X|Input],Templates)->
		U_Templates=case best_match(X,Templates) of
			false ->
				New_T = new_Template(X,length(Templates)+1),
				[New_T|Templates];
			T->
				U_T=update_Template(T,X),
				lists:keyreplace(U_T#template.index,2,Templates,U_T)
		end,
		train(Input,U_Templates);
	train([],Templates)->
		Templates.

	best_match(_X,[])->false;
	best_match(X,Templates)->best_match(X,Templates,{void,void}).
	best_match(X,[T|Templates],{BestT,Best})->
		PreProcessor=none,%Could be normalization, or self diff, or vector diff
		SignalIntegrator=distance,
		ActivationFunction=none,
		PostProcessor=threshold,
		Plasticity=none,
		W = T#template.weights,
		Threshold=0,
		
		ProcessedInput = neuron:PreProcessor(W,X),
		IntegratedSignal = neuron:SignalIntegrator(W,ProcessedInput),
		Output = neuron:ActivationFunction(IntegratedSignal),
		Result = neuron:PostProcessor(W,X,Output,Threshold),
		
		%D=distance(T#template.weights,X),
		case Output < Best of
			true ->
				best_match(X,Templates,{T,Output});
			false ->
				best_match(X,Templates,{BestT,Best})
		end;
	best_match(_X,[],{BestT,Best})->
		case Best < BestT#template.threshold of
			true ->
				BestT;
			false ->
				false
		end.
		
		none(X)->X.
		
	new_Template(X,Index)->
		#template{index=Index,weights=X}.
		
	update_Template(T,X)->
		B=0.1,
		W = T#template.weights,
		U_W = vector_add([(1-B)*Val||Val<-W],[B*Val|| Val<-X]), %U_W = (1-B)*W + B*X
		T#template{weights=U_W}.
		
	distance(A,B)->distance(A,B,0).
	distance([V1|A],[V2|B],Acc)->
		distance(A,B,math:pow(V1-V2,2)+Acc);
	distance([],[],Acc)->
		math:sqrt(Acc).
		
%What about generalization and robustness, perhaps differential evolution has the right idea, after all if you do not set specifics in evolution, then it has to optimize for general good performance, keeping in mind the small variations of the synaptic weights. How about analyzing average performance of NNs which allow for synaptic weight variation within some range. SO then, it is differential evolutioanry algorithm, but at the same time we vary the range, and take an average of 3, 5, 10? The average performance/fitness, is the fitness of the agent. If it's performance drops, or is low (average performance), then it is not robust.










%CT/CF talks with the NN/ART system. Sensors/Actuators communicate with the environment/ART.
%Sensors/Actuators, specify the scape, which specifies the training set...
%AART can be NN specified, internal FFNN specified (each template has a different FFNN), can use standard approaches like Fuzzy, Mahalabinobis, guassian... but each template could be different.
%FMode:: train, process.

-record(class,{label,categories=[],threshold,lp,range,stats}).
-record(category,{label,weights,threshold,lp,range,stats}).
-record(state,{type,plasticity,morphology,specie_id,sensors,actuators,cf,ct,complexity,op_mode,max_attempts,dimensions,densities,behavior}).
prep(ExoSelf,Self,Id,State,Sensors,Actuators,CT,CF,OpMode,Classes)->
	Self ! {self(),tik},
	aart:loop(ExoSelf,self(),Id,State,Sensors,Actuators,CT,CF,{Classes,train},OpMode).

loop(ExoSelf,Self,Id,State,I,O,CT,CF,{Classes,FMode},OpMode)->
	receive
		{Self,tik}->
			{U_Classes,U_FMode,OAcc} = reason(I,O,CT,CF,{Classes,FMode}),
			%io:format("id:~p OAcc:~p~n",[self(),OAcc]),
			U_State=case ?BEHAVIORAL_TRACE of
				true ->
					Act = [Val|| {_A,Val}<-OAcc],
					Behavior = State#state.behavior,
					State#state{behavior=[Act|Behavior]};
				false ->
					State
			end,
			case cortex:OpMode(ExoSelf,U_State#state.specie_id,OAcc,0,0,U_Classes,U_State#state.behavior) of
				end_training ->
					%leave_scape(),
					%io:format("U_Classes:~p~n",[U_Classes]),
					aart:loop(ExoSelf,Self,Id,U_State,I,O,CT,CF,{U_Classes,U_FMode},OpMode);
				reset_IProfile ->
					Self ! {Self,tik},
					put(classes,U_Classes),
					Mutated_Classes=mutate_Classes(Classes),
					aart:loop(ExoSelf,Self,Id,U_State,I,O,CT,CF,{Mutated_Classes,U_FMode},OpMode);
				revert_IProfile ->
					Self ! {Self,tik},
					%io:format("##Debug##, WHERE DID THE SIGNAL COME FROM?~n"),
					Reverted_Classes = get(classes),
					Mutated_Classes=mutate_Classes(Reverted_Classes),
					aart:loop(ExoSelf,Self,Id,U_State,I,O,CT,CF,{Mutated_Classes,U_FMode},OpMode);
				_ ->%Continue
					%io:format("Strange return from cortex:OpMode(...), Cortex:~p CxCT:~p return:~p~n",[Id,CT,StrangeReturn]),
					Self ! {Self,tik},
					aart:loop(ExoSelf,Self,Id,U_State,I,O,CT,CF,{U_Classes,U_FMode},OpMode)
			end;
		{ExoSelf,terminate}->
%			io:format("Resulting Classes:~p~n",[Classes]),
			void;
		Msg ->
			io:format("*** UNKNOWN MSG ***:~p~n",[Msg]),
			aart:loop(ExoSelf,Self,Id,State,I,O,CT,CF,{Classes,FMode},OpMode)
		after 20000 ->
			io:format("********ERROR: AART_Cortex Crashed:~p~n",[{ExoSelf,Self,Id,State,I,O,CT,CF,{Classes,FMode},OpMode}])
	end.

mutate_Classes([])->
	[];
mutate_Classes(Classes)->
	% With some probability decide whether to mutate or not the global parameters/Classes.
	% If not, return Classes, if yes go to next step
	% Perturb template's center.
	% Delete Classes
	% Perturb learning parameters
	% perturb vigilence
	% Dependent on the decision boundary hypervolume, decide on what other hypervolume specific perturbation to perform.
	% Return perturbed Classes.
	case random:uniform() < 0.5 of
		true ->
			Class_MP = case math:sqrt(length(Classes)) of
				0.0 -> io:format("Class mutation prob: 0%~n"), 0;
				Val -> 1/Val
			end,
			%io:format("Ok:~p~n",[{Class_MP,Classes}]),
			lists:flatten([mutate_class(Class,Class_MP) || Class <- Classes]);
		false ->
			Classes
	end.

	mutate_class(Class,Class_MP)->
		%io:format("Class_MP:~p~n",[Class_MP]),
		U_Class=case random:uniform() < Class_MP of
			true ->%io:format("Tot Categories:~p~n",[Class]),
				case length(Class#class.categories) of
					0 ->
						[];
					TotCategories ->
						Category_MP=math:sqrt(1/TotCategories),
						U_Categories = lists:flatten([mutate_category(Category,Category_MP) || Category <- Class#class.categories]),
						Class#class{categories=U_Categories}
				end;
			false ->
				Class
		end.
		
		mutate_category(Category,Category_MP)->
			case random:uniform()< Category_MP of
				true ->
					Perturbers = [delete_category,perturb_weights,perturb_threshold],
					Perturber=lists:nth(random:uniform(length(Perturbers)),Perturbers),
					%io:format("Pertuber:~p~n",[Perturber]),
					aart:Perturber(Category);
				false ->
					Category
			end.
			
			perturb_threshold(Category)->
				Threshold=Category#category.threshold,
				U_Threshold=Threshold+random:uniform()-0.5,%TODO
				%io:format("Threshold:~p U_Threshold:~p~n",[Threshold,U_Threshold]),
				Category#category{threshold=U_Threshold}.
				
			perturb_lp(Category)->
				LP=Category#category.lp,
				U_LP=LP+(random:uniform()-0.5),
				Category#category{lp=U_LP}.
				
			perturb_weights(Category)->
				Weights=Category#category.weights,
				%io:format("Weights:~p~n",[Weights]),
				U_Weights=[W+(random:uniform()-0.5) || W <- Weights],
				Category#category{weights=U_Weights}.
				
			delete_category(Category)->%TODO
				[].
				
			
		
%Sensory_Pool: [Sensor1...] Sensor: {sensor,Name,Id,VL,Parameters}
%Actuator_Pool:[Actuator1...] Actuator: {actuator,Name,Id,VL,Parameters}
reason(Sensors,Actuators,CT,CF,{Classes,FMode})->
	Input = [sensors:Name(VL,SensorId,Parameters) || {sensor,Name,SensorId,Format,VL,Parameters,_Obj,_Vis}<- Sensors],
	%io:format("Sensors:~p Actuators:~p Input:~p~n",[Sensors,Actuators,Input]),
	{Output,U_Classes} = calculate_AARTMAP_Output(Classes,Input,CT,CF),
	%io:format("U_Classes:~p Output:~p~n",[U_Classes,Output]),
	%io:format("Output:~p~n",[Output]),
	{U_Classes,FMode,form_output([Output],Actuators,[])}.
	
	form_output([O|Output],[A|Actuators],Acc)->
		form_output(Output,Actuators,[{A,O}|Acc]);
	form_output([],[],Acc)->
		Acc.
	
%We have to always use a devided dataset. Half used for training, the other for testing fitness. Why not use the same? What does it matter? Ok. First X rounds we do training, then we do testing of fitness.
%The cortex contacts its sensors and get results.
%The cortex drops into reaason, during which the template contacts cpps, and the output cep decides on the distance/inclusion rule, and learning speed for each template vs input vector. The smallest distance is chosen with its learning speed. If it is below threshold, then it is added to that template and the template updated with that learning rule. If it is not below that threshold, it is used to create a new template.
%This continues for every input. After which the classification categories have been created.
%Should this be repeated multiple times?

calculate_ART_Output([],Input,CT,CF)->
	New_Category = new_Category(Input,1),
	{[New_Category#category.label],[New_Category]};
calculate_ART_Output(Categories,Input,CT,CF)->
%Compare Input vector to every Categories using NN
%Choose the closest (shortest distance)
%compare to threshold in Categories
%If passes, update tempplates
%If not, create new entry in categories
	{BestDist,BestMatch,BestParameters}=get_BestMatch(Categories,Input,CT,CF,{undefined,undefined,undefined}),
	case BestDist < BestMatch#category.threshold of
		true ->
			U_Category=update_Category(BestMatch,Input,BestParameters),
			{[U_Category#category.label],lists:keyreplace(U_Category#category.label,2,Categories,U_Category)};
		false ->
			New_Category = new_Category(Input,length(Categories)+1),
			{[New_Category#category.label],[New_Category|Categories]}
	end.

calculate_AARTMAP_Output(Classes,[Input],CT,CF)->%Currently supports only a single sensor input, but we can extend it, so that each template is actually a multivector based tepmlate, or we can append all the input vectors together... but then how do we expand sensors?
	case Input of
		{Label,SF}->%Training
			%SampleFeatures = normalize(SF),
			SampleFeatures = SF,
			%io:format("Training~n"),
			%SampleFeatures = SF,
			U_Classes=case lists:keyfind(Label, 2, Classes) of
				false ->%io:format("new~p~n",[{Label,SampleFeatures}]),
					%create new class with a single sample, give it the appropriate label.
					New_Category = new_Category(SampleFeatures,Label),
					NewClass = #class{label=Label,categories=[New_Category]},
					%io:format("#### Creating new class ####~n Label: ~p FeatureSet: ~p~n NewClass:~p~n~n",[Label,SampleFeatures,NewClass]),
					[NewClass|Classes];
				Class ->%io:format("updating~p~n",[{Label,SampleFeatures}]),
					%io:format("Class:~p~n",[Class]),
					U_Categories= case Class#class.categories of
						[] ->
							New_Category = new_Category(SampleFeatures,Label),
							[New_Category|Class#class.categories];
						_ ->
							{BestDist,BestMatch,BestParameters}=get_BestMatch(Class#class.categories,SampleFeatures,CT,CF,{undefined,undefined,undefined}),
							case BestMatch of
								undefined ->
									New_Category = new_Category(SampleFeatures,Label),
									[New_Category|Class#class.categories];
								_ ->
									%TODO: BestDist, the output of the NN, can be negative. Currently we deal with that by simply using it as an amplitude.
									%TODO add, so that if threshold is still undefined, to base it on the diff between the weight, and this new label.
									%io:format("BestDist:~p Threshold:~p~n",[BestDist,BestMatch#category.threshold]),
									U_Category=update_Category(BestMatch,SampleFeatures,BestParameters),
									lists:keyreplace(U_Category#category.label,2,Class#class.categories,U_Category)
							end

					end,
					U_Class = Class#class{categories=U_Categories},
					%io:format("#### Updating existing class ####~n Label: ~p FeatureSet: ~p~n BestDist: ~p Threshold: ~p~n OldClass: ~p~n NewClass: ~p~n~n",[Label,SampleFeatures,BestDist,BestMatch#category.threshold,Class,U_Class]),
					lists:keyreplace(Label,2,Classes,U_Class)
			end,
			{[Label],U_Classes};
		SF ->%Classifying
			%io:format("Classifying~n"),
			case Classes of
				[] ->
					{[undefined],Classes};
				_ ->
					%SampleFeatures = normalize(SF),
					SampleFeatures = SF,
					[{BestDist,BestMatch,BestParameters}|Tail_BestMatches] = lists:sort([get_BestMatch(Class#class.categories,SampleFeatures,CT,CF,{undefined,undefined,undefined}) || Class <- Classes]),
					case BestMatch of
						undefined ->
							{[undefined],Classes};
						_ ->
							{[BestMatch#category.label],Classes}
					end
			end
	end.
		
	normalize(SF)-> 
		Normalizer = math:sqrt(lists:sum([Val*Val||Val<-SF])),
		[Val/Normalizer || Val <- SF].

		get_BestMatch([C|Categories],Sample,CT,CF,{BestMatch,BestDist,BestParameters})->
			advanced_fanout2(CT,Sample,C#category.weights),
			[{SCCF,N_Ids}] = CF,
			{NewDist,NewParameters}=case SCCF#sCF.name of
				template_update ->
					Delta_VectorP=neuro_FanIn(N_Ids,[]);
				distance_lp ->
					receive
						{I_PId,forward,[Distance,LP]}->
							{Distance,LP}
					end;
				distance_threshold ->
					receive
						{I_PId,forward,[Distance,Threshold]}->
							{Distance,Threshold}
					end;
				distance ->
					receive
						{I_PId,forward,[Distance]}->
							{Distance,undefined}
					end
			end,
%PreProcessor=none,%Could be normalization, or self diff, or vector diff
%SignalIntegrator=distance,
%ActivationFunction=none,
%PostProcessor=threshold,
%Plasticity=none,
%W = T#category.weights,
%Threshold=0,
%io:format("Here~n"),
%ProcessedInput = Sample,%neuron:PreProcessor(T#category.weights,Sample),
%IntegratedSignal = vector_distance(W,ProcessedInput),
%Output = IntegratedSignal,%neuron:ActivationFunction(IntegratedSignal),
%Result = neuron:threshold(W,Sample,Output,Threshold),
%NewDist = Output,
%NewParameters = 0.1,io:format("there~n"),
			%io:format("NewDist:~p~n",[NewDist]),
			case (NewDist < BestDist) and (NewDist < C#category.threshold) of
				true ->
					get_BestMatch(Categories,Sample,CT,CF,{C,NewDist,NewParameters});
				false ->
					get_BestMatch(Categories,Sample,CT,CF,{BestMatch,BestDist,BestParameters})
			end;
		get_BestMatch([],_Sample,_CT,_CF,{BestMatch,BestDist,BestParameters})->
			{BestDist,BestMatch,BestParameters}.
			
			neuro_FanIn(N_Ids)->
				neuro_FanIn(N_Ids,[]).
			neuro_FanIn([N_Id|N_Ids],Acc)->
				receive
					{I_PId,forward,[Val]}->
						neuro_FanIn(N_Ids,[Val|Acc])
				end;
			neuro_FanIn([],Acc)->
				lists:reverse(Acc).
			
			advanced_fanout2([{SCT,To_PIdPs}|CT],I_Coord,Coord)->
				Function=SCT#sCT.name,
				Vector = geometry:Function(I_Coord,Coord),
				[To_PId ! {self(),forward,Vector} || {To_PId,_FilterTag}<-To_PIdPs],
				advanced_fanout2(CT,I_Coord,Coord);
			advanced_fanout2([],_I_Coord,_Coord)->
				done.
	new_Category(X,Label)->
		%Threshold=math:sqrt(length(X)*abs(lists:max(X))),
		Threshold=0.1,
		%LP = random:uniform(),
		LP = 0,1,
		%io:format("New Threshold:~p~n",[Threshold]),
		#category{label=Label,weights=X,threshold=Threshold,lp=LP}.
	
	update_Category(T,X,undefined)->
		update_Category(T,X,T#category.lp);
	update_Category(T,X,LP)->
		W = T#category.weights,
		U_W = vector_add([(1-LP)*Val||Val<-W],[LP*Val|| Val<-X]), %U_W = (1-B)*W + B*X
		T#category{weights=U_W}.













%We shall have, for AARTMAP, not a single template, but a list of lists, or a list of records where each record holds multiple categories. So then, when a new sample arives and does not belong to the existing categories, we create a new one but belonging to the same index, the same record, and add that template to the record. Is this somewhat similar (except for the indirect encoding part) to how SFAM works?
train_ART(Samples)->
	Categories=[],
	train_ART(Samples,Categories).
	
	train_ART([X|Input],Categories)->
		U_Categories=case Categories of
			[] ->
				New_Category = new_Category(X,length(Categories)+1),
				[New_Category|Categories];
			_ ->
				{BestMatch,BestDist,BestParameters}=get_BestMatch(Categories,X,void,void,{undefined,undefined,undefined}),
				case BestMatch == undefined of
					false ->
						U_Category=update_Category(BestMatch,X,BestParameters),
						lists:keyreplace(U_Category#category.label,2,Categories,U_Category);
					true ->
						New_Category = new_Category(X,length(Categories)+1),
						[New_Category|Categories]
				end
		end,
		train_ART(Input,U_Categories);
	train_ART([],Categories)->
		Categories.

train_ARTMAP(Labled_Samples)->
	Classes = [],
	train_ARTMAP(Classes,Labled_Samples).

	train_ARTMAP(Classes,[Labled_Sample|Labled_Samples])->
		case Labled_Sample of
			{Label,SampleFeatures}->
				U_Classes=case lists:keyfind(Label, 2, Classes) of
					false ->
						%create new class with a single sample, give it the appropriate label.
						New_Category = new_Category(SampleFeatures,Label),
						NewClass = #class{label=Label,categories=[New_Category]},
						[NewClass|Classes];
					Class ->
						{BestDist,BestMatch,BestParameters}=get_BestMatch(Class#class.categories,SampleFeatures,{undefined,undefined,undefined}),
						U_Categories=case BestMatch == undefined of
							false ->
								U_Category=update_Category(BestMatch,SampleFeatures,BestParameters),
								lists:keyreplace(U_Category#category.label,2,Class#class.categories,U_Category);
							true ->
								New_Category = new_Category(SampleFeatures,Label),
								[New_Category|Class#class.categories]
						end,
						U_Class = Class#class{categories=U_Categories},
						lists:keyreplace(Label,2,Classes,U_Class)
				end,
				io:format("Label:~p~n",[Label]),
				train_ARTMAP(U_Classes,Labled_Samples);
			SampleFeatures->
				[{BestDist,BestMatch,BestParameters}|TailBestMatches]=lists:sort([get_BestMatch(Class#class.categories,SampleFeatures,{undefined,undefined,undefined})||Class<-Classes]),
				case BestMatch == undefined of
					true ->
						io:format("Label:~p~n",[undefined]);
					false ->
						io:format("Label:~p~n",[BestMatch#category.label])
				end,
				train_ARTMAP(Classes,Labled_Samples)
		end;
	train_ARTMAP(Classes,[])->
		Classes.
		
		get_BestMatch([Category|Categories],SampleFeatures,{BestMatch,BestDist,BestParameters})->
			PreProcessor=none,%Could be normalization, or self diff, or vector diff
			SignalIntegrator=distance,
			ActivationFunction=none,
			PostProcessor=threshold,
			Plasticity=none,
			W = Category#category.weights,
			Threshold=0,
			ProcessedInput = SampleFeatures,%neuron:PreProcessor(T#category.weights,Sample),
			IntegratedSignal = vector_distance(W,ProcessedInput),
			Output = IntegratedSignal,%neuron:ActivationFunction(IntegratedSignal),
			%Result = neuron:threshold(W,SampleFeatures,Output,Threshold),
			NewDist = Output,
			NewParameters = 0.1,

			case (NewDist < BestDist) and (NewDist < Category#category.threshold) of
				true ->
					get_BestMatch(Categories,SampleFeatures,{Category,NewDist,NewParameters});
				false ->
					get_BestMatch(Categories,SampleFeatures,{BestMatch,BestDist,BestParameters})
			end;
		get_BestMatch([],_SampleFeatures,{BestMatch,BestDist,BestParameters})->
			{BestDist,BestMatch,BestParameters}.

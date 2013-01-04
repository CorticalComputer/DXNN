%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This source code and work is provided and developed by Gene I. Sher & DXNN Research Group WWW.DXNNResearch.COM
%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%


-module(actuators).
-compile(export_all).
-include("forex_db.hrl").
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Effectors Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DISPLAY_MODE,false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Affectors are functions that affect upon the environment, such as move the system, write to files, write to databases, move appendages...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Pole balancing actuator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Output: [Force], used to push a cart, between -1 and 1, scaled. with -1 pushing in the oposite direction of 1.
pole2_balancing(ExoSelf,Output,ActuatorId,Parameters)->
	%preprocess:
	[Force|_] = Output,
	%send_signal
%	Scape_PId = get(scape_PId),
%	{AFF,F} = gen_server:call(Scape_PId,{control,pole2_balancing,{actuator,Force,ActuatorId,Parameters}}).
	{Progress,Fitness}=simulations:pole2_balancing(ExoSelf,Force,ActuatorId,Parameters),
	case get(opmode) of
		test ->
			{Progress,[Fitness]};
		_ ->
			{Progress,[Fitness]}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% db compare actuator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Output: Outputs a value [Val1...Valn], and compares it to the one in a database TableName.
db_mimic(ExoSelf,Output,ActuatorId,Parameters)->
	TableName = ActuatorId,
	[Feature] = Parameters,
	simulations:mimic(ExoSelf,Output,TableName,Feature).

xor_output(ExoSelf,Output,ActuatorId,[Feature])->
	{Index,FitnessAcc}= get(index),
	TargetOutput = ets:lookup_element(xor_table,Index,Feature),
	Fitness = simulations:sse(TargetOutput,Output,0),
	U_FitnessAcc = FitnessAcc + Fitness,
	NextIndex = ets:next(xor_table,Index),
	case NextIndex == '$end_of_table' of
		true ->
			erase(index),
			{1,simulations:sse(U_FitnessAcc)};
		false ->
			put(index,{NextIndex,U_FitnessAcc}),
			{0,U_FitnessAcc}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% classifier actuator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vowel_recognition(ExoSelf,Output,ActuatorId,Parameters)->
	%Read from ets.
	%compare to Output
	%Give fitness score.
	%{Progress,Fitness} 1=done, 0 = go
	Index = get(vowel_recognition),
	[Class]=ets:lookup_element(vowel_recognition,Index,4),
	Expected_Output = class2list(Class,11,[]),
	%Fitness=case [round(Val)||Val<-Output]==Expected_Output of
	%	true ->
	%		1;
	%	false ->
	%		0
	%end,
	Distance = geometry:distance(Output,Expected_Output),
	Fitness = 1/(Distance+1),
	%io:format("Expected Output:~p~n Output:~p~n",[Expected_Output,Output]),
	case Index == 528 of %1-528 train, 529-990 test
		true ->
			erase(vowel_recognition),
			{1,[Fitness]};
		false ->
			put(vowel_recognition,Index+1),
			{0,[Fitness]}
	end.
	
	class2list(Class,0,Acc)->
		Acc;
	class2list(Class,Index,Acc)->
		case Class == Index of
			true ->
				class2list(Class,Index-1,[1|Acc]);
			false ->
				class2list(Class,Index-1,[0|Acc])
		end.
			
mines_vs_rocks(ExoSelf,Output,ActuatorId,Parameters)->
	%Read from ets.
	%compare to Output
	%Give fitness score.
	%{Progress,Fitness} 1=done, 0 = go
	Index = get(mines_vs_rocks),
	Expected_Output=ets:lookup_element(mines_vs_rocks,Index,4),
	Fitness=case [round(Val)||Val<-Output]==Expected_Output of
		true ->
			1;
		false ->
			0
	end,
	%Distance = geometry:distance(Output,Expected_Output),
	%Error = 1/(Distance+0.0001),
	case Index == 104 of %1-104 105-208
		true ->
			erase(mines_vs_rocks),
			{1,[Fitness]};
		false ->
			put(mines_vs_rocks,Index+1),
			{0,[Fitness]}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FLATLANDER_ACTUATORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_and_rotate(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),move_and_rotate,Output}),
	{Progress,[Fitness]}.

rotate_and_move(ExoSelf,Output,ActuatorId,Parameters)->
	%io:format("Output:~p~n",[Output]),
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),rotate_and_move,Output}),
	{Progress,[Fitness]}.
	
move_and_translate(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),move_and_translate}),
	{Progress,[Fitness]}.
	
send_signal(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),send_signal}),
	{Progress,[Fitness]}.
	
two_wheels(ExoSelf,Output,ActuatorId,Parameters)->
	OrderFitness=case get(order) of
		{_CCooloff,_CCommandHold,Command,1}->
			Command_CartesianDistance = geometry:distance(Command,Output),
			CImportance = 1,
			(1/Command_CartesianDistance)*CImportance;
		_ ->
			0
	end,
	GuardFitness=case get(guard) of
		{_GCooloff,_GCommandHold,DesiredRange,[R,_Theta],1} ->
			%RangeDif=abs(DesiredRange-R),
			%io:format("here~n"),
			GImportance=1,
			(1/(R+1))*GImportance;
		_ ->
			0
	end,
	ObedienceFitness = OrderFitness+GuardFitness,
	%io:format("ObedienceFitness:~p ORderFitness:~p GuardFitness:~p~n",[ObedienceFitness,OrderFitness,GuardFitness]),
	%io:format("Output:~p~n",[Output]),
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),two_wheels,Output}),
	{Progress,[Fitness+ObedienceFitness]}.
	
speak(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),speak,Output}),
	{Progress,[Fitness]}.

create_offspring(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),create_offspring,[ExoSelf|Output]}),
	{Progress,[Fitness]}.
	
spear(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),spear,Output}),
	{Progress,[Fitness]}.
	
shoot(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),shoot,Output}),
	{Progress,[Fitness]}.

fx_Trade(ExoSelf,Output,ActuatorId,Parameters)->
	case get(fx_pid) of
		undefined ->
			PId = fx:sim(self()),
			put(fx_pid,PId);
		PId ->
			PId
	end,
	[TradeSignal] = Output,
	PId ! {self(),trade,'EURUSD15',functions:trinary(TradeSignal)},
	receive 
		{From,{Progress,Fitness}}->
%			io:format("Result:~p~n",[Result]),
			case get(opmode) of
				test ->
					{Progress,[Fitness,0,0]};
				_ ->
					{Progress,[Fitness]}
			end
	end.
	
abc_pred(ExoSelf,[Output],ActuatorId,Parameters)->
	[TableName,StartIndex,EndIndex,StartBenchIndex,EndBenchIndex,StartTestIndex,EndTestIndex] = Parameters,
	case get(abc_pred) of
		undefined ->
			exit("ERROR in actuators:abc_pred/4, key not present~n");
		Ind ->
			Index = case Ind == 0 of
				true -> 1;
				false -> Ind
			end,
			Classification = ets:lookup_element(TableName,Index,3),
			%io:format("Classificatoin:~p~n",[Classification]),
			TP=case (Classification == 1) and (1==functions:bin(Output)) of
				true -> 1;
				false -> 0
			end,
			TN=case (Classification == 0) and (0==functions:bin(Output)) of
				true -> 1;
				false -> 0
			end,	
			Fitness=case Classification == functions:bin(Output) of
				true -> 1;
				false -> 0
			end,
			Progress = case get(opmode) of
				gt ->
					case Index == EndIndex of
						true -> erase(abc_pred),1;
						false -> put(abc_pred,(Index+1) rem 1401),0
					end;
				validation ->
					case Index == EndBenchIndex of
						true -> erase(abc_pred),1;
						false -> put(abc_pred,(Index+1) rem 1401),0
					end;
				test ->
					case Index == EndTestIndex of
						true -> erase(abc_pred),1;
						false -> put(abc_pred,(Index+1) rem 1401),0
					end	
			end,
			%io:format("Progress~p~n",[Progress]),
			case get(opmode) of
				test ->
					{Progress,[Fitness,TP,TN]};
				_ ->
					{Progress,[Fitness]}
			end
	end.

epiwalker_Mark(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness,Accuracy,Sensitivity,Specificity,MCC} = epiwalker_act({mark,Output}),
%	io:format("Result:~p~n",[Result]),
	case get(opmode) of
		gt ->   
			case Progress of
				1 ->% io:format("Trn_Fitness:~p Accuracy:~p Sensitivity:~p Specificity:~p MCC:~p~n",[Fitness,Accuracy,Sensitivity,Specificity,MCC]); 
					ok;
				_ -> ok
			end,
			{Progress,[Fitness]};
		validation ->%io:format("Val_Fitness:~p Accuracy:~p Sensitivity:~p Specificity:~p MCC:~p~n",[Fitness,Accuracy,Sensitivity,Specificity,MCC]),
			{Progress,[Fitness,Accuracy,Sensitivity,Specificity,MCC]};
		test ->%io:format("Tst_Fitness:~p Accuracy:~p Sensitivity:~p Specificity:~p MCC:~p~n",[Fitness,Accuracy,Sensitivity,Specificity,MCC]),
			{Progress,[Fitness,Accuracy,Sensitivity,Specificity,MCC]}
	end.

epiwalker_MarkAART(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness,Accuracy,Sensitivity,Specificity,MCC} = epiwalker_act({mark,Output}),
	%io:format("Output:~p~n",[Output]),
	case get(first) of
		undefined ->%io:format("Actuators, first~n"),
			case Progress of
				1 ->io:format("Actuators, progress=1, put(first,ok)~n"),
					put(first,ok);
				_ ->%io:format("Actuators, progress=/=1~n"),
					ok
			end;
		_ ->%io:format("Actuators, first==ok~n"),
			ok
	end,
	case get(opmode) of
		gt ->%io:format("Actuators, gt~n"),
			case Progress of
				1 ->%io:format("Actuators, gt, progress 1~n"),
					case get(epi_walker_table) of
						training ->%io:format("Actuators, gt, progres1, put validation~n"),
							put(epi_walker_table,validation),
							U_Progress = 0,
							io:format("Trn_Fitness(Just training, no true fitness):~p Accuracy:~p Sensitivity:~p Specificity:~p MCC:~p~n",[Fitness,Accuracy,Sensitivity,Specificity,MCC]),
							{U_Progress,[0]};
						validation ->%io:format("Actuators, gt, progress 1, put training~n"),
							put(epi_walker_table,training),
							U_Progress = Progress,
							io:format("Val_Fitness:~p Accuracy:~p Sensitivity:~p Specificity:~p MCC:~p~n",[Fitness,Accuracy,Sensitivity,Specificity,MCC]),
							{U_Progress,[Fitness]}
					end;
				0 ->%io:format("Actuators, progresss=0~n"),
					U_Progress = Progress,
					{U_Progress,[Fitness]}
			end;
		validation ->%io:format("Actuators, validation~n"),
			case Progress ==1 of
				true ->
					io:format("Val/Tst_Fitness:~p Accuracy:~p Sensitivity:~p Specificity:~p MCC:~p~n",[Fitness,Accuracy,Sensitivity,Specificity,MCC]),
					ok;
				false -> ok
			end,
			{Progress,[Fitness,Accuracy,Sensitivity,Specificity,MCC]};
		test ->%io:format("Actuators, test~n"),
			case Progress ==1 of
				true ->
					io:format("Test_Fitness:~p Accuracy:~p Sensitivity:~p Specificity:~p MCC:~p~n",[Fitness,Accuracy,Sensitivity,Specificity,MCC]),
					ok;
				false -> ok
			end,
			{Progress,[Fitness,Accuracy,Sensitivity,Specificity,MCC]}
	end.

	-record(epiwalker_state,{
		table_name,
		key,
		mode,
		prim_seq,
		pcc,
		marker_seq,
		window_size,
		map,
		epi_reward,
		nonepi_reward,
		true_positive_acc=0,
		true_negative_acc=0,
		false_positive_acc=0,
		false_negative_acc=0,
		tot_epi_residues=0,
		tot_nonepi_residues=0,
		epi_acc=0,
		nonepi_acc=0,
		next
	}).
	
	epiwalker_act({mark,Output})->
		Mark = case Output of
			[undefined] -> 0;
			[O] -> O
		end,
		Threshold=0.1,
		%[Mark] = Output,
		S = get(epiwalker_state),
		TableName=S#epiwalker_state.table_name,
		[TargetResidue|TailMarkerSeq]=S#epiwalker_state.marker_seq,
		%Mark=-1,
		[TruePositive,TrueNegative,FalsePositive,FalseNegative]=case (TargetResidue == 69) or (TargetResidue == 101) of
			true ->%io:format("TargetResidue:~p Mark:~p~n",[TargetResidue,Mark]),
				case Mark > Threshold of
					true -> 
						[1,0,0,0];
					false -> 
						case Mark < (-Threshold) of
							true ->
								[0,0,0,1];
							false ->
								[0,0,0,0]
						end
				end;
			false ->
				case Mark < (-Threshold) of
					true ->
						[0,1,0,0];
					false ->
						case Mark > Threshold of
							true ->
								[0,0,1,0];
							false ->
								[0,0,0,0]
						end
				end
		end,
		%io:format("Mark:~p EpiPred:~p NonEpiPred:~p EpiReside:~p~n",[Mark,TruePositive,TrueNegative,((TargetResidue == 69) or (TargetResidue == 101))]),
		[_|TailPrimSeq] = S#epiwalker_state.prim_seq,
		[_|TailPCCSeq] = S#epiwalker_state.pcc,
		%io:format("Fitness:~p~n",[Fitness]),
		case TailMarkerSeq of
			[] ->
				case ets:next(S#epiwalker_state.table_name,S#epiwalker_state.key) of
					'$end_of_table' ->%io:format("EndOfTable~n"),
						put(epiwalker_state,S#epiwalker_state{next=reset,true_positive_acc=0,true_negative_acc=0,false_positive_acc=0,false_negative_acc=0}),
						EpiReward = S#epiwalker_state.epi_reward,
						NonEpiReward = S#epiwalker_state.nonepi_reward,
						
						TP=S#epiwalker_state.true_positive_acc+TruePositive,
						TN=S#epiwalker_state.true_negative_acc+TrueNegative,
						FP=S#epiwalker_state.false_positive_acc+FalsePositive,
						FN=S#epiwalker_state.false_negative_acc+FalseNegative,
						
						%TP_Epi=(S#epiwalker_state.epi_acc+EpiPred),
						%TN_Epi=(S#epiwalker_state.nonepi_acc+NonEpiPred),
						%Accuracy = TP_Epi*EpiReward + TN_Epi*NonEpiReward,% - FP_Epi*EpiReward - FN_Epi*NonEpiReward,
						%io:format("TP:~p TN:~p FP:~p FN:~p~n",[TP,TN,FP,FN]),
						Sensitivity= case (TP+FN) == 0 of
							true -> 0;
							false -> 100*(TP/(TP+FN))
						end,
						Specificity = case (TN+FP) == 0 of
							true -> 0;
							false -> 100*(TN/(TN+FP))
						end,
						Accuracy = case (TP+TN+FP+FN) == 0 of
							true -> 0;
							false -> 100*((TP+TN)/(TP+TN+FP+FN))
						end,
						MCC = case math:sqrt((TP+FN)*(TN+FP)*(TP*FP)*(TN+FN)) == 0 of
							true ->
								0;
							false ->
								(TP*TN - FP*FN)/math:sqrt((TP+FN)*(TN+FP)*(TP*FP)*(TN+FN))
						end,
						Fitness = (EpiReward/NonEpiReward)*TP+TN,
						io:format("Fitness:~p TP:~p TN:~p FP:~p FN:~p~n",[Fitness,TP,TN,FP,FN]),
						%io:format("TP:~p~n TN:~p~n FP:~p~n FN:~p~n EpiReward:~p~n NonEpiReward:~p~n Sensitivity:~p~n Specificity:~p~n Accuracy:~p~n MCC:~p~n Fitness:~p~n~n",[TP,TN,FP,FN,EpiReward,NonEpiReward,Sensitivity,Specificity,Accuracy,MCC,Fitness]),
						%TP=TP_Epi/S#epiwalker_state.tot_epi_residues,
						%TN=TN_Epi/S#epiwalker_state.tot_nonepi_residues,
						%FP=FP_Epi/S#epiwalker_state.tot_epi_residues,
						%FN=FN_Epi/S#epiwalker_state.tot_nonepi_residues,
						%io:format("TP_Epi:~p TN_Epi:~p Fitness:~p TP:~p TN:~p TotEpi:~p TotNonEpi:~p~n",[TP_Epi,TN_Epi,Fitness,TP,TN,S#epiwalker_state.tot_epi_residues,S#epiwalker_state.tot_nonepi_residues]),
						{1,Fitness,Accuracy,Sensitivity,Specificity,MCC};
					NextKey ->%io:format("NextKey:~p~n",[NextKey]),
						put(epiwalker_state,S#epiwalker_state{
							next=next,
							true_positive_acc=S#epiwalker_state.true_positive_acc+TruePositive,
							true_negative_acc=S#epiwalker_state.true_negative_acc+TrueNegative,
							false_positive_acc=S#epiwalker_state.false_positive_acc+FalsePositive,
							false_negative_acc=S#epiwalker_state.false_negative_acc+FalseNegative
						}),
%						{0,Fitness,TP,TN}
						{0,0,0,0,0,0}
				end;
			_ ->%io:format("Act~n"),
				%io:format("length(TailPrimSeq):~p~n",[length(TailPrimSeq)]),
				put(epiwalker_state,S#epiwalker_state{
					prim_seq=TailPrimSeq,
					marker_seq=TailMarkerSeq,
					pcc=TailPCCSeq,
					true_positive_acc=S#epiwalker_state.true_positive_acc+TruePositive,
					true_negative_acc=S#epiwalker_state.true_negative_acc+TrueNegative,
					false_positive_acc=S#epiwalker_state.false_positive_acc+FalsePositive,
					false_negative_acc=S#epiwalker_state.false_negative_acc+FalseNegative
				}),
%				{0,Fitness,TP,TN}
				{0,0,0,0,0,0}
		end.
		
aart_classifier(ExoSelf,Output,ActuatorId,Parameters)->
	%io:format("aart_classifier:~p~n",[{ExoSelf,Output,ActuatorId,Parameters}]),
	{Progress,Reward}=classification_scape:classify_act(Output),
	%io:format("Result:~p~n",[{Progress,Reward}]),
	case get(opmode) of
		test ->
			{Progress,[Reward,0,0]};
		_ ->
			{Progress,[Reward]}
	end.
	
	classify_act([OutputLabel])->
		{[{Seq,Label,Reward}|List],TN} = get(seqPs),
		%io:format("Acting~n"),
		Progress = case List of
			[] ->
				1;
			_ ->
				0
		end,
		put(seqPs,{List,TN}),
		case OutputLabel == Label of
			true ->
				{Progress,Reward};
			false ->
				{Progress,0}
		end.

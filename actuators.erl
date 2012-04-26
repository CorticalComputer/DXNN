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
	simulations:pole2_balancing(ExoSelf,Force,ActuatorId,Parameters).

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
			{1,Fitness};
		false ->
			put(vowel_recognition,Index+1),
			{0,Fitness}
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
			{1,Fitness};
		false ->
			put(mines_vs_rocks,Index+1),
			{0,Fitness}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FLATLANDER_ACTUATORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_and_rotate(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),move_and_rotate,Output}).

rotate_and_move(ExoSelf,Output,ActuatorId,Parameters)->
	%io:format("Output:~p~n",[Output]),
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),rotate_and_move,Output}).
	
move_and_translate(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),move_and_translate}).
	
send_signal(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),send_signal}).
	
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
	{Progress,Fitness+ObedienceFitness}.
	
speak(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),speak,Output}).

create_offspring(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),create_offspring,[ExoSelf|Output]}).
	
spear(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),spear,Output}).
	
shoot(ExoSelf,Output,ActuatorId,Parameters)->
	{Progress,Fitness}=gen_server:call(get(scape),{actuator,get(morphology),shoot,Output}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Pole balancing actuator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Output: [Force], used to push a cart, between -1 and 1, scaled. with -1 pushing in the oposite direction of 1.
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
		{From,Result}->
%			io:format("Result:~p~n",[Result]),
			Result
	end.
	
abc_pred(ExoSelf,[Output],ActuatorId,Parameters)->
	[TableName,StartIndex,EndIndex,StartBenchIndex,EndBenchIndex] = Parameters,
	case get(abc_pred) of
		undefined ->
			exit("ERROR in actuators:abc_pred/4, key not present~n");
		Index ->
			Classification = ets:lookup_element(TableName,Index,3),
			%io:format("Classificatoin:~p~n",[Classification]),
			Progress = case get(opmode) of
				gt ->
					case Index == EndIndex of
						true -> erase(abc_pred),1;
						false -> put(abc_pred,Index+1),0
					end;
				benchmark ->
					case Index == EndBenchIndex of
						true -> erase(abc_pred),1;
						false -> put(abc_pred,Index+1),0
					end
			end,
			%io:format("Progress~p~n",[Progress]),
			case Classification == functions:bin(Output) of
				true ->
					{Progress,1};
				false ->
					{Progress,0}
			end
	end.

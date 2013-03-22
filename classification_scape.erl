%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(classification_scape).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Simulations Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TRN,0.35).
-define(VAL,0.35).
-define(TST,0.30).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(cstate,{
	agent_pid,
	table_name,
	key,
	seqPs,
	start_key,
	end_key,
	tp=0,
	tn=0,
	fp=0,
	fn=0,
	tot=0,
	err=0
}).

start()->io:format("Start~n"),
	spawn(classification_scape,loop,[#cstate{}]).
	
loop(SeqPs)->
	receive
		{From,sense,TableName,Modes}->
			case SeqPs of
				[] ->
					[{Seq,Label,Reward}|List] = compose_list(TableName,Modes),
					From ! {self(),percept,[Label|Seq]},
					classification_scape:loop([{Seq,Label,Reward}|List]);
				[{Seq,Label,Reward}|List] ->
					From ! {self(),percept,[Label|Seq]},
					classification_scape:loop([{Seq,Label,Reward}|List])
			end;
		{From,classification,[OutputLabel]}->
			[{Seq,Label,Reward}|List] = SeqPs,
			Progress = case List of
				[] ->
					1;
				_ ->
					0
			end,
			case OutputLabel == Label of
				true ->
					From ! {self(),mark_reply,{Progress,Reward}};
				false ->
					From ! {self(),mark_reply,{Progress,0}}
			end,
			classification_scape:loop(List);
		terminate->
			ok
	end.

classify_sense(TableName,Modes)->
	%io:format("classify_sense(TableName,Modes): ~p ~p~n",[TableName,Modes]),
	U_SeqPs=case get(classification_state) of
		undefined ->
			%io:format("undefined~n"),
			{ok,TN} = ets:file2tab(TableName),
			[{Percept,Label,Reward}|List] = compose_list(TN,Modes--[trn]),
			S = #cstate{seqPs={[{Percept,Label,Reward}|List],TN}},
			put(classification_state,S),
			Percept;
		S->
			case S#cstate.seqPs of
				{[{Percept,Label,Reward}|List],TN} ->
					Percept;
				{[],TN} ->
					%io:format("[] ~p~n",[Modes]),
					[{Percept,Label,Reward}|List] = compose_list(TN,Modes),
					U_S = S#cstate{seqPs={[{Percept,Label,Reward}|List],TN}},
					put(classification_state,U_S),
					Percept
			end
	end.

classify_act([OutputLabel])->
	S = get(classification_state),
	{[{Seq,Label,Reward}|List],TN} = S#cstate.seqPs,
	%io:format("Acting~n"),
	U_TP=case OutputLabel == Label of
		true -> 
			S#cstate.tp+1;
		false ->
			S#cstate.tp
	end,
	U_Tot = S#cstate.tot+1,
	Progress = case List of
		[] ->
			put(classification_state,S#cstate{seqPs={List,TN},tot=0,tp=0}),
			Fitness=U_TP/U_Tot,
			{1,Fitness};
		_ ->
			put(classification_state,S#cstate{seqPs={List,TN},tot=U_Tot,tp=U_TP}),
			{0,0}
	end.

classify_glass([Output])->
	S = get(classification_state),
	{[{Seq,Label,Reward}|List],TN} = S#cstate.seqPs,
	%io:format("Acting~n"),
	Err = math:pow(Output-Label,2),
	U_TP=S#cstate.tp+Err,
	U_Tot = S#cstate.tot+1,
	Progress = case List of
		[] ->
			put(classification_state,S#cstate{seqPs={List,TN},tot=0,tp=0}),
			Fitness=1/U_TP,
			{1,Fitness};
		_ ->
			put(classification_state,S#cstate{seqPs={List,TN},tot=U_Tot,tp=U_TP}),
			{0,0}
	end.


%Converts the provided database into a a list with rewards and labes, used by the loop.
compose_list(TableName,Modes)->
	List = lists:append([extract_mode_samples(TableName,0,Mode,[])||Mode<-Modes]),
	%io:format("Data sample list:~n~p~n",[List]),
	List.
	
	extract_mode_samples(TableName,Offset,Mode,Acc)->%io:format("Compose list:~p ~p~n",[TableName,Mode]),
		{Start_Key,End_Key,LastKey} = get_Keys(Offset,TableName,Mode),
		%io:format("Start_Key:~p End_Key:~p~n",[Start_Key,End_Key]),
		SeqPs=get_SeqPs(Start_Key,End_Key,TableName,[]),
		SeqFull=case Mode of
			trn->
				[{{Label,Seq},Label,0}||{Seq,Label}<-SeqPs];
			_ ->
				[{Seq,Label,1}||{Seq,Label}<-SeqPs]
		end,
		case ets:member(TableName,LastKey+1) of
			true ->
				extract_mode_samples(TableName,LastKey,Mode,lists:append(SeqFull,Acc));
			false ->
				%io:format("Total length:~p Mode:~p~n",[length(lists:append(SeqFull,Acc)),Mode]),
				lists:append(SeqFull,Acc)
		end.
		
		get_SeqPs(End_Key,End_Key,TableName,Acc)->
			%io:format("End_Key:~p~n",[{End_Key}]),
			[{Key,Seq,Label}] = ets:lookup(TableName,End_Key),
			[{Seq,Label}|Acc];
		get_SeqPs(Key,End_Key,TableName,Acc)->
			%io:format("all:~p~n",[{Key,End_Key}]),
			[{Key,Seq,Label}] = ets:lookup(TableName,Key),
			get_SeqPs(Key+1,End_Key,TableName,[{Seq,Label}|Acc]).
			
%The point here is that we have to, during training at least, choose X percent from each category to present to the classification system. Thus we define macros specifying the percentage we will take from each category/class, to present during each mode (training, validation, testing). So then, if TRN = 30, and VAL=60, then TRN takes the first 30 percent, and VAL takes the second 30 percent (from 30 to 60). And If we also have Testing, then Testing takes from Traning to the end, in this case 60-100, or 40% of the total data.

get_Keys(Offset,TableName,Mode)->
	%StartKey, rn until the end of the class
	%Count the members int he class
	%based on mode, calculate the index of start and finish
	%return the index keys.
	%io:format("Here~n"),
	{Length,LastKey}=count_class_length(Offset+1,TableName),%LastKey is also the next class' ClassOffset
	%io:format("Here:~p~n",[{Length,LastKey}]),
	case Mode of
		trn ->
			Start_Key = Offset+1,
			End_Key = Offset+trunc(?TRN*Length),
			{Start_Key,End_Key,LastKey};
		val ->
			Start_Key = Offset+trunc(?TRN*Length)+1,
			End_Key = Offset+trunc(?TRN*Length)+trunc(?VAL*Length),
			%io:format("Val length:~p~n",[End_Key-Start_Key]),
			{Start_Key,End_Key,LastKey};
		tst ->
			Start_Key = Offset+trunc(?TRN*Length)+trunc(?VAL*Length)+1,
			End_Key = LastKey,
			{Start_Key,End_Key,LastKey}
	end.
	
	count_class_length(StartKey,TableName)->
		Label=ets:lookup_element(TableName,StartKey,3),
		%io:format("Label:~p~n",[Label]),
		count_class_length(StartKey,TableName,Label,1).
		
		count_class_length(Key,TableName,Label,Acc)->
			%io:format("Lookup:~p~n",[ets:lookup(TableName,Key)]),
			case ets:member(TableName,Key) of
				true ->
					case Label == ets:lookup_element(TableName,Key,3) of
						true ->
							count_class_length(Key+1,TableName,Label,Acc+1);
						false ->
							{Acc-1,Key-1}
					end;
				false ->
					{Acc-1,Key-1}
			end.

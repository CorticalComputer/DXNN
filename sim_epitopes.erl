%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(sim_epitopes).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Simulations Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{
	agent_pid,
	table_name,
	key,
	mode,
	prim_seq,
	marker_seq,
	window_size,
	pcc
}).

load_tables()->
	spawn(sim_epitopes,loader,[]).
	loader()->
		ets:file2tab(training),
		ets:file2tab(validation),
		ets:file2tab(testing).
		
residue_counter(TableName)->
	{ok,TN}=ets:file2tab(TableName),
	residue_counter(TN,ets:first(TN),0).
	
	residue_counter(TableName,'$end_of_table',TotResidues)->
		ets:delete(TableName),
		io:format("TotResidues:~p~n",[TotResidues]);
	residue_counter(TableName,Key,Acc)->
		Sequence=ets:lookup_element(TableName,Key,4),
		residue_counter(TableName,ets:next(TableName,Key),length(Sequence)+Acc).
%The ets is composed of multiple sequences, and the agent has a particular sliding window lenth, where the element in the center is the one that the agent's actuator marks. 
%Baed on the length of the sliding window, the extra Xs are added to the start of every sequence and end, so that the agent can always be "focused"
%on the central element when writing, so then when the sliding window's central element is the first element in the sequence, the WindowLength/2 number of elements need
%to be added, thesea re the X elements.
start()->io:format("Start~n"),
	spawn(sim_epitopes,loop,[#state{}]).
	
loop(S)->
	receive
		{From,prim_seq,WindowSize,TableName}->
			Percept=case S#state.agent_pid of
				undefined ->
					SideLength = trunc(WindowSize/2),
					{ok,TN}=ets:file2tab(TableName),
					Key=ets:first(TN),
					SideSeq = lists:flatten(lists:duplicate(SideLength,"X")),
					Prim_Seq = SideSeq ++ ets:lookup_element(TN,Key,3) ++ SideSeq,
					U_S = #state{
						key=Key,
						prim_seq=Prim_Seq,
						marker_seq=ets:lookup_element(TN,Key,4),
						table_name = TN,
						agent_pid = From,
						window_size = WindowSize
					},
					%io:format("1prim_seq WindowSize:~p~n WindowSize:~p~n Seq:~p~n",[TableName,WindowSize,lists:sublist(Prim_Seq,WindowSize)]),
					From ! {self(),percept_PrimSeq,lists:sublist(Prim_Seq,WindowSize)},
					loop(U_S);
					%create new index position with the set table name, ets should already exist.
				APId ->%io:format("2prim_seq WindowSize:~p~n WindowSize:~p~n Seq:~p~n",[TableName,WindowSize,lists:sublist(S#state.prim_seq,WindowSize)]),
					From ! {self(),percept_PrimSeq,lists:sublist(S#state.prim_seq,WindowSize)},
					loop(S)
					%access the tablename and return the vector
			end;
		{From,pcc,WindowSize,TableName}->
			ok;
		{From,mark,Output}->
			TableName=S#state.table_name,
			[TargetResidue|TailMarkerSeq]=S#state.marker_seq,
			[Mark] = Output,
			%io:format("Mark:~p~n",[{Mark,TargetResidue}]),
			Fitness=case (TargetResidue == 69) or (TargetResidue == 101) of
				true ->
					(2 - (1 - functions:sat(Mark,1,-1)))/2;
				false ->
					(2 + (-1 - functions:sat(Mark,1,-1)))/2
			end,
			[_|TailPrimSeq] = S#state.prim_seq,
			%io:format("length(TailMarkerSeq):~p~n",[length(TailMarkerSeq)]),
			Progress=case TailMarkerSeq of
				[] ->
					%Time to move on to the next sequence
					case ets:next(S#state.table_name,S#state.key) of
						'$end_of_table' ->
							From ! {self(),mark_reply,{1,Fitness}},
							loop(#state{});
						NextKey ->
							From ! {self(),mark_reply,{0,Fitness}},
							WindowSize=S#state.window_size,
							SideLength = trunc(WindowSize/2),
							SideSeq = lists:flatten(lists:duplicate(SideLength,"X")),
							PrimSeq = SideSeq ++ ets:lookup_element(TableName,NextKey,3) ++ SideSeq,
							MarkerSeq=ets:lookup_element(TableName,NextKey,4),
							U_S=S#state{
								key=NextKey,
								prim_seq=PrimSeq,
								marker_seq=MarkerSeq
							},
							loop(U_S)
					end;
				_ ->
								%Check marker_seq,
					%calcualte fitness based on mark output and the actual mark
					%move forward on both, tehe mark_seq and prim_seq
					From ! {self(),mark_reply,{0,Fitness}},
					loop(S#state{prim_seq=TailPrimSeq,marker_seq=TailMarkerSeq})
			end;
		terminate->
			ok
	end.

%Get sequence
%Add sides of length halfwindow trunc
%Extract window
%	remove element from left on window, remove element from left on seq, add seq_elemen to right on window, feed, and move to the right on the marker seq.
%When seq is empty, move to next seq
%	If no more seqs left, training, validation, or testing, is over
init_sequences(TableName,Key,S,WindowSize)->
	SideLength = (WindowSize-1)/2,
	{ok,TN}=ets:file2tab(TableName),
	Key=ets:first(TN),
	PrimSideSeq = lists:flatten(lists:duplicate(SideLength,"X")),
	PrimSeq = ets:lookup_element(TN,Key,3),
	Proper_PrimSeq = PrimSideSeq ++ PrimSeq ++ PrimSideSeq,
	SideSeq = lists:flatten(lists:duplicate(SideLength,"X")),
	CPPSideSeq = lists:flatten(lists:duplicate(SideLength,-1)),
	CPP = calculate_ppc(PrimSeq),
	Proper_CPP = CPPSideSeq ++ CPP ++ CPPSideSeq,
	U_S = #state{
		key=Key,
		prim_seq=Proper_PrimSeq,
		pcc = Proper_CPP,
		marker_seq=ets:lookup_element(TN,Key,4),
		table_name = TN,
		window_size = WindowSize
	}.
	
	calculate_ppc(PrimSeq)->
		SeqLength = length(PrimSeq),
		TableName=ets:new(table,[set,private]),
		calculate_ppc(PrimSeq,TableName),
		CPP=[(ets:lookup_element(TableName,Char,2)/SeqLength)*100 || Char <- PrimSeq],
		ets:delete(TableName),
		CPP.
	calculate_ppc([Char|PrimSeq],TableName)->
		case ets:lookup(TableName,Char) of
			[] ->
				ets:insert(TableName,{Char,1});
			[{Char,Count}]->
				ets:insert(TableName,{Char,Count+1})
		end,
		calculate_ppc(PrimSeq,TableName);
	calculate_ppc([],_TableName)->
		ok.

%{"Amino Acid",	"3char","1char,	"Side Chain Polarity","Side-chain Charge, pH 7.4","Hydropathy Index"}
%{Alanine,	Ala,	A, 	nonpolar, 	neutral, 	1.8,	65}
%{Arginine, 	Arg, 	R, 	polar, 		positive, 	−4.5,	82}
%{Asparagine, 	Asn, 	N, 	polar, 		neutral, 	−3.5,	78}
%{Aspartic acid, Asp, 	D, 	polar, 		negative, 	−3.5,	68}
%{Cysteine, 	Cys, 	C, 	polar, 		neutral, 	2.5,	67}
%{Glutamic acid,Glu, 	E, 	polar, 		negative, 	−3.5,	69}
%{Glutamine, 	Gln, 	Q, 	polar, 		neutral, 	−3.5,	81}
%{Glycine, 	Gly, 	G, 	nonpolar, 	neutral, 	−0.4,	71}
%{Histidine, 	His, 	H, 	polar, 		positive(10%), 		72}
%						neutral(90%) 	−3.2,	
%{Isoleucine, 	Ile, 	I, 	nonpolar, 	neutral, 	4.5,	73}
%{Leucine, 	Leu, 	L, 	nonpolar, 	neutral, 	3.8,	76}
%{Lysine, 	Lys, 	K, 	polar, 		positive, 	−3.9,	75}
%{Methionine, 	Met, 	M, 	nonpolar, 	neutral, 	1.9,	77}
%{Phenylalanine, Phe, 	F, 	nonpolar, 	neutral, 	2.8,	70}
%{Proline, 	Pro, 	P, 	nonpolar, 	neutral, 	−1.6,	80}
%{Serine, 	Ser, 	S, 	polar, 		neutral, 	−0.8,	83}
%{Threonine, 	Thr, 	T, 	polar, 		neutral, 	−0.7,	84}
%{Tryptophan, 	Trp, 	W, 	nonpolar, 	neutral, 	−0.9,	87}
%{Tyrosine, 	Tyr, 	Y, 	polar, 		neutral, 	−1.3,	89}
%{Valine, 	Val, 	V, 	nonpolar, 	neutral, 	4.2,	86}
%X 88

%{{MapName::String(),AminoAcid::1Char},MapChar}.
%{{MapName::String(),notes},Notes::String()}

write_test()->
	PrimSeqDec={"PrimSeqDec",[
		{65,-10},
		{82,-9},
		{78,-8},
		{68,-7},
		{67,-6},
		{69,-5},
		{81,-4},
		{71,-3},
		{72,-2},
		{73,-1},
		{88,0},
		{76,1},
		{75,2},
		{77,3},
		{70,4},
		{80,5},
		{83,6},
		{84,7},
		{87,8},
		{89,9},
		{86,10}]},
	
	SideChainPolarity={"SideChainPolarity",[
		{65,-1},
		{82,1},
		{78,1},
		{68,1},
		{67,1},
		{69,1},
		{81,1},
		{71,-1},
		{72,1},
		{73,-1},
		{76,-1},
		{75,1},
		{77,-1},
		{70,-1},
		{80,-1},
		{83,1},
		{84,1},
		{87,-1},
		{89,1},
		{86,-1},
		{88,0}]},
	
	SideChainCharge={"SideChainCharge",[
		{65,0},
		{82,1},
		{78,0},
		{68,-1},
		{67,0},
		{69,-1},
		{81,10},
		{71,0},
		{72,0.1},
		{73,0},
		{76,0},
		{75,1},
		{77,0},
		{70,0},
		{80,0},
		{83,0},
		{84,0},
		{87,0},
		{89,0},
		{86,0},
		{88,0}]},
		
	Hydropathy={"Hydropathy",[
		{65,1.8},
		{82,-4.5},
		{78,-3.5},
		{68,-3.5},
		{67,2.5},
		{69,-3.5},
		{81,-3.5},
		{71,-0.4},
		{72,-3.2},
		{73,4.5},
		{76,3.8},
		{75,-3.9},
		{77,1.9},
		{70,2.8},
		{80,-1.6},
		{83,-0.8},
		{84,-0.7},
		{87,-0.9},
		{89,-1.3},
		{86,4.3},
		{88,0}]},

	%new_map(),
	%{ok,TN} = ets:file2tab(epi_map),
	TN = ets:new(epi_map,[set,private]),
	[update_map(TN,Map) || Map <- [PrimSeqDec,SideChainPolarity,SideChainCharge,Hydropathy]],
	ets:tab2file(TN,epi_map),
	ets:delete(TN).
	
new_empty_map()->
	TN = ets:new(epi_map,[set,private]),
	ets:tab2file(TN,epi_map).

update_map(TN,{MapName,Entries})->
	map(TN,MapName,Entries).
	
	map(TN,MapName,[{AminoAcid,Propensity}|Entries])->
		ets:insert(TN,{{MapName,AminoAcid},Propensity}),
		map(TN,MapName,Entries);
	map(_TN,MapName,[])->
		io:format("Finished mapping:~p~n",[MapName]).
		
ok(EpiReward,NonEpiReward,TargetResidue,Mark)->
	case (TargetResidue == 69) or (TargetResidue == 101) of
		true ->
			(2*EpiReward - (EpiReward - EpiReward*functions:sat(Mark,1,-1)))/2;
		false ->
			(2*NonEpiReward + (-NonEpiReward - NonEpiReward*functions:sat(Mark,1,-1)))/2
	end.

-record(stats,{
	epi_residues=0,
	non_epi_residues=0,
	seq_acc=0,
	tot_epi_reward=0,
	tot_non_epi_reward=0,
	fitness=0,
	tp=0,
	tn=0
}).	
stats()->
	{ok,Tr} = ets:file2tab(training),
	{ok,Va} = ets:file2tab(validation),
	{ok,Te} = ets:file2tab(testing),
	
	count(ets:first(Tr),Tr,#stats{}),
	count(ets:first(Va),Va,#stats{}),
	count(ets:first(Te),Te,#stats{}).
	
	count('$end_of_table',TN,S)->
		io:format("Table:~p Statistics for ~p Seqs:~n Tot Epitope Residues:~p Tot Non-Epitopes Residues:~p EpiReward:~p NonEpiReward:~p Fitness:~p True Positive:~p True Negative:~p~n",[TN,S#stats.seq_acc,S#stats.epi_residues,S#stats.non_epi_residues,S#stats.tot_epi_reward,S#stats.tot_non_epi_reward, S#stats.fitness,S#stats.tp,S#stats.tn]);
	count(Key,TN,S)->
		PrimSeq=ets:lookup_element(TN,Key,3),
		MarkerSeq=ets:lookup_element(TN,Key,4),
		TotResidues = length(PrimSeq),
		EpiResidues = length([Char|| Char<- MarkerSeq, (Char == 69) or (Char == 101)]),
		NonEpiResidues = TotResidues - EpiResidues,
		
		TotEpitopeResidues=length([Char||Char <- MarkerSeq, (Char==69) or (Char==101)]),
		TotResidues=length(PrimSeq),
		EpiReward=50/TotEpitopeResidues,
		NonEpiReward=50/(TotResidues-TotEpitopeResidues),
		
		{Fitness,TPos,TNeg}=lists:unzip3([fitness_calc(Char,EpiReward,NonEpiReward) || Char <- MarkerSeq]),
		U_S=S#stats{
			epi_residues = S#stats.epi_residues+EpiResidues,
			non_epi_residues = S#stats.non_epi_residues+NonEpiResidues,
			seq_acc = S#stats.seq_acc+1,
			tot_epi_reward = S#stats.tot_epi_reward+EpiReward*EpiResidues,
			tot_non_epi_reward = S#stats.tot_non_epi_reward+NonEpiReward*NonEpiResidues,
			fitness = S#stats.fitness+lists:sum(Fitness),
			tp = S#stats.tp + lists:sum(TPos),
			tn = S#stats.tn + lists:sum(TNeg)
		},
			
		count(ets:next(TN,Key),TN,U_S).
		
fitness_calc(TargetResidue,EpiReward,NonEpiReward)->
	{Fitness,TP,TN}=case (TargetResidue == 69) or (TargetResidue == 101) of
		true ->
			Mark = 1,
			Val=case Mark > 0 of
				true -> 1;
				false -> 0
			end,
			{(2*EpiReward - (EpiReward - EpiReward*functions:sat(Mark,1,-1)))/2,Val,0};
		false ->
			Mark = -1,
			Val=case Mark < 0 of
				true -> 1;
				false -> 0
			end,
			{(2*NonEpiReward + (-NonEpiReward - NonEpiReward*functions:sat(Mark,1,-1)))/2,0,Val}
	end.

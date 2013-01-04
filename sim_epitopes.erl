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
		{67,-1},
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
		{81,0},
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
		{86,4.2},
		{88,0}]},
		

	Polarity_Grantham_1974={"Polarity_Grantham_1974",[
		{65,8.1},
		{82,10.5},
		{78,11.6},
		{68,13},
		{67,5.5},
		{69,12.3},
		{81,10.5},
		{71,9},
		{72,10.4},
		{73,5.2},
		{76,4.9},
		{75,11.3},
		{77,5.7},
		{70,5.2},
		{80,8},
		{83,9.2},
		{84,8.6},
		{87,5.4},
		{89,6.2},
		{86,5.9},
		{88,0}]},
		
	Flexibility_KarplusSchulz_1985={"Flexibility_KarplusSchulz_1985",[
		{65,1.041},
		{82,1.038},
		{78,1.117},
		{68,1.033},
		{67,0.96},
		{69,1.094},
		{81,1.165},
		{71,1.142},
		{72,0.982},
		{73,1.002},
		{76,0.967},
		{75,1.093},
		{77,0.947},
		{70,0.93},
		{80,1.055},
		{83,1.169},
		{84,1.073},
		{87,0.925},
		{89,0.961},
		{86,0.982},
		{88,0}]},
		
	Antigenicity_KolaskarTongaonkar_1990={"Antigenicity_KolaskarTongaonkar_1990",[
		{65,1.064},
		{82,0.873},
		{78,0.776},
		{68,0.866},
		{67,1.412},
		{69,0.851},
		{81,1.015},
		{71,0.874},
		{72,1.105},
		{73,1.152},
		{76,1.25},
		{75,0.93},
		{77,0.826},
		{70,1.091},
		{80,1.064},
		{83,1.012},
		{84,0.909},
		{87,0.893},
		{89,1.161},
		{86,1.383},
		{88,0}]},
		
	Hydrophilicity_Parker_1986={"Hydrophilicity_Parker_1986",[
		{65,2.1},
		{82,4.2},
		{78,7},
		{68,10},
		{67,1.4},
		{69,7.8},
		{81,6},
		{71,5.7},
		{72,2.1},
		{73,-8},
		{76,-9.2},
		{75,5.7},
		{77,-4.2},
		{70,-9.2},
		{80,2.1},
		{83,6.5},
		{84,5.2},
		{87,-10},
		{89,-1.9},
		{86,-3.7},
		{88,0}]},
		
	Polarity_Ponnuswamy_1980={"Polarity_Ponnuswamy_1980",[
		{65,0},
		{82,52},
		{78,3.38},
		{68,40.7},
		{67,1.48},
		{69,49.91},
		{81,3.53},
		{71,0},
		{72,51.6},
		{73,0.15},
		{76,0.45},
		{75,49.5},
		{77,1.43},
		{70,0.35},
		{80,1.58},
		{83,1.67},
		{84,1.66},
		{87,2.1},
		{89,1.61},
		{86,0.13},
		{88,0}]},
		
%Amino 
%acid 	Property
%	P1	P2	P3	P4	P5
%Ala 	8.1 	1.041	1.064	2.1	0
%Arg	10.5	1.038	0.873	4.2	52
%Asn	11.6	1.117	0.776	7	3.38
%Asp	13	1.033	0.866	10	40.7
%Cys	5.5	0.96	1.412	1.4	1.48
%Glu	12.3	1.094	0.851	7.8	49.91
%Gln	10.5	1.165	1.015	6	3.53
%Gly	9	1.142	0.874	5.7	0
%His	10.4	0.982	1.105	2.1	51.6
%Ile	5.2	1.002	1.152	-8	0.15
%Leu	4.9	0.967	1.25	-9.2	0.45
%Lys	11.3	1.093	0.93	5.7	49.5
%Met	5.7	0.947	0.826	-4.2	1.43
%Phe	5.2	0.93	1.091	-9.2	0.35
%Pro	8	1.055	1.064	2.1	1.58
%Ser	9.2	1.169	1.012	6.5	1.67
%Thr	8.6	1.073	0.909	5.2	1.66
%Trp	5.4	0.925	0.893	-10	2.1
%Tyr	6.2	0.961	1.161	-1.9	1.61
%Val	5.9	0.982	1.383	-3.7	0.13
		
%P1= Polarity (Grantham, 1974)
%P3= Flexibility (Karplus-Schulz, 1985)
%P3= Antigenicity Kolaskar and Tongaonkar (1990) 
%P4= Hydrophilicity scale (Parker et al., 1986)
%P5= Polarity (Ponnuswamy et al., 1980)

	%new_map(),
	%{ok,TN} = ets:file2tab(epi_map),
	TN = ets:new(epi_map,[set,private]),
	[update_map(TN,Map) || Map <- [PrimSeqDec,SideChainPolarity,SideChainCharge,Hydropathy,Polarity_Grantham_1974,Flexibility_KarplusSchulz_1985,Antigenicity_KolaskarTongaonkar_1990,Hydrophilicity_Parker_1986,Polarity_Ponnuswamy_1980]],
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
	tot_residues=0,
	seq_acc=0,
	epi_reward,
	non_epi_reward
}).

stats()->
	{ok,Tr} = ets:file2tab(training),
	{ok,Va} = ets:file2tab(validation),
	{ok,Te} = ets:file2tab(testing),
	Tr_Stats=count(ets:first(Tr),Tr,0,0),
	Va_Stats=count(ets:first(Va),Va,0,0),
	Te_Stats=count(ets:first(Te),Te,0,0),
	io:format("Tr_Stats:~p~nVa_Stats:~p~nTe_Stats:~p~n",[Tr_Stats,Va_Stats,Te_Stats]).
	
	count('$end_of_table',TN,EpiAcc,NonEpiAcc)->
		EpiReward = 0.5/EpiAcc,
		NonEpiReward = 0.5/NonEpiAcc,
		{EpiReward,NonEpiReward,EpiAcc,NonEpiAcc};
	count(Key,TN,EpiAcc,NonEpiAcc)->
		MarkerSeq=ets:lookup_element(TN,Key,4),
		Residues = length(MarkerSeq),
		EpiResidues = length([Char|| Char<- MarkerSeq, (Char == 69) or (Char == 101)]),
		NonEpiResidues = Residues - EpiResidues,
		count(ets:next(TN,Key),TN,EpiAcc+EpiResidues,NonEpiAcc+NonEpiResidues).

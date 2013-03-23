%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(phenotypic_diversity).
-compile(export_all).
-include("records.hrl").
-define(G,2).%Insertion
-define(H,0.5).%Consecutive insertion

compare_behavior(SHOF,BehavioralSequence)->
	%io:format("********Sequence:~p~n",[BehavioralSequence]),
	[compare(gotoh,Champion,BehavioralSequence) || Champion <- SHOF].
	
	compare(AlgorithmName,Champion,BehavioralSequence)->%AlgorithmName::gotoh,statistics
		[A]=mnesia:read({dx,Champion#champion.id}),
		C_BehavioralSequence=A#dx.behavioral_trace,
		%io:format("********Champion Sequence:~p~n",[C_BehavioralSequence]),
		phenotypic_diversity:AlgorithmName(C_BehavioralSequence,BehavioralSequence).

gotoh(A,B)->
	gotoh(A,B,?G).
gotoh(A,B,TStar)->
	T = ?G,
	{CC,DD} = for1([],[],T,length(B)),
	for2(CC,DD,A,B,inf,inf,inf,TStar).

	for1(CCAcc,DDAcc,_T,0)->
		{[0|lists:reverse(CCAcc)],lists:reverse(DDAcc)}; %CC = [0|...]	
	for1(CCAcc,DDAcc,T,N)->
		U_T = T + ?H,
		for1([U_T|CCAcc],[U_T+?G|DDAcc],U_T,N-1).
		
	for2([U_S|CCTail],DD,[Ai|A],B,E,C,S,T)->
		C0 = U_C = U_T = T + ?H,
		U_E = U_T+?G,
		{U_CC,U_DD}=for3(CCTail,DD,Ai,B,U_E,U_C,U_S,[],[]),
		for2([C0|U_CC],U_DD,A,B,U_E,U_C,U_S,U_T);
	for2(CC,DD,[],_B,_E,_C,_S,_T)->
		[CScore|_]=lists:reverse(CC),
		%io:format("Alignment score:~p~n",[CScore]),
		CScore.
		
		for3([CCj|CC],[DDj|DD],Ai,[Bj|B],E,C,S,Acc1,Acc2)->
			U_E = min(E,C+?G) +?H,
			U_DDj = min(DDj,CCj+?G)+?H,
			U_C = lists:min([U_DDj,U_E,S+w(Ai,Bj)]),
			U_S = CCj,
			U_CCj = U_C,
			for3(CC,DD,Ai,B,U_E,U_C,U_S,[U_CCj|Acc1],[U_DDj|Acc2]);
		for3([],[],_Ai,[],_E,_C,_S,Acc1,Acc2)->
			{lists:reverse(Acc1),lists:reverse(Acc2)}.
			
	gap(K) ->?G + ?H*K.
	
	w(Val,Val)->0;%replacement
	w(Val1,Val2)->abs(Val1-Val2).

%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(preprocessor).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LearningTypes Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
none(DIV)->
	DIV.

normalize(DIV)->
	Normalizer=calculate_normalizer(DIV,0),
	normalize(Normalizer,DIV,[]).
	
	normalize(Normalizer,[{Pid,Input}|DIV],NDIVAcc)->
		normalize(Normalizer,DIV,[{Pid,[I/Normalizer||I<-Input]}|NDIVAcc]);
	normalize(_,[],NDIVAcc)->
		lists:reverse(NDIVAcc).

	calculate_normalizer([{_,InputCluster}|DIV],Acc)->
		IC_Normalizer = lists:sum([Val*Val||Val<- InputCluster]),
		calculate_normalizer(DIV,IC_Normalizer+Acc);
	calculate_normalizer([],Acc)->
		case math:sqrt(Acc) of
			0.0 -> 
				1;
			Normalizer -> 
				Normalizer					
		end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXPERIMENTAL Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_scale()->
	TotInputs = 4,
	DWP = [{From,[{w,{1,-1,-1000,1000,ss},lp}|| _ <- lists:seq(1,2)]} || From <- lists:seq(1,TotInputs)],
	DIV1 = [{From,[random:uniform(),random:uniform()]} || From <- lists:seq(1,TotInputs)],
	DIV2 = [{From,[random:uniform(),random:uniform()]} || From <- lists:seq(1,TotInputs)],
	io:format("DWP:~p DIV:~p~n",[DWP,DIV1]),
	put(sa,98),
	DWP1 = scale(DIV1,DWP),
	io:format("DWP1:~p DIV1:~p~n",[DWP1,DIV1]),
	put(sa,99),
	DWP2 = scale(DIV2,DWP1),
	io:format("DWP2:~p DIV2:~p~n",[DWP2,DIV2]),
	put(sa,100),
	DWP3 = scale(DIV2,DWP2),
	io:format(" DWP3:~p~n DIV2:~p~n",[DWP3,DIV2]).

	scale(DIV,DWP)->
		case get(sa) of %sampling_attempt
			100 ->
				put(sa,1),
				Updated_DWP = scale(DIV,DWP,[]),
				adjust_mm(Updated_DWP,[]);
			_ ->
				scale(DIV,DWP,[])
		end.
	scale([{From,Input}|DIV],[{From,WPC}|DWP],Acc)->
		Updated_WPC = adjust_smm(Input,WPC,[]),
		scale(DIV,DWP,[{From,Updated_WPC}|Acc]);
	scale([],[{threshold,WPC}],Acc)->
		lists:reverse([{threshold,WPC}|Acc]).
		
		
		adjust_smm([I|Input],[{W,{Max,Min,SMax,SMin,Sample_Size},LP}|WPC],Acc)->
			U_SMax = case I > SMax of
				true ->
					I;
				false ->
					SMax
			end,
			U_SMin = case I < SMin of
				true ->
					I;
				false ->
					SMin
			end,
			adjust_smm(Input,WPC,[{W,{Max,Min,U_SMax,U_SMin,Sample_Size},LP}|Acc]);
		adjust_smm([],[],Acc)->
			lists:reverse(Acc).

	adjust_mm([{From,WPC}|DWP],Acc)->
		Updated_WPC = [{W,{(Max+SMax)/2,(Min+SMin)/2,-1000,1000,Sample_Size},LP}||{W,{Max,Min,SMax,SMin,Sample_Size},LP} <- WPC],
		adjust_mm(DWP,[{From,Updated_WPC}|Acc]);
	adjust_mm([],Acc)->
		lists:reverse(Acc).

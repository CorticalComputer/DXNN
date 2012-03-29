%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(learning_types).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LearningTypes Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculate_Output(DWP,DIV,AF)->
	case DWP of %Due to neural type, bst or standard
		{Weight,Bias} ->
			BST_DotProduct = bst_dp(DIV,Weight,Bias,0),
			functions:sat(functions:AF(BST_DotProduct),1,-1);
		_ ->
			%DWP = scale(Normalized_DIV,DWP1),
			Standard_DotProduct = calculate_dotproduct(DIV,DWP,0),
			functions:sat(functions:AF(Standard_DotProduct),1,-1)
	end.

learn(Adapter,DWP,DIV,Output)->
	case Adapter of
		none ->
			DWP;
		modulated ->
			modulated_learning(DIV,DWP,Output);
		differential_hebbian ->
			void
	end.

	bst_dp([{_From,Input}|DIV],Weight,Bias,Acc)->
		%io:format("Input:~p~n",[Input]),
		Sum = lists:sum([Val|| Val <- Input]),
		bst_dp(DIV,Weight,Bias,Sum+Acc);
	bst_dp([],Weight,Bias,Acc)->
		%io:format("~p~n",[{_Weight,Bias}]),
		Bias+Acc*Weight.

%DWij = N(A*Oi*Oj + B*Oi + C*Oj)
modulated_learning(DIV,DWP,Output)->
	{DotProduct1,DotProduct2} = calculate_dotproduct2(DIV,DWP,0,0),
	N = functions:sat(functions:tanh(DotProduct1),1,-1),
	A = functions:sat(functions:tanh(DotProduct2),1,-1),
	B = 0,
	C = 0,
	%io:format("N:~p A:~p~n",[N,A]),
	modulate_DWP(DIV,DWP,Output,N,A,B,C,[]).

	calculate_dotproduct2([{From,Input}|DIV],[{From,WPC}|DWP],DotProductAcc1,DotProductAcc2)->
		{DotProduct1,DotProduct2} = dot2(Input,WPC,0,0),
		calculate_dotproduct2(DIV,DWP,DotProductAcc1+DotProduct1,DotProductAcc2+DotProduct2);
	calculate_dotproduct2([],[],DotProductAcc1,DotProductAcc2)->
		{DotProductAcc1,DotProductAcc2};
	calculate_dotproduct2([],[{threshold,[{_,Threshold1,Threshold2}]}],DotProductAcc1,DotProductAcc2)->
		%[{Threshold,_PDW,_LP}] = get(threshold),
		%io:format("Threshold:~p~n",[Threshold]),
		{DotProductAcc1+Threshold1,DotProductAcc2+Threshold2}.
		
		dot2([Val|Input],[{_,W1,W2}|WPC],DotAcc1,DotAcc2)->
			%io:format("Val:~p W:~p, Val*W:~p~n",[Val,W,Val*W]),
			dot2(Input,WPC,DotAcc1+(Val*W1),DotAcc2+(Val*W2));
		dot2([],[],DotAcc1,DotAcc2)->
			{DotAcc1,DotAcc2}.
		
	modulate_DWP([{From,Input}|DIV],[{From,WPC}|DWP],Output,N,A,B,C,Acc)->
		U_WPC = modulate_WPC(Input,WPC,Output,N,A,B,C,[]),
		modulate_DWP(DIV,DWP,Output,N,A,B,C,[{From,U_WPC}|Acc]);
	modulate_DWP([],[],_Output,_N,_A,_B,_C,Acc)->
		lists:reverse(Acc);
	modulate_DWP([],[{threshold,WPC}],Output,N,A,B,C,Acc)->
		U_WPC = modulate_WPC([1],WPC,Output,N,A,B,C,[]),
		U_DWP = [{threshold,U_WPC}|Acc],
		lists:reverse(U_DWP).
		
		modulate_WPC([Val|Input],[{W,W1,W2}|WPC],Output,N,A,B,C,Acc)->
			U_W = W+N*(A*Val*Output + B*Val + C*Output),
			modulate_WPC(Input,WPC,Output,N,A,B,C,[{U_W,W1,W2}|Acc]);
		modulate_WPC([],[],_Output,_N,_A,_B,_C,Acc)->
			lists:reverse(Acc).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Shared Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculate_dotproduct([{From,Input}|DIV],[{From,WPC}|DWP],DotProductAcc)->
	%WPC = get(From),
	%io:format("From:~p~n Input:~p WPC:~p~n",[From,Input,WPC]),
	DotProduct = dot(Input,WPC,0),
	calculate_dotproduct(DIV,DWP,DotProductAcc+DotProduct);
calculate_dotproduct([],[],DotProductAcc)->
	DotProductAcc;
calculate_dotproduct([],[{threshold,[{Threshold,_PDW,_LP}]}],DotProductAcc)->
	%[{Threshold,_PDW,_LP}] = get(threshold),
	%io:format("Threshold:~p~n",[Threshold]),
	DotProductAcc+Threshold.
	
	dot([Val|Input],[{W,_,_}|WPC],DotAcc)->
		%io:format("Val:~p W:~p, Val*W:~p~n",[Val,W,Val*W]),
		dot(Input,WPC,DotAcc+(Val*W));
	dot([],[],DotAcc)->
		DotAcc.

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

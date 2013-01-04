%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(plasticity).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LearningTypes Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
none(_DIV,_Output,DWP)->
	DWP.
	
%DWij = N(A*Oi*Oj + B*Oi + C*Oj)
modulated(DIV,Output,DWP)->
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

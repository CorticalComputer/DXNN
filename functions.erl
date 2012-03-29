%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(functions).
-compile(export_all).

beta(TotError,DotProduct,EF)->
	TotError*derivatives:EF(DotProduct).
	
	calculate_dotproduct([{From,Input}|DIV],[{From,WPC}|DWP],DotProductAcc)->
		DotProduct = dot(Input,WPC,0),
		calculate_dotproduct(DIV,DWP,DotProductAcc+DotProduct);
	calculate_dotproduct([],FunctionParameters,DotProductAcc)->
		{DotProductAcc,FunctionParameters}.
		
		dot([Val|Input],[{W,_,_}|WPC],DotAcc)->
			dot(Input,WPC,DotAcc+(Val*W));
		dot([],[],DotAcc)->
			DotAcc.
	
saturation(Val)->
	case Val > 1000 of
		true ->
			1000;
		false ->
			case Val < -1000 of
				true ->
					-1000;
				false ->
					Val
			end
	end.
	
saturation(Val,Spread)->
	case Val > Spread of
		true ->
			Spread;
		false ->
			case Val < -Spread of
				true ->
					-Spread;
				false ->
					Val
			end
	end.
scale([H|T],Max,Min)->
	[scale(Val,Max,Min)||Val<-[H|T]];
scale(Val,Max,Min)-> %Nm = (Y*2 - (Max + Min))/(Max-Min)
	%io:format("Val:~p Max:~p Min:~p~n",[Val,Max,Min]),
	case Max == Min of
		true ->
			0;
		false ->
			(Val*2 - (Max+Min))/(Max-Min)
	end.

sat(Val,Max,Min)->
	case Val > Max of
		true ->
			Max;
		false ->
			case Val < Min of
				true ->
					Min;
				false ->
					Val
			end
	end.

sat_dzone(Val,Max,Min,DZMax,DZMin)->
	case (Val < DZMax) and (Val > DZMin) of
		true ->
			0;
		false ->
			sat(Val,Max,Min)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Activation Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tanh(DIV,DWP)->	
	{DotProduct,FunctionParameters} = calculate_dotproduct(DIV,DWP,0),
	%io:format("FP:~p~n",[FunctionParameters]),
	[{threshold,[{Threshold,_TPDW,_TLP}]},{oscaler,[{OScaler,_OsPDW,_OsLP}]},{offset,[{Offset,_OfPDW,_OfLP}]}] = FunctionParameters,
	tanh(DotProduct+Threshold)*OScaler + Offset.
	
tanh(Val)->		
	math:tanh(Val).
		
cos(Val)->
	math:cos(Val).

sin(Val)->
	math:sin(Val).

sgn(0)->
	0;
sgn(Val)->
	case Val > 0 of
		true -> 1;
		false -> -1
	end.

bin(Val)->
	case Val > 0 of
		true -> 1;
		false -> 0
	end.

trinary(Val)->
	if
		(Val < 0.33) and (Val > -0.33) -> 0;
		Val >= 0.33 -> 1;
		Val =< -0.33 -> -1
	end.
	
multiquadric(Val)->
	math:pow(Val*Val + 0.01,0.5).

absolute(Val)->
	abs(Val).
	
linear(Val)->
	Val.

quadratic(Val)->
	sgn(Val)*Val*Val.

gaussian(Val)->
	gaussian(2.71828183,Val).

gaussian(Const,Val)->
	V = case Val > 10 of
		true ->
			10;
		false ->
			case Val < -10 of
				true ->
					-10;
				false ->
					Val
			end
	end,
	math:pow(Const,-V*V).

sqrt(Val)->
	sgn(Val)*math:sqrt(abs(Val)).
	
log(Val)->
	case Val == 0 of
		true ->
			0;
		false ->
			sgn(Val)*math:log(abs(Val))
	end.

sigmoid(Val)-> %(-1 : 1)--Der:Y*(1-Y)
	V = case Val > 10 of
		true ->
			10;
		false ->
			case Val < -10 of
				true ->
					-10;
				false ->
					Val
			end
	end,
	2/(1+math:pow(2.71828183,-V)) - 1.

sigmoid1(Val)-> %(-1 : 1) -- Der:1/((1+abs(val))*(1+abs(val)))
	Val/(1+abs(Val)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Error Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%DiversityList: [[GenN,GenN-1...Gen1], [GenN,GenN-1...Gen1]]
%LL = [[Gen1...GenN]...[Gen1...GenN]]
avg_diversity(DiversityList)->
	LL = reorder(DiversityList,[]),
	avg_diversity(LL,[],[],[]).

	
	reorder([List|DL],Acc)->
		reorder(DL,[lists:reverse(List)|Acc]);
	reorder([],Acc)->
		lists:reverse(Acc).

	avg_diversity([List|Lists],SumAcc,AvgAcc,Acc)->
		case List of
			[Val|ListTail] ->
				avg_diversity(Lists,[Val|SumAcc],AvgAcc,[ListTail|Acc]);
			[] ->
				avg_diversity(Lists,SumAcc,AvgAcc,[List|Acc])
		end;
	avg_diversity([],SumAcc,AvgAcc,Acc)->
		%Avg = lists:sum(SumAcc)/length(SumAcc),
		case lists:sum(lists:flatten(Acc)) == 0 of
			true ->
				List = [lists:reverse(SumAcc)|AvgAcc],
				{List,[avg(L)||L<-List],[std(L)||L<-List]};
			false ->
				avg_diversity(lists:reverse(Acc),[],[lists:reverse(SumAcc)|AvgAcc],[])
		end.

avg(List)->
	lists:sum(List)/length(List).
std(List)->
	Avg = avg(List),
	std(List,Avg,[]).
	
	std([Val|List],Avg,Acc)->
		std(List,Avg,[math:pow(Avg-Val,2)|Acc]);
	std([],_Avg,Acc)->
		Variance = lists:sum(Acc)/length(Acc),
		math:sqrt(Variance).

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
bip(Val)->
	case Val > 0 of
		true -> 1;
		false -> -1
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
	
-record(complex,{r,i}).
complex_add(A,B)->
	#complex{r=A#complex.r+B#complex.r, i=A#complex.i+B#complex.i}.

complex_subtract(A,B)->
	#complex{r=A#complex.r-B#complex.r, i=A#complex.i-B#complex.i}.

complex_multiply(A,B)->
	 R = A#complex.r*B#complex.r - A#complex.i*B#complex.i,
	 I = A#complex.i*B#complex.r + A#complex.r*B#complex.i,
	 #complex{r=R,i=I}.
	 
complex_divide(A,B)->
	Denom = math:pow(B#complex.r,2) +  math:pow(B#complex.i,2),
	R = A#complex.r*B#complex.r + A#complex.i*B#complex.i,
	I = A#complex.i*A#complex.r + A#complex.r*A#complex.i,
	#complex{r=R/Denom,i=I/Denom}.

complex_processing(Input,Weights)->
	Max = 1,
	HP = math:pi()/2,
	Complex_Form = [#complex{r=math:cos(Val*HP/Max),i=math:sin(Val*HP/Max)} || Val <- Input],
	Zipped_Form = lists:zip(Input,Weights),
	Real = [math:cos((I/Max)*HP + (W/Max)*HP) || {I,W} <- Zipped_Form],
	Imaginary = [math:sin((I/Max)*HP + (W/Max)*HP) || {I,W} <- Zipped_Form],
	Magnitude = math:sqrt(math:pow(lists:sum(Real),2) + math:pow(lists:sum(Imaginary),2)),
	io:format(" Input:~p~n Weights:~p~n Max:~p HP:~p~n Complex Input:~p~n Real:~p~n Imaginary:~p~n Magnitude:~p~n",[Input,Weights,Max,HP,Complex_Form,Real, Imaginary, Magnitude]).
	
complex_af1({RealAcc,ImaginaryAcc})->
	RealSqrd = math:pow(sigmoid(RealAcc),2),
	ImaginarySqrd = math:pow(sigmoid(ImaginaryAcc),2),
	2*(math:sqrt(RealSqrd + ImaginarySqrd)-1).
	
complex_af2({RealAcc,ImaginaryAcc})->
	2*(math:pow(sigmoid_01(RealAcc)-sigmoid_01(ImaginaryAcc),2) - 1).

	sigmoid_01(Val)->
		1/(1+math:exp(-Val)).

rbf_gaussian(Centroidal_Distance)->
	math:exp(-Centroidal_Distance).
	
rbf_multiquadric(Centroidal_Distance)->
	math:pow(1+Centroidal_Distance,0.5).
	
rbf_inverse_multiquadric(Centroidal_Distance)->
	math:pow(1+Centroidal_Distance,-0.5).
	
rbf_cauchy(Centroidal_Distance)->
	math:pow(1+Centroidal_Distance,-1).

	centroidal_distance(CV,IV,R)->
		DV=vector_distance(IV,CV,[]),
		lists:sum([Val*Val||Val<-DV])/(R*R).
	
		vector_distance([Val1|V1],[Val2|V2],Acc)->
			vector_distance(V1,V2,[Val1-Val2|Acc]);
		vector_distance([],[],Acc)->
			lists:reverse(Acc).

sigmoid_unipolar(Val)->%(-inf,inf) onto (0,1)
	1/(1+math:exp(-Val)).
	
sigmoid_bipolar(Val)->%(-inf,inf) onto (-1,1)
	(1-math:exp(-Val))/(1+math:exp(-Val)).

conic_section_function(CV,IV,WV)->
	void.
	

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
		
o()->
	spawn(functions,o,[abc_pred10]).
	
o(TableName)->
	Result = ets:file2tab(TableName),
	io:format("Result~p~n",[Result]),
	timer:sleep(100000).

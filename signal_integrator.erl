%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(signal_integrator).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LearningTypes Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dot(DIV,DWP)->
	%io:format("self:~p~n DWP:~p~n DIV:~p~n",[self(),DWP,DIV]),
	dot(DIV,DWP,0).
	
	dot([{From,Input}|DIV],[{From,WPC}|DWP],DotProductAcc)->
		DotProduct = dot_product(Input,WPC,0),
		dot(DIV,DWP,DotProductAcc+DotProduct);
	dot([],[],DotProductAcc)->
		DotProductAcc;
	dot([],[{threshold,[{Threshold,_,_}]}],DotProductAcc)->
		DotProductAcc+Threshold.
	%dot(A,B,C)->
		%io:format("Something went wrong:~p~n ~p~Wn ~p~n",[A,B,C]).
	
		dot_product([Val|Input],[{W,_,_}|WPC],DotAcc)->
			%io:format("Val:~p W:~p, Val*W:~p~n",[Val,W,Val*W]),
			dot_product(Input,WPC,DotAcc+(Val*W));
		dot_product([],[],DotAcc)->
			DotAcc.
			
complex_dot(DIV,DWP)->
	Max=1,
	Min=-1,
	complex_dot(DIV,DWP,0,0,Min,Max).
	
	complex_dot([{From,Input}|DIV],[{From,WPC}|DWP],RealAcc,ImaginaryAcc,Min,Max)->
		{Real,Imaginary} = complex_dot_product(Input,WPC,0,0,Min,Max),
		complex_dot(DIV,DWP,Real+RealAcc,Imaginary+ImaginaryAcc,Min,Max);
	complex_dot([],[],RealAcc,ImaginaryAcc,Min,Max)->
		{RealAcc,ImaginaryAcc};
	complex_dot([],[{threshold,Threshold}],RealAcc,ImaginaryAcc,Min,Max)->
		{Real,Imaginary} = complex_dot_product([1],Threshold,0,0,Min,Max),
		U_RealAcc = Real+RealAcc,
		U_ImaginaryAcc = Imaginary+ImaginaryAcc,
		{U_RealAcc,U_ImaginaryAcc}.
		
		complex_dot_product([I|Input],[{W,_,_}|WPC],RealAcc,ImaginaryAcc,Min,Max)->
			Phi = math:pi()*(I-Min)/(Max-Min),
			Real = math:cos(Phi + (W+math:pi())/(2*math:pi())),%Where W is between 0 and Pi
			Imaginary = math:sin(Phi + W),
			complex_dot_product(Input,WPC,RealAcc+Real,ImaginaryAcc+Imaginary,Min,Max);
		complex_dot_product([],[],RealAcc,ImaginaryAcc,Min,Max)->
			{RealAcc,ImaginaryAcc}.

c_perceptron(Weights,Input,0)->
	Weights;
c_perceptron(Weights,Input,Index)->
	case complex_training(Weights,Input) of
		{U_Weights,0} ->
			io:format("U_Weights:~p~n",[U_Weights]);
		{U_Weights,Error}->
			io:format("Classification error:~p~n",[Error]),
			c_perceptron(U_Weights,Input,Index-1)
	end.
	
	complex_training(Weights,[{Input,Expected}|Inputs])->
		{RealAcc,ImaginaryAcc}=complex_dot(Weights,Input,0,0),
		RealSqrd = math:pow(sigmoid(RealAcc),2),
		ImaginarySqrd = math:pow(sigmoid(ImaginaryAcc),2),
		Output1 = math:sqrt(RealSqrd + ImaginarySqrd),
		Output2 = math:pow(sigmoid(RealAcc)-sigmoid(ImaginaryAcc),2),
		%U_Weights=update_complex_perceptron_weights(Weights,Magnitude,[]),
		U_Weights = Weights,
		io:format("Output1:~p Output2:~p~n",[Output1,Output2]),
		complex_training(U_Weights,Inputs);
	complex_training(Weights,[])->
		{Weights,undefined}.
		
		complex_dot([W|Weights],[I|Input],RAcc,IAcc)->
			Max = 1,
			Min = -1,
			Phi = math:pi()*(I-Min)/(Max-Min),
			Real = math:cos(Phi + W),%Where W is between 0 and Pi
			Imaginary = math:sin(Phi + W),
			complex_dot(Weights,Input,RAcc+Real,IAcc+Real);
		complex_dot([],[],RAcc,IAcc)->
			{RAcc,IAcc}.
			
		update_complex_perceptron_weights(Weights,Magnitude,[])->
			void.
			
		sigmoid(Val)->
			1/(1+math:exp(-Val)).

perceptron(Weights,Input,0)->
	Weights;
perceptron(Weights,Input,Index)->
	case perceptron_training(Weights,Input,[]) of
		{U_Weights,0} ->
			io:format("U_Weights:~p~n",[U_Weights]);
		{U_Weights,Error}->
			io:format("Classification error:~p~n",[Error]),
			perceptron(U_Weights,Input,Index-1)
	end.
	
	perceptron_training([W|Weights],[{Input,Expected}|Input],Acc)->
		void.
		

vector_distance(DIV,DWP)->
	vec_distance(DIV,DWP,0).
	
	vec_distance([{From,Input}|DIV],[{From,WPC}|DWP],Acc)->
		Val=vd(Input,WPC,0),
		vec_distance(DIV,DWP,Acc+Val);
	vec_distance([],[],Acc)->
		math:sqrt(Acc);
	vec_distance([],[{threshold,[{Threshold,_,_}]}],Acc)->
		math:sqrt(Acc)+Threshold.
		
		vd([I|Input],[{W,_,_}|WPC],Acc)->
			vd(Input,WPC,math:pow(I-W,2)+Acc);
		vd([],[],Acc)->
			Acc.

within_hypersphere(DIV,DWP)->
	within_hypersphere(DIV,DWP,0).
	
	within_hypersphere([{From,Input}|DIV],[{From,WPC}|DWP],Acc)->
		Val=vd(Input,WPC,0),
		within_hypersphere(DIV,DWP,Acc+Val);
	within_hypersphere([],[],Acc)->
		case Acc  < 1 of
			true ->
				1;
			false ->
				-1
		end;
	within_hypersphere([],[{threshold,[{Threshold,_,_}]}],Acc)->
		case Acc < Threshold of
			true ->
				1;
			false ->
				-1
		end.

within_hypercube(DIV,DWP)->
	within_hypercube(DIV,DWP,1).
	
	within_hypercube([{From,Input}|DIV],[{From,WPC}|DWP],Acc)->
		Val=hc(Input,WPC,1),
		within_hypercube(DIV,DWP,Acc*Val);
	within_hypercube([],[],Acc)->
		case Acc of
			1 ->
				1;
			0 ->
				-1
		end.
			
	
		hc([I|Input],[{W,Boundary,_}|WPC],Acc)->
			Val=case abs(I-W) < Boundary of
				true ->
					1;
				false ->
					0
			end,
			hc(Input,WPC,Val*Acc);
		hc([],[],Acc)->
			Acc.
			
pun(DIV,DWP)->
	pun(DIV,DWP,1).
	
	pun([{From,Input}|DIV],[{From,WPC}|DWP],Acc)->
		Val=pun1(Input,WPC,1),
		pun(DIV,DWP,Val*Acc);
	pun([],[],Acc)->
		Acc;
	pun([],[{threshold,[{Threshold,_,_}]}],Acc)->
		Acc*Threshold.
		
		pun1([I|Input],[{W,_,_}|WPC],Acc)->
			%io:format("I:~p W:~p~n",[I,W]),
			pun1(Input,WPC,math:pow(abs(I),abs(W))*Acc);
		pun1([],[],Acc)->
			Acc.
				
higher_order(DIV,DWP)->
	higher_order(DIV,DWP,0).
	
	higher_order([{From,Input}|DIV],[{From,WPC}|DWP],Acc)->
		Val=ho(Input,WPC,0),
		higher_order(DIV,DWP,Val+Acc);
	higher_order([],[],Acc)->
		Acc;
	higher_order([],[{threshold,[{Threshold,_,_}]}],Acc)->
		Acc+Threshold.
		
		ho([I|Input],[{W,_,_}|WPC],Acc)->
			ho(Input,WPC,W*math:pow(I,2)+Acc);
		ho([],[],Acc)->
			Acc.

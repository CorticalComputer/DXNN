%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This source code and work is provided and developed by Gene I. Sher & DXNN Research Group WWW.DXNNResearch.COM
%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%

-module(derivatives).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Activation Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tanh(Val)->%%%Derivative
	TanH = math:tanh(Val),
	1-math:pow(TanH,2).
	
cos(Val)->%%%Derivative
	-math:sin(Val).
	
sin(Val)->%%%Derivative
	math:cos(Val).

multiquadric(Val)->%%%Derivative
	-0.5*math:pow(Val*Val+0.01,-1.5)*Val.
	%linear(Val).
	
absolute(Val)->
	case Val > 0 of
		true ->
			1;
		false ->
			-1
	end.
	
linear(_Val)->%%%Derivative
	1.
	
quadratic(Val)->%%%Derivative
	functions:sgn(Val)*Val*2.
	
guassian(Val)->%%%Derivative
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
	math:pow(2.71828183,-V*V)*V*-2.
	
guassian1(Val)->%%%Derivative
	V = case Val > 50 of
		true ->
			50;
		false ->
			case Val < -50 of
				true ->
					-50;
				false ->
					Val
			end
	end,
	math:pow(2.71828183,-0.1*V*V)*-0.2*V.

sigmoid(Val)->%%%Derivative
	Sigmoid = functions:sigmoid(Val),
	Sigmoid*(1-Sigmoid).
	
sigmoid1(Val)->%%%Derivative
	1/((1+abs(Val))*(1+abs(Val))).

test(Val)->
	tanh(Val).

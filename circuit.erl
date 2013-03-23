%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This source code and work is provided and developed by Gene I. Sher & DXNN Research Group WWW.DXNNResearch.COM
%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.
%
%The original release of this source code and the DXNN MK2 system was introduced and explained (architecture and the logic behind it) in my book: Handbook of Neuroevolution Through Erlang. Springer 2012, print ISBN: 978-1-4614-4462-6 ebook ISBN: 978-1-4614-4463-6. 
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%

-module(circuit).
-compile(export_all).
-include("records.hrl").
-define(OUTPUT_SAT_LIMIT,math:pi()).

transfer_function(IAcc,Circuit,AF,_Plasticity)->
	IVector=lists:flatten([Input||{_From,Input}<-IAcc]),
	case AF of
		tanh ->
			calculate_output_std(IVector,Circuit);
		rbf ->
			calculate_output_rbf(IVector,Circuit);
		furier ->
			ok
	end.

calculate_output_std(IVector,[Cur_NeurodeLayer|Circuit])->
	%io:format("{IVector,Cur_NeurodeLayer}:~p~n",[{IVector,Cur_NeurodeLayer}]),
	U_IVector = [calculate_neurode_output_std(IVector,N#neurode.weights,N#neurode.bias,0) || N <- Cur_NeurodeLayer],
%	io:format("U_IVector:~p~n",[U_IVector]),
	calculate_output_std(U_IVector,Circuit);
calculate_output_std([Output],[])->
	%io:format("Output:~p~n",[Output]),
	Output.
			
	calculate_neurode_output_std([I|IVector],[Weight|Weights],Bias,Acc)->
		calculate_neurode_output_std(IVector,Weights,Bias,I*Weight+Acc);
	calculate_neurode_output_std([],[],undefined,Acc)->
		functions:tanh(Acc);
	calculate_neurode_output_std([],[],Bias,Acc)->
		functions:tanh(Acc+Bias).

calculate_output_rbf(IVector,[Output_NeurodeLayer])->
	[Output] = [calculate_rbf_circuit_output(IVector,N#neurode.weights,N#neurode.bias,0) || N <- Output_NeurodeLayer],
	Output;
calculate_output_rbf(IVector,[Cur_NeurodeLayer|Circuit])->
	%io:format("{IVector,Cur_NeurodeLayer}:~p~n",[{IVector,Cur_NeurodeLayer}]),
	U_IVector = [calculate_neurode_output_rbf(IVector,N#neurode.weights,N#neurode.bias,0) || N <- Cur_NeurodeLayer],
%	io:format("U_IVector:~p~n",[U_IVector]),
	calculate_output_rbf(U_IVector,Circuit).

	calculate_neurode_output_rbf([I|IVector],[Weight|Weights],Bias,Acc)->
		calculate_neurode_output_rbf(IVector,Weights,Bias,math:pow(I-Weight,2)+Acc);
	calculate_neurode_output_rbf([],[],undefined,Acc)->
		math:exp(-Acc);
	calculate_neurode_output_rbf([],[],Bias,Acc)->
		math:exp(-Acc/math:pow(Bias,2)).

	calculate_rbf_circuit_output([I|IVector],[Weight|Weights],Bias,Acc)->
		calculate_rbf_circuit_output(IVector,Weights,Bias,I*Weight+Acc);
	calculate_rbf_circuit_output([],[],undefined,Acc)->
		Acc;
	calculate_rbf_circuit_output([],[],Bias,Acc)->
		Acc+Bias.

plasticity_function(DIV,Output,Circuit)->
	ok.
	
perturb_circuit(Circuit,DMultiplier)->
	MutationOperators = [{perturb_weights,95},{add_neurode,0},{add_layer,0},{add_bias,2},{remove_bias,3}],
	Tot=lists:sum([RelativeProbability||{_,RelativeProbability}<-MutationOperators]),
	Mutagen=get_RandomMutagen(0,MutationOperators,random:uniform(Tot)),
	%io:format("Applying Mutagen:~p to circuit:~p in neuron:~p~n",[Mutagen,Circuit,self()]),
	U_Circuit=circuit:Mutagen(Circuit,DMultiplier),
	%io:format("U_Circuit:~p in neuron:~p~n",[U_Circuit,self()]),
	U_Circuit.
				
	get_RandomMutagen(Range_From,[{Mutagen,Prob}|Mutagens],Choice)->
		Range_To = Range_From+Prob,
		case (Choice >= Range_From) and (Choice =< Range_To) of
			true ->
				Mutagen;
			false ->
				get_RandomMutagen(Range_To,Mutagens,Choice)
		end;
	get_RandomMutagen(_Rage_From,[],_Choice)->
		exit("********ERROR:get_RandomMutagen:: in get_RandomMutagen(Mutagens,Choice), Mutagens reached []").

	perturb_weights(Circuit,DMultiplier)->
		TotWeights=lists:sum(lists:flatten([[length(N#neurode.weights)||N<-NeurodeLayer] || NeurodeLayer<-Circuit])),
		MP = 1/math:sqrt(TotWeights),
		[perturb_Neurodes(NeurodeLayer,DMultiplier,MP,[])||NeurodeLayer<-Circuit].

		perturb_Neurodes(NeurodeLayer,DMultiplier,MP,Acc)->
			[N#neurode{weights=perturb_Weights(N#neurode.weights,MP,DMultiplier,[]),parameters=perturb_Weights(N#neurode.parameters,MP,DMultiplier,[]),bias=perturb_Weight(N#neurode.bias,MP,DMultiplier)}|| N <- NeurodeLayer].

			perturb_Weights([W|Weights],MP,DMultiplier,Acc)->
				WLimit = ?OUTPUT_SAT_LIMIT,
				%io:format("Perturb_W:~p~n",[{W,MP,DMultiplier}]),
				U_W=case random:uniform() < MP of
					true ->
						DW = (random:uniform()-0.5)*DMultiplier,
						%io:format("DW:~p~n",[DW]),
						functions:sat(W + DW,WLimit,-WLimit);
					false ->
						W
				end,
				perturb_Weights(Weights,MP,DMultiplier,[U_W|Acc]);
			perturb_Weights([],_MP,_DMultiplier,Acc)->
				lists:reverse(Acc).
			
			perturb_Weight(undefined,_MP,_DMultiplier)->
				undefined;
			perturb_Weight(W,MP,DMultiplier)->
				WLimit = ?OUTPUT_SAT_LIMIT,
				case random:uniform() < MP of
					true ->
						DW = (random:uniform()-0.5)*DMultiplier,
						functions:sat(W + DW,WLimit,-WLimit);
					false ->
						W
				end.
				
	add_neurode(Circuit,_DMultiplier)->
		TotLayers = length(Circuit)-1,
		LayerIndex = random:uniform(TotLayers),
		VL = case LayerIndex == 1 of
			true ->
				[[N|_]|_]=Circuit,
				length(N#neurode.weights);
			false ->
				undefined
		end,
		circuit:add_neurode(Circuit,LayerIndex,VL).
		
	add_layer(Circuit,_DMultiplier)->
		TotLayers = length(Circuit),
		LayerIndex = random:uniform(TotLayers),
		VL = case LayerIndex == TotLayers of
			true ->
				random:uniform(1);
			false ->
				random:uniform(3)
		end,
		circuit:add_layer(Circuit,LayerIndex,VL).
		
		
		
		delete_weights(TargetIndex,TotWeights,Weights)->delete_weights(1,TargetIndex,TotWeights,Weights,[]).
		delete_weights(_TargetIndex,_TargetIndex,0,Weights,Acc)->
			lists:reverse(Acc)++Weights;
		delete_weights(TargetIndex,TargetIndex,WeightIndex,[_W|Weights],Acc)->
			delete_weights(TargetIndex,TargetIndex,WeightIndex-1,Weights,Acc);
		delete_weights(Index,TargetIndex,WeightIndex,[W|Weights],Acc)->
			delete_weights(Index+1,TargetIndex,WeightIndex,Weights,[W|Acc]).
		
		add_weights(TargetIndex,TotWeights,Weights)->add_weights(1,TargetIndex,TotWeights,Weights,[]).
		add_weights(_TargetIndex,_TargetIndex,0,Weights,Acc)->
			lists:reverse(Acc)++Weights;
		add_weights(TargetIndex,TargetIndex,WeightIndex,Weights,Acc)->
			add_weights(TargetIndex,TargetIndex,WeightIndex-1,Weights,[random:uniform()-0.5|Acc]);
		add_weights(Index,TargetIndex,WeightIndex,[W|Weights],Acc)->
			add_weights(Index+1,TargetIndex,WeightIndex,Weights,[W|Acc]).	
		
		add_bias(Circuit,_DMultiplier)->
				[[N#neurode{bias=add_bias(N#neurode.bias)}|| N <- NeurodeLayer]||NeurodeLayer<-Circuit].
		add_bias(Val)->
			case Val of
				undefined ->
					case random:uniform() < 0.5 of
						true ->
							random:uniform()-0.5;
						false ->
							Val
					end;
				_ ->
					Val
			end.
			
		remove_bias(Circuit,_DMultiplier)->
				[[N#neurode{bias=remove_bias(N#neurode.bias)}|| N <- NeurodeLayer]||NeurodeLayer<-Circuit].
		remove_bias(Val)->
			case Val of
				undefined ->
					case random:uniform() < 0.5 of
						true ->
							undefined;
						false ->
							Val
					end;
				_ ->
					Val
			end.
		
		add_neurode(Circuit,LayerIndex,VL)->
			add_neurode(Circuit,LayerIndex,VL,[]).
		add_neurode([NeurodeLayer|Circuit],1,VL,Acc)->
			case Circuit of
				[] ->
					U_NeurodeLayer=case Acc of
						[] ->
							[#neurode{id=technome_constructor:generate_UniqueId(),weights=add_weights(1,1,VL,[],[])}|NeurodeLayer];
						[L|_] ->
							[#neurode{id=technome_constructor:generate_UniqueId(),weights=add_weights(1,1,length(L),[],[])}|NeurodeLayer]
					end,
					lists:reverse(Acc)++[U_NeurodeLayer];
				[TrailingLayer|CircuitRemainder] ->
					U_NeurodeLayer=case Acc of
						[] ->
							[#neurode{id=technome_constructor:generate_UniqueId(),weights=add_weights(1,1,VL,[],[])}|NeurodeLayer];
						[L|_] ->
							
							[#neurode{id=technome_constructor:generate_UniqueId(),weights=add_weights(1,1,length(L),[],[])}|NeurodeLayer]
					end,
					U_TrailingLayer=[N#neurode{weights=add_weights(1,1,1,N#neurode.weights,[])}||N<-TrailingLayer],
					lists:reverse(Acc)++[U_NeurodeLayer]++[U_TrailingLayer]++CircuitRemainder
			end;
		add_neurode([NeurodeLayer|Circuit],LayerIndex,VL,Acc)->
			add_neurode(Circuit,LayerIndex-1,VL,[NeurodeLayer|Acc]).
			
		delete_neurode(Circuit,LayerIndex,VL)->
			delete_neurode(Circuit,LayerIndex,VL,[]).
		delete_neurode([NeurodeLayer|Circuit],1,VL,Acc)->
			case Circuit of
				[] ->
					[_|U_NeurodeLayer] = NeurodeLayer,
					case U_NeurodeLayer of
						[] ->
							lists:reverse(Acc)++[NeurodeLayer];
						_ ->
							lists:reverse(Acc)++[U_NeurodeLayer]
					end;
				[TrailingLayer|CircuitRemainder] ->
					[_|U_NeurodeLayer] = NeurodeLayer,
					case U_NeurodeLayer of
						[] ->
							lists:reverse(Acc)++[NeurodeLayer]++[TrailingLayer]++CircuitRemainder;
						_ ->
							U_TrailingLayer=[N#neurode{weights=delete_weights(1,1,1,N#neurode.weights,[])}||N<-TrailingLayer],
							lists:reverse(Acc)++[U_NeurodeLayer]++[U_TrailingLayer]++CircuitRemainder
					end
			end;
		delete_neurode([NeurodeLayer|Circuit],LayerIndex,VL,Acc)->
			delete_neurode(Circuit,LayerIndex-1,VL,[NeurodeLayer|Acc]).
			
		add_layer(Circuit,LayerIndex,LayerSize)->%As long as it's not last, and if it is last, make sure the length of the layer is 1, for now no multi element vector based output.
			case (LayerIndex < length(Circuit)) or ((LayerIndex == length(Circuit)) and (LayerSize == 1)) of
				true ->
					add_layer(Circuit,LayerIndex,LayerSize,[]);
				false ->
					exit("add_layer(Circuit,LayerIndex,LayerSize)::~p ~p ~p~n",[Circuit,LayerIndex,LayerSize])
			end.
		add_layer([Layer|Circuit],1,LayerSize,Acc)->
			TotWeights=length(Layer),
			NewLayer=[#neurode{id=technome_constructor:generate_UniqueId(),weights=add_weights(1,1,TotWeights,[],[])}||_<-lists:seq(1,LayerSize)],
			case Circuit of
				[FollowingLayer|RemainingLayer]->
					Diff = LayerSize-TotWeights,
					case Diff < 0 of
						true ->
							U_FollowingLayer=[N#neurode{weights=delete_weights(1,abs(Diff),N#neurode.weights)}||N<-FollowingLayer],
							case RemainingLayer of
								[] ->
									lists:reverse([Layer|Acc])++[NewLayer]++[U_FollowingLayer];
								_->
									lists:reverse([Layer|Acc])++[NewLayer]++[U_FollowingLayer]++RemainingLayer
							end;
						false ->
							U_FollowingLayer=[N#neurode{weights=add_weights(1,Diff,N#neurode.weights)}||N<-FollowingLayer],
							case RemainingLayer of
								[] ->
									lists:reverse([Layer|Acc])++[NewLayer]++[U_FollowingLayer];
								_ ->
									lists:reverse([Layer|Acc])++[NewLayer]++[U_FollowingLayer]++RemainingLayer
							end
					end;
				[] ->
					lists:reverse([Layer|Acc])++[NewLayer]
			end;			
		add_layer([Layer|Circuit],LayerIndex,LayerSize,Acc)->
			add_layer(Circuit,LayerIndex-1,LayerSize,[Layer|Acc]).
			
		test_DeleteWeights(TargetIndex,TargetVL)->
			Circuit = [
				[#neurode{id=a,weights=[1,2,3,4,5]},#neurode{id=b,weights=[2,3,4,5,6]},#neurode{id=c,weights=[3,4,5,6,7]}],
				[#neurode{id=d,weights=[1,2,3]},#neurode{id=e,weights=[2,3,4]},#neurode{id=f,weights=[3,4,5]}]
			],
			[NeurodeLayer|RemainderLayers] = Circuit,
			U_NeurodeLayer=[N#neurode{weights=delete_weights(1,TargetIndex,TargetVL,N#neurode.weights,[])}||N<-NeurodeLayer],
			U_Circuit=[U_NeurodeLayer|RemainderLayers],
			io:format("Circuit:~n~p~n",[Circuit]),
			io:format("U_Circuit:~n~p~n",[U_Circuit]).
			
		test_AddWeights(TargetIndex,TargetVL)->
			Circuit = [
				[#neurode{id=a,weights=[1,2,3,4,5]},#neurode{id=b,weights=[2,3,4,5,6]},#neurode{id=c,weights=[3,4,5,6,7]}],
				[#neurode{id=d,weights=[1,2,3]},#neurode{id=e,weights=[2,3,4]},#neurode{id=f,weights=[3,4,5]}]
			],
			[NeurodeLayer|RemainderLayers] = Circuit,
			U_NeurodeLayer=[N#neurode{weights=add_weights(1,TargetIndex,TargetVL,N#neurode.weights,[])}||N<-NeurodeLayer],
			U_Circuit=[U_NeurodeLayer|RemainderLayers],
			io:format("Circuit:~n~p~n",[Circuit]),
			io:format("U_Circuit:~n~p~n",[U_Circuit]).
			
		test_AddNeurode(LayerIndex,VL)->
			Circuit = [
				[#neurode{id=a,weights=[1,2,3,4,5]},#neurode{id=b,weights=[2,3,4,5,6]},#neurode{id=c,weights=[3,4,5,6,7]}],
				[#neurode{id=d,weights=[1,2,3]},#neurode{id=e,weights=[2,3,4]},#neurode{id=f,weights=[3,4,5]}],
				[#neurode{id=g,weights=[2,2,2]},#neurode{id=h,weights=[3,3,3]}]
			],
			add_neurode(Circuit,LayerIndex,VL).
	
		test_DeleteNeurode(LayerIndex,VL)->
			Circuit = [
				[#neurode{id=a,weights=[1,2,3,4,5]},#neurode{id=b,weights=[2,3,4,5,6]},#neurode{id=c,weights=[3,4,5,6,7]}],
				[#neurode{id=d,weights=[1,2,3]},#neurode{id=e,weights=[2,3,4]},#neurode{id=f,weights=[3,4,5]}],
				[#neurode{id=g,weights=[2,2,2]},#neurode{id=h,weights=[3,3,3]}]
			],
			U_Circuit=delete_neurode(Circuit,LayerIndex,VL),
			io:format("Circuit:~n~p~n",[Circuit]),
			io:format("U_Circuit:~n~p~n",[U_Circuit]).
		
		test_AddLayer(LayerIndex,LayerSize)->
			Circuit = [
				[#neurode{id=a,weights=[1,2,3,4,5]},#neurode{id=b,weights=[2,3,4,5,6]},#neurode{id=c,weights=[3,4,5,6,7]}],
				[#neurode{id=d,weights=[1,2,3]},#neurode{id=e,weights=[2,3,4]},#neurode{id=f,weights=[3,4,5]}],
				[#neurode{id=g,weights=[2,2,2]},#neurode{id=h,weights=[3,3,3]}]
			],
			U_Circuit=add_layer(Circuit,LayerIndex,LayerSize),
			io:format("Circuit:~n~p~n",[Circuit]),
			io:format("U_Circuit:~n~p~n",[U_Circuit]).
			
		test_Mutation(Mutagen,P)->
			Circuit = [
				[#neurode{id=a,weights=[1,2,3,4,5]},#neurode{id=b,weights=[2,3,4,5,6]},#neurode{id=c,weights=[3,4,5,6,7]}],
				[#neurode{id=d,weights=[1,2,3]},#neurode{id=e,weights=[2,3,4]},#neurode{id=f,weights=[3,4,5]}],
				[#neurode{id=g,weights=[2,2,2]},#neurode{id=h,weights=[3,3,3]}]
			],
			circuit:Mutagen(Circuit,P).
			
		test_perturb_circuit()->
			Circuit = [
				[#neurode{id=a,weights=[1,2,3,4,5]},#neurode{id=b,weights=[2,3,4,5,6]},#neurode{id=c,weights=[3,4,5,6,7]}],
				[#neurode{id=d,weights=[1,2,3]},#neurode{id=e,weights=[2,3,4]},#neurode{id=f,weights=[3,4,5]}],
				[#neurode{id=g,weights=[2,2,2]},#neurode{id=h,weights=[3,3,3]}]
			],
			perturb_circuit(Circuit,4).
			
		test_std_output()->
			Circuit = [
				[#neurode{id=a,weights=[1,2,3,4,5]},#neurode{id=b,weights=[2,3,4,5,6]},#neurode{id=c,weights=[3,4,5,6,7]}],
				[#neurode{id=d,weights=[1,2,3]},#neurode{id=e,weights=[2,3,4]},#neurode{id=f,weights=[3,4,5]}],
				[#neurode{id=g,weights=[2,2,2]},#neurode{id=h,weights=[3,3,3]}]
			].
			
		test_rbf_output()->
			Circuit = [
				[#neurode{id=a,weights=[1,2,3,4,5]},#neurode{id=b,weights=[2,3,4,5,6]},#neurode{id=c,weights=[3,4,5,6,7]}],
				[#neurode{id=d,weights=[1,2,3]},#neurode{id=e,weights=[2,3,4]},#neurode{id=f,weights=[3,4,5]}],
				[#neurode{id=g,weights=[2,2,2]},#neurode{id=h,weights=[3,3,3]}]
			].

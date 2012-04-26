%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(neuron).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Neuron Parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-define(DELTA_MULTIPLIER,math:pi()).
-define(SAT_LIMIT,math:pi()).
-define(RO_SIGNAL,0).%(random:uniform()-0.5)*2).
%-record(state, {exoself,id,su_id,i,im,i_acc,o,om,lt,dwp,dwpm,ro}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen(ExoSelf,Node) ->
	PId = spawn(Node,neuron,prep,[ExoSelf]),
	{ok,PId}.
		
prep(ExoSelf)->
	{A,B,C} = now(),
	random:seed(A,B,C),
	put(sa,0),%TODO:Lets see if this works.
	receive
		{ExoSelf,init,InitState}->
			%State = {self(),N_Id,SU_Id,TotIVL,I_PIds,TotOVL,O_PIds,LearningType,RO_PIds,NDWP}
			%io:format("Neuron:Prep:: InitState:~p~n",[InitState]),
			{ExoSelf,Neuron_Id,SU_Id,_TotIVL,I,_TotOVL,O,LT,RO,DWP} = InitState,
			fanout(RO,{self(),forward,[?RO_SIGNAL]}),%lists:duplicate(TotOVL,0)}),
			neuron:neuron(ExoSelf,Neuron_Id,SU_Id,{I,I},[],{O,O},LT,{DWP,DWP},RO)
	end.

neuron(ExoSelf,Neuron_Id,SU_Id,{[{IPid,_IVL}|I],IM},IAcc,{[OPid|O],OM},LT,{DWP,DWPM},RO)->
	receive
		{IPid,forward,Input}->
			%io:format("IPid:~p Input:~p~n",[IPid,Input]),
			neuron:neuron(ExoSelf,Neuron_Id,SU_Id,{I,IM},[{IPid,Input}|IAcc],{[OPid|O],OM},LT,{DWP,DWPM},RO);
		{_From,gt,weight_mutate,DMultiplier}->
			{Adapter,_AF} = LT,
			Updated_DWP = case is_list(DWP) of %use type for this, standard vs bst
				true ->
					MutationP = 1/math:sqrt(length(DWP)),%TODO: Find a better way to find the length of DWP, perhaps calculate it once and store.
					mutate_DWP(DWP,MutationP,DMultiplier,Adapter,[]);
				false ->
					{Weight,Bias} = DWP,
					WLimit = ?SAT_LIMIT,
					case random:uniform(2) of
						1 ->
							Mutated_Weight = functions:sat(Weight + (random:uniform()-0.5)*DMultiplier,WLimit,-WLimit),
							{Mutated_Weight,Bias};
						2 ->
							Mutated_Weight = functions:sat(Weight + (random:uniform()-0.5)*DMultiplier,WLimit,-WLimit),
							Mutated_Bias = functions:sat(Bias + (random:uniform()-0.5)*DMultiplier,WLimit,-WLimit),
							{Mutated_Weight,Mutated_Bias}
					end
			end,
%			io:format("Mutating DWP:~p to Updated_DW:~p~n",[DWP,Updated_DWP]),
			neuron:neuron(ExoSelf,Neuron_Id,SU_Id,{[{IPid,_IVL}|I],IM},IAcc,{[OPid|O],OM},LT,{Updated_DWP,DWPM},RO);
		{_From,gt,weight_save}->
			WS_DWP = case LT of
				{modulated,_} ->%TODO, what's the point of keeping old W and changing W2 and W3? Might as well have full lamarkian.
					reload_DWP(DWP,DWPM,[]);
				_ ->
					DWP
			end,
			neuron:neuron(ExoSelf,Neuron_Id,SU_Id,{[{IPid,_IVL}|I],IM},IAcc,{[OPid|O],OM},LT,{WS_DWP,WS_DWP},RO);
		{_From,gt,weight_revert}->
			%io:format("Curr_State:~p, Prev_State:~p~n",[LT,Reverted_LT]),
			%io:format("Reverting to DWPM:~p~n",[DWPM]),
			neuron:neuron(ExoSelf,Neuron_Id,SU_Id,{[{IPid,_IVL}|I],IM},IAcc,{[OPid|O],OM},LT,{DWPM,DWPM},RO);
		{ExoSelf,memory_reset}->
			neuron:flush_buffer(Neuron_Id),
			ExoSelf ! {self(),ready},
			receive 
				{ExoSelf, reset}->
					fanout(RO,{self(),forward,[?RO_SIGNAL]})
			end,
%			io:format("N_Id:~p reseting~n",[Neuron_Id]),
			neuron:neuron(ExoSelf,Neuron_Id,SU_Id,{IM,IM},[],{OM,OM},LT,{DWP,DWPM},RO);
		{ExoSelf,get_backup}->
			ExoSelf ! {self(),backup,{IM,OM,RO,LT,DWPM}},
			neuron:neuron(ExoSelf,Neuron_Id,SU_Id,{[{IPid,_IVL}|I],IM},IAcc,{[OPid|O],OM},LT,{DWP,DWPM},RO);
		{ExoSelf,terminate}->
%			io:format("terminate:~p DWP:~p~n",[Neuron_Id,DWP]),
			done
		%after 10000 ->
			%io:format("NeuronStuck:~p~n",[{ExoSelf,Neuron_Id,{[{IPid,_IVL}|I],IM},IAcc,{[OPid|O],OM},LT,EF,DWP,RO}])
	end;
neuron(ExoSelf,Neuron_Id,SU_Id,{[],IM},IAcc,{O,OM},LT,{DWP,DWPM},RO)->
	DIV = lists:reverse(IAcc),	
	Updated_DWP = feedforward(LT,DIV,DWP,O),
	neuron:neuron(ExoSelf,Neuron_Id,SU_Id,{IM,IM},[],{O,OM},LT,{Updated_DWP,DWPM},RO).

%%==================================================================== Internal Functions
fanout([Pid|Pids],Msg)->
	Pid ! Msg,
	fanout(Pids,Msg);
fanout([],_Msg)->
	true.

flush_buffer(Neuron_Id)->
	receive 
		ANY -> %io:format("ANY:~p~n",[{ANY,self(),Neuron_Id}]),
		flush_buffer(Neuron_Id)
	after 0 ->
		done
end.

mutate_DWP([{Key,WPC}|DWP],MutationP,DMultiplier,Adapter,Acc)->
	U_WPC = case random:uniform() < MutationP of
		true ->
			MP = 1/math:sqrt(length(WPC)),
			mutate_WPC(WPC,MP,DMultiplier,Adapter,[]);
		false ->
			WPC
	end,
	mutate_DWP(DWP,MutationP,DMultiplier,Adapter,[{Key,U_WPC}|Acc]);
mutate_DWP([],_MutationP,_DMultiplier,_Adapter,Acc)->
	lists:reverse(Acc).

	mutate_WPC([{W,W1,W2}|WPC],MutationP,DMultiplier,Adapter,Acc)->
		{M_W,M_W1,M_W2} = case Adapter of
			modulated ->
				case random:uniform() < 0.5 of
					true ->
						{W,mutate_W(W1,MutationP,DMultiplier),W2};
					false ->
						{W,W1,mutate_W(W2,MutationP,DMultiplier)}
				end;
			_ ->
				{mutate_W(W,MutationP,DMultiplier),W1,W2}
		end,
		mutate_WPC(WPC,MutationP,DMultiplier,Adapter,[{M_W,M_W1,M_W2}|Acc]);
	mutate_WPC([],_MutationP,_DMultiplier,_Adapter,Acc)->
		lists:reverse(Acc).
		
		mutate_W(W,MutationP,DMultiplier)->
			%DMultiplier = ?DELTA_MULTIPLIER,
			WLimit = ?SAT_LIMIT,
			case random:uniform() < MutationP of
				true ->
					functions:sat(W + (random:uniform()-0.5)*DMultiplier,WLimit,-WLimit);
				false ->
					W
			end.
		
feedforward(LT,DIV,DWP,O)->
	%Normalizer = calculate_standardizer(DIV,0),
	%Input_Normalizer = calculate_normalizer(DIV,0),
	%Normalized_DIV = normalize_DIV(Input_Normalizer,DIV,[]),
	%{Adapter,ActivationFunction} = LT,
	{Adapter,AF} = LT,
	Output = learning_types:calculate_Output(DWP,DIV,AF),
	fanout(O,{self(),forward,[Output]}),
	Updated_DWP = learning_types:learn(Adapter,DWP,DIV,Output),
	Updated_DWP.
	
	normalize_DIV(Normalizer,[{Pid,Input}|DIV],NDIVAcc)->
		normalize_DIV(Normalizer,DIV,[{Pid,[I/Normalizer||I<-Input]}|NDIVAcc]);
	normalize_DIV(_,[],NDIVAcc)->
		lists:reverse(NDIVAcc).
	
	
	calculate_standardizer([{_,InputCluster}|DIV],StandardizerAcc)->
		IC_Standardizer = calculate_ICS(InputCluster,0),
		calculate_standardizer(DIV,IC_Standardizer+StandardizerAcc); %%%Needs to use absolute val.
	calculate_standardizer([],StandardizerAcc)->
		case StandardizerAcc of
			0.0 -> 
				1;
			0 ->
				1;
			_ -> 
				StandardizerAcc
		end.
		
		calculate_ICS([Val|InputCluster],ICSAcc)->
			calculate_ICS(InputCluster,abs(Val)+ICSAcc);
		calculate_ICS([],ICSAcc)->
			ICSAcc.
		
	calculate_normalizer([{_,InputCluster}|DIV],NormalizerAcc)->
		IC_Normalizer = calculate_ICN(InputCluster,0),
		calculate_normalizer(DIV,IC_Normalizer+NormalizerAcc);
	calculate_normalizer([],NormalizerAcc)->
		case math:sqrt(NormalizerAcc) of
			0.0 -> 
				1;
			Normalizer -> 
				Normalizer		
					
		end.
		
		calculate_ICN([Val|InputCluster],ICNAcc)->
			%io:format("ICN:~p~n",[{Val,InputCluster,ICNAcc}]),
			calculate_ICN(InputCluster,Val*Val+ICNAcc);
		calculate_ICN([],ICNAcc)->
			ICNAcc.
			
reload_DWP([{Id,WPC}|DWP],[{Id,WPCM}|DWPM],Acc)->
	R_WPC = reload_WPC(WPC,WPCM,[]),
	reload_DWP(DWP,DWPM,[{Id,R_WPC}|Acc]);
reload_DWP([],[],Acc)->
	lists:reverse(Acc).
	
	reload_WPC([{W,W1,W2}|WPC],[{W_M,W1_M,W2_M}|WPCM],Acc)->
		reload_WPC(WPC,WPCM,[{W_M,W1,W2}|Acc]);
	reload_WPC([],[],Acc)->
		lists:reverse(Acc).

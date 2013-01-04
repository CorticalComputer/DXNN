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
-record(state, {
	exoself,
	id,
	su_id,
	heredity_type,
	i,
	ivl,
	o,
	ovl,
	ro,
	si_dwp_bl,
	si_dwp_current,
	si_dwp_backup,
	mi_dwp_bl,
	mi_dwp_current,
	mi_dwp_backup,
	parameters,
	preprocessor,
	signal_integrator,
	activation_function,
	postprocessor,
	plasticity,
	mlffnn_module,
	neural_type
}).
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
			{ExoSelf,Neuron_Id,SU_Id,IVL,I,OVL,O,RO,DWP,Parameters,PreProcessors,SignalIntegrator,ActivationFunction,PostProcessor,Plasticity,MLFFNN_Module,Neural_Type,Heredity_Type} = InitState,
			fanout(RO,{self(),forward,[?RO_SIGNAL]}),%lists:duplicate(TotOVL,0)}),
			S = #state{
				exoself=ExoSelf,
				id=Neuron_Id,
				su_id=SU_Id,
				i=I,
				ivl=IVL,
				o=O,
				ovl=OVL,
				ro=RO,
				si_dwp_current=DWP,
				si_dwp_bl=DWP,
				si_dwp_backup=DWP,
				parameters=Parameters,
				preprocessor=PreProcessors,
				signal_integrator=SignalIntegrator,
				activation_function=ActivationFunction,
				postprocessor=PostProcessor,
				plasticity=Plasticity,
				mlffnn_module=MLFFNN_Module,
				neural_type = Neural_Type,
				heredity_type = Heredity_Type
			},
			%io:format("S:~p~n",[S]),
			neuron:neuron(S,I,[])
	end.

neuron(S,[{IPid,_IVL}|I],IAcc)->
	receive
%		Msg ->
%			io:format("Msg:~p~n",[Msg]),
%			neuron(S,I,IAcc);
		{IPid,forward,Input}->
			%io:format("IPid:~p Input:~p~n",[IPid,Input]),
			neuron:neuron(S,I,[{IPid,Input}|IAcc]);
		{_From,gt,weight_mutate,DMultiplier}->
			%io:format("weight_mutate~n"),
			%{Adapter,_AF} = LT,
			DWP = S#state.si_dwp_backup,
			case S#state.neural_type of
				standard ->
					MutationP = 1/math:sqrt(length(DWP)),%TODO: Find a better way to find the length of DWP, perhaps calculate it once and store.
					Perturbed_DWP=perturb_DWP(DWP,MutationP,DMultiplier,[]);
%					io:format("Mutating DWP:~p to Updated_DW:~p~n",[DWP,Updated_DWP]),
				modular ->
					Perturbed_DWP=modular_perturb_(DWP,DMultiplier,[])
			end,	
			U_S=S#state{
				si_dwp_bl=Perturbed_DWP,
				si_dwp_current=Perturbed_DWP
				%mi_pidps_current=Perturbed_MIPIdPs
			},
			neuron:neuron(U_S,[{IPid,_IVL}|I],IAcc);
		
		
%		{ExoSelf_PId,weight_perturb,Spread}->
%			Perturbed_SIPIdPs=perturb_IPIdPs(Spread,S#state.si_pidps_backup),
%			Perturbed_MIPIdPs=perturb_IPIdPs(Spread,S#state.mi_pidps_backup),
%			U_S=S#state{
%				si_pidps_bl=Perturbed_SIPIdPs,
%				si_pidps_current=Perturbed_SIPIdPs,
%				mi_pidps_current=Perturbed_MIPIdPs
%			},
%			loop(U_S,ExoSelf_PId,[SI_PId|SI_PIds],[MI_PId|MI_PIds],SIAcc,MIAcc);		
%		{ExoSelf_PId,weight_backup}->
%			U_S=case S#state.heredity_type of
%				darwinian ->
%					S#state{
%						si_dwp_backup=S#state.si_dwp_bl,
%						mi_dwp_backup=S#state.mi_dwp_current
%					};
%				lamarckian ->
%					S#state{
%						si_dwp_backup=S#state.si_dwp_current,
%						mi_dwp_backup=S#state.mi_dwp_current
%					}
%			end,
%			loop(U_S,ExoSelf_PId,[SI_PId|SI_PIds],[MI_PId|MI_PIds],SIAcc,MIAcc);
%		{ExoSelf_PId,weight_restore}->
%			U_S = S#state{
%				si_pidps_bl=S#state.si_pidps_backup,
%				si_pidps_current=S#state.si_pidps_backup,
%				mi_pidps_current=S#state.mi_pidps_backup
%			},
%			loop(U_S,ExoSelf_PId,[SI_PId|SI_PIds],[MI_PId|MI_PIds],SIAcc,MIAcc);
		
		
		{_From,gt,weight_save}->
			%io:format("weight_save~n"),
			U_S=case S#state.heredity_type of
				darwinian ->
					S#state{
						si_dwp_backup=S#state.si_dwp_bl
						%mi_dwp_backup=S#state.mi_dwp_current
					};
				lamarckian ->
					S#state{
						si_dwp_backup=S#state.si_dwp_current
						%mi_dwp_backup=S#state.mi_dwp_current
					}
			end,
			neuron:neuron(U_S,[{IPid,_IVL}|I],IAcc);
		{_From,gt,weight_revert}->
			%io:format("weight_revert~n"),
			%io:format("Curr_State:~p, Prev_State:~p~n",[LT,Reverted_LT]),
			%io:format("Reverting to DWPM:~p~n",[DWPM]),
			U_S = S#state{
				si_dwp_bl=S#state.si_dwp_backup,
				si_dwp_current=S#state.si_dwp_backup
				%mi_dwp_current=S#state.mi_dwp_backup
			},
			neuron:neuron(U_S,[{IPid,_IVL}|I],IAcc);
		{ExoSelf,memory_reset}->
			%io:format("memory_reset~n"),
			neuron:flush_buffer(),
			ExoSelf ! {self(),ready},
			receive 
				{ExoSelf, reset}->
					fanout(S#state.ro,{self(),forward,[?RO_SIGNAL]})
			end,
%			io:format("N_Id:~p reseting~n",[Neuron_Id]),
			neuron:neuron(S,S#state.i,[]);
		{ExoSelf,get_backup}->
			%io:format("get_backup~n"),
			ExoSelf ! {self(),backup,{S#state.i,S#state.o,S#state.ro,S#state.si_dwp_backup}},
			neuron:neuron(S,[{IPid,_IVL}|I],IAcc);
		{ExoSelf,terminate}->
			%io:format("terminate~n"),
%			io:format("terminate:~p DWP:~p~n",[Neuron_Id,DWP]),
			done
		%after 10000 ->
			%io:format("NeuronStuck:~p~n",[{ExoSelf,Neuron_Id,{[{IPid,_IVL}|I],IM},IAcc,{[OPid|O],OM},LT,EF,DWP,RO}])
	end;
neuron(S,[],IAcc)->
	DIV = lists:reverse(IAcc),	
	WeightsP = S#state.si_dwp_current,
	%io:format("DIV:~p~n WeightsP:~p~n",[DIV,WeightsP]),
	PreProc = S#state.preprocessor,
	SigInt = S#state.signal_integrator,
	AF = S#state.activation_function,
	PostProc = S#state.postprocessor,
	Plasticity = S#state.plasticity,
	case S#state.neural_type of
		standard ->
			Out = postprocessor:PostProc(functions:AF(signal_integrator:SigInt(preprocessor:PreProc(DIV),WeightsP))),
			Output=functions:sat(Out,1,-1),
			fanout(S#state.o,{self(),forward,[Output]});
		modular ->
			Out = neuron:modular(DIV,WeightsP),
			Output=functions:sat(Out,1,-1),
			fanout(S#state.o,{self(),forward,[Output]})			
		%hebbian ->
		%	Out = postprocessor:none(activation_function:AF(signal_integrator:SigInt(preprocessor:PreProc(Input),Weights))),
		%	Output=functions:sat(Out,1,-1),
		%	fanout(O,{self(),forward,[Output]});
		%perceptron ->
		%	void;
		%grossberg ->
		%	void;
		%general ->
		%	Out = postprocessor:PostProc(activation_function:AF(signal_integrator:SigInt(preprocessor:PreProc(Input),Weights))),
		%	Output=functions:sat(Out,1,-1),
		%	fanout(O,{self(),forward,[Output]})
	end,
	U_DWP = plasticity:Plasticity(DIV,Output,WeightsP),
	U_S=S#state{si_dwp_current=U_DWP},
	neuron:neuron(U_S,U_S#state.i,[]).
	
	modular(DIV,Module)->
		%[[{AF,Weights}...{AF,Weights}],[...],[{AF,Weights}]],
		[{AF1,Weights1},{AF2,Weights2},{AF3,Weights3}] = Module,
		Out1 = functions:AF1(signal_integrator:dot(DIV,Weights1)),
		Out2 = functions:AF2(signal_integrator:dot(DIV,Weights1)),
		
	
%%==================================================================== Internal Functions
fanout([Pid|Pids],Msg)->
	Pid ! Msg,
	fanout(Pids,Msg);
fanout([],_Msg)->
	true.

flush_buffer()->
	receive 
		_ANY -> %io:format("ANY:~p self():~p~n",[ANY,self()]),
		flush_buffer()
	after 0 ->
		done
end.

perturb_DWP([{Key,WPC}|DWP],MutationP,DMultiplier,Acc)->
	U_WPC = case random:uniform() < MutationP of
		true ->
			MP = 1/math:sqrt(length(WPC)),
			perturb_WPC(WPC,MP,DMultiplier,[]);
		false ->
			WPC
	end,
	perturb_DWP(DWP,MutationP,DMultiplier,[{Key,U_WPC}|Acc]);
perturb_DWP([],_MutationP,_DMultiplier,Acc)->
	lists:reverse(Acc).

	perturb_WPC([{W,W1,W2}|WPC],MutationP,DMultiplier,Acc)->
		{M_W,M_W1,M_W2} = {perturb_W(W,MutationP,DMultiplier),W1,W2},
		perturb_WPC(WPC,MutationP,DMultiplier,[{M_W,M_W1,M_W2}|Acc]);
	perturb_WPC([],_MutationP,_DMultiplier,Acc)->
		lists:reverse(Acc).
		
		perturb_W(W,MutationP,DMultiplier)->
			%DMultiplier = ?DELTA_MULTIPLIER,
			WLimit = ?SAT_LIMIT,
			case random:uniform() < MutationP of
				true ->
					DW = (random:uniform()-0.5)*DMultiplier,
					%io:format("DW:~p~n",[DW]),
					functions:sat(W + DW,WLimit,-WLimit);
				false ->
					W
			end.

reload_DWP([{Id,WPC}|DWP],[{Id,WPCM}|DWPM],Acc)->
	R_WPC = reload_WPC(WPC,WPCM,[]),
	reload_DWP(DWP,DWPM,[{Id,R_WPC}|Acc]);
reload_DWP([],[],Acc)->
	lists:reverse(Acc).
	
	reload_WPC([{W,W1,W2}|WPC],[{W_M,W1_M,W2_M}|WPCM],Acc)->
		reload_WPC(WPC,WPCM,[{W_M,W1,W2}|Acc]);
	reload_WPC([],[],Acc)->
		lists:reverse(Acc).

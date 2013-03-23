%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(interactive_evolution).
-compile(export_all).
-include("forex_db.hrl").
-include("records.hrl").

select(Specie_Id,RemainingChampionDesignators)->
	%open screen
	[S] = mnesia:read({specie,Specie_Id}),
	Morphology = S#specie.morphology,
	SHOF = S#specie.hall_of_fame,
	io:format("SHOF:~p~n",[SHOF]),
	Champion_Designators = RemainingChampionDesignators++SHOF,
	io:format("Champion_Designators:~p~n",[Champion_Designators]),
	Choices=visualize(Morphology,Champion_Designators),
	generate_new_generation(Choices,Specie_Id).
	
	visualize(Morphology,Champion_Designators)->
		%devide into length(Agents) section
		%based on it, set the steps, and scale the screens
		%print to screen
		HRes=1800,
		VRes=1000,
		case whereis(vis) of
			undefined ->
				PId = spawn(interactive_evolution,vis,[]),
				register(vis,PId);
			PId ->
				void
		end,
		EndIndex = 200,
		StartIndex = 1000,
		TotTicks = 800,
		TableName = 'EURUSD15',
		End_Key = case EndIndex of
			last ->
				ets:last(TableName);
			_ ->
				fx:prev(TableName,ets:last(TableName),prev,EndIndex)
		end,
		Start_Key = fx:prev(TableName,ets:last(TableName),prev,StartIndex),
		TotGraphs=length(Champion_Designators),
		XOffset=0,
		YOffset=0,
		{XScale,YScale,XStep,YMin,FXList} = prepare_FXList(HRes,VRes,Start_Key,TotTicks,TableName),
		io:format("FXList Length:~p~n",[{XScale,YScale,XStep,YMin,length(FXList)}]),
		[io:format("~p~n",[C])||C<-Champion_Designators],
		present_MultiGraphs(HRes,VRes,YOffset,{Champion_Designators,Champion_Designators},1,XScale,YScale,XStep,YMin,FXList,[]).
			
		present_MultiGraphs(HRes,VRes,YOffset,{[C|Champion_Designators],All_Champs},Champ_Index,XScale,YScale,XStep,YMin,FXList,Acc)->
			[A]=mnesia:read({dx,C#champion.id}),
			Choices = A#dx.behavioral_trace,
			XStart=0,
			%io:format("BEFORE MAKING OBJECTS:~p~n",[{C,length(Choices)}]),
			%io:format("{XScale,YScale,XStep,FXList}:~p~n",[{XScale,YScale,XStep,length(FXList)}]),
			io:format("Index:~p Champ:~p~n",[Champ_Index,C]),
			Objects=prepare_FXObjects(Choices,FXList,XScale,YScale,XStart,XStep,YMin,0,YOffset,HRes,VRes,[]),
			%io:format("Here1:~p~n",[Objects]),
			vis ! {self(),draw_plane,Objects},
			receive 
				done -> 
					ok
			end,
			io:format("Choose trader?: ~n"),
			receive 
				yes ->
					present_MultiGraphs(HRes,VRes,YOffset,{Champion_Designators,All_Champs},Champ_Index+1,XScale,YScale,XStep,YMin,FXList,[C|Acc]);
				no ->
					present_MultiGraphs(HRes,VRes,YOffset,{Champion_Designators,All_Champs},Champ_Index+1,XScale,YScale,XStep,YMin,FXList,Acc);
				print_all ->
					present_All(HRes,VRes/length(All_Champs),YOffset,All_Champs,XScale,YScale/length(All_Champs),XStep,YMin,FXList,[]),
					receive ok -> ok end,
					present_MultiGraphs(HRes,VRes,YOffset,{[C|Champion_Designators],All_Champs},Champ_Index,XScale,YScale,XStep,YMin,FXList,Acc);
				complete ->
					Acc
					
			end;
		present_MultiGraphs(HRes,VRes,YOffset,{[],All_Champs},Champ_Index,XScale,YScale,XStep,YMin,FXList,Acc)->
			present_MultiGraphs(HRes,VRes,YOffset,{All_Champs,All_Champs},1,XScale,YScale,XStep,YMin,FXList,Acc).
			
			present_All(HRes,VRes,YOffset,[C|Champion_Designators],XScale,YScale,XStep,YMin,FXList,Acc)->
				[A]=mnesia:read({dx,C#champion.id}),
				Choices = A#dx.behavioral_trace,
				XStart=0,
				io:format("BEFORE MAKING OBJECTS:~p~n",[{C,length(Choices)}]),
				Objects=prepare_FXObjects(Choices,FXList,XScale,YScale,XStart,XStep,YMin,0,YOffset,HRes,VRes,[]),
				%io:format("{XScale,YScale,XStep,FXList}:~p~n",[{XScale,YScale,XStep,length(FXList)}]),
				present_All(HRes,VRes,YOffset+VRes,Champion_Designators,XScale,YScale,XStep,YMin,FXList,lists:append(Objects,Acc));
			present_All(HRes,VRes,YOffset,[],XScale,YScale,XStep,YMin,FXList,Acc)->
				%io:format("HRes,VRes,YOffset,[],XScale,YScale,XStep,YMin,FXList,Acc:~p~n",[{HRes,VRes,YOffset,[],XScale,YScale,XStep,YMin,FXList,Acc}]),
				vis ! {self(),draw_plane,lists:reverse(Acc)},
				receive
					done ->
						io:format("Received Done~n")
				end.
		
	generate_new_generation(Choices,Specie_Id)->
		%Save away the chosen agents, ensuring they will not be touched.
		%Create offspring, making 10 new agents in total
		%NewGen_Ids=lists:flatten([create_offspring(Champ#champion.id,TotOffspring)||Champ<-Choices]),
		Allotments = lists:reverse(lists:sort([{Champ#champion.main_fitness,Champ#champion.id}||Champ<-Choices])),
		TotFitness = lists:sum([Val || {Val,_Id}<-Allotments]),
		Specie_Size_Limit = 10,
		%io:format("{Allotments,Tot}:~p~n",[{Allotments,TotFitness}]),
		NewGen_Ids=population_monitor:choose_Winners(Specie_Id,Allotments,TotFitness,[],[],Specie_Size_Limit),
		%io:format("NewGen_Ids:~p~n",[NewGen_Ids]),
		[S] = mnesia:read({specie,Specie_Id}),
		mnesia:write(S#specie{dx_ids=NewGen_Ids}),
		NewGen_Ids.
		
		create_offspring(_Agent_Id,0,Acc)->
			Acc;
		create_offspring(Agent_Id,TotOffspring,Acc)->
			[A] = mnesia:read({dx,Agent_Id}),
			OffspringAgent_Id = population_monitor:create_MutantDXCopy(Agent_Id),
			U_A = A#dx{offspring_ids=[OffspringAgent_Id|A#dx.offspring_ids]},%true, false, lost, rentered
			mnesia:write(U_A),
			[OffspringA] = mnesia:read({dx,OffspringAgent_Id}),
			U_OffspringA = OffspringA#dx{champion_flag=[false|OffspringA#dx.champion_flag]},
			mnesia:write(U_OffspringA),
			create_offspring(Agent_Id,TotOffspring-1,[OffspringAgent_Id|Acc]).
		
	terminate()->
		case whereis(vis) of
			undefined ->
				io:format("vis not currently spawned.~n");
			PId ->
				PId ! terminate
		end.

v()->
	spawn(interactive_evolution,vis,[]).

test_MultiGraphs(HRes,VRes,YOffset,0,Acc)->
	vis ! {self(),draw_plane,lists:reverse(Acc)},
	receive
		done ->
			io:format("Received Done~n")
	end;
test_MultiGraphs(HRes,VRes,YOffset,GraphIndex,Acc)->
	Choices=[],
	XStart=0,
	StartIndex = 1000,
	EndIndex = 200,
	TotTicks = 800,
	TableName = 'EURUSD15',
	case whereis(vis) of
		undefined ->
			PId = spawn(interactive_evolution,vis,[]),
			register(vis,PId);
		PId ->
			void
	end,
	End_Key = case EndIndex of
		last ->
			ets:last(TableName);
		_ ->
			fx:prev(TableName,ets:last(TableName),prev,EndIndex)
	end,
	Start_Key = fx:prev(TableName,ets:last(TableName),prev,StartIndex),
	{XScale,YScale,XStep,YMin,FXList} = prepare_FXList(HRes,VRes,Start_Key,TotTicks,TableName),
	io:format("Vals:~p~n",[{length(Choices),length(FXList)}]),
	Objects=prepare_FXObjects(Choices,FXList,XScale,YScale,XStart,XStep,YMin,0,YOffset,HRes,VRes,[]),
	io:format("{XScale,YScale,XStep,FXList}:~p~n",[{XScale,YScale,XStep,FXList}]),
	test_MultiGraphs(HRes,VRes,YOffset+VRes,GraphIndex-1,lists:append(Objects,Acc)).

test_l2plane(HRes,VRes,XOffset,YOffset)->
	Choices=[],
	XStart=0,
	StartIndex = 1000,
	EndIndex = 200,
	TotTicks = 800,
	TableName = 'EURUSD15',
	case whereis(vis) of
			undefined ->
				PId = spawn(interactive_evolution,vis,[]),
				register(vis,PId);
			PId ->
				void
		end,
	End_Key = case EndIndex of
		last ->
			ets:last(TableName);
		_ ->
			fx:prev(TableName,ets:last(TableName),prev,EndIndex)
	end,
	Start_Key = fx:prev(TableName,ets:last(TableName),prev,StartIndex),
	{XScale,YScale,XStep,YMin,FXList} = prepare_FXList(HRes,VRes,Start_Key,TotTicks,TableName),
	Objects=prepare_FXObjects(Choices,FXList,XScale,YScale,XStart,XStep,YMin,XOffset,YOffset,HRes,VRes,[]),
	io:format("{XScale,YScale,XStep,FXList}:~p~n",[{XScale,YScale,XStep,FXList,Objects}]),
	vis ! {self(),draw_plane,Objects},
	receive
		done ->
			io:format("Received Done~n")
	end.

	prepare_FXList(HRes,VRes,StartKey,TotTicks,TableName)->
%		EndBL = 200,
%		StartBL = 1000,
%		TableName = 'EURUSD15',
		io:format("StartKey:~p~n",[StartKey]),
		FXList = fx:fx_GetPriceList(TableName,StartKey,TotTicks,[]),
		YMax = lists:max([High||{_Open,_Close,High,_Low}<-FXList]),
		YMin = lists:min([Low||{_Open,_Close,_High,Low}<-FXList]),
		XRange = length(FXList),
		XScale = HRes/XRange,
		XStep = 1,
		YRange = YMax-YMin,
		YScale = VRes/YRange,
		{XScale,YScale,XStep,YMin,FXList}.
			
	prepare_FXObjects([Choice|Choices],[{Open,Close,High,Low}|VList],XScale,YScale,X,XStep,YMin,XOffset,YOffset,HRes,VRes,Acc)->
		{BHigh,BLow} = case Open > Close of
			true ->
				{Open,Close};
			false ->
				{Close,Open}
		end,
		Coords = [{XOffset+X*XScale,YOffset+(BLow-YMin)*YScale},{XOffset+(X+XStep)*XScale,YOffset+(BLow-YMin)*YScale},{XOffset+(X+XStep)*XScale,YOffset+(BHigh-YMin)*YScale},{XOffset+X*XScale,YOffset+(BHigh-YMin)*YScale}],
		Object=case functions:trinary(Choice) of
			1 ->
				{polygon,undefined,green,undefined,Coords,undefined};
			0 ->
				{polygon,undefined,black,undefined,Coords,undefined};
			-1 ->
				{polygon,undefined,red,undefined,Coords,undefined}
		end,
		TC=[{XOffset+X*XScale,YOffset+(BHigh-YMin)*YScale},{XOffset+(X+XStep)*XScale,YOffset+(BHigh-YMin)*YScale},{XOffset+(X+XStep)*XScale,YOffset+(High-YMin)*YScale},{XOffset+X*XScale,YOffset+(High-YMin)*YScale}],
		BC=[{XOffset+X*XScale,YOffset+(BLow-YMin)*YScale},{XOffset+(X+XStep)*XScale,YOffset+(BLow-YMin)*YScale},{XOffset+(X+XStep)*XScale,YOffset+(Low-YMin)*YScale},{XOffset+X*XScale,YOffset+(Low-YMin)*YScale}],
		TObj = {polygon,undefined,gray,undefined,TC,undefined},
		BObj = {polygon,undefined,gray,undefined,BC,undefined},
		prepare_FXObjects(Choices,VList,XScale,YScale,X+XStep,XStep,YMin,XOffset,YOffset,HRes,VRes,[Object,TObj,BObj|Acc]);
	prepare_FXObjects([],[],XScale,YScale,X,XStep,YMin,XOffset,YOffset,HRes,VRes,Acc)->
		%io:format("~p~n",[Acc]),
		lists:reverse(Acc);
	prepare_FXObjects([],[{Open,Close,High,Low}|VList],XScale,YScale,X,XStep,YMin,XOffset,YOffset,HRes,VRes,Acc)->
		{BHigh,BLow} = case Open > Close of
			true ->
				{Open,Close};
			false ->
				{Close,Open}
		end,
		Coords = [{XOffset+X*XScale,YOffset+(BLow-YMin)*YScale},{XOffset+(X+XStep)*XScale,YOffset+(BLow-YMin)*YScale},{XOffset+(X+XStep)*XScale,YOffset+(BHigh-YMin)*YScale},{XOffset+X*XScale,YOffset+(BHigh-YMin)*YScale}],
		Object={polygon,undefined,black,undefined,Coords,undefined},
		prepare_FXObjects([],VList,XScale,YScale,X+XStep,XStep,YMin,XOffset,YOffset,HRes,VRes,[Object|Acc]).

vis()->
	Width = 1800,
	Height = 1000,
	vis(Width,Height).
	
vis(Width,Height)->
	GS = gs:start(),
	Window = gs:create(window,GS,[{title,"Agent Trading Strategy Visor"},{width,Width},{height,Height}]),
	Canvas = gs:create(canvas,Window,[{width,Width},{height,Height}]),
	gs:config(Window,{map,true}),
	vis(Window,Canvas,Width,Height,[]).
	
	vis(Window,Canvas,CWidth,CHeight,Objects)->
		receive 
			{From,draw_plane,PlaneObjects}->
				%PlaneObjects= create_PlaneObjects(Plane,XOffset,YOffset,HRes,VRes,Width,Height),
				remove_objects(Objects),
				Drawn_PlaneObjects = draw_objects(Canvas,PlaneObjects,[]),
				From ! done,
				interactive_evolution:vis(Window,Canvas,CWidth,CHeight,Drawn_PlaneObjects);
			terminate ->
				done
		end.
					
		draw_objects(Canvas,[skip|Objects],Acc)->
			draw_objects(Canvas,Objects,Acc);
		draw_objects(Canvas,[Object|Objects],Acc)->
			{ObjName,_IdPlaceHolder,Color,Pivot,Coords,Parameter} = Object,		
			Id = case ObjName of
				circle ->
					[{Cx,Cy}] = Coords,
					R = Parameter,
					Draw_Coords = [{Cx-R,Cy-R},{Cx+R,Cy+R}],
					gs:create(oval,Canvas,[{coords,Draw_Coords},{fill,Color},{fg,Color}]);
				arrow ->
					gs:create(line,Canvas,[{coords,Coords},{arrow,last},{fg,Color}]);
				polygon ->
					gs:create(polygon,Canvas,[{coords,Coords},{fill,Color},{fg,Color}]);
				_ ->
					gs:create(ObjName,Canvas,[{coords,Coords},{fg,Color}])
			end,
			%io:format("Coords:~p, Id:~p~n",[Coords,Id]),
			U_Object = {ObjName,Id,Color,Pivot,Coords,Parameter},
			draw_objects(Canvas,Objects,[U_Object|Acc]);
		draw_objects(_Canvas,[],Acc)->
			Acc.

		remove_objects(undefined)->
			done;
		remove_objects(Objects)->
			[gs:destroy(Id)||{_Type,Id,_,_,_,_}<-Objects].
	
	print_plane(List,0,MIndex)->
		io:format("~n"),
		print_plane(List,MIndex,MIndex);
	print_plane([Val|List],HIndex,MIndex)->
		io:format("~p",[Val]),
		print_plane(List,HIndex-1,MIndex);
	print_plane([],_MIndex,_MIndex)->
		ok.

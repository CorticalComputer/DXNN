%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(graph).
-include("records.hrl").
-compile(export_all).
-record(state,{window,canvas,update_rate,filter,scape,scape_type}).
-define(WIDTH,1440).
-define(HEIGHT,900).
-record(graph,{x_title,y_title,description,points}).	

start()->
	GraphVisor_PId = spawn(graph,graph,[]),
	register(graph,GraphVisor_PId),
	test().
	
test()->
	[DX] = mnesia:dirty_read({dx,dx_test}),
	[Cx] = mnesia:dirty_read({cortex,DX#dx.cx_id}),
	Pattern = Cx#cortex.pattern,
	N_Ids = DX#dx.n_ids,
	PatternP=organize(N_Ids,[{LI,[]}||{LI,_LL}<-Pattern]),
	%Elements = nn_to_elements(PatternP),
	io:format("PatternP:~p~n",[PatternP]),
	graph ! {new,PatternP}.
	
	organize([N_Id|N_Ids],PatternP)->
		{{LI,UId},neuron} = N_Id,
		{LI,Ids} = lists:keyfind(LI, 1, PatternP),
		U_PatternP=lists:keyreplace(LI, 1, PatternP, {LI,[N_Id|Ids]}),
		organize(N_Ids,U_PatternP);
	organize([],PatternP)->
		PatternP.

gh()->
	graph ! {substrate,cortex:create_substrate2([3],[2,4],[2])}.
		
graph()->
	Width = 1400,
	Height = 900,
	GS = gs:start(),
	Window = gs:create(window,GS,[{title,"Graph Visor"},{width,Width},{height,Height}]),
	Canvas = gs:create(canvas,Window,[{width,Width},{height,Height}]),
	gs:config(Window,{map,true}),
	ITE = ets:new(ite,[set,private]),
	graph(Window,Canvas,Width,Height,ITE).

%{ObjName,undefined,Color,Pivot,Coords,Parameter} = Object,		
%ND = {circle,Id,Color,Pivot,Coords,Radius}.
%-record(circle,{id,color,loc,pivot,r}).
%-record(line,{id,color,loc,pivot,coords}).
%draw_objects(Canvas,[Object|Objects],Acc)
		graph(Window,Canvas,Width,Height,ITE)->
			receive
				{new,PatternP}->
					case ets:member(ITE,objects) of
						true ->
							remove_objects(ets:lookup_element(ITE,objects,2));
						false ->
							done
					end,
					nn_to_elements(Height,Width,ITE,PatternP),
					draw_elements(Canvas,ITE,ets:first(ITE),[]),
					graph:graph(Window,Canvas,Width,Height,ITE);
				{substrate,PropSubstrate}->
					case ets:member(ITE,objects) of
						true ->
							remove_objects(ets:lookup_element(ITE,objects,2));
						false ->
							done
					end,
					Substrate = [[{[-LI|SCoord],O,W}||{[LI|SCoord],O,W}<-SSubstrate]||SSubstrate<-PropSubstrate],
					[I,H,O] = Substrate,
					%io:format("Substrate:~p~n",[PropSubstrate]),
					%case ??? == 1 of
					%	true ->
							PatternP = substrate2patternP(I,H,O),
							neurons_to_elements(ITE,PatternP),
							links_to_elements2(ITE,PatternP),
							draw_elements(Canvas,ITE,ets:first(ITE),[]),
					%	false ->
					%		io:format("Can't visualize, no 3d visualization yet~n")
					%end,
					graph:graph(Window,Canvas,Width,Height,ITE);
				terminate ->
					done
			end.
			
			substrate2patternP(I,H,O)->
				I_PatternP=[{-1,[Coord || {Coord,_O,_W}<-I]}],
				H_PatternP=substrate2patternP(H),
				O_PatternP=[{1,[Coord || {Coord,_O,_W}<-O]}],
				%io:format("PatternP:~p~n",[lists:append([I_PatternP,H_PatternP,O_PatternP])]),
				lists:append([I_PatternP,H_PatternP,O_PatternP]).
				
				substrate2patternP(HSubstrate)->
					[{Coord,_O,_W}|Substrate] = HSubstrate,
					[LI|_SCoord] = Coord,
					substrate2patternP(Substrate,{LI,[Coord]},[],void).
					
					substrate2patternP([{[LI|SCoord],_O,_W}|Substrate],{LI,N_IdAcc},Acc,void)->
						substrate2patternP(Substrate,{LI,[[LI|SCoord]|N_IdAcc]},Acc,void);
					substrate2patternP([{[NLI|SCoord],_O,_W}|Substrate],{LI,N_IdAcc},Acc,void)->
						substrate2patternP(Substrate,{NLI,[[NLI|SCoord]]},[{LI,N_IdAcc}|Acc],void);
					substrate2patternP([],{LI,N_IdAcc},Acc,void)->
						[{LI,N_IdAcc}|Acc].
				
			remove_objects(undefined)->
				done;
			remove_objects(Objects)->
				[gs:destroy(Id)||{_Type,Id,_,_,_,_}<-Objects].
				
			draw_elements(Canvas,ITE,'$end_of_table',Acc)->
				ets:insert(ITE,{objects,Acc});
			draw_elements(Canvas,ITE,Key,Acc)->
				[{N_Id,N_Coord,OL_Coords}] = ets:lookup(ITE,Key),
				{Cx,Cy} = N_Coord,
				N_R = 5,
				N_Color = green,
				L_Color = black,
%				N = {circle,undefined,green,[N_Coord],[N_Coord],10},
				Oval_Id=gs:create(oval,Canvas,[{coords,[{Cx-N_R,Cy-N_R},{Cx+N_R,Cy+N_R}]},{fg,N_Color}]),
				Link_Ids=[gs:create(line,Canvas,[{coords,[N_Coord,ON_Coord]},{arrow,last},{fg,L_Color}]) || ON_Coord <- OL_Coords],
%				Links = [{arrow,undefined,black,pivot,[N_Coord,ON_Coord],parameter}||ON_Coord <- OL_Coords],
				io:format("Objects:~p~n",[{N_Id,N_Coord,OL_Coords}]),
%				U_Elements=draw_objects(Canvas,[N|Links],[]),
				draw_elements(Canvas,ITE,ets:next(ITE,Key),lists:append([Oval_Id|Link_Ids],Acc)).
					
			nn_to_elements(Height,Width,ITE,PatternP)->
				neurons_to_elements(ITE,PatternP),
				links_to_elements(ITE,PatternP).
				
				neurons_to_elements(ITE,[{LI,N_Ids}|PatternP])->
					LL=length(N_Ids),
					nte(ITE,LL+1,1,LI,N_Ids),
					neurons_to_elements(ITE,PatternP);
				neurons_to_elements(ITE,[])->
					done.
					
					nte(ITE,LL,LayerPos,LI,[N_Id|N_Ids])->
						ets:insert(ITE,{N_Id,{LayerPos*40,40+700*(LI+1)/2}}),
						nte(ITE,LL,LayerPos+1,LI,N_Ids);
					nte(ITE,LL,LL,LI,[])->
						done.
						
				links_to_elements(ITE,[{LI,N_Ids}|PatternP])->
					lte(ITE,N_Ids),
					links_to_elements(ITE,PatternP);
				links_to_elements(_ITE,[])->
					done.
					
					lte(ITE,[N_Id|N_Ids])->
						[N] = mnesia:dirty_read({neuron,N_Id}),
						N_Coord=ets:lookup_element(ITE,N_Id,2),
						OL_Coords=[ets:lookup_element(ITE,{Id,neuron},2)||{Id,neuron}<-N#neuron.o],
						ets:insert(ITE,{N_Id,N_Coord,OL_Coords}),
						lte(ITE,N_Ids);
					lte(_ITE,[])->
						done.
						
				links_to_elements2(ITE,[{LI,N_Ids},{NextLI,NextN_Ids}|PatternP])->
					lte(ITE,N_Ids,NextN_Ids),
					links_to_elements2(ITE,[{NextLI,NextN_Ids}|PatternP]);
				links_to_elements2(ITE,[{OutputLI,OutputN_Ids}])->
					lte(ITE,OutputN_Ids,[]),
					done.
					
					lte(ITE,[N_Id|N_Ids],NextN_Ids)->
						N_Coord=ets:lookup_element(ITE,N_Id,2),
						OL_Coords=[ets:lookup_element(ITE,Id,2)||Id<-NextN_Ids],
						ets:insert(ITE,{N_Id,N_Coord,OL_Coords}),
						lte(ITE,N_Ids,NextN_Ids);
					lte(_ITE,[],_NextN_Ids)->
						done.

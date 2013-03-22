%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(geometry).
-include("records.hrl").
-compile(export_all).

%HYPERCUBE:
%Specifications:[I,H,O]
%I,O:[LL1...LLn] Layer length, where LL is on the X axis, and 1-n is on the Y axis, making this a 2d specification.
% [[LL1...LLx],[LL1...LLx]] Depth 2 list, 3d I_Hypercube.
% [{Dimension,[{Coord1...CoordN}]},{Dimension,[{Coord1...CoordN}]}]
%H:[N..Z,Y,X] Where N is the depth, and Z,Y,X specifies the dimensions of a symetric hypercube. Z by Y by X.
%abc,none
cartesian(I_Coord,Coord)->
	lists:append(I_Coord,Coord).
polar(I_Coord,Coord)->
	lists:append(cart2pol(I_Coord),cart2pol(Coord)).
spherical(I_Coord,Coord)->
	lists:append(cart2spher(I_Coord),cart2spher(Coord)).
centripital_distances(I_Coord,Coord)->
	[centripital_distance(I_Coord,0),centripital_distance(Coord,0)].
cartesian_distance(I_Coord,Coord)->
	[calculate_distance(I_Coord,Coord,0)].
cartesian_CoordDiffs(I_Coord,Coord)->%I:[X1,Y1,Z1] [X2,Y2,Z2] O:[X2-X1,Y2-Y1,Z2-Z1]
	cartesian_CoordDiffs1(I_Coord,Coord,[]).
	
	cartesian_CoordDiffs1([FromCoord|FromCoords],[ToCoord|ToCoords],Acc)->
		cartesian_CoordDiffs1(FromCoords,ToCoords,[ToCoord-FromCoord|Acc]);
	cartesian_CoordDiffs1([],[],Acc)->
		lists:reverse(Acc).

cartesian_GaussedCoordDiffs(FromCoords,ToCoords)->%I:[X1,Y1,Z1] [X2,Y2,Z2] O:[gauss(X2-X1),gauss(Y2-Y1),gauss(Z2-Z1)]
	cartesian_GaussedCoordDiffs1(FromCoords,ToCoords,[]).
	
	cartesian_GaussedCoordDiffs1([FromCoord|FromCoords],[ToCoord|ToCoords],Acc)->
		cartesian_GaussedCoordDiffs1(FromCoords,ToCoords,[functions:gaussian(ToCoord-FromCoord)|Acc]);
	cartesian_GaussedCoordDiffs1([],[],Acc)->
		lists:reverse(Acc).
		
%Iterative
cartesian(I_Coord,Coord,[I,O,W])->
	[I,O,W|lists:append(I_Coord,Coord)].
polar(I_Coord,Coord,[I,O,W])->
	[I,O,W|lists:append(cart2pol(I_Coord),cart2pol(Coord))].
spherical(I_Coord,Coord,[I,O,W])->
	[I,O,W|lists:append(cart2spher(I_Coord),cart2spher(Coord))].
centripital_distances(I_Coord,Coord,[I,O,W])->
	[I,O,W,centripital_distance(I_Coord,0),centripital_distance(Coord,0)].
cartesian_distance(I_Coord,Coord,[I,O,W])->
	[I,O,W,calculate_distance(I_Coord,Coord,0)].
cartesian_CoordDiffs(FromCoords,ToCoords,[I,O,W])->
	[I,O,W,cartesian_CoordDiffs(FromCoords,ToCoords)].
cartesian_GaussedCoordDiffs(FromCoords,ToCoords,[I,O,W])->
	[I,O,W,cartesian_GaussedCoordDiffs(FromCoords,ToCoords)].
iow(_I_Coord,_Coord,IOW)->
	IOW.
		
	cart2pol([Y,X])->
		R = math:sqrt(X*X + Y*Y),
		Theta = case R == 0 of
			true ->
				0;
			false ->
				if
					(X>0)	and	(Y>=0)	-> math:atan(Y/X);
					(X>0)	and	(Y<0)	-> math:atan(Y/X) + 2*math:pi();
					(X<0)			-> math:atan(Y/X) + math:pi();
					(X==0)	and	(Y>0)	-> math:pi()/2;
					(X==0)	and	(Y<0)	-> 3*math:pi()/2
				end
		end,
		[R,Theta].
		
	cart2spher([Z,Y,X])->
		%Pi = math:pi(),
		PreR = X*X + Y*Y,
		R = math:sqrt(PreR),
		P = math:sqrt(PreR + Z*Z),
		Theta = case R == 0 of
			true ->
				0;
			false ->
				if
					(X>0)	and	(Y>=0)	-> math:atan(Y/X);
					(X>0)	and	(Y<0)	-> math:atan(Y/X) + 2*math:pi();
					(X<0)			-> math:atan(Y/X) + math:pi();
					(X==0)	and	(Y>0)	-> math:pi()/2;
					(X==0)	and	(Y<0)	-> 3*math:pi()/2
				end
		end,
		Phi = case P == 0 of
			false ->
				math:acos(Z/P);
			true ->
				0
		end,
		[P,Theta,Phi].
					centripital_distance([Val|Coord],Acc)->
						centripital_distance(Coord,Val*Val+Acc);
					centripital_distance([],Acc)->
						math:sqrt(Acc).
						
					calculate_distance([Val1|Coord1],[Val2|Coord2],Acc)->
						Distance = Val2 - Val1,
						calculate_distance(Coord1,Coord2,Distance*Distance+Acc);
					calculate_distance([],[],Acc)->
						math:sqrt(Acc).

to_cartesian(Direction)->
	case Direction of
		{spherical,Coordinates}->
			{cartesian,spherical2cartesian(Coordinates)};
		{polar,Coordinates}->
			{cartesian,polar2cartesian(Coordinates)};
		{cartesian,Coordinates}->
			{cartesian,Coordinates}
	end.
						
normalize(Vector)->
	Normalizer = calculate_normalizer(Vector,0),
	normalize(Vector,Normalizer,[]).
					
	calculate_normalizer([Val|Vector],Acc)->
		calculate_normalizer(Vector,Val*Val+Acc);
		calculate_normalizer([],Acc)->
		math:sqrt(Acc).
					
	normalize([Val|Vector],Normalizer,Acc)->
		normalize(Vector,Normalizer,[Val/Normalizer|Acc]);
		normalize([],_Normalizer,Acc)->
		lists:reverse(Acc).
				
spherical2cartesian({P,Theta,Phi})->
	X = P*math:sin(Phi)*math:cos(Theta),
	Y = P*math:sin(Phi)*math:sin(Theta),
	Z = P*math:cos(Phi),
	{X,Y,Z}.

%Theta: 0-2Pi, Phi:0-Pi, R: 0+, P: 0+		
cartesian2spherical({X,Y})->
	cartesian2spherical({X,Y,0});
cartesian2spherical({X,Y,Z})->
	%Pi = math:pi(),
	PreR = X*X + Y*Y,
	R = math:sqrt(PreR),
	P = math:sqrt(PreR + Z*Z),
	Theta = case R == 0 of
		true ->
			0;
		false ->
			if
				(X>0)	and	(Y>=0)	-> math:atan(Y/X);
				(X>0)	and	(Y<0)	-> math:atan(Y/X) + 2*math:pi();
				(X<0)			-> math:atan(Y/X) + math:pi();
				(X==0)	and	(Y>0)	-> math:pi()/2;
				(X==0)	and	(Y<0)	-> 3*math:pi()/2
			end
	end,
	Phi = case P == 0 of
		false ->
			math:acos(Z/P);
		true ->
			0
	end,
	{P,Theta,Phi}.
				
polar2cartesian({R,Theta})->
	X = R*math:cos(Theta),
	Y = R*math:sin(Theta),
	{X,Y,0}.

%Theta: 0-2Pi, R: 0+			
cartesian2polar({X,Y})->
	cartesian2polar({X,Y,0});
cartesian2polar({X,Y,_Z})->
	R = math:sqrt(X*X + Y*Y),
	Theta = case R == 0 of
		true ->
			0;
		false ->
			if
				(X>0)	and	(Y>=0)	-> math:atan(Y/X);
				(X>0)	and	(Y<0)	-> math:atan(Y/X) + 2*math:pi();
				(X<0)			-> math:atan(Y/X) + math:pi();
				(X==0)	and	(Y>0)	-> math:pi()/2;
				(X==0)	and	(Y<0)	-> 3*math:pi()/2
			end
	end,
	{R,Theta}.

distance(Vector1,Vector2)->
	distance(Vector1,Vector2,0).	
distance([Val1|Vector1],[Val2|Vector2],Acc)->
	distance(Vector1,Vector2,Acc+math:pow(Val2-Val1,2));
distance([],[],Acc)->
	math:sqrt(Acc).
	
vector_difference(Vector1,Vector2)->
	vector_difference(Vector1,Vector2,[]).
vector_difference([Val1|Vector1],[Val2|Vector2],Acc)->
	vector_difference(Vector1,Vector2,[Val2-Val1|Acc]);
vector_difference([],[],Acc)->
	lists:reverse(Acc).

































	

g()->
	spawn(geometry,loop,[]).

visor(EnvPId,UpdateRate)->
	{Parameters,Environment} = get_EnvData(EnvPId),
	
	GS = gs:start(),
	Window = gs:create(window,GS,[{title,"Visor"},{width,1440},{height,900}]),
	Canvas = gs:create(canvas,Window,[{width,1440},{height,900}]),
	gs:config(Window,{map,true}),
	
	draw_Env(Environment),
	spawn(geometry,visor,[EnvPId,UpdateRate,GS,Window,Canvas]).
	
	visor(EnvPId,UpdateRate,GS,Window,Canvas)->
		receive 
			{gs,FromId,click,Data,Args}->
				io:format("FromId:~p Data:~p Args:~p~n",[FromId,Data,Args]),
				visor(EnvPId,UpdateRate,GS,Window,Canvas);
			{new_UpdateRate,New_UpdateRate}->
				io:format("Visor recieved a new UpdateRate:~p~n",[New_UpdateRate]),
				visor(EnvPId,New_UpdateRate,GS,Window,Canvas);
			terminate ->
				done
		after UpdateRate ->
			{Parameters,Environment} = get_EnvData(EnvPId),
			draw_Env(Environment),
			geometry:visor(EnvPId,UpdateRate,GS,Window,Canvas)
		end.
		
		get_EnvData(_)->
			done.
		draw_Env(_)->
			done.
loop()->
	{A,B,C} = erlang:now(),
	random:seed(A,B,C),
	register(loop,self()),
	GS = gs:start(),
	Window = gs:create(window,GS,[{title,"Visor"},{width,700},{height,900}]),
	Canvas = gs:create(canvas,Window,[{width,700},{height,900}]),
	gs:config(Window,{map,true}),
	
	%gs:config(Line,[{buttonpress,true}]),
	%io:format("GS:~p Window:~p Button:~p~n",[GS,Window,Button]),
	%io:format("Line:~p~n",[Line]),
	loop(GS,Window,Canvas).
	
	loop(GS,Window,Canvas)->
		receive
			{gs,FromId,click,Data,Args} ->
				io:format("FromId:~p Data:~p Args:~p~n",[FromId,Data,Args]),
				geometry:loop(GS,Window,Canvas);
			{draw,line,Loc}->
				draw_line(Canvas,Loc),
				geometry:loop(GS,Window,Canvas);
			{draw,oval,Loc}->
				draw_oval(Canvas,Loc),
				geometry:loop(GS,Window,Canvas);
			{draw,polygon,Loc}->
				draw_polygon(Canvas,Loc),
				geometry:loop(GS,Window,Canvas);
			{draw,move,Position}->
				case get(object) of
					undefined ->
						Object = create_cell(Canvas,{0,0}),
						put(object,Object);
					Object ->
						Object
				end,
				translation(Object,Position),
				geometry:loop(GS,Window,Canvas);
			{draw,rotate,Angle}->
				case get(object) of
					undefined ->
						io:format("No object");
					Object ->
						rotation(Object,Angle,{0,0})
				end,
				geometry:loop(GS,Window,Canvas);
			{draw,rotate,Angle,Point}->
				case get(object) of
					undefined ->
						io:format("No object");
					Object ->
						rotation(Object,Angle,Point)
				end,
				geometry:loop(GS,Window,Canvas);
			{draw,delete}->
				Object = get(object),
				erase(object),
				[gs:destroy(Element) || Element <-Object],
				geometry:loop(GS,Window,Canvas);
			{create,grid,2} ->
				%create_grid(),
				geometry:loop(GS,Window,Canvas);
			reset ->
				gs:stop(),
				unregister(loop),
				erase(),
				geometry:loop();
			Msg->
				io:format("Msg:~p~n",[Msg])
		after 1000 ->
			%io:format("Position:~p~n",[Position]),
%			DX = random:uniform()-0.5,
%			DY = random:uniform()-0.5,
%			gs:config(Line,[{move,{DX*10,DY*10}}]),
			%draw_random(Canvas,3,{random:uniform(700),random:uniform(700)}),
			%gs:config(Line,[{x,500+DX*10},{y,500+DY*10}]),
			geometry:loop(GS,Window,Canvas)
		end.

	translation([Element|Object],DV)->
		gs:config(Element,[{move,DV}]),
		translation(Object,DV);
	translation([],_DV)->
		done.

	rotation(Object,Angle)->
		rotation(Object,Angle,{0,0}).
	rotation([Element|Object],Angle,{PX,PY})->
		Coords = gs:read(Element,coords),
		%Coords1 = [{X -PX,Y -PY} || {X,Y} <- Coords],
		U_Coords = [{(X-PX)*math:cos(Angle) - (Y-PY)*math:sin(Angle) +PX,(X-PX)*math:sin(Angle) + (Y-PY)*math:cos(Angle) +PY} || {X,Y} <- Coords],
		gs:config(Element,[{coords,U_Coords}]),
		io:format("Coords:~p~n",[Coords]),
		rotation(Object,Angle,{PX,PY});
	rotation([],_Angle,_Point)->
		done.

create_avatar(Canvas,Loc,Type,I,O)->
	case Type of
		cell ->
			create_cell(Canvas,Loc);
		tank ->
			void
	end.

	create_cell(Canvas,Loc)->
		{X,Y} = Loc,
		E1_Id = draw_polygon(Canvas,[{0+X,0+Y},{0+X,100+Y},{100+X,0+Y}]),
		E2_Id = draw_arrow(Canvas,[{50+X,50+Y},{110+X,110+Y}]),
		E1 = #e{id=E1_Id,type=oval,pivot={50+X,50+Y}},
		E2 = #e{id=E2_Id,type=arrow,pivot={50+X,50+Y}},
%		Object = #object{id=object1,type = cell,pivot={50+X,50+Y},elements=[E1,E2],element_ids=[E1_Id,E2_Id]},
		[E1_Id,E2_Id].

delete_avatar(Id)->
	void.

create_object(Canvas,Loc,Type)->
	case Type of
		cell->
			void
	end.

delete_object(Id)->
	void.

create_element(Canvas,Loc,Pivot,Type)->
	case Type of
		line ->
			void;
		polygon ->
			void;
		rectangle ->
			void;
		oval ->
			void
	end.

	draw_line(Canvas,Loc)->
		[{X1,Y1},{X2,Y2}] = Loc,
		gs:create(line,Canvas,[{coords,[{X1,Y1},{X2,Y2}]}]).
		
	draw_arrow(Canvas,Loc)->
		[{X1,Y1},{X2,Y2}] = Loc,
		gs:create(line,Canvas,[{coords,[{X1,Y1},{X2,Y2}]},{arrow,last}]).
			
	draw_oval(Canvas,Loc)->
		[{X1,Y1},{X2,Y2}] = Loc,
		gs:create(oval,Canvas,[{coords,[{X1,Y1},{X2,Y2}]}]).

	draw_polygon(Canvas,Coords)->
		case is_list(Coords) of
			true ->
				gs:create(polygon,Canvas,[{coords,Coords}]);
			false ->
				io:format("not a list in draw_polygon")
		end.

create_element(Type,Id,Loc)->
	case Type of
		fire ->%red
			%burns_organism,
			%located in various places, emerges when earth + water + air clay is put together,
			%when put together with water, earth, fire, creates clay
			%fire plus water = air;
			ok;
		earth ->%brown
			%round, takes energy to move around
			%earth + earth creates heavier chunks;
			ok;
		water ->%blue
			%comes from large water pools at regular intervals
			%organism requires
			%can be made to stream between clay walls
			ok;
		air ->%light_blue
			ok;
		clay ->%white
			%inert, stays put
			ok;
		energy_plant ->%green
			%is required to do anything,
			%grows around water pools
			ok
	end.
		
create_atom(Canvas,Loc,Type)->
	case Type of
		hydrogen->
			void;
		carbon ->
			void;
		oxygen ->
			void
	end.

%HRes = Val,
%VRes = Val,
%ISubstrate = %[[{[Z1,Y,X],o,[W1...Wn]}...{[Z1,Yn,Xk],o,[W1...Wn]}]...[{[Zs,Y,X],o,[W1...Wn]}...]],
%calculate_output2([{_I_Coord,O,_I_Weights}|I_Hypercube],{Coord,Prev_O,[Weight|Weights]},Acc)->
%	calculate_output2(I_Hypercube,{Coord,Prev_O,Weights},O*Weight+Acc);
%calculate_output2([],{Coord,Prev_O,[]},Acc)->
%	functions:tanh(Acc).
%Encoding1: Brackets only for every changei in last dimension. Dimension = coord length.
%	[
%		[
%			{[Z1...],O,Weights}...{[Z1...],O,Weights}
%		],
%		[
%			{[Z2...],O,Weights}...{[Z2...],O,Weights}
%		],
%		...
%	]
%Encoding2: Brackets for every dimension, Dimension = bracket depth = coord length
%	[
%		[
%			[
%				{[Z1,Y1,X1],O,Weights},
%				{[Z1,Y1,X2],O,Weights},
%				...
%			],
%			[
%				{[Z1,Y2,X1],O,Weights},
%				{[Z1,Y2,X2],O,Weights},
%				...
%			]
%			...
%		]
%		[
%			[
%				{[Z2,Y1,X1],O,Weights},
%				{[Z2,Y1,X2],O,Weights},
%				...
%			],
%			[
%				{[Z2,Y2,X1],O,Weights},
%				{[Z2,Y2,X2],O,Weights},
%				...
%			]
%			...
%		]
%	]

v()->
	spawn(geometry,vis,[]).
test_l2plane(Type,HRes,VRes)->
	Plane = test_list2plane(Type,HRes,VRes),
	%Plane = fx:plane_encoded(HRes,VRes,'EURUSD15'),
	vis ! {self(),draw_plane,Plane,HRes},
	receive
		done ->
			io:format("Received Done~n")
	end.
	
vis()->
	Width = 900,
	Height = 800,
	GS = gs:start(),
	Window = gs:create(window,GS,[{title,"List_To_Graph/Plane"},{width,Width},{height,Height}]),
	Canvas = gs:create(canvas,Window,[{width,Width},{height,Height}]),
	register(vis,self()),
	gs:config(Window,{map,true}),
	%ITE = ets:new(ite,[set,private]),
	vis(Window,Canvas,Width,Height,[]).
	
	vis(Window,Canvas,Width,Height,Objects)->
		receive 
			{From,draw_plane,Plane,HRes}->
				PlaneObjects= create_PlaneObjects(Plane,HRes,Width,Height),
				remove_objects(Objects),
				Drawn_PlaneObjects = visor:draw_objects(Canvas,PlaneObjects,[]),
				From ! done,
				geometry:vis(Window,Canvas,Width,Height,Drawn_PlaneObjects);
			{From,fill_polygon,Plane,PolygonCoords,HRes}->
				Polygon = {polygon,undefined,black,{0,0},[{(X+1)*Width/2,(Y+1)*Height/2}||{X,Y}<-PolygonCoords],1},
				PlaneObjects= create_PlaneObjects(lists:reverse(Plane),HRes,Width,Height),
				remove_objects(Objects),
				Drawn_PlaneObjects = visor:draw_objects(Canvas,[Polygon|PlaneObjects],[]),
				From ! done,
				geometry:vis(Window,Canvas,Width,Height,Drawn_PlaneObjects);
			terminate ->
				done
		end.

%gs:create(polygon,Canvas,[{coords,Coords}])
			create_PlaneObjects(Plane,HRes,Width,Height)->
				VRes = length(Plane)/HRes,
				XD = Width/HRes,
				YD = Height/VRes,
				XR = XD/2,
				YR = YD/2,
				io:format("HRes:~p VRes:~p XR:~p YR:~p~n",[HRes,VRes,XR,YR]),
				create_PlaneObjects(Plane,HRes,HRes,XR,YR,XR,YR,[]).
%gs:create(rectangle,Canvas,[{coords,[{30,30},{70,70}]},{fill,cyan},{bw,2}]).

				create_PlaneObjects(Plane,0,MHRes,X,Y,XR,YR,Acc)->
					create_PlaneObjects(Plane,MHRes,MHRes,XR,Y+2*YR,XR,YR,Acc);
				create_PlaneObjects([Val|Plane],HRes,MHRes,X,Y,XR,YR,Acc)->
					case Val of
						1 ->	
							Object = {polygon,undefined,black,{X,Y},[{X-XR,Y-YR},{X+XR,Y-YR},{X+XR,Y+YR},{X-XR,Y+YR}],(XR+YR)/2},
							%Object = {circle,undefined,black,{X,Y},[{X,Y}],R},
							create_PlaneObjects(Plane,HRes-1,MHRes,X+2*XR,Y,XR,YR,[Object|Acc]);
						0 ->
							Object = {polygon,undefined,gray,{X,Y},[{X-XR,Y-YR},{X+XR,Y-YR},{X+XR,Y+YR},{X-XR,Y+YR}],(XR+YR)/2},
							create_PlaneObjects(Plane,HRes-1,MHRes,X+2*XR,Y,XR,YR,[Object|Acc]);
						-1 ->
							create_PlaneObjects(Plane,HRes-1,MHRes,X+2*XR,Y,XR,YR,Acc)
					end;
				create_PlaneObjects([],_MHRes,_MHRes,_X,_Y,_XR,_YR,Acc)->
					lists:reverse(Acc).
%list2plane, where the magnitude of the values in the list is mapped to the y dimension in the ISubstrate, with 0=NoDraw, 1=Draw
%This is done while keeping the ISubstrate using the pre-specified dimensions HRes(horizontal) and VRes(vertical). 
test_list2plane(Type,HRes,VRes)->
	List = [math:sin(Val/10)||Val <- lists:seq(1,HRes)],%[4,2,2,4,3,3,4,5,6,7],
	Plane = geometry:list2plane(Type,List,HRes,VRes,void,void).
	%io:format("Plane:~p~n",[Plane]),
	%print_plane(Plane,HRes,HRes).
	
	print_plane(List,0,MIndex)->
		io:format("~n"),
		print_plane(List,MIndex,MIndex);
	print_plane([Val|List],HIndex,MIndex)->
		io:format("~p",[Val]),
		print_plane(List,HIndex-1,MIndex);
	print_plane([],_MIndex,_MIndex)->
		ok.

list2plane(Type,VList,HRes,VRes,VMin,VMax)->
	case {VMin,VMax} of
		{void,void}->
			LVMax1 = lists:max(VList),
			LVMin1 = lists:min(VList),
			LVMax = LVMax1+abs(LVMax1)/20,
			LVMin = LVMin1-abs(LVMin1)/20,
			VStep = (LVMax-LVMin)/VRes,
			HStep = 2/HRes,
			HMin = -1,
			HMax = 1,
			V_StartPos = LVMin + VStep/2,
			H_StartPos = HMin + HStep/2;
		_ ->
			LVMax = void,
			LVMin = void,
			VStep = (VMax-VMin)/VRes,
			HStep = void,
			V_StartPos = VMin
	end,
%	io:format("VLMax:~p VLMin:~p VStep:~p V_StartPos:~p HStep:~p~n",[LVMax,LVMin,VStep,V_StartPos,HStep]),
	case Type of
		p ->
			l2p(HRes*VRes,{VList,VList},V_StartPos,VStep,[]);
		g ->
			l2g(HRes*VRes,{VList,VList},V_StartPos,VStep,[]);
		fx ->
			%FXList = [{random:uniform()/5,random:uniform()/5,random:uniform()/5,-random:uniform()/5}|| Val <- VList],
			Trailing_Index=case get(index) of
				undefined ->
					Val=ets:first('EURUSD15'),
					put(index,Val),
					Val;
				OldVal ->
					Val=ets:next('EURUSD15',OldVal),
					put(index,Val),
					Val
			end,
			io:format("Index:~p~n",[Trailing_Index]),
			%io:format("Index:~p~n",[Trailing_Index]),
			FXList = fx:fx_GetPriceList('EURUSD15',Trailing_Index,HRes,[]),
			LMax1 = lists:max([High||{_Open,_Close,High,_Low}<-FXList]),
			LMin1 = lists:min([Low||{_Open,_Close,_High,Low}<-FXList]),
			LMax =LMax1+abs(LMax1-LMin1)/20,
			LMin =LMin1-abs(LMax1-LMin1)/20,
			Step = (LMax-LMin)/VRes,
			VStartPos = LMin + Step/2,
			
			%io:format("FXList:~p ~p ~p~n",[FXList,VStartPos,Step]),
			l2fx(HRes*VRes,{FXList,FXList},VStartPos,Step,[])
	end.
	
	l2g(Index,{[Val,NextVal|VList],MemList},VPos,VStep,Acc)->
		O = case Val < NextVal of
			true ->
				case (VPos+VStep/2 > Val) and (VPos-VStep/2 < NextVal) of
					true ->
						1;
					false ->
						-1
				end; 
			false ->
				case (VPos-VStep/2 < Val) and (VPos+VStep/2 > NextVal) of
					true ->
						1;
					false ->
						-1
				end
		end,
		%io:format("Val:~p VPos:~p VStep:~p O:~p~n",[Val,VPos,VStep,O]),
		l2g(Index-1,{[NextVal|VList],MemList},VPos,VStep,[O|Acc]);
	l2g(0,{[],_MemList},_VPos,_VStep,Acc)->
		Acc;
	l2g(Index,{[Val],MemList},VPos,VStep,Acc)->
		O = case (Val =< VPos+VStep/2) and (Val > VPos-VStep/2) of
			true ->
				1;
			false ->
				-1
		end,
		l2g(Index-1,{[],MemList},VPos,VStep,[O|Acc]);
	l2g(Index,{[],MemList},VPos,VStep,Acc)->
		l2g(Index,{MemList,MemList},VPos+VStep,VStep,Acc).
	
	l2p(Index,{[Val|VList],MemList},VPos,VStep,Acc)->
		O = case (Val =< VPos+VStep/2) and (Val > VPos-VStep/2) of
			true ->
				1;
			false ->
				-1
		end,
		%io:format("Val:~p VPos:~p VStep:~p O:~p~n",[Val,VPos,VStep,O]),
		l2p(Index-1,{VList,MemList},VPos,VStep,[O|Acc]);
	l2p(0,{[],_MemList},_VPos,_VStep,Acc)->
		Acc;
	l2p(Index,{[],MemList},VPos,VStep,Acc)->
		l2p(Index,{MemList,MemList},VPos+VStep,VStep,Acc).
		
	l2fx(Index,{[{Open,Close,High,Low}|VList],MemList},VPos,VStep,Acc)->
%		io:format("Index:~p {Open,Close,High,Low}:~p VPos:~p VStep:~p~n",[Index,{Open,Close,High,Low},VPos,VStep]),
		{BHigh,BLow} = case Open > Close of
			true ->
				{Open,Close};
			false ->
				{Close,Open}
		end,
		O = case (VPos+VStep/2 > BLow) and (VPos-VStep/2 =< BHigh) of %(VPos+VStep)/2 > Open	(Close =< VPos+VStep/2) and (Close > VPos-VStep/2) of
			true ->
				1;
			false ->
				case (VPos+VStep/2 > Low) and (VPos-VStep/2 =< High) of
					true ->
						0;
					false ->
						-1
				end
		end,
		%io:format("Val:~p VPos:~p VStep:~p O:~p~n",[O,VPos,VStep,O]),
		l2fx(Index-1,{VList,MemList},VPos,VStep,[O|Acc]);
	l2fx(0,{[],_MemList},_VPos,_VStep,Acc)->
		%io:format("~p~n",[Acc]),
		Acc;
	l2fx(Index,{[],MemList},VPos,VStep,Acc)->
		%io:format("Acc:~p~n",[Acc]),
		l2fx(Index,{MemList,MemList},VPos+VStep,VStep,Acc).
		
%{From,fill_polygon,Plane,Polygon,HRes}
test_pip2plane(PolygonCoords,HRes,VRes)->
	Plane = pip2plane(PolygonCoords,HRes,VRes),
	{Xs,Ys} = lists:unzip(PolygonCoords),
	XCenter = lists:sum(Xs)/length(Xs),
	YCenter = lists:sum(Ys)/length(Ys),
	%Polygon = {polygon,undefined,black,{XCenter,YCenter},PolygonCoords,1},
	io:format("Plane:~p~n",[Plane]),
	vis ! {self(),fill_polygon,Plane,PolygonCoords,HRes},
	receive
		done ->
			io:format("Received Done~n")
	end.
		%P1=[{1,1},{1,-1},{-1,-1},{-1,1}].
pip2plane(Polygon,HRes,VRes)->
	VStep = 2/VRes,
	HStep = 2/HRes,
	HMin = -1,
	HMax = 1,
	VMin = -1,
	VMax = 1,
	
	V_StartPos = VMin + VStep/2,
	H_StartPos = HMin + HStep/2,
	io:format("VMax:~p VMin:~p VStep:~p V_StartPos:~p HStep:~p~n",[VMax,VMin,VStep,V_StartPos,HStep]),
	pip2p(HRes*VRes,Polygon,{HRes,HRes},{H_StartPos,H_StartPos},V_StartPos,HStep,VStep,[]).
	
	pip2p(0,_Polygon,{0,_MHRes},{_HPos,_HStartPos},_VPos,_HStep,_VStep,Acc)->
		Acc;
	pip2p(Index,Polygon,{0,MHRes},{_HPos,HStartPos},VPos,HStep,VStep,Acc)->
		pip2p(Index,Polygon,{MHRes,MHRes},{HStartPos,HStartPos},VPos+VStep,HStep,VStep,Acc);
	pip2p(Index,Polygon,{HIndex,MHRes},{HPos,HStartPos},VPos,HStep,VStep,Acc)->
		X=HPos,
		Y=VPos,
		O = case point_in_polygon({X,Y},Polygon) of
			true ->
				1;
			false ->
				-1
		end,
		%io:format("Val:~p VPos:~p VStep:~p O:~p~n",[Val,VPos,VStep,O]),
		pip2p(Index-1,Polygon,{HIndex-1,MHRes},{HPos+HStep,HStartPos},VPos,HStep,VStep,[O|Acc]).
		
point_in_polygon(Point,PNodes)->
	[LeadingNode|TailNodes] = PNodes,
	point_in_polygon(LeadingNode,Point,LeadingNode,TailNodes,0).

point_in_polygon(LN,{X1,Y1},{X3,Y3},[{X4,Y4}|Nodes],Acc)->
	%{S,D} = Gaze,
	%{X1,Y1} = S,
	{XD0,YD0} = {X1,Y1+1},%D,
	PerpXD1 = Y4-Y3,
	PerpYD1 = -(X4-X3),
	PerpXD0 = YD0,
	PerpYD0 = -XD0,
%	io:format("PerpXD1:~p PerpYD1:~p PerpXD0:~p PerpYD0:~p S:~p D:~p~n",[PerpXD1,PerpYD1,PerpXD0,PerpYD0,{X1,Y1},{XD0,YD0}]),
	Denom = PerpXD1*XD0 + PerpYD1*YD0,
	Intersection=case Denom == 0 of
		true ->
			%inf;
			0;%no intersection
		false ->
			%io:format("Denom:~p~n",[Denom]),
			RayLength = ((PerpXD1*(X3-X1)) + (PerpYD1*(Y3-Y1)))/Denom,
			T = ((PerpXD0*(X3-X1)) + (PerpYD0*(Y3-Y1)))/Denom,			
%			io:format("RayLength:~p T:~p Denom:~p~n",[RayLength,T,Denom]),
			case (RayLength >= 0) and (T >= 0) and (T =< 1) of
				true ->
					%RayLength;
					1;%intersection present
				false ->
					%inf
					0%no intersection
			end
	end,
	point_in_polygon(LN,{X1,Y1},{X4,Y4},Nodes,Acc+Intersection);
point_in_polygon({X4,Y4},{X1,Y1},{X3,Y3},[],Acc)->
	{XD0,YD0} = {X1,Y1+1},%D,
	PerpXD1 = Y4-Y3,
	PerpYD1 = -(X4-X3),
	PerpXD0 = YD0,
	PerpYD0 = -XD0,
	Denom = PerpXD1*XD0 + PerpYD1*YD0,
%	io:format("PerpXD1:~p PerpYD1:~p PerpXD0:~p PerpYD0:~p S:~p D:~p~n",[PerpXD1,PerpYD1,PerpXD0,PerpYD0,{X1,Y1},{XD0,YD0}]),
	Intersection=case Denom == 0 of
		true ->
			%inf;
			0;%no intersection
		false ->
			RayLength = ((PerpXD1*(X3-X1)) + (PerpYD1*(Y3-Y1)))/Denom,
			T = ((PerpXD0*(X3-X1)) + (PerpYD0*(Y3-Y1)))/Denom,		
%			io:format("RayLength:~p T:~p Denom:~p~n",[RayLength,T,Denom]),	
			case (RayLength >= 0) and (T >= 0) and (T =< 1) of
				true ->
					%RayLength;
					1;%intersection present
				false ->
					%inf
					0%no intersection
			end
	end,
%	io:format("Tot Intersections:~p~n",[Intersection+Acc]),
	Tot = Intersection+Acc,
	case  Tot == 0 of
		true ->
			false;
		false ->
			case (Tot rem 2) == 0 of
				true ->
					false;
				false ->
					true
			end
	end.

remove_objects(undefined)->
	done;
remove_objects(Objects)->
	[gs:destroy(Id)||{_Type,Id,_,_,_,_}<-Objects].

%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(simulations).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Simulations Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
g()->
	spawn(simulations,loop,[]).
	
loop()->
	{A,B,C} = erlang:now(),
	random:seed(A,B,C),
	GS = gs:start(),
	Window = gs:create(window,GS,[{title,"Visor"},{width,700},{height,900}]),
	Canvas = gs:create(canvas,Window,[{width,700},{height,900}]),
	%Button = gs:create(button,Window,[{label,{text,"Press Me"}}]),
	Line = gs:create(line,Canvas,[{coords,[{15,15},{16,16}]}]),
	gs:config(Window,{map,true}),
	%gs:config(Line,[{buttonpress,true}]),
	%io:format("GS:~p Window:~p Button:~p~n",[GS,Window,Button]),
	loop(GS,Window,Canvas,Line).
	
	loop(GS,Window,Canvas,Line)->
		receive
			{gs,FromId,click,Data,Args} ->
				io:format("FromId:~p Data:~p Args:~p~n",[FromId,Data,Args]),
				simulations:loop(GS,Window,Canvas,Line);
			Msg->
				io:format("Msg:~p~n",[Msg])
		after 100 ->
			%io:format("Position:~p~n",[Position]),
			DX = random:uniform()-0.5,
			DY = random:uniform()-0.5,
			gs:config(Line,[{move,{DX*10,DY*10}}]),
			%draw_random(Canvas,random:uniform(3),{random:uniform(700),random:uniform(700)}),
			%gs:config(Line,[{x,500+DX*10},{y,500+DY*10}]),
			simulations:loop(GS,Window,Canvas,Line)
		end.

		draw_random(Canvas,Option,XY)->
			XY2 = {random:uniform(700),random:uniform(700)},
			case Option of
				1 ->
					gs:create(text,Canvas,[{coords,[XY]}]);
				2 ->
					gs:create(rectangle,Canvas,[{coords,[XY,XY2]}]);
				3 ->
					gs:create(oval,Canvas,[{coords,[XY,XY2]}])
			end.

%Double Pole Balancing Visual
dp()->
	spawn(simulations,dp_visor,[10000]).
dp(Timer)->
	spawn(simulations,dp_visor,[Timer]).
dp_visor(Timer)->
	register(dp_visor,self()),
	GS = gs:start(),
	{A,B,C} = now(),
	random:seed(A,B,C),
	
	Window = gs:create(window,GS,[{title,"Visor"},{width,700},{height,900}]),
	Canvas = gs:create(canvas,Window,[{width,700},{height,900}]),
	Floor = gs:create(line,Canvas,[{coords,[{0,800},{700,800}]}]),
	Cart = gs:create(rectangle,Canvas,[{coords,[{325,800},{375,795}]}]),
	Pole1 = gs:create(line,Canvas,[{coords,[{350,795},{358.72,695.2}]}]),
	Pole2 = gs:create(line,Canvas,[{coords,[{350,795},{350,785}]}]),
	gs:config(Window,{map,true}),
	dp_visor(GS,Window,Canvas,Floor,Cart,Pole1,Pole2,Timer).
	
	dp_visor(GS,Window,Canvas,Floor,Cart,Pole1,Pole2,Timer)->
		receive
			{gs,FromId,click,Data,Args} ->
				io:format("FromId:~p Data:~p Args:~p~n",[FromId,Data,Args]),
				simulations:dp_visor(GS,Window,Canvas,Floor,Cart,Pole1,Pole2,Timer);
			{dp_NewState,From,NewState}->
				visualize(GS,Window,Canvas,Floor,Cart,Pole1,Pole2,NewState),
				put(prev_state,NewState),
				simulations:dp_visor(GS,Window,Canvas,Floor,Cart,Pole1,Pole2,Timer);
			terminate ->
				exit("Exiting dp_visor");
			Msg->
				io:format("Msg:~p~n",[Msg])
		after Timer ->
			io:format("dp_visor exiting")
		end.
			
		visualize(GS,Window,Canvas,Floor,Cart,Pole1,Pole2,NewState)->
			{CPosition,CVel,PAngle1,PVel1,PAngle2,PVel2,TimeStep,FitnessAcc} = case get(prev_state) of
				undefined ->
					NewState;
				State ->
					State
			end,
			{NextCPosition,NextCVel,NextPAngle1,NextPVel1,NextPAngle2,NextPVel2,NextTimeStep,NextFitnessAcc} = NewState,
			%simulations:config([Cart,Pole1,Pole2],[{move,{1,0}}]),
			DCPosition = NextCPosition - CPosition,
			DCVel = NextCVel - CVel,
			DPAngle1 = NextPAngle1 - PAngle1,
			DPAngle2 = NextPAngle2 - PAngle2,
			DPVel2 = NextPVel2 - PVel2,
			[{P1X1,P1Y1},{P1X2,P1Y2}] = gs:read(Pole1,coords),
			[{P2X1,P2Y1},{P2X2,P2Y2}] = gs:read(Pole2,coords),
			%[{C1X1,C1Y1},{C1X2,C1Y2}] = gs:read(Cart,coords),
			io:format("~p~n",[{{P1X1,P1Y1},{P1X2,P1Y2}}]),
			geometry:translation([Cart,Pole1,Pole2],{DCPosition*100,0}),
			geometry:rotation([Pole1],DPAngle1,{P1X1,P1Y1}),
			geometry:rotation([Pole2],DPAngle2,{P2X1,P2Y1}),
			%io:format("PoleAngle1:~p PoleAngle2:~p PD1:~p PD2:~p P1L:~p P2L:~p~n",[NextPAngle1,NextPAngle2,{DX1,DY1},{DX2,DY2},P1L,P2L]),
			io:format("PoleAngle1:~p PoleAngle2:~p TimeStep:~p T:~p~n",[NextPAngle1,NextPAngle2,TimeStep,[CPosition,CVel,PVel1,PVel2]]),
			%gs:config(Pole1,[{coords,[{X1,Y1},{X2+(random:uniform()-0.5)*10,Y2+(random:uniform()-0.5)*10}]}]),
			AL = (36/360)*(2*math:pi()),
			case  (NextPAngle2 > AL) or (NextPAngle2 < -AL)of
				true ->
					receive 
						exit -> done
					after 1000 ->
						exit("done")
					end;
				false ->
					done
			end.
	
		config([Object|Objects],Options)->
			gs:config(Object,Options),
			config(Objects,Options);
		config([],_Options)->
			done.


-record(pb_state,{cpos=0,cvel=0,p1_angle=3.6*(2*math:pi()/360),p1_vel=0,p2_angle=0,p2_vel=0,time_step=0,goal_steps=100000,fitness_acc=0}).
%pb_sim(ExoSelf_PId)->
%	random:seed(now()),
%	%io:format("Starting pb_sim:~p~n",[self()]),
%	pb_sim(ExoSelf_PId,#pb_state{}).
	
pole2_balancing1(SensorId,Parameter)->
	S=case get({pole2_balancing,SensorId}) of
		undefined->
			random:seed(now()),
			InitState = #pb_state{},
			put({pole2_balancing,SensorId},InitState),
			InitState;
		Existing_State ->
			Existing_State
	end,%io:format("S:~p~n",[S]),
%		{From_PId,sense, [Parameter]}->%io:format("Sense request received:~p~n",[From_PId]),
			AngleLimit = 2*math:pi()*(36/360),
			Scaled_CPosition = functions:scale(S#pb_state.cpos,2.4,-2.4),
			Scaled_CVel = functions:scale(S#pb_state.cvel,10,-10),
			Scaled_PAngle1 = functions:scale(S#pb_state.p1_angle,AngleLimit,-AngleLimit),
			Scaled_PAngle2 = functions:scale(S#pb_state.p2_angle,AngleLimit,-AngleLimit),
			P1_Vel = S#pb_state.p1_vel,
			P2_Vel = S#pb_state.p2_vel,
			case get(p1_vel) of
				undefined ->
					put(p1_vel,P1_Vel);
				OldMax1 ->
					case P1_Vel > OldMax1 of
						true ->
							put(p1_vel,P1_Vel);
						false ->
							ok
					end
			end,
			case get(p2_vel) of
				undefined ->
					put(p2_vel,P2_Vel);
				OldMax2 ->
					case P2_Vel > OldMax2 of
						true ->
							put(p2_vel,P2_Vel);
						false ->
							ok
					end
			end,
			io:format("P1_Vel:~p P2_Vel:~p~n",[get(p1_vel),get(p2_vel)]),
			
			SenseSignal=case Parameter of
				cpos -> [Scaled_CPosition];
				cvel -> [Scaled_CVel];
				p1_angle -> [Scaled_PAngle1];
				p1_vel -> [P1_Vel];
				p2_angle -> [Scaled_PAngle2];
				p2_vel -> [P2_Vel];
				2 -> [Scaled_CPosition,Scaled_PAngle1];
				3 -> [Scaled_CPosition,Scaled_PAngle1,Scaled_PAngle2];
				4 -> [Scaled_CPosition,Scaled_CVel,Scaled_PAngle1,P1_Vel];
				6 -> [Scaled_CPosition,Scaled_CVel,Scaled_PAngle1,Scaled_PAngle2,P1_Vel,P2_Vel]
			end.
%			From_PId ! {self(),percept,SenseSignal},
%			scape:pb_sim(ExoSelf_PId,S);
pole2_balancing1(ExoSelf,F,ActuatorId,Parameters)->%io:format("Whadup_nigga~n"),
		Damping_Flag = with_damping,
		DPB_Flag = 1,
%		{From_PId,push,[Damping_Flag,DPB_Flag],[F]}->
		S = get({pole2_balancing,ActuatorId}),
			
			AL = 2*math:pi()*(36/360),
			U_S=sm_DoublePole(F*10,S,2),
			TimeStep=U_S#pb_state.time_step,
			CPos=U_S#pb_state.cpos,
			CVel=U_S#pb_state.cvel,
			PAngle1=U_S#pb_state.p1_angle,
			PVel1=U_S#pb_state.p1_vel,
			%io:format("U_S:~p~n",[U_S]),
			case (abs(PAngle1) > AL) or (abs(U_S#pb_state.p2_angle)*DPB_Flag > AL) or (abs(CPos) > 2.4) or (TimeStep > U_S#pb_state.goal_steps)of
				true ->
					erase({pole2_balancing,ActuatorId}),
					case (TimeStep > U_S#pb_state.goal_steps) of
						true ->%Fitness goal reached.
							Fitness = case Damping_Flag of
								without_damping ->
									1;
								with_damping ->
									Fitness1 = TimeStep/1000,
									Fitness2 = case TimeStep < 100 of
										true ->
											0;
										false ->
											0.75/(abs(CPos) +abs(CVel) + abs(PAngle1) + abs(PVel1))
									end,
									Fitness1*0.1 + Fitness2*0.9
							end,
%							From_PId ! {self(),Fitness,goal_reached},
%							scape:pb_sim(ExoSelf_PId,#pb_state{});
%							put({pole2_balancing,ActuatorId},U_S),
							put(goal,reached),
							{1,0};
						false ->
%							From_PId ! {self(),0,1},
%							scape:pb_sim(ExoSelf_PId,#pb_state{})
%							put({pole2_balancing,ActuatorId},U_S),
							{1,0}
					end;
				false ->
					Fitness = case Damping_Flag of
						without_damping ->
							1;
						with_damping ->
							Fitness1 = TimeStep/1000,
							Fitness2 = case TimeStep < 100 of
								true ->
									0;
								false ->
									0.75/(abs(CPos) +abs(CVel) + abs(PAngle1) + abs(PVel1))
							end,
							Fitness1*0.1 + Fitness2*0.9
					end,		
					put({pole2_balancing,ActuatorId},U_S#pb_state{fitness_acc=U_S#pb_state.fitness_acc+Fitness}),
					{0,Fitness}
			end.
	
	sm_DoublePole(_F,S,0)->
		S#pb_state{time_step=S#pb_state.time_step+1};
	sm_DoublePole(F,S,SimStepIndex)->
		CPos=S#pb_state.cpos,
		CVel=S#pb_state.cvel,
		PAngle1=S#pb_state.p1_angle,
		PAngle2=S#pb_state.p2_angle,
		PVel1=S#pb_state.p1_vel,
		PVel2=S#pb_state.p2_vel,
		X = CPos, %EdgePositions = [-2.4,2.4],
		PHalfLength1 = 0.5, %Half-length of pole 1
		PHalfLength2 = 0.05, %Half-length of pole 2
		M = 1, %CartMass
		PMass1 = 0.1, %Pole1 mass
		PMass2 = 0.01, %Pole2 mass
		MUc = 0.0005, %Cart-Track Friction Coefficient
		MUp = 0.000002, %Pole-Hinge Friction Coefficient
		G = -9.81, %Gravity
		Delta = 0.01, %Timestep
		EM1 = PMass1*(1-(3/4)*math:pow(math:cos(PAngle1),2)),
		EM2 = PMass2*(1-(3/4)*math:pow(math:cos(PAngle2),2)),
		EF1 = PMass1*PHalfLength1*math:pow(PVel1,2)*math:sin(PAngle1)+(3/4)*PMass1*math:cos(PAngle1)*(((MUp*PVel1)/(PMass1*PHalfLength1))+G*math:sin(PAngle1)),
		EF2 = PMass2*PHalfLength2*math:pow(PVel2,2)*math:sin(PAngle2)+(3/4)*PMass2*math:cos(PAngle2)*(((MUp*PVel2)/(PMass1*PHalfLength2))+G*math:sin(PAngle2)),
		NextCAccel = (F - MUc*functions:sgn(CVel)+EF1+EF2)/(M+EM1+EM2),
		NextPAccel1 = -(3/(4*PHalfLength1))*((NextCAccel*math:cos(PAngle1))+(G*math:sin(PAngle1))+((MUp*PVel1)/(PMass1*PHalfLength1))),
		NextPAccel2 = -(3/(4*PHalfLength2))*((NextCAccel*math:cos(PAngle2))+(G*math:sin(PAngle2))+((MUp*PVel2)/(PMass2*PHalfLength2))),
	
		NextCVel = CVel+(Delta*NextCAccel),
		NextCPos = CPos+(Delta*CVel),
		NextPVel1 = PVel1+(Delta*NextPAccel1),
		NextPAngle1 = PAngle1+(Delta*NextPVel1),
		NextPVel2 = PVel2+(Delta*NextPAccel2),
		NextPAngle2 = PAngle2+(Delta*NextPVel2),
		U_S=S#pb_state{
			cpos=NextCPos,
			cvel=NextCVel,
			p1_angle=NextPAngle1,
			p1_vel=NextPVel1,
			p2_angle=NextPAngle2,
			p2_vel=NextPVel2
		},
		sm_DoublePole(0,U_S,SimStepIndex-1).

pole2_balancing(SensorId,Parameter)->
	{CPosition,CVel,PAngle1,PVel1,PAngle2,PVel2,TimeStep,GoalTimeSteps,MaxTimeSteps,FitnessAcc}=case get({pole2_balancing,SensorId}) of
		undefined ->
			{A,B,C} = now(),
			random:seed(A,B,C),
			case get(opmode) of
				_ ->
					%Angle1 = (random:uniform() - 0.5)*2*(2*math:pi()/360),
					%Angle2 = (random:uniform() - 0.5)*2*(2*math:pi()/360),
					Angle1 = 3.6*(2*math:pi()/360),
					Angle2 = 0,
					InitState = {0,0,Angle1,0,Angle2,0,1,100000,100000,0},
					InitState
			end,
			put({pole2_balancing,SensorId},InitState),
			InitState;
		PrevState->
			PrevState
	end,
%	io:format("~p ~p ~p~n",[{CPosition,CVel,PAngle1,PVel1,PAngle2,PVel2,TimeStep,GoalTimeSteps,MaxTimeSteps,FitnessAcc},Parameter,TimeStep]),
	Rad2Angle = 2*math:pi()/360,
	AngleLimit = Rad2Angle*36,
%	io:format("PAngle2:~p~n",[PAngle2/(2*math:pi()/360)]),
	Scaled_CPosition = functions:scale(CPosition,2.4,-2.4),
	Scaled_CVel = functions:scale(CVel,10,-10),
	Scaled_PAngle1 = functions:scale(PAngle1,AngleLimit,-AngleLimit),
	Scaled_PAngle2 = functions:scale(PAngle2,AngleLimit,-AngleLimit),
	%Scaled_PVel1 = functions:scale(PVel1,5,-5),
	%Scaled_PVel2 = functions:scale(P2,10,-10),
%			case get(p1_vel) of
%				undefined ->
%					put(p1_vel,PVel1);
%				OldMax1 ->
%					case PVel1 < OldMax1 of
%						true ->
%							put(p1_vel,PVel1);
%						false ->
%							ok
%					end
%			end,
%			case get(p2_vel) of
%				undefined ->
%					put(p2_vel,PVel2);
%				OldMax2 ->
%					case PVel2 < OldMax2 of
%						true ->
%							put(p2_vel,PVel2);
%						false ->
%							ok
%					end
%			end,
%			io:format("P1_Vel:~p P2_Vel:~p~n",[get(p1_vel),get(p2_vel)]),
	case Parameter of
		cpos -> [Scaled_CPosition];
		cvel -> [Scaled_CVel];
		pangle1 -> [Scaled_PAngle1];
		pvel1 -> [PVel1];
		pangle2 -> [Scaled_PAngle2];
		pvel2 -> [PVel2];
		3 -> [Scaled_CPosition,Scaled_PAngle1,Scaled_PAngle2];
		6 -> [Scaled_CPosition,Scaled_CVel,Scaled_PAngle1,Scaled_PAngle2,PVel1,PVel2]
	end.

pole2_balancing(ExoSelf,F,ActuatorId,Parameters)->
	{CPosition,CVel,PAngle1,PVel1,PAngle2,PVel2,TimeStep,GoalTimeSteps,MaxTimeSteps,FitnessAcc} =  get({pole2_balancing,ActuatorId}),
	AL = 2*math:pi()*(36/360),
	{NextCPosition,NextCVel,NextPAngle1,NextPVel1,NextPAngle2,NextPVel2}=sm_DoublePole(F*10,CPosition,CVel,PAngle1,PVel1,PAngle2,PVel2,2),
	case get(opmode) of
		test ->
			case whereis(dp_visor) of
				undefined ->
					ok;
				PId ->
					timer:sleep(100),
					PId ! {dp_NewState,self(),{NextCPosition,NextCVel,NextPAngle1,NextPVel1,NextPAngle2,NextPVel2,TimeStep,FitnessAcc}}
				end;
		_ ->
			done
	end,
	case(NextPAngle1 > AL)or (NextPAngle1 < -AL) or (NextPAngle2 > AL) or (NextPAngle2 < -AL) or (CPosition > 2.4) or (CPosition < -2.4) or (TimeStep >= MaxTimeSteps)of
		true ->
			erase({pole2_balancing,ActuatorId}),
			case TimeStep >= GoalTimeSteps of
				true ->
					put(goal,reached);
				false ->
					done
			end,
			{1,0};
		false ->
			Fitness = case without_damping of
				without_damping ->
					1;
				with_damping ->
					Fitness1 = TimeStep/1000,
					Fitness2 = case TimeStep < 100 of
						true ->
							0;
						false ->
							0.75/(abs(CPosition) +abs(CVel) + abs(PAngle1) + abs(PVel1))
					end,
					Fitness1*0.1 + Fitness2*0.9
			end,		
			U_FitnessAcc = FitnessAcc+Fitness,
			NewState = {NextCPosition,NextCVel,NextPAngle1,NextPVel1,NextPAngle2,NextPVel2,TimeStep+1,GoalTimeSteps,MaxTimeSteps,U_FitnessAcc},
			put({pole2_balancing,ActuatorId},NewState),
%			io:format("Fitness:~p TimeStep:~p Parameter:~p ~n",[Fitness,TimeStep,Parameters]),
%			io:format("~p~n",[{CPosition,CVel,CAccel,PAngle1,PVel1,PAccel1,PAngle2,PVel2,PAccel2,TimeStep,FitnessAcc}]),
			{0,Fitness}
	end.

sm_DoublePole(_F,CPosition,CVel,PAngle1,PVel1,PAngle2,PVel2,0)->
	{CPosition,CVel,PAngle1,PVel1,PAngle2,PVel2};
sm_DoublePole(F,CPosition,CVel,PAngle1,PVel1,PAngle2,PVel2,TimeSteps)->
	X = CPosition, %EdgePositions = [-2.4,2.4],
	PHalfLength1 = 0.5,
	PHalfLength2 = 0.05,
	M = 1, %CartMass
	PMass1 = 0.1,
	PMass2 = 0.01,
	MUc = 0.0005, %CartTrackFrictionCoefficient
	MUp = 0.000002, %PoleHingeFrictionCoefficient
	G = -9.81,
	Delta = 0.01,
	EM1 = PMass1*(1-(3/4)*math:pow(math:cos(PAngle1),2)),
	EM2 = PMass2*(1-(3/4)*math:pow(math:cos(PAngle2),2)),
	EF1 = PMass1*PHalfLength1*math:pow(PVel1,2)*math:sin(PAngle1)+(3/4)*PMass1*math:cos(PAngle1)*(((MUp*PVel1)/(PMass1*PHalfLength1))+G*math:sin(PAngle1)),
	EF2 = PMass2*PHalfLength2*math:pow(PVel2,2)*math:sin(PAngle2)+(3/4)*PMass2*math:cos(PAngle2)*(((MUp*PVel2)/(PMass1*PHalfLength2))+G*math:sin(PAngle2)),
	NextCAccel = (F - MUc*functions:sgn(CVel)+EF1+EF2)/(M+EM1+EM2),
	NextPAccel1 = -(3/(4*PHalfLength1))*((NextCAccel*math:cos(PAngle1))+(G*math:sin(PAngle1))+((MUp*PVel1)/(PMass1*PHalfLength1))),
	NextPAccel2 = -(3/(4*PHalfLength2))*((NextCAccel*math:cos(PAngle2))+(G*math:sin(PAngle2))+((MUp*PVel2)/(PMass2*PHalfLength2))),
	
	NextCVel = CVel+(Delta*NextCAccel),
	NextCPosition = CPosition+(Delta*CVel),
	NextPVel1 = PVel1+(Delta*NextPAccel1),
	NextPAngle1 = PAngle1+(Delta*NextPVel1),
	NextPVel2 = PVel2+(Delta*NextPAccel2),
	NextPAngle2 = PAngle2+(Delta*NextPVel2),
	sm_DoublePole(0,NextCPosition,NextCVel,NextPAngle1,NextPVel1,NextPAngle2,NextPVel2,TimeSteps-1).

				
mimic(ExoSelf,Output,TableName,Feature)->
	{IndexStart,IndexEnd,Index,FitnessAcc} = get(TableName),
	T = db:lookup_element(TableName,Index,Feature),
	Target = case is_list(T) of
		true ->
			T;
		false ->
			[T]
	end,
	%io:format("TableName:~p Feature:~p IndexStart:~p IndexEnd:~p Index:~p Target:~p Output:~p~n",[TableName,Feature,IndexStart,IndexEnd,Index,Target,Output]),
	Fitness = sse(Target,Output,0),
	U_FitnessAcc = FitnessAcc + Fitness,
	case IndexEnd == Index of
		true ->
			put(TableName,{IndexStart,IndexEnd,IndexStart,0}),
			{1,sse(U_FitnessAcc)};
		false ->
			put(TableName,{IndexStart,IndexEnd,db:next(TableName,Index),U_FitnessAcc}),
			{0,U_FitnessAcc}
	end.
	
	sse([T|Target],[O|Output],SSEAcc)->
		SSE = math:pow(T-O,2),
		sse(Target,Output,SSE+SSEAcc);
	sse([],[],SSEAcc)->
		SSEAcc.
	sse(SquaredSummedErrors)->
		1/(SquaredSummedErrors + 0.000001).
	
	rms([T|Target],[O|Output],RMSAcc)->
		SSE = math:pow(T-O,2),
		rms(Target,Output,SSE+RMSAcc);
	rms([],[],RMSAcc)->
		RMSAcc.
	rms(SquaredSummedErrors)->
		1/(math:sqrt(SquaredSummedErrors)+0.000001).


	ape([T|Target],[O|Output],APEAcc)->
		APE = abs(T-O)/abs(T),
		ape(Target,Output,APE+APEAcc);
	ape([],[],APEAcc)->
		APEAcc.
	ape(AveragePercentageError)->
		1/(AveragePercentageError+0.000001).
	
	mae([T|Target],[O|Output],MAEAcc)->
		MAE = abs(T-O),
		mae(Target,Output,MAE+MAEAcc);
	mae([],[],MAEAcc)->
		MAEAcc.
	mae(MeanAverageError)->
		1/(MeanAverageError+0.000001).

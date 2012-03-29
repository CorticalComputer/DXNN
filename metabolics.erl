%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(metabolics).
-compile(export_all).
-include("records.hrl").

%Static::::::
static(plant,Avatar)->
	Avatar;
static(prey,Avatar)->
	Avatar;
static(predator,Avatar)->
	Avatar;
static(flatlander,Avatar)->
	Avatar;
static(rock,Avatar)->
	Avatar.
%Dynamic::::::
%Scape Metabolics: +Energy
%	The total energy falling on the scape is specified by the researcher.
%Plant Metabolics: -/+ Energy, +Mass
%	Plants absorb energy, and become larger, untill they create offspring when their accumulated energy is > X, where X is specified by the researcher.
%	U_Energy = Old_Energy + Energy, 
%	Mass = Energy/3.1415,
%	R = math:sqrt(Mass),
%	When Energy > X, Plant reproduces
%Flatlander Metabolics: -/+ Energy, -Food, +/-Mass
%	Flatlanders eat plants/food, convert it into energy. When energy goes to 0, the Flatlander dies. Food slowly decreases, as it is converted to Energy.
%	U_Energy = Energy - Actions - Upkeep + (Food*0.1)*Effeciency,
%	U_Food = Food - (Food*0.1),
%	Mass = 100 + Energy/10,
%	Action_Magnitude = (Action*Effeciency),
%	Effeciency = gaussian(-0.5 + Age/20000),
%Energy: Energy output in Scape specified by researcher.
%Food:
%
dynamic(plant,Avatar)->
	void;
dynamic(prey,Avatar)->
	done;
dynamic(predator,Avatar)->
	done;
dynamic(flatlander,Avatar)->
	done;
dynamic(rock,Avatar)->
	done.
	
	
metabolics([Avatar|Avatars],Metabolic_Module,Acc)->
%	io:format("Avatar:~p~n",[Avatar]),
	case Avatar#avatar.type of
		plant ->
			case Metabolic_Module of
				static ->
					case Avatar#avatar.state of
						no_respawn->
							case Avatar#avatar.age < 1000 of
								true ->
									U_Avatar=ripen(Avatar),
									metabolics(Avatars,Metabolic_Module,[U_Avatar|Acc]);
								false ->
									case get(visor) of
										undefined->
											done;
										{_Visor_PId,_Canvas} ->
										     [gs:destroy(Id)||{_ObjType,Id,_Color,_Pivot,_Coords,_Parameter}<- Avatar#avatar.objects]
									end,
									metabolics(Avatars,Metabolic_Module,Acc)
							end;
						respawn ->
							%RespawnedAvatar = respawn_avatar(Avatar),
							metabolics(Avatars,Metabolic_Module,[Avatar|Acc])
					end;
				dynamic ->
					void
			end;	
		flatlander ->
			Energy = Avatar#avatar.energy,
			%io:format("Flatlander:~p~n",[Energy]),
			U_Avatar=Avatar#avatar{energy = Energy -0.01},
			metabolics(Avatars,Metabolic_Module,[U_Avatar|Acc]);
		prey ->
			Energy = Avatar#avatar.energy,
			%io:format("Prey:~p~n",[Energy]),
			U_Avatar=Avatar#avatar{energy = Energy -0.01},
			metabolics(Avatars,Metabolic_Module,[U_Avatar|Acc]);
		_ ->
			metabolics(Avatars,Metabolic_Module,[Avatar|Acc])
	end;
metabolics([],_Metabolic_Module,Acc)->
%	io:format("Time_Marches on, Avatar_Count:~p~n",[length(Acc)]),
	Acc.
	
	ripen(Avatar)->
		%io:format("Here~n"),
		Energy = Avatar#avatar.energy,
		Age = Avatar#avatar.age,
		New_Color=case Energy of
			-2000 ->
				black;
			%-500 ->
				%cyan;
			%0 ->
			%	green;
			%500 ->
			%	yellow;
			%0 ->
				%yellow;
			500 ->
				green;
			%1300 ->
				%grey;
			%1500 ->
				%white;
			_ ->
				no_change
		end,
		%io:format("Plant:~p~n",[Energy]),
		case New_Color of
			no_change ->
				%io:format("not ripe:~n"),
				Avatar#avatar{energy = functions:saturation(Energy+2,1000),age = Age+1};
			_ ->
				%io:format("ripe~n"),
				U_Energy = functions:saturation(Energy+2,1000),
			     Avatar#avatar{energy=U_Energy,age = Age+1,objects=[{circle,Id,New_Color,Loc,Coords,R}||{circle,Id,_Color,Loc,Coords,R}<-Avatar#avatar.objects]}
		end.


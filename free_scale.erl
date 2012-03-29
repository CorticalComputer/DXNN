%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(free_scale).
-compile(export_all).
-include("records.hrl").

preferential_link(DX)->
% analyze topology, rank based on inlink/outlink
% Choose randomly an element to connect to, preferential to high frequency inlink/outlink node
	N_Ids = DX#dx.n_ids,
	ConnectionDegreePs = lists:reverse(lists:sort([compute_degree(N_Id) || N_Id <- N_Ids])),
	void.
	

compute_degree(N_Id)->
	[N] = mnesia:dirty_read({neuron,N_Id}),
	Tot_Inports = length(N#neuron.i),
	Tot_Outports = length(N#neuron.o),
	Degree = Tot_Inports+Tot_Outports,
	{Degree,N_Id}.

analyze()->
% Calculate inlink/outlink distribution for the entire current population.
% Calculate the inlink/outlink distribution for the 50% chosen fit subset
% Calculate the inlink/outlink distribution fro the 50% unfit subset
void.

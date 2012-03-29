%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.


-module(committee).
-compile(export_all).
-include("records.hrl").

start()->
	case lists:member(committee,registered()) of
		true ->
			io:format("******** ERROR: Committee already registered, can't start");
		false ->
			init()
	end.

	init()->
		gen(node()).
	
		gen(Node) ->
			PId = spawn(Node,committee,prep,[]),
			register(committee,PId),
			{ok,PId}.

terminate()->
	case lists:member(committee,registered()) of
		true ->
			committee ! terminate;
		false ->
			io:format("******** ERROR: Committee not registered, can't terminate")
	end.
		
restart()->
	terminate(),
	timer:sleep(1000),
	start().		
		
prep()->
	Population_Id = test,
	{A,B,C} = now(),
	random:seed(A,B,C),
	%TopDX_DIds = get_TopDXDIds(Info,Specie_Ids),

	io:format("******** Committee started.~n"),
	committee().

	
committee()->
	done.

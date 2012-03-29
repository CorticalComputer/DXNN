%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(technome_mutator).
-compile(export_all).
-include("records.hrl").

test()->
	F = fun()->
		mutate(dx_test)
	end,
	mnesia:transaction(F).		
	

mutate(DX_Id)->
	F = fun()->
		[DX] = mnesia:read({dx,DX_Id}),
		CX_Id = DX#dx.cx_id,
		[CX] = mnesia:read({cortex,CX_Id}),
		modular_mutator:mutate(DX_Id)
	end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic,_} ->
%			io:format("******** Mutation Succesful.~n"),
			done;
		_->
			io:format("******** Mutation Failure:~p~n",[Result]),
			mutate(DX_Id)
	end.

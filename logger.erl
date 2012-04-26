%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(logger).
-compile(export_all).
-include("forex_db.hrl").
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Logger Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start()->
	PId = spawn(logger,server,[void]),
	register(logger,PId).

stop()->
	logger ! {self(),stop}.

server(void)->
	receive 
		{_From, start_logger}->
			logger:server(high);
		{_From, start_logger,Log_Type}->
			logger:server(Log_Type);
		{_From, stop}->
			io:format("Logger is shutting down normally.~n");
		MSG ->
			io:format("MSG:~p~n",[MSG]),
			logger:server(void)
	end;
server(Log_Type)->
	receive 
		{log_info,Info}->
			display_info(Log_Type,Info),
			logger:server(Log_Type);
		{new_LT,New_LogType}->
			logger:server(New_LogType)
	end.

	display_info(Log_Type,Info)->
		case Log_Type of
			all ->
				io:format("***** Loging Type:~p *****~n",[Log_Type]),
				io:format("Logger Info:~p~n",[Info]);
			none ->
				done
		end.

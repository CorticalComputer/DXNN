%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(forex_db).
-compile(export_all).
-include("forex_db.hrl").

test(FXTable,Feature)->
	Start = forex_db:lookup_element(metadata,FXTable,start),
	Stop = forex_db:lookup_element(metadata,FXTable,stop),
	buy_profit(FXTable,Feature,Start,Stop),
	random_profit(FXTable,Feature,Start,Stop),
	max_profit(FXTable,Feature,Start,Stop),
	counter(FXTable,Start,Stop,0).
	
max(FXTable,Feature,StartKey,EndKey)->
	mp(FXTable,Feature,StartKey,EndKey,0).
		
	counter(_FXTable,EndKey,EndKey,Counter)->
		Counter+1;
	counter(FXTable,Key,EndKey,Counter)->
		counter(FXTable,next(FXTable,Key),EndKey,Counter+1).
	
	buy_profit(FXTable,Feature,Start,Stop)->
		BP = bp(FXTable,Feature,Start,Stop,0),
		io:format("BuyProfit:~p in Table:~p Feature:~p~n",[BP,FXTable,map(FXTable,Feature)]).
			
		bp(FXTable,Feature,EndKey,EndKey,Acc)->
			put(prev_state,undefined),
			Acc;
		bp(FXTable,Feature,Key,EndKey,Acc)->
			Val = lookup_element(FXTable,Key,Feature),
			%NextVal = lookup_element(FXTable,next(FXTable,Key),Feature),
			Action = 1,
			Profit = trader(Val,Action),
			bp(FXTable,Feature,next(FXTable,Key),EndKey,Profit+Acc).
		
	random_profit(FXTable,Feature,Start,Stop)->
		RP = rp(FXTable,Feature,Start,Stop,0),
		io:format("RandomProfit:~p in Table:~p Feature:~p~n",[RP,FXTable,map(FXTable,Feature)]).
		
		rp(FXTable,Feature,EndKey,EndKey,Acc)->
			put(prev_state,undefined),
			Acc;
		rp(FXTable,Feature,Key,EndKey,Acc)->
			Val = lookup_element(FXTable,Key,Feature),
			%NextVal = lookup_element(FXTable,next(FXTable,Key),Feature),
			Action = case (random:uniform() - 0.5) > 0 of
				true -> 1;
				false -> -1
			end,
			Profit = trader(Val,Action),
			rp(FXTable,Feature,next(FXTable,Key),EndKey,Profit+Acc).
		
	max_profit(FXTable,Feature,Start,Stop)->
		MP = mp(FXTable,Feature,Start,Stop,0),
		io:format("MaxProfit:~p in Table:~p Feature:~p~n",[MP,FXTable,map(FXTable,Feature)]).

		mp(FXTable,Feature,EndKey,EndKey,Acc)->
			put(prev_state,undefined),
			Acc;
		mp(FXTable,Feature,Key,EndKey,Acc)->
			Val = lookup_element(FXTable,Key,Feature),
			%NextVal = lookup_element(FXTable,next(FXTable,Key),Feature),
			Action = max,
			Profit = trader(Val,Action),
			mp(FXTable,Feature,next(FXTable,Key),EndKey,Profit+Acc).
			
			trader(RawClose,AE)->
				case get(prev_state) of
					undefined ->
						put(prev_state,{AE,RawClose}),
						0;
					{Prev_AE,Prev_RawClose} ->
						put(prev_state,{AE,RawClose}),
						LotSize = 10000,
						PriceDiff = (RawClose - Prev_RawClose)*LotSize,
						SpreadFee = 0,
						case AE of
							max ->
								Profit = abs(PriceDiff),
								case Profit < SpreadFee of
									true ->
										0;
									false ->
										Profit - SpreadFee
								end;
							_ ->
								case (Prev_AE < 0.1) and (Prev_AE > -0.1) of
									true ->
										0;
									false ->
										Prev_AE*PriceDiff - SpreadFee
								end
						end
				end.

		trader(Val,NextVal,Action)->
			LotSize = 10000,
			PriceDiff = (NextVal-Val)*LotSize,
			SpreadFee = 0,
			case Action of
				max ->
					Profit = abs(PriceDiff),
					case Profit < SpreadFee of
						true ->
							0;
						false ->
							Profit - SpreadFee
					end;
				_ ->
					case (Action < 0.1) and (Action > -0.1) of
						true ->
							0;
						false ->
							Action*PriceDiff - SpreadFee
					end
			end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Init %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init()->
	Standard_TableTuples = init_StandardTables(),
	Forex_TableTuples = init_ForexTables(),
	insert_TestData(),
	AllTableTuples = lists:append(Standard_TableTuples,Forex_TableTuples),
	[ets:tab2file(TableId,"forex_db/"++atom_to_list(TableName)) || {TableName,TableId} <- AllTableTuples],
	[ets:delete(TableId) || {_TableName,TableId}<-AllTableTuples].
	
	init_StandardTables()->
		TableTuples1 = [{TableName,ets:new(TableName,[set,public,named_table])} || TableName <-[map,metadata]],
		memorize(metadata,?METADATA,1),
		memorize(test,?TEST,1),
		TableTuples2 = [{TableName,create_table(TableName,[ordered_set,public,named_table])} || TableName <-[test]],
		lists:append(TableTuples1,TableTuples2).
	
		memorize(TableName,[Feature|FeatureList],Pos)->
			ets:insert(map,{{TableName,Feature},Pos}),
			ets:insert(map,{{TableName,Pos},Feature}),
			memorize(TableName,FeatureList,Pos+1);
		memorize(_TableName,[],_Pos)->
			done.
		
	init_ForexTables()->
		TableTuples1 = [{TName,ets:new(TName,[ordered_set,public,named_table])} || TName <- [forex_metadata]],
		TableTuples2 = [{TName,create_table(TName,[ordered_set,public,named_table])} || TName <- [fx1,fx5,fx15,fx30,fx60,fx240,fx1440]],
		ForexFeatures = [key]++[{CPair,FX_Technical} || CPair <- ?CPAIRS, FX_Technical <-?FOREX_TECHNICAL--[key]],
		%io:format("Forex Features:~p~n",[ForexFeatures]),
		memorize(forex_metadata,?FOREX_METADATA,1),
		[memorize(FX_Table,ForexFeatures,1) || FX_Table <- ?FX_TABLES --[forex_metadata]],
		prep_ForexMetaData(),
		lists:append(TableTuples1,TableTuples2).
	
		prep_ForexMetaData()->
			[insert(forex_metadata,{FX_Table,CPair,Feature},[{first,undefined},{last,undefined},{avg,undefined},{dev,undefined},{stdev,undefined},{quantile25,undefined},{quantile50,undefined},{quantile75,undefined},{max,undefined},{min,undefined},{operators_applied,[]}]) ||
			FX_Table <- ?FX_TABLES--[forex_metadata],CPair <- ?CPAIRS, Feature <- ?FOREX_TECHNICAL--[key]].
			
		create_table(TableName,Options)->
			TableId = ets:new(TableName,Options),
			insert(metadata,TableName,[{size,0},{last_updated,now()},{start,0},{stop,0}]),
			TableId.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DB Commands %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start()->
	TableNames = ?ALL_TABLES,
	_TableTuples = summon_tables(TableNames,[]),
	io:format("******** database started~n").
	
	summon_tables([TableName|TableNames],TableTupleAcc)->
		case ets:file2tab("forex_db/"++atom_to_list(TableName)) of
			{ok,TableId} ->
				summon_tables(TableNames,[{TableName,TableId}|TableTupleAcc]);
			{error,Reason}->
				exit(Reason)
		end;
	summon_tables([],TableTupleAcc)->
		TableTupleAcc.

heartbeat()->
	register(heartbeat,spawn(forex_db,heartbeat,[5000])),
	io:format("******** heartbeat started~n").
		
	heartbeat(Time)->
		receive
			{new_time,NewTime}->
				forex_db:heartbeat(NewTime);
			terminate ->
				io:format("******** heartbeat terminated~n")
		after Time ->
			forex_db:updater(),
			forex_db:heartbeat(Time)
		end.

		updater()->
			%io:format("Updating.~n"),
			forex_db:insert_ForexRaw("/home/puter/.wine/dosdevices/c:/Program Files/MetaTrader - Alpari (US)/experts/files/EURUSD15.txt",update).

stop()->
	backup(),
	terminate(),
	io:format("******** database stopped~n").

backup()->
	TableNames = ?ALL_TABLES,
	backup(TableNames,[]).
	
	backup([TableName|TableNames],ErrAcc)->	
		try first(TableName) of
			_->
				ets:tab2file(TableName,"forex_db/"++atom_to_list(TableName)),
				backup(TableNames,ErrAcc)
		catch 
			_:Why ->
				io:format("******** FOREX_DB backup of table:~p faled due to:~p~n",[TableName,Why]),
				backup(TableNames,[TableName|ErrAcc])
		end;
	backup([],ErrAcc)->
		case ErrAcc of
			[] ->
				io:format("******** All tables within FOREX_DB have been backed up~n");
			_ ->
				io:format("******** The following tables within FOREX_DB could not be backed up:~n~p~n",[ErrAcc])
		end.

terminate()->
	TableNames = ?ALL_TABLES,
	[ets:delete(TableName) || TableName<-TableNames],
	io:format("******** database terminated~n").
	
delete()->
	TableNames = ?ALL_TABLES,
	[file:delete("forex_db/"++atom_to_list(TableName)) || TableName <- TableNames].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Table Commands %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prep(TableName,Key)->
	[key|FeatureList] = featurelist_selector(TableName),
	ets:insert(TableName,list_to_tuple([Key|FeatureList])).

	featurelist_selector(TableName)->
		case TableName of
			metadata ->
				?METADATA;
			test ->
				?TEST;
			forex_metadata->
				?FOREX_METADATA;
			_ ->
				ForexFeatures = [key]++[0 || _CPair <- ?CPAIRS, _FX_Technical <- ?FOREX_TECHNICAL -- [key]],
				ForexFeatures
		end.
	
lookup(TableName,Key)->
	ets:lookup(TableName,Key).
	
insert(TableName,Key,TupleList)->
	case is_list(TupleList) of
		true ->
			case ets:member(TableName,Key) of
				true ->
					update_element(TableName,Key,TupleList);
				false ->
					prep(TableName,Key),
					update_element(TableName,Key,TupleList)
			end;
		false ->
			exit("******** In insert(TableName,Key,TupleList), TupleList =/= list")
	end.

update_element(TableName,Key,TupleList)->
	update_element(TableName,Key,TupleList,[]).
update_element(TableName,Key,[{Feature,Val}|TupleList],FormatedAcc)->
	ElemPos = map(TableName,Feature),
	%io:format("ElemPos:~p~n",[{Feature,ElemPos}]),
	update_element(TableName,Key,TupleList,[{ElemPos,Val}|FormatedAcc]);
update_element(TableName,Key,[],FormatedAcc)->
	ets:update_element(TableName,Key,FormatedAcc).

tot_ftf()->
	%io:format("~p~n",[?FOREX_TECHNICAL]),
	%io:format("CFTAGS:~p~n",[?CFTAGS]),
	%io:format("CTTAGS:~p~n",[?CTTAGS]),
	%io:format("Length:~p~n",[length(?FOREX_TECHNICAL)]),
	((length(?FOREX_TECHNICAL)-1) * length(?CPAIRS)) + 1.
	
cttags()->
	?CTTAGS.
	
lookup_element(TableName,Key,Feature)->
	case is_forex(TableName) of
		true -> 
			ElemPos = map(TableName,Feature),
			TotFeatures =tot_ftf(), %%%without the key
			case ElemPos =< TotFeatures of
				true ->
					ets:lookup_element(TableName,Key,ElemPos);
				false ->
					TimeBack = trunc(ElemPos/(TotFeatures+0.1)),
					TrueElemPos = ElemPos - TimeBack*TotFeatures,
					PrevTimeKey = index2key(TableName,Key,prev,TimeBack),
					%io:format("TimeBack:~p TrueElemPos:~p Feature:~p ElemPos:~p, TotFeatures:~p~n",[TimeBack,TrueElemPos,Feature,ElemPos,TotFeatures]),
					ets:lookup_element(TableName,PrevTimeKey,TrueElemPos)
			end;
		false ->
			ElemPos = map(TableName,Feature),
			ets:lookup_element(TableName,Key,ElemPos)
	end.

lookup_longvector(TableName,Key,Feature,Length)->
	case is_forex(TableName) of
		true -> 
			ElemPos = map(TableName,Feature),
			LastKey = index2key(TableName,Key,prev,Length),
			construct_longvector(TableName,Key,prev,LastKey,Feature,[]);
		false ->
			ElemPos = map(TableName,Feature),
			ets:lookup_element(TableName,Key,ElemPos)
	end.
	
		construct_longvector(TableName,EndKey,_Direction,EndKey,Feature,Acc)->
			Val = ets:lookup_element(TableName,EndKey,Feature),
			[Val|Acc];
		construct_longvector(TableName,Key,Direction,EndKey,Feature,Acc)->
			Val = ets:lookup_element(TableName,Key,Feature),
			construct_longvector(TableName,ets:Direction(TableName,Key),Direction,EndKey,Feature,[Val|Acc]).

	is_forex(TableName) ->
		case TableName of
			fx1 -> true;
			fx5 -> true;
			fx15 ->true;
			fx30 ->true;
			fx60 ->true;
			fx240 ->true;
			fx1440 -> true;
			_ -> false
		end.
		
first(TableName)->
	ets:first(TableName).
	
last(TableName)->
	ets:last(TableName).
	
next(TableName,Key)->
	ets:next(TableName,Key).
	
prev(TableName,Key)->
	ets:prev(TableName,Key).
	
member(TableName,Key)->
	ets:member(TableName,Key).

map(TableName,Feature)->
	case is_integer(Feature) of
		true ->
			Feature;
		false ->
			ets:lookup_element(map,{TableName,Feature},2)
	end.
	
rmap(TableName,ElemPos)->
	case is_integer(ElemPos) of
		true ->
			TotFeatures =tot_ftf(), %%%without the key
			case ElemPos =< TotFeatures of
				true ->
					ets:lookup_element(map,{TableName,ElemPos},2);
				false ->
					TimeBack = trunc(ElemPos/(TotFeatures+0.1)),
					TrueElemPos = ElemPos - TimeBack*TotFeatures,
					ets:lookup_element(map,{TableName,TrueElemPos},2)
			end;
		false ->
			ElemPos
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Insert Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%=============================== Test Data Insertion =====================================
%Description: -record(test,{key,q,a}).%%%key=Index}
%Input: textfile/cvsfile
insert_TestData()->
	insert(test,1,[{q,[1,1]},{a,-1}]),
	insert(test,2,[{q,[-1,-1]},{a,-1}]),
	insert(test,3,[{q,[1,-1]},{a,1}]),
	insert(test,4,[{q,[-1,1]},{a,1}]),
	insert(metadata,test,[{size,4},{last_updated,now()},{start,1},{stop,4}]).

metadata_category(TableNames)->
	[FirstTable|_] = TableNames,
	case lists:sum([1|| TableName <- TableNames, TableName == FirstTable]) == length(TableNames) of
		true ->
			all_tables_the_same;
		false ->
			exit("******** MetaDataCategory ERROR: not all tables the same in the list")
	end,
	FirstTable.

%%=============================== Forex Data Insertion =====================================
%Description: -record(forex_raw,{key,open,high,low,close,volume}). %%%key={currency_pair,Year,Month,Day,Hour,Minute,Second}
%Input: textfile/cvsfile
%2009.05.15,00:00,0.88880,0.89060,0.88880,0.88950,362 :: date/time/open/high/low/close/volume
%URL= ???/????/????/File  File= FileName.FileExtension, FileName= [CPair][TimeFrame]
insert_ForexRaw(URL,Flag)->%TODO: instead of file have files, and then after the total insertion, do a data clean and then the updates.
	{Dir,File} = extract_dir(URL),
	{FileName,_FileExtension} = extract_filename(File),
	{CPair,TimeFrame} = extract_cpair(FileName), 
	TableName = get_FXTable(TimeFrame),
	case lookup(metadata,TableName) of
		[{_TableName,_Size,_Last_Updated,_Start,_Stop}] ->
			%io:format("CPair:~p populated:~p, adding data...~n",[CPair,{TableName,Size,Last_Updated,Start,Stop}]),
			List = case file:read_file(URL) of
					{ok,Data} ->
						binary_to_list(Data);
					{error,Error} ->
						io:format("******** Error:~p~n",[Error]),
						exit(Error)
			end,
			case Flag of
				init ->
					init_ForexDB(TableName,CPair,List),
					calculate_MetaData(TableName),
					calculate_ForexTechnicals(TableName,CPair,first(TableName)),
					calculate_ForexMetaData(TableName,CPair,?FOREX_TECHNICAL--[key]),
					done;
				update ->
					case update_ForexDB(TableName,CPair,List) of
						undefined ->
							done;
						NewestKey ->
							io:format("New FOREX_DB update starting with:~p~n",[NewestKey]),
							calculate_MetaData(TableName),
							calculate_ForexTechnicals(TableName,CPair,NewestKey),
							%calculate_ForexMetaData(TableName,CPair,?FOREX_TECHNICAL--[key]),
							committee:si_sender(NewestKey),
							done
					end
			end;
		[] ->
			io:format("******** CurrencyPair:~p is unknown, file rejected.~n",[CPair])
	end.
	
	extract_dir(URL)-> extract_dir(URL,[]).
	extract_dir(List,DirAcc)->
		case split_with(47,List) of % 47 == '/'
			{File,[]}->
				Dir = lists:concat(DirAcc),
				{Dir,File};
			{DirPart,Remainder}->
				extract_dir(Remainder,lists:merge([DirPart,'/'],DirAcc))
		end.
	
	extract_filename(File)->
		split_with(46,File,[]).		% . 46
			
	extract_cpair(FileName)->
		lists:split(6,FileName).
	
	get_FXTable(TimeFrame)->
		case TimeFrame of
			"1" -> fx1;
			"5" -> fx5;
			"15" -> fx15;
			"30" -> fx30;
			"60" -> fx60;
			"240" -> fx240;
			"1440" -> fx1440
		end.
		
	init_ForexDB(TableName,CPair,[])->
		io:format("Parsing and inserting Forex Currency-Pair:~p complete.~n",[{TableName,CPair}]);
	init_ForexDB(TableName,CPair,List)->
		{YearL,Remainder1} = split_with(46,List),		% . 46
		{MonthL,Remainder2} = split_with(46,Remainder1),	% . 46
		{DayL,Remainder3} = split_with(44,Remainder2),		% , 44
		{HourL,Remainder4} = split_with(58,Remainder3),		% : 58
		{MinuteL,Remainder5} = split_with(44,Remainder4),	% , 44
		{OpenL,Remainder6} = split_with(44,Remainder5),		% , 44
		{HighL,Remainder7} = split_with(44,Remainder6),		% , 44
		{LowL,Remainder8} = split_with(44,Remainder7),		% , 44
		{CloseL,Remainder9} = split_with(44,Remainder8),	% , 44
		{VolumeL,Remainder10} = split_with(13,Remainder9),	%\r 13
		[_|Remainder] = Remainder10, %gets rid of (\n 10)
		%[YearL,46,MonthL,46,DayL,44,HourL,58,MinuteL,44,OpenL,44,HighL,44,LowL,44,CloseL,44,VolumeL,_] = List,
		%io:format("here~p~n",[{YearL,MonthL,DayL,HourL,MinuteL,OpenL,HighL,LowL,CloseL,VolumeL}]),
		Year = list_to_integer(YearL),
		Month = list_to_integer(MonthL),
		Day = list_to_integer(DayL),
		Hour = list_to_integer(HourL),
		Minute = list_to_integer(MinuteL),
		Open = list_to_number(OpenL),
		High = list_to_number(HighL),
		Low = list_to_number(LowL),
		Close = list_to_number(CloseL),
		Volume = list_to_integer(VolumeL),
		Key = {Year,Month,Day,Hour,Minute,0},
		case ((Open+High+Low+Close) < 1000) and ((Open+High+Low+Close) > -1000) of
			true ->
				insert(TableName,Key,[{{CPair,open},Open},{{CPair,high},High},{{CPair,low},Low},{{CPair,close},Close},{{CPair,volume},Volume}]);
			false ->
				exit("******** ERROR during initial FX data insertion, exiting.")
		end,
		init_ForexDB(TableName,CPair,Remainder).

	update_ForexDB(_TableName,_CPair,[])->
		Key = get(new_key),
		put(new_key,undefined),
		Key;
	update_ForexDB(TableName,CPair,List)->
		{YearL,Remainder1} = split_with(46,List),		% . 46
		{MonthL,Remainder2} = split_with(46,Remainder1),	% . 46
		{DayL,Remainder3} = split_with(44,Remainder2),		% , 44
		{HourL,Remainder4} = split_with(58,Remainder3),		% : 58
		{MinuteL,Remainder5} = split_with(58,Remainder4),	% : 44
		{SecondL,Remainder6} = split_with(44,Remainder5),	% , 44
		{OpenL,Remainder7} = split_with(44,Remainder6),		% , 44
		{HighL,Remainder8} = split_with(44,Remainder7),		% , 44
		{LowL,Remainder9} = split_with(44,Remainder8),		% , 44
		{CloseL,Remainder10} = split_with(44,Remainder9),	% , 44
		{VolumeL,Remainder11} = split_with(13,Remainder10),	%\r 13
		[_|Remainder] = Remainder11,				%gets rid of (\n 10)
		%io:format("here~p~n",[{YearL,MonthL,DayL,HourL,MinuteL,OpenL,HighL,LowL,CloseL,VolumeL}]),
		Year = list_to_integer(YearL),
		Month = list_to_integer(MonthL),
		Day = list_to_integer(DayL),
		Hour = list_to_integer(HourL),
		Minute = list_to_integer(MinuteL),
		Second = list_to_integer(SecondL),
		Open = list_to_number(OpenL),
		High = list_to_number(HighL),
		Low = list_to_number(LowL),
		Close = list_to_number(CloseL),
		Volume = list_to_integer(VolumeL),
		Key = {Year,Month,Day,Hour,Minute,0},
		case (Second == 0) and ((Open+High+Low+Close) < 1000) and ((Open+High+Low+Close) > -1000) of
			true ->
				case member(TableName,Key) of
					false ->
					    insert(TableName,Key,[{{CPair,open},Open},{{CPair,high},High},{{CPair,low},Low},{{CPair,close},Close},{{CPair,volume},Volume}]),
						case get(new_key) of
							undefined ->
								put(new_key,Key);
							_ ->
								done
						end;
					true ->
						%io:format("******** ERROR during FX data insertion.~n"),
						done
				end;
			false ->
				done
		end,			
		update_ForexDB(TableName,CPair,Remainder).
		
		split_with(Seperator,List)->
			split_with(Seperator,List,[]).
		
			split_with(Seperator,[Char|List],ValAcc)->
				case Char of
					Seperator->
						{lists:reverse(ValAcc),List};
					_ ->
						split_with(Seperator,List,[Char|ValAcc])
				end;
			split_with(_Seperator,[],ValAcc)->
				{lists:reverse(ValAcc),[]}.

	calculate_MetaData(TableName)->
		USize = table_size(TableName),
		Now = now(),
		%Seperator = trunc(USize*0.10),
		Start = index2key(TableName,last(TableName),prev,10000),
		Stop = last(TableName),
		update_element(metadata,TableName,[{size,USize},{last_updated,Now},{start,Start},{stop,Stop}]).

	calculate_ForexTechnicals(TableName,CPair,StartKey)->
		diff(TableName,{CPair,close},TableName,{CPair,diff},StartKey),
		%updiff_downdiff(TableName,{CPair,diff},TableName,{CPair,up_diff},{CPair,down_diff},StartKey),
		%ema(TableName,{CPair,up_diff},TableName,{CPair,ud_ema27},27,StartKey),
		%ema(TableName,{CPair,down_diff},TableName,{CPair,dd_ema27},27,StartKey),		
		%ema(TableName,{CPair,close},TableName,{CPair,ema2},2,StartKey),			
		%ema(TableName,{CPair,close},TableName,{CPair,ema3},3,StartKey),			
		ema(TableName,{CPair,close},TableName,{CPair,ema6},6,StartKey),		
		%ema(TableName,{CPair,close},TableName,{CPair,ema9},9,StartKey),		
		ema(TableName,{CPair,close},TableName,{CPair,ema14},14,StartKey),			
		ema(TableName,{CPair,close},TableName,{CPair,ema26},26,StartKey),			
		ema(TableName,{CPair,close},TableName,{CPair,ema50},50,StartKey),			
		%ema(TableName,{CPair,close},TableName,{CPair,ema100},100,StartKey),	
		%sma(TableName,{CPair,close},TableName,{CPair,sma2},2,StartKey),			
		%sma(TableName,{CPair,close},TableName,{CPair,sma3},3,StartKey),			
		%sma(TableName,{CPair,close},TableName,{CPair,sma6},6,StartKey),		
		%sma(TableName,{CPair,close},TableName,{CPair,sma9},9,StartKey),		
		%sma(TableName,{CPair,close},TableName,{CPair,sma14},14,StartKey),			
		%sma(TableName,{CPair,close},TableName,{CPair,sma26},26,StartKey),			
		%sma(TableName,{CPair,close},TableName,{CPair,sma50},50,StartKey),			
		%sma(TableName,{CPair,close},TableName,{CPair,sma100},100,StartKey),	
		%rsi(TableName,{CPair,ud_ema27},{CPair,dd_ema27},TableName,{CPair,rsi},StartKey),
		%macd(TableName,{CPair,ema14},{CPair,ema26},TableName,{CPair,macd},StartKey),
		%ema(TableName,{CPair,macd},TableName,{CPair,macd_signal},9,StartKey),
		%adi(TableName,{CPair,high},{CPair,low},{CPair,close},{CPair,volume},TableName,{CPair,adi},StartKey),
		%sts(TableName,{CPair,high},{CPair,low},{CPair,close},TableName,{CPair,sts_kfast},{CPair,sts_dfast},{CPair,sts_dslow},{CPair,sts_dfull},StartKey),
		%ema(TableName,{CPair,ema9},TableName,{CPair,bix9},9,StartKey),
		%ema(TableName,{CPair,bix9},TableName,{CPair,trix9},9,StartKey),
		done.
	
	calculate_ForexMetaData(TableName,CPair,[Feature|FeatureList])->
		First = first(TableName),
		Last = last(TableName),
		SortedList = lists:sort(to_list(TableName,First,Last,{CPair,Feature},[])),
		Avg = average(TableName,{CPair,Feature}),
		StDev = standard_deviation(TableName,First,next,Last,{CPair,Feature},Avg,1,0),
		Dev = StDev*StDev,
		Q25 = quantile(TableName,SortedList,0.25),
		Q50 = quantile(TableName,SortedList,0.50),
		Q75 = quantile(TableName,SortedList,0.75),
		Max = max(TableName,{CPair,Feature},index2key(TableName,last(TableName),prev,5000)),
		Min = min(TableName,{CPair,Feature},index2key(TableName,last(TableName),prev,5000)),
		update_element(forex_metadata,{TableName,CPair,Feature},[{first,First},{last,Last},{avg,Avg},{dev,Dev},{stdev,StDev},{quantile25,Q25},{quantile50,Q50},{quantile75,Q75},{max,Max},{min,Min}]),
		calculate_ForexMetaData(TableName,CPair,FeatureList);
	calculate_ForexMetaData(_TableName,_CPair,[])->
		done.

		to_list(TableName,Key,EndKey,Feature,Acc)->
			E = lookup_element(TableName,Key,Feature),
			Element = case is_list(E) of
				true ->
					E;
				false ->
					[E]
			end,
			case Key == EndKey of
				false ->
					to_list(TableName,next(TableName,Key),EndKey,Feature,lists:append(Element,Acc));
				true ->
					lists:append(Element,Acc)
			end.
			
		quantile(TableName,SortedList,Quantile)->
			Size = table_size(TableName),
			lists:nth(trunc(Size*Quantile),SortedList).
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DataAnalysis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%===============================Find Start Index=====================================
%Calculates: The index number Pos elements in the direction Direction, and returns that Index.
%Input: TableName::ets_table_name, Index::val, Direction::next|prev, Pos::int
%Output: Index::val
index2key(TableName,Index)->
	index2key(TableName,first(TableName),next,Index).

	index2key(TableName,'$end_of_table',_Direction,_Pos)->
		io:format("******** get_Index error: $end_of_table reached before proper key was reached,returning forex_db:last(TableName).~n"),
		last(TableName);
	index2key(_TableName,Key,_Direction,1)->
		Key;
	index2key(TableName,Key,Direction,Index)->
		index2key(TableName,forex_db:Direction(TableName,Key),Direction,Index-1).

%%===============================List Average=====================================
%Calculates: The average of the list: SUMi-n(Ei)/n
%Input: List::list
%Output: List_Average::float
list_Average(List)-> %Simple Average.
	lists:sum(List)/lists:length(List).
	
%%===============================Table Size=====================================
%Gets: The total number of elements in the table.	
%Input: TableName::ets_table_name
%Output: TableSize::int
table_size(TableName)->
	[_,_,_,{size,Size},_,_,_,_,_] = ets:info(TableName),
	Size.	

%%===============================Table Largest Value=====================================
%Gets: The largest value in the table in tupple's feature: Feature
%Input: TableName::ets_table_name, StartIndex:val, Direction::next|prev, EndIndex:val, ElemPos::int
%Output: LargestVal:float|int
max(TableName,Feature)-> 
	StartKey = first(TableName),
	EndKey = '$end_of_table',
	max(TableName,StartKey,next,EndKey,Feature).

max(TableName,Feature,StartKey)-> 
	EndKey = '$end_of_table',
	max(TableName,StartKey,next,EndKey,Feature).
	
	max(TableName,StartKey,Direction,EndKey,Feature)->
		StartVal = lookup_element(TableName,StartKey,Feature),
		max(TableName,StartKey,Direction,EndKey,Feature,StartVal).
		
	max(_TableName,EndKey,_Direction,EndKey,_Feature,Largest)->
		Largest;
	max(TableName,Key,Direction,EndKey,Feature,Largest)->
		Val = lookup_element(TableName,Key,Feature),
		case Val > Largest of
			true ->
				max(TableName,forex_db:Direction(TableName,Key),Direction,EndKey,Feature,Val);
			false ->
				max(TableName,forex_db:Direction(TableName,Key),Direction,EndKey,Feature,Largest)
		end.
				
%%===============================Table Smallest Value=====================================
%Calculates: The smallest value in the table in tupple's position: ElemPos
%Input: TableName::ets_table_name, StartIndex:val, Direction::next|prev, EndIndex:val, ElemPos::int
%Output: LargestVal:float|int
min(TableName,Feature)-> 
	StartKey = first(TableName),
	EndKey = '$end_of_table',
	min(TableName,StartKey,next,EndKey,Feature).
	
min(TableName,Feature,StartKey)-> 
	EndKey = '$end_of_table',
	min(TableName,StartKey,next,EndKey,Feature).
		
	min(TableName,StartKey,Direction,EndKey,Feature)->
		StartVal = lookup_element(TableName,StartKey,Feature),
		min(TableName,StartKey,Direction,EndKey,Feature,StartVal).
		
	min(_TableName,EndKey,_Direction,EndKey,_Feature,Smallest)->
		Smallest;
	min(TableName,Key,Direction,EndKey,Feature,Smallest)->
		Val = lookup_element(TableName,Key,Feature),
		case Val < Smallest of
			true ->
				min(TableName,forex_db:Direction(TableName,Key),Direction,EndKey,Feature,Val);
			false ->
				min(TableName,forex_db:Direction(TableName,Key),Direction,EndKey,Feature,Smallest)
		end.

%%===============================Table Average=====================================
%Calculates: The average of the table: SUMi-n(Ei)/n for the element at position: ElemPos
%Input: TableName::ets_table_name, StartIndex::val, Direction::next|prev, EndIndex::val, ElemPos::int
%Output: Average::float
average(TableName,Feature)->
	StartKey = first(TableName),
	EndKey =  last(TableName),
	average(TableName,StartKey,EndKey,Feature).

average(TableName,Feature,StartKey)->
	EndKey =  last(TableName),
	average(TableName,StartKey,EndKey,Feature).

	average(TableName,StartKey,EndKey,Feature)->
		average(TableName,StartKey,EndKey,Feature,0,0).
		
	average(TableName,EndKey,EndKey,Feature,ElemCounter,AvgAcc)->
		case EndKey == '$end_of_table' of
			true ->
				AvgAcc/ElemCounter;
			false ->
				Val = lookup_element(TableName,EndKey,Feature),
				(Val+AvgAcc)/(ElemCounter+1)
		end;
	average(TableName,Key,EndKey,Feature,ElemCounter,AvgAcc)->
		Val = lookup_element(TableName,Key,Feature),
		average(TableName,next(TableName,Key),EndKey,Feature,ElemCounter+1,Val+AvgAcc).

%%===============================Table Standard Deviation=====================================
%Calculates: The standard deviation of the table: Sqrt(SUMi-n(pow(Ei-TabAvg,2))) for the element at position: ElemPos
%Input: TableName::ets_table_name, StartIndex::val, Direction::next|prev, EndIndex::val, ElemPos::int
%Output: StandardDeviation::float
standard_deviation(TableName,Feature)->
	FirstKey = first(TableName),
	EndKey = '$end_of_table',
	standard_deviation(TableName,FirstKey,next,EndKey,Feature).

	standard_deviation(TableName,StartKey,Direction,EndKey,Feature)->
		Average = average(TableName,StartKey,EndKey,Feature),
		standard_deviation(TableName,StartKey,Direction,EndKey,Feature,Average,0,0).

	standard_deviation(_TableName,EndKey,_Direction,EndKey,_Feature,_Avg,ElemCounter,SDAcc)->
		math:sqrt(SDAcc/ElemCounter);
	standard_deviation(TableName,Key,Direction,EndKey,Feature,Avg,ElemCounter,SDAcc)->
		Val = lookup_element(TableName,Key,Feature),
		standard_deviation(TableName,forex_db:Direction(TableName,Key),Direction,EndKey,Feature,Avg,ElemCounter+1,math:pow(Val-Avg,2)+SDAcc).
			
%%===============================Table Normalizer=====================================
%Calcualtes: The normalizer of the table: sqrt(SUMi-n(Ei*Ei))
%Input: TableName::ets_table_name, ElemPos::int
%Output: Normalizer::float
calculate_normalizer(TableName,Feature)->%TODO: allow for normalizer to be stored in metadata for consistant retrevial values.
	StartKey = first(TableName),
	EndKey = '$end_of_table',
	calculate_normalizer(TableName,StartKey,next,EndKey,Feature,0).

	calculate_normalizer(TableName,StartIndex,Direction,EndIndex,Feature)->
		calculate_normalizer(TableName,StartIndex,Direction,EndIndex,Feature,0).


	calculate_normalizer(_TableName,EndIndex,_Direction,EndIndex,_Feature,NormalizerAcc)->
		math:sqrt(NormalizerAcc);
	calculate_normalizer(TableName,Index,Direction,EndIndex,Feature,NormalizerAcc)->
		Val = lookup_element(TableName,Index,Feature),
		calculate_normalizer(TableName,forex_db:Direction(TableName,Index),Direction,EndIndex,Feature,math:pow(Val,2)+NormalizerAcc).
			
%%===============================Diff=====================================
%Calculates:
%Output:
diff(FromTableName,FromFeature,ToTableName,ToFeature)->
	StartKey = next(FromTableName,first(FromTableName)),
	EndKey = '$end_of_table',
	diff(FromTableName,FromFeature,ToTableName,ToFeature,StartKey,EndKey).
	
diff(FromTableName,FromFeature,ToTableName,ToFeature,StartKey)->
	EndKey = '$end_of_table',
	diff(FromTableName,FromFeature,ToTableName,ToFeature,StartKey,EndKey).	

	diff(_FromTableName,_FromFeature,_ToTableName,_ToFeature,EndKey,EndKey)->
		%io:format("Diff complete: FTN:~p FF:~p TTN:~p TF:~p~n",[FromTableName,FromFeature,ToTableName,ToFeature]),
		done;
	diff(FromTableName,FromFeature,ToTableName,ToFeature,Key,EndKey)->
		case prev(FromTableName,Key) of
			'$end_of_table' ->
				update_element(ToTableName,Key,[{ToFeature,0}]);
			PrevKey ->
				CurrVal = lookup_element(FromTableName,Key,FromFeature),
				PrevVal = lookup_element(FromTableName,PrevKey,FromFeature),
				Diff = CurrVal - PrevVal,
				update_element(ToTableName,Key,[{ToFeature,Diff}])
		end,
		diff(FromTableName,FromFeature,ToTableName,ToFeature,next(FromTableName,Key),EndKey).

%%===============================Up Diff & Down Diff=====================================
%Calculates:
%Output:
updiff_downdiff(FromTableName,FromDiff,ToTableName,ToUpDiff,ToDownDiff)->
	FirstKey = first(FromTableName),
	EndKey = '$end_of_table',
	updiff_downdiff(FromTableName,FromDiff,ToTableName,ToUpDiff,ToDownDiff,FirstKey,EndKey).
	
updiff_downdiff(FromTableName,FromDiff,ToTableName,ToUpDiff,ToDownDiff,Key)->
	FirstKey = Key,
	EndKey = '$end_of_table',
	updiff_downdiff(FromTableName,FromDiff,ToTableName,ToUpDiff,ToDownDiff,FirstKey,EndKey).

	updiff_downdiff(_FromTableName,_FromDiff,_ToTableName,_ToUpDiff,_ToDownDiff,EndKey,EndKey)->
		%io:format("updiff_downdiff complete: FTN:~p FD:~p TTN:~p TUD:~p TDD:~p~n",[FromTableName,FromDiff,ToTableName,ToUpDiff,ToDownDiff]),
		done;
	updiff_downdiff(FromTableName,FromDiff,ToTableName,ToUpDiff,ToDownDiff,Key,EndKey)->
		CloseDiff = lookup_element(FromTableName,Key,FromDiff),
		{UpDiff,DownDiff} = updiff_downdiff(CloseDiff),
		update_element(ToTableName,Key,[{ToUpDiff,UpDiff},{ToDownDiff,DownDiff}]),
		updiff_downdiff(FromTableName,FromDiff,ToTableName,ToUpDiff,ToDownDiff,next(FromTableName,Key),EndKey).

		updiff_downdiff(CloseDiff)->
			case CloseDiff == 0 of
				false ->
					case CloseDiff > 0 of
						true ->
							{CloseDiff,0};
						false ->
							{0,CloseDiff}
					end;
				true ->
					{0,0}
			end.

%%=============================== Exponential Moving Average=====================================	
%EMA = P*Alpha + (PrevEMA*(1-Alpha)) = P*Alpha + PrevEMA - PrevEMA*Alpha
%EMAWiki = PrevEMA + Alpha*(P-PrevEMA) = PrevEMA + Alpha*P - Alpha*PrevEMA = P*Alpha + PrevEMA - PrevEMA*Alpha
%P = CurPrice, Alpha = 2/(1+N), N = Number of Time Periods, First EMA is SMA.
ema(FromTableName,FromFeature,ToTableName,ToFeature,Period)->
	Alpha = 2/(1+Period),
	StartKey = first(FromTableName),
	EndKey = '$end_of_table',
	ema(FromTableName,FromFeature,ToTableName,ToFeature,StartKey,EndKey,Alpha).

ema(FromTableName,FromFeature,ToTableName,ToFeature,Period,StartKey)->
	Alpha = 2/(1+Period),
	EndKey = '$end_of_table',
	ema(FromTableName,FromFeature,ToTableName,ToFeature,StartKey,EndKey,Alpha).

	ema(_FromTableName,_FromFeature,_ToTableName,_ToFeature,EndKey,EndKey,_Alpha)->
		%io:format("ema complete: FTN:~p FF:~p TTN:~p TF:~p~n",[FromTableName,FromFeature,ToTableName,ToFeature]),
		done;
	ema(FromTableName,FromFeature,ToTableName,ToFeature,Key,EndKey,Alpha)->
		case prev(ToTableName,Key) of
			'$end_of_table' ->
				FirstVal = lookup_element(FromTableName,Key,FromFeature),
				update_element(ToTableName,Key,[{ToFeature,FirstVal}]);
			PrevKey ->
				Price = lookup_element(FromTableName,Key,FromFeature),
				PrevEMA = lookup_element(ToTableName,PrevKey,ToFeature),
				EMA = Price*Alpha + (PrevEMA*(1-Alpha)),
				update_element(ToTableName,Key,[{ToFeature,EMA}])
		end,
		ema(FromTableName,FromFeature,ToTableName,ToFeature,next(FromTableName,Key),EndKey,Alpha).
		
%%=============================== Simple Moving Average=====================================
% 1 2 3 4 5
% 1+2+3/3 = 2, 2+3+4/3 = 3, 3+4+5/3 = 4
% 2 -1/3 + 4/3 = 2+3/3 = 3,  3 -2/3 + 5/3 = 4
sma(FromTableName,FromFeature,ToTableName,ToFeature,Period)->
	StartKey = index2key(FromTableName,Period),
	TrailingKey = first(FromTableName),
	EndKey = '$end_of_table',
	Avg = average(FromTableName,TrailingKey,StartKey,FromFeature),
	%io:format("~p~n",[{StartKey,TrailingKey,EndKey,Avg}]),
	update_element(ToTableName,StartKey,[{ToFeature,Avg}]),
	sma(FromTableName,FromFeature,ToTableName,ToFeature,TrailingKey,next(FromTableName,StartKey),EndKey,Period).

sma(FromTableName,FromFeature,ToTableName,ToFeature,Period,StartKey)->
	case StartKey == first(FromTableName) of
		true ->
			sma(FromTableName,FromFeature,ToTableName,ToFeature,Period);
		false ->
			TrailingKey = index2key(FromTableName,StartKey,prev,Period),
			EndKey = '$end_of_table',
			Avg = average(FromTableName,TrailingKey,StartKey,FromFeature),
			update_element(ToTableName,StartKey,[{ToFeature,Avg}]),
			sma(FromTableName,FromFeature,ToTableName,ToFeature,TrailingKey,next(FromTableName,StartKey),EndKey,Period)
	end.

	sma(_FromTableName,_FromFeature,_ToTableName,_ToFeature,_TrailingKey,EndKey,EndKey,_Period)->
		%io:format("sma complete: FTN:~p FF:~p TTN:~p TF:~p~n",[FromTableName,FromFeature,ToTableName,ToFeature]),
		done;
	sma(FromTableName,FromFeature,ToTableName,ToFeature,TrailingKey,Key,EndKey,Period)->
		CurrPrice = lookup_element(FromTableName,Key,FromFeature),
		TrailingPrice = lookup_element(FromTableName,TrailingKey,FromFeature),
		PrevSMA = lookup_element(ToTableName,prev(ToTableName,Key),ToFeature),
		SMA = PrevSMA - TrailingPrice/Period + CurrPrice/Period,
		%io:format("~p~n",[{Key,TrailingKey,EndKey,CurrPrice,TrailingPrice,PrevSMA,SMA}]),
		update_element(ToTableName,Key,[{ToFeature,SMA}]),
		sma(FromTableName,FromFeature,ToTableName,ToFeature,next(FromTableName,TrailingKey),next(FromTableName,Key),EndKey,Period).

%%=============================== Relative Strength Index=====================================
rsi(FromTableName,FromFeature_EMAofU,FromFeature_EMAofD,ToTableName,ToFeature_RSI)->
	StartKey = first(FromTableName),
	EndKey = '$end_of_table',
	rsi(FromTableName,FromFeature_EMAofU,FromFeature_EMAofD,ToTableName,ToFeature_RSI,StartKey,EndKey).

rsi(FromTableName,FromFeature_EMAofU,FromFeature_EMAofD,ToTableName,ToFeature_RSI,Key)->
	StartKey = Key,
	EndKey = '$end_of_table',
	rsi(FromTableName,FromFeature_EMAofU,FromFeature_EMAofD,ToTableName,ToFeature_RSI,StartKey,EndKey).

	rsi(_FromTableName,_From_MAofU,_From_MAofD,_ToTableName,_ToFeature,EndKey,EndKey)->
		%io:format("rsi complete: FTN:~p FMAofU:~p FMAofD:~p TTN:~p TF:~p~n",[FromTableName,From_MAofU,From_MAofD,ToTableName,ToFeature]),
		done;
	rsi(FromTableName,FromFeature_EMAofU,FromFeature_EMAofD,ToTableName,ToFeature_RSI,Key,EndKey)->
		EMAofU = lookup_element(FromTableName,Key,FromFeature_EMAofU),
		EMAofD = lookup_element(FromTableName,Key,FromFeature_EMAofD),
		RS = case EMAofD == 0 of
			true ->
				-2;
			false ->								
				EMAofU/EMAofD
		end,
		if 
			RS == 1 -> io:format("RS == 1, Key:~p~n",[Key]);
			EMAofD == 0 -> io:format("EMAofD == 0, Key:~p~n",[Key]);
			true -> done
		end,
		%io:format("RS:~p~n",[RS]),
		RSI = 100 - 100*(1/(1+RS)),
		update_element(ToTableName,Key,[{ToFeature_RSI,RSI}]),
		rsi(FromTableName,FromFeature_EMAofU,FromFeature_EMAofD,ToTableName,ToFeature_RSI,next(FromTableName,Key),EndKey).

%%=============================== Accumulation/Distribution Index =====================================
%CLV = ((Close-Low)-(High-Close))/(High-Low)
%accdist = AccDistPrev + Volume*CLV
adi(FromTableName,FromHigh,FromLow,FromClose,FromVolume,ToTableName,ToADI)->
	FirstKey = first(FromTableName),
	EndKey = '$end_of_table',
	adi(FromTableName,FromHigh,FromLow,FromClose,FromVolume,ToTableName,ToADI,FirstKey,EndKey).

adi(FromTableName,FromHigh,FromLow,FromClose,FromVolume,ToTableName,ToADI,Key)->
	FirstKey = Key,
	EndKey = '$end_of_table',
	adi(FromTableName,FromHigh,FromLow,FromClose,FromVolume,ToTableName,ToADI,FirstKey,EndKey).

	adi(_FromTableName,_FromHigh,_FromLow,_FromClose,_FromVolume,_ToTableName,_ToADI,EndKey,EndKey)->
		%io:format("clv cmplt: FTN:~p FH:~p FL:~p FC:~p FV:~p TTN:~p",[FromTableName,FromHigh,FromLow,FromClose,FromVolume,ToTableName]),
		done;
	adi(FromTableName,FromHigh,FromLow,FromClose,FromVolume,ToTableName,ToADI,Key,EndKey)->
		case prev(ToTableName,Key) of
			'$end_of_table' ->
				update_element(ToTableName,Key,[{ToADI,0}]);
			PrevKey ->
				Volume = lookup_element(FromTableName,Key,FromVolume),
				PrevADI = lookup_element(ToTableName,PrevKey,ToADI),
				High = lookup_element(FromTableName,Key,FromHigh),
				Low = lookup_element(FromTableName,Key,FromLow),
				Close = lookup_element(FromTableName,Key,FromClose),
				CLV = clv(High,Low,Close),
				ADI = PrevADI + Volume*CLV,
				update_element(ToTableName,Key,[{ToADI,ADI}])
		end,
		adi(FromTableName,FromHigh,FromLow,FromClose,FromVolume,ToTableName,ToADI,next(FromTableName,Key),EndKey).
	
		clv(High,Low,Close)->
			case High == Low of
				true ->
					0;
				false ->
					((Close - Low) - (High-Close))/(High-Low)
			end.

%%=============================== Stochastic Oscilator=====================================	
%K_Fast = 100*(Close - Lowest_from_n_periods)/(Highiest_from_n_periods-Lowest_from_n_periods) n = 14 DONE
%D_Fast = EMA(K_Fast,3) DONE
%K_Slow = EMA(K_Fast,3)
%D_Slow = EMA(K_Slow,14) DONE
%K_Full = EMA(K_Fast,14)
%D_Full = EMA(K_Full,14) DONE
sts(FromTableName,FromHigh,FromLow,FromClose,ToTableName,ToKFast,ToDFast,ToDSlow,ToDFull)->
	StartKey = index2key(FromTableName,first(FromTableName),next,15),
	EndKey = '$end_of_table',
	sts(FromTableName,FromHigh,FromLow,FromClose,ToTableName,ToKFast,ToDFast,ToDSlow,ToDFull,StartKey,EndKey).

sts(FromTableName,FromHigh,FromLow,FromClose,ToTableName,ToKFast,ToDFast,ToDSlow,ToDFull,StartKey)->
	EndKey = '$end_of_table',
	Key = case StartKey == first(FromTableName) of
		true ->
			index2key(FromTableName,first(FromTableName),next,15);
		false ->
			StartKey
	end,
	sts(FromTableName,FromHigh,FromLow,FromClose,ToTableName,ToKFast,ToDFast,ToDSlow,ToDFull,Key,EndKey).
	
	sts(FromTableName,FromHigh,FromLow,FromClose,ToTableName,ToKFast,ToDFast,ToDSlow,ToDFull,StartKey,EndKey)->
		sts_kfast(FromTableName,FromHigh,FromLow,FromClose,ToTableName,ToKFast,StartKey,EndKey),
		ema(ToTableName,ToKFast,ToTableName,ToDFast,StartKey,EndKey,1/3), %dfast
		%ema(ToTableName,ToKFast,ToTableName,ToKSlow,StartKey,EndKey,1/3), %kslow
		ema(ToTableName,ToDFast,ToTableName,ToDSlow,StartKey,EndKey,1/14), %dslow
		%ema(ToTableName,ToKFast,ToTableName,ToKFull,StartKey,EndKey,1/14), %dkfull
		ema(ToTableName,ToDSlow,ToTableName,ToDFull,StartKey,EndKey,1/14). %dfull
	
	sts_kfast(_FromTableName,_FromHigh,_FromLow,_FromClose,_ToTableName,_ToKFast,EndKey,EndKey)->
		%io:format("sts complete: FTN:~p FH:~p FL:~p FC:~p TTN:~p TSTS:~p~n",[FromTableName,FromHigh,FromLow,FromClose,ToTableName,ToSTS]);
		done;
	sts_kfast(FromTableName,FromHigh,FromLow,FromClose,ToTableName,ToKFast,Key,EndKey)->
		Current_Close = lookup_element(FromTableName,Key,FromClose),
		Highest_High = max(FromTableName,index2key(FromTableName,Key,prev,14),next,next(FromTableName,Key),FromHigh,0),
		Lowest_Low = min(FromTableName,index2key(FromTableName,Key,prev,14),next,next(FromTableName,Key),FromLow,0),
		K_Fast = case Highest_High == Lowest_Low of
				true ->
					0;
				false ->
					(Current_Close - Lowest_Low)/(Highest_High - Lowest_Low)
		end,
		update_element(ToTableName,Key,[{ToKFast,K_Fast}]),
		sts_kfast(FromTableName,FromHigh,FromLow,FromClose,ToTableName,ToKFast,next(FromTableName,Key),EndKey).
		
%%=============================== MACD=====================================
%MACD = EMA(12)ofprice - EMA(26)ofprice
%Signal = EMA(9)ofMACD
%Stochastics = MACD - Signal
macd(FromTableName,FromFeature_EMA12,FromFeature_EMA26,ToTableName,ToFeature_MACD)->
	StartKey = first(FromTableName),
	EndKey = '$end_of_table',
	macd(FromTableName,FromFeature_EMA12,FromFeature_EMA26,ToTableName,ToFeature_MACD,StartKey,EndKey).

macd(FromTableName,FromFeature_EMA12,FromFeature_EMA26,ToTableName,ToFeature_MACD,Key)->
	StartKey = Key,
	EndKey = '$end_of_table',
	macd(FromTableName,FromFeature_EMA12,FromFeature_EMA26,ToTableName,ToFeature_MACD,StartKey,EndKey).
	
	macd(_FromTableName,_FromEMA12,_FromEMA26,_ToTableName,_ToMACD,EndKey,EndKey)->
		%io:format("macd complete: FTN:~p FEMA12:~p FEMA26:~p TTN:~p TMACD:~p~n",[FromTableName,FromEMA12,FromEMA26,ToTableName,ToMACD]),
		done;
	macd(FromTableName,FromFeature_EMA12,FromFeature_EMA26,ToTableName,ToFeature_MACD,Key,EndKey)->
		EMA12 = lookup_element(FromTableName,Key,FromFeature_EMA12),
		EMA26 = lookup_element(FromTableName,Key,FromFeature_EMA26),
		%io:format("~p~n",[{EMA12,EMA26,Index}]),
		MACD = EMA12 - EMA26,
		update_element(ToTableName,Key,[{ToFeature_MACD,MACD}]),
		macd(FromTableName,FromFeature_EMA12,FromFeature_EMA26,ToTableName,ToFeature_MACD,next(FromTableName,Key),EndKey).
		
display_table(_Table,'$end_of_table')->
	done;
display_table(Table,Key)->
	io:format("~p~n",[lookup(Table,Key)]),
	display_table(Table,next(Table,Key)).
	
display_fmetadata(Table,CPair)->
	io:format("~p~n",[[lookup(forex_metadata,{Table,CPair,Feature}) || Feature <- ?FOREX_TECHNICAL--[key]]]).
	
test_ops()->
	T = ets:new(t,[ordered_set,public]),
	ets:insert(T,{1,-2,-1,-4,0}),
	ets:insert(T,{2,-1,0,-2,0}),
	ets:insert(T,{3,1,1,0,0}),
	ets:insert(T,{4,2,2,2,0}),
	ets:insert(T,{5,-3,4,4,0}),
	%diff(T,2,T,5),
	ema(T,2,T,5,3,2),
	display_table(T,1),
	%io:format("~p~n",[{max(T,2),min(T,5)}]),
	done.
	
system1()->
	done.
	
system2()->
	done.

system3()->
	done.
	
list_to_number(List)->
	try list_to_float(List) of
		Float ->
			Float
	catch 
		_:_ ->
			list_to_integer(List)
	end.

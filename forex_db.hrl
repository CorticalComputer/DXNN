%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-define(TL,forex_db:tot_ftf()).
-define(CFTAGS,[{1,{TableName,ElemPosition}} || TableName <- [fx15], ElemPosition <-lists:seq(5,5)]).
-define(CTTAGS,[{1,{TableName,ElemPosition}}||TableName <- [fx15], ElemPosition<-lists:seq(2,5*?TL)--lists:seq(1,5*?TL,?TL)]).

-define(STD_TABLES,[
	map,
	metadata,
	test]).

-define(FX_TABLES,[
	forex_metadata,
	fx1,
	fx5,
	fx15,
	fx30,
	fx60,
	fx240,
	fx1440]).
	
-define(ALL_TABLES,[
	map,
	metadata,
	test,
	forex_metadata,
	fx1,
	fx5,
	fx15,
	fx30,
	fx60,
	fx240,
	fx1440]).

-define(METADATA,[
	key,	%%%key = tablename,
	size,
	last_updated,
	start,
	stop
]).
-define(TEST,[
	key,	%%%key= Index
	q,
	a
]).
-define(FOREX_METADATA,[
	key,	%%%key = {TableName,cpair,feature}
	first,
	last,
	avg,
	dev,
	stdev,
	quantile25,
	quantile50,
	quantile75,
	max,
	min,
	operators_applied
]).

-define(FOREX_TECHNICAL,[
	key,	%%%key={Year,Month,Day,Hour,Minute,Second}
	open,
	high,
	low,
	close,
	volume,
	diff,
	ema6,
	ema14,
	ema26,
	ema50
]).

-define(FOREX_TECHNICAL1,[
	key,	%%%key={Year,Month,Day,Hour,Minute,Second}
	open,
	high,
	low,
	close,
	volume,
	diff,
	up_diff,
	down_diff,
	ud_ema27,
	dd_ema27,
	ema2,
	ema3,
	ema6,
	ema9,
	ema14,
	ema26,
	ema50,
	ema100,
	sma2,
	sma3,
	sma6,
	sma9,
	sma14,
	sma26,
	sma50,
	sma100,
	rsi,
	macd,
	macd_signal,
	adi,
	sts_kfast,
	sts_dfast,
	sts_dslow,
	sts_dfull,
	bix9,
	trix9
]).
	
-define(TIMEFRAMES,[
	1,
	5,
	15,
	30,
	240,
	1440]).

-define(CPAIRS,["EURUSD"]).
%-define(CPAIRS,[
%	"EURUSD",
%	"USDJPY",
%	"GBPUSD",
%	"USDCHF",
%	"USDCAD",
%	"USDAUD",
%	"USDNZD"]).

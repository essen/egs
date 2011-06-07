%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc EGS seasons management.
%% @todo When we know how to do it we should change the lobby automatically to the next season.
%%
%%	This file is part of EGS.
%%
%%	EGS is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU Affero General Public License as
%%	published by the Free Software Foundation, either version 3 of the
%%	License, or (at your option) any later version.
%%
%%	EGS is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU Affero General Public License for more details.
%%
%%	You should have received a copy of the GNU Affero General Public License
%%	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

-module(egs_seasons).
-behavior(gen_server).

-export([start_link/0, stop/0, read/1]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

%% Seasonal values: {IsSeasonal, SeasonID, QuestList}.
-define(SEASON_NONE,      {0,255, []}).
-define(SEASON_PARTY,     {1,  0, [1100000]}).
-define(SEASON_NEWYEAR,   {1,  1, [1100000, 1102000]}).
-define(SEASON_VALENTINE, {1,  2, [1100000]}).
-define(SEASON_WHITEDAY,  {1,  3, [1100000]}).
-define(SEASON_SPRING,    {1,  4, [1100000, 1102000]}).
-define(SEASON_EASTER,    {1,  5, [1100000]}).
-define(SEASON_PARUMUNIF, {1,  6, [1101000]}).
-define(SEASON_SONIC,     {1,  7, [1100000]}).
-define(SEASON_HOLYLIGHT, {1,  8, [1102000]}).
-define(SEASON_FIREWORKS, {1,  9, [1102000]}).
-define(SEASON_AUTUMN,    {1, 10, [1100000, 1102000]}).
-define(SEASON_HALLOWEEN, {1, 11, [1100000, 1101000, 1103000]}).
-define(SEASON_NATIVE,    {1, 12, [1103000]}).
-define(SEASON_CHRISTMAS, {1, 13, [1100000, 1101000, 1103000]}).
-define(SEASON_WINTER,    {1, 14, [1100000, 1101000]}).
-define(SEASON_WEDDING,   {1, 15, [1100000, 1101000, 1102000, 1103000]}).

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec read(QuestID) -> {IsSeasonal, SeasonID}
read(QuestID) ->
	gen_server:call(?SERVER, {read, QuestID}).

%% gen_server.

init([]) ->
	error_logger:info_report("egs_seasons started"),
	{ok, undefined}.

handle_call({read, QuestID}, _From, State) ->
	{{_, Month, Day}, _} = calendar:universal_time(),
	{IsSeasonal, SeasonID, QuestList} = if
		Month =:=  1, Day =< 14            -> ?SEASON_NEWYEAR;
		Month =:=  1, Day >= 25            -> ?SEASON_WINTER;
		Month =:=  2, Day =< 7             -> ?SEASON_WINTER;
		Month =:=  2, Day >= 14            -> ?SEASON_VALENTINE;
		Month =:=  3, Day =< 6             -> ?SEASON_VALENTINE;
		Month =:=  3, Day >= 14            -> ?SEASON_WHITEDAY;
		Month =:=  4, Day =< 3             -> ?SEASON_WHITEDAY;
		Month =:=  4, Day >= 4, Day =< 24  -> ?SEASON_EASTER;
		Month =:=  4, Day >= 25            -> ?SEASON_SPRING;
		Month =:=  5, Day =< 8             -> ?SEASON_SPRING;
		Month =:=  5, Day >= 17, Day =< 30 -> ?SEASON_WEDDING;
		Month =:=  6, Day >= 3, Day =< 16  -> ?SEASON_PARUMUNIF;
		Month =:=  6, Day >= 23            -> ?SEASON_SONIC;
		Month =:=  7, Day =< 13            -> ?SEASON_SONIC;
		Month =:=  7, Day >= 18            -> ?SEASON_HOLYLIGHT;
		Month =:=  8, Day =< 21            -> ?SEASON_FIREWORKS;
		Month =:=  8, Day >= 28            -> ?SEASON_NATIVE;
		Month =:=  9, Day =< 10            -> ?SEASON_NATIVE;
		Month =:=  9, Day >= 24            -> ?SEASON_AUTUMN;
		Month =:= 10, Day =< 7             -> ?SEASON_AUTUMN;
		Month =:= 10, Day >= 15, Day =< 28 -> ?SEASON_PARTY;
		Month =:= 10, Day >= 31            -> ?SEASON_HALLOWEEN;
		Month =:= 11, Day =< 20            -> ?SEASON_HALLOWEEN;
		Month =:= 12, Day >= 11            -> ?SEASON_CHRISTMAS;
		true                               -> ?SEASON_NONE
	end,
	if	IsSeasonal =:= 0 ->
			{reply, {0, 255}, State};
		true ->
			case lists:member(QuestID, QuestList) of
				true  -> {reply, {IsSeasonal, SeasonID}, State};
				false -> {reply, {0, 255}, State}
			end
	end;

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

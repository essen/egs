%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc EGS quests database and cache manager.
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

-module(egs_quests_db).
-behavior(gen_server).

-export([start_link/0, stop/0, quest_nbl/1, zone_nbl/2, area_type/2, quest_zones/1, set/3, reload/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

-record(state, {quests=[], quests_bin=[], zones_bin=[], sets=[]}).

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec quest_nbl(QuestID) -> binary()
quest_nbl(QuestID) ->
	gen_server:call(?SERVER, {quest_nbl, QuestID}).

%% @spec zone_nbl(QuestID, ZoneID) -> binary()
zone_nbl(QuestID, ZoneID) ->
	gen_server:call(?SERVER, {zone_nbl, QuestID, ZoneID}).

area_type(QuestID, ZoneID) ->
	gen_server:call(?SERVER, {area_type, QuestID, ZoneID}).

quest_zones(QuestID) ->
	gen_server:call(?SERVER, {quest_zones, QuestID}).

set(QuestID, ZoneID, SetID) ->
	gen_server:call(?SERVER, {set, QuestID, ZoneID, SetID}).

%% @spec reload() -> ok
reload() ->
	gen_server:cast(?SERVER, reload).

%% gen_server.

init([]) ->
	{ok, #state{}}.

%% @doc Return a quest information either from the cache or from the configuration file,
%% in which case it gets added to the cache for subsequent attempts.
handle_call({quest_nbl, QuestID}, _From, State=#state{quests=Cache, quests_bin=BinCache}) ->
	case proplists:get_value(QuestID, BinCache) of
		undefined ->
			Dir = io_lib:format("priv/quests/~b/", [QuestID]),
			ConfFilename = Dir ++ "quest.conf",
			{ok, Settings} = file:consult(ConfFilename),
			{QuestXnrData, QuestXnrPtrs} = egs_files:load_quest_xnr(Settings),
			UnitTitleBinFiles = load_unit_title_bin_files(Dir, Settings),
			Files = [{data, "quest.xnr", QuestXnrData, QuestXnrPtrs}],
			Files2 = Files ++ case UnitTitleBinFiles of
				ignore -> [];
				_Any ->
					TablePos = egs_files:nbl_padded_size(byte_size(QuestXnrData)),
					TextSize = lists:sum([egs_files:nbl_padded_size(byte_size(D)) || {data, _F, D, _P} <- UnitTitleBinFiles]),
					TablePos2 = TablePos + TextSize,
					{UnitTitleTableRelData, UnitTitleTableRelPtrs} = egs_files:load_unit_title_table_rel(ConfFilename, TablePos2),
					UnitTitleBinFiles ++ [{data, "unit_title_table.rel", UnitTitleTableRelData, UnitTitleTableRelPtrs}]
			end,
			QuestNbl = egs_files:nbl_pack([{files, Files2}]),
			{reply, QuestNbl, State#state{quests=[{QuestID, Settings}|Cache], quests_bin=[{QuestID, QuestNbl}|BinCache]}};
		QuestNbl ->
			{reply, QuestNbl, State}
	end;

%% @doc Return a zone information either from the cache or from the configuration files.
%% @todo FilePos, text.bin, other sets, enemies.
handle_call({zone_nbl, QuestID, ZoneID}, _From, State=#state{quests=QuestsCache, zones_bin=BinCache}) ->
	case proplists:get_value({QuestID, ZoneID}, BinCache) of
		undefined ->
			Dir = io_lib:format("priv/quests/~b/", [QuestID]),
			ZoneDir = Dir ++ io_lib:format("zone-~b/", [ZoneID]),
			QuestSettings = proplists:get_value(QuestID, QuestsCache),
			Zones = proplists:get_value(zones, QuestSettings),
			Zone = proplists:get_value(ZoneID, Zones),
			AreaID = proplists:get_value(areaid, Zone),
			Maps = proplists:get_value(maps, Zone),
			FilePos = 0, %% @todo
			{Set0, SetPtrs} = egs_files:load_set_rel(ZoneDir ++ io_lib:format("set_r~b.conf", [0]), AreaID, Maps, FilePos),
			ScriptBin = egs_files:load_script_bin(ZoneDir ++ "script.es"),
			ScriptBinSize = byte_size(ScriptBin),
			ScriptBin2 = prs:compress(ScriptBin),
			ScriptBinSize2 = byte_size(ScriptBin2),
			ScriptBin3 = << ScriptBinSize:32/little, ScriptBinSize2:32/little, 0:32, 1:32/little, 0:96, ScriptBin2/binary >>,
			TextBin = egs_files:load_text_bin(ZoneDir ++ "text.bin.en_US.txt"),
			ZoneNbl = egs_files:nbl_pack([{files, [
				{data, "set_r0.rel", Set0, SetPtrs},
				{data, "script.bin", ScriptBin3, []},
				{data, "text.bin", TextBin, []}
			]}]),
			{reply, ZoneNbl, State#state{zones_bin=[{{QuestID, ZoneID}, ZoneNbl}|BinCache]}};
		ZoneNbl ->
			{reply, ZoneNbl, State}
	end;

handle_call({area_type, QuestID, ZoneID}, _From, State=#state{quests=QuestsCache}) ->
	{_, Quest}	= lists:keyfind(QuestID, 1, QuestsCache),
	{_, Zones}	= lists:keyfind(zones, 1, Quest),
	{_, Zone}	= lists:keyfind(ZoneID, 1, Zones),
	{_, AreaID}	= lists:keyfind(areaid, 1, Zone),
	AreaType = case AreaID of
		   0 -> lobby;
		   2 -> lobby;
		   3 -> lobby;
		   4 -> lobby;
		   5 -> lobby;
		  22 -> myroom;
		_Any -> mission
	end,
	{reply, AreaType, State};

handle_call({quest_zones, QuestID}, _From, State=#state{quests=QuestsCache}) ->
	{_, Quest}	= lists:keyfind(QuestID, 1, QuestsCache),
	{_, Zones}	= lists:keyfind(zones, 1, Quest),
	{reply, Zones, State};

%% @todo The set file is loaded both here and in zone_nbl. Thinking about it zone_nbl should call this function.
%% @todo Same for quest_nbl loading quest files and binaries, there should be a function for the file itself called only when needed.
handle_call({set, QuestID, ZoneID, SetID}, _From, State=#state{sets=SetsCache}) ->
	case proplists:get_value({QuestID, ZoneID, SetID}, SetsCache) of
		undefined ->
			SetFilename = io_lib:format("priv/quests/~b/zone-~b/set_r~b.conf", [QuestID, ZoneID, SetID]),
			{ok, Set} = file:consult(SetFilename),
			{reply, Set, State#state{sets=[{{QuestID, ZoneID, SetID}, Set}|SetsCache]}};
		CachedSet ->
			{reply, CachedSet, State}
	end;

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(reload, _State) ->
	{noreply, #state{}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

load_unit_title_bin_files(Dir, Settings) ->
	case proplists:get_value(notitles, Settings) of
		true -> ignore;
		_Any ->
			Zones = proplists:get_value(zones, Settings),
			[load_unit_title_bin(Dir, Zone) || Zone <- Zones]
	end.

load_unit_title_bin(Dir, {ZoneID, _ZoneParams}) ->
	Filename = io_lib:format("unit_title_~2.10.0b.bin", [ZoneID]),
	TxtFilename = io_lib:format("~s~s.en_US.txt", [Dir, Filename]),
	{data, Filename, egs_files:load_text_bin(TxtFilename), []}.

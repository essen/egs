%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
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
-export([start_link/0, stop/0, quest/1, reload/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec quest(QuestID) -> binary()
quest(QuestID) ->
	gen_server:call(?SERVER, {quest, QuestID}).

%% @spec reload() -> ok
reload() ->
	gen_server:cast(?SERVER, reload).

%% gen_server.

init([]) ->
	{ok, []}.

%% @doc Possible keys: quest.
handle_call({Key, QuestID}, _From, State) ->
	{Quest, State2} = get_quest(QuestID, State),
	{reply, proplists:get_value(Key, Quest), State2};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(reload, _State) ->
	{noreply, []};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

%% @doc Return a quest information either from the cache or from the configuration file,
%% in which case it gets added to the cache for subsequent attempts.
get_quest(QuestID, Cache) ->
	case proplists:get_value(QuestID, Cache) of
		undefined ->
			Dir = io_lib:format("priv/quests/~b/", [QuestID]),
			ConfFilename = Dir ++ "quest.conf",
			{QuestXnrData, QuestXnrPtrs} = egs_files:load_quest_xnr(ConfFilename),
			UnitTitleBinFiles = load_unit_title_bin_files(Dir, ConfFilename),
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
			Quest = [{quest, QuestNbl}],
			Cache2 = [{QuestID, Quest}|Cache],
			{Quest, Cache2};
		Quest ->
			{Quest, Cache}
	end.

load_unit_title_bin_files(Dir, ConfFilename) ->
	{ok, Settings} = file:consult(ConfFilename),
	case proplists:get_value(notitles, Settings) of
		true -> ignore;
		_Any ->
			Zones = proplists:get_value(zones, Settings),
			[load_unit_title_bin(Dir, Zone) || Zone <- Zones]
	end.

load_unit_title_bin(Dir, Zone) ->
	ZoneID = proplists:get_value(zoneid, Zone),
	Filename = io_lib:format("unit_title_~2.10.0b.bin", [ZoneID]),
	TxtFilename = io_lib:format("~s~s.en_US.txt", [Dir, Filename]),
	{data, Filename, egs_files:load_text_bin(TxtFilename), []}.

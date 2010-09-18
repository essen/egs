%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc NPC characters handling.
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

-module(psu_npc).
-compile(export_all).

-include("include/records.hrl").
-include("include/psu/npc.hrl").

%% @todo Improve the NPC handling. Handle more than Lou.
%% @todo Handle stats, experience, based on level.
%% @todo Level shouldn't go below 1 or above 200.
user_init(NPCid, BaseLevel) ->
	NPCGID = 16#ff000000 + mnesia:dirty_update_counter(counters, tmpgid, 1),
	Settings = proplists:get_value(NPCid, ?NPC),
	TmpUCS2Name = << << X:8, 0:8 >> || X <- Settings#psu_npc.name >>,
	PaddingSize = 8 * (64 - byte_size(TmpUCS2Name)),
	UCS2Name = << TmpUCS2Name/binary, 0:PaddingSize >>,
	#psu_npc{race=Race, gender=Gender, class=Class, level=LevelDiff, appearance=Appearance} = Settings,
	Character = #characters{gid=NPCGID, slot=0, type=npc, npcid=NPCid, name=UCS2Name, race=Race, gender=Gender, class=Class, appearance=Appearance,
		mainlevel={level, calc_level(BaseLevel, LevelDiff), 0}, blastbar=0, luck=2, money=0, playtime=0, stats={stats, 0, 0, 0, 0, 0, 0, 0}, se=[], currenthp=100, maxhp=100},
	#egs_user_model{id=NPCGID, state=online, character=Character, areatype=lobby, area={psu_area, 0, 0, 0}, entryid=0,
		prev_area={psu_area, 0, 0, 0}, prev_entryid=0, pos={pos, 0.0, 0.0, 0.0, 0.0}}.

calc_level(BaseLevel, LevelDiff) ->
	TmpLevel = BaseLevel + LevelDiff,
	if	TmpLevel < 1 -> 1;
		TmpLevel > 200 -> 200;
		true -> TmpLevel
	end.

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc EGS file creation functions.
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

-module(egs_files).
-export([load_counter_pack/2, load_table_rel/1, load_text_bin/1]).

%% @doc Build a counter's pack file, options and return them along with the background value.
load_counter_pack(ConfFilename, CounterNbl) ->
	{ok, Settings} = file:consult(ConfFilename),
	Groups = proplists:get_value(groups, Settings),
	{FilesBin, PosList, SizeList} = load_counter_pack_groups(Groups, [0], [byte_size(CounterNbl)]),
	NbFiles = length(PosList),
	PosBin = iolist_to_binary([<< P:32/little >> || P <- PosList]),
	SizeBin = iolist_to_binary([<< S:32/little >> || S <- SizeList]),
	Padding = 8 * (512 - byte_size(PosBin)),
	Pack = << PosBin/binary, 0:Padding, SizeBin/binary, 0:Padding, NbFiles:32/little, CounterNbl/binary, FilesBin/binary >>,
	Opts = case length(Groups) of
		0 -> << >>;
		L ->
			OptsList = lists:seq(1, L + length(PosList)),
			iolist_to_binary([<< 3:8 >> || _N <- OptsList])
	end,
	Opts2 = if byte_size(Opts) rem 2 =:= 0 -> Opts; true -> << Opts/binary, 0 >> end,
	[{bg, proplists:get_value(bg, Settings, 255)}, {opts, Opts2}, {pack, Pack}].

load_counter_pack_groups(Groups, PosList, SizeList) ->
	load_counter_pack_groups(Groups, PosList, SizeList, []).
load_counter_pack_groups([], PosList, SizeList, Acc) ->
	GroupsBin = iolist_to_binary(lists:reverse(Acc)),
	{GroupsBin, lists:reverse(PosList), lists:reverse(SizeList)};
load_counter_pack_groups([Group|Tail], PosList, SizeList, Acc) ->
	Quests = proplists:get_value(quests, Group),
	{QuestsBin, PosList2, SizeList2} = load_counter_pack_quests(Quests, PosList, SizeList),
	load_counter_pack_groups(Tail, PosList2, SizeList2, [QuestsBin|Acc]).

load_counter_pack_quests(Quests, PosList, SizeList) ->
	load_counter_pack_quests(Quests, PosList, SizeList, []).
load_counter_pack_quests([], PosList, SizeList, Acc) ->
	QuestsBin = iolist_to_binary(lists:reverse(Acc)),
	{QuestsBin, PosList, SizeList};
load_counter_pack_quests([{_QuestID, Filename}|Tail], PosList, SizeList, Acc) ->
	{ok, File} = file:read_file("data/missions/" ++ Filename),
	Pos = lists:sum(SizeList),
	Size = byte_size(File),
	load_counter_pack_quests(Tail, [Pos|PosList], [Size|SizeList], [File|Acc]).

%% @doc Load a counter configuration file and return a table.rel binary along with its pointers array.
load_table_rel(ConfFilename) ->
	{ok, Settings} = file:consult(ConfFilename),
	TName = proplists:get_value(t_name, Settings),
	TDesc = proplists:get_value(t_desc, Settings),
	{CursorX, CursorY} = proplists:get_value(cursor, Settings),
	{NbQuests, QuestsBin, NbGroups, GroupsBin} = load_table_rel_groups_to_bin(proplists:get_value(groups, Settings)),
	QuestsPos = 16,
	GroupsPos = 16 + byte_size(QuestsBin),
	UnixTime = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now()))
		- calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	MainBin = << 16#00000100:32, UnixTime:32/little, GroupsPos:32/little, QuestsPos:32/little,
		NbGroups:16/little, NbQuests:16/little, TName:16/little, TDesc:16/little, CursorX:16/little, CursorY:16/little,
		0:32, 0:16, 16#ffff:16 >>,
	MainPos = GroupsPos + byte_size(GroupsBin),
	{CityPos, CityBin} = case proplists:get_value(city, Settings) of
		undefined -> {0, << >>};
		{QuestID, ZoneID, MapID, EntryID} ->
			{MainPos + byte_size(MainBin) + 4, << QuestID:32/little, ZoneID:16/little, MapID:16/little, EntryID:16/little >>}
	end,
	Data = << MainPos:32/little, 0:32, QuestsBin/binary, GroupsBin/binary, MainBin/binary, CityPos:32/little, CityBin/binary >>,
	Size = byte_size(Data),
	Data2 = << $N, $X, $R, 0, Size:32/little, Data/binary >>,
	case CityPos of
		0 -> {Data2, [MainPos + 8, MainPos + 12]};
		_ -> {Data2, [MainPos + 8, MainPos + 12, MainPos + 36]}
	end.

%% @doc Convert groups of quests to their binary equivalent for load_table_rel.
load_table_rel_groups_to_bin(Groups) ->
	load_table_rel_groups_to_bin(Groups, 0, [], []).
load_table_rel_groups_to_bin([], N, QAcc, GAcc) ->
	NbGroups = length(GAcc),
	Quests = iolist_to_binary(lists:reverse(QAcc)),
	Groups = iolist_to_binary(lists:reverse(GAcc)),
	{N, Quests, NbGroups, Groups};
load_table_rel_groups_to_bin([Settings|Tail], N, QAcc, GAcc) ->
	TName = proplists:get_value(t_name, Settings),
	TDesc = proplists:get_value(t_desc, Settings),
	Quests = proplists:get_value(quests, Settings),
	QuestsBin = [<< Q:32/little >> || {Q, _Filename} <- Quests],
	L = length(Quests),
	GroupBin = << N:16/little, L:16/little, TName:16/little, TDesc:16/little, 0:16, 16#ff03:16, 0:160 >>,
	load_table_rel_groups_to_bin(Tail, N + L, [QuestsBin|QAcc], [GroupBin|GAcc]).

%% @doc Load a text.bin file from its UCS-2 txt file equivalent.
load_text_bin(TextFilename) ->
	{ok, << 16#fffe:16, File/binary >>} = file:read_file(TextFilename),
	Strings = re:split(File, "\n."),
	TextBin = load_text_bin_strings(Strings),
	Size = 12 + byte_size(TextBin),
	<< Size:32/little, 8:32/little, 12:32/little, TextBin/binary >>.

load_text_bin_strings(Strings) ->
	load_text_bin_strings(Strings, 0, [], []).
load_text_bin_strings([], _Pos, PosList, Acc) ->
	L = length(PosList) * 4,
	PosList2 = [P + L + 12 || P <- lists:reverse(PosList)],
	PosBin = iolist_to_binary([<< P:32/little >> || P <- PosList2]),
	StringsBin = iolist_to_binary(lists:reverse(Acc)),
	<< PosBin/binary, StringsBin/binary >>;
%% empty line at the end of a text.bin.txt file.
load_text_bin_strings([<< >>], Pos, PosList, Acc) ->
	load_text_bin_strings([], Pos, PosList, Acc);
load_text_bin_strings([String|Tail], Pos, PosList, Acc) ->
	String2 = re:replace(String, "~.n.", "\n\0", [global, {return, binary}]),
	String3 = << String2/binary, 0, 0 >>,
	load_text_bin_strings(Tail, Pos + byte_size(String3), [Pos|PosList], [String3|Acc]).

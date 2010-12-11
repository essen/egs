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
-export([load_counter_pack/2, load_quest_xnr/1, load_script_bin/1, load_set_rel/4, load_table_rel/1,
	load_text_bin/1, load_unit_title_table_rel/2, nbl_pack/1, nbl_padded_size/1]).

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

%% @doc Load a quest configuration file and return a quest.xnr binary along with its pointers array.
load_quest_xnr(ConfFilename) ->
	{ok, Settings} = file:consult(ConfFilename),
	QuestID = proplists:get_value(questid, Settings),
	%% Temp flags.
	TmpFlagsList = proplists:get_value(temp_flags, Settings),
	TmpFlagsBin = load_quest_xnr_flags(TmpFlagsList),
	%% Value flags.
	ValFlagsList = proplists:get_value(value_flags, Settings),
	ValFlagsBin = load_quest_xnr_flags(ValFlagsList),
	%% Bool flags.
	BoolFlagsList = proplists:get_value(bool_flags, Settings),
	BoolFlagsBin = load_quest_xnr_flags(BoolFlagsList),
	%% Items.
	ItemsPos = 16 + byte_size(TmpFlagsBin) + byte_size(ValFlagsBin) + byte_size(BoolFlagsBin),
	ItemsList = proplists:get_value(items, Settings),
	ItemsBin = load_quest_xnr_items(ItemsList),
	NbItems = length(ItemsList),
	%% Item pointers.
	ItemsPtrsPos = ItemsPos + byte_size(ItemsBin),
	ItemsPtrs = << ItemsPos:32/little, NbItems:32/little >>,
	%% Zones.
	ZonesBasePos = ItemsPtrsPos + byte_size(ItemsPtrs),
	ZonesList = proplists:get_value(zones, Settings),
	{SetsBin, SetsPtrsList, ZonesBin} = load_quest_xnr_zones(ZonesList, ZonesBasePos),
	ZonesPos = ZonesBasePos + byte_size(SetsBin),
	NbZones = length(ZonesList),
	%% Warps.
	WarpsPos = ZonesPos + byte_size(ZonesBin),
	WarpsList = proplists:get_value(warps, Settings),
	WarpsBin = load_quest_xnr_warps(QuestID, WarpsList),
	NbWarps = length(WarpsList),
	%% Temp flag pointers.
	TmpFlagsPtrsPos = WarpsPos + byte_size(WarpsBin),
	{NbTmpFlags, TmpFlagsPtrs} = load_quest_xnr_flag_ptrs(TmpFlagsList, 0),
	TmpFlagsPtrsPos2 = if NbTmpFlags =/= 0 -> TmpFlagsPtrsPos; true -> 0 end,
	%% Value flag pointers.
	ValFlagsPtrsPos = TmpFlagsPtrsPos + byte_size(TmpFlagsPtrs),
	{NbValFlags, ValFlagsPtrs} = load_quest_xnr_flag_ptrs(ValFlagsList, NbTmpFlags),
	ValFlagsPtrsPos2 = if NbValFlags =/= 0 -> ValFlagsPtrsPos; true -> 0 end,
	%% Bool flag pointers.
	BoolFlagsPtrsPos = ValFlagsPtrsPos + byte_size(ValFlagsPtrs),
	{NbBoolFlags, BoolFlagsPtrs} = load_quest_xnr_flag_ptrs(BoolFlagsList, NbTmpFlags + NbValFlags),
	BoolFlagsPtrsPos2 = if NbBoolFlags =/= 0 -> BoolFlagsPtrsPos; true -> 0 end,
	%% Main pointers.
	MainPos = BoolFlagsPtrsPos + byte_size(BoolFlagsPtrs),
	NbNPCs = 0, %% @todo
	NPCsPos = MainPos + 260, %% @todo if NbNPCs =/= 0 -> todo; true -> MainPos + 260 end,
	NPCsBin = << 0:64 >>,
	%% Main options.
	EnterWarp = load_quest_xnr_warp(QuestID, proplists:get_value(enter_warp, Settings)),
	ExitWarp = load_quest_xnr_warp(QuestID, proplists:get_value(exit_warp, Settings)),
	FailWarp = load_quest_xnr_warp(QuestID, proplists:get_value(fail_warp, Settings)),
	MissionOpts = 0, %% @todo
	NbCustomNPCs = 0, %% @todo
	Icon = proplists:get_value(icon, Settings),
	{PartySizeMin, PartySizeMax} = proplists:get_value(party_size, Settings),
	{CursorX, CursorY} = proplists:get_value(cursor, Settings),
	UnixTime = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now()))
		- calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	MainBin = << 16#16000600:32, UnixTime:32/little, QuestID:32/little, ZonesPos:32/little, NbZones:32/little,
		WarpsPos:32/little, NbWarps:32/little, EnterWarp/binary, ExitWarp/binary, MissionOpts:8, 16#040200:24, 0:288,
		Icon:16/little, 0:16, PartySizeMin:8, PartySizeMax:8, CursorX:16/little, CursorY:16/little, 0:16, ItemsPtrsPos:32/little,
		FailWarp/binary, 0:160, TmpFlagsPtrsPos2:32/little, ValFlagsPtrsPos2:32/little, BoolFlagsPtrsPos2:32/little, NbTmpFlags:8,
		NbValFlags:8, NbBoolFlags:8, NbNPCs:8, NPCsPos:32/little, 16#00000100:32, 16#00000100:32, NbCustomNPCs:32/little, 0:704 >>,
	%% Wrapping it up.
	Data = << MainPos:32/little, 0:32, TmpFlagsBin/binary, ValFlagsBin/binary, BoolFlagsBin/binary,
		ItemsBin/binary, ItemsPtrs/binary, SetsBin/binary, ZonesBin/binary, WarpsBin/binary,
		TmpFlagsPtrs/binary, ValFlagsPtrs/binary, BoolFlagsPtrs/binary, MainBin/binary, NPCsBin/binary >>,
	Size = 8 + byte_size(Data),
	Data2 = << $N, $X, $R, 0, Size:32/little, Data/binary >>,
	%% Calculate the pointers and return.
	L0 = [ItemsPtrsPos] ++ SetsPtrsList,
	L1 = L0 ++ lists:seq(ZonesPos + 16, ZonesPos + 16 + 64 * (NbZones - 1), 64),
	L2 = L1 ++ lists:seq(TmpFlagsPtrsPos, TmpFlagsPtrsPos + 4 * (NbTmpFlags + NbValFlags + NbBoolFlags - 1), 4),
	L3 = L2 ++ [MainPos + 12, MainPos + 20, MainPos + 104],
	L4 = if TmpFlagsPtrsPos2 =/= 0 -> L3 ++ [MainPos + 140]; true -> L3 end,
	L5 = if ValFlagsPtrsPos2 =/= 0 -> L4 ++ [MainPos + 144]; true -> L4 end,
	L6 = if BoolFlagsPtrsPos2 =/= 0 -> L5 ++ [MainPos + 148]; true -> L5 end,
	{Data2, L6 ++ [MainPos + 156]}.

load_quest_xnr_flag_ptrs([], _N) ->
	{0, << >>};
load_quest_xnr_flag_ptrs(FlagsList, N) ->
	NbFlags = length(FlagsList),
	L1 = lists:seq(16 + N * 16, 16 + N * 16 + (NbFlags - 1) * 16, 16),
	L2 = [<< X:32/little >> || X <- L1],
	{NbFlags, iolist_to_binary(L2)}.

load_quest_xnr_flags(FlagsList) ->
	load_quest_xnr_flags(FlagsList, []).
load_quest_xnr_flags([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
load_quest_xnr_flags([Flag|Tail], Acc) ->
	L = length(Flag),
	Padding = 8 * (16 - L),
	FlagBin = list_to_binary(Flag),
	Bin = << FlagBin/binary, 0:Padding >>,
	load_quest_xnr_flags(Tail, [Bin|Acc]).

load_quest_xnr_items(ItemsList) ->
	load_quest_xnr_items(ItemsList, []).
load_quest_xnr_items([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
load_quest_xnr_items([Item|Tail], Acc) ->
	Index = proplists:get_value(index, Item),
	ItemID = proplists:get_value(itemid, Item),
	NbItems = proplists:get_value(nb_items, Item),
	Type = proplists:get_value(type, Item),
	Money = proplists:get_value(money, Item),
	Bin = << Index:8, ItemID:32, 0:16, NbItems:8, Type:32/little, Money:32/little >>,
	load_quest_xnr_items(Tail, [Bin|Acc]).

load_quest_xnr_warp(QuestID, {WarpQuestID, WarpZoneID, WarpMapID, WarpEntryID}) ->
	WarpQuestID2 = if WarpQuestID =:= QuestID -> 16#ffffffff; true -> WarpQuestID end,
	<< WarpQuestID2:32/little, WarpZoneID:16/little, WarpMapID:16/little, WarpEntryID:16/little, 0:16 >>.

load_quest_xnr_warps(QuestID, WarpsList) ->
	load_quest_xnr_warps(QuestID, WarpsList, []).
load_quest_xnr_warps(_QuestID, [], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
load_quest_xnr_warps(QuestID, [Warp|Tail], Acc) ->
	{CurrentWarp, NextWarp} = Warp,
	Bin1 = load_quest_xnr_warp(QuestID, CurrentWarp),
	Bin2 = load_quest_xnr_warp(QuestID, NextWarp),
	load_quest_xnr_warps(QuestID, Tail, [<< Bin1/binary, Bin2/binary >>|Acc]).

%% @todo Counter(16#7fffffff) has ffff before EnemyLevel, why?
%% @todo Spaceport(1104000) and counter(16#7fffffff) has 04010000 be 00010000, why?
load_quest_xnr_zones(ZonesList, BasePos) ->
	load_quest_xnr_zones(ZonesList, BasePos, [], [], []).
load_quest_xnr_zones([], _BasePos, SetsAcc, SetsPtrsAcc, ZonesAcc) ->
	SetsBin = iolist_to_binary(lists:reverse(SetsAcc)),
	SetsPtrsList = lists:flatten(lists:reverse(SetsPtrsAcc)),
	ZonesBin = iolist_to_binary(lists:reverse(ZonesAcc)),
	{SetsBin, SetsPtrsList, ZonesBin};
load_quest_xnr_zones([{ZoneID, ZoneParams}|Tail], BasePos, SetsAcc, SetsPtrsAcc, ZonesAcc) ->
	AreaID = proplists:get_value(areaid, ZoneParams),
	EnemyLevel = proplists:get_value(enemy_level, ZoneParams),
	SetList = proplists:get_value(sets, ZoneParams),
	NbSets = length(SetList),
	SetsBin = iolist_to_binary([<< Set:32/little >> || Set <- SetList]),
	SetsBin2 = << SetsBin/binary, BasePos:32/little, NbSets:32/little >>,
	SetPos = BasePos + NbSets * 4,
	ZoneBin = << ZoneID:16/little, AreaID:16/little, AreaID:32/little, 0:16, EnemyLevel:8, 16#ff:8, 16#04010000:32, SetPos:32/little, 0:352 >>,
	load_quest_xnr_zones(Tail, BasePos + byte_size(SetsBin2), [SetsBin2|SetsAcc], [SetPos|SetsPtrsAcc], [ZoneBin|ZonesAcc]).

%% @doc Load a script file and compile it into the bytecode used by the game.
load_script_bin(ScriptFilename) ->
	{ok, Script} = file:read_file(ScriptFilename),
	{ok, Tokens, _NbLines} = egs_script_lexer:string(binary_to_list(Script)),
	{ok, ParseTree} = egs_script_parser:parse(Tokens),
	egs_script_compiler:compile(ParseTree).

%% @doc Load a set configuration file and return a set_r*.rel binary.
load_set_rel(ConfFilename, AreaID, Maps, FilePos) ->
	{ok, Settings} = file:consult(ConfFilename),
	NbMaps = length(Maps),
	AreaPtr = FilePos + 16,
	MapsPtr = FilePos + 24,
	{MapsBin, Ptrs} = load_set_rel_maps(Settings, Maps, MapsPtr, MapsPtr + NbMaps * 12),
	Ptrs2 = [FilePos + 20|Ptrs],
	Size = byte_size(MapsBin) + 24,
	{<< $N, $X, $R, 0, Size:32/little, AreaPtr:32/little, 0:32,
		AreaID:16/little, NbMaps:16/little, MapsPtr:32/little,
		MapsBin/binary >>, Ptrs2}.

load_set_rel_maps(Settings, Maps, MapsPos, GroupsPos) ->
	load_set_rel_maps(Settings, Maps, MapsPos, GroupsPos, [], [], []).
load_set_rel_maps(_Settings, [], _MapsPos, _GroupsPos, Ptrs, GroupsAcc, MapsAcc) ->
	ObjectsBin = iolist_to_binary(lists:reverse(GroupsAcc)),
	MapsBin = iolist_to_binary(lists:reverse(MapsAcc)),
	{<< MapsBin/binary, ObjectsBin/binary >>, lists:sort(Ptrs)};
load_set_rel_maps(Settings, [MapID|Tail], MapsPos, GroupsPos, Ptrs, GroupsAcc, MapsAcc) ->
	Map = proplists:get_value({map, MapID}, Settings),
	NbGroups = length(Map),
	{GroupsBin, Ptrs2} = load_set_rel_groups(Map, GroupsPos, GroupsPos + NbGroups * 40, Ptrs),
	MapBin = << MapID:16/little, NbGroups:16/little, GroupsPos:32/little, 0:32 >>,
	load_set_rel_maps(Settings, Tail, MapsPos + 12, GroupsPos + byte_size(GroupsBin), [MapsPos + 4|Ptrs2], [GroupsBin|GroupsAcc], [MapBin|MapsAcc]).

%% @todo 0:144, 16#ffff:16 0:32 can have some values.
load_set_rel_groups(Groups, GroupsPos, ObjectsPos, Ptrs) ->
	load_set_rel_groups(Groups, GroupsPos, ObjectsPos, Ptrs, 0, [], []).
load_set_rel_groups([], _GroupsPos, _ObjectsPos, Ptrs, _N, ObjectsAcc, GroupsAcc) ->
	ObjectsBin = iolist_to_binary(lists:reverse(ObjectsAcc)),
	GroupsBin = iolist_to_binary(lists:reverse(GroupsAcc)),
	{<< GroupsBin/binary, ObjectsBin/binary >>, Ptrs};
load_set_rel_groups([Group|Tail], GroupsPos, ObjectsPos, Ptrs, N, ObjectsAcc, GroupsAcc) ->
	NbObjects = length(Group),
	{ObjectsBin, Ptrs2} = load_set_rel_objects(Group, ObjectsPos, ObjectsPos + NbObjects * 52, Ptrs),
	GroupBin = << 16#ffffffff:32, 0:144, 16#ffff:16, 0:32, N:32/little, 0:16, NbObjects:16/little, ObjectsPos:32/little >>,
	load_set_rel_groups(Tail, GroupsPos + 40, ObjectsPos + byte_size(ObjectsBin), [GroupsPos + 36|Ptrs2], N + 1, [ObjectsBin|ObjectsAcc], [GroupBin|GroupsAcc]).

load_set_rel_objects(Objects, StdPos, ParamsPos, Ptrs) ->
	load_set_rel_objects(Objects, StdPos, ParamsPos, Ptrs, [], []).
load_set_rel_objects([], _StdPos, _ParamsPos, Ptrs, StdAcc, ParamsAcc) ->
	StdBin = iolist_to_binary(lists:reverse(StdAcc)),
	ParamsBin = iolist_to_binary(lists:reverse(ParamsAcc)),
	{<< StdBin/binary, ParamsBin/binary >>, Ptrs};
load_set_rel_objects([{ObjType, ObjPos, ObjRot, ObjParams}|Tail], StdPos, ParamsPos, Ptrs, StdAcc, ParamsAcc) ->
	ParamsBin = load_set_rel_object_params(ObjType, ObjParams),
	ParamsSize = byte_size(ParamsBin),
	{ClassID, TypeID} = load_set_rel_object_id(ObjType),
	{PosX, PosY, PosZ} = ObjPos,
	{RotX, RotY, RotZ} = ObjRot,
	StdBin = <<	16#ffffffff:32, ClassID:32/little, 16#ffffffff:32, 16#ffff:16, TypeID:16/little, 0:32,
				PosX:32/little-float, PosY:32/little-float, PosZ:32/little-float,
				RotX:32/little-float, RotY:32/little-float, RotZ:32/little-float,
				ParamsSize:32/little, ParamsPos:32/little >>,
	load_set_rel_objects(Tail, StdPos + 52, ParamsPos + ParamsSize, [StdPos + 48|Ptrs], [StdBin|StdAcc], [ParamsBin|ParamsAcc]).

load_set_rel_object_id(static_model)    -> {4,  4};
load_set_rel_object_id(sensor)          -> {4,  9};
load_set_rel_object_id(invisible_block) -> {1, 10};
load_set_rel_object_id(npc)             -> {2, 18};
load_set_rel_object_id(door)            -> {5, 20};
load_set_rel_object_id(entrance)        -> {2, 26};
load_set_rel_object_id(exit)            -> {6, 27};
load_set_rel_object_id(label)           -> {2, 53};
load_set_rel_object_id(chair)           -> {2, 56};
load_set_rel_object_id(uni_cube)        -> {0, 60};
load_set_rel_object_id(pp_cube)         -> {0, 62};
load_set_rel_object_id({raw, ClassID, TypeID}) -> {ClassID, TypeID}.

load_set_rel_object_params(static_model, Params) ->
	Model = proplists:get_value(model, Params),
	Size = proplists:get_value(size, Params, 1.0),
	<<	Model:32/little, Size:32/little-float, 16#0000ff00:32,
			16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
			16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>;

%% @todo If 00010000 has a 01 at the end, the sensor doesn't have an associated function in the script.
%% @todo Not sure about the box. It's probably wrong.
load_set_rel_object_params(sensor, Params) ->
	ID = proplists:get_value(id, Params),
	{Rad, X, Y, Z} = proplists:get_value(box, Params),
	<<	Rad:32/little-float, X:32/little-float, Y:32/little-float, Z:32/little-float, 0:64, ID:32/little, 16#00010000:32, 0:96,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>;
load_set_rel_object_params(invisible_block, Params) ->
	{Width, Height, Depth} = proplists:get_value(dimension, Params),
	<<	Width:32/little-float, Height:32/little-float, Depth:32/little-float, 16#ffff0000:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, %% First ffffffff can be events required to enable (each being a 16-bit integer).
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>; %% First ffffffff can be events required to disable.
load_set_rel_object_params(npc, Params) ->
	Model = proplists:get_value(model, Params),
	ID = proplists:get_value(id, Params),
	{CanTalk, TalkRadius} = case proplists:get_value(talk_radius, Params) of undefined -> {0, 0.0}; T -> {1, T} end,
	{CanWalk, WalkRadius} = case proplists:get_value(walk_radius, Params) of undefined -> {0, 0.0}; W -> {1, W} end,
	<<	Model:16/little, ID:16/little, TalkRadius:32/little-float, WalkRadius:32/little-float,
		CanWalk:8, 0:24, CanTalk:8, 0:24, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>;
%% @todo The byte right after the first ffff can take the value 1.
%% @todo The 2 bytes starting the many ffffffff at the end can take an unknown value.
load_set_rel_object_params(door, Params) ->
	Model = proplists:get_value(model, Params),
	Variant = proplists:get_value(variant, Params, 255),
	IsClosed = case proplists:get_value(closed, Params, false) of true -> 1; false -> 0 end,
	<<	Model:32/little, IsClosed:8, 0:8, 16#ffff:16, 0:16, Variant:8, 0:8,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>;
%% @todo 16#2041 is unknown (10.0 float). Other unknown values following can be non-0. Probably entrance and camera movements.
load_set_rel_object_params(entrance, Params) ->
	EntryID = proplists:get_value(entryid, Params),
	<< EntryID:16/little, 0:32, 16#2041:16, 0:1088 >>;
%% @todo The 2 bytes after CameraMovZ can have unknown values.
load_set_rel_object_params(exit, Params) ->
	EntryID = proplists:get_value(entryid, Params),
	Animation = proplists:get_value(animation, Params),
	AnimationID = case Animation of stop -> 0; walk -> 1; run -> 2 end,
	{ExitBoxRad, ExitBoxW, ExitBoxH, ExitBoxD} = proplists:get_value(exit_box, Params),
	{ExitMovX, ExitMovY, ExitMovZ} = proplists:get_value(exit_movement, Params),
	{CameraBoxRad, CameraBoxW, CameraBoxH, CameraBoxD} = proplists:get_value(camera_box, Params),
	{CameraMovX, CameraMovY, CameraMovZ} = proplists:get_value(camera_movement, Params),
	CommonBin = <<	ExitBoxRad:32/little-float, ExitBoxW:32/little-float, ExitBoxH:32/little-float, ExitBoxD:32/little-float,
					ExitMovX:32/little-float, ExitMovY:32/little-float, ExitMovZ:32/little-float, 0:192,
					CameraBoxRad:32/little-float, CameraBoxW:32/little-float, CameraBoxH:32/little-float, CameraBoxD:32/little-float,
					CameraMovX:32/little-float, CameraMovY:32/little-float, CameraMovZ:32/little-float,
					16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>,
	case proplists:get_value(type, Params) of
		counter ->
			CounterID = proplists:get_value(counterid, Params),
			<< CounterID:16/little, 1:16/little, 0:16, AnimationID:8, 1:8, EntryID:8, 0:16, 1:8, CommonBin/binary >>;
		map ->
			<< EntryID:16/little, 0:32, AnimationID:8, 1:8, 255:8, 0:16, 1:8, CommonBin/binary >>
	end;
%% @todo Not sure about the box. It's probably wrong.
%% @todo Can only have up to 10 different label ids.
load_set_rel_object_params(label, Params) ->
	ID = proplists:get_value(id, Params),
	{Rad, X, Y, Z} = proplists:get_value(box, Params),
	<<	Rad:32/little-float, X:32/little-float, Y:32/little-float, Z:32/little-float, ID:8, 1:8, 0:16,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>;
%% @todo Many unknown values...
load_set_rel_object_params(chair, Params) ->
	ID = proplists:get_value(id, Params),
	ActionRadius = 1.0,
	UnknownA = -20.0,
	UnknownB = 30.0,
	UnknownC = 110.0,
	UnknownD = -6.0,
	StandUpMoveDistance = 13.0, %% If not big enough, can't stand up. 13.0 sounds good for all chairs so far.
	UnknownE = 94.0,
	<<	ID:32/little, ActionRadius:32/little-float, 0, 1, 1, 0, UnknownA:32/little-float, UnknownB:32/little-float,
		UnknownC:32/little-float, 0:32, UnknownD:32/little-float, StandUpMoveDistance:32/little-float, UnknownE:32/little-float >>;
%% @todo First ffff sometimes is 0, find out why.
load_set_rel_object_params(uni_cube, Params) ->
	I = proplists:get_value(i, Params),
	EntryID = proplists:get_value(entryid, Params),
	<<	I:8, EntryID:16/little, 0:8,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>;
%% @todo First ffff can be that same old unknown type of value.
%% @todo First byte set to 1 = hidden?
load_set_rel_object_params(pp_cube, _Params) ->
	<< 0:32, 16#ffffffff:32, 16#ffffffff:32 >>;
%% @doc Raw binary, used for testing.
load_set_rel_object_params({raw, _ClassID, _TypeID}, ParamsBin) ->
	ParamsBin.
%% @todo ghosts: NbGhosts:32, Width:32/float, Unknown:32/float

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

%% @doc Create the unit_title_table.rel file based on a quest configuration file and a starting file position.
%% The file position depends on the previously built quest.xnr.
load_unit_title_table_rel(ConfFilename, FilePos) ->
	{ok, Settings} = file:consult(ConfFilename),
	{Titles, NbTitles} = load_unit_title_table_rel_zones(proplists:get_value(zones, Settings)),
	MainPos = 16 + byte_size(Titles),
	Size = 8 + MainPos,
	TitlesPtr = FilePos + 16,
	Data = << $N, $X, $R, 0, Size:32/little, MainPos:32/little, 0:32, Titles/binary, TitlesPtr:32/little, NbTitles:32/little >>,
	{Data, [FilePos + 16 + byte_size(Titles)]}.

load_unit_title_table_rel_maps(ZoneID, Maps) ->
	load_unit_title_table_rel_maps(ZoneID, Maps, 0, []).
load_unit_title_table_rel_maps(_ZoneID, [], N, Acc) ->
	{iolist_to_binary(lists:reverse(Acc)), N};
load_unit_title_table_rel_maps(ZoneID, [MapID|Tail], N, Acc) ->
	Bin = << ZoneID:16/little, MapID:16/little, N:32/little >>,
	load_unit_title_table_rel_maps(ZoneID, Tail, N + 1, [Bin|Acc]).

load_unit_title_table_rel_zones(Zones) ->
	load_unit_title_table_rel_zones(Zones, 0, []).
load_unit_title_table_rel_zones([], N, Acc) ->
	{iolist_to_binary(lists:reverse(Acc)), N};
load_unit_title_table_rel_zones([{ZoneID, ZoneParams}|Tail], N, Acc) ->
	Maps = proplists:get_value(maps, ZoneParams),
	{Bin, N2} = load_unit_title_table_rel_maps(ZoneID, Maps),
	load_unit_title_table_rel_zones(Tail, N + N2, [Bin|Acc]).

%% @doc Pack an nbl file according to the given Options.
%%      Example usage: nbl:pack([{files, [{file, "table.rel", [16#184, 16#188, 16#1a0]}, {file, "text.bin", []}]}]).
%%      The data will be automatically compressed if the file is big enough to make it worth it.
%% @todo The 0010 value is unknown. If it was too low it would crash the client when it cleans up the nbl.
nbl_pack(Options) ->
	Files = proplists:get_value(files, Options),
	{Header, Data, DataSize, CompressedDataSize, PtrArray, PtrArraySize} = nbl_pack_files(Files),
	NbFiles = length(Files),
	HeaderSize = 16#30 + 16#60 * NbFiles,
	EncryptSeed = 0,
	<<	$N, $M, $L, $L, 2:16/little, 16#0010:16, HeaderSize:32/little, NbFiles:32/little,
		DataSize:32/little, CompressedDataSize:32/little, PtrArraySize:32/little, EncryptSeed:32/little,
		0:128, Header/binary, Data/binary, PtrArray/binary >>.

%% @doc Pack a list of files and return the header, data and pointer array parts.
nbl_pack_files(Files) ->
	nbl_pack_files(Files, {[], [], [], 0, 0}).
nbl_pack_files([], {AccH, AccD, AccP, _FilePos, _PtrIndex}) ->
	BinH = iolist_to_binary(lists:reverse(AccH)),
	PaddingH = 8 * (16#7d0 - (byte_size(BinH) rem 16#800)),
	PaddingH2 = if PaddingH =< 0 -> 16#800 + PaddingH; true -> PaddingH end,
	BinD = iolist_to_binary(lists:reverse(AccD)),
	BinDSize = byte_size(BinD),
	{BinD3, CompressedDataSize} = if BinDSize < 16#800 ->
			{BinD, 0};
		true ->
			BinD2 = egs_prs:compress(BinD),
			BinD2Size = byte_size(BinD2),
			{BinD2, BinD2Size}
	end,
	PaddingD = 8 * (16#800 - (byte_size(BinD3) rem 16#800)),
	PaddingD2 = if PaddingD =:= 8 * 16#800 -> 0; true -> PaddingD end,
	BinP = iolist_to_binary(lists:reverse(AccP)),
	PtrSize = byte_size(BinP),
	PtrArray = case PtrSize of
		0 -> << >>;
		_ ->
			PaddingP = 8 * (16#800 - (byte_size(BinP) rem 16#800)),
			<< BinP/binary, 0:PaddingP >>
	end,
	{<< BinH/binary, 0:PaddingH2 >>, << BinD3/binary, 0:PaddingD2 >>, BinDSize, CompressedDataSize, PtrArray, PtrSize};
nbl_pack_files([{data, Filename, Data, PtrList}|Tail], {AccH, AccD, AccP, FilePos, PtrIndex}) ->
	ID = case filename:extension(Filename) of
		".bin" -> << $S, $T, $D, 0 >>;
		[$.|String] -> list_to_binary(string:to_upper(String ++ [0]))
	end,
	FilenameBin = iolist_to_binary(Filename),
	FilenamePaddingBits = 8 * (32 - byte_size(FilenameBin)),
	DataSize = byte_size(Data),
	PaddedSize = nbl_padded_size(DataSize),
	DataPaddingBits = 8 * (PaddedSize - DataSize),
	PtrSize = 4 * length(PtrList),
	BinD = << Data/binary, 0:DataPaddingBits >>,
	BinH = << ID/binary, 16#60000000:32, 0:64, FilenameBin/binary, 0:FilenamePaddingBits,
		FilePos:32/little, DataSize:32/little, PtrIndex:32/little, PtrSize:32/little >>,
	NXIF = case filename:extension(Filename) of
		".bin" -> << 0:256 >>;
		_ -> nbl_pack_nxif(DataSize, PtrSize)
	end,
	BinH2 = << BinH/binary, NXIF/binary >>,
	BinP = iolist_to_binary([ << Ptr:32/little >> || Ptr <- PtrList]),
	nbl_pack_files(Tail, {[BinH2|AccH], [BinD|AccD], [BinP|AccP], FilePos + PaddedSize, PtrIndex + PtrSize});
nbl_pack_files([{file, Filename, PtrList}|Tail], Acc) ->
	io:format("~p~n", [Filename]),
	{ok, Data} = file:read_file(Filename),
	nbl_pack_files([{data, Filename, Data, PtrList}|Tail], Acc).

%% @doc Return an NXIF chunk data for a specific data and pointer array size.
nbl_pack_nxif(DataSize, PtrSize) ->
	DataSize2 = DataSize + 16#20,
	PtrSize2 = PtrSize + 16#20 - (PtrSize rem 16#20),
	<< $N, $X, $I, $F, 16#18000000:32, 16#01000000:32, 16#20000000:32,
		DataSize:32/little, DataSize2:32/little, PtrSize2:32/little, 16#01000000:32 >>.

%% @doc Return the padded size of a file to be packed in an nbl archive.
nbl_padded_size(Size) ->
	Size + 16#20 - (Size rem 16#20).

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Handle game clients.
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

-module(psu_game).
-compile(export_all). %% @todo Temporarily export all until send_xxxx functions are moved to psu_proto.

-include("include/records.hrl").
-include("include/maps.hrl").
-include("include/missions.hrl").
-include("include/psu/items.hrl").
-include("include/psu/npc.hrl").

%% @doc Load and send the character information to the client.
%% @todo Should wait for the 021c reply before doing area_change.
%% @todo Move this whole function directly to psu_proto, probably.
char_load(User) ->
	State = #state{socket=User#egs_user_model.socket, gid=User#egs_user_model.id, lid=User#egs_user_model.lid},
	send_0d01(User),
	% 0246
	send_0a0a((User#egs_user_model.character)#characters.inventory),
	psu_proto:send_1006(5, 0, State), %% @todo The 0 here is PartyPos, save it in User.
	send_1005((User#egs_user_model.character)#characters.name),
	psu_proto:send_1006(12, State),
	psu_proto:send_0210(State),
	send_0222(),
	send_1500(User),
	send_1501(),
	send_1512(),
	% 0303
	send_1602(),
	psu_proto:send_021b(State).

%% @doc Return the current season information.
area_get_season(QuestID) ->
	{{_, Month, Day}, _} = calendar:universal_time(),
	[IsSeasonal, SeasonID, SeasonQuestIDs] = if
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
	if	IsSeasonal =:= 1 ->
			case lists:member(QuestID, SeasonQuestIDs) of
				true  -> [{status, IsSeasonal}, {season, SeasonID}];
				false -> [{status, 0}, {season, 255}]
			end;
		true ->
			[{status, 0}, {season, 255}]
	end.

%% @doc Load the given map as a standard lobby.
area_load(QuestID, ZoneID, MapID, EntryID, State) ->
	{ok, OldUser} = egs_user_model:read(get(gid)),
	[{type, AreaType}, {file, QuestFile}|MissionInfo] = proplists:get_value(QuestID, ?QUESTS, [{type, undefined}, {file, undefined}]),
	[IsStart, RealZoneID, RealMapID, RealEntryID, NbSetsInQuest] = case AreaType of
		mission ->
			if	ZoneID =:= 65535 ->
					[{start, [TmpZoneID, TmpMapID, TmpEntryID]}, {sets, TmpNbSets}] = MissionInfo,
					[true, TmpZoneID, TmpMapID, TmpEntryID, TmpNbSets];
				true -> [false, ZoneID, MapID, EntryID, ignored]
			end;
		myroom ->
			if	ZoneID =:= 0 ->
					[false, 0, 423, EntryID, ignored];
				true -> [false, ZoneID, MapID, EntryID, ignored]
			end;
		_ ->
			[false, ZoneID, MapID, EntryID, ignored]
	end,
	[{file, ZoneFile}|ZoneSetInfo] = proplists:get_value([QuestID, RealZoneID], ?ZONES, [{file, undefined}]),
	NbSetsInZone = case ZoneSetInfo of [] -> 1; [{sets, TmpNbSetsInZone}] -> TmpNbSetsInZone end,
	if	AreaType =:= myroom ->
			AreaName = "Your Room";
		true ->
			[{name, AreaName}] = proplists:get_value([QuestID, RealMapID], ?MAPS, [{name, "dammy"}])
	end,
	{InstancePid, SetID} = if IsStart =:= true -> % initialize the mission
			Zones = proplists:get_value(QuestID, ?MISSIONS),
			{ok, RetPid} = psu_instance:start_link(Zones),
			RetSetID = crypto:rand_uniform(0, NbSetsInQuest),
			{RetPid, RetSetID};
		true -> {OldUser#egs_user_model.instancepid, OldUser#egs_user_model.setid}
	end,
	User = OldUser#egs_user_model{instancepid=InstancePid, areatype=AreaType, area={psu_area, QuestID, RealZoneID, RealMapID}, entryid=RealEntryID},
	egs_user_model:write(User),
	RealSetID = if SetID > NbSetsInZone - 1 -> NbSetsInZone - 1; true -> SetID end,
	area_load(AreaType, IsStart, RealSetID, OldUser, User, QuestFile, ZoneFile, AreaName, State).

area_load(AreaType, IsStart, SetID, OldUser, User, QuestFile, ZoneFile, AreaName, State) ->
	#psu_area{questid=OldQuestID, zoneid=OldZoneID} = OldUser#egs_user_model.area,
	#psu_area{questid=QuestID, zoneid=ZoneID, mapid=_MapID} = User#egs_user_model.area,
	QuestChange = if OldQuestID /= QuestID, QuestFile /= undefined -> true; true -> false end,
	if	ZoneFile =:= undefined ->
			ZoneChange = false;
		true ->
			ZoneChange = if OldQuestID =:= QuestID, OldZoneID =:= ZoneID -> false; true -> true end
	end,
	[{status, IsSeasonal}, {season, SeasonID}] = area_get_season(QuestID),
	% broadcast spawn and unspawn to other people
	{ok, UnspawnList} = egs_user_model:select({neighbors, OldUser}),
	{ok, SpawnList} = egs_user_model:select({neighbors, User}),
	lists:foreach(fun(Other) -> Other#egs_user_model.pid ! {egs, player_unspawn, User} end, UnspawnList),
	if	AreaType =:= lobby ->
			lists:foreach(fun(Other) -> Other#egs_user_model.pid ! {egs, player_spawn, User} end, SpawnList);
		true -> ignore
	end,
	% load area
	if	QuestChange =:= true ->
			% load new quest
			psu_proto:send_0c00(User, State),
			psu_proto:send_020e(QuestFile, State);
		true -> ignore
	end,
	%% @todo The LID changes here.
	if	IsStart =:= true ->
			psu_proto:send_0215(16#ffffffff, State);
		true -> ignore
	end,
	if	ZoneChange =:= true ->
			% load new zone
			psu_proto:send_0a05(State),
			if AreaType =:= lobby ->
					psu_proto:send_0111(User#egs_user_model{lid=0}, 6, State);
				true -> ignore
			end,
			psu_proto:send_010d(User#egs_user_model{lid=0}, State),
			psu_proto:send_0200(ZoneID, AreaType, State),
			psu_proto:send_020f(ZoneFile, SetID, SeasonID, State);
		true -> ignore
	end,
	State2 = State#state{areanb=State#state.areanb + 1},
	psu_proto:send_0205(User, IsSeasonal, State2),
	psu_proto:send_100e(User#egs_user_model.area, User#egs_user_model.entryid, AreaName, State2),
	if	AreaType =:= mission ->
			psu_proto:send_0215(0, State2),
			if	IsStart =:= true ->
					psu_proto:send_0215(0, State2),
					send_0c09();
				true -> ignore
			end;
		true ->
			psu_proto:send_020c(State2)
	end,
	if	ZoneChange =:= true ->
			case AreaType of
				myroom ->
					send_1332(),
					send_1202(),
					psu_proto:send_1204(State2),
					send_1206();
				mission ->
					send_1202(),
					psu_proto:send_1204(State2),
					send_1206(),
					send_1207();
				_ -> ignore
			end;
		true -> ignore
	end,
	if	ZoneChange =:= true, AreaType =/= spaceport ->
			send_1212();
		true -> ignore
	end,
	if	AreaType =:= myroom ->
			send_1309();
		true -> ignore
	end,
	psu_proto:send_0201(User#egs_user_model{lid=0}, State2),
	if	ZoneChange =:= true ->
			send_0a06();
		true -> ignore
	end,
	send_0233(SpawnList),
	case User#egs_user_model.partypid of
		undefined -> ignore;
		_ -> send_022c(0, 16#12)
	end,
	State3 = State2#state{areanb=State2#state.areanb + 1},
	psu_proto:send_0208(State3),
	psu_proto:send_0236(State3),
	if	User#egs_user_model.partypid =/= undefined, AreaType =:= mission ->
			{ok, NPCList} = psu_party:get_npc(User#egs_user_model.partypid),
			npc_load(User, NPCList);
		true -> ok
	end,
	{ok, State3}.

%% @todo Don't change the NPC info unless you are the leader!
npc_load(_Leader, []) ->
	ok;
npc_load(Leader, [{PartyPos, NPCGID}|NPCList]) ->
	State = #state{socket=Leader#egs_user_model.socket, gid=Leader#egs_user_model.id, lid=Leader#egs_user_model.lid},
	{ok, OldNPCUser} = egs_user_model:read(NPCGID),
	#egs_user_model{instancepid=InstancePid, area=Area, entryid=EntryID, pos=Pos} = Leader,
	NPCUser = OldNPCUser#egs_user_model{lid=PartyPos, instancepid=InstancePid, areatype=mission, area=Area, entryid=EntryID, pos=Pos},
	%% @todo This one on mission end/abort?
	%~ OldNPCUser#egs_user_model{lid=PartyPos, instancepid=undefined, areatype=AreaType, area={psu_area, 0, 0, 0}, entryid=0, pos={pos, 0.0, 0.0, 0.0, 0}}
	egs_user_model:write(NPCUser),
	psu_proto:send_010d(NPCUser, State),
	psu_proto:send_0201(NPCUser, State),
	psu_proto:send_0215(0, State),
	send_0a04(NPCUser#egs_user_model.id),
	send_1004(npc_mission, NPCUser, PartyPos),
	send_100f((NPCUser#egs_user_model.character)#characters.npcid, PartyPos),
	send_1601(PartyPos),
	send_1016(PartyPos),
	npc_load(Leader, NPCList).

%% @doc Build the packet header.
header(Command) ->
	GID = get(gid),
	<< Command:16/unsigned-integer, 16#0300:16, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64 >>.

%% @doc Send the given packet to the client.
%% @todo Consolidate the receive and send functions better.
send(Packet) ->
	psu_proto:packet_send(get(socket), Packet).

%% @doc Send a shop listing.
send_010a(ItemsList) ->
	GID = get(gid),
	NbItems = length(ItemsList),
	ItemsBin = build_010a_list(ItemsList, []),
	send(<< 16#010a0300:32, 0:64, GID:32/little, 0:64, 16#00011300:32, GID:32/little, 0:64,
		GID:32/little, 0:32, 1:16/little, NbItems:8, 2:8, 0:32, ItemsBin/binary >>).

%% @todo The values set to 0 are unknown.
build_010a_list([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
build_010a_list([ItemID|Tail], Acc) ->
	#psu_item{name=Name, rarity=Rarity, buy_price=SellPrice, data=Data} = proplists:get_value(ItemID, ?ITEMS),
	UCS2Name = << << X:8, 0:8 >> || X <- Name >>,
	NamePadding = 8 * (46 - byte_size(UCS2Name)),
	RarityBin = Rarity - 1,
	DataBin = build_item_constants(Data),
	BinItemID = case element(1, Data) of
		psu_clothing_item -> %% Change the ItemID to enable all colors.
			<< A:8, _:4, B:12, _:8 >> = << ItemID:32 >>,
			<< A:8, 3:4, B:12, 16#ff:8 >>;
		_Any ->
			<< ItemID:32 >>
	end,
	Bin = << UCS2Name/binary, 0:NamePadding, RarityBin:8, 0:8, BinItemID/binary, SellPrice:32/little, DataBin/binary >>,
	build_010a_list(Tail, [Bin|Acc]).

%% @todo Types capability list.
send_0113() ->
	{ok, File} = file:read_file("p/typesinfo.bin"),
	GID = get(gid),
	send(<< 16#01130300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, GID:32/little-unsigned-integer, File/binary >>).

%% @todo Not sure. Used for unspawning, and more.
send_0204(DestUser, TargetUser, Action) ->
	DestGID = DestUser#egs_user_model.id,
	TargetTypeID = case (TargetUser#egs_user_model.character)#characters.type of
		npc -> 16#00001d00;
		_ -> 16#00001200
	end,
	#egs_user_model{id=TargetGID, lid=TargetLID} = TargetUser,
	send(<< 16#02040300:32, 0:32, TargetTypeID:32, TargetGID:32/little-unsigned-integer, 0:64,
		16#00011300:32, DestGID:32/little-unsigned-integer, 0:64, TargetGID:32/little-unsigned-integer,
		TargetLID:32/little-unsigned-integer, Action:32/little-unsigned-integer >>).

%% @doc Send the list of available universes.
send_021e() ->
	{ok, Count} = egs_user_model:count(),
	[StrCount] = io_lib:format("~b", [Count]),
	Unis = [{16#ffffffff, center, "Your Room", ""}, {1, justify, "Reload", "     "}, {2, justify, "EGS Test", StrCount}],
	NbUnis = length(Unis),
	Bin = send_021e_build(Unis, []),
	send(<< 16#021e0300:32, 0:288, NbUnis:32/little-unsigned-integer, Bin/binary >>).

send_021e_build([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
send_021e_build([{ID, Align, Name, Pop}|Tail], Acc) ->
	UCS2Name = << << X:8, 0:8 >> || X <- Name >>,
	UCS2Pop = << << X:8, 0:8 >> || X <- Pop >>,
	NamePadding = 8 * (32 - byte_size(UCS2Name)),
	PopPadding = 8 * (12 - byte_size(UCS2Pop)),
	IntAlign = case Align of justify -> 643; center -> 0 end,
	send_021e_build(Tail, [<< ID:32/little-unsigned-integer, 0:16, IntAlign:16/little-unsigned-integer, UCS2Name/binary, 0:NamePadding, UCS2Pop/binary, 0:PopPadding >>|Acc]).

%% @doc Send the current universe name and number.
%% @todo Currently only have universe number 2, named EGS Test.
%% @todo We must have a parameter indicating whether this is a room or a normal universe.
send_0222() ->
	UCS2Name = << << X:8, 0:8 >> || X <- "EGS Test" >>,
	Padding = 8 * (44 - byte_size(UCS2Name)),
	UniID = 2,
	GID = get(gid),
	send(<< 16#02220300:32, 16#ffff:16, 0:16, 16#00001200:32, GID:32/little, 0:64, 16#00011300:32, GID:32/little, 0:64,
		UniID:32/little, 16#01009e02:32, UCS2Name/binary, 0:Padding, 16#aa000000:32 >>).

%% @todo No idea!
send_022c(A, B) ->
	send(<< (header(16#022c))/binary, A:16/little-unsigned-integer, B:16/little-unsigned-integer >>).

%% @todo Not sure. Sent when going to or from room.
send_0230() ->
	GID = get(gid),
	send(<< 16#02300300:32, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64 >>).

%% @todo Figure out what the other things are.
send_0233(Users) ->
	NbUsers = length(Users),
	case NbUsers of
		0 ->
			ignore;
		_ ->
			GID = get(gid),
			Header = << 16#02330300:32, 0:32, 16#00001200:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32,
				GID:32/little-unsigned-integer, 0:64, NbUsers:32/little-unsigned-integer >>,
			Contents = build_0233_contents(Users),
			send(<< Header/binary, Contents/binary >>)
	end.

%% @todo God this function is ugly. Use tail recursion!
build_0233_contents([]) ->
	<< >>;
build_0233_contents(Users) ->
	[User|Rest] = Users,
	LID = 16#010000 + User#egs_user_model.lid, %% @todo The LID must be 16 bits and 0233 seems to (almost always) require that 01 right there...
	CharBin = psu_characters:character_user_to_binary(User#egs_user_model{lid=LID}),
	IsGM = 0,
	GameVersion = 0,
	Chunk = << CharBin/binary, IsGM:8, 0:8, GameVersion:8, 0:8 >>,
	Next = build_0233_contents(Rest),
	<< Chunk/binary, Next/binary >>.

%% @doc Send a chat command.
send_0304(FromTypeID, FromGID, FromName, Modifiers, Message) ->
	{chat_modifiers, ChatType, ChatCutIn, ChatCutInAngle, ChatMsgLength, ChatChannel, ChatCharacterType} = Modifiers,
	send(<< 16#03040300:32, 0:288, FromTypeID:32/unsigned-integer, FromGID:32/little-unsigned-integer, 0:64,
		ChatType:8, ChatCutIn:8, ChatCutInAngle:8, ChatMsgLength:8, ChatChannel:8, ChatCharacterType:8, 0:16, FromName:512/bits, Message/bits >>).

%% @todo Force send a new player location. Used for warps.
%% @todo The value before IntDir seems to be the player's current animation. 01 stand up, 08 ?, 17 normal sit
send_0503(#pos{x=PrevX, y=PrevY, z=PrevZ, dir=_}) ->
	{ok, User} = egs_user_model:read(get(gid)),
	#egs_user_model{id=GID, pos=#pos{x=X, y=Y, z=Z, dir=Dir}, area=#psu_area{questid=QuestID, zoneid=ZoneID, mapid=MapID}, entryid=EntryID} = User,
	IntDir = trunc(Dir * 182.0416),
	send(<< 16#05030300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, GID:32/little-unsigned-integer, 0:32,
		16#1000:16, IntDir:16/little-unsigned-integer, PrevX:32/little-float, PrevY:32/little-float, PrevZ:32/little-float, X:32/little-float, Y:32/little-float, Z:32/little-float,
		QuestID:32/little-unsigned-integer, ZoneID:32/little-unsigned-integer, MapID:32/little-unsigned-integer, EntryID:32/little-unsigned-integer, 1:32/little-unsigned-integer >>).

%% @todo NPC inventory. Guessing it's only for NPC characters...
send_0a04(NPCGID) ->
	GID = get(gid),
	{ok, Bin} = file:read_file("p/packet0a04.bin"),
	send(<< 16#0a040300:32, 0:32, 16#00001d00:32, NPCGID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Bin/binary >>).

%% @todo Inventory related. Figure out everything in this packet and handle it correctly.
%% @todo It sends 60 values so it's probably some kind of options for all 60 items in the inventory?
send_0a06() ->
	{ok, << _:32, A:96/bits, _:32, B:96/bits, _:32, C:1440/bits, _:32, D/bits >>} = file:read_file("p/packet0a06.bin"),
	GID = get(gid),
	send(<< A/binary, GID:32/little-unsigned-integer, B/binary, GID:32/little-unsigned-integer, C/binary, GID:32/little-unsigned-integer, D/binary >>).

%% @todo Handle more than just goggles.
send_0a0a(Inventory) ->
	{ok, << _:68608/bits, Rest/bits >>} = file:read_file("p/packet0a0a.bin"),
	GID = get(gid),
	NbItems = length(Inventory),
	ItemVariables = build_0a0a_item_variables(Inventory, []),
	ItemConstants = build_0a0a_item_constants(Inventory, []),
	send(<< 16#0a0a0300:32, 16#ffff:16, 0:144, 16#00011300:32, GID:32/little, 0:64, NbItems:8, 0:8, 6:8, 0:72, 0:192, 0:2304, ItemVariables/binary, ItemConstants/binary, 0:13824, Rest/binary >>).

%% @todo That ItemUUID have to be handled properly.
build_0a0a_item_variables([], Acc) ->
	Bin = iolist_to_binary(lists:reverse(Acc)),
	Padding = 17280 - 8 * byte_size(Bin),
	<< Bin/binary, 0:Padding >>;
build_0a0a_item_variables([{ItemID, Variables}|Tail], Acc) ->
	build_0a0a_item_variables(Tail, [build_item_variables(ItemID, 0, Variables)|Acc]).

build_item_variables(ItemID, ItemUUID, #psu_clothing_item_variables{color=ColorNb}) ->
	#psu_item{rarity=Rarity, data=#psu_clothing_item{colors=ColorsBin}} = proplists:get_value(ItemID, ?ITEMS),
	RarityInt = Rarity - 1,
	ColorInt = if ColorNb < 5 -> ColorNb; true -> 16#10 + ColorNb - 5 end,
	Bits = ColorNb * 8,
	<< _Before:Bits, ColorA:4, ColorB:4, _After/bits >> = ColorsBin,
	<< 0:32, ItemUUID:32/little, ItemID:32, 0:88, RarityInt:8, ColorA:8, ColorB:8, ColorInt:8, 0:72 >>;
build_item_variables(ItemID, ItemUUID, #psu_consumable_item_variables{quantity=Quantity}) ->
	#psu_item{rarity=Rarity, data=#psu_consumable_item{max_quantity=MaxQuantity, action=Action}} = proplists:get_value(ItemID, ?ITEMS),
	RarityInt = Rarity - 1,
	<< 0:32, ItemUUID:32/little, ItemID:32, Quantity:32/little, MaxQuantity:32/little, 0:24, RarityInt:8, Action:8, 0:88 >>;
build_item_variables(ItemID, ItemUUID, #psu_parts_item_variables{}) ->
	#psu_item{rarity=Rarity} = proplists:get_value(ItemID, ?ITEMS),
	RarityInt = Rarity - 1,
	<< 0:32, ItemUUID:32/little, ItemID:32, 0:88, RarityInt:8, 0:96 >>;
%% @todo Handle rank, rarity and hands properly.
build_item_variables(ItemID, ItemUUID, Variables) when element(1, Variables) =:= psu_striking_weapon_item_variables ->
	#psu_striking_weapon_item_variables{is_active=IsActive, slot=Slot, current_pp=CurrentPP, max_pp=MaxPP,
		element=#psu_element{type=EleType, percent=ElePercent}, pa=#psu_pa{type=PAType, level=PALevel}} = Variables,
	Rank = 4,
	Grind = 0,
	Rarity = 14, %% Rarity - 1
	Hand = both,
	<< _:8, WeaponType:8, _:16 >> = << ItemID:32 >>,
	HandBin = case Hand of
		both -> << 16#0000:16 >>;
		_ -> error
	end,
	<< IsActive:8, Slot:8, 0:16, ItemUUID:32/little, ItemID:32, 0:32, CurrentPP:16/little, MaxPP:16/little, 0:16, %% @todo What's this 0:16?
		Grind:4, Rank:4, Rarity:8, EleType:8, ElePercent:8, HandBin/binary, WeaponType:8, PAType:8, PALevel:8, 0:40 >>;
build_item_variables(ItemID, ItemUUID, #psu_special_item_variables{}) ->
	Action = case ItemID of
		16#11010000 -> << 16#12020100:32 >>;
		16#11020000 -> << 16#15000000:32 >>;
		16#11020100 -> << 0:32 >>;
		16#11020200 -> << 0:32 >>
	end,
	<< 0:32, ItemUUID:32/little, ItemID:32, 0:24, 16#80:8, 0:56, 16#80:8, 0:32, Action/binary, 0:32 >>;
build_item_variables(ItemID, ItemUUID, #psu_trap_item_variables{quantity=Quantity}) ->
	#psu_item{rarity=Rarity, data=#psu_trap_item{max_quantity=MaxQuantity}} = proplists:get_value(ItemID, ?ITEMS),
	RarityInt = Rarity - 1,
	<< 0:32, ItemUUID:32/little, ItemID:32, Quantity:32/little, MaxQuantity:32/little, 0:24, RarityInt:8, 0:96 >>.

build_0a0a_item_constants([], Acc) ->
	Bin = iolist_to_binary(lists:reverse(Acc)),
	Padding = 34560 - 8 * byte_size(Bin),
	<< Bin/binary, 0:Padding >>;
build_0a0a_item_constants([{ItemID, _Variables}|Tail], Acc) ->
	#psu_item{name=Name, rarity=Rarity, sell_price=SellPrice, data=Data} = proplists:get_value(ItemID, ?ITEMS),
	UCS2Name = << << X:8, 0:8 >> || X <- Name >>,
	NamePadding = 8 * (46 - byte_size(UCS2Name)),
	<< Category:8, _:24 >> = << ItemID:32 >>,
	DataBin = build_item_constants(Data),
	RarityInt = Rarity - 1,
	Bin = << UCS2Name/binary, 0:NamePadding, RarityInt:8, Category:8, SellPrice:32/little, DataBin/binary >>,
	build_0a0a_item_constants(Tail, [Bin|Acc]).

build_item_constants(#psu_clothing_item{appearance=Appearance, manufacturer=Manufacturer, type=Type, overlap=Overlap, gender=Gender, colors=Colors}) ->
	GenderInt = case Gender of male -> 16#1b; female -> 16#2b end,
	<< Appearance:16, Type:4, Manufacturer:4, Overlap:8, GenderInt:8, Colors/binary, 0:40 >>;
build_item_constants(#psu_consumable_item{max_quantity=MaxQuantity, pt_diff=PointsDiff,
	status_effect=StatusEffect, target=Target, use_condition=UseCondition, item_effect=ItemEffect}) ->
	<< 0:8, MaxQuantity:8, Target:8, UseCondition:8, PointsDiff:16/little, StatusEffect:8, ItemEffect:8, 0:96 >>;
build_item_constants(#psu_parts_item{appearance=Appearance, manufacturer=Manufacturer, type=Type, overlap=Overlap, gender=Gender}) ->
	GenderInt = case Gender of male -> 16#14; female -> 16#24 end,
	<< Appearance:16, Type:4, Manufacturer:4, Overlap:8, GenderInt:8, 0:120 >>;
%% @todo Handle rank properly.
build_item_constants(#psu_striking_weapon_item{pp=PP, atp=ATP, ata=ATA, atp_req=Req, shop_element=#psu_element{type=EleType, percent=ElePercent},
	hand=Hand, max_upgrades=MaxUpgrades, attack_label=AttackLabel}) ->
	Rank = 4,
	HandInt = case Hand of
		both -> 0;
		_ -> error
	end,
	<< PP:16/little, ATP:16/little, ATA:16/little, Req:16/little, 16#ffffff:24,
		EleType:8, ElePercent:8, HandInt:8, 0:8, Rank:8, 0:8, MaxUpgrades:8, AttackLabel:8, 0:8 >>;
build_item_constants(#psu_trap_item{max_quantity=MaxQuantity}) ->
	<< 2:32/little, 16#ffffff:24, MaxQuantity:8, 0:96 >>;
build_item_constants(#psu_special_item{}) ->
	<< 0:160 >>.

%% @todo Figure out last 4 bytes!
send_0c02() ->
	send(<< (header(16#0c02))/binary, 0:32 >>).

%% @doc Send the huge pack of quest files available in the counter.
send_0c06(Filename) ->
	{ok, << File/bits >>} = file:read_file(Filename),
	send(<< 16#0c060300:32, 0:288, 1:32/little-unsigned-integer, File/binary >>).

%% @doc Reply whether the player is allowed to use the transport option.
%%      Use true for allowing it, and false otherwise.
send_0c08(Response) ->
	Value = if Response =:= true -> 0; true -> 1 end,
	send(<< (header(16#0c08))/binary, Value:32 >>).

%% @doc Send the trial start notification.
send_0c09() ->
	send(<< (header(16#0c09))/binary, 0:64 >>).

%% @doc Send the counter's mission options (0 = invisible, 2 = disabled, 3 = available).
send_0c10(Options) ->
	GID = get(gid),
	send(<< 16#0c100300:32, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		16#00011300:32, GID:32/little-unsigned-integer, 0:64, Options/binary >>).

%% @doc Send the data for the selected character.
%% @todo The large chunk of 0s can have some values set... but what are they used for?
%% @todo The values after the Char variable are the flags. Probably use bits to define what flag is and isn't set. Handle correctly.
send_0d01(User) ->
	GID = User#egs_user_model.id,
	CharBin = psu_characters:character_tuple_to_binary(User#egs_user_model.character),
	OptionsBin = psu_characters:options_tuple_to_binary((User#egs_user_model.character)#characters.options),
	send(<< 16#0d010300:32, 16#ffff:16, 0:144, 16#00011300:32, GID:32/little, 0:64, CharBin/binary,
		16#ffbbef1c:32, 16#f8ff0700:32, 16#fc810916:32, 16#7802134c:32,
		16#b0c0040f:32, 16#7cf0e583:32, 16#b7bce0c6:32, 16#7ff8f963:32,
		16#3fd7ffff:32, 16#fff7ffff:32, 16#f3ff63e0:32, 16#1fe00000:32,
		0:7744, OptionsBin/binary >>).

%% @doc Send the character list for selection.
%% @todo There's a few odd values blanked, also the last known location apparently.
send_0d03(Data0, Data1, Data2, Data3) ->
	[{status, Status0}, {char, Char0}|_] = Data0,
	[{status, Status1}, {char, Char1}|_] = Data1,
	[{status, Status2}, {char, Char2}|_] = Data2,
	[{status, Status3}, {char, Char3}|_] = Data3,
	GID = get(gid),
	send(<< 16#0d030300:32/unsigned-integer, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:104,
		Status0:8/unsigned-integer, 0:48, Char0/binary, 0:520,
		Status1:8/unsigned-integer, 0:48, Char1/binary, 0:520,
		Status2:8/unsigned-integer, 0:48, Char2/binary, 0:520,
		Status3:8/unsigned-integer, 0:48, Char3/binary, 0:512 >>).

%% @todo Add a character (NPC or real) to the party members on the right of the screen.
%% @todo NPCid is 65535 for normal characters.
%% @todo Apparently the 4 location ids are set to 0 when inviting an NPC in the lobby - NPCs have their location set to 0 when in lobby; also odd value before PartyPos related to missions
%% @todo Not sure about LID. But seems like it.
send_1004(Type, User, PartyPos) ->
	[TypeID, LID, SomeFlag] = case Type of
		npc_mission -> [16#00001d00, PartyPos, 2];
		npc_invite -> [0, 16#ffffffff, 3];
		_ -> 1 %% seems to be for players
	end,

	UserGID = get(gid),
	#egs_user_model{id=GID, character=Character, area={psu_area, QuestID, ZoneID, MapID}, entryid=EntryID} = User,
	#characters{npcid=NPCid, name=Name, mainlevel=MainLevel} = Character,
	Level = MainLevel#level.number,
	send(<< 16#10040300:32, 16#ffff0000:32, 0:128, 16#00011300:32, UserGID:32/little-unsigned-integer, 0:64,
		TypeID:32, GID:32/little-unsigned-integer, 0:64, Name/binary,
		Level:16/little-unsigned-integer, 16#ffff:16,
		SomeFlag, 1, PartyPos:8, 1,
		NPCid:16/little-unsigned-integer, 0:16,

		%% Odd unknown values. PA related? No idea. Values on invite, 0 in-mission.
		%~ 16#00001f08:32, 0:32, 16#07000000:32,
		%~ 16#04e41f08:32, 0:32, 16#01000000:32,
		%~ 16#64e41f08:32, 0:32, 16#02000000:32,
		%~ 16#64e41f08:32, 0:32, 16#03000000:32,
		%~ 16#64e41f08:32, 0:32, 16#12000000:32,
		%~ 16#24e41f08:32,
		0:512,

		QuestID:32/little-unsigned-integer, ZoneID:32/little-unsigned-integer, MapID:32/little-unsigned-integer, EntryID:32/little-unsigned-integer,
		LID:32/little,
		0:64,
		16#01000000:32, 16#01000000:32, %% @todo first is current hp, second is max hp
		0:608 >>).

%% @todo Figure out what the packet is.
send_1005(Name) ->
	{ok, File} = file:read_file("p/packet1005.bin"),
	<< _:352, Before:160/bits, _:608, After/bits >> = File,
	GID = get(gid),
	send(<< 16#10050300:32, 16#ffff:16, 0:144, 16#00011300:32, GID:32/little, 0:64, Before/binary, GID:32/little, 0:64, Name/binary, After/binary >>).

%% @todo No idea. Also the 2 PartyPos in the built packet more often than not match, but sometimes don't? That's probably because one is PartyPos and the other is LID or something.
send_100f(NPCid, PartyPos) ->
	send(<< (header(16#100f))/binary, NPCid:16/little-unsigned-integer, 1, PartyPos:8, PartyPos:32/little-unsigned-integer >>).

%% @doc Send the mission's quest file when starting a new mission.
%% @todo Handle correctly. 0:32 is actually a missing value. Value before that is unknown too.
send_1015(QuestID) ->
	[{type, _}, {file, QuestFile}|_] = proplists:get_value(QuestID, ?QUESTS),
	{ok, File} = file:read_file(QuestFile),
	Size = byte_size(File),
	send(<< (header(16#1015))/binary, QuestID:32/little-unsigned-integer, 16#01010000:32, 0:32, Size:32/little-unsigned-integer, File/binary >>).

%% @todo No idea.
send_1016(PartyPos) ->
	GID = get(gid),
	send(<< 16#10160300:32, 16#ffff0000:32, 0:128, 16#00011300:32, GID:32/little, 0:64, PartyPos:32/little >>).

%% @todo No idea.
send_101a(NPCid, PartyPos) ->
	send(<< (header(16#101a))/binary, NPCid:16/little-unsigned-integer, PartyPos:16/little-unsigned-integer, 16#ffffffff:32 >>).

%% @todo Boss related command.
send_110e(Data) ->
	send(<< (header(16#110e))/binary, Data/binary, 0:32, 5:16/little-unsigned-integer, 12:16/little-unsigned-integer, 0:32, 260:32/little-unsigned-integer >>).

%% @todo Boss related command.
send_1113(Data) ->
	send(<< (header(16#1113))/binary, Data/binary >>).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.
send_1202() ->
	send(<< (header(16#1202))/binary, 0:32, 16#10000000:32, 0:64, 16#14000000:32, 0:32 >>).

%% @doc Object events response?
%% @todo Not sure what Value does exactly. It's either 0 or 1.
send_1205(EventID, BlockID, Value) ->
	send(<< (header(16#1205))/binary, EventID, BlockID, 0:16, Value, 0:24 >>).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.
send_1206() ->
	send(<< (header(16#1206))/binary, 0:32, 16#80020000:32, 0:5120 >>).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.
send_1207() ->
	Chunk = << 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 0:224, 16#0000ffff:32, 16#ff000000:32, 16#64000a00:32 >>,
	send(<< (header(16#1207))/binary, Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary >>).

%% @todo Object interaction? Figure out. C probably the interaction type.
%% @todo Apparently A would be TargetID/ffffffff, B would be the player LID, C would be the object type? D still completely unknown.
send_1211(A, B, C, D) ->
	send(<< (header(16#1211))/binary, A:32/little-unsigned-integer, B:32/little-unsigned-integer, C:32/little-unsigned-integer, D:32/little-unsigned-integer >>).

%% @doc Make the client load the quest previously sent.
send_1212() ->
	send(<< (header(16#1212))/binary, 0:19200 >>).

%% @todo Not sure. Related to keys.
send_1213(A, B) ->
	send(<< (header(16#1213))/binary, A:32/little-unsigned-integer, B:32/little-unsigned-integer >>).

%% @todo Related to boss gates.
send_1215(A, B) ->
	send(<< (header(16#1215))/binary, A:32/little-unsigned-integer, 0:16, B:16/little-unsigned-integer >>).

%% @todo Not sure yet. Value is probably a TargetID. Used in Airboard Rally. Replying with the same value starts the race.
send_1216(Value) ->
	GID = get(gid),
	send(<< 16#12160300:32, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Value:32/little-unsigned-integer >>).

%% @todo Figure out this room packet.
send_1309() ->
	{ok, << _Size:32, Packet/bits >>} = file:read_file("p/packet1309.bin"),
	send(Packet).

%% @todo Figure out this room packet.
send_1332() ->
	{ok, << _Size:32, Packet/bits >>} = file:read_file("p/packet1332.bin"),
	send(Packet).

%% @doc Send the player's partner card.
%% @todo Find out the remaining values.
send_1500(User) ->
	GID = User#egs_user_model.id,
	#characters{slot=Slot, name=Name, race=Race, gender=Gender, class=Class} = User#egs_user_model.character,
	RaceBin = psu_characters:race_atom_to_binary(Race),
	GenderBin = psu_characters:gender_atom_to_binary(Gender),
	ClassBin = psu_characters:class_atom_to_binary(Class),
	send(<< 16#15000300:32, 16#ffff:16, 0:144, 16#00011300:32, GID:32/little, 0:64,
		Name/binary, RaceBin:8, GenderBin:8, ClassBin:8, 0:40, GID:32/little, 0:3040, 16#010401:24, Slot:8, 0:64 >>).

%% @todo Send an empty partner card list.
send_1501() ->
	GID = get(gid),
	send(<< 16#15010300:32, 16#ffff:16, 0:144, 16#00011300:32, GID:32/little, 0:96 >>).

%% @todo Send an empty blacklist.
send_1512() ->
	GID = get(gid),
	send(<< 16#15120300:32, 16#ffff:16, 0:144, 16#00011300:32, GID:32/little, 0:46144 >>).

%% @todo NPC related packet, sent when there's an NPC in the area.
send_1601(PartyPos) ->
	{ok, << _:32, Bin/bits >>} = file:read_file("p/packet1601.bin"),
	send(<< (header(16#1601))/binary, PartyPos:32/little, Bin/binary >>).

%% @doc Send the player's NPC and PM information.
%% @todo The value 4 is the card priority. Find what 3 is. When sending, the first 0 is an unknown value.
send_1602() ->
	NbNPC = lists:sum([1 || {_NPCid, Data} <- ?NPC, Data#psu_npc.has_card =:= true]),
	Bin = iolist_to_binary([<< NPCid:8, 0, 4, 0, 3, 0:24 >> || {NPCid, Data} <- ?NPC, Data#psu_npc.has_card =:= true]),
	MiddlePaddingSize = 8 * (344 - byte_size(Bin)),
	PMName = "My PM",
	UCS2PMName = << << X:8, 0:8 >> || X <- PMName >>,
	EndPaddingSize = 8 * (64 - byte_size(UCS2PMName)),
	GID = get(gid),
	send(<< 16#16020300:32, 16#ffff:16, 0:144, 16#00011300:32, GID:32/little, 0:96, Bin/binary, 0:MiddlePaddingSize, NbNPC, 0:24, UCS2PMName/binary, 0:EndPaddingSize, 0:32 >>).

%% @doc Party information.
%% @todo Handle existing parties.
send_1706(CharName) ->
	send(<< (header(16#1706))/binary, 16#00000300:32, 16#d5c0faff:32, 0:64, CharName/binary,
		16#78000000:32, 16#01010000:32, 0:1536, 16#0100c800:32, 16#0601010a:32, 16#ffffffff:32, 0:32 >>).

%% @doc Party settings. Item distribution is random for now.
%% @todo Handle correctly.
send_170a() ->
	send(<< (header(16#170a))/binary, 16#01010c08:32 >>).

%% @todo Find what the heck this packet is.
send_170c() ->
	{ok, File} = file:read_file("p/packet170c.bin"),
	send(<< (header(16#170c))/binary, File/binary >>).

%% @doc Send the background to use for the counter.
%% @todo Background has more info past the first byte.
send_1711(Background) ->
	send(<< (header(16#1711))/binary, Background:32/little-unsigned-integer >>).

%% @doc Unknown dialog-related handler.
%% @todo Everything!
send_1a02(A, B, C, D, E) ->
	send(<< (header(16#1a02))/binary, A:32/little-unsigned-integer, B:16/little-unsigned-integer,
		C:16/little-unsigned-integer, D:16/little-unsigned-integer, E:16/little-unsigned-integer >>).

%% @doc Lumilass handler. Possibly more.
%% @todo Figure out how Lumilass work exactly. The 4 bytes before the file may vary.
send_1a03() ->
	{ok, User} = egs_user_model:read(get(gid)),
	Character = User#egs_user_model.character,
	Filename = case {Character#characters.race, Character#characters.gender} of
		{cast, male} -> "p/lumilass-metal-male.bin";
		{cast, female} -> "p/lumilass-metal-female.bin";
		{_, male} -> "p/lumilass-flesh-male.bin";
		{_, female} -> "p/lumilass-flesh-female.bin"
	end,
	{ok, File} = file:read_file(Filename),
	send(<< (header(16#1a03))/binary, 0:32, File/binary >>).

%% @doc PP cube handler.
%% @todo The 4 bytes before the file may vary. Everything past that is the same. Figure things out.
send_1a04() ->
	{ok, File} = file:read_file("p/ppcube.bin"),
	send(<< (header(16#1a04))/binary, 0:32, File/binary >>).

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

%% @doc Load and send the character information to the client.
%% @todo Move this whole function directly to psu_proto, probably.
char_load(User, Client) ->
	psu_proto:send_0d01(User#users.character, Client),
	%% 0246
	send_0a0a((User#users.character)#characters.inventory),
	psu_proto:send_1006(5, 0, Client), %% @todo The 0 here is PartyPos, save it in User.
	psu_proto:send_1005(User#users.character, Client),
	psu_proto:send_1006(12, Client),
	psu_proto:send_0210(Client),
	psu_proto:send_0222(User#users.uni, Client),
	psu_proto:send_1500(User#users.character, Client),
	psu_proto:send_1501(Client),
	psu_proto:send_1512(Client),
	%% 0303
	psu_proto:send_1602(Client),
	psu_proto:send_021b(Client).

%% @doc Load the given map as a standard lobby.
area_load(QuestID, ZoneID, MapID, EntryID, Client) ->
	{ok, OldUser} = egs_users:read(Client#client.gid),
	{OldQuestID, OldZoneID, _OldMapID} = OldUser#users.area,
	QuestChange = OldQuestID /= QuestID,
	ZoneChange = if OldQuestID =:= QuestID, OldZoneID =:= ZoneID -> false; true -> true end,
	AreaType = egs_quests_db:area_type(QuestID, ZoneID),
	AreaShortName = "dammy", %% @todo Load the short name from egs_quests_db.
	{IsSeasonal, SeasonID} = egs_seasons:read(QuestID),
	User = OldUser#users{areatype=AreaType, area={QuestID, ZoneID, MapID}, entryid=EntryID},
	egs_users:write(User), %% @todo Booh ugly! But temporary.
	%% Load the quest.
	User2 = if QuestChange ->
			psu_proto:send_0c00(User, Client),
			psu_proto:send_020e(egs_quests_db:quest_nbl(QuestID), Client),
			User#users{questpid=egs_universes:lobby_pid(User#users.uni, QuestID)};
		true -> User
	end,
	%% Load the zone.
	Client1 = if ZoneChange ->
			ZonePid = egs_quests:zone_pid(User2#users.questpid, ZoneID),
			egs_zones:leave(User2#users.zonepid, User2#users.gid),
			NewLID = egs_zones:enter(ZonePid, User2#users.gid),
			NewClient = Client#client{lid=NewLID},
			{ok, User3} = egs_users:read(User2#users.gid),
			psu_proto:send_0a05(NewClient),
			psu_proto:send_0111(User3, 6, NewClient),
			psu_proto:send_010d(User3, NewClient),
			psu_proto:send_0200(ZoneID, AreaType, NewClient),
			psu_proto:send_020f(egs_quests_db:zone_nbl(QuestID, ZoneID), egs_zones:setid(ZonePid), SeasonID, NewClient),
			NewClient;
		true ->
			User3 = User2,
			Client
	end,
	%% Save the user.
	egs_users:write(User3),
	%% Load the player location.
	Client2 = Client1#client{areanb=Client#client.areanb + 1},
	psu_proto:send_0205(User3, IsSeasonal, Client2),
	psu_proto:send_100e(User3#users.area, User3#users.entryid, AreaShortName, Client2),
	%% Load the zone objects.
	if ZoneChange ->
			psu_proto:send_1212(Client2); %% @todo Only sent if there is a set file.
		true -> ignore
	end,
	%% Load the player.
	psu_proto:send_0201(User3, Client2),
	if ZoneChange ->
			psu_proto:send_0a06(User3, Client2),
			%% Load the other players in the zone.
			OtherPlayersGID = egs_zones:get_all_players(User3#users.zonepid, User3#users.gid),
			if	OtherPlayersGID =:= [] -> ignore;
				true ->
					OtherPlayers = egs_users:select(OtherPlayersGID),
					psu_proto:send_0233(OtherPlayers, Client)
			end;
		true -> ignore
	end,
	%% End of loading.
	Client3 = Client2#client{areanb=Client2#client.areanb + 1},
	psu_proto:send_0208(Client3),
	psu_proto:send_0236(Client3),
	%% @todo Load APC characters.
	{ok, Client3}.

%% @todo Don't change the NPC info unless you are the leader!
npc_load(_Leader, [], _Client) ->
	ok;
npc_load(Leader, [{PartyPos, NPCGID}|NPCList], Client) ->
	{ok, OldNPCUser} = egs_users:read(NPCGID),
	#users{instancepid=InstancePid, area=Area, entryid=EntryID, pos=Pos} = Leader,
	NPCUser = OldNPCUser#users{lid=PartyPos, instancepid=InstancePid, areatype=mission, area=Area, entryid=EntryID, pos=Pos},
	%% @todo This one on mission end/abort?
	%~ OldNPCUser#users{lid=PartyPos, instancepid=undefined, areatype=AreaType, area={0, 0, 0}, entryid=0, pos={0.0, 0.0, 0.0, 0}}
	egs_users:write(NPCUser),
	psu_proto:send_010d(NPCUser, Client),
	psu_proto:send_0201(NPCUser, Client),
	psu_proto:send_0215(0, Client),
	psu_proto:send_0a04(NPCUser#users.gid, Client),
	psu_proto:send_1004(npc_mission, NPCUser, PartyPos, Client),
	psu_proto:send_100f((NPCUser#users.character)#characters.npcid, PartyPos, Client),
	psu_proto:send_1601(PartyPos, Client),
	psu_proto:send_1016(PartyPos, Client),
	npc_load(Leader, NPCList, Client).

%% @doc Send the given packet to the client.
%% @todo Consolidate the receive and send functions better.
send(Packet) ->
	psu_proto:packet_send(get(socket), Packet).

%% @todo Handle more than just goggles.
send_0a0a(Inventory) ->
	{ok, << _:68608/bits, Rest/bits >>} = file:read_file("p/packet0a0a.bin"),
	GID = get(gid),
	NbItems = length(Inventory),
	ItemVariables = build_0a0a_item_variables(Inventory, 1, []),
	ItemConstants = build_0a0a_item_constants(Inventory, []),
	send(<< 16#0a0a0300:32, 16#ffff:16, 0:144, 16#00011300:32, GID:32/little, 0:64, NbItems:8, 0:8, 6:8, 0:72, 0:192, 0:2304, ItemVariables/binary, ItemConstants/binary, 0:13824, Rest/binary >>).

build_0a0a_item_variables([], _N, Acc) ->
	Bin = iolist_to_binary(lists:reverse(Acc)),
	Padding = 17280 - 8 * byte_size(Bin),
	<< Bin/binary, 0:Padding >>;
build_0a0a_item_variables([{ItemID, Variables}|Tail], N, Acc) ->
	build_0a0a_item_variables(Tail, N + 1, [build_item_variables(ItemID, N, Variables)|Acc]).

build_item_variables(ItemID, ItemUUID, #psu_clothing_item_variables{color=ColorNb}) ->
	#psu_item{rarity=Rarity, data=#psu_clothing_item{colors=ColorsBin}} = egs_items_db:read(ItemID),
	RarityInt = Rarity - 1,
	ColorInt = if ColorNb < 5 -> ColorNb; true -> 16#10 + ColorNb - 5 end,
	Bits = ColorNb * 8,
	<< _Before:Bits, ColorA:4, ColorB:4, _After/bits >> = ColorsBin,
	<< 0:32, ItemUUID:32/little, ItemID:32, 0:88, RarityInt:8, ColorA:8, ColorB:8, ColorInt:8, 0:72 >>;
build_item_variables(ItemID, ItemUUID, #psu_consumable_item_variables{quantity=Quantity}) ->
	#psu_item{rarity=Rarity, data=#psu_consumable_item{max_quantity=MaxQuantity, action=Action}} = egs_items_db:read(ItemID),
	RarityInt = Rarity - 1,
	<< 0:32, ItemUUID:32/little, ItemID:32, Quantity:32/little, MaxQuantity:32/little, 0:24, RarityInt:8, Action:8, 0:88 >>;
build_item_variables(ItemID, ItemUUID, #psu_parts_item_variables{}) ->
	#psu_item{rarity=Rarity} = egs_items_db:read(ItemID),
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
	#psu_item{rarity=Rarity, data=#psu_trap_item{max_quantity=MaxQuantity}} = egs_items_db:read(ItemID),
	RarityInt = Rarity - 1,
	<< 0:32, ItemUUID:32/little, ItemID:32, Quantity:32/little, MaxQuantity:32/little, 0:24, RarityInt:8, 0:96 >>.

build_0a0a_item_constants([], Acc) ->
	Bin = iolist_to_binary(lists:reverse(Acc)),
	Padding = 34560 - 8 * byte_size(Bin),
	<< Bin/binary, 0:Padding >>;
build_0a0a_item_constants([{ItemID, _Variables}|Tail], Acc) ->
	#psu_item{name=Name, rarity=Rarity, sell_price=SellPrice, data=Data} = egs_items_db:read(ItemID),
	UCS2Name = << << X:8, 0:8 >> || X <- Name >>,
	NamePadding = 8 * (46 - byte_size(UCS2Name)),
	<< Category:8, _:24 >> = << ItemID:32 >>,
	DataBin = psu_proto:build_item_constants(Data),
	RarityInt = Rarity - 1,
	Bin = << UCS2Name/binary, 0:NamePadding, RarityInt:8, Category:8, SellPrice:32/little, DataBin/binary >>,
	build_0a0a_item_constants(Tail, [Bin|Acc]).

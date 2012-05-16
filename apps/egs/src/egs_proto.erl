%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc Independent implementation of the PSU protocol.
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

-module(egs_proto).
-compile(export_all).

-include("include/records.hrl").

%% @spec assert() -> ok
%% @doc Log a detailed message when the function is called.
-define(ASSERT(), io:format("assert error in module ~p on line ~p~n", [?MODULE, ?LINE])).

%% @spec assert(A, B) -> ok
%% @doc Log a detailed message when the assertion A =:= B fails.
-define(ASSERT_EQ(A, B), if A =:= B -> ok; true -> io:format("assert error in module ~p on line ~p~n", [?MODULE, ?LINE]) end).

%% @doc Send a shop listing.
%% @todo This packet (and its build_010a_list function) hasn't been reviewed at all yet.
send_010a(ItemsList, Client=#egs_net{gid=DestGID}) ->
	NbItems = length(ItemsList),
	ItemsBin = build_010a_list(ItemsList, []),
	packet_send(Client, << 16#010a0300:32, 0:64, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		DestGID:32/little, 0:32, 1:16/little, NbItems:8, 2:8, 0:32, ItemsBin/binary >>).

%% @todo The values set to 0 are unknown.
build_010a_list([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
build_010a_list([ItemID|Tail], Acc) ->
	#psu_item{name=Name, rarity=Rarity, buy_price=SellPrice, data=Data} = egs_items_db:read(ItemID),
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

%% @doc Send character appearance and other information.
%% @todo Probably don't pattern match the data like this...
send_010d(CharUser, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	CharGID = CharUser#users.gid,
	CharLID = CharUser#users.lid,
	<< _:640, CharBin/bits >> = psu_characters:character_user_to_binary(CharUser),
	packet_send(Client, << 16#010d0300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little,
		0:64, 1:32/little, 0:32, 16#00000300:32, 16#ffff0000:32, 0:32, CharGID:32/little,
		0:192, CharGID:32/little, CharLID:32/little, 16#ffffffff:32, CharBin/binary >>).

%% @doc Trigger a character-related event.
send_0111(CharUser, EventID, Client) ->
	send_0111(CharUser, EventID, 0, Client).
send_0111(#users{gid=CharGID, lid=CharLID}, EventID, Param, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#01110300:32, DestLID:16/little, 0:48, CharGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		CharGID:32/little, CharLID:32/little, EventID:32/little, Param:32/little >>).

%% @todo Types capability list.
%% @todo This packet hasn't been reviewed at all yet.
send_0113(Client=#egs_net{gid=DestGID}) ->
	{ok, File} = file:read_file("p/typesinfo.bin"),
	packet_send(Client, << 16#01130300:32, 0:64, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64, DestGID:32/little, File/binary >>).

%% @doc Update the character level, blastbar, luck and money information.
send_0115(User, Client) ->
	send_0115(User, 16#ffffffff, Client).
send_0115(User=#users{gid=CharGID, lid=CharLID}, EnemyTargetID, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#01150300:32, DestLID:16/little, 0:48, CharGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		CharGID:32/little, CharLID:32/little, EnemyTargetID:32/little, (build_char_level(User))/binary >>).

%% @doc Revive player with optional SEs.
%% @todo SEs.
send_0117(#users{gid=CharGID, lid=CharLID, currenthp=HP}, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	SE = << 0:64 >>,
	packet_send(Client, << 16#01170300:32, DestLID:16/little, 0:48, CharGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		CharGID:32/little, CharLID:32/little, SE/binary, HP:32/little, 0:32 >>).

%% @doc Send the zone initialization command.
%% @todo Handle NbPlayers properly. There's more than 1 player!
send_0200(ZoneID, ZoneType, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	Var = case ZoneType of
		mission -> << 16#06000500:32, 16#01000000:32, 0:64, 16#00040000:32, 16#00010000:32, 16#00140000:32 >>;
		myroom -> << 16#06000000:32, 16#02000000:32, 0:64, 16#40000000:32, 16#00010000:32, 16#00010000:32 >>;
		_ -> << 16#00040000:32, 0:160, 16#00140000:32 >>
	end,
	packet_send(Client, << 16#02000300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		DestLID:16/little, ZoneID:16/little, 1:32/little, 16#ffffffff:32, Var/binary, 16#ffffffff:32, 16#ffffffff:32 >>).

%% @doc Send character location, appearance and other information.
send_0201(CharUser, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	[CharTypeID, GameVersion] = case CharUser#users.type of
		npc -> [16#00001d00, 255];
		_ -> [16#00001200, 0]
	end,
	CharGID = CharUser#users.gid,
	CharBin = psu_characters:character_user_to_binary(CharUser),
	IsGM = 0,
	OnlineStatus = 0,
	packet_send(Client, << 16#02010300:32, DestLID:16/little, 0:16, CharTypeID:32, CharGID:32/little,
		0:64, 16#00011300:32, DestGID:32/little, 0:64, CharBin/binary, IsGM:8, 0:8, OnlineStatus:8, GameVersion:8, 0:608 >>).

%% @doc Spawn a player with the given GID and LID.
send_0203(#users{gid=CharGID, lid=CharLID}, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#02030300:32, DestLID:16/little, 0:144, 16#00011300:32,
		DestGID:32/little, 0:64, CharGID:32/little, CharLID:32/little >>).

%% @doc Unspawn the given character.
%% @todo The last 4 bytes are probably the number of players remaining in the zone.
send_0204(User, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	CharTypeID = case User#users.type of
		npc -> 16#00001d00;
		_ -> 16#00001200
	end,
	#users{gid=CharGID, lid=CharLID} = User,
	packet_send(Client, << 16#02040300:32, DestLID:16/little, 0:16, CharTypeID:32, CharGID:32/little, 0:64,
		16#00011300:32, DestGID:32/little, 0:64, CharGID:32/little, CharLID:32/little, 100:32/little >>).

%% @doc Make the client load a new map.
send_0205(CharUser, IsSeasonal, Client=#egs_net{gid=DestGID, lid=DestLID, areanb=AreaNb}) ->
	#users{lid=CharLID, area={_QuestID, ZoneID, MapID}, entryid=EntryID} = CharUser,
	packet_send(Client, << 16#02050300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		16#ffffffff:32, ZoneID:32/little, MapID:32/little, EntryID:32/little, AreaNb:32/little, CharLID:16/little, 0:8, IsSeasonal:8 >>).

%% @doc Indicate to the client that loading should finish.
send_0208(Client=#egs_net{gid=DestGID, lid=DestLID, areanb=AreaNb}) ->
	packet_send(Client, << 16#02080300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, AreaNb:32/little >>).

%% @todo No idea what this one does. For unknown reasons it uses channel 2.
%% @todo Handle the DestLID properly?
send_020c(Client) ->
	packet_send(Client, << 16#020c0200:32, 16#ffff0000:32, 0:256 >>).

%% @doc Send the quest file to be loaded by the client.
%% @todo Handle the DestLID properly?
send_020e(QuestData, Client) ->
	Size = byte_size(QuestData),
	packet_send(Client, << 16#020e0300:32, 16#ffff:16, 0:272, Size:32/little, 0:32, QuestData/binary, 0:32 >>).

%% @doc Send the zone file to be loaded.
send_020f(ZoneData, SetID, SeasonID, Client) ->
	Size = byte_size(ZoneData),
	packet_send(Client, << 16#020f0300:32, 16#ffff:16, 0:272, SetID, SeasonID, 0:16, Size:32/little, ZoneData/binary >>).

%% @doc Send the current UNIX time.
send_0210(Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	{M, S, _} = erlang:now(),
	UnixTime = M * 1000000 + S,
	packet_send(Client, << 16#02100300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:96, UnixTime:32/little >>).

%% @todo No idea what this is doing.
send_0215(UnknownValue, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#02150300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, UnknownValue:32/little >>).

%% @doc End of character loading.
send_021b(Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#021b0300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Send the list of available universes.
send_021e(Universes, Client) ->
	NbUnis = length(Universes),
	UnisBin = build_021e_uni(Universes, []),
	packet_send(Client, << 16#021e0300:32, 0:288, NbUnis:32/little, UnisBin/binary >>).

build_021e_uni([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
build_021e_uni([{_UniID, {myroom, Name, NbPlayers, _MaxPlayers}}|Tail], Acc) ->
	Padding = 8 * (44 - byte_size(Name)),
	Bin = << 16#ffffffff:32, NbPlayers:16/little, 0:16, Name/binary, 0:Padding >>,
	build_021e_uni(Tail, [Bin|Acc]);
build_021e_uni([{UniID, {universe, Name, NbPlayers, _MaxPlayers}}|Tail], Acc) ->
	Padding = 8 * (32 - byte_size(Name)),
	PopString = lists:flatten(io_lib:format("~5b", [NbPlayers])),
	PopString2 = << << X:8, 0:8 >> || X <- PopString >>,
	Bin = << UniID:32/little, NbPlayers:16/little, 643:16/little, Name/binary, 0:Padding, PopString2/binary, 0:16 >>,
	build_021e_uni(Tail, [Bin|Acc]).

%% @doc Send the current universe info along with the current level cap.
send_0222(UniID, Client=#egs_net{gid=DestGID}) ->
	{_Type, Name, NbPlayers, MaxPlayers} = egs_universes:read(UniID),
	Padding = 8 * (44 - byte_size(Name)),
	LevelCap = egs_conf:read(level_cap),
	packet_send(Client, << 16#02220300:32, 16#ffff:16, 0:16, 16#00001200:32, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		UniID:32/little, NbPlayers:16/little, MaxPlayers:16/little, Name/binary, 0:Padding, LevelCap:32/little >>).

%% @doc Display a notice on the player's screen.
%%      There are four types of notices: dialog, top, scroll and timeout.
%% * dialog: A dialog in the center of the screen, which can be OK'd by players.
%% * top: Horizontal scroll on top of the screen, traditionally used for server-wide messages.
%% * scroll: Vertical scroll on the right of the screen, traditionally used for rare missions obtention messages.
%% * timeout: A dialog in the center of the screen that disappears after Duration seconds.
send_0228(Type, Duration, Message, Client=#egs_net{gid=DestGID}) ->
	TypeInt = case Type of dialog -> 0; top -> 1; scroll -> 2; timeout -> 3 end,
	UCS2Message = << << X:8, 0:8 >> || X <- Message >>,
	packet_send(Client, << 16#02280300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		TypeInt:32/little, Duration:32/little, UCS2Message/binary, 0:16 >>).

%% @todo No idea!
%% @todo This packet hasn't been reviewed at all yet.
send_022c(A, B, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#022c0300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, A:16/little, B:16/little >>).

%% @todo Not sure. Sent when going to or from room. Possibly when changing universes too?
send_0230(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#02300300:32, 16#ffff:16, 0:16, 16#00011300:32, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Send the list of players already spawned in the zone when entering it.
send_0233(Users, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	NbUsers = length(Users),
	Bin = build_0233_users(Users, []),
	packet_send(Client, << 16#02330300:32, DestLID:16/little, 0:16, 16#00001200:32, DestGID:32/little, 0:64,
		16#00011300:32, DestGID:32/little, 0:64, NbUsers:32/little, Bin/binary, 0:608 >>).

build_0233_users([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
build_0233_users([User|Tail], Acc) ->
	Bin = psu_characters:character_user_to_binary(User),
	build_0233_users(Tail, [<< Bin/binary, 0:32 >>|Acc]).

%% @doc Start the zone handling: load the zone file and the objects sent separately.
send_0236(Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#02360300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Chat message.
send_0304(FromGID, ChatTypeID, ChatGID, ChatName, ChatModifiers, ChatMessage, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	{chat_modifiers, ChatType, ChatCutIn, ChatCutInAngle, ChatMsgLength, ChatChannel, ChatCharacterType} = ChatModifiers,
	packet_send(Client, << 16#03040300:32, DestLID:16/little, 0:16, 16#00011300:32, FromGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		ChatTypeID:32, ChatGID:32/little, 0:64, ChatType:8, ChatCutIn:8, ChatCutInAngle:8, ChatMsgLength:8,
		ChatChannel:8, ChatCharacterType:8, 0:16, ChatName/binary, ChatMessage/binary >>).

%% @todo Force send a new player location. Used for warps.
%% @todo The value before IntDir seems to be the player's current animation. 01 stand up, 08 ?, 17 normal sit
%% @todo This packet hasn't been reviewed at all yet.
send_0503({PrevX, PrevY, PrevZ, _AnyDir}, Client=#egs_net{gid=DestGID}) ->
	{ok, User} = egs_users:read(DestGID),
	#users{pos={X, Y, Z, Dir}, area={QuestID, ZoneID, MapID}, entryid=EntryID} = User,
	IntDir = trunc(Dir * 182.0416),
	packet_send(Client, << 16#05030300:32, 0:64, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64, DestGID:32/little, 0:32,
		16#1000:16, IntDir:16/little, PrevX:32/little-float, PrevY:32/little-float, PrevZ:32/little-float, X:32/little-float, Y:32/little-float, Z:32/little-float,
		QuestID:32/little, ZoneID:32/little, MapID:32/little, EntryID:32/little, 1:32/little >>).

%% @todo NPC inventory. Guessing it's only for NPC characters...
%% @todo This packet hasn't been reviewed at all yet.
send_0a04(NPCGID, Client=#egs_net{gid=DestGID}) ->
	{ok, Bin} = file:read_file("p/packet0a04.bin"),
	packet_send(Client, << 16#0a040300:32, 0:32, 16#00001d00:32, NPCGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64, Bin/binary >>).

%% @todo Inventory related. Doesn't seem to do anything.
send_0a05(Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#0a050300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Send the list of ItemUUID for the items in the inventory.
send_0a06(CharUser, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	Len = length(CharUser#users.inventory),
	UUIDs = lists:seq(1, Len),
	Bin = iolist_to_binary([ << N:32/little >> || N <- UUIDs]),
	Blanks = lists:seq(1, 60 - Len),
	Bin2 = iolist_to_binary([ << 16#ffffffff:32 >> || _N <- Blanks]),
	packet_send(Client, << 16#0a060300:32, DestLID:16/little, 0:48, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64, Bin/binary, Bin2/binary >>).

%% @todo Handle more than just goggles.
%% @todo This packet hasn't been reviewed at all yet.
send_0a0a(Inventory, Client=#egs_net{gid=DestGID}) ->
	{ok, << _:68608/bits, Rest/bits >>} = file:read_file("p/packet0a0a.bin"),
	NbItems = length(Inventory),
	ItemVariables = build_0a0a_item_variables(Inventory, 1, []),
	ItemConstants = build_0a0a_item_constants(Inventory, []),
	packet_send(Client, << 16#0a0a0300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		NbItems:8, 0:8, 6:8, 0:72, 0:192, 0:2304, ItemVariables/binary, ItemConstants/binary, 0:13824, Rest/binary >>).

build_0a0a_item_variables([], _N, Acc) ->
	Bin = iolist_to_binary(lists:reverse(Acc)),
	Padding = 17280 - 8 * byte_size(Bin),
	<< Bin/binary, 0:Padding >>;
build_0a0a_item_variables([{ItemID, Variables}|Tail], N, Acc) ->
	build_0a0a_item_variables(Tail, N + 1, [build_item_variables(ItemID, N, Variables)|Acc]).

build_0a0a_item_constants([], Acc) ->
	Bin = iolist_to_binary(lists:reverse(Acc)),
	Padding = 34560 - 8 * byte_size(Bin),
	<< Bin/binary, 0:Padding >>;
build_0a0a_item_constants([{ItemID, _Variables}|Tail], Acc) ->
	#psu_item{name=Name, rarity=Rarity, sell_price=SellPrice, data=Data} = egs_items_db:read(ItemID),
	UCS2Name = << << X:8, 0:8 >> || X <- Name >>,
	NamePadding = 8 * (46 - byte_size(UCS2Name)),
	<< Category:8, _:24 >> = << ItemID:32 >>,
	DataBin = build_item_constants(Data),
	RarityInt = Rarity - 1,
	Bin = << UCS2Name/binary, 0:NamePadding, RarityInt:8, Category:8, SellPrice:32/little, DataBin/binary >>,
	build_0a0a_item_constants(Tail, [Bin|Acc]).

%% @doc Send an item's description.
send_0a11(ItemID, ItemDesc, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	Length = 1 + byte_size(ItemDesc) div 2,
	packet_send(Client, << 16#0a110300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		ItemID:32, Length:32/little, ItemDesc/binary, 0:16 >>).

%% @doc Quest init.
%% @todo When first entering a zone it seems LID should be set to ffff apparently.
send_0c00(CharUser, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	#users{area={QuestID, _ZoneID, _MapID}} = CharUser,
	packet_send(Client, << 16#0c000300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, QuestID:32/little,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>).

%% @todo Figure out last 4 bytes!
%% @todo This packet hasn't been reviewed at all yet.
send_0c02(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#0c020300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, 0:32 >>).

%% @doc Send the huge pack of quest files available in the counter.
send_0c06(Pack, Client) ->
	packet_send(Client, << 16#0c060300:32, 0:288, 1:32/little, Pack/binary >>).

%% @doc Reply that the player is allowed to use the lobby transport. Always allow.
send_0c08(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#0c080300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:96 >>).

%% @doc Send the trial start notification.
%% @todo This packet hasn't been reviewed at all yet.
send_0c09(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#0c090300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, 0:64 >>).

%% @doc Send the counter's mission options (0 = invisible, 2 = disabled, 3 = available).
send_0c10(Options, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	Size = byte_size(Options),
	packet_send(Client, << 16#0c100300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 1, 0, Size:16/little, Options/binary >>).

%% @todo Add a character (NPC or real) to the party members on the right of the screen.
%% @todo NPCid is 65535 for normal characters.
%% @todo Apparently the 4 location ids are set to 0 when inviting an NPC in the lobby - NPCs have their location set to 0 when in lobby; also odd value before PartyPos related to missions
%% @todo Not sure about LID. But seems like it.
%% @todo This packet hasn't been reviewed at all yet.
send_1004(Type, User, PartyPos, Client=#egs_net{gid=DestGID}) ->
	[TypeID, LID, SomeFlag] = case Type of
		npc_mission -> [16#00001d00, PartyPos, 2];
		npc_invite -> [0, 16#ffffffff, 3];
		_ -> 1 %% seems to be for players
	end,
	#users{gid=GID, npcid=NPCid, name=Name, level=Level, area={QuestID, ZoneID, MapID}, entryid=EntryID} = User,
	packet_send(Client, << 16#10040300:32, 16#ffff0000:32, 0:128, 16#00011300:32, DestGID:32/little, 0:64,
		TypeID:32, GID:32/little, 0:64, Name/binary,
		Level:16/little, 16#ffff:16,
		SomeFlag, 1, PartyPos:8, 1,
		NPCid:16/little, 0:16,
		%% Odd unknown values. PA related? No idea. Values on invite, 0 in-mission.
		%~ 16#00001f08:32, 0:32, 16#07000000:32,
		%~ 16#04e41f08:32, 0:32, 16#01000000:32,
		%~ 16#64e41f08:32, 0:32, 16#02000000:32,
		%~ 16#64e41f08:32, 0:32, 16#03000000:32,
		%~ 16#64e41f08:32, 0:32, 16#12000000:32,
		%~ 16#24e41f08:32,
		0:512,
		QuestID:32/little, ZoneID:32/little, MapID:32/little, EntryID:32/little,
		LID:32/little,
		0:64,
		16#01000000:32, 16#01000000:32, %% @todo first is current hp, second is max hp
		0:608 >>).

%% @doc Send the client's own player's party information, on the bottom left of the screen.
%% @todo Location and the 20 bytes following sometimes have values, not sure why; when joining a party maybe?
send_1005(User, Client=#egs_net{gid=DestGID}) ->
	#users{name=Name, level=Level, currenthp=CurrentHP, maxhp=MaxHP} = User,
	Location = << 0:512 >>,
	packet_send(Client, << 16#10050300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		16#00000100:32, 0:32, 16#ffffffff:32, 0:32, 16#00011200:32, DestGID:32/little, 0:64,
		Name/binary, Level:8, 0:16, 1:8, 16#01010000:32, 0:32, Location/binary,
		16#ffffffff:32, 0:96, 16#ffffffff:32, 0:64, CurrentHP:32/little, MaxHP:32/little, 0:640,
		16#0100ffff:32, 16#0000ff00:32, 16#ffff0000:32, 0:640, 16#ffffffff:32, 0:768,
		16#0100ffff:32, 16#0000ff00:32, 16#ffff0000:32, 0:640, 16#ffffffff:32, 0:768,
		16#0100ffff:32, 16#0000ff00:32, 16#ffff0000:32, 0:640, 16#ffffffff:32, 0:768,
		16#0100ffff:32, 16#0000ff00:32, 16#ffff0000:32, 0:640, 16#ffffffff:32, 0:768,
		16#0100ffff:32, 16#0000ff00:32, 16#ffff0000:32, 0:640, 16#ffffffff:32, 0:448,
		16#ffffffff:32, 0:32, 16#ff020000:32, 16#ffff0000:32, 16#ffff0000:32, 16#ffff0000:32,
		16#ffff0000:32, 16#ffff0000:32, 16#ffff0000:32, 0:3680 >>).

%% @doc Party-related events.
send_1006(EventID, Client) ->
	send_1006(EventID, 0, Client).
send_1006(EventID, PartyPos, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#10060300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, EventID:8, PartyPos:8, 0:16 >>).

%% @doc Send the player's current location.
%% @todo Handle PartyPos.
%% @todo Receive the AreaName as UCS2 directly to allow for color codes and the like.
%% @todo Handle TargetLID probably (right after the padding).
%% @todo Do counters even have a name?
send_100e(CounterID, AreaName, Client=#egs_net{gid=DestGID}) ->
	PartyPos = 0,
	UCS2Name = << << X:8, 0:8 >> || X <- AreaName >>,
	Padding = 8 * (64 - byte_size(UCS2Name)),
	CounterType = if CounterID =:= 16#ffffffff -> 2; true -> 1 end,
	packet_send(Client, << 16#100e0300:32, 16#ffffffbf:32, 0:128, 16#00011300:32, DestGID:32, 0:64,
		1, PartyPos, 0:48, 16#ffffff7f:32, UCS2Name/binary, 0:Padding, 0:32, CounterID:32/little, CounterType:32/little >>).
send_100e({QuestID, ZoneID, MapID}, EntryID, AreaName, Client=#egs_net{gid=DestGID}) ->
	PartyPos = 0,
	UCS2Name = << << X:8, 0:8 >> || X <- AreaName >>,
	Padding = 8 * (64 - byte_size(UCS2Name)),
	packet_send(Client, << 16#100e0300:32, 16#ffffffbf:32, 0:128, 16#00011300:32, DestGID:32, 0:64,
		1, PartyPos, ZoneID:16/little, MapID:16/little, EntryID:16/little, QuestID:32/little,
		UCS2Name/binary, 0:Padding, 0:32, 16#ffffffff:32, 0:32 >>).

%% @todo No idea. Also the 2 PartyPos in the built packet more often than not match, but sometimes don't? That's probably because one is PartyPos and the other is LID or something.
%% @todo This packet hasn't been reviewed at all yet.
send_100f(NPCid, PartyPos, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#100f0300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, NPCid:16/little, 1, PartyPos:8, PartyPos:32/little >>).

%% @doc Send the mission's quest file when starting a new mission.
%% @todo Handle correctly. 0:32 is actually a missing value. Value before that is unknown too.
%% @todo This packet hasn't been reviewed at all yet.
send_1015(QuestID, Client=#egs_net{gid=DestGID}) ->
	QuestData = egs_quests_db:quest_nbl(QuestID),
	Size = byte_size(QuestData),
	packet_send(Client, << 16#10150300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, QuestID:32/little, 16#01010000:32, 0:32, Size:32/little, QuestData/binary >>).

%% @todo No idea.
%% @todo This packet hasn't been reviewed at all yet.
send_1016(PartyPos, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#10160300:32, 16#ffff0000:32, 0:128, 16#00011300:32, DestGID:32/little, 0:64, PartyPos:32/little >>).

%% @todo No idea.
%% @todo This packet hasn't been reviewed at all yet.
send_101a(NPCid, PartyPos, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#101a0300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, NPCid:16/little, PartyPos:16/little, 16#ffffffff:32 >>).

%% @doc Mission start related.
send_1020(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#10200300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Update HP in the party members information on the left.
%% @todo Handle PartyPos. Probably only pass HP later.
send_1022(#users{currenthp=HP}, Client=#egs_net{gid=DestGID}) ->
	PartyPos = 0,
	packet_send(Client, << 16#10220300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, HP:32/little, PartyPos:32/little >>).

%% @todo Boss related command.
%% @todo This packet hasn't been reviewed at all yet.
send_110e(Data, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#110e0300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, Data/binary, 0:32, 5:16/little, 12:16/little, 0:32, 260:32/little >>).

%% @todo Boss related command.
%% @todo This packet hasn't been reviewed at all yet.
send_1113(Data, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#11130300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, Data/binary >>).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.
%% @todo This packet hasn't been reviewed at all yet.
send_1202(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#12020300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 0:32, 16#10000000:32, 0:64, 16#14000000:32, 0:32 >>).

%% @todo Always the same value, no idea what it's for.
send_1204(Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#12040300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:96, 16#20000000:32, 0:256 >>).

%% @doc Object events response?
%% @todo Not sure what Value does exactly. It's either 0 or 1.
%% @todo This packet hasn't been reviewed at all yet.
send_1205(EventID, BlockID, Value, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#12050300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, EventID, BlockID, 0:16, Value, 0:24 >>).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.
%% @todo This packet hasn't been reviewed at all yet.
send_1206(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#12060300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 0:32, 16#80020000:32, 0:5120 >>).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.
%% @todo This packet hasn't been reviewed at all yet.
send_1207(Client=#egs_net{gid=DestGID}) ->
	Chunk = << 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 0:224, 16#0000ffff:32, 16#ff000000:32, 16#64000a00:32 >>,
	packet_send(Client, << 16#12070300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary >>).

%% @todo Object interaction? Figure out. C probably the interaction type.
%% @todo Apparently A would be TargetID/ffffffff, B would be the player LID, C would be the object type? D still completely unknown.
%% @todo This packet hasn't been reviewed at all yet.
send_1211(A, B, C, D, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#12110300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, A:32/little, B:32/little, C:32/little, D:32/little >>).

%% @doc Make the client load the quest previously sent.
%% @todo This packet hasn't been reviewed at all yet.
send_1212(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#12120300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 0:19200 >>).

%% @todo Not sure. Related to keys.
%% @todo This packet hasn't been reviewed at all yet.
send_1213(A, B, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#12130300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, A:32/little, B:32/little >>).

%% @todo Related to boss gates.
%% @todo This packet hasn't been reviewed at all yet.
send_1215(A, B, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#12150300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, A:32/little, 0:16, B:16/little >>).

%% @todo Not sure yet. Value is probably a TargetID. Used in Airboard Rally. Replying with the same value starts the race.
%% @todo This packet hasn't been reviewed at all yet.
send_1216(Value, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#12160300:32, 0:32, 16#00011300:32, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64, Value:32/little >>).

%% @todo Send an empty partner card list.
%% @todo This packet hasn't been reviewed at all yet.
send_1501(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#15010300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:96 >>).

%% @todo Send an empty blacklist.
%% @todo This packet hasn't been reviewed at all yet.
send_1512(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#15120300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:46144 >>).

%% @todo NPC related packet, sent when there's an NPC in the area.
%% @todo This packet hasn't been reviewed at all yet.
send_1601(PartyPos, Client=#egs_net{gid=DestGID}) ->
	{ok, << _:32, Bin/bits >>} = file:read_file("p/packet1601.bin"),
	packet_send(Client, << 16#16010300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, PartyPos:32/little, Bin/binary >>).

%% @doc Send the player's NPC and PM information.
%% @todo The value 4 is the card priority. Find what 3 is. When sending, the first 0 is an unknown value.
%% @todo This packet hasn't been reviewed at all yet.
send_1602(Client=#egs_net{gid=DestGID}) ->
	NPCList = egs_npc_db:all(),
	NbNPC = length(NPCList),
	Bin = iolist_to_binary([<< NPCid:8, 0, 4, 0, 3, 0:24 >> || {NPCid, _Data} <- NPCList]),
	MiddlePaddingSize = 8 * (344 - byte_size(Bin)),
	PMName = "My PM",
	UCS2PMName = << << X:8, 0:8 >> || X <- PMName >>,
	EndPaddingSize = 8 * (64 - byte_size(UCS2PMName)),
	packet_send(Client, << 16#16020300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:96,
		Bin/binary, 0:MiddlePaddingSize, NbNPC, 0:24, UCS2PMName/binary, 0:EndPaddingSize, 0:32 >>).

%% @doc Send the list of parties to join.
%% @todo Handle lists of parties.
%% @todo Probably has to handle a LID here, although it should always be 0.
send_1701(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#17010300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:96 >>).

%% @doc Party information.
%% @todo Handle existing parties.
%% @todo This packet hasn't been reviewed at all yet.
send_1706(CharName, Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#17060300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		16#00000300:32, 16#d5c0faff:32, 0:64, CharName/binary,
		16#78000000:32, 16#01010000:32, 0:1536, 16#0100c800:32, 16#0601010a:32, 16#ffffffff:32, 0:32 >>).

%% @doc Party settings. Item distribution is random for now.
%% @todo Handle correctly.
%% @todo This packet hasn't been reviewed at all yet.
send_170a(Client=#egs_net{gid=DestGID}) ->
	packet_send(Client, << 16#170a0300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 16#01010c08:32 >>).

%% @todo Find what the heck this packet is.
%% @todo This packet hasn't been reviewed at all yet.
send_170c(Client=#egs_net{gid=DestGID}) ->
	{ok, File} = file:read_file("p/packet170c.bin"),
	packet_send(Client, << 16#170c0300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, File/binary >>).

%% @doc Send the background to use for the counter.
send_1711(Bg, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#17110300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, Bg:8, 0:24 >>).

%% @doc NPC shop request reply.
send_1a02(A, B, C, D, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#1a020300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:96,
		A:16/little, B:16/little, C:16/little, D:16/little >>).

%% @doc Lumilass available hairstyles/headtypes handler.
send_1a03(User, Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	{ok, Conf} = file:consult("priv/lumilass.conf"),
	NbHeadtypes = proplists:get_value({headtypes, User#users.gender, User#users.race}, Conf, 0),
	HairstylesList = proplists:get_value({hairstyles, User#users.gender}, Conf),
	NbHairstyles = length(HairstylesList),
	HairstylesBin = iolist_to_binary([ << N:32 >> || N <- HairstylesList]),
	packet_send(Client, << 16#1a030300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:96,
		NbHairstyles:32/little, NbHeadtypes:32/little, 0:416, HairstylesBin/binary, 0:32 >>).

%% @doc PP cube handler.
%% @todo The 4 bytes before the file may vary. Everything past that is the same. Figure things out.
%% @todo This packet hasn't been reviewed at all yet.
send_1a04(Client=#egs_net{gid=DestGID}) ->
	{ok, File} = file:read_file("p/ppcube.bin"),
	packet_send(Client, << 16#1a040300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 0:32, File/binary >>).

%% @doc Available types handler. Enable all 16 types.
send_1a07(Client=#egs_net{gid=DestGID, lid=DestLID}) ->
	packet_send(Client, << 16#1a070300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:160,
		16#01010101:32, 16#01010101:32, 16#01010101:32, 16#01010101:32 >>).

%% Common binary building functions.

%% @todo Handle class levels.
build_char_level(#users{type=Type, level=Level, exp=EXP, blastbar=BlastBar, luck=Luck, money=Money}) ->
	ClassesBin = case Type of
		npc ->
			<<	16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32,
				16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32,
				16#4e4f4630:32, 16#08000000:32, 0:32, 0:32, 16#4e454e44:32 >>;
		_ ->
			<<	0:160,
				16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32,
				16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32 >>
	end,
	PlayTime = 0, %% @todo
	<< Level:32/little, BlastBar:16/little, Luck:8, 0:40, EXP:32/little, 0:32, Money:32/little, PlayTime:32/little, ClassesBin/binary >>.

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

%% Utility functions.

%% @doc Prepare a packet. Return the real size and padding at the end.
packet_prepare(Packet) ->
	Size = 4 + byte_size(Packet),
	case Size rem 4 of
		0 -> {ok, Size, <<>>};
		2 -> {ok, Size + 2, << 0:16 >>};
		_ -> {error, badarg}
	end.

%% @doc Send a packet. The packet argument must not contain the size field.
packet_send(Client, Packet) ->
	{ok, Size, Padding} = packet_prepare(Packet),
	packet_send(Client, << Size:32/little, Packet/binary, Padding/binary >>, Size).

%% Send a normal command.
packet_send(#egs_net{socket=Socket, transport=Transport}, Packet, Size)
		when Size =< 16#4000 ->
	Transport:send(Socket, Packet);
%% Send a fragmented command when size is too big.
packet_send(Client, Packet, Size) ->
	packet_fragment_send(Client, Packet, Size, 0).

%% Send the last chunk of a fragmented command.
packet_fragment_send(#egs_net{socket=Socket, transport=Transport}, Packet,
		Size, Current) when Size - Current =< 16#4000 ->
	FragmentSize = 16#10 + byte_size(Packet),
	Fragment = << FragmentSize:32/little, 16#0b030000:32, Size:32/little, Current:32/little, Packet/binary >>,
	Transport:send(Socket, Fragment);
%% Send another chunk of a fragmented command.
packet_fragment_send(Client=#egs_net{socket=Socket, transport=Transport}, Packet,
		Size, Current) ->
	<< Chunk:131072/bits, Rest/bits >> = Packet,
	Fragment = << 16#10400000:32, 16#0b030000:32, Size:32/little, Current:32/little, Chunk/binary >>,
	Transport:send(Socket, Fragment),
	packet_fragment_send(Client, Rest, Size, Current + 16#4000).

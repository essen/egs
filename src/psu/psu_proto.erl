%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
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

-module(psu_proto).
-compile(export_all).

-include("include/records.hrl").

%% @doc Log the message.
log(Msg) ->
	io:format("~p ~s~n", [get(gid), Msg]).

%% @spec log(Msg, FmtVars) -> ok
%% @doc Format and log the message.
log(Msg, FmtVars) ->
	FmtMsg = io_lib:format(Msg, FmtVars),
	log(FmtMsg).

%% @spec assert() -> ok
%% @doc Log a detailed message when the function is called.
-define(ASSERT(), log("assert error in module ~p on line ~p", [?MODULE, ?LINE])).

%% @spec assert(A, B) -> ok
%% @doc Log a detailed message when the assertion A =:= B fails.
-define(ASSERT_EQ(A, B), if A =:= B -> ok; true -> log("assert error in module ~p on line ~p", [?MODULE, ?LINE]) end).

%% @spec parse(Packet) -> Result
%% @doc Parse the packet and return a result accordingly.
parse(<< Size:32/little, Command:16, Channel:8, _Unknown:8, Data/bits >>) ->
	parse(Size, Command, Channel, Data).

%% @todo Maybe we shouldn't ignore it?
%% @todo VarI is probably animation state related and defines what the player is doing.
parse(Size, 16#0102, 2, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, _FromGID:32/little, VarC:32/little, VarD:32/little,
		VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, _TargetGID:32/little, _TargetLID:32/little,
		_VarI:8, _IntDir:24/little, VarJ:32/little, _X:32/little-float, _Y:32/little-float, _Z:32/little-float,
		_QuestID:32/little, _ZoneID:32/little, _MapID:32/little, _EntryID:32/little, VarK:32/little >> = Data,
	?ASSERT_EQ(Size, 92),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarJ, 0),
	?ASSERT_EQ(VarK, 0),
	ignore;

%% @todo One of the missing events is probably learning a new PA.
parse(Size, 16#0105, Channel, Data) ->
	<<	_LID:16/little, _VarB:16/little, VarC:32/little, _FromGID:32/little, VarD:32/little, VarE:32/little, TypeID:32/little, GID:32/little,
		VarF:32/little, VarG:32/little, TargetGID:32/little, TargetLID:32/little, ItemIndex:8, EventID:8, _PAIndex:8, VarH:8, VarI:32/little, Rest/bits >> = Data,
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(TypeID, 0),
	?ASSERT_EQ(GID, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	Event = case EventID of
		1 -> item_equip;
		2 -> item_unequip;
		3 -> ignore; %% @todo item_link_pa;
		4 -> ignore; %% @todo item_unlink_pa;
		5 -> item_drop;
		7 -> ignore; %% @todo item_learn_pa;
		8 -> ignore; %% @todo item_use;
		9 -> item_set_trap;
		18 -> ignore; %% @todo item_unlearn_pa;
		_ -> log("unknown 0105 EventID ~p", [EventID])
	end,
	case Event of
		item_drop ->
			?ASSERT_EQ(Size, 76),
			<< _Quantity:32/little, _PosX:32/little-float, _PosY:32/little-float, _PosZ:32/little-float >> = Rest,
			%~ {Event, ItemIndex, Quantity, ...};
			ignore;
		ignore ->
			?ASSERT_EQ(Size, 60),
			ignore;
		_ ->
			?ASSERT_EQ(Size, 60),
			{Event, ItemIndex, TargetGID, TargetLID, VarH, VarI}
	end;

parse(Size, 16#010a, Channel, Data) ->
	<<	HeaderLID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little,
		_GID:32/little, BodyLID:32/little, EventID:16/little, QuantityOrColor:8, VarK:8, Param:16/bits, VarL:16 >> = Data,
	?ASSERT_EQ(Size, 60),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(HeaderLID, BodyLID),
	case EventID of
		1 ->
			<< ShopID:16/little >> = Param,
			?ASSERT_EQ(QuantityOrColor, 0),
			?ASSERT_EQ(VarK, 0),
			?ASSERT_EQ(VarL, 0),
			{npc_shop_enter, ShopID};
		2 ->
			<< ShopItemIndex:16/little >> = Param,
			?ASSERT_EQ(QuantityOrColor, VarK),
			?ASSERT_EQ(VarL, 0),
			{npc_shop_buy, ShopItemIndex, QuantityOrColor};
		3 ->
			<< InventoryItemIndex:8, _Unknown:8 >> = Param,
			?ASSERT_EQ(VarK, 0),
			?ASSERT_EQ(VarL, 0),
			{npc_shop_sell, InventoryItemIndex, QuantityOrColor};
		4 -> ignore; %% @todo npc_shop_gift_wrap
		5 ->
			<< ShopID:16/little >> = Param,
			?ASSERT_EQ(QuantityOrColor, 0),
			?ASSERT_EQ(VarK, 0),
			?ASSERT_EQ(VarL, 0),
			{npc_shop_leave, ShopID};
		6 -> ?ASSERT(), ignore
	end;

%% @todo We probably want to check some of those values and save the others. It's mostly harmless though, ignore for now.
%% @todo We also probably should send the spawn to everyone in response to this command rather than on area_change.
parse(Size, 16#010b, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, HeaderGID:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little,
		BodyGID:32/little, _PartyPosOrLID:32/little, VarJ:16/little, _IntDir:16/little, _X:32/little-float, _Y:32/little-float, _Z:32/little-float,
		VarK:32/little, VarL:32/little, _QuestID:32/little, _ZoneID:32/little, _MapID:32/little, _EntryID:32/little >> = Data,
	?ASSERT_EQ(Size, 92),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	?ASSERT_EQ(VarK, 0),
	?ASSERT_EQ(VarL, 0),
	?ASSERT_EQ(HeaderGID, BodyGID),
	ignore; %% @todo player_enter_area

parse(Size, 16#0110, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, HeaderGID:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, BodyGID:32/little, _PartyPosOrLID:32/little, EventID:32/little, Param:32/little >> = Data,
	?ASSERT_EQ(Size, 60),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(HeaderGID, BodyGID),
	case EventID of
		1 -> ?ASSERT_EQ(Param, 0), ?ASSERT(), ignore;
		2 -> ?ASSERT_EQ(Param, 0), player_type_capabilities_request;
		3 -> ignore; %% @todo {player_type_change, Param};
		4 -> ?ASSERT_EQ(Param, 0), ignore; %% @todo (related to npc death)
		6 -> ?ASSERT_EQ(Param, 0), ignore; %% @todo
		7 -> ?ASSERT_EQ(Param, 0), player_death;
		8 -> ?ASSERT_EQ(Param, 0), player_death_return_to_lobby;
		9 -> ?ASSERT_EQ(Param, 10), ignore; %% @todo
		10 -> ignore; %% @todo {player_online_status_change, Param};
		_ -> log("unknown 0110 EventID ~p", [EventID])
	end;

parse(Size, 16#020b, Channel, Data) ->
	<<	LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, Slot:32/little, VarJ:8, BackToPreviousField:8, VarK:16/little >> = Data,
	?ASSERT_EQ(Size, 52),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(LID, 16#ffff),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	?ASSERT_EQ(VarK, 0),
	AtomBackToPreviousField = if BackToPreviousField =:= 0 -> false; true -> true end,
	{char_select_enter, Slot, AtomBackToPreviousField};

parse(Size, 16#020d, Channel, Data) ->
	<<	LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, AuthGID:32/little, AuthKey:32/bits, VarJ:32/little, VarK:32/little >> = Data,
	?ASSERT_EQ(Size, 60),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(LID, 16#ffff),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	?ASSERT_EQ(VarK, 0),
	{system_key_auth_request, AuthGID, AuthKey};

parse(Size, 16#0217, Channel, Data) ->
	<< LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(LID, 16#ffff),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	system_game_server_request;

parse(Size, 16#0219, Channel, Data) ->
	<<	LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, UsernameBlob:192/bits, PasswordBlob:192/bits, _VarJ:32/little, _VarK:32/little >> = Data,
	?ASSERT_EQ(Size, 100),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(LID, 16#ffff),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	[Username|_] = re:split(UsernameBlob, "\\0"),
	[Password|_] = re:split(PasswordBlob, "\\0"),
	{system_login_auth_request, Username, Password};

parse(Size, 16#021c, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	char_load_complete;

parse(Size, 16#021d, Channel, Data) ->
	<<	_LID:16/little, VarB:16/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, VarJ:32/little, _EntryID:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	unicube_request;

parse(Size, 16#021f, Channel, Data) ->
	<<	_LID:16/little, VarB:16/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, VarJ:32/little, UniID:32/little, EntryID:32/little >> = Data,
	?ASSERT_EQ(Size, 52),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	Selection = case UniID of
		0 -> cancel;
		_ -> UniID
	end,
	{unicube_select, Selection, EntryID};

%% @doc Seems to be exactly the same as 023f, except Channel, and that it's used for JP clients.
parse(Size, 16#0226, Channel, Data) ->
	<<	LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, Page:8, Language:8, VarJ:16/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 3),
	?ASSERT_EQ(LID, 16#ffff),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	{system_motd_request, Page, language_integer_to_atom(Language)};

%% @doc Whether the MOTD was accepted. Safely ignored.
parse(Size, 16#0227, Channel, Data) ->
	<<	LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, _AcceptMOTD:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(LID, 16#ffff),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	ignore; %% {system_motd_accept, true|false (AcceptMOTD)};

%% @doc Seems to be exactly the same as 0226, except Channel, and that it's used for US clients.
parse(Size, 16#023f, Channel, Data) ->
	<<	LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, Page:8, Language:8, VarJ:16/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(LID, 16#ffff),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	{system_motd_request, Page, language_integer_to_atom(Language)};

parse(_Size, 16#0304, Channel, Data) ->
	<<	_LID:16/little, VarB:16/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, VarJ:32/little, FromTypeID:32, FromGID:32/little,
		VarK:32/little, VarL:32/little, ChatType:8, ChatCutIn:8, ChatCutInAngle:8, ChatMsgLength:8,
		ChatChannel:8, ChatCharacterType:8, VarN:8, _VarO:8, FromName:512/bits, ChatMsg/bits >> = Data,
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	?ASSERT_EQ(VarK, 0),
	?ASSERT_EQ(VarL, 0),
	?ASSERT_EQ(VarN, 0),
	Modifiers = {chat_modifiers, ChatType, ChatCutIn, ChatCutInAngle, ChatMsgLength, ChatChannel, ChatCharacterType},
	{chat, FromTypeID, FromGID, FromName, Modifiers, ChatMsg};

%% @doc Probably safely ignored. _AreaNb is apparently replied with the same value sent by 0205, the one after EntryID.
parse(Size, 16#0806, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, _AreaNb:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	ignore;

parse(Size, 16#0807, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little,
		QuestID:32/little, ZoneID:16/little, MapID:16/little, EntryID:16/little, _AreaChangeNb:16/little, PartyPos:32/little >> = Data,
	?ASSERT_EQ(Size, 60),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	{area_change, QuestID, ZoneID, MapID, EntryID, PartyPos};

%% @doc Probably safely ignored. _AreaNb is apparently replied with the same value sent by 0208.
parse(Size, 16#0808, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, _AreaNb:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	ignore;

%% @todo Check that _Rest is full of 0s.
parse(Size, 16#080c, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, VarJ:32/little, NPCid:16/little,
		_VarK:16/little, VarL:32/little, VarM:32/little, VarN:16/little, _Var0:16/little, _Rest/bits >> = Data,
	?ASSERT_EQ(Size, 648),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 16#ffffffff),
	?ASSERT_EQ(VarL, 16#ffffffff),
	?ASSERT_EQ(VarM, 16#ffffffff),
	?ASSERT_EQ(VarN, 0),
	{npc_force_invite, NPCid};

%% @doc This command should be safely ignored. Probably indicates that a non-mission area change was successful.
parse(Size, 16#080d, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	ignore;

%% @todo Make sure the Language field is the right one.
parse(Size, 16#080e, Channel, Data) ->
	<<	LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little,
		VarJ:8, Language:8, VarK:8, Entrance:8, Platform:8, VarM:24/little, Revision:8, Minor:4, _VarN:12, Major:4, _VarO:4, VarP:32/little, VarQ:32/little, VarR:32/little >> = Data,
	?ASSERT_EQ(Size, 68),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(LID, 16#ffff),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	?ASSERT_EQ(VarK, 1),
	?ASSERT_EQ(VarM, 0),
	?ASSERT_EQ(VarP, 0),
	?ASSERT_EQ(VarQ, 0),
	?ASSERT_EQ(VarR, 0),
	AtomPlatform = case Platform of
		0 -> ps2;
		1 -> pc;
		_ -> log("unknown 080e Platform ~p", [Platform]), unknown
	end,
	Version = Major * 1000000 + Minor * 1000 + Revision,
	{system_client_version_info, Entrance, language_integer_to_atom(Language), AtomPlatform, Version};

%% @todo Find out what it's really doing!
parse(Size, 16#080f, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, _PartyPos:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	ignore;

parse(Size, 16#0811, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little,
		_CounterType:8, VarJ:8, FromZoneID:16/little, FromMapID:16/little, FromEntryID:16/little, CounterID:32/little, VarK:32/little >> = Data,
	?ASSERT_EQ(Size, 60),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 41),
	?ASSERT_EQ(VarK, 16#ffffffff),
	{counter_enter, CounterID, FromZoneID, FromMapID, FromEntryID};

parse(Size, 16#0812, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	counter_leave;

parse(Size, 16#0813, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, VarJ:32/little, NPCid:32/little >> = Data,
	?ASSERT_EQ(Size, 52),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 16#ffffffff),
	{npc_invite, NPCid};

%% @doc This command should be safely ignored. Probably indicates that a mission area change was successful.
parse(Size, 16#0814, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	ignore;

%% @doc This command should be safely ignored. Probably indicates that a non-mission area change was successful.
parse(Size, 16#0815, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	ignore;

parse(Size, 16#0818, Channel, Data) ->
	<< LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little,
		VarH:32/little, VarI:32/little, BinGPU:512/bits, BinCPU:384/bits, _VarJ:32/little >> = Data,
	?ASSERT_EQ(Size, 160),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(LID, 16#ffff),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	[_StringCPU|_] = re:split(BinCPU, "\\0", [{return, binary}]),
	[_StringGPU|_] = re:split(BinGPU, "\\0", [{return, binary}]),
	ignore; %% @todo {system_client_hardware_info, StringGPU, StringCPU}; worth logging?

parse(Size, 16#0a10, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, ItemID:32 >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	{item_description_request, ItemID};

parse(Size, 16#0b05, _Channel, _Data) ->
	?ASSERT_EQ(Size, 8),
	ignore;

parse(Size, 16#0c01, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, QuestID:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	{mission_start, QuestID};

parse(Size, 16#0c05, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, CounterID:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	{counter_quest_files_request, CounterID};

%% @doc On official, Price = Rate x 200.
parse(Size, 16#0c07, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, _QuestID:32/little, _Rate:32/little >> = Data,
	?ASSERT_EQ(Size, 52),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	lobby_transport_request;

%% @doc This command should be safely ignored. Probably indicates that a mission area change was successful.
parse(Size, 16#0c0d, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	ignore;

parse(Size, 16#0c0e, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	mission_abort;

parse(Size, 16#0c0f, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, CounterID:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	{counter_quest_options_request, CounterID};

%% @todo Return a tuple rather than a binary!
%% @todo Parse and validate the data here rather than in psu_game.
parse(Size, 16#0d02, Channel, Data) ->
	<<	VarA:32/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, Slot:32/little, CharBin/bits >> = Data,
	?ASSERT_EQ(Size, 324),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	{char_select_create, Slot, CharBin};

parse(Size, 16#0d06, Channel, Data) ->
	<<	VarA:32/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	char_select_request;

%% @todo Return a tuple rather than a binary!
parse(Size, 16#0d07, Channel, Data) ->
	<<	VarA:32/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little,
		TextDisplaySpeed:8, Sound:8, MusicVolume:8, SoundEffectVolume:8, Vibration:8, RadarMapDisplay:8,
		CutInDisplay:8, MainMenuCursorPosition:8, VarJ:8, Camera3rdY:8, Camera3rdX:8, Camera1stY:8, Camera1stX:8,
		Controller:8, WeaponSwap:8, LockOn:8, Brightness:8, FunctionKeySetting:8, _VarK:8, ButtonDetailDisplay:8, VarL:32/little >> = Data,
	?ASSERT_EQ(Size, 68),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	?ASSERT_EQ(VarL, 0),
	%% Make sure the options are valid.
	true = TextDisplaySpeed =< 1,
	true = Sound =< 1,
	true = MusicVolume =< 9,
	true = SoundEffectVolume =< 9,
	true = Vibration =< 1,
	true = RadarMapDisplay =< 1,
	true = CutInDisplay =< 1,
	true = MainMenuCursorPosition =< 1,
	true = Camera3rdY =< 1,
	true = Camera3rdX =< 1,
	true = Camera1stY =< 1,
	true = Camera1stX =< 1,
	true = Controller =< 1,
	true = WeaponSwap =< 1,
	true = LockOn =< 1,
	true = Brightness =< 4,
	true = FunctionKeySetting =< 1,
	true = ButtonDetailDisplay =< 2,
	%% Options are considered safe past this point.
	Options = {options, TextDisplaySpeed, Sound, MusicVolume, SoundEffectVolume, Vibration, RadarMapDisplay,
						CutInDisplay, MainMenuCursorPosition, Camera3rdY, Camera3rdX, Camera1stY, Camera1stX,
						Controller, WeaponSwap, LockOn, Brightness, FunctionKeySetting, ButtonDetailDisplay},
	{player_options_change, psu_characters:options_tuple_to_binary(Options)}; %% @todo {player_options_change, Options};

%% @todo Many unknown vars in the command header.
parse(Size, 16#0e00, Channel, Data) ->
	<< _UnknownVars:288/bits, NbHits:32/little, _PartyPosOrLID:32/little, _HitCommandNb:32/little, Hits/bits >> = Data,
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(Size, 56 + NbHits * 80),
	{hits, parse_hits(Hits, [])};

parse(Size, 16#0f0a, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, BlockID:16/little, ListNb:16/little,
		ObjectNb:16/little, _MapID:16/little, ObjectID:16/little, VarJ:16/little, ObjectTargetID:32/little,
		ObjectType:16/little, VarK:16/little, ObjectBaseTargetID:16/little, VarL:16/little, PartyPosOrLID:32/little,
		VarN:32/little, VarO:32/little, VarP:32/little, VarQ:32/little, VarR:32/little, VarS:32/little,
		VarT:32/little, VarU:32/little, ObjectType2:16/little, EventID:8, VarV:8, VarW:32/little >> = Data,
	?ASSERT_EQ(Size, 112),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarK, 0),
	?ASSERT_EQ(VarP, 16#ffffffff),
	?ASSERT_EQ(VarQ, 16#ffffffff),
	?ASSERT_EQ(VarR, 16#ffffffff),
	?ASSERT_EQ(VarS, 0),
	?ASSERT_EQ(VarT, 0),
	?ASSERT_EQ(VarU, 0),
	?ASSERT_EQ(ObjectType, ObjectType2),
	case [ObjectType, EventID] of
		[ 5, 13] ->
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_switch_on, ObjectID};
		[ 5, 14] ->
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_switch_off, ObjectID};
		[ 9, 20] ->
			%% @todo We probably need to handle it for Airboard Rally.
			ignore; %% object_sensor_trigger
		[14,  0] ->
			?ASSERT_EQ(ObjectID, 16#ffff),
			?ASSERT_EQ(ObjectTargetID, 16#ffffffff),
			?ASSERT_EQ(ObjectBaseTargetID, 16#ffff),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			{object_warp_take, BlockID, ListNb, ObjectNb};
		[22, 12] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(ObjectTargetID, 16#ffffffff),
			?ASSERT_EQ(ObjectBaseTargetID, 16#ffff),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_key_console_enable, ObjectID};
		[22, 23] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(ObjectTargetID, 16#ffffffff),
			?ASSERT_EQ(ObjectBaseTargetID, 16#ffff),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_key_console_init, ObjectID};
		[22, 24] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(ObjectTargetID, 16#ffffffff),
			?ASSERT_EQ(ObjectBaseTargetID, 16#ffff),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_key_console_open_gate, ObjectID};
		[31, 12] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(ObjectTargetID, 16#ffffffff),
			?ASSERT_EQ(ObjectBaseTargetID, 16#ffff),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_key_enable, ObjectID};
		[48,  4] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_boss_gate_enter, ObjectID};
		[48,  5] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_boss_gate_leave, ObjectID};
		[48,  6] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_boss_gate_activate, ObjectID};
		[48,  7] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			?ASSERT(),
			ignore; %% @todo object_boss_gate_???
		[49,  3] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(ObjectTargetID, 16#ffffffff),
			?ASSERT_EQ(ObjectBaseTargetID, 16#ffff),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_crystal_activate, ObjectID};
		[50,  9] ->
			%% @todo Make NPC characters be healed too. This would use VarN and VarO as PartyPosOrLID, and VarV would be > 1.
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(ObjectTargetID, 16#ffffffff),
			?ASSERT_EQ(ObjectBaseTargetID, 16#ffff),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_healing_pad_tick, [PartyPosOrLID]};
		[51,  1] ->
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(ObjectTargetID, VarN),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_goggle_target_activate, ObjectID};
		[56, 25] ->
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_chair_sit, ObjectTargetID};
		[56, 26] ->
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_chair_stand, ObjectTargetID};
		[57, 12] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(ObjectTargetID, 16#ffffffff),
			?ASSERT_EQ(ObjectBaseTargetID, 16#ffff),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			{object_vehicle_boost_enable, ObjectID};
		[57, 28] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(ObjectTargetID, 16#ffffffff),
			?ASSERT_EQ(ObjectBaseTargetID, 16#ffff),
			?ASSERT_EQ(VarL, 116),
			?ASSERT_EQ(VarN, 16#ffffffff),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			{object_vehicle_boost_respawn, ObjectID};
		[71, 27] ->
			?ASSERT_EQ(VarJ, 134),
			?ASSERT_EQ(ObjectTargetID, VarN),
			?ASSERT_EQ(VarO, 16#ffffffff),
			?ASSERT_EQ(VarV, 1),
			?ASSERT_EQ(VarW, 0),
			?ASSERT(),
			ignore; %% @todo object_trap(3rd)_???
		_ -> %% Unhandled actions.
			log("unknown 0f0a ObjectType ~p EventID ~p", [ObjectType, EventID]),
			ignore
	end;

parse(Size, 16#1007, Channel, Data) ->
	<< VarA:32/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, PartyPos:32/little, _Name:512/bits >> = Data,
	?ASSERT_EQ(Size, 112),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	{party_remove_member, PartyPos};

parse(Size, 16#1701, Channel, Data) ->
	<<	VarA:32/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little,
		VarG:32/little, VarH:32/little, VarI:32/little, VarJ:32/little, VarK:32/little >> = Data,
	?ASSERT_EQ(Size, 52),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(VarJ, 0),
	?ASSERT_EQ(VarK, 16#ffffffff),
	counter_join_party_request;

parse(Size, 16#1705, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	counter_party_info_request;

%% @todo Currently selected quest. Probably need to broadcast it to other players in the party.
parse(Size, 16#1707, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, _QuestID:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	ignore; %% @todo {counter_quest_selection, QuestID}

parse(Size, 16#1709, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	counter_party_options_request;

parse(Size, 16#170b, Channel, Data) ->
	<< _LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little >> = Data,
	?ASSERT_EQ(Size, 44),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	counter_background_locations_request;

parse(Size, 16#1710, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little,
		VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little, CounterID:32/little >> = Data,
	?ASSERT_EQ(Size, 48),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	{counter_options_request, CounterID};

parse(Size, 16#1a01, Channel, Data) ->
	<<	HeaderLID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little,
		BodyLID:32/little, ShopID:32/little, EventID:32/little, VarJ:32/little, VarK:32/little >> = Data,
	?ASSERT_EQ(Size, 64),
	?ASSERT_EQ(Channel, 2),
	?ASSERT_EQ(VarA, 0),
	?ASSERT_EQ(VarB, 0),
	?ASSERT_EQ(VarC, 0),
	?ASSERT_EQ(VarD, 0),
	?ASSERT_EQ(VarE, 0),
	?ASSERT_EQ(VarF, 0),
	?ASSERT_EQ(VarG, 0),
	?ASSERT_EQ(VarH, 0),
	?ASSERT_EQ(VarI, 0),
	?ASSERT_EQ(HeaderLID, BodyLID),
	case EventID of
		0 -> ?ASSERT_EQ(VarJ, 0), {npc_shop_request, ShopID};
		2 ->
			?ASSERT_EQ(ShopID, 0),
			?ASSERT_EQ(VarJ, 0),
			lumilass_options_request;
		3 ->
			?ASSERT_EQ(ShopID, 0),
			?ASSERT_EQ(VarJ, 0),
			ppcube_request;
		4 -> ?ASSERT_EQ(ShopID, 0), ignore; %% @todo ppcube_recharge_all
		5 -> ?ASSERT_EQ(ShopID, 0), ignore; %% @todo ppcube_recharge_one
		6 ->
			?ASSERT_EQ(ShopID, 0),
			?ASSERT_EQ(VarJ, 0),
			?ASSERT(), ignore; %% @todo put_on_outfit
		7 ->
			?ASSERT_EQ(ShopID, 0),
			?ASSERT(), ignore; %% @todo remove_outfit
		9 ->
			?ASSERT_EQ(ShopID, 0),
			?ASSERT_EQ(VarJ, 0),
			?ASSERT_EQ(VarK, 0),
			player_type_availability_request;
		_ -> log("unknown 1a01 EventID ~p", [EventID])
	end;

%% @doc Unknown command,
parse(_Size, Command, Channel, _Data) ->
	{command, Command, Channel}.

%% @todo Many unknown vars in the hit values.
parse_hits(<< >>, Acc) ->
	lists:reverse(Acc);
parse_hits(Hits, Acc) ->
	<< A:224/bits, B:128/bits, _C:128/bits, _D:160/bits, Rest/bits >> = Hits,
	<< _PosX1:32/little-float, _PosY1:32/little-float, _PosZ1:32/little-float, FromTargetID:32/little, ToTargetID:32/little, _AEnd1:32, _AEnd2:32 >> = A,
	%~ << Stuff2:32, PosX2:32/little-float, PosY2:32/little-float, PosZ2:32/little-float >> = B, %% player
	%~ << Stuff3:32, PosX3:32/little-float, PosY3:32/little-float, PosZ3:32/little-float >> = C, %% target
	%~ << D1:32, D2:32, D3:32, D4:32, D5:32 >> = D,
	parse_hits(Rest, [{hit, FromTargetID, ToTargetID, A, B}|Acc]).

%% @doc Send character appearance and other information.
%% @todo Probably don't pattern match the data like this...
send_010d(CharUser, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	CharGID = CharUser#users.gid,
	CharLID = CharUser#users.lid,
	<< _:640, CharBin/bits >> = psu_characters:character_user_to_binary(CharUser),
	packet_send(Socket, << 16#010d0300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little,
		0:64, 1:32/little, 0:32, 16#00000300:32, 16#ffff0000:32, 0:32, CharGID:32/little,
		0:192, CharGID:32/little, CharLID:32/little, 16#ffffffff:32, CharBin/binary >>).

%% @doc Trigger a character-related event.
send_0111(CharUser, EventID, Client) ->
	send_0111(CharUser, EventID, 0, Client).
send_0111(#users{gid=CharGID, lid=CharLID}, EventID, Param, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#01110300:32, DestLID:16/little, 0:48, CharGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		CharGID:32/little, CharLID:32/little, EventID:32/little, Param:32/little >>).

%% @doc Update the character level, blastbar, luck and money information.
send_0115(CharUser, Client) ->
	send_0115(CharUser, 16#ffffffff, Client).
send_0115(#users{gid=CharGID, lid=CharLID, character=Character}, EnemyTargetID, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#01150300:32, DestLID:16/little, 0:48, CharGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		CharGID:32/little, CharLID:32/little, EnemyTargetID:32/little, (build_char_level(Character))/binary >>).

%% @todo Handle class levels.
build_char_level(#characters{type=Type, mainlevel=#level{number=Level, exp=EXP}, blastbar=BlastBar, luck=Luck, money=Money, playtime=PlayTime}) ->
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
	<< Level:32/little, BlastBar:16/little, Luck:8, 0:40, EXP:32/little, 0:32, Money:32/little, PlayTime:32/little, ClassesBin/binary >>.

%% @doc Revive player with optional SEs.
%% @todo SEs.
send_0117(#users{gid=CharGID, lid=CharLID, character=#characters{currenthp=HP}}, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	SE = << 0:64 >>,
	packet_send(Socket, << 16#01170300:32, DestLID:16/little, 0:48, CharGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		CharGID:32/little, CharLID:32/little, SE/binary, HP:32/little, 0:32 >>).

%% @doc Send the zone initialization command.
%% @todo Handle NbPlayers properly. There's more than 1 player!
send_0200(ZoneID, ZoneType, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	Var = case ZoneType of
		mission -> << 16#06000500:32, 16#01000000:32, 0:64, 16#00040000:32, 16#00010000:32, 16#00140000:32 >>;
		myroom -> << 16#06000000:32, 16#02000000:32, 0:64, 16#40000000:32, 16#00010000:32, 16#00010000:32 >>;
		_ -> << 16#00040000:32, 0:160, 16#00140000:32 >>
	end,
	packet_send(Socket, << 16#02000300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		DestLID:16/little, ZoneID:16/little, 1:32/little, 16#ffffffff:32, Var/binary, 16#ffffffff:32, 16#ffffffff:32 >>).

%% @doc Send character location, appearance and other information.
send_0201(CharUser, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	[CharTypeID, GameVersion] = case (CharUser#users.character)#characters.type of
		npc -> [16#00001d00, 255];
		_ -> [16#00001200, 0]
	end,
	CharGID = CharUser#users.gid,
	CharBin = psu_characters:character_user_to_binary(CharUser),
	IsGM = 0,
	OnlineStatus = 0,
	packet_send(Socket, << 16#02010300:32, DestLID:16/little, 0:16, CharTypeID:32, CharGID:32/little,
		0:64, 16#00011300:32, DestGID:32/little, 0:64, CharBin/binary, IsGM:8, 0:8, OnlineStatus:8, GameVersion:8, 0:608 >>).

%% @doc Hello command. Sent when a client connects to the game or login server.
%% @todo Can contain an error message if 0:1024 is setup similar to this: 0:32, 3:32/little, 0:48, Len:16/little, Error/binary, 0:Padding.
send_0202(#client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#020203bf:32, DestLID:16/little, 0:272, DestGID:32/little, 0:1024 >>).

%% @doc Spawn a player with the given GID and LID.
send_0203(#users{gid=CharGID, lid=CharLID}, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#02030300:32, DestLID:16/little, 0:144, 16#00011300:32,
		DestGID:32/little, 0:64, CharGID:32/little, CharLID:32/little >>).

%% @doc Unspawn the given character.
%% @todo The last 4 bytes are probably the number of players remaining in the zone.
send_0204(User, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	CharTypeID = case (User#users.character)#characters.type of
		npc -> 16#00001d00;
		_ -> 16#00001200
	end,
	#users{gid=CharGID, lid=CharLID} = User,
	packet_send(Socket, << 16#02040300:32, DestLID:16/little, 0:16, CharTypeID:32, CharGID:32/little, 0:64,
		16#00011300:32, DestGID:32/little, 0:64, CharGID:32/little, CharLID:32/little, 100:32/little >>).

%% @doc Make the client load a new map.
send_0205(CharUser, IsSeasonal, #client{socket=Socket, gid=DestGID, lid=DestLID, areanb=AreaNb}) ->
	#users{lid=CharLID, area={_QuestID, ZoneID, MapID}, entryid=EntryID} = CharUser,
	packet_send(Socket, << 16#02050300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		16#ffffffff:32, ZoneID:32/little, MapID:32/little, EntryID:32/little, AreaNb:32/little, CharLID:16/little, 0:8, IsSeasonal:8 >>).

%% @doc Indicate to the client that loading should finish.
send_0208(#client{socket=Socket, gid=DestGID, lid=DestLID, areanb=AreaNb}) ->
	packet_send(Socket, << 16#02080300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, AreaNb:32/little >>).

%% @todo No idea what this one does. For unknown reasons it uses channel 2.
%% @todo Handle the DestLID properly?
send_020c(#client{socket=Socket}) ->
	packet_send(Socket, << 16#020c0200:32, 16#ffff0000:32, 0:256 >>).

%% @doc Send the quest file to be loaded by the client.
%% @todo Handle the DestLID properly?
send_020e(QuestData, #client{socket=Socket}) ->
	Size = byte_size(QuestData),
	packet_send(Socket, << 16#020e0300:32, 16#ffff:16, 0:272, Size:32/little, 0:32, QuestData/binary, 0:32 >>).

%% @doc Send the zone file to be loaded.
send_020f(ZoneData, SetID, SeasonID, #client{socket=Socket}) ->
	Size = byte_size(ZoneData),
	packet_send(Socket, << 16#020f0300:32, 16#ffff:16, 0:272, SetID, SeasonID, 0:16, Size:32/little, ZoneData/binary >>).

%% @doc Send the current UNIX time.
send_0210(#client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	UnixTime = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now()))
		- calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	packet_send(Socket, << 16#02100300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:96, UnixTime:32/little >>).

%% @todo No idea what this is doing.
send_0215(UnknownValue, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#02150300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, UnknownValue:32/little >>).

%% @doc Send the game server's IP and port that the client requested.
send_0216(IP, Port, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#02160300:32, DestLID:16/little, 0:144, 16#00000f00:32, DestGID:32/little, 0:64, IP/binary, Port:16/little, 0:16 >>).

%% @doc End of character loading.
send_021b(#client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#021b0300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Send the list of available universes.
send_021e(Universes, #client{socket=Socket}) ->
	NbUnis = length(Universes),
	UnisBin = build_021e_uni(Universes, []),
	packet_send(Socket, << 16#021e0300:32, 0:288, NbUnis:32/little, UnisBin/binary >>).

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
send_0222(UniID, #client{socket=Socket, gid=DestGID}) ->
	{_Type, Name, NbPlayers, MaxPlayers} = egs_universes:read(UniID),
	Padding = 8 * (44 - byte_size(Name)),
	LevelCap = egs_conf:read(level_cap),
	packet_send(Socket, << 16#02220300:32, 16#ffff:16, 0:16, 16#00001200:32, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		UniID:32/little, NbPlayers:16/little, MaxPlayers:16/little, Name/binary, 0:Padding, LevelCap:32/little >>).

%% @doc Send the auth key, or, in case of failure, a related error message.
send_0223(AuthGID, AuthKey, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#02230300:32, 0:160, 16#00000f00:32, DestGID:32/little, 0:64, AuthGID:32/little, AuthKey:32/bits >>).
send_0223(ErrorMsg, #client{socket=Socket, gid=DestGID}) ->
	Length = byte_size(ErrorMsg) div 2 + 2,
	packet_send(Socket, << 16#02230300:32, 0:160, 16#00000f00:32, DestGID:32/little, 0:128, 3:32/little, 0:48, Length:16/little, ErrorMsg/binary, 0:16 >>).

%% @doc Send a MOTD page.
send_0225(MOTD, CurrentPage, #client{socket=Socket, lid=DestLID}) ->
	Tokens = re:split(MOTD, "\n."),
	Msg = << << Line/binary, "\n", 0 >> || Line <- lists:sublist(Tokens, 1 + CurrentPage * 15, 15) >>,
	NbPages = 1 + length(Tokens) div 15,
	Length = byte_size(Msg) div 2 + 2,
	packet_send(Socket, << 16#02250300:32, DestLID:16/little, 0:272, NbPages:8, CurrentPage:8, Length:16/little, Msg/binary, 0:16 >>).

%% @doc Display a notice on the player's screen.
%%      There are four types of notices: dialog, top, scroll and timeout.
%% * dialog: A dialog in the center of the screen, which can be OK'd by players.
%% * top: Horizontal scroll on top of the screen, traditionally used for server-wide messages.
%% * scroll: Vertical scroll on the right of the screen, traditionally used for rare missions obtention messages.
%% * timeout: A dialog in the center of the screen that disappears after Duration seconds.
send_0228(Type, Duration, Message, #client{socket=Socket, gid=DestGID}) ->
	TypeInt = case Type of dialog -> 0; top -> 1; scroll -> 2; timeout -> 3 end,
	UCS2Message = << << X:8, 0:8 >> || X <- Message >>,
	packet_send(Socket, << 16#02280300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		TypeInt:32/little, Duration:32/little, UCS2Message/binary, 0:16 >>).

%% @todo No idea!
%% @todo This packet hasn't been reviewed at all yet.
send_022c(A, B, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#022c0300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, A:16/little, B:16/little >>).

%% @todo Not sure. Sent when going to or from room. Possibly when changing universes too?
send_0230(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#02300300:32, 16#ffff:16, 0:16, 16#00011300:32, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Forward the player to a website. The website will open when the player closes the game. Used for login issues mostly.
send_0231(URL, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	URLBin = list_to_binary(URL),
	Length = byte_size(URLBin) + 1,
	Padding = 8 * (512 - Length - 1),
	packet_send(Socket, << 16#02310300:32, DestLID:16/little, 0:16, 16#00000f00:32, DestGID:32/little, 0:64,
		16#00000f00:32, DestGID:32/little, 0:64, Length:32/little, URLBin/binary, 0:Padding >>).

%% @doc Send the list of players already spawned in the zone when entering it.
send_0233(Users, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	NbUsers = length(Users),
	Bin = build_0233_users(Users, []),
	packet_send(Socket, << 16#02330300:32, DestLID:16/little, 0:16, 16#00001200:32, DestGID:32/little, 0:64,
		16#00011300:32, DestGID:32/little, 0:64, NbUsers:32/little, Bin/binary, 0:608 >>).

build_0233_users([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
build_0233_users([User|Tail], Acc) ->
	Bin = psu_characters:character_user_to_binary(User),
	build_0233_users(Tail, [<< Bin/binary, 0:32 >>|Acc]).

%% @doc Start the zone handling: load the zone file and the objects sent separately.
send_0236(#client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#02360300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Chat message.
send_0304(FromGID, ChatTypeID, ChatGID, ChatName, ChatModifiers, ChatMessage, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	{chat_modifiers, ChatType, ChatCutIn, ChatCutInAngle, ChatMsgLength, ChatChannel, ChatCharacterType} = ChatModifiers,
	packet_send(Socket, << 16#03040300:32, DestLID:16/little, 0:16, 16#00011300:32, FromGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64,
		ChatTypeID:32, ChatGID:32/little, 0:64, ChatType:8, ChatCutIn:8, ChatCutInAngle:8, ChatMsgLength:8,
		ChatChannel:8, ChatCharacterType:8, 0:16, ChatName/binary, ChatMessage/binary >>).

%% @todo Inventory related. Doesn't seem to do anything.
send_0a05(#client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#0a050300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Send the list of ItemUUID for the items in the inventory.
send_0a06(CharUser, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	Len = length((CharUser#users.character)#characters.inventory),
	UUIDs = lists:seq(1, Len),
	Bin = iolist_to_binary([ << N:32/little >> || N <- UUIDs]),
	Blanks = lists:seq(1, 60 - Len),
	Bin2 = iolist_to_binary([ << 16#ffffffff:32 >> || _N <- Blanks]),
	packet_send(Socket, << 16#0a060300:32, DestLID:16/little, 0:48, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64, Bin/binary, Bin2/binary >>).

%% @doc Send an item's description.
send_0a11(ItemID, ItemDesc, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	Length = 1 + byte_size(ItemDesc) div 2,
	packet_send(Socket, << 16#0a110300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		ItemID:32, Length:32/little, ItemDesc/binary, 0:16 >>).

%% @doc Quest init.
%% @todo When first entering a zone it seems LID should be set to ffff apparently.
send_0c00(CharUser, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	#users{area={QuestID, _ZoneID, _MapID}} = CharUser,
	packet_send(Socket, << 16#0c000300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, QuestID:32/little,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>).

%% @todo Figure out last 4 bytes!
%% @todo This packet hasn't been reviewed at all yet.
send_0c02(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#0c020300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, 0:32 >>).

%% @doc Send the huge pack of quest files available in the counter.
send_0c06(Pack, #client{socket=Socket}) ->
	packet_send(Socket, << 16#0c060300:32, 0:288, 1:32/little, Pack/binary >>).

%% @doc Reply that the player is allowed to use the lobby transport. Always allow.
send_0c08(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#0c080300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:96 >>).

%% @doc Send the trial start notification.
%% @todo This packet hasn't been reviewed at all yet.
send_0c09(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#0c090300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, 0:64 >>).

%% @doc Send the counter's mission options (0 = invisible, 2 = disabled, 3 = available).
send_0c10(Options, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	Size = byte_size(Options),
	packet_send(Socket, << 16#0c100300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 1, 0, Size:16/little, Options/binary >>).

%% @doc Send the general data and flags for the selected character.
%% @todo Handle bitflags and value flags properly.
send_0d01(Character, #client{socket=Socket, gid=DestGID}) ->
	CharBin = psu_characters:character_tuple_to_binary(Character),
	OptionsBin = psu_characters:options_tuple_to_binary(Character#characters.options),
	packet_send(Socket, << 16#0d010300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, CharBin/binary,
		16#ffbbef1c:32, 16#f8ff0700:32, 16#fc810916:32, 16#7802134c:32, 16#b0c0040f:32, 16#7cf0e583:32,
		16#b7bce0c6:32, 16#7ff8f963:32, 16#3fd7ffff:32, 16#fff7ffff:32, 16#f3ff63e0:32, 16#1fe00000:32,
		0:7744, OptionsBin/binary >>).

%% @doc Send the character list for selection.
%% @todo There's a few odd values blanked, also the last known location apparently.
%% @todo This packet hasn't been reviewed at all yet.
send_0d03(Data0, Data1, Data2, Data3, #client{socket=Socket, gid=DestGID}) ->
	[{status, Status0}, {char, Char0}|_] = Data0,
	[{status, Status1}, {char, Char1}|_] = Data1,
	[{status, Status2}, {char, Char2}|_] = Data2,
	[{status, Status3}, {char, Char3}|_] = Data3,
	packet_send(Socket, << 16#0d030300:32, 0:32, 16#00011300:32, DestGID:32/little, 0:64,
		16#00011300:32, DestGID:32/little, 0:104,
		Status0:8, 0:48, Char0/binary, 0:520,
		Status1:8, 0:48, Char1/binary, 0:520,
		Status2:8, 0:48, Char2/binary, 0:520,
		Status3:8, 0:48, Char3/binary, 0:512 >>).

%% @doc Send the flags list. This is the whole list of available values, not the character's.
%%      Sent without fragmentation on official for unknown reasons. Do the same here.
send_0d05(#client{socket=Socket, gid=DestGID}) ->
	{ok, Flags} = file:read_file("p/flags.bin"),
	Packet = << 16#0d050300:32, 0:32, 16#00011300:32, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64, Flags/binary >>,
	Size = 4 + byte_size(Packet),
	ssl:send(Socket, << Size:32/little, Packet/binary >>).

%% @doc Send the client's own player's party information, on the bottom left of the screen.
%% @todo Location and the 20 bytes following sometimes have values, not sure why; when joining a party maybe?
send_1005(Character, #client{socket=Socket, gid=DestGID}) ->
	#characters{name=Name, mainlevel=#level{number=Level}, currenthp=CurrentHP, maxhp=MaxHP} = Character,
	Location = << 0:512 >>,
	packet_send(Socket, << 16#10050300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
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
send_1006(EventID, PartyPos, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#10060300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, EventID:8, PartyPos:8, 0:16 >>).

%% @doc Send the player's current location.
%% @todo Handle PartyPos.
%% @todo Receive the AreaName as UCS2 directly to allow for color codes and the like.
%% @todo Handle TargetLID probably (right after the padding).
%% @todo Do counters even have a name?
send_100e(CounterID, AreaName, #client{socket=Socket, gid=DestGID}) ->
	PartyPos = 0,
	UCS2Name = << << X:8, 0:8 >> || X <- AreaName >>,
	Padding = 8 * (64 - byte_size(UCS2Name)),
	CounterType = if CounterID =:= 16#ffffffff -> 2; true -> 1 end,
	packet_send(Socket, << 16#100e0300:32, 16#ffffffbf:32, 0:128, 16#00011300:32, DestGID:32, 0:64,
		1, PartyPos, 0:48, 16#ffffff7f:32, UCS2Name/binary, 0:Padding, 0:32, CounterID:32/little, CounterType:32/little >>).
send_100e({QuestID, ZoneID, MapID}, EntryID, AreaName, #client{socket=Socket, gid=DestGID}) ->
	PartyPos = 0,
	UCS2Name = << << X:8, 0:8 >> || X <- AreaName >>,
	Padding = 8 * (64 - byte_size(UCS2Name)),
	packet_send(Socket, << 16#100e0300:32, 16#ffffffbf:32, 0:128, 16#00011300:32, DestGID:32, 0:64,
		1, PartyPos, ZoneID:16/little, MapID:16/little, EntryID:16/little, QuestID:32/little,
		UCS2Name/binary, 0:Padding, 0:32, 16#ffffffff:32, 0:32 >>).

%% @todo No idea. Also the 2 PartyPos in the built packet more often than not match, but sometimes don't? That's probably because one is PartyPos and the other is LID or something.
%% @todo This packet hasn't been reviewed at all yet.
send_100f(NPCid, PartyPos, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#100f0300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, NPCid:16/little, 1, PartyPos:8, PartyPos:32/little >>).

%% @doc Send the mission's quest file when starting a new mission.
%% @todo Handle correctly. 0:32 is actually a missing value. Value before that is unknown too.
%% @todo This packet hasn't been reviewed at all yet.
send_1015(QuestID, #client{socket=Socket, gid=DestGID}) ->
	QuestData = egs_quests_db:quest_nbl(QuestID),
	Size = byte_size(QuestData),
	packet_send(Socket, << 16#10150300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, QuestID:32/little, 16#01010000:32, 0:32, Size:32/little, QuestData/binary >>).

%% @todo No idea.
%% @todo This packet hasn't been reviewed at all yet.
send_1016(PartyPos, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#10160300:32, 16#ffff0000:32, 0:128, 16#00011300:32, DestGID:32/little, 0:64, PartyPos:32/little >>).

%% @todo No idea.
%% @todo This packet hasn't been reviewed at all yet.
send_101a(NPCid, PartyPos, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#101a0300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:64, NPCid:16/little, PartyPos:16/little, 16#ffffffff:32 >>).

%% @doc Mission start related.
send_1020(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#10200300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64 >>).

%% @doc Update HP in the party members information on the left.
%% @todo Handle PartyPos. Probably only pass HP later.
send_1022(#users{character=#characters{currenthp=HP}}, #client{socket=Socket, gid=DestGID}) ->
	PartyPos = 0,
	packet_send(Socket, << 16#10220300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, HP:32/little, PartyPos:32/little >>).

%% @todo Boss related command.
%% @todo This packet hasn't been reviewed at all yet.
send_110e(Data, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#110e0300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, Data/binary, 0:32, 5:16/little, 12:16/little, 0:32, 260:32/little >>).

%% @todo Boss related command.
%% @todo This packet hasn't been reviewed at all yet.
send_1113(Data, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#11130300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, Data/binary >>).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.
%% @todo This packet hasn't been reviewed at all yet.
send_1202(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#12020300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 0:32, 16#10000000:32, 0:64, 16#14000000:32, 0:32 >>).

%% @todo Always the same value, no idea what it's for.
send_1204(#client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#12040300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:96, 16#20000000:32, 0:256 >>).

%% @doc Object events response?
%% @todo Not sure what Value does exactly. It's either 0 or 1.
%% @todo This packet hasn't been reviewed at all yet.
send_1205(EventID, BlockID, Value, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#12050300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, EventID, BlockID, 0:16, Value, 0:24 >>).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.
%% @todo This packet hasn't been reviewed at all yet.
send_1206(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#12060300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 0:32, 16#80020000:32, 0:5120 >>).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.
%% @todo This packet hasn't been reviewed at all yet.
send_1207(#client{socket=Socket, gid=DestGID}) ->
	Chunk = << 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 0:224, 16#0000ffff:32, 16#ff000000:32, 16#64000a00:32 >>,
	packet_send(Socket, << 16#12070300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary >>).

%% @todo Object interaction? Figure out. C probably the interaction type.
%% @todo Apparently A would be TargetID/ffffffff, B would be the player LID, C would be the object type? D still completely unknown.
%% @todo This packet hasn't been reviewed at all yet.
send_1211(A, B, C, D, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#12110300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, A:32/little, B:32/little, C:32/little, D:32/little >>).

%% @doc Make the client load the quest previously sent.
%% @todo This packet hasn't been reviewed at all yet.
send_1212(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#12120300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 0:19200 >>).

%% @todo Not sure. Related to keys.
%% @todo This packet hasn't been reviewed at all yet.
send_1213(A, B, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#12130300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, A:32/little, B:32/little >>).

%% @todo Related to boss gates.
%% @todo This packet hasn't been reviewed at all yet.
send_1215(A, B, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#12150300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, A:32/little, 0:16, B:16/little >>).

%% @todo Not sure yet. Value is probably a TargetID. Used in Airboard Rally. Replying with the same value starts the race.
%% @todo This packet hasn't been reviewed at all yet.
send_1216(Value, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#12160300:32, 0:32, 16#00011300:32, DestGID:32/little, 0:64, 16#00011300:32, DestGID:32/little, 0:64, Value:32/little >>).

%% @doc Send the player's partner card.
%% @todo Handle the LID and comment properly.
send_1500(Character, #client{socket=Socket, gid=DestGID}) ->
	#characters{slot=Slot, name=Name, race=Race, gender=Gender, class=Class, appearance=Appearance} = Character,
	case Appearance of
		#flesh_appearance{voicetype=VoiceType, voicepitch=VoicePitch} -> ok;
		#metal_appearance{voicetype=VoiceType, voicepitch=VoicePitch} -> ok
	end,
	RaceBin = psu_characters:race_atom_to_binary(Race),
	GenderBin = psu_characters:gender_atom_to_binary(Gender),
	ClassBin = psu_characters:class_atom_to_binary(Class),
	Comment = << 0:2816 >>,
	packet_send(Socket, << 16#15000300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		Name/binary, RaceBin:8, GenderBin:8, ClassBin:8, VoiceType:8, VoicePitch:8, 0:24,
		DestGID:32/little, 0:224, Comment/binary, 1, 4, 1, Slot, 0:64 >>).

%% @todo Send an empty partner card list.
%% @todo This packet hasn't been reviewed at all yet.
send_1501(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#15010300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:96 >>).

%% @todo Send an empty blacklist.
%% @todo This packet hasn't been reviewed at all yet.
send_1512(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#15120300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:46144 >>).

%% @todo NPC related packet, sent when there's an NPC in the area.
%% @todo This packet hasn't been reviewed at all yet.
send_1601(PartyPos, #client{socket=Socket, gid=DestGID}) ->
	{ok, << _:32, Bin/bits >>} = file:read_file("p/packet1601.bin"),
	packet_send(Socket, << 16#16010300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, PartyPos:32/little, Bin/binary >>).

%% @doc Send the player's NPC and PM information.
%% @todo The value 4 is the card priority. Find what 3 is. When sending, the first 0 is an unknown value.
%% @todo This packet hasn't been reviewed at all yet.
send_1602(#client{socket=Socket, gid=DestGID}) ->
	NPCList = egs_npc_db:all(),
	NbNPC = length(NPCList),
	Bin = iolist_to_binary([<< NPCid:8, 0, 4, 0, 3, 0:24 >> || {NPCid, _Data} <- NPCList]),
	MiddlePaddingSize = 8 * (344 - byte_size(Bin)),
	PMName = "My PM",
	UCS2PMName = << << X:8, 0:8 >> || X <- PMName >>,
	EndPaddingSize = 8 * (64 - byte_size(UCS2PMName)),
	packet_send(Socket, << 16#16020300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:96,
		Bin/binary, 0:MiddlePaddingSize, NbNPC, 0:24, UCS2PMName/binary, 0:EndPaddingSize, 0:32 >>).

%% @doc Send the list of parties to join.
%% @todo Handle lists of parties.
%% @todo Probably has to handle a LID here, although it should always be 0.
send_1701(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#17010300:32, 0:160, 16#00011300:32, DestGID:32/little, 0:96 >>).

%% @doc Party information.
%% @todo Handle existing parties.
%% @todo This packet hasn't been reviewed at all yet.
send_1706(CharName, #client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#17060300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64,
		16#00000300:32, 16#d5c0faff:32, 0:64, CharName/binary,
		16#78000000:32, 16#01010000:32, 0:1536, 16#0100c800:32, 16#0601010a:32, 16#ffffffff:32, 0:32 >>).

%% @doc Party settings. Item distribution is random for now.
%% @todo Handle correctly.
%% @todo This packet hasn't been reviewed at all yet.
send_170a(#client{socket=Socket, gid=DestGID}) ->
	packet_send(Socket, << 16#170a0300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 16#01010c08:32 >>).

%% @todo Find what the heck this packet is.
%% @todo This packet hasn't been reviewed at all yet.
send_170c(#client{socket=Socket, gid=DestGID}) ->
	{ok, File} = file:read_file("p/packet170c.bin"),
	packet_send(Socket, << 16#170c0300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, File/binary >>).

%% @doc Send the background to use for the counter.
send_1711(Bg, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#17110300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:64, Bg:8, 0:24 >>).

%% @doc NPC shop request reply.
send_1a02(A, B, C, D, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#1a020300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:96,
		A:16/little, B:16/little, C:16/little, D:16/little >>).

%% @doc Lumilass available hairstyles/headtypes handler.
send_1a03(CharUser, #client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	{ok, Conf} = file:consult("priv/lumilass.conf"),
	Character = CharUser#users.character,
	NbHeadtypes = proplists:get_value({headtypes, Character#characters.gender, Character#characters.race}, Conf, 0),
	HairstylesList = proplists:get_value({hairstyles, Character#characters.gender}, Conf),
	NbHairstyles = length(HairstylesList),
	HairstylesBin = iolist_to_binary([ << N:32 >> || N <- HairstylesList]),
	packet_send(Socket, << 16#1a030300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:96,
		NbHairstyles:32/little, NbHeadtypes:32/little, 0:416, HairstylesBin/binary, 0:32 >>).

%% @doc PP cube handler.
%% @todo The 4 bytes before the file may vary. Everything past that is the same. Figure things out.
%% @todo This packet hasn't been reviewed at all yet.
send_1a04(#client{socket=Socket, gid=DestGID}) ->
	{ok, File} = file:read_file("p/ppcube.bin"),
	packet_send(Socket, << 16#1a040300:32, 16#ffff:16, 0:144, 16#00011300:32, DestGID:32/little, 0:64, 0:32, File/binary >>).

%% @doc Available types handler. Enable all 16 types.
send_1a07(#client{socket=Socket, gid=DestGID, lid=DestLID}) ->
	packet_send(Socket, << 16#1a070300:32, DestLID:16/little, 0:144, 16#00011300:32, DestGID:32/little, 0:160,
		16#01010101:32, 16#01010101:32, 16#01010101:32, 16#01010101:32 >>).

%% Utility functions.

%% @doc Return the language as an atom from its integer value.
%% @todo Identify which of the english languages is american and which is uk.
language_integer_to_atom(0) -> japanese;
language_integer_to_atom(1) -> american_english;
language_integer_to_atom(2) -> british_english;
language_integer_to_atom(3) -> french;
language_integer_to_atom(4) -> german;
language_integer_to_atom(5) -> spanish;
language_integer_to_atom(6) -> italian;
language_integer_to_atom(7) -> korean;
language_integer_to_atom(8) -> simplified_chinese;
language_integer_to_atom(9) -> traditional_chinese;
language_integer_to_atom(Language) -> log("unknown 080e Language ~p", [Language]).

%% @doc Prepare a packet. Return the real size and padding at the end.
packet_prepare(Packet) ->
	Size = 4 + byte_size(Packet),
	case Size rem 4 of
		0 -> {ok, Size, <<>>};
		2 -> {ok, Size + 2, << 0:16 >>};
		_ -> {error, badarg}
	end.

%% @doc Send a packet. The packet argument must not contain the size field.
packet_send(CSocket, Packet) ->
	{ok, Size, Padding} = packet_prepare(Packet),
	packet_send(CSocket, << Size:32/little, Packet/binary, Padding/binary >>, Size).

%% @doc Send a normal command.
packet_send(CSocket, Packet, Size) when Size =< 16#4000 ->
	ssl:send(CSocket, Packet);

%% @doc Send a fragmented command when size is too big.
packet_send(CSocket, Packet, Size) ->
	packet_fragment_send(CSocket, Packet, Size, 0).

%% @doc Send the last chunk of a fragmented command.
packet_fragment_send(CSocket, Packet, Size, Current) when Size - Current =< 16#4000 ->
	FragmentSize = 16#10 + byte_size(Packet),
	Fragment = << FragmentSize:32/little, 16#0b030000:32, Size:32/little, Current:32/little, Packet/binary >>,
	ssl:send(CSocket, Fragment);

%% @doc Send another chunk of a fragmented command.
packet_fragment_send(CSocket, Packet, Size, Current) ->
	<< Chunk:131072/bits, Rest/bits >> = Packet,
	Fragment = << 16#10400000:32, 16#0b030000:32, Size:32/little, Current:32/little, Chunk/binary >>,
	ssl:send(CSocket, Fragment),
	packet_fragment_send(CSocket, Rest, Size, Current + 16#4000).

%% @doc Keepalive. Just send an empty packet, the game doesn't really care.
%% @todo If there's an actual keepalive command, use it instead.
send_keepalive(CSocket) ->
	Packet = << 0:32 >>,
	packet_send(CSocket, Packet).

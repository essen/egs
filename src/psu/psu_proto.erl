%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Independent implementation of the PSU protocol.
%%
%%	This file is part of EGS.
%%
%%	EGS is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU General Public License as published by
%%	the Free Software Foundation, either version 3 of the License, or
%%	(at your option) any later version.
%%
%%	EGS is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU General Public License for more details.
%%
%%	You should have received a copy of the GNU General Public License
%%	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

-module(psu_proto).
-compile(export_all).

%~ %% @todo We probably want to use active connections everywhere instead of doing this.
%~ recv %% remove later?

%~ %% @todo We probably want to remove this after all send functions are moved back in psu_proto.
%~ send %% fragments automatically if needed

%~ split

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
-define(ASSERT(), log("assert error in module ~p on line ~p~n", [?MODULE, ?LINE])).

%% @spec assert(A, B) -> ok
%% @doc Log a detailed message when the assertion A =:= B fails.
-define(ASSERT_EQ(A, B), if A =:= B -> ok; true -> log("assert error in module ~p on line ~p~n", [?MODULE, ?LINE]) end).

%% @spec parse(Packet) -> Result
%% @doc Parse the packet and return a result accordingly.
parse(<< Size:32/little, Command:16, Channel:8, _Unknown:8, Data/bits >>) ->
	parse(Size, Command, Channel, Data).

%% @todo One of the missing events is probably learning a new PA.
parse(Size, 16#0105, Channel, Data) ->
	<<	_LID:16/little, _VarB:16/little, VarC:32/little, _FromGID:32/little, VarD:32/little, VarE:32/little, TypeID:32/little, GID:32/little,
		VarF:32/little, VarG:32/little, TargetGID:32/little, TargetLID:32/little, ItemID:8, EventID:8, _PAID:8, VarH:8, VarI:32/little, Rest/bits >> = Data,
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
		7 -> ?ASSERT(), ignore; %% @todo
		8 -> ignore; %% @todo item_use;
		9 -> ?ASSERT(), ignore; %% @todo
		18 -> ignore; %% @todo item_unlearn_pa;
		_ -> log("unknown 0105 EventID ~p", [EventID])
	end,
	case Event of
		item_drop ->
			?ASSERT_EQ(Size, 76),
			<< _Quantity:32/little, _PosX:32/little-float, _PosY:32/little-float, _PosZ:32/little-float >> = Rest,
			%~ {Event, ItemID, Quantity, ...};
			ignore;
		ignore ->
			?ASSERT_EQ(Size, 60),
			ignore;
		_ ->
			?ASSERT_EQ(Size, 60),
			{Event, ItemID, TargetGID, TargetLID, VarH, VarI}
	end;

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

parse(Size, 16#0807, Channel, Data) ->
	<<	_LID:16/little, VarA:16/little, VarB:32/little, VarC:32/little, VarD:32/little, VarE:32/little, VarF:32/little, VarG:32/little, VarH:32/little, VarI:32/little,
		QuestID:32/little, ZoneID:16/little, MapID:16/little, EntryID:16/little, _AreaChangeNb:16/little, VarJ:32/little >> = Data,
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
	?ASSERT_EQ(VarJ, 16#ffffffff),
	{area_change, QuestID, ZoneID, MapID, EntryID};

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

parse(_Size, Command, Channel, Data) ->
	%% @todo log unknown command?
	%~ ignore.
	<< _:288, Rest/bits >> = Data,
	{command, Command, Channel, Rest}.



%% @doc Prepare a packet. Return the real size and padding at the end.

packet_prepare(Packet) ->
	Size = 4 + byte_size(Packet),
	case Size rem 4 of
		0 ->
			{ok, Size, <<>>};
		2 ->
			{ok, Size + 2, << 0:16 >>};
		_ ->
			{error, badarg}
	end.

%% @doc Receive exactly one packet command. Handle errors properly. Return the full packet for the command.

packet_recv(CSocket, Timeout) ->
	case packet_safe_recv(CSocket, 4, Timeout) of
		{error, A} ->
			{error, A};
		{ok, << Size:32/little-unsigned-integer >>} ->
			case packet_safe_recv(CSocket, Size - 4, Timeout) of
				{error, B} ->
					{error, B};
				{ok, Tail} ->
					{ok, << Size:32/little-unsigned-integer, Tail/binary >>}
			end
	end.

%% @doc Safely receive a packet. Close the connection if an error happens.

packet_safe_recv(CSocket, Size, Timeout) ->
	try ssl:recv(CSocket, Size, Timeout) of
		{ok, Packet} ->
			{ok, Packet};
		{error, timeout} ->
			{error, timeout};
		{error, _} ->
			ssl:close(CSocket),
			{error, closed}
	catch
		_ ->
			ssl:close(CSocket),
			{error, closed}
	end.

%% @doc Send a packet. The packet argument must not contain the size field.

packet_send(CSocket, Packet) ->
	{ok, Size, Padding} = packet_prepare(Packet),
	packet_send(CSocket, << Size:32/little-unsigned-integer, Packet/binary, Padding/binary >>, Size).

%% @doc Send a normal command.

packet_send(CSocket, Packet, Size) when Size =< 16#4000 ->
	ssl:send(CSocket, Packet);

%% @doc Send a fragmented command when size is too big.
%% @todo Wait for fragments reception confirmation?

packet_send(CSocket, Packet, Size) ->
	packet_fragment_send(CSocket, Packet, Size, 0).

%% @doc Send the last chunk of a fragmented command.

packet_fragment_send(CSocket, Packet, Size, Current) when Size - Current =< 16#4000 ->
	FragmentSize = 16#10 + byte_size(Packet),
	Fragment = << FragmentSize:32/little-unsigned-integer, 16#0b030000:32/unsigned-integer,
		Size:32/little-unsigned-integer, Current:32/little-unsigned-integer, Packet/binary >>,
	ssl:send(CSocket, Fragment);

%% @doc Send another chunk of a fragmented command.

packet_fragment_send(CSocket, Packet, Size, Current) ->
	<< Chunk:131072/bits, Rest/bits >> = Packet,
	Fragment = << 16#10400000:32/unsigned-integer, 16#0b030000:32/unsigned-integer,
		Size:32/little-unsigned-integer, Current:32/little-unsigned-integer, Chunk/binary >>,
	ssl:send(CSocket, Fragment),
	packet_fragment_send(CSocket, Rest, Size, Current + 16#4000).

%% @doc Split a packet received into commands. This is only needed when receiving packets in active mode.

packet_split(Packet) ->
	packet_split(Packet, []).

packet_split(Packet, Result) ->
	<< Size:32/little-unsigned-integer, _/bits >> = Packet,
	case Size > byte_size(Packet) of
		true ->
			{Result, Packet};
		false ->
			BitSize = Size * 8,
			<< Split:BitSize/bits, Rest/bits >> = Packet,
			case Rest of
				<< >> ->
					{Result ++ [Split], << >>};
				_ ->
					packet_split(Rest, Result ++ [Split])
			end
	end.

%% @doc Parse the packet header returns the header information along with the data chunk.
%%      0b05 is handled differently because it's only 16 bytes long and use a different format.

packet_parse(<< _:32, 16#0b05:16, _/bits >>) ->
	{command, 16#0b05, ignore, ignore};

packet_parse(Orig) ->
	<< _:32, Command:16/unsigned-integer, Channel:8, _:296, Data/bits >> = Orig,
	{command, Command, Channel, Data}.

%% @doc Shortcut for send_global/4.

send_global(CSocket, Type, Message) ->
	send_global(CSocket, Type, Message, 2).

%% @doc Send a global message.
%%      There are four types of global messages: dialog, top, scroll and timeout.
%%      * dialog: A dialog in the center of the screen, which can be OK'd by players.
%%      * top: Horizontal scroll on top of the screen, traditionally used for server-wide messages.
%%      * scroll: Vertical scroll on the right of the screen, traditionally used for Player X joined the party.
%%      * timeout: A dialog in the center of the screen that disappears after Duration seconds.

send_global(CSocket, Type, Message, Duration) ->
	TypeID = case Type of
		dialog -> 0;
		top -> 1;
		scroll -> 2;
		timeout -> 3;
		_ -> 1
	end,
	UCS2Message = << << X:8, 0:8 >> || X <- Message >>,
	try
		Packet = << 16#02280300:32, 0:288, TypeID:32/little-unsigned-integer, Duration:32/little-unsigned-integer, UCS2Message/binary, 0, 0 >>,
		packet_send(CSocket, Packet)
	catch
		_:_ ->
			ignore
	end.

%% @doc Keepalive. Just send an empty packet, the game doesn't really care.
%% @todo If there's an actual keepalive command, use it instead.

send_keepalive(CSocket) ->
	Packet = << 0:32 >>,
	packet_send(CSocket, Packet).

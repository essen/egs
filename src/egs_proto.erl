%	EGS: Erlang Game Server
%	Copyright (C) 2010  Loic Hoguin
%
%	This file is part of EGS.
%
%	EGS is free software: you can redistribute it and/or modify
%	it under the terms of the GNU General Public License as published by
%	the Free Software Foundation, either version 3 of the License, or
%	(at your option) any later version.
%
%	EGS is distributed in the hope that it will be useful,
%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%	GNU General Public License for more details.
%
%	You should have received a copy of the GNU General Public License
%	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

-module(egs_proto).
-compile(export_all).

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

%% @doc Parse a chat command. AOTI v2.000 version of the command.

parse_chat(0, Packet) ->
	<< _:384, FromGID:32/unsigned-integer, Modifiers:128/bits, Message/bits >> = Packet,
	[{gid, FromGID}, {name, missing}, {modifiers, Modifiers}, {message, Message}];

%% @doc Parse a chat command. AOTI since an unknown version of the game.

parse_chat(_, Packet) ->
	<< _:384, FromGID:32/unsigned-integer, Modifiers:128/bits, FromName:512/bits, Message/bits >> = Packet,
	[{gid, FromGID}, {name, FromName}, {modifiers, Modifiers}, {message, Message}].

%% @doc Parse the game server auth command. Used when first connecting to a game server.

parse_game_auth(Packet) ->
	<< _:352, GID:32/little-unsigned-integer, Auth:32/bits, _/bits >> = Packet,
	[{gid, GID}, {auth, Auth}].

%% @doc Parse a lobby change command.

parse_lobby_change(Packet) ->
	<< _:352, Quest:32/little-unsigned-integer, MapType:16/little-unsigned-integer,
		MapNumber:16/little-unsigned-integer, MapEntry:16/little-unsigned-integer, _/bits >> = Packet,
	[{quest, Quest}, {maptype, MapType}, {mapnumber, MapNumber}, {mapentry, MapEntry}].

%% @doc Parse the options change command. Retrieve the options for saving.

parse_options_change(Packet) ->
	<< _:352, Options/bits >> = Packet,
	[{options, Options}].

%% @doc Center the camera on the player, if possible.
%% @todo Probably.

send_camera_center(CSocket, GID) ->
	Packet = << 16#0236:16, 0:208, GID:32/little-unsigned-integer, 0:64 >>,
	packet_send(CSocket, Packet).

%% @doc Send the character list for selection.

send_character_list(CSocket, GID, Data0, Data1, Data2, Data3) ->
	[{status, Status0}, {char, Char0}|_] = Data0,
	[{status, Status1}, {char, Char1}|_] = Data1,
	[{status, Status2}, {char, Char2}|_] = Data2,
	[{status, Status3}, {char, Char3}|_] = Data3,
	Packet = << 16#0d03:16/unsigned-integer, 0:80, GID:32/little-unsigned-integer, 0:96, GID:32/little-unsigned-integer, 0:104,
		Status0:8/unsigned-integer, 0:48, Char0/binary, 0:520,
		Status1:8/unsigned-integer, 0:48, Char1/binary, 0:520,
		Status2:8/unsigned-integer, 0:48, Char2/binary, 0:520,
		Status3:8/unsigned-integer, 0:48, Char3/binary, 0:512 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the data for the selected character.

send_character_selected(CSocket, GID, Char, Options) ->
	Packet = << 16#0d01:16, 0:208, GID:32/little-unsigned-integer, 0:64, Char/binary, 0:8128, Options/binary >>,
	packet_send(CSocket, Packet).

%% @doc Send a chat command. AOTI v2.000 version of the command.

send_chat(CSocket, 0, FromGID, _, Modifiers, Message) ->
	Packet = << 16#0304:16/unsigned-integer, 0:320, 16#1200:16/unsigned-integer, FromGID:32/little-unsigned-integer, Modifiers:128/bits, Message/bits >>,
	packet_send(CSocket, Packet);

%% @doc Send a chat command. AOTI since an unknown version of the game.

send_chat(CSocket, _, FromGID, FromName, Modifiers, Message) ->
	Packet = << 16#0304:16, 0:320, 16#1200:16/unsigned-integer, FromGID:32/little-unsigned-integer, Modifiers:128/bits, FromName:512/bits, Message/bits >>,
	packet_send(CSocket, Packet).

%% @doc Send the character flags list.
%%      Sent without fragmentation on official for unknown reasons. Do the same here.

send_flags(CSocket, GID) ->
	{ok, Flags} = file:read_file("p/flags.bin"),
	Packet = << 16#0d050300:32, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Flags/binary >>,
	Size = 4 + byte_size(Packet),
	ssl:send(CSocket, << Size:32/little-unsigned-integer, Packet/binary >>).

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
		Packet = << 16#0228:16, 0:304, TypeID:32/little-unsigned-integer, Duration:32/little-unsigned-integer, UCS2Message/binary, 0, 0 >>,
		packet_send(CSocket, Packet)
	catch
		_:_ ->
			ignore
	end.

%% @doc Init quest.

send_init_quest(CSocket, GID, QuestID) ->
	Packet = << 16#0c000300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, QuestID:32/little-unsigned-integer,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>,
	packet_send(CSocket, Packet).

%% @doc Keepalive.

send_keepalive(CSocket, GID) ->
	Packet = << 16#2b1b:16, 0:208, GID:32/little-unsigned-integer, 0:64 >>,
	packet_send(CSocket, Packet).

%% @doc Make the client load the quest previously sent.

send_load_quest(CSocket, GID) ->
	Packet = << 16#1212:16, 0:208, GID:32/little-unsigned-integer, 0:64 >>,
	packet_send(CSocket, Packet).

%% @doc Indicate to the client that loading should finish.

send_loading_end(CSocket, GID) ->
	Packet = << 16#0208:16, 0:208, GID:32/little-unsigned-integer, 0:96 >>,
	packet_send(CSocket, Packet).

%% @doc Send the player's current location.
%% @todo Figure out what the last value is. No counter without it.

send_location(CSocket, GID, Quest, MapType, MapNumber, Location) ->
	UCS2Location = << << X:8, 0:8 >> || X <- Location >>,
	Packet = << 16#100e0300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		1:32/little-unsigned-integer, MapNumber:16/little-unsigned-integer, MapType:16/little-unsigned-integer,
		Quest:32/little-unsigned-integer, UCS2Location/binary >>,
	PaddingSize = (128 - byte_size(Packet) - 4) * 8,
	packet_send(CSocket, << Packet/binary, 0:PaddingSize, 1:32/little-unsigned-integer >>).

%% @doc Send the map ID to be loaded by the client.

send_map(CSocket, Quest, MapType, MapNumber, MapEntry) ->
	Packet = << 16#0205:16, 0:304, Quest:32/little-unsigned-integer, MapType:32/little-unsigned-integer,
		MapNumber:32/little-unsigned-integer, MapEntry:32/little-unsigned-integer, 0:64 >>,
	packet_send(CSocket, Packet).

%% @doc Send the player's NPC and PM information.

send_npc_info(CSocket, GID) ->
	{ok, File} = file:read_file("p/npc.bin"),
	Packet = << 16#16020300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96, File/binary >>,
	packet_send(CSocket, Packet).

%% @doc Send the player's partner card.

send_player_card(CSocket, GID, Char) ->
	<< CharInfo:576/bits, _/bits >> = Char,
	Packet = << 16#1500:16, 0:208, GID:32/little-unsigned-integer, 0:64, CharInfo/binary, 0:3072, 16#01040103:32, 0:64 >>,
	packet_send(CSocket, Packet).

%% @doc Send the quest file to be loaded.

send_quest(CSocket, Filename) ->
	{ok, File} = file:read_file(Filename),
	Size = byte_size(File),
	Packet = << 16#020e:16, 0:304, Size:32/little-unsigned-integer, 0:32, File/binary >>,
	packet_send(CSocket, Packet).

%% @doc Send the trial start notification.

send_trial_start(CSocket, GID) ->
	Packet = << 16#0c090300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:128 >>,
	packet_send(CSocket, Packet).

%% @doc Send the list of available universes.

send_universe_cube(CSocket) ->
	{ok, File} = file:read_file("p/unicube.bin"),
	Packet = << 16#021e:16, 0:304, File/binary >>,
	packet_send(CSocket, Packet).

%% @doc Send the current universe name and number.
%% @todo Currently only have universe number 2, named EGS Test.

send_universe_info(CSocket, GID) ->
	Packet = << 16#0222:16, 0:80, GID:32/little-unsigned-integer, 0:96, GID:32/little-unsigned-integer, 0:64,
		2:32/little-unsigned-integer, 0:32, 16#45, 0, 16#47, 0, 16#53, 0, 16#20, 0, 16#54, 0, 16#65, 0, 16#73, 0, 16#74, 0:24 >>,
	packet_send(CSocket, Packet).

%% @doc Send the zone file to be loaded.

send_zone(CSocket, Filename) ->
	{ok, File} = file:read_file(Filename),
	Size = byte_size(File),
	Packet = << 16#020f:16, 0:336, Size:32/little-unsigned-integer, File/binary >>,
	packet_send(CSocket, Packet).

%% @doc Send the zone initialization notification.

send_zone_init(CSocket, GID, ZoneType) ->
	case ZoneType of
		mission ->
			Var = << 16#06000500:32, 16#01000000:32, 0:64, 16#00040000:32, 16#00010000:32, 16#00140000:32 >>;
		_ ->
			Var = << 16#00040000:32, 0:160, 16#00140000:32 >>
	end,
	Packet = << 16#02000300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96, 16#01000000:32,
		16#ffffffff:32, Var/binary, 16#ffffffff:32, 16#ffffffff:32 >>,
	egs_proto:packet_send(CSocket, Packet).

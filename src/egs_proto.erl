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

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
%	gasetools is distributed in the hope that it will be useful,
%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%	GNU General Public License for more details.
%
%	You should have received a copy of the GNU General Public License
%	along with gasetools.  If not, see <http://www.gnu.org/licenses/>.

-module(egs_patch).
-export([start/0]). % external
-export([listen/1, accept/1, process/1]). % internal

-include("include/network.hrl").

%% @doc Start the patch server. Currently supports AOTI US and JP.

start() ->
	JPPid = spawn_link(?MODULE, listen, [?PATCH_PORT_JP]),
	USPid = spawn_link(?MODULE, listen, [?PATCH_PORT_US]),
	[{jp, JPPid}, {us, USPid}].

%% @doc Listen for connections.

listen(Port) ->
	{ok, LSocket} = gen_tcp:listen(Port, ?PATCH_LISTEN_OPTIONS),
	?MODULE:accept(LSocket).

%% @doc Accept connections.

accept(LSocket) ->
	case gen_tcp:accept(LSocket, 5000) of
		{ok, CSocket} ->
			spawn_link(?MODULE, process, [CSocket]);
		{error, timeout} ->
			reload
	end,
	?MODULE:accept(LSocket).

%% @doc Fake the patch server by sending what the game wants to hear: no updates available.
%%      Ignore all the return values.

process(CSocket) ->
	io:format("faking patch server: no updates~n"),
	_ = send_packet(CSocket, "p/patch-0.bin"),
	_ = gen_tcp:recv(CSocket, 0, 5000),
	_ = send_packet(CSocket, "p/patch-1.bin"),
	_ = send_packet(CSocket, "p/patch-2.bin"),
	_ = gen_tcp:recv(CSocket, 0, 5000),
	_ = send_packet(CSocket, "p/patch-3.bin"),
	_ = send_packet(CSocket, "p/patch-4.bin"),
	_ = gen_tcp:close(CSocket).

%% @doc Send a packet from a file.

send_packet(CSocket, PacketFilename) ->
	{ok, Packet} = file:read_file(PacketFilename),
	gen_tcp:send(CSocket, Packet).

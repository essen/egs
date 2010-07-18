%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Process patch requests.
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

-module(psu_patch).
-export([start_link/1]). %% External.
-export([listen/1, accept/1, process/1]). %% Internal

-define(OPTIONS, [binary, {send_timeout, 5000}, {packet, 0}, {active, false}, {reuseaddr, true}]).

%% @spec start_link(Port) -> {ok,Pid::pid()}
%% @doc Start the PSU patch server for inclusion in a supervisor tree.
start_link(Port) ->
	Pid = spawn(?MODULE, listen, [Port]),
	{ok, Pid}.

%% @spec listen(Port) -> ok
%% @doc Listen for connections.
listen(Port) ->
	{ok, LSocket} = gen_tcp:listen(Port, ?OPTIONS),
	?MODULE:accept(LSocket).

%% @spec accept(LSocket) -> ok
%% @doc Accept connections.
accept(LSocket) ->
	case gen_tcp:accept(LSocket, 5000) of
		{ok, CSocket} ->
			spawn(?MODULE, process, [CSocket]);
		{error, timeout} ->
			reload
	end,
	?MODULE:accept(LSocket).

%% @spec process(CSocket) -> ok
%% @doc Fake the patch server by sending what the game wants to hear: no updates available.
process(CSocket) ->
	io:format("faking patch server: no updates~n"),
	send_packet(CSocket, "priv/psu_patch/patch-0.bin"),
	gen_tcp:recv(CSocket, 0, 5000),
	send_packet(CSocket, "priv/psu_patch/patch-1.bin"),
	send_packet(CSocket, "priv/psu_patch/patch-2.bin"),
	gen_tcp:recv(CSocket, 0, 5000),
	send_packet(CSocket, "priv/psu_patch/patch-3.bin"),
	send_packet(CSocket, "priv/psu_patch/patch-4.bin"),
	gen_tcp:close(CSocket).

%% @spec send_packet(CSocket, PacketFilename) -> ok
%% @doc Send a packet from a file.
send_packet(CSocket, PacketFilename) ->
	{ok, Packet} = file:read_file(PacketFilename),
	gen_tcp:send(CSocket, Packet).

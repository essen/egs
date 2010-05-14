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

-module(egs_login).
-export([start/0]). % external
-export([listen/2, accept/2, process/2, loop/2]). % internal

-include("include/records.hrl").
-include("include/network.hrl").

%% @doc Start the login server. Currently AOTI JP and US only.

start() ->
	JPPidE1 = spawn_link(?MODULE, listen, [?LOGIN_PORT_JP_ONE, 10000001]),
	JPPidE2 = spawn_link(?MODULE, listen, [?LOGIN_PORT_JP_TWO, 20000001]),
	USPid = spawn_link(?MODULE, listen, [?LOGIN_PORT_US, 30000001]),
	[{jp_e1, JPPidE1}, {jp_e2, JPPidE2}, {us, USPid}].

%% @doc Listen for connections.

listen(Port, SessionID) ->
	{ok, LSocket} = ssl:listen(Port, ?LOGIN_LISTEN_OPTIONS),
	?MODULE:accept(LSocket, SessionID).

%% @doc Accept connections.

accept(LSocket, SessionID) ->
	case ssl:transport_accept(LSocket, 5000) of
		{ok, CSocket} ->
			% in the future, modulo to avoid conflicts future with real GIDs
			%~ NextID = (SessionID + 1) rem 1000000,
			NextID = SessionID + 1,
			ssl:ssl_accept(CSocket),
			spawn_link(?MODULE, process, [CSocket, SessionID]);
		{error, timeout} ->
			NextID = SessionID,
			reload
	end,
	?MODULE:accept(LSocket, NextID).

%% @doc Process the new connections. Send an hello packet and start the loop.

process(CSocket, SessionID) ->
	log(SessionID, "hello"),
	egs_proto:send_hello(CSocket, SessionID),
	?MODULE:loop(CSocket, SessionID).

%% @doc Main loop for the login server.

loop(CSocket, SessionID) ->
	case egs_proto:packet_recv(CSocket, 5000) of
		{ok, Packet} ->
			<< _:32, Command:16/unsigned-integer, _/bits >> = Packet,
			case handle(Command, CSocket, SessionID, Packet) of
				ok ->
					?MODULE:loop(CSocket, SessionID);
				closed ->
					ignore
			end;
		{error, timeout} ->
			reload,
			?MODULE:loop(CSocket, SessionID);
		{error, closed} ->
			log(SessionID, "recv error, closing"),
			egs_db:users_delete(SessionID)
	end.

%% @doc Game server info request handler.

handle(16#0217, CSocket, SessionID, _) ->
	log(SessionID, "send game server info"),
	egs_proto:send_game_server_info(CSocket, SessionID, ?GAME_PORT),
	ssl:close(CSocket),
	closed;

%% @doc Authentication request handler. Currently always succeed.
%%      Use the temporary session ID as the GID for now.
%%      Use username and password as a folder name for saving character data.
%% @todo Handle real GIDs whenever there's real authentication.

handle(16#0219, CSocket, SessionID, Packet) ->
	[{username, Username}, {password, Password}] = egs_proto:parse_auth_request(Packet),
	log(SessionID, io_lib:format("auth success for ~s ~s", [Username, Password])),
	Auth = crypto:rand_bytes(4),
	Folder = << Username/binary, "-", Password/binary >>,
	log(SessionID, Folder),
	egs_db:users_insert(#users{gid=SessionID, pid=self(), socket=CSocket, auth=Auth, folder=Folder}),
	egs_proto:send_auth_success(CSocket, SessionID, SessionID, Auth);

%% @doc MOTD request handler. Handles both forms of MOTD requests, US and JP.
%%      Currently ignore the language and send the same MOTD file to everyone.

handle(Command, CSocket, SessionID, Packet) when Command =:= 16#0226; Command =:= 16#023f ->
	[{page, Page}, {language, _}] = egs_proto:parse_motd_request(Packet),
	log(SessionID, io_lib:format("send MOTD page ~.10b", [Page + 1])),
	egs_proto:send_motd(CSocket, Page);

%% @doc Unknown command handler. Do nothing.

handle(Command, _, SessionID, _) ->
	log(SessionID, io_lib:format("(login) dismissed packet ~4.16.0b", [Command])).

%% @doc Log message to the console.

log(SessionID, Message) ->
	io:format("login (~.10b): ~s~n", [SessionID, Message]).

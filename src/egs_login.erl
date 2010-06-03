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
	process_flag(trap_exit, true),
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
		_ ->
			NextID = SessionID,
			reload
	end,
	?MODULE:accept(LSocket, NextID).

%% @doc Process the new connections. Send an hello packet and start the loop.

process(CSocket, SessionID) ->
	log(SessionID, "hello"),
	egs_proto:packet_send(CSocket, << 16#02020300:32, 0:288, SessionID:32/little-unsigned-integer >>),
	?MODULE:loop(CSocket, SessionID).

%% @doc Main loop for the login server.

loop(CSocket, SessionID) ->
	case egs_proto:packet_recv(CSocket, 5000) of
		{ok, Packet} ->
			<< _:32, Command:16/unsigned-integer, _/bits >> = Packet,
			case handle(Command, CSocket, SessionID, Packet) of
				closed ->
					ignore;
				_ ->
					?MODULE:loop(CSocket, SessionID)
			end;
		{error, timeout} ->
			reload,
			?MODULE:loop(CSocket, SessionID);
		{error, closed} ->
			log(SessionID, "quit"),
			egs_db:users_delete(SessionID)
	end.

%% @doc Game server info request handler.

handle(16#0217, CSocket, SessionID, _) ->
	log(SessionID, "forward to game server"),
	IP = ?GAME_IP,
	Port = ?GAME_PORT,
	Packet = << 16#02160300:32, 0:192, SessionID:32/little-unsigned-integer, 0:64, IP/binary, Port:32/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet),
	ssl:close(CSocket),
	closed;

%% @doc Authentication request handler. Currently always succeed.
%%      Use the temporary session ID as the GID for now.
%%      Use username and password as a folder name for saving character data.
%% @todo Handle real GIDs whenever there's real authentication. GID is the second SessionID in the reply.
%% @todo Apparently it's possible to ask a question in the reply here. Used for free course on JP.

handle(16#0219, CSocket, SessionID, Orig) ->
	<< _:352, UsernameBlob:192/bits, PasswordBlob:192/bits, _/bits >> = Orig,
	Username = re:replace(UsernameBlob, "\\0", "", [global, {return, binary}]),
	Password = re:replace(PasswordBlob, "\\0", "", [global, {return, binary}]),
	log(SessionID, "auth success for ~s ~s", [Username, Password]),
	Auth = crypto:rand_bytes(4),
	Folder = << Username/binary, "-", Password/binary >>,
	Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	egs_db:users_insert(#users{gid=SessionID, pid=self(), socket=CSocket, auth=Auth, time=Time, folder=Folder}),
	Packet = << 16#02230300:32, 0:192, SessionID:32/little-unsigned-integer, 0:64, SessionID:32/little-unsigned-integer, Auth:32/bits >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc MOTD request handler. Handles both forms of MOTD requests, US and JP. Page number starts at 0.
%% @todo Currently ignore the language and send the same MOTD file to everyone. Language is 8 bits next to Page.
%% @todo Use a normal ASCII file rather than an UCS2 one?

handle(Command, CSocket, SessionID, Orig) when Command =:= 16#0226; Command =:= 16#023f ->
	<< _:352, Page:8/little-unsigned-integer, _/bits >> = Orig,
	log(SessionID, "send MOTD page ~.10b", [Page + 1]),
	{ok, File} = file:read_file("conf/motd.txt"),
	Tokens = re:split(File, "\n."),
	MOTD = << << Line/binary, "\n", 0 >> || Line <- lists:sublist(Tokens, 1 + Page * 15, 15) >>,
	NbPages = 1 + length(Tokens) div 15,
	Packet = << 16#0225:16, 0:304, NbPages:8, Page:8, 16#8200:16/unsigned-integer, MOTD/binary, 0:16 >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc Silently ignore packets 0227 and 080e.

handle(Command, _, _, _) when Command =:= 16#0227; Command =:= 16#080e ->
	ignore;

%% @doc Unknown command handler. Print a log message about it.

handle(Command, _, SessionID, _) ->
	log(SessionID, "dismissed packet ~4.16.0b", [Command]).

%% @doc Log message to the console.

log(SessionID, Message) ->
	io:format("login (~.10b): ~s~n", [SessionID, Message]).

log(SessionID, Message, Format) ->
	RealMessage = io_lib:format(Message, Format),
	log(SessionID, RealMessage).

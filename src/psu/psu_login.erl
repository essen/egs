%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Process login requests.
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

-module(psu_login).
-export([start_link/2]). %% External.
-export([listen/2, accept/2, process/2, loop/2]). %% Internal.

-include("include/records.hrl").
-include("include/network.hrl").

-define(OPTIONS, [binary, {active, false}, {reuseaddr, true}, {certfile, "priv/ssl/servercert.pem"}, {keyfile, "priv/ssl/serverkey.pem"}, {password, "alpha"}]).

%% @spec start_link(Port) -> {ok,Pid::pid()}
%% @doc Start the PSU login server for inclusion in a supervisor tree.
start_link(Port, SessionID) ->
	Pid = spawn(?MODULE, listen, [Port, SessionID]),
	{ok, Pid}.

%% @spec listen(Port, SessionID) -> ok
%% @doc Listen for connections.
listen(Port, SessionID) ->
	error_logger:info_report(io_lib:format("psu_login listening on port ~b, with a sessionid of ~b", [Port, SessionID])),
	{ok, LSocket} = ssl:listen(Port, ?OPTIONS),
	?MODULE:accept(LSocket, SessionID).

%% @spec accept(LSocket, SessionID) -> ok
%% @doc Accept connections.
accept(LSocket, SessionID) ->
	case ssl:transport_accept(LSocket, 5000) of
		{ok, CSocket} ->
			%% in the future, modulo to avoid conflicts future with real GIDs
			%% NextID = (SessionID + 1) rem 1000000,
			NextID = SessionID + 1,
			ssl:ssl_accept(CSocket),
			spawn(?MODULE, process, [CSocket, SessionID]);
		{error, _Reason} ->
			NextID = SessionID,
			reload
	end,
	?MODULE:accept(LSocket, NextID).

%% @spec process(CSocket, SessionID) -> ok
%% @doc Process the new connections. Send an hello packet and start the loop.
process(CSocket, SessionID) ->
	log(SessionID, "hello"),
	psu_proto:send_0202(CSocket, SessionID),
	?MODULE:loop(CSocket, SessionID).

%% @spec loop(CSocket, SessionID) -> ok
%% @doc Main loop for the login server.
loop(CSocket, SessionID) ->
	case psu_proto:packet_recv(CSocket, 5000) of
		{ok, Orig} ->
			<< _:32, Command:16/unsigned-integer, _/bits >> = Orig,
			case handle(Command, CSocket, SessionID, Orig) of
				closed -> ok;
				_ -> ?MODULE:loop(CSocket, SessionID)
			end;
		{error, timeout} ->
			reload,
			?MODULE:loop(CSocket, SessionID);
		{error, closed} ->
			log(SessionID, "quit"),
			egs_user_model:delete(SessionID)
	end.

%% @spec handle(Command, CSocket, SessionID, Orig) -> ok | closed
%% @doc Login server client commands handler.

%% Game server info request handler.
%% @todo Remove the dependency on network.hrl
handle(16#0217, CSocket, SessionID, _) ->
	log(SessionID, "forward to game server"),
	IP = ?GAME_IP,
	Port = ?GAME_PORT,
	Packet = << 16#02160300:32, 0:192, SessionID:32/little-unsigned-integer, 0:64, IP/binary, Port:32/little-unsigned-integer >>,
	psu_proto:packet_send(CSocket, Packet),
	ssl:close(CSocket),
	closed;

%% Authentication request handler. Currently always succeed.
%% Use the temporary session ID as the GID for now.
%% Use username and password as a folder name for saving character data.
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
	egs_user_model:write(#egs_user_model{id=SessionID, pid=self(), socket=CSocket, state={wait_for_authentication, Auth}, time=Time, folder=Folder}),
	Packet = << 16#02230300:32, 0:192, SessionID:32/little-unsigned-integer, 0:64, SessionID:32/little-unsigned-integer, Auth:32/bits >>,
	psu_proto:packet_send(CSocket, Packet);

%% MOTD request handler. Handles both forms of MOTD requests, US and JP. Page number starts at 0.
%% @todo Currently ignore the language and send the same MOTD file to everyone. Language is 8 bits next to Page.
%% @todo Use a normal ASCII file rather than an UCS2 one?
handle(Command, CSocket, SessionID, Orig) when Command =:= 16#0226; Command =:= 16#023f ->
	<< _:352, Page:8/little-unsigned-integer, _/bits >> = Orig,
	log(SessionID, "send MOTD page ~.10b", [Page + 1]),
	{ok, File} = file:read_file("priv/psu_login/motd.txt"),
	Tokens = re:split(File, "\n."),
	MOTD = << << Line/binary, "\n", 0 >> || Line <- lists:sublist(Tokens, 1 + Page * 15, 15) >>,
	NbPages = 1 + length(Tokens) div 15,
	Packet = << 16#0225:16, 0:304, NbPages:8, Page:8, 16#8200:16/unsigned-integer, MOTD/binary, 0:16 >>,
	psu_proto:packet_send(CSocket, Packet);

%% @doc Platform information handler. Reject versions < 2.0009.2.
handle(16#080e, CSocket, SessionID, << _:352, _:64, Revision:8, Minor:4, _:12, Major:4, _:4, _/bits >>) ->
	Version = Major * 1000000 + Minor * 1000 + Revision,
	if	Version < 2009002 ->
			log(SessionID, "reject outdated version ~b.~b.~b", [Major, Minor, Revision]),
			Website = << "http://psumods.co.uk/forums/comments.php?DiscussionID=40#Item_1" >>,
			Size = byte_size(Website),
			WebsiteLen = Size + 1,
			PaddingSize = 8 * (512 - Size),
			WebsitePacket = << 16#02310300:32, 16#ffff0000:32, 16#00000f00:32, SessionID:32/little, 0:64, 16#00000f00:32, SessionID:32/little, 0:64,
				WebsiteLen:32/little, Website/binary, 0:PaddingSize >>,
			psu_proto:packet_send(CSocket, WebsitePacket),
			{ok, File} = file:read_file("priv/psu_login/error_version.txt"),
			ErrorLen = byte_size(File) div 2 + 2,
			ErrorPacket = << 16#02230300:32, 0:160, 16#00000f00:32, SessionID:32/little, 0:96, 16#f9dbce73:32, 3:32/little, 0:48, ErrorLen:16/little, File/binary, 0:16 >>,
			psu_proto:packet_send(CSocket, ErrorPacket),
			closed;
		true ->
			ignore
	end;

%% Silently ignore command 0227.
handle(16#0227, _, _, _) ->
	ignore;

%% Unknown command handler. Print a log message about it.
handle(Command, _, SessionID, _) ->
	log(SessionID, "dismissed packet ~4.16.0b", [Command]).

%% @spec log(SessionID, Message) -> ok
%% @doc Log message to the console.
log(SessionID, Message) ->
	io:format("login (~.10b): ~s~n", [SessionID, Message]).

%% @spec log(SessionID, Message, Format) -> ok
%% @doc Format a message and log it to the console.
log(SessionID, Message, Format) ->
	RealMessage = io_lib:format(Message, Format),
	log(SessionID, RealMessage).

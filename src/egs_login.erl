%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Log in and authentication callback module.
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

-module(egs_login).
-export([keepalive/1, info/2, cast/3, raw/3, event/2]).

%% @todo This header is only included because of egs_user_model. We don't want that here.
-include("include/records.hrl").
-include("include/network.hrl").

-record(state, {socket, gid}).

%% @doc Don't keep alive here, authentication should go fast.
keepalive(_State) ->
	ok.

%% @doc We don't expect any message here.
%% @todo Throw an error instead?
info(_Msg, _State) ->
	ok.

%% @doc Nothing to broadcast.
%% @todo Throw an error instead?
cast(_Command, _Data, _State) ->
	ok.

%% Raw commands.
%% @todo Move all of them to events.

%% @doc MOTD request handler. Handles both forms of MOTD requests, US and JP. Page number starts at 0.
%% @todo Currently ignore the language and send the same MOTD file to everyone. Language is 8 bits next to Page.
raw(Command, << _:352, Page:8, _/bits >>, #state{socket=Socket}) when Command =:= 16#0226; Command =:= 16#023f ->
	{ok, File} = file:read_file("priv/psu_login/motd.txt"),
	Tokens = re:split(File, "\n."),
	MOTD = << << Line/binary, "\n", 0 >> || Line <- lists:sublist(Tokens, 1 + Page * 15, 15) >>,
	NbPages = 1 + length(Tokens) div 15,
	Packet = << 16#0225:16, 0:304, NbPages:8, Page:8, 16#8200:16/unsigned-integer, MOTD/binary, 0:16 >>,
	psu_proto:packet_send(Socket, Packet);

%% @doc Dismiss all raw commands with a log notice.
%% @todo Have a log event handler instead.
raw(Command, _Data, _State) ->
	io:format("~p: dismissed command ~4.16.0b~n", [?MODULE, Command]).

%% Events.

%% @doc Reject version < 2.0009.2.
%% @todo Reject wrong platforms too.
event({system_client_version_info, _Language, _Platform, Version}, #state{socket=Socket, gid=GID}) ->
	if Version >= 2009002 -> ignore; true ->
		Website = << "http://psumods.co.uk/forums/comments.php?DiscussionID=40#Item_1" >>,
		Size = byte_size(Website),
		WebsiteLen = Size + 1,
		PaddingSize = 8 * (512 - Size),
		WebsitePacket = << 16#02310300:32, 16#ffff0000:32, 16#00000f00:32, GID:32/little, 0:64, 16#00000f00:32, GID:32/little, 0:64,
			WebsiteLen:32/little, Website/binary, 0:PaddingSize >>,
		psu_proto:packet_send(Socket, WebsitePacket),
		{ok, File} = file:read_file("priv/psu_login/error_version.txt"),
		ErrorLen = byte_size(File) div 2 + 2,
		ErrorPacket = << 16#02230300:32, 0:160, 16#00000f00:32, GID:32/little, 0:96, 16#f9dbce73:32, 3:32/little, 0:48, ErrorLen:16/little, File/binary, 0:16 >>,
		psu_proto:packet_send(Socket, ErrorPacket),
		ssl:close(Socket),
		closed
	end;

%% Game server info request handler.
%% @todo Remove the dependency on network.hrl through configuration files.
event(system_game_server_request, #state{socket=Socket, gid=GID}) ->
	IP = ?GAME_IP,
	Port = ?GAME_PORT,
	Packet = << 16#02160300:32, 0:192, GID:32/little-unsigned-integer, 0:64, IP/binary, Port:32/little-unsigned-integer >>,
	psu_proto:packet_send(Socket, Packet),
	ssl:close(Socket),
	closed;

%% @doc Authenticate the user by pattern matching its saved state against the key received.
%%      If the user is authenticated, send him the character flags list.
%% @todo Remove the put calls when all the send_xxxx are moved out of psu_game and into psu_proto.
event({system_key_auth_request, AuthGID, AuthKey}, #state{socket=Socket}) ->
	{ok, User} = egs_user_model:read(AuthGID),
	{wait_for_authentication, AuthKey} = User#egs_user_model.state,
	put(socket, Socket),
	put(gid, AuthGID),
	LID = 1 + mnesia:dirty_update_counter(counters, lobby, 1) rem 1023,
	Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	User2 = User#egs_user_model{id=AuthGID, pid=self(), socket=Socket, state=authenticated, time=Time, lid=LID},
	egs_user_model:write(User2),
	psu_proto:send_0d05(User2),
	{ok, egs_char_select, {state, Socket, AuthGID}};

%% @doc Authentication request handler. Currently always succeed.
%%      Use the temporary session ID as the GID for now.
%%      Use username and password as a folder name for saving character data.
%% @todo Handle real GIDs whenever there's real authentication. GID is the second SessionID in the reply.
%% @todo Apparently it's possible to ask a question in the reply here. Used for free course on JP.
event({system_login_auth_request, Username, Password}, #state{socket=Socket}) ->
	io:format("auth success for ~s ~s~n", [Username, Password]),
	RealGID = 10000000 + mnesia:dirty_update_counter(counters, gid, 1),
	Auth = crypto:rand_bytes(4),
	Folder = << Username/binary, "-", Password/binary >>,
	Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	egs_user_model:write(#egs_user_model{id=RealGID, pid=self(), socket=Socket, state={wait_for_authentication, Auth}, time=Time, folder=Folder}),
	Packet = << 16#02230300:32, 0:192, RealGID:32/little-unsigned-integer, 0:64, RealGID:32/little-unsigned-integer, Auth:32/bits >>,
	psu_proto:packet_send(Socket, Packet).

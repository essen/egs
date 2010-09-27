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

-include("include/records.hrl").
-include("include/network.hrl").

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

%% @doc Dismiss all raw commands with a log notice.
%% @todo Have a log event handler instead.
raw(Command, _Data, _State) ->
	io:format("~p: dismissed command ~4.16.0b~n", [?MODULE, Command]).

%% Events.

%% @doc Reject version < 2.0009.2.
%% @todo Reject wrong platforms too.
%% @todo f9dbce73 is an auth key too.
event({system_client_version_info, _Entrance, _Language, _Platform, Version}, State=#state{socket=Socket}) ->
	if Version >= 2009002 -> ignore; true ->
		psu_proto:send_0231("http://psumods.co.uk/forums/comments.php?DiscussionID=40#Item_1", State),
		{ok, ErrorMsg} = file:read_file("priv/psu_login/error_version.txt"),
		psu_proto:send_0223(ErrorMsg, State),
		ssl:close(Socket),
		closed
	end;

%% Game server info request handler.
%% @todo Remove the dependency on network.hrl through configuration files.
event(system_game_server_request, State=#state{socket=Socket}) ->
	psu_proto:send_0216(?GAME_IP, ?GAME_PORT, State),
	ssl:close(Socket),
	closed;

%% @doc Authenticate the user by pattern matching its saved state against the key received.
%%      If the user is authenticated, send him the character flags list.
%% @todo Remove the put calls when all the send_xxxx are moved out of psu_game and into psu_proto.
event({system_key_auth_request, AuthGID, AuthKey}, State=#state{socket=Socket}) ->
	egs_user_model:key_auth(AuthGID, AuthKey, Socket),
	put(socket, Socket),
	put(gid, AuthGID),
	State2 = State#state{gid=AuthGID},
	psu_proto:send_0d05(State2),
	{ok, egs_char_select, State2};

%% @doc Authentication request handler. Currently always succeed.
%%      Use the temporary session ID as the GID for now.
%%      Use username and password as a folder name for saving character data.
%% @todo Handle real GIDs whenever there's real authentication. GID is the second SessionID in the reply.
%% @todo Apparently it's possible to ask a question in the reply here. Used for free course on JP.
event({system_login_auth_request, Username, Password}, State=#state{socket=Socket}) ->
	io:format("auth success for ~s ~s~n", [Username, Password]),
	AuthGID = 10000000 + mnesia:dirty_update_counter(counters, gid, 1),
	AuthKey = crypto:rand_bytes(4),
	Folder = << Username/binary, "-", Password/binary >>,
	Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	egs_user_model:write(#egs_user_model{id=AuthGID, pid=self(), socket=Socket, state={wait_for_authentication, AuthKey}, time=Time, folder=Folder}),
	psu_proto:send_0223(AuthGID, AuthKey, State);

%% @doc MOTD request handler. Page number starts at 0.
%% @todo Currently ignore the language and send the same MOTD file to everyone.
event({system_motd_request, Page, _Language}, State) ->
	{ok, MOTD} = file:read_file("priv/psu_login/motd.txt"),
	psu_proto:send_0225(MOTD, Page, State).

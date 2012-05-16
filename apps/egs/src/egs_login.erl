%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
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
-export([info/2, cast/3, event/2]).

-include("include/records.hrl").

%% @doc We don't expect any message here.
info(_Msg, _Client) ->
	ok.

%% @doc Nothing to broadcast.
cast(_Command, _Data, _Client) ->
	ok.

%% Events.

%% @doc Reject version < 2.0009.2.
%% @todo Reject wrong platforms too.
%% @todo Put the URL in a configuration file.
event({client_version, _Entrance, _Language, _Platform, Version}, Client)
		when Version < 2009002 ->
	egs_net:system_open_url(<<"http://psumods.co.uk/forums/comments.php?DiscussionID=40#Item_1">>, Client),
	{ok, Error} = file:read_file("priv/login/error_version.txt"),
	egs_net:system_auth_error(Error, Client),
	egs_net:terminate(Client),
	closed;
event({client_version, _Entrance, _Language, _Platform, _Version}, _Client) ->
	ok;

%% @doc Game server info request handler.
event(system_game_server_request, Client) ->
	{ServerIP, ServerPort} = egs_conf:read(game_server),
	egs_net:system_game_server_response(ServerIP, ServerPort, Client),
	egs_net:terminate(Client),
	closed;

%% @doc Authenticate the user by pattern matching its saved state against the key received.
%%      If the user is authenticated, send him the character flags list.
event({system_key_auth, AuthGID, AuthKey}, Client) ->
	egs_accounts:key_auth(AuthGID, AuthKey),
	Client2 = egs_net:set_gid(AuthGID, Client),
	ValueFlags = egs_conf:read(value_flags),
	BoolFlags = egs_conf:read(bool_flags),
	TempFlags = egs_conf:read(temp_flags),
	egs_net:account_flags(ValueFlags, BoolFlags, TempFlags, Client2),
	Client3 = egs_net:set_handler(egs_char_select, Client2),
	Client4 = egs_net:set_keepalive(Client3),
	{ok, Client4};

%% @doc Authentication request handler. Currently always succeed.
%% @todo Handle real GIDs whenever there's real authentication. GID is the second SessionID in the reply.
%% @todo Apparently it's possible to ask a question in the reply here. Used for free course on JP.
event({system_login_auth, Username, Password}, Client) ->
	{ok, AuthGID} = egs_accounts:login_auth(Username, Password),
	{ok, AuthKey} = egs_accounts:key_auth_init(AuthGID),
	io:format("auth success for ~s ~s~n", [Username, Password]),
	egs_net:system_key_auth_info(AuthGID, AuthKey, Client);

%% @doc MOTD request handler. Page number starts at 0.
%% @todo Currently ignore the language and send the same MOTD file to everyone.
event({system_motd_request, Page, _Language}, Client) ->
	{ok, MOTD} = file:read_file("priv/login/motd.txt"),
	egs_net:system_motd_response(MOTD, Page, Client).

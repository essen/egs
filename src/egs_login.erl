%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Game server's client authentication callback module.
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
-export([init/1, keepalive/1, info/2, cast/3, raw/3, event/2]).

%% @todo This header is only included because of egs_user_model. We don't want that here.
-include("include/records.hrl").

-record(state, {socket}).

%% @doc Initialize the game state and start receiving messages.
%% @todo Link against egs_exit_mon.
%% @todo Handle keepalive messages globally.
%% @todo Probably have init where start_link is.
init(Socket) ->
	psu_proto:send_0202(Socket, 0),
	timer:send_interval(5000, {egs, keepalive}),
	egs_network:recv(<< >>, ?MODULE, #state{socket=Socket}).

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

%% @doc Dismiss all raw commands with a log notice.
%% @todo Have a log event handler instead.
raw(Command, _Data, _State) ->
	io:format("~p: dismissed command ~4.16.0b~n", [?MODULE, Command]).

%% Events.

%% @todo Check the client version info here too. Not just on login.
event({system_client_version_info, _Language, _Platform, _Version}, _State) ->
	ok;

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
	{ok, egs_char_select, {state, Socket, AuthGID}}.

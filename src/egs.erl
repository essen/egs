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

-module(egs).
-compile(export_all).

-include("include/records.hrl").

-define(MODULES, [egs, egs_cron, egs_db, egs_game, egs_login, egs_patch, egs_proto]).

%% @doc Start all the application servers. Return the PIDs of the listening processes.

start() ->
	application:start(crypto),
	application:start(ssl),
	ssl:seed(crypto:rand_bytes(256)),
	egs_db:create(),
	Cron = egs_cron:start(),
	Game = egs_game:start(),
	Login = egs_login:start(),
	Patch = egs_patch:start(),
	[{patch, Patch}, {login, Login}, {game, Game}, {cron, Cron}].

%% @doc Reload all the modules.

reload() ->
	[code:soft_purge(Module) || Module <- ?MODULES],
	[code:load_file(Module) || Module <- ?MODULES].

%% @doc Send a global message.

global(Type, Message) ->
	lists:foreach(fun(User) -> egs_proto:send_global(User#users.socket, Type, Message) end, egs_db:users_select_all()).

%% @doc Warp all players to a new map.

warp(Quest, MapType, MapNumber, MapEntry) ->
	lists:foreach(fun(User) -> egs_game:lobby_load(User#users.socket, User#users.gid, Quest, MapType, MapNumber, MapEntry) end, egs_db:users_select_all()).

%% @doc Warp one player to a new map.

warp(GID, Quest, MapType, MapNumber, MapEntry) ->
	User = egs_db:users_select(GID),
	egs_game:lobby_load(User#users.socket, User#users.gid, Quest, MapType, MapNumber, MapEntry).

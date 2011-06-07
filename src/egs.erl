%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc EGS startup code and utility functions.
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

-module(egs).
-export([start/0, stop/0, global/1, warp/4, warp/5]). %% API.

%% Player and account-related types.

-type gid() :: 0..16#ffffffff.
-type lid() :: 0..1023 | 16#ffff.
-type character_slot() :: 0..3.
-export_type([gid/0, lid/0, character_slot/0]).

%% Location related types.

-type questid() :: 0..16#ffffffff. %% @todo What's the real max?
-type zoneid() :: 0..16#ffff. %% @todo What's the real max?
-type mapid() :: 0..9999.
-type entryid() :: 0..16#ffff. %% @todo What's the real max?
-type area() :: {questid(), zoneid(), mapid()}. %% @todo Probably remove later.
-type position() :: {X::float(), Y::float(), Z::float(), Dir::float()}.
-export_type([questid/0, zoneid/0, mapid/0, entryid/0, area/0, position/0]).

%% API.

-spec start() -> ok.
start() ->
	ensure_started(crypto),
	ensure_started(public_key),
	ensure_started(ssl),
	ssl:seed(crypto:rand_bytes(256)),
	ensure_started(cowboy),
	application:start(egs).

-spec stop() -> ok.
stop() ->
	Res = application:stop(egs),
	ok = application:stop(cowboy),
	ok = application:stop(ssl),
	ok = application:stop(public_key),
	ok = application:stop(crypto),
	Res.

%% @doc Send a global message.
-spec global(string()) -> ok.
global(Message) when length(Message) > 511 ->
	io:format("global: message too long~n");
global(Message) ->
	egs_users:broadcast_all({egs, notice, top, Message}).

%% @doc Warp all players to a new map.
-spec warp(questid(), zoneid(), mapid(), entryid()) -> ok.
warp(QuestID, ZoneID, MapID, EntryID) ->
	egs_users:broadcast_all({egs, warp, QuestID, ZoneID, MapID, EntryID}).

%% @doc Warp one player to a new map.
-spec warp(gid(), questid(), zoneid(), mapid(), entryid()) -> ok.
warp(GID, QuestID, ZoneID, MapID, EntryID) ->
	egs_users:broadcast({egs, warp, QuestID, ZoneID, MapID, EntryID}, [GID]).

%% Internal.

-spec ensure_started(module()) -> ok.
ensure_started(App) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end.

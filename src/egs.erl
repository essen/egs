%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc EGS startup code.
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
-export([start/0, stop/0, global/1, warp/4, warp/5]).

%% @spec ensure_started(App) -> ok
%% @doc Make sure the given App is started.
ensure_started(App) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end.

%% @spec start() -> ok
%% @doc Start the EGS server.
start() ->
	ensure_started(crypto),
	ensure_started(public_key),
	ensure_started(ssl),
	ssl:seed(crypto:rand_bytes(256)),
	application:start(egs).

%% @spec stop() -> ok
%% @doc Stop the EGS server.
stop() ->
	Res = application:stop(egs),
	application:stop(ssl),
	application:stop(public_key),
	application:stop(crypto),
	Res.

%% @doc Send a global message.
global(Message) ->
	if	length(Message) > 511 ->
			io:format("global: message too long~n");
		true ->
			egs_users:broadcast_all({egs, notice, top, Message})
	end.

%% @doc Warp all players to a new map.
warp(QuestID, ZoneID, MapID, EntryID) ->
	egs_users:broadcast_all({egs, warp, QuestID, ZoneID, MapID, EntryID}).

%% @doc Warp one player to a new map.
warp(GID, QuestID, ZoneID, MapID, EntryID) ->
	egs_users:broadcast({egs, warp, QuestID, ZoneID, MapID, EntryID}, [GID]).

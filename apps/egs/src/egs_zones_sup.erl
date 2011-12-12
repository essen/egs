%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2011 Loïc Hoguin.
%% @doc Supervisor for the egs_zones gen_server.
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

-module(egs_zones_sup).
-behaviour(supervisor).

-export([start_link/0, start_zone/4]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

-spec start_zone(egs:uniid(), egs:questid(), egs:zoneid(), tuple())
	-> {ok, pid()}.
start_zone(UniID, QuestID, ZoneID, ZoneData) ->
	supervisor:start_child(?SUPERVISOR, [UniID, QuestID, ZoneID, ZoneData]).

%% supervisor.

-spec init([]) -> {ok, {{simple_one_for_one, 0, 1}, [{egs_zones,
	{egs_zones, start_link, []}, temporary, brutal_kill,
	worker, [egs_zones]}]}}.
init([]) ->
	{ok, {{simple_one_for_one, 0, 1}, [{egs_zones,
		{egs_zones, start_link, []}, temporary, brutal_kill,
		worker, [egs_zones]}]}}.

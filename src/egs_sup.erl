%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc Supervisor for the egs application.
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

-module(egs_sup).
-behaviour(supervisor).

-export([init/1]). %% Supervisor callbacks.
-export([start_link/0]). %% Other functions.

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
	Procs = [
		{egs_conf, {egs_conf, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_quests_sup, {egs_quests_sup, start_link, []}, permanent, 5000, supervisor, [egs_quests_sup]},
		{egs_zones_sup, {egs_zones_sup, start_link, []}, permanent, 5000, supervisor, [egs_zones_sup]},
		{egs_accounts, {egs_accounts, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_users, {egs_users, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_seasons, {egs_seasons, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_counters_db, {egs_counters_db, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_items_db, {egs_items_db, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_npc_db, {egs_npc_db, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_patch_files_db, {egs_patch_files_db, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_quests_db, {egs_quests_db, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_shops_db, {egs_shops_db, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_universes, {egs_universes, start_link, []}, permanent, 5000, worker, dynamic}
	],
	{ok, {{one_for_one, 10, 10}, Procs}}.

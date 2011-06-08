%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc Top-level supervisor for the egs application.
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

-export([start_link/0]). %% API.
-export([init/1]). %% Supervisor.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{one_for_one, 10, 10}, [{_, _, _, _, _, _}, ...]}}.
init([]) ->
	Procs = procs([egs_conf, {sup, egs_quests_sup}, {sup, egs_zones_sup},
		egs_accounts, egs_users, egs_seasons, egs_counters_db, egs_items_db,
		egs_npc_db, egs_patch_files_db, egs_quests_db, egs_shops_db,
		egs_universes], []),
	{ok, {{one_for_one, 10, 10}, Procs}}.

%% Internal.

-spec procs([module()|{sup, module()}], [{_, _, _, _, _, _}])
	-> [{_, _, _, _, _, _}].
procs([], Acc) ->
	lists:reverse(Acc);
procs([{sup, Module}|Tail], Acc) ->
	procs(Tail, [sup(Module)|Acc]);
procs([Module|Tail], Acc) ->
	procs(Tail, [worker(Module)|Acc]).

-spec worker(M) -> {M, {M, start_link, []}, permanent, 5000, worker, dynamic}.
worker(Module) ->
	{Module, {Module, start_link, []}, permanent, 5000, worker, dynamic}.

-spec sup(M) -> {M, {M, start_link, []}, permanent, 5000, supervisor, [M]}.
sup(Module) ->
	{Module, {Module, start_link, []}, permanent, 5000, supervisor, [Module]}.

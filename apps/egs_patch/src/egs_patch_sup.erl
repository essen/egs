%%	Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

-module(egs_patch_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% Supervisor.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{one_for_one, 10, 10}, [supervisor:child_spec(), ...]}}.
init([]) ->
	Procs = [{egs_patch_files_db, {egs_patch_files_db, start_link, []},
		permanent, 5000, worker, [egs_patch_files_db]}],
	{ok, {{one_for_one, 10, 10}, Procs}}.

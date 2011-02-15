%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2011 Loïc Hoguin.
%% @doc Supervisor for the patch, login and game listener processes.
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

-module(egs_servers_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
	PatchPorts = egs_conf:read(patch_ports),
	LoginPorts = egs_conf:read(login_ports),
	{_ServerIP, GamePort} = egs_conf:read(game_server),
	PatchProcs = [{{egs_patch_server, Port}, {egs_patch_server, start_link, [Port]}, permanent, 5000, worker, dynamic} || Port <- PatchPorts],
	LoginProcs = [{{egs_login_server, Port}, {egs_login_server, start_link, [Port]}, permanent, 5000, worker, dynamic} || Port <- LoginPorts],
	Procs = lists:flatten([PatchProcs, LoginProcs, {egs_game_server, {egs_game_server, start_link, [GamePort]}, permanent, 5000, worker, dynamic}]),
	{ok, {{one_for_one, 10, 10}, Procs}}.

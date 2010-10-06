%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
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
-export([start_link/0, upgrade/0]). %% Other functions.

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
	{ok, {_, Specs}} = init([]),
	Old = sets:from_list(
		[Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
	New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
	Kill = sets:subtract(Old, New),
	sets:fold(fun (Id, ok) ->
		supervisor:terminate_child(?MODULE, Id),
		supervisor:delete_child(?MODULE, Id),
		ok
	end, ok, Kill),
	[supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
	ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
%% @todo Probably link egs_conf to a supervisor too.
init([]) ->
	egs_conf:start_link(),
	PatchPorts = egs_conf:read(patch_ports),
	LoginPorts = egs_conf:read(login_ports),
	{_ServerIP, GamePort} = egs_conf:read(game_server),
	PatchProcs = [{{egs_patch_server, Port}, {psu_patch, start_link, [Port]}, permanent, 5000, worker, dynamic} || Port <- PatchPorts],
	LoginProcs = [{{egs_login_server, Port}, {egs_login_server, start_link, [Port]}, permanent, 5000, worker, dynamic} || Port <- LoginPorts],
	OtherProcs = [
		{egs_user_model, {egs_user_model, start_link, []}, permanent, 5000, worker, dynamic},
		{egs_game_server, {egs_game_server, start_link, [GamePort]}, permanent, 5000, worker, dynamic}
	],
	{ok, {{one_for_one, 10, 10}, PatchProcs ++ LoginProcs ++ OtherProcs}}.

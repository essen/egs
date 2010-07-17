%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Supervisor for the egs application.
%%
%%	This file is part of EGS.
%%
%%	EGS is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU General Public License as published by
%%	the Free Software Foundation, either version 3 of the License, or
%%	(at your option) any later version.
%%
%%	EGS is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU General Public License for more details.
%%
%%	You should have received a copy of the GNU General Public License
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
init([]) ->
	%% Start egs_cron, egs_game, egs_login, egs_patch. To be replaced by configurable modules.
	Processes = [{egs_cron, {egs_cron, start, []}, permanent, 5000, worker, dynamic},
				 {egs_game, {egs_game, start, []}, permanent, 5000, worker, dynamic},
				 {egs_login, {egs_login, start, []}, permanent, 5000, worker, dynamic},
				 {egs_patch, {egs_patch, start, []}, permanent, 5000, worker, dynamic}],
	{ok, {{one_for_one, 10, 10}, Processes}}.

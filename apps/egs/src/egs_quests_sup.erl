%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2011 Loïc Hoguin.
%% @doc Supervisor for the egs_quests gen_server.
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

-module(egs_quests_sup).
-behaviour(supervisor).

-export([start_link/0, start_quest/2]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

-spec start_quest(egs:uniid(), egs:questid()) -> {ok, pid()}.
start_quest(UniID, QuestID) ->
	supervisor:start_child(?SUPERVISOR, [UniID, QuestID]).

%% supervisor.

-spec init([]) -> {ok, {{simple_one_for_one, 0, 1}, [{egs_quests,
	{egs_quests, start_link, []}, temporary, brutal_kill,
	worker, [egs_quests]}]}}.
init([]) ->
	{ok, {{simple_one_for_one, 0, 1}, [{egs_quests,
		{egs_quests, start_link, []}, temporary, brutal_kill,
		worker, [egs_quests]}]}}.

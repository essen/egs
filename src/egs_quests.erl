%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2011 Loïc Hoguin.
%% @doc Quest handler.
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

-module(egs_quests).
-behaviour(gen_server).

-export([start_link/2, stop/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-record(state, {zones}).

-define(SERVER, ?MODULE).

%% API.

%% @spec start_link(UniID, QuestID) -> {ok,Pid::pid()}
start_link(UniID, QuestID) ->
	gen_server:start_link(?MODULE, [UniID, QuestID], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% gen_server.

init([UniID, QuestID]) ->
	Zones = egs_quests_db:quest_zones(QuestID),
	ZonesPids = lists:map(fun({ZoneID, _Params}) ->
		{ok, Pid} = supervisor:start_child(egs_zones_sup, {{zone, UniID, QuestID, ZoneID}, {egs_zones, start_link, [UniID, QuestID, ZoneID]}, permanent, 5000, worker, dynamic}),
		{ZoneID, Pid}
	end, Zones),
	{ok, #state{zones=ZonesPids}}.

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

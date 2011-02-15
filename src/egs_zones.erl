%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2011 Loïc Hoguin.
%% @doc Zone handler.
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

-module(egs_zones).
-behaviour(gen_server).

-export([start_link/4, stop/1, setid/1]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-record(state, {
	setid = 0 :: integer()
}).

%% API.

%% @spec start_link(UniID, QuestID, ZoneID, ZoneData) -> {ok,Pid::pid()}
start_link(UniID, QuestID, ZoneID, ZoneData) ->
	gen_server:start_link(?MODULE, [UniID, QuestID, ZoneID, ZoneData], []).

%% @spec stop(Pid) -> stopped
stop(Pid) ->
	gen_server:call(Pid, stop).

setid(Pid) ->
	gen_server:call(Pid, setid).

%% gen_server.

init([UniID, QuestID, ZoneID, ZoneData]) ->
	SetID = rand_setid(proplists:get_value(sets, ZoneData, [100])),
	{ok, #state{setid=SetID}}.

handle_call(setid, _From, State) ->
	{reply, State#state.setid, State};

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

%% Internal.

%% @doc Return a random setid from a list of chances per set.
rand_setid(Sets) ->
	N = crypto:rand_uniform(1, lists:sum(Sets)),
	rand_setid(N, Sets, 0).
rand_setid(N, [Set|_Tail], I) when N < Set ->
	I;
rand_setid(N, [Set|Tail], I) ->
	rand_setid(N - Set, Tail, I + 1).

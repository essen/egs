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

-export([start_link/4, stop/1, setid/1, enter/2, leave/2]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-record(state, {
	setid = 0		:: integer(),
	objects = []	:: list(),
	indexes = []	:: list(),
	targets = []	:: list(),
	players = []	:: list(),
	freelids = []	:: list()
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

enter(Pid, GID) ->
	gen_server:call(Pid, {enter, GID}).

leave(Pid, GID) ->
	gen_server:cast(Pid, {leave, GID}).

%% gen_server.

init([UniID, QuestID, ZoneID, ZoneData]) ->
	SetID = rand_setid(proplists:get_value(sets, ZoneData, [100])),
	Set = egs_quests_db:set(QuestID, ZoneID, SetID),
	Objects = create_units(Set),
	{Indexes, Targets} = index_objects(Objects),
	FreeLIDs = lists:seq(0, 1023),
	{ok, #state{setid=SetID, objects=Objects, indexes=Indexes, targets=Targets, freelids=FreeLIDs}}.

handle_call(setid, _From, State) ->
	{reply, State#state.setid, State};

handle_call({enter, GID}, _From, State) ->
	Players = State#state.players,
	[LID|FreeLIDs] = State#state.freelids,
	%% @todo Broadcast spawn to other players in the zone.
	{reply, LID, State#state{players=[{GID, LID}|Players], freelids=FreeLIDs}};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({leave, GID}, State) ->
	{_, LID} = lists:keyfind(GID, 1, State#state.players),
	Players = lists:delete({GID, LID}, State#state.players),
	FreeLIDs = State#state.freelids,
	%% @todo Broadcast unspawn to other players in the zone.
	{noreply, State#state{players=Players, freelids=[LID|FreeLIDs]}};

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

%% @doc Create the objects for all units in a set.
create_units(Set) ->
	create_units(Set, 0, []).
create_units([], _MapNb, Acc) ->
	lists:flatten(lists:reverse(Acc));
create_units([{{map, _MapID}, Groups}|Tail], MapNb, Acc) ->
	MapObjects = create_groups(Groups, MapNb),
	create_units(Tail, MapNb + 1, [MapObjects|Acc]).

%% @doc Create the objects for all groups in a unit.
create_groups(Groups, MapNb) ->
	create_groups(Groups, MapNb, 0, []).
create_groups([], _MapNb, _GroupNb, Acc) ->
	lists:flatten(lists:reverse(Acc));
create_groups([Objects|Tail], MapNb, GroupNb, Acc) ->
	GroupObjects = create_objects(Objects, MapNb, GroupNb),
	create_groups(Tail, MapNb, GroupNb + 1, [GroupObjects|Acc]).

%% @doc Create the given objects.
create_objects(Objects, MapNb, GroupNb) ->
	create_objects(Objects, MapNb, GroupNb, 0, []).
create_objects([], _MapNb, _GroupNb, _ObjectNb, Acc) ->
	lists:reverse(Acc);
create_objects([{ObjType, ObjPos, ObjRot, ObjParams}|Tail], MapNb, GroupNb, ObjectNb, Acc) ->
	Object = create_object(ObjType, ObjPos, ObjRot, ObjParams),
	create_objects(Tail, MapNb, GroupNb, ObjectNb + 1, [{{MapNb, GroupNb, ObjectNb}, Object}|Acc]).

%% @doc Create the given object.
create_object(Type, Pos, Rot, Params) ->
	M = list_to_existing_atom(lists:flatten(["egs_obj_", atom_to_list(Type)])),
	M:init(Pos, Rot, Params).

%% @doc Build a list of object indexes and targets based on the list of objects.
index_objects(Objects) ->
	index_objects(Objects, 0, [], 1024, []).
index_objects([], _Index, IndexesAcc, _Target, TargetsAcc) ->
	{lists:reverse(IndexesAcc), lists:reverse(TargetsAcc)};
index_objects([{Key, Object}|Tail], Index, IndexesAcc, Target, TargetsAcc) ->
	M = element(1, Object),
	Attrs = M:module_info(attributes),
	{Index2, IndexesAcc2} = case lists:keyfind(is_indexed, 1, Attrs) of
		{_, [true]}  -> {Index + 1, [{Index, Key}|IndexesAcc]};
		{_, [false]} -> {Index, IndexesAcc}
	end,
	{Target2, TargetsAcc2} = case lists:keyfind(is_target, 1, Attrs) of
		{_, [true]}  -> {Target + 1, [{Target, Key}|TargetsAcc]};
		{_, [false]} -> {Target, TargetsAcc}
	end,
	index_objects(Tail, Index2, IndexesAcc2, Target2, TargetsAcc2).

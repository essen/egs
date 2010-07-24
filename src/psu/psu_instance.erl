%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Handle instances, their objects and their events.
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

-module(psu_instance).
-behavior(gen_server).

-export([start_link/1, stop/1, floor_button_event/3, key_event/3, spawn_cleared_event/3, warp_event/5, hit/3]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-include_lib("stdlib/include/qlc.hrl").
-include("include/records.hrl").

%% @spec do(Q) -> Record
%% @doc Perform a mnesia transaction using a QLC query.
do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link(Zones) ->
	gen_server:start_link(?MODULE, [Zones], []).

%% @spec stop() -> stopped
stop(InstancePid) ->
	gen_server:call(InstancePid, stop).

%% @todo @spec
floor_button_event(InstancePid, ZoneID, ObjectID) ->
	gen_server:call(InstancePid, {floor_button_event, ZoneID, ObjectID}).

%% @todo @spec event(ServerPid, ObjectID, Args) -> Response
key_event(InstancePid, ZoneID, ObjectID) ->
	gen_server:call(InstancePid, {key_event, ZoneID, ObjectID}).

spawn_cleared_event(InstancePid, ZoneID, SpawnID) ->
	gen_server:call(InstancePid, {spawn_cleared_event, ZoneID, SpawnID}).

warp_event(InstancePid, ZoneID, BlockID, ListIndex, ObjectIndex) ->
	gen_server:call(InstancePid, {warp_event, ZoneID, BlockID, ListIndex, ObjectIndex}).

%% @todo @spec hit(ServerPid, TargetID, Args) -> Response
hit(User, SourceID, TargetID) ->
	gen_server:call(User#egs_user_model.instancepid, {hit, (User#egs_user_model.area)#psu_area.zoneid, User, SourceID, TargetID}).

%% gen_server.

init([Zones]) ->
	[map_init(Maps, ZoneID, 0, 0, 1024) || {ZoneID, Maps} <- Zones],
	{ok, undefined}.

map_init(undefined, _ZoneID, _BlockID, _ObjectID, _TargetID) ->
	ignore; %% @todo Temporary clause until everything works fine.
map_init([], _ZoneID, _BlockID, _ObjectID, _TargetID) ->
	ok;
map_init([{_MapID, ObjectLists}|Tail], ZoneID, BlockID, ObjectID, TargetID) ->
	{ok, NextObjectID, NextTargetID} = list_init(ObjectLists, ZoneID, BlockID, ObjectID, TargetID, 0),
	map_init(Tail, ZoneID, BlockID + 1, NextObjectID, NextTargetID).

list_init([], _ZoneID, _BlockID, ObjectID, TargetID, _ListIndex) ->
	{ok, ObjectID, TargetID};
list_init([Objects|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex) ->
	{ok, NextObjectID, NextTargetID} = object_init(Objects, ZoneID, BlockID, ObjectID, TargetID, ListIndex, 0),
	list_init(Tail, ZoneID, BlockID, NextObjectID, NextTargetID, ListIndex + 1).

object_init([], _ZoneID, _BlockID, ObjectID, TargetID, _ListIndex, _ObjectIndex) ->
	{ok, ObjectID, TargetID};

%% @doc box: {InstancePid, ZoneID, TargetID}
object_init([{box, _Model, Breakable, TrigEventID}|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex) ->
	case Breakable of
		false -> ignore;
		true -> object_insert(#psu_object{id={self(), ZoneID, TargetID}, instancepid=self(), type=box, args={BlockID, ObjectID, TrigEventID}})
	end,
	object_init(Tail, ZoneID, BlockID, ObjectID + 1, TargetID + 1, ListIndex, ObjectIndex + 1);

%% @doc floor_button: {InstancePid, ZoneID, floor_button, ObjectID
object_init([{floor_button, TrigEventID}|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex) ->
	object_insert(#psu_object{id={self(), ZoneID, floor_button, ObjectID}, instancepid=self(), type=floor_button, args={BlockID, TrigEventID}}),
	object_init(Tail, ZoneID, BlockID, ObjectID + 1, TargetID + 1, ListIndex, ObjectIndex + 1);

%% @doc key: {InstancePid, ZoneID, key, ObjectID}
object_init([{key, _KeySet, TrigEventID, _ReqEventID}|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex) ->
	object_insert(#psu_object{id={self(), ZoneID, key, ObjectID}, instancepid=self(), type=key, args={BlockID, [TrigEventID]}}),
	object_init(Tail, ZoneID, BlockID, ObjectID + 1, TargetID, ListIndex, ObjectIndex + 1);

%% @doc key_console: @see key; @todo handled the same for now
object_init([{key_console, KeySet, TrigEventID, _ReqEventID}|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex) ->
	object_insert(#psu_object{id={self(), ZoneID, key, ObjectID}, instancepid=self(), type=key_console, args={BlockID, [243 + KeySet, 201 + KeySet, TrigEventID]}}),
	object_init(Tail, ZoneID, BlockID, ObjectID + 1, TargetID, ListIndex, ObjectIndex + 1);

%% @doc spawn: {InstancePid, ZoneID, 'spawn', SpawnID}
%% @todo save enemies individually, do something, etc.
%% @todo temporarily save the spawn to handle events properly
object_init([{'spawn', NbTargets, TrigEventID, _ReqEventID}|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex) ->
	object_insert(#psu_object{id={self(), ZoneID, 'spawn', TargetID - 1024}, instancepid=self(), type='spawn', args={BlockID, TrigEventID}}),
	object_init(Tail, ZoneID, BlockID, ObjectID + 1, TargetID + NbTargets, ListIndex, ObjectIndex + 1);

%% @doc warp: {InstancePid, ZoneID, warp, BlockID, ListIndex, ObjectIndex}
object_init([{warp, DestX, DestY, DestZ, DestDir}|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex) ->
	object_insert(#psu_object{id={self(), ZoneID, warp, BlockID, ListIndex, ObjectIndex}, instancepid=self(), type=warp, args=#pos{x=DestX, y=DestY, z=DestZ, dir=DestDir}}),
	object_init(Tail, ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex + 1);

%% @doc Ignore for now: boss_gate, shoot_button, goggle_target, trap (all kinds)
object_init([Object|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex)
	when	Object =:= boss_gate;
			Object =:= shoot_button;
			Object =:= goggle_target;
			Object =:= trap ->
	object_init(Tail, ZoneID, BlockID, ObjectID + 1, TargetID + 1, ListIndex, ObjectIndex + 1);

%% @doc Ignore for now: 'exit' (seems to take a TargetID but not an ObjectID
object_init(['exit'|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex) ->
	object_init(Tail, ZoneID, BlockID, ObjectID, TargetID + 1, ListIndex, ObjectIndex + 1);

%% @doc Ignore for now: objects without any ObjectID or TargetID.
object_init([Object|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex)
	when	Object =:= static_model;
			Object =:= invisible_block;
			Object =:= entrance;
			Object =:= label;
			Object =:= colored_minimap_section;
			Object =:= fog;
			Object =:= pp_cube;
			Object =:= healing_pad ->
	object_init(Tail, ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex + 1);

%% @doc Ignore everything else for now: objects with an ObjectID but without a TargetID.
object_init([_Object|Tail], ZoneID, BlockID, ObjectID, TargetID, ListIndex, ObjectIndex) ->
	object_init(Tail, ZoneID, BlockID, ObjectID + 1, TargetID, ListIndex, ObjectIndex + 1).

%% Event handlers

handle_call({floor_button_event, ZoneID, ObjectID}, _From, State) ->
	#psu_object{args=Args} = object_select({self(), ZoneID, floor_button, ObjectID}),
	{reply, Args, State};

handle_call({key_event, ZoneID, ObjectID}, _From, State) ->
	#psu_object{args=Args} = object_select({self(), ZoneID, key, ObjectID}),
	{reply, Args, State};

handle_call({spawn_cleared_event, ZoneID, SpawnID}, _From, State) ->
	#psu_object{args=Args} = object_select({self(), ZoneID, 'spawn', SpawnID}),
	{reply, Args, State};

handle_call({warp_event, ZoneID, BlockID, ListIndex, ObjectIndex}, _From, State) ->
	#psu_object{args=Args} = object_select({self(), ZoneID, warp, BlockID, ListIndex, ObjectIndex}),
	{reply, Args, State};

%% Hit handler

%% @todo Handle everything correctly.
handle_call({hit, ZoneID, User, _SourceID, TargetID}, _From, State) ->
	try
		Box = object_select({self(), ZoneID, TargetID}),
		BoxReply = box_hit(Box),
		{reply, BoxReply, State}
	catch _:_ ->
		OtherReply = if TargetID =:= 0 ->
					player_hit(User);
			true -> enemy_hit(User)
		end,
		{reply, OtherReply, State}
	end;

%% @doc Remove all objects from the database when the process is stopped.
handle_call(stop, _From, State) ->
	List = do(qlc:q([X || X <- mnesia:table(psu_object), X#psu_object.instancepid =:= self()])),
	[mnesia:transaction(fun() -> mnesia:delete({psu_object, ID}) end) || #psu_object{id=ID} <- List],
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

%% Internal functions

%% @todo doc
object_insert(Object) ->
	mnesia:transaction(fun() -> mnesia:write(Object) end).

%% @todo doc
object_select(ID) ->
	case mnesia:transaction(fun() -> mnesia:read({psu_object, ID}) end) of
		{atomic, []} -> undefined;
		{atomic, [Val]} -> Val
	end.

box_hit(#psu_object{args={BlockID, ObjectID, TrigEventID}}) ->
	Events =
		if	TrigEventID =:= false -> [{explode, ObjectID}];
			true -> [{explode, ObjectID}, {event, [BlockID, TrigEventID]}]
		end,
	#hit_response{type=box, events=Events}.

enemy_hit(User) ->
	Damage = 1,
	IncEXP = 1,
	Character = User#egs_user_model.character,
	Level = Character#characters.mainlevel,
	NewEXP = Level#level.exp + IncEXP,
	NewLevel = Level#level{exp=NewEXP},
	NewCharacter = Character#characters{mainlevel=NewLevel},
	NewUser = User#egs_user_model{character=NewCharacter},
	% todo delete the enemy from the db when it dies
	#hit_response{type=enemy, user=NewUser, exp=true, damage=Damage, targethp=0, targetse=[death]}.

player_hit(User) ->
	Damage = 10,
	Character = User#egs_user_model.character,
	TmpHP = Character#characters.currenthp - Damage,
	if	TmpHP =< 0 ->
			NewHP = 0,
			SE = [flinch, death];
		true ->
			NewHP = TmpHP,
			SE = [flinch]
	end,
	NewCharacter = Character#characters{currenthp=NewHP},
	NewUser = User#egs_user_model{character=NewCharacter},
	#hit_response{type=player, user=NewUser, damage=Damage, targethp=NewHP, targetse=SE}.

%	EGS: Erlang Game Server
%	Copyright (C) 2010  Loic Hoguin
%
%	This file is part of EGS.
%
%	EGS is free software: you can redistribute it and/or modify
%	it under the terms of the GNU General Public License as published by
%	the Free Software Foundation, either version 3 of the License, or
%	(at your option) any later version.
%
%	EGS is distributed in the hope that it will be useful,
%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%	GNU General Public License for more details.
%
%	You should have received a copy of the GNU General Public License
%	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

-module(psu_missions).
-export([
	start/3, key_event/2, object_hit/3, spawn_cleared/2
]).

-include("include/maps.hrl").
-include("include/records.hrl").

start(InstanceID, QuestID, _SetID) ->
	MapList = proplists:get_value(QuestID, ?MISSIONS),
	[map_init(InstanceID, BlockID, ObjectsList) || {_MapID, BlockID, ObjectsList} <- MapList],
	ok.

map_init(InstanceID, BlockID, [{boxes, BoxesList}, {keys, KeysList}, {spawns, SpawnsList}]) ->
	[box_init(InstanceID, BlockID, Box) || Box <- BoxesList],
	[key_init(InstanceID, BlockID, Key) || Key <- KeysList],
	[spawn_init(InstanceID, BlockID, Spawn) || Spawn <- SpawnsList],
	ok.

box_init(InstanceID, BlockID, {ObjectID, TargetID, EventID}) ->
	egs_db:objects_insert(#objects{id=[InstanceID, ObjectID], instanceid=InstanceID, objectid=ObjectID, type=box, targetid=TargetID, blockid=BlockID, triggereventid=EventID}).

key_init(InstanceID, BlockID, {ObjectID, EventID}) ->
	egs_db:objects_insert(#objects{id=[InstanceID, {key, ObjectID}], instanceid=InstanceID, objectid=ObjectID, type=key, blockid=BlockID, triggereventid=EventID}).

spawn_init(InstanceID, BlockID, {ObjectID, EventID, _RequireEventID}) ->
	% todo save enemies individually, do something, etc.
	% todo temporarily save the spawn to handle events properly
	egs_db:objects_insert(#objects{id=[InstanceID, {'spawn', ObjectID}], type='spawn', blockid=BlockID, triggereventid=EventID}).

key_event(InstanceID, ObjectID) ->
	#objects{triggereventid=EventID, blockid=BlockID} = egs_db:objects_select([InstanceID, {key, ObjectID}]),
	[EventID, BlockID].

object_hit(User, _SourceID, TargetID) ->
	try
		Object = egs_db:objects_select_by_targetid(User#users.instanceid, TargetID),
		if	Object#objects.type =:= box ->
				box_hit(User, Object);
			true ->
				io:format("unknown object hit~n")
		end
	catch _:_ ->
		if	TargetID =:= 0 ->
				player_hit(User);
			true ->
				enemy_hit(User)
		end
	end.

box_hit(User, Box) ->
	% todo delete the box from the db
	EventsResponse =
		if	Box#objects.triggereventid =:= false -> [{explode, Box#objects.objectid}];
			true -> [{explode, Box#objects.objectid}, {event, [Box#objects.triggereventid, Box#objects.blockid]}]
		end,
	#hit_response{type=box, user=User, events=EventsResponse}.

enemy_hit(User) ->
	Damage = 1,
	IncEXP = 1,
	Character = User#users.character,
	Level = Character#characters.mainlevel,
	NewEXP = Level#level.exp + IncEXP,
	NewLevel = Level#level{exp=NewEXP},
	NewCharacter = Character#characters{mainlevel=NewLevel},
	NewUser = User#users{character=NewCharacter},
	% todo delete the enemy from the db when it dies
	#hit_response{type=enemy, user=NewUser, exp=true, damage=Damage, targethp=0, targetse=[death]}.

player_hit(User) ->
	Damage = 10,
	Character = User#users.character,
	TmpHP = Character#characters.currenthp - Damage,
	if	TmpHP =< 0 ->
			NewHP = 0,
			SE = [flinch, death];
		true ->
			NewHP = TmpHP,
			SE = [flinch]
	end,
	NewCharacter = Character#characters{currenthp=NewHP},
	NewUser = User#users{character=NewCharacter},
	#hit_response{type=player, user=NewUser, damage=Damage, targethp=NewHP, targetse=SE}.

spawn_cleared(InstanceID, SpawnID) ->
	#objects{triggereventid=EventID, blockid=BlockID} = egs_db:objects_select([InstanceID, {'spawn', SpawnID}]),
	[EventID, BlockID].

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
	start/3, stop/1, key_event/2, object_hit/3, spawn_cleared/2
]).

-include("include/missions.hrl").
-include("include/records.hrl").

start(InstanceID, QuestID, _SetID) ->
	MapList = proplists:get_value(QuestID, ?MISSIONS),
	map_init(InstanceID, MapList, 0, 0, 1024).

map_init(_InstanceID, [], _BlockID, _ObjectID, _TargetID) ->
	ok;
map_init(InstanceID, [Map|Tail], BlockID, ObjectID, TargetID) ->
	{_MapID, Objects} = Map,
	{ok, NewObjectID, NewTargetID} = object_init(InstanceID, BlockID, Objects, ObjectID, TargetID),
	map_init(InstanceID, Tail, BlockID + 1, NewObjectID, NewTargetID).

object_init(_InstanceID, _BlockID, [], ObjectID, TargetID) ->
	{ok, ObjectID, TargetID};
object_init(InstanceID, BlockID, [{box, _Model, Breakable, TrigEventID}|Tail], ObjectID, TargetID) ->
	case Breakable of
		false -> ignore;
		true ->
			egs_db:objects_insert(#objects{id=[InstanceID, ObjectID], instanceid=InstanceID, objectid=ObjectID, type=box, targetid=TargetID, blockid=BlockID, triggereventid=TrigEventID})
	end,
	object_init(InstanceID, BlockID, Tail, ObjectID + 1, TargetID + 1);
%% @todo key and key_console event handling will have to be fixed.
object_init(InstanceID, BlockID, [{key, TrigEventID, _ReqEventID}|Tail], ObjectID, TargetID) ->
	egs_db:objects_insert(#objects{id=[InstanceID, {key, ObjectID}], instanceid=InstanceID, objectid=ObjectID, type=key, blockid=BlockID, triggereventid=[TrigEventID]}),
	object_init(InstanceID, BlockID, Tail, ObjectID + 1, TargetID);
%% @todo Maybe separate key from key_console in its handling?
object_init(InstanceID, BlockID, [{key_console, _NoKeyEventID, TrigEventID}|Tail], ObjectID, TargetID) ->
	egs_db:objects_insert(#objects{id=[InstanceID, {key, ObjectID}], instanceid=InstanceID, objectid=ObjectID, type=key, blockid=BlockID, triggereventid=[244, 202, TrigEventID]}),
	object_init(InstanceID, BlockID, Tail, ObjectID + 1, TargetID);
%% @todo save enemies individually, do something, etc.
%% @todo temporarily save the spawn to handle events properly
object_init(InstanceID, BlockID, [{'spawn', TrigEventID, _ReqEventID}|Tail], ObjectID, TargetID) ->
	egs_db:objects_insert(#objects{id=[InstanceID, {'spawn', TargetID - 1024}], instanceid=InstanceID, type='spawn', blockid=BlockID, triggereventid=TrigEventID}),
	object_init(InstanceID, BlockID, Tail, ObjectID + 1, TargetID + 30);
%% @todo Not sure where these 2 come from yet, assuming crystal but might not be that.
object_init(InstanceID, BlockID, [crystal|Tail], ObjectID, TargetID) ->
	object_init(InstanceID, BlockID, Tail, ObjectID + 1, TargetID + 2);
%% A few object types don't have an ObjectID nor a TargetID. Disregard them completely.
object_init(InstanceID, BlockID, [ObjType|Tail], ObjectID, TargetID)
	when	ObjType =:= static_model;
			ObjType =:= invisible_block;
			ObjType =:= entrance;
			ObjType =:= 'exit';
			ObjType =:= label ->
	object_init(InstanceID, BlockID, Tail, ObjectID, TargetID);
%% Others are normal objects, we don't handle them but they have an ObjectID.
object_init(InstanceID, BlockID, [_|Tail], ObjectID, TargetID) ->
	object_init(InstanceID, BlockID, Tail, ObjectID + 1, TargetID).

stop(InstanceID) ->
	egs_db:objects_delete(InstanceID).

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

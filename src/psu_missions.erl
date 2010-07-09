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
	start/3, object_hit/3
]).

-include("include/maps.hrl").
-include("include/records.hrl").

start(InstanceID, QuestID, _SetID) ->
	MapList = proplists:get_value(QuestID, ?MISSIONS),
	[map_init(InstanceID, ObjectsList) || {_, ObjectsList} <- MapList],
	ok.

map_init(InstanceID, [{boxes, BoxesList}, {keys, KeysList}, {spawns, SpawnsList}]) ->
	[box_init(InstanceID, Box) || Box <- BoxesList],
	[key_init(InstanceID, Key) || Key <- KeysList],
	[spawn_init(InstanceID, Spawn) || Spawn <- SpawnsList],
	ok.

box_init(InstanceID, {ObjectID, TargetID}) ->
	egs_db:objects_insert(#objects{id=[InstanceID, ObjectID], instanceid=InstanceID, objectid=ObjectID, type=box, targetid=TargetID, params=[]}).

key_init(InstanceID, {ObjectID, EventID}) ->
	egs_db:objects_insert(#objects{id=[InstanceID, ObjectID], instanceid=InstanceID, objectid=ObjectID, type=key, params=[{event, EventID}]}).

spawn_init(InstanceID, {ObjectID, EventID}) ->
	% todo save enemies individually, do something, etc.
	%~ egs_db:objects_insert(#objects{id=[InstanceID, ObjectID], type='spawn', params=[{event, EventID}]}).
	ok.

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
	#hit_response{type=box, user=User, event={explode, Box#objects.objectid}}.

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
	Damage = 1,
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
	io:format("~b~n", [NewHP]),
	#hit_response{type=player, user=NewUser, damage=Damage, targethp=NewHP, targetse=SE}.

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

-module(egs_db).
-compile(export_all).

-include("include/records.hrl").

%% IMPORTANT: The next line must be included
%%            if we want to call qlc:q(...)

-include_lib("stdlib/include/qlc.hrl").

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

%% @doc Retrieve the next unique ID.
%% @todo Used only for the LID so far...
next(Type) ->
	mnesia:dirty_update_counter(ids, Type, 1).

%% @doc Insert a new object in the database.
%% @todo Group with users_insert, it's the same function really.

objects_insert(Object) ->
	mnesia:transaction(fun() -> mnesia:write(Object) end).

%% @doc Select an object by ID.

objects_select(ID) ->
	case mnesia:transaction(fun() -> mnesia:read({objects, ID}) end) of
		{atomic, []} ->
			undefined;
		{atomic, [Val]} ->
			Val
	end.

%% @doc Select an object by instanceid and targetid.

objects_select_by_targetid(InstanceID, TargetID) ->
	[Object] = do(qlc:q([X || X <- mnesia:table(objects),
		X#objects.instanceid =:= InstanceID,
		X#objects.targetid =:= TargetID])),
	Object.

%% @doc Select all IDs.
%% @todo This is for debug purposes only. Delete later.

objects_select_all() ->
	do(qlc:q([X || X <- mnesia:table(objects)])).

%% @doc Delete all the objects for the given instance.

objects_delete(InstanceID) ->
	List = do(qlc:q([X || X <- mnesia:table(objects), X#objects.instanceid =:= InstanceID])),
	[mnesia:transaction(fun() -> mnesia:delete({objects, ID}) end) || #objects{id=ID} <- List],
	ok.

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

%% @doc Count the number of users online.

users_count() ->
	mnesia:table_info(users, size).

%% @doc Select exactly one user by its GID. Return an #users record.

users_select(GID) ->
	case mnesia:transaction(fun() -> mnesia:read({users, GID}) end) of
		{atomic, []} ->
			error;
		{atomic, [Val]} ->
			Val
	end.

%% @doc Select exactly one user by its Pid. Return an #users record.

users_select_by_pid(Pid) ->
	[User] = do(qlc:q([X || X <- mnesia:table(users), X#users.pid =:= Pid])),
	User.

%% @doc Select all users. Return a list of #users records.

users_select_all() ->
	do(qlc:q([X || X <- mnesia:table(users), (X#users.character)#characters.slot /= undefined])).

%% @doc Select all other users in the same area. Return a list of #users records.

users_select_others_in_area(Self) ->
	do(qlc:q([X || X <- mnesia:table(users),
		X#users.gid /= Self#users.gid,
		(X#users.character)#characters.slot /= undefined,
		X#users.instanceid =:= Self#users.instanceid,
		X#users.questid =:= Self#users.questid,
		X#users.zoneid =:= Self#users.zoneid,
		X#users.mapid =:= Self#users.mapid
	])).

%% @doc Insert or update an user.

users_insert(User) ->
	mnesia:transaction(fun() -> mnesia:write(User) end).

%% @doc Delete an user.

users_delete(GID) ->
	mnesia:transaction(fun() -> mnesia:delete({users, GID}) end).

%% @doc Cleanup the disconnected users who failed after the login stage but before the game stage.

users_cleanup() ->
	Timeout = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 300,
	Users = do(qlc:q([X#users.gid || X <- mnesia:table(users),
		X#users.auth /= success, X#users.time < Timeout])),
	mnesia:transaction(fun() ->
		lists:foreach(fun(GID) -> users_delete(GID) end, Users)
	end).

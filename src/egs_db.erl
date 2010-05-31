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

%% @doc Create the database.

create() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(ids, [{attributes, record_info(fields, ids)}]),
	mnesia:dirty_update_counter(ids, lobby, 0),
	mnesia:create_table(users, [{attributes, record_info(fields, users)}]).

%% @doc Retrieve the next unique ID.

next(Type) ->
	mnesia:dirty_update_counter(ids, Type, 1).

%% @doc Select exactly one user by its GID. Return an #users record.

users_select(GID) ->
	case mnesia:transaction(fun() -> mnesia:read({users, GID}) end) of
		{atomic, []} ->
			error;
		{atomic, [Val]} ->
			Val
	end.

%% @doc Select all users. Return a list of #users records.

users_select_all() ->
	do(qlc:q([X || X <- mnesia:table(users), X#users.charnumber /= undefined])).

%% @doc Select all other users. Return a list of #users records.

users_select_others(GID) ->
	do(qlc:q([X || X <- mnesia:table(users), X#users.gid /= GID, X#users.charnumber /= undefined])).

%% @doc Select all other users in the same area. Return a list of #users records.

users_select_others_in_area(Self) ->
	do(qlc:q([X || X <- mnesia:table(users),
		X#users.gid /= Self#users.gid,
		X#users.charnumber /= undefined,
		X#users.instanceid =:= Self#users.instanceid,
		X#users.quest =:= Self#users.quest,
		X#users.maptype =:= Self#users.maptype,
		X#users.mapnumber =:= Self#users.mapnumber
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

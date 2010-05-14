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
%	gasetools is distributed in the hope that it will be useful,
%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%	GNU General Public License for more details.
%
%	You should have received a copy of the GNU General Public License
%	along with gasetools.  If not, see <http://www.gnu.org/licenses/>.

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
	{atomic, [Val]} = mnesia:transaction(fun() -> mnesia:read({users, GID}) end),
	Val.

%% @doc Select all users. Return a list of #users records.

users_select_all() ->
	do(qlc:q([X || X <- mnesia:table(users)])).

%% @doc Select all other users. Return a list of #users records.

users_select_others(GID) ->
	do(qlc:q([X || X <- mnesia:table(users), X#users.gid /= GID, X#users.charnumber /= undefined])).

%% @doc Insert or update an user.

users_insert(User) ->
	mnesia:transaction(fun() -> mnesia:write(User) end).

%% @doc Delete an user.

users_delete(GID) ->
	mnesia:transaction(fun() -> mnesia:delete({users, GID}) end).

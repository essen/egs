%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Accounts handling.
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

-module(egs_accounts).
-export([get_folder/1, key_auth/2, key_auth_init/1, key_auth_timeout/1, login_auth/2]).

-define(TABLE, accounts).

-include("include/records.hrl").

%% @todo Temporary code until we properly save the player data.
get_folder(GID) ->
	{atomic, [#accounts{username=Username, password=Password}]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	<< Username/binary, "-", Password/binary >>.

key_auth(GID, AuthKey) ->
	{atomic, [#accounts{auth_state=AuthState}]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	case AuthState of
		{wait_for_authentication, AuthKey, TRef} ->
			timer:cancel(TRef),
			mnesia:transaction(fun() ->
				Account = mnesia:read({?TABLE, GID}),
				mnesia:write(Account#accounts{auth_state=undefined})
			end),
			ok;
		_Any ->
			{error, badarg}
	end.

key_auth_init(GID) ->
	AuthKey = crypto:rand_bytes(4),
	TRef = timer:apply_after(10000, ?MODULE, key_auth_timeout, [GID]),
	mnesia:transaction(fun() ->
		[Account] = mnesia:read({?TABLE, GID}),
		mnesia:write(Account#accounts{auth_state={wait_for_authentication, AuthKey, TRef}})
	end),
	{ok, AuthKey}.

key_auth_timeout(GID) ->
	mnesia:transaction(fun() ->
		Account = mnesia:read({?TABLE, GID}),
		mnesia:write(Account#accounts{auth_state=undefined})
	end).

%% @todo Properly handle login authentication when accounts are saved.
login_auth(Username, Password) ->
	GID = 10000000 + mnesia:dirty_update_counter(counters, gid, 1),
	mnesia:transaction(fun() -> mnesia:write(#accounts{gid=GID, username=Username, password=Password}) end),
	{ok, GID}.

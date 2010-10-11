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
-behavior(gen_server).
-export([start_link/0, stop/0, get_folder/1, key_auth/2, key_auth_init/1, login_auth/2]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).
-define(TABLE, accounts).

-include("include/records.hrl").

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @todo Temporary code until we properly save the player data.
get_folder(GID) ->
	gen_server:call(?SERVER, {get_folder, GID}).

key_auth(GID, AuthKey) ->
	gen_server:call(?SERVER, {key_auth, GID, AuthKey}).

key_auth_init(GID) ->
	gen_server:call(?SERVER, {key_auth_init, GID}).

%% @todo Properly handle login authentication when accounts are saved.
login_auth(Username, Password) ->
	gen_server:call(?SERVER, {login_auth, Username, Password}).

%% gen_server.

init([]) ->
	error_logger:info_report("egs_accounts started"),
	{ok, undefined}.

handle_call({get_folder, GID}, _From, State) ->
	{atomic, [#accounts{username=Username, password=Password}]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	{reply, << Username/binary, "-", Password/binary >>, State};

handle_call({key_auth, GID, AuthKey}, _From, State) ->
	{atomic, [#accounts{auth_state=AuthState}]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	case AuthState of
		{wait_for_authentication, AuthKey, TRef} ->
			timer:cancel(TRef),
			mnesia:transaction(fun() ->
				Account = mnesia:read({?TABLE, GID}),
				mnesia:write(Account#accounts{auth_state=undefined})
			end),
			{reply, ok, State};
		_Any ->
			{reply, {error, badarg}, State}
	end;

handle_call({key_auth_init, GID}, _From, State) ->
	AuthKey = crypto:rand_bytes(4),
	TRef = timer:send_after(10000, {key_auth_timeout, GID}),
	mnesia:transaction(fun() ->
		[Account] = mnesia:read({?TABLE, GID}),
		mnesia:write(Account#accounts{auth_state={wait_for_authentication, AuthKey, TRef}})
	end),
	{reply, {ok, AuthKey}, State};

handle_call({login_auth, Username, Password}, _From, State) ->
	GID = 10000000 + mnesia:dirty_update_counter(counters, gid, 1),
	mnesia:transaction(fun() -> mnesia:write(#accounts{gid=GID, username=Username, password=Password}) end),
	{reply, {ok, GID}, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({key_auth_timeout, GID}, State) ->
	mnesia:transaction(fun() ->
		Account = mnesia:read({?TABLE, GID}),
		mnesia:write(Account#accounts{auth_state=undefined})
	end),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
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
-behaviour(gen_server).

-export([start_link/0, stop/0, get_folder/1, key_auth/2, key_auth_init/1, key_auth_timeout/1, login_auth/2, tmp_gid/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-define(SERVER, ?MODULE).

%% @todo Make accounts permanent.
%% @todo Hash the password.
%% @todo Add email, password_salt, is_ingame, register_time, last_login_time, etc.
-record(accounts, {
	gid			:: integer(),
	username	:: string(),
	password	:: string(),
	auth_state	:: undefined | {wait_for_authentication, binary(), any()}
}).

-record(state, {
	accounts = []			:: list({GID::integer(), #accounts{}}),
	next_gid = 10000001		:: integer(),
	tmp_gid  = 16#ff000001	:: integer()
}).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
	gen_server:call(?SERVER, stop).

%% @todo Temporary code until we properly save the player data.
get_folder(GID) ->
	gen_server:call(?SERVER, {get_folder, GID}).

-spec key_auth(GID::integer(), AuthKey::binary()) -> ok | {error, badarg}.
%% @doc Authenticate using the given key.
key_auth(GID, AuthKey) ->
	gen_server:call(?SERVER, {key_auth, GID, AuthKey}).

-spec key_auth_init(GID::integer()) -> {ok, AuthKey::binary()}.
%% @doc Initialize key authentication. Obtain a key for a subsequent re-authentication on a different connection.
key_auth_init(GID) ->
	gen_server:call(?SERVER, {key_auth_init, GID}).

-spec key_auth_timeout(GID::integer()) -> ok.
%% @doc Key authentication timeout handling.
%% @todo Probably handle the authentication in a gen_fsm properly.
key_auth_timeout(GID) ->
	gen_server:cast(?SERVER, {key_auth_timeout, GID}).

-spec login_auth(Username::binary(), Password::binary()) -> {ok, GID::integer()}.
%% @doc Authenticate using the given username and password.
%% @todo Properly handle login authentication when accounts are saved.
login_auth(Username, Password) ->
	gen_server:call(?SERVER, {login_auth, Username, Password}).

-spec tmp_gid() -> GID::integer().
%% @doc Return an unused temporary GID for initial connection and APC characters.
tmp_gid() ->
	gen_server:call(?SERVER, tmp_gid).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({get_folder, GID}, _From, State) ->
	{_, #accounts{username=Username, password=Password}} = lists:keyfind(GID, 1, State#state.accounts),
	{reply, << Username/binary, "-", Password/binary >>, State};

handle_call({key_auth, GID, AuthKey}, _From, State) ->
	{_, Account = #accounts{auth_state=AuthState}} = lists:keyfind(GID, 1, State#state.accounts),
	case AuthState of
		{wait_for_authentication, AuthKey, TRef} ->
			timer:cancel(TRef),
			Accounts = lists:delete({GID, Account}, State#state.accounts),
			{reply, ok, State#state{accounts=[{GID, Account#accounts{auth_state=undefined}}|Accounts]}};
		undefined ->
			{reply, {error, badarg}, State}
	end;

handle_call({key_auth_init, GID}, _From, State) ->
	AuthKey = crypto:rand_bytes(4),
	TRef = timer:apply_after(10000, ?MODULE, key_auth_timeout, [GID]),
	{_, Account} = lists:keyfind(GID, 1, State#state.accounts),
	Accounts = lists:delete({GID, Account}, State#state.accounts),
	{reply, {ok, AuthKey}, State#state{accounts=
		[{GID, Account#accounts{auth_state={wait_for_authentication, AuthKey, TRef}}}|Accounts]}};

handle_call({login_auth, Username, Password}, _From, State) ->
	GID = State#state.next_gid,
	Account = #accounts{gid=GID, username=Username, password=Password},
	{reply, {ok, GID}, State#state{next_gid=GID + 1, accounts=[{GID, Account}|State#state.accounts]}};

handle_call(tmp_gid, _From, State) ->
	GID = State#state.tmp_gid,
	{reply, GID, State#state{tmp_gid=GID + 1}};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({key_auth_timeout, GID}, State) ->
	{_, Account} = lists:keyfind(GID, 1, State#state.accounts),
	Accounts = lists:delete({GID, Account}, State#state.accounts),
	{noreply, State#state{accounts= [{GID, Account#accounts{auth_state=undefined}}|Accounts]}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

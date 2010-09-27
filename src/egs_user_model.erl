%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc User domain model.
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

-module(egs_user_model).
-behavior(gen_server).
-export([start_link/0, stop/0, count/0, read/1, select/1, write/1, delete/1, key_auth/3, login_auth/2]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name and for the table name.
-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-include("include/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @spec do(Q) -> Record
%% @doc Perform a mnesia transaction using a QLC query.
do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

%% API

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec count() -> {ok, Count}
count() ->
	gen_server:call(?SERVER, count).

%% @spec read({pid, Pid}) -> {ok, User} | {error, badarg}
%% @spec read(ID) -> {ok, User} | {error, badarg}
read(ID) ->
	gen_server:call(?SERVER, {read, ID}).

%% @spec select(all) -> {ok, List}
%% @spec select({neighbors, User}) -> {ok, List}
select(Type) ->
	gen_server:call(?SERVER, {select, Type}).

%% @spec write(User) -> ok
write(User) ->
	gen_server:cast(?SERVER, {write, User}).

%% @spec delete(ID) -> ok
delete(ID) ->
	gen_server:cast(?SERVER, {delete, ID}).

key_auth(GID, AuthKey, Socket) ->
	gen_server:call(?SERVER, {key_auth, GID, AuthKey, Socket}).

login_auth(Username, Password) ->
	gen_server:call(?SERVER, {login_auth, Username, Password}).

%% gen_server

init([]) ->
	timer:apply_interval(30000, gen_server, cast, [?SERVER, cleanup]),
	error_logger:info_report("egs_user_model started"),
	{ok, undefined}.

handle_call(count, _From, State) ->
	Count = mnesia:table_info(?TABLE, size),
	{reply, {ok, Count}, State};

handle_call({read, {pid, Pid}}, _From, State) ->
	List = do(qlc:q([X || X <- mnesia:table(?TABLE), X#?TABLE.pid =:= Pid])),
	case List of
		[] -> {reply, {error, badarg}, State};
		[User] -> {reply, {ok, User}, State}
	end;

handle_call({read, ID}, _From, State) ->
	case mnesia:transaction(fun() -> mnesia:read({?TABLE, ID}) end) of
		{atomic, []} -> {reply, {error, badarg}, State};
		{atomic, [Val]} -> {reply, {ok, Val}, State}
	end;

%% @todo state = undefined | {wait_for_authentication, Key} | authenticated | online
handle_call({select, all}, _From, State) ->
	List = do(qlc:q([X || X <- mnesia:table(?TABLE),
		X#?TABLE.pid /= undefined,
		X#?TABLE.state =:= online
	])),
	{reply, {ok, List}, State};

handle_call({select, {neighbors, User}}, _From, State) ->
	List = do(qlc:q([X || X <- mnesia:table(?TABLE),
		X#?TABLE.id /= User#?TABLE.id,
		X#?TABLE.pid /= undefined,
		X#?TABLE.state =:= online,
		X#?TABLE.instancepid =:= User#?TABLE.instancepid,
		X#?TABLE.area =:= User#?TABLE.area
	])),
	{reply, {ok, List}, State};

%% @todo Handle LIDs properly, so not here.
handle_call({key_auth, GID, AuthKey, Socket}, {Pid, _Tag}, State) ->
	{atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	{wait_for_authentication, AuthKey} = User#egs_user_model.state,
	LID = 1 + mnesia:dirty_update_counter(counters, lobby, 1) rem 1023,
	Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	User2 = User#egs_user_model{pid=Pid, socket=Socket, state=authenticated, time=Time, lid=LID},
	mnesia:transaction(fun() -> mnesia:write(User2) end),
	{reply, ok, State};

%% @todo Handle GIDs and accounts and login properly. We currently accept everyone and give a new GID each time.
handle_call({login_auth, Username, Password}, _From, State) ->
	AuthGID = 10000000 + mnesia:dirty_update_counter(counters, gid, 1),
	AuthKey = crypto:rand_bytes(4),
	Folder = << Username/binary, "-", Password/binary >>,
	egs_user_model:write(#egs_user_model{id=AuthGID, state={wait_for_authentication, AuthKey}, folder=Folder}),
	{reply, {ok, AuthGID, AuthKey}, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({write, User}, State) ->
	mnesia:transaction(fun() -> mnesia:write(User) end),
	{noreply, State};

handle_cast({delete, ID}, State) ->
	mnesia:transaction(fun() -> mnesia:delete({?TABLE, ID}) end),
	{noreply, State};

%% @todo Cleanup more than the auth failures?
handle_cast(cleanup, State) ->
	Timeout = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 300,
	List = do(qlc:q([X#?TABLE.id || X <- mnesia:table(?TABLE),
		X#?TABLE.state /= authenticated,
		X#?TABLE.state /= online,
		X#?TABLE.time < Timeout
	])),
	mnesia:transaction(fun() ->
		lists:foreach(fun(ID) -> delete(ID) end, List)
	end),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc EGS universes handler.
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

-module(egs_universes).
-behavior(gen_server).

-export([start_link/0, stop/0, all/0, defaultid/0, enter/1, leave/1, myroomid/0, read/1, lobby_pid/2, reload/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

-record(state, {unis=[], lobbies=[]}).

%% Default universe IDs.
-define(MYROOM_ID, 21).
-define(DEFAULT_ID, 26).

%% Lobbies: permanent quests to start with the universe.
-define(LOBBIES, [1100000]).

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec all() -> term()
all() ->
	gen_server:call(?SERVER, all).

%% @spec defaultid() -> 26
%% @doc Return the default universe, Uni 01, with ID 26.
defaultid() ->
	?DEFAULT_ID.

%% @spec enter(UniID) -> term()
enter(UniID) ->
	gen_server:cast(?SERVER, {enter, UniID}).

%% @spec leave(UniID) -> term()
leave(UniID) ->
	gen_server:cast(?SERVER, {leave, UniID}).

%% @spec myroomid() -> 21
%% @doc Return the ID for the myroom universe.
myroomid() ->
	?MYROOM_ID.

%% @spec read(UniID) -> term()
read(UniID) ->
	gen_server:call(?SERVER, {read, UniID}).

lobby_pid(UniID, QuestID) ->
	gen_server:call(?SERVER, {lobby_pid, UniID, QuestID}).

%% @spec reload() -> ok
reload() ->
	gen_server:cast(?SERVER, reload).

%% gen_server.

%% @doc Create the unis, then load all the permanent quests nbl files, then create processes for all the needed quests.
init([]) ->
	State = #state{unis=[create_myroom()|create_unis()]},
	[egs_quests_db:quest_nbl(QuestID) || QuestID <- ?LOBBIES],
	Lobbies = lists:flatten([init_lobbies(UniID) || {UniID, {Type, _Name, _NbPlayers, _MaxPlayers}} <- State#state.unis, Type =:= universe]),
	{ok, State#state{lobbies=Lobbies}}.

handle_call(all, _From, State) ->
	{reply, State#state.unis, State};

handle_call({read, UniID}, _From, State) ->
	{reply, proplists:get_value(UniID, State#state.unis), State};

handle_call({lobby_pid, UniID, QuestID}, _From, State) ->
	{_, Pid} = lists:keyfind({UniID, QuestID}, 1, State#state.lobbies),
	{reply, Pid, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({enter, UniID}, State) ->
	{Type, Name, NbPlayers, MaxPlayers} = proplists:get_value(UniID, State#state.unis),
	Unis = proplists:delete(UniID, State#state.unis),
	Unis2 = [{UniID, {Type, Name, NbPlayers + 1, MaxPlayers}}|Unis],
	Unis3 = lists:keysort(1, Unis2),
	{noreply, State#state{unis=Unis3}};

handle_cast({leave, UniID}, State) ->
	{Type, Name, NbPlayers, MaxPlayers} = proplists:get_value(UniID, State#state.unis),
	Unis = proplists:delete(UniID, State#state.unis),
	Unis2 = [{UniID, {Type, Name, NbPlayers - 1, MaxPlayers}}|Unis],
	Unis3 = lists:keysort(1, Unis2),
	{noreply, State#state{unis=Unis3}};

handle_cast(reload, _State) ->
	{noreply, #state{unis=[create_myroom()|create_unis()]}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

%% @doc Max players defaults to 5000 for now.
create_myroom() ->
	{ok, << 16#fffe:16, MyRoomName/binary >>} = file:read_file("priv/universes/myroom.en_US.txt"),
	{?MYROOM_ID, {myroom, MyRoomName, 0, 5000}}.

%% @doc Max players defaults to 1000 for now.
create_unis() ->
	{ok, << 16#fffe:16, Universes/binary >>} = file:read_file("priv/universes/universes.en_US.txt"),
	Universes2 = re:split(Universes, "\n."),
	create_unis(Universes2, ?DEFAULT_ID, []).
create_unis([], _UniID, Acc) ->
	lists:reverse(Acc);
create_unis([Name|Tail], UniID, Acc) ->
	create_unis(Tail, UniID + 2, [{UniID, {universe, Name, 0, 1000}}|Acc]).

%% @doc Start lobbies for the given universe.
init_lobbies(UniID) ->
	lists:map(fun(QuestID) ->
		{ok, Pid} = supervisor:start_child(egs_quests_sup, {{quest, UniID, QuestID}, {egs_quests, start_link, [UniID, QuestID]}, permanent, 5000, worker, dynamic}),
		{{UniID, QuestID}, Pid}
	end, ?LOBBIES).

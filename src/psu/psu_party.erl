%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Party gen_server.
%%
%%	This file is part of EGS.
%%
%%	EGS is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU General Public License as published by
%%	the Free Software Foundation, either version 3 of the License, or
%%	(at your option) any later version.
%%
%%	EGS is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU General Public License for more details.
%%
%%	You should have received a copy of the GNU General Public License
%%	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

-module(psu_party).
-behavior(gen_server).
-export([start_link/1, stop/1, join/3, leave/2, get_instance/1, set_instance/2, remove_instance/1, get_npc/1]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-record(state, {free_spots, users, instancepid}).

%% API

%% @spec start_link() -> {ok,Pid::pid()}
start_link(UserID) ->
	gen_server:start_link(?MODULE, [UserID], []).

%% @spec stop() -> stopped
stop(PartyPid) ->
	gen_server:call(PartyPid, stop).

%% @doc PlayerType is either player or npc.
join(PartyPid, PlayerType, UserID) ->
	gen_server:call(PartyPid, {join, PlayerType, UserID}).

leave(PartyPid, UserID) ->
	gen_server:cast(PartyPid, {leave, UserID}).

get_instance(PartyPid) ->
	gen_server:call(PartyPid, get_instance).

set_instance(PartyPid, InstancePid) ->
	gen_server:cast(PartyPid, {set_instance, InstancePid}).

remove_instance(PartyPid) ->
	gen_server:cast(PartyPid, remove_instance).

%% @doc Returns a list of NPC UserID.
get_npc(PartyPid) ->
	gen_server:call(PartyPid, get_npc).

%% gen_server

init([UserID]) ->
	error_logger:info_report("a psu_party has been started"),
	{ok, {state, [1,2,3,4,5], [{0, leader, UserID}], undefined}}. %% 0 is party leader

%% @todo Probably want to broadcast to other players that you joined the party.
%% @todo Handle party passwords.
handle_call({join, PlayerType, UserID}, _From, State) ->
	List = case PlayerType of
		npc -> lists:reverse(State#state.free_spots);
		_ -> State#state.free_spots
	end,
	case List of
		[] ->
			{reply, {error, full}, State};
		[Spot|FreeSpots] ->
			Users = State#state.users,
			SavedFreeSpots = case PlayerType of
				npc -> lists:reverse(FreeSpots);
				_ -> FreeSpots
			end,
			{reply, {ok, Spot}, State#state{free_spots=SavedFreeSpots, users=[{Spot, PlayerType, UserID}|Users]}}
	end;

handle_call(get_instance, _From, State) ->
	{reply, {ok, State#state.instancepid}, State};

handle_call(get_npc, _From, State) ->
	List = [{Spot, UserID} || {Spot, PlayerType, UserID} <- State#state.users, PlayerType =:= npc],
	{reply, {ok, List}, State};

%% @todo Delete npc users when the party stops.
handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%% @todo Probably want to broadcast to other players that you left the party.
%% @todo Stop the party when it becomes empty.
%% @todo Delete npc users when the leader leaves.
%% @todo Give leader to someone else.
handle_cast({leave, _UserID}, State) ->
	%% @todo Do it.
	{noreply, State};

%% @todo Probably want to broadcast to other players that an instance started.
handle_cast({set_instance, InstancePid}, State) ->
	{noreply, State#state{instancepid=InstancePid}};

%% @todo Probably want to broadcast to other players that an instance stopped.
handle_cast(remove_instance, State) ->
	{noreply, State#state{instancepid=undefined}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

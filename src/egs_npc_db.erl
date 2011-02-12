%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc EGS NPC database.
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

-module(egs_npc_db).
-behavior(gen_server).
-export([start_link/0, stop/0, all/0, create/2, reload/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

-include("include/records.hrl").
-include("priv/npc.hrl").

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec all() -> List
all() ->
	gen_server:call(?SERVER, all).

%% @spec read(NPCid, BaseLevel) -> term()
create(NPCid, BaseLevel) ->
	gen_server:call(?SERVER, {create, NPCid, BaseLevel}).

%% @spec reload() -> ok
reload() ->
	gen_server:cast(?SERVER, reload).

%% gen_server.

init([]) ->
	error_logger:info_report("egs_npc_db started"),
	{ok, undefined}.

handle_call(all, _From, State) ->
	{reply, ?NPC, State};

%% @todo Handle stats, experience, based on level.
handle_call({create, NPCid, BaseLevel}, _From, State) ->
	NPCGID = 16#ff000000 + mnesia:dirty_update_counter(counters, tmpgid, 1),
	#npc{name=Name, race=Race, gender=Gender, class=Class, level_diff=LevelDiff, appearance=Appearance} = proplists:get_value(NPCid, ?NPC),
	TmpUCS2Name = << << X:8, 0:8 >> || X <- Name >>,
	Padding = 8 * (64 - byte_size(TmpUCS2Name)),
	UCS2Name = << TmpUCS2Name/binary, 0:Padding >>,
	Character = #characters{gid=NPCGID, slot=0, type=npc, npcid=NPCid, name=UCS2Name, race=Race, gender=Gender, class=Class, appearance=Appearance,
		mainlevel={level, calc_level(BaseLevel, LevelDiff), 0}, blastbar=0, luck=2, money=0, playtime=0, stats={stats, 0, 0, 0, 0, 0, 0, 0}, se=[], currenthp=100, maxhp=100},
	User = #users{gid=NPCGID, character=Character, areatype=lobby, area={0, 0, 0}, entryid=0},
	{reply, User, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%% @doc Compile this file here and let the reloader reload the code properly.
handle_cast(reload, State) ->
	compile:file(?FILE, [verbose, report_errors, report_warnings, {outdir, "ebin/"}]),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

calc_level(BaseLevel, LevelDiff) ->
	TmpLevel = BaseLevel + LevelDiff,
	if	TmpLevel < 1 -> 1;
		TmpLevel > 200 -> 200;
		true -> TmpLevel
	end.

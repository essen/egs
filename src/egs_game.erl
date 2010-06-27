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

-module(egs_game).
-export([start/0]). % external
-export([supervisor_init/0, supervisor/0, listen/1, accept/2, process/2, char_select/3, area_load/6, loop/3, loop/4]). % internal

-include("include/records.hrl").
-include("include/network.hrl").
-include("include/maps.hrl").

%% @doc Start the game server.

start() ->
	SPid = spawn(?MODULE, supervisor_init, []),
	LPid = spawn(?MODULE, listen, [SPid]),
	[{listener, LPid}, {supervisor, SPid}].

%% @doc Game processes supervisor initialization.

supervisor_init() ->
	process_flag(trap_exit, true),
	supervisor().

%% @doc Game processes supervisor. Make sure everything is cleaned up when an unexpected error occurs.

supervisor() ->
	receive
		{link, Pid} ->
			link(Pid);
		{'EXIT', Pid, _} ->
			supervisor_close(Pid);
		_ ->
			reload
	after 5000 ->
		reload
	end,
	?MODULE:supervisor().

%% @doc Close the connection for the given user and cleanup.

supervisor_close(Pid) ->
	try
		User = egs_db:users_select_by_pid(Pid),
		log(User#users.gid, "quit"),
		lists:foreach(fun(Other) -> Other#users.pid ! {psu_player_unspawn, User} end, egs_db:users_select_others_in_area(User)),
		egs_db:users_delete(User#users.gid)
	catch _:_ ->
		ignore
	end.

%% @doc Listen for connections.

listen(SPid) ->
	{ok, LSocket} = ssl:listen(?GAME_PORT, ?GAME_LISTEN_OPTIONS),
	?MODULE:accept(LSocket, SPid).

%% @doc Accept connections.

accept(LSocket, SPid) ->
	case ssl:transport_accept(LSocket, 5000) of
		{ok, CSocket} ->
			ssl:ssl_accept(CSocket),
			try
				send_0202(CSocket),
				Pid = spawn(?MODULE, process, [CSocket, 0]),
				SPid ! {link, Pid},
				ssl:controlling_process(CSocket, Pid)
			catch
				_:_ ->
					reload
			end;
		_ ->
			reload
	end,
	?MODULE:accept(LSocket, SPid).

%% @doc Process the new connections.
%%      Send an hello packet, authenticate the user and send him to character select.

process(CSocket, Version) ->
	case egs_proto:packet_recv(CSocket, 5000) of
		{ok, Orig} ->
			<< _:32, Command:16/unsigned-integer, _/bits >> = Orig,
			process_handle(Command, CSocket, Version, Orig);
		{error, timeout} ->
			reload,
			?MODULE:process(CSocket, Version);
		{error, closed} ->
			closed
	end.

%% @doc Game server auth request handler.

process_handle(16#020d, CSocket, Version, Orig) ->
	<< _:352, GID:32/little-unsigned-integer, Auth:32/bits, _/bits >> = Orig,
	case egs_db:users_select(GID) of
		error ->
			log(GID, "can't find user, closing"),
			ssl:close(CSocket);
		User ->
			case User#users.auth of
				Auth ->
					log(GID, "auth success"),
					LID = 1 + egs_db:next(lobby) rem 1023,
					Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
					egs_db:users_insert(#users{gid=GID, pid=self(), socket=CSocket, auth=success, time=Time, folder=User#users.folder, lid=LID}),
					send_0d05(CSocket, GID),
					?MODULE:char_select(CSocket, GID, Version);
				_ ->
					log(GID, "quit, auth failed"),
					egs_db:users_delete(GID),
					ssl:close(CSocket)
			end
	end;

%% @doc Platform information handler. Obtain the game version.

process_handle(16#080e, CSocket, _, Orig) ->
	<< _:416, Version:32/little-unsigned-integer, _/bits >> = Orig,
	?MODULE:process(CSocket, Version);

%% @doc Unknown command handler. Do nothing.

process_handle(Command, CSocket, Version, _) ->
	log(0, "(process) dismissed packet ~4.16.0b", [Command]),
	?MODULE:process(CSocket, Version).

%% @doc Character selection screen loop.
%%      The default entry point currently is first floor, near the uni cube.

char_select(CSocket, GID, Version) ->
	case egs_proto:packet_recv(CSocket, 5000) of
		{ok, Orig} ->
			<< _:32, Command:16/unsigned-integer, _/bits >> = Orig,
			char_select_handle(Command, CSocket, GID, Version, Orig);
		{error, timeout} ->
			egs_proto:send_keepalive(CSocket),
			reload,
			?MODULE:char_select(CSocket, GID, Version);
		{error, closed} ->
			log(GID, "quit"),
			egs_db:users_delete(GID)
	end.

%% @doc Character selection handler.

char_select_handle(16#020b, CSocket, GID, Version, Orig) ->
	<< _:352, Number:32/little-unsigned-integer, _/bits >> = Orig,
	log(GID, "selected character ~b", [Number]),
	char_select_load(CSocket, GID, Version, Number);

%% @doc Character creation handler.

char_select_handle(16#0d02, CSocket, GID, Version, Orig) ->
	log(GID, "character creation"),
	User = egs_db:users_select(GID),
	<< _:352, Number:32/little-unsigned-integer, Char/bits >> = Orig,
	_ = file:make_dir(io_lib:format("save/~s", [User#users.folder])),
	file:write_file(io_lib:format("save/~s/~b-character", [User#users.folder, Number]), Char),
	file:write_file(io_lib:format("save/~s/~b-character.options", [User#users.folder, Number]), << 0:192 >>),
	char_select_load(CSocket, GID, Version, Number);

%% @doc Character selection screen request.

char_select_handle(16#0d06, CSocket, GID, Version, _) ->
	User = egs_db:users_select(GID),
	send_0d03(CSocket, GID,
		data_load(User#users.folder, 0),
		data_load(User#users.folder, 1),
		data_load(User#users.folder, 2),
		data_load(User#users.folder, 3)),
	?MODULE:char_select(CSocket, GID, Version);

%% @doc Silently ignore packet 0818. Gives CPU/GPU information.

char_select_handle(16#0818, CSocket, GID, Version, _) ->
	?MODULE:char_select(CSocket, GID, Version);

%% @doc Unknown command handler. Do nothing.

char_select_handle(Command, CSocket, GID, Version, _) ->
	log(GID, "(char_select) dismissed packet ~4.16.0b", [Command]),
	?MODULE:char_select(CSocket, GID, Version).

%% @doc Load the selected character in the start lobby and start the main game's loop.

char_select_load(CSocket, GID, Version, Number) ->
	User = egs_db:users_select(GID),
	[{status, 1}, {char, Char}, {options, Options}] = data_load(User#users.folder, Number),
	<< Name:512/bits, _/bits >> = Char,
	NewRow = User#users{charnumber=Number, charname=Name},
	egs_db:users_insert(NewRow),
	char_load(CSocket, GID, Char, Options, Number),
	send_021b(CSocket, GID),
	area_load(CSocket, GID, 1100000, 0, 1, 1),
	ssl:setopts(CSocket, [{active, true}]),
	?MODULE:loop(CSocket, GID, Version).

%% @doc Load the given character's data.

data_load(Folder, Number) ->
	Filename = io_lib:format("save/~s/~b-character", [Folder, Number]),
	case file:read_file(Filename) of
		{ok, Char} ->
			{ok, Options} = file:read_file(io_lib:format("~s.options", [Filename])),
			[{status, 1}, {char, Char}, {options, Options}];
		{error, _} ->
			[{status, 0}, {char, << 0:2208 >>}]
	end.

%% @doc Load and send the character information to the client.

char_load(CSocket, GID, Char, Options, Number) ->
	send_0d01(CSocket, GID, Char, Options),
	% 0246
	send_0a0a(CSocket, GID),
	send_1006(CSocket, GID, 5),
	send_1005(CSocket, GID, Char),
	send_1006(CSocket, GID, 12),
	% 0210
	send_0222(CSocket, GID),
	send_1500(CSocket, GID, Char, Number),
	send_1501(CSocket, GID),
	send_1512(CSocket, GID),
	% 0303
	send_1602(CSocket, GID).

%% @doc Load the given map as a mission counter.

counter_load(CSocket, GID, QuestID, ZoneID, MapID, EntryID) ->
	OldUser = egs_db:users_select(GID),
	User = OldUser#users{areatype=counter, questid=QuestID, zoneid=ZoneID, mapid=MapID, entryid=EntryID,
		savedquestid=OldUser#users.questid, savedzoneid=OldUser#users.zoneid, savedmapid=ZoneID, savedentryid=MapID},
	egs_db:users_insert(User),
	[{status, 1}, {char, Char}, {options, _}] = data_load(User#users.folder, User#users.charnumber),
	AreaName = "Mission counter",
	QuestFile = "data/lobby/counter.quest.nbl",
	ZoneFile = "data/lobby/counter.zone.nbl",
	% broadcast unspawn to other people
	lists:foreach(fun(Other) -> Other#users.pid ! {psu_player_unspawn, User} end, egs_db:users_select_others_in_area(OldUser)),
	% load counter
	send_0c00(CSocket, GID, 16#7fffffff),
	send_020e(CSocket, QuestFile),
	send_0a05(CSocket, GID),
	% 010d
	send_0200(CSocket, GID, mission),
	send_020f(CSocket, ZoneFile, 0, 16#ff),
	send_0205(CSocket, 0, 0, 0, 0),
	send_100e(CSocket, GID, 16#7fffffff, 0, 0, AreaName, EntryID),
	send_0215(CSocket, GID, 0),
	send_0215(CSocket, GID, 0),
	send_020c(CSocket),
	send_1202(CSocket, GID),
	send_1204(CSocket, GID),
	send_1206(CSocket, GID),
	send_1207(CSocket, GID),
	send_1212(CSocket, GID),
	send_0201(CSocket, GID, User, Char),
	send_0a06(CSocket, GID),
	send_0208(CSocket, GID),
	send_0236(CSocket, GID).

%% @doc Return the current season information.

area_get_season(QuestID) ->
	{{_, Month, Day}, _} = calendar:universal_time(),
	[IsSeasonal, Season, SeasonQuestIDs] = if
		Month =:=  1, Day =< 14            -> ?SEASON_NEWYEAR;
		Month =:=  1, Day >= 25            -> ?SEASON_WINTER;
		Month =:=  2, Day =< 7             -> ?SEASON_WINTER;
		Month =:=  2, Day >= 14            -> ?SEASON_VALENTINE;
		Month =:=  3, Day =< 6             -> ?SEASON_VALENTINE;
		Month =:=  3, Day >= 14            -> ?SEASON_WHITEDAY;
		Month =:=  4, Day =< 3             -> ?SEASON_WHITEDAY;
		Month =:=  4, Day >= 4, Day =< 24  -> ?SEASON_EASTER;
		Month =:=  4, Day >= 25            -> ?SEASON_SPRING;
		Month =:=  5, Day =< 8             -> ?SEASON_SPRING;
		Month =:=  5, Day >= 17, Day =< 30 -> ?SEASON_WEDDING;
		Month =:=  6, Day >= 3, Day =< 16  -> ?SEASON_PARUMUNIF;
		Month =:=  6, Day >= 23            -> ?SEASON_SONIC;
		Month =:=  7, Day =< 13            -> ?SEASON_SONIC;
		Month =:=  7, Day >= 18            -> ?SEASON_HOLYLIGHT;
		Month =:=  8, Day =< 21            -> ?SEASON_FIREWORKS;
		Month =:=  8, Day >= 28            -> ?SEASON_NATIVE;
		Month =:=  9, Day =< 10            -> ?SEASON_NATIVE;
		Month =:=  9, Day >= 24            -> ?SEASON_AUTUMN;
		Month =:= 10, Day =< 7             -> ?SEASON_AUTUMN;
		Month =:= 10, Day >= 15, Day =< 28 -> ?SEASON_PARTY;
		Month =:= 10, Day >= 31            -> ?SEASON_HALLOWEEN;
		Month =:= 11, Day =< 20            -> ?SEASON_HALLOWEEN;
		Month =:= 12, Day >= 11            -> ?SEASON_CHRISTMAS;
		true                               -> ?SEASON_NONE
	end,
	if	IsSeasonal =:= 1 ->
			case lists:member(QuestID, SeasonQuestIDs) of
				true  -> [{status, IsSeasonal}, {season, Season}];
				false -> [{status, 0}, {season, 255}]
			end;
		true ->
			[{status, 0}, {season, 255}]
	end.

%% @doc Load the given map as a standard lobby.

area_load(CSocket, GID, QuestID, ZoneID, MapID, EntryID) ->
	OldUser = egs_db:users_select(GID),
	[{type, AreaType}, {file, QuestFile}|StartInfo] = proplists:get_value(QuestID, ?QUESTS, [{type, undefined}, {file, undefined}]),
	[IsStart, InstanceID, RealZoneID, RealMapID, RealEntryID] = case AreaType of
		mission ->
			if	ZoneID =:= 65535 ->
					[{start, [TmpZoneID, TmpMapID, TmpEntryID]}] = StartInfo,
					[true, GID, TmpZoneID, TmpMapID, TmpEntryID];
				true -> [false, GID, ZoneID, MapID, EntryID]
			end;
		myroom ->
			if	ZoneID =:= 0 ->
					[false, undefined, 0, 423, EntryID];
				true -> [false, undefined, ZoneID, MapID, EntryID]
			end;
		_ ->
			[false, undefined, ZoneID, MapID, EntryID]
	end,
	[{file, ZoneFile}] = proplists:get_value([QuestID, RealZoneID], ?ZONES, [{file, undefined}]),
	if	AreaType =:= myroom ->
			AreaName = "Your Room";
		true ->
			[{name, AreaName}] = proplists:get_value([QuestID, RealMapID], ?MAPS, [{name, "dammy"}])
	end,
	User = OldUser#users{instanceid=InstanceID, areatype=AreaType, questid=QuestID, zoneid=RealZoneID, mapid=RealMapID, entryid=RealEntryID},
	egs_db:users_insert(User),
	area_load(CSocket, GID, AreaType, IsStart, OldUser, User, QuestFile, ZoneFile, AreaName).

area_load(CSocket, GID, AreaType, IsStart, OldUser, User, QuestFile, ZoneFile, AreaName) ->
	[{status, 1}, {char, Char}, {options, Options}] = data_load(User#users.folder, User#users.charnumber),
	QuestChange = if OldUser#users.questid /= User#users.questid, QuestFile /= undefined -> true; true -> false end,
	if	ZoneFile =:= undefined ->
			ZoneChange = false;
		true ->
			ZoneChange = if OldUser#users.questid =:= User#users.questid, OldUser#users.zoneid =:= User#users.zoneid -> false; true -> true end
	end,
	[{status, IsSeasonal}, {season, Season}] = area_get_season(User#users.questid),
	% broadcast spawn and unspawn to other people
	lists:foreach(fun(Other) -> Other#users.pid ! {psu_player_unspawn, User} end, egs_db:users_select_others_in_area(OldUser)),
	if	AreaType =:= lobby ->
			lists:foreach(fun(Other) -> Other#users.pid ! {psu_player_spawn, User} end, egs_db:users_select_others_in_area(User));
		true -> ignore
	end,
	% load area
	if	QuestChange =:= true ->
			% reload the character if entering or leaving the room quest
			if	OldUser#users.questid =:= 1120000; User#users.questid =:= 1120000 ->
					char_load(CSocket, GID, Char, Options, User#users.charnumber);
				true -> ignore
			end,
			% load new quest
			send_0c00(CSocket, GID, User#users.questid),
			send_020e(CSocket, QuestFile);
		true -> ignore
	end,
	if	IsStart =:= true ->
			send_0215(CSocket, GID, 16#ffffffff);
		true -> ignore
	end,
	if	ZoneChange =:= true ->
			% load new zone
			send_0a05(CSocket, GID),
			if AreaType =:= lobby ->
					send_0111(CSocket, GID);
				true -> ignore
			end,
			% 010d
			send_0200(CSocket, GID, AreaType),
			Layout = if IsStart =:= true -> crypto:rand_uniform(0, 4);
				true -> 0
			end,
			send_020f(CSocket, ZoneFile, Layout, Season);
		true -> ignore
	end,
	send_0205(CSocket, User#users.zoneid, User#users.mapid, User#users.entryid, IsSeasonal),
	send_100e(CSocket, GID, User#users.questid, User#users.zoneid, User#users.mapid, AreaName, 16#ffffffff),
	if	AreaType =:= mission ->
			send_0215(CSocket, GID, 0),
			if	IsStart =:= true ->
					send_0215(CSocket, GID, 0),
					send_0c09(CSocket, GID);
				true -> ignore
			end;
		true ->
			send_020c(CSocket)
	end,
	case AreaType of
		myroom ->
			myroom_send_packet(CSocket, "p/packet1332.bin"),
			send_1202(CSocket, GID),
			send_1204(CSocket, GID),
			send_1206(CSocket, GID);
		mission ->
			send_1202(CSocket, GID),
			send_1204(CSocket, GID),
			send_1206(CSocket, GID),
			send_1207(CSocket, GID);
		_ -> ignore
	end,
	if	AreaType /= spaceport ->
			send_1212(CSocket, GID);
		true -> ignore
	end,
	if	AreaType =:= myroom ->
			myroom_send_packet(CSocket, "p/packet1309.bin");
		true -> ignore
	end,
	send_0201(CSocket, GID, User, Char),
	if	ZoneChange =:= true ->
			send_0a06(CSocket, GID);
		true -> ignore
	end,
	send_0233(CSocket, GID, egs_db:users_select_others_in_area(User)),
	send_0208(CSocket, GID),
	send_0236(CSocket, GID).

myroom_send_packet(CSocket, Filename) ->
	{ok, << _:32, File/bits >>} = file:read_file(Filename),
	egs_proto:packet_send(CSocket, File).

%% @doc Alias for the game main's loop when the buffer is empty.

loop(CSocket, GID, Version) ->
	loop(CSocket, GID, Version, << >>).

%% @doc Game's main loop.

loop(CSocket, GID, Version, SoFar) ->
	receive
		{psu_broadcast, Orig} ->
			<< A:64/bits, _:32, B:96/bits, _:64, C/bits >> = Orig,
			Packet = << A/binary, 16#00011300:32, B/binary, 16#00011300:32, GID:32/little-unsigned-integer, C/binary >>,
			egs_proto:packet_send(CSocket, Packet),
			?MODULE:loop(CSocket, GID, Version, SoFar);
		{psu_chat, ChatGID, ChatName, ChatModifiers, ChatMessage} ->
			send_0304(CSocket, Version, ChatGID, ChatName, ChatModifiers, ChatMessage),
			?MODULE:loop(CSocket, GID, Version, SoFar);
		{psu_keepalive} ->
			egs_proto:send_keepalive(CSocket),
			?MODULE:loop(CSocket, GID, Version, SoFar);
		{psu_player_spawn, Spawn} ->
			send_spawn(CSocket, GID, Spawn),
			?MODULE:loop(CSocket, GID, Version, SoFar);
		{psu_player_unspawn, Spawn} ->
			send_0204(CSocket, GID, Spawn#users.gid, Spawn#users.lid, 5),
			?MODULE:loop(CSocket, GID, Version, SoFar);
		{ssl, _, Data} ->
			{Packets, Rest} = egs_proto:packet_split(<< SoFar/bits, Data/bits >>),
			[dispatch(CSocket, GID, Version, Orig) || Orig <- Packets],
			?MODULE:loop(CSocket, GID, Version, Rest);
		{ssl_closed, _} ->
			exit(ssl_closed);
		{ssl_error, _, _} ->
			exit(ssl_error);
		_ ->
			?MODULE:loop(CSocket, GID, Version, SoFar)
	after 1000 ->
		reload,
		?MODULE:loop(CSocket, GID, Version, SoFar)
	end.

%% @doc Dispatch the command to the right handler.
%%      Command 0b05 uses the channel for something else. Conflicts could occur. Better to just ignore it anyway.

dispatch(CSocket, GID, Version, Orig) ->
	<< _:32, Command:16/unsigned-integer, Channel:8/little-unsigned-integer, _/bits >> = Orig,
	case [Command, Channel] of
		[16#0b05, _] ->
			ignore;
		[_, 1] ->
			broadcast(Command, GID, Orig);
		_ ->
			handle(Command, CSocket, GID, Version, Orig)
	end.

%% @doc Position change broadcast handler. Save the position and then dispatch it.

broadcast(16#0503, GID, Orig) ->
	<< 100:32/little-unsigned-integer, 16#050301:24/unsigned-integer, _:360, Direction:32/bits, Coords:96/bits, _:96,
		QuestID:32/little-unsigned-integer, ZoneID:32/little-unsigned-integer, MapID:32/little-unsigned-integer,
		EntryID:32/little-unsigned-integer, _:32 >> = Orig,
	User = egs_db:users_select(GID),
	NewUser = User#users{direction=Direction, coords=Coords, questid=QuestID, zoneid=ZoneID, mapid=MapID, entryid=EntryID},
	egs_db:users_insert(NewUser),
	broadcast(default, GID, Orig);

%% @doc Stand still broadcast handler. Save the position and then dispatch it.

broadcast(16#0514, GID, Orig) ->
	<< _:320, _:96, Direction:32/bits, Coords:96/bits, QuestID:32/little-unsigned-integer, ZoneID:32/little-unsigned-integer,
		MapID:32/little-unsigned-integer, EntryID:32/little-unsigned-integer, _/bits >> = Orig,
	User = egs_db:users_select(GID),
	NewUser = User#users{direction=Direction, coords=Coords, questid=QuestID, zoneid=ZoneID, mapid=MapID, entryid=EntryID},
	egs_db:users_insert(NewUser),
	broadcast(default, GID, Orig);

%% @doc Default broadcast handler. Dispatch the command to everyone.
%%      We clean up the command and use the real GID and LID of the user, disregarding what was sent and possibly tampered with.
%%      Only a handful of commands are allowed to broadcast. An user tampering with it would get disconnected instantly.
%% @todo Don't query the user data everytime! Keep an User instead of a GID probably.

broadcast(Command, GID, Orig)
	when	Command =:= 16#0101;
			Command =:= 16#0102;
			Command =:= 16#0104;
			Command =:= 16#0107;
			Command =:= 16#010f;
			Command =:= 16#050f;
			Command =:= default ->
	<< _:32, A:64/bits, _:64, B:192/bits, _:64, C/bits >> = Orig,
	case egs_db:users_select(GID) of
		error ->
			ignore;
		Self ->
			LID = Self#users.lid,
			Packet = << A/binary, 16#00011300:32, GID:32/little-unsigned-integer, B/binary,
				GID:32/little-unsigned-integer, LID:32/little-unsigned-integer, C/binary >>,
			lists:foreach(fun(User) -> User#users.pid ! {psu_broadcast, Packet} end, egs_db:users_select_others_in_area(Self))
	end.

%% @doc Movement (non-broadcast) handler. Do nothing.

handle(16#0102, _, _, _, _) ->
	ignore;

%% @doc Weapon equip, unequip, item drop, and more... handler. Do what we can.
%%      Melee uses a format similar to: AAAA--BBCCCC----DDDDDDDDEE----FF with
%%      AAAA the attack sound effect, BB the range, CCCC and DDDDDDDD unknown but related to angular range or similar, EE number of targets and FF the model.
%%      Bullets and tech weapons formats are unknown but likely use a slightly different format.
%% @todo Others probably want to see that you changed your weapon.
%% @todo Apparently B is always ItemID+1. Not sure why.
%% @todo Currently use a separate file for the data sent for the weapons.

handle(16#0105, CSocket, GID, _, Orig) ->
	<< _:384, A:32/little-unsigned-integer, ItemID:8, Action:8, _:8, B:8, C:32/little-unsigned-integer, _/bits >> = Orig,
	log(GID, "0105 action ~b item ~b (~b ~b ~b)", [Action, ItemID, A, B, C]),
	Category = case ItemID of
		% units would be 8, traps would be 12
		19 -> 2; % armor
		Y when Y =:= 5; Y =:= 6; Y =:= 7 -> 0; % clothes
		_ -> 1 % weapons
	end,
	case Action of
		1 -> % equip item
			Filename = case ItemID of
				% weapons
				16 -> "p/packet0105_sword.bin";
				13 -> "p/packet0105_twindaggers.bin";
				15 -> "p/packet0105_dagger.bin";
				 9 -> "p/packet0105_rcsm.bin";
				14 -> "p/packet0105_saber.bin";
				 8 -> "p/packet0105_mgun.bin";
				X when X =:= 17; X =:= 18 ->
					"p/packet0105_twinguns.bin";
				% armor
				19 -> "p/packet0105_armor.bin";
				% clothes
				X when X =:= 5; X =:= 6; X =:= 7 ->
					none;
				_ -> % default: do nothing
					none
			end,
			case Filename of
				none -> File = << >>;
				_ -> {ok, File} = file:read_file(Filename)
			end,
			Packet = << 16#01050300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer,
				0:64, GID:32/little-unsigned-integer, A:32/little-unsigned-integer, ItemID, Action, Category, B, C:32/little-unsigned-integer,
				File/binary >>,
			egs_proto:packet_send(CSocket, Packet);
		2 -> % unequip item
			Packet = << 16#01050300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer,
				0:64, GID:32/little-unsigned-integer, A:32/little-unsigned-integer, ItemID, Action, Category, B, C:32/little-unsigned-integer >>,
			egs_proto:packet_send(CSocket, Packet);
		5 -> % drop item
			ignore;
		_ ->
			ignore
	end;

%% @doc Shop listing request. Currently return the normal item shop for everything.
%% @todo Return the other shops appropriately.

handle(16#010a, CSocket, GID, _, Orig) ->
	<< _:384, A:32/little-unsigned-integer, B:32/little-unsigned-integer, C:32/little-unsigned-integer >> = Orig,
	log(GID, "shop listing request (~b, ~b, ~b)", [A, B, C]),
	{ok, File} = file:read_file("p/itemshop.bin"),
	Packet = << 16#010a0300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32,
		GID:32/little-unsigned-integer, 0:64, GID:32/little-unsigned-integer, 0:32, File/binary >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc Character death, and more, handler. Warp to 4th floor for now.
%% @todo Recover from death correctly.

handle(16#0110, CSocket, GID, _, Orig) ->
	<< _:384, A:32/little-unsigned-integer, B:32/little-unsigned-integer, C:32/little-unsigned-integer >> = Orig,
	case B of
		2 -> % triggered when looking at the type menu
			send_0113(CSocket, GID);
		3 -> % type change
			log(GID, "changed type to ~b", [C]);
		7 -> % player death
			area_load(CSocket, GID, 1100000, 0, 4, 6);
		10 -> % online status change
			log(GID, "changed status to ~b", [C]);
		_ ->
			log(GID, "unknown 0110 (~b, ~b, ~b)", [A, B, C])
	end;

%% @doc Uni cube handler.

handle(16#021d, CSocket, _, _, _) ->
	send_021e(CSocket);

%% @doc Uni selection handler.
%%      When selecting 'Your room', load a default room.
%%      When selecting 'Reload', reload the character in the current lobby.
%% @todo There's probably an entryid given in the uni selection packet.

handle(16#021f, CSocket, GID, _, Orig) ->
	<< _:352, Uni:32/little-unsigned-integer, _/bits >> = Orig,
	case Uni of
		0 -> % cancelled uni selection
			ignore;
		16#ffffffff ->
			log(GID, "uni selection (my room)"),
			send_0230(CSocket, GID),
			% 0220
			area_load(CSocket, GID, 1120000, 0, 100, 0);
		_ ->
			log(GID, "uni selection (reload)"),
			send_0230(CSocket, GID),
			% 0220
			% force reloading the character and data files (hack)
			User = egs_db:users_select(GID),
			NewRow = User#users{questid=1120000, zoneid=undefined},
			egs_db:users_insert(NewRow),
			area_load(CSocket, GID, User#users.questid, User#users.zoneid, User#users.mapid, User#users.entryid)
	end;

%% @doc Shortcut changes handler. Do nothing.
%% @todo Save it.

handle(16#0302, _, GID, _, _) ->
	log(GID, "dismissed shortcut changes");

%% @doc Chat broadcast handler. Dispatch the message to everyone (for now).
%%      We must take extra precautions to handle different versions of the game correctly.
%%      Disregard the name sent by the server in later versions of the game. Use the name saved in memory instead, to prevent client-side editing.
%% @todo Only broadcast to people in the same map.

handle(16#0304, _, GID, Version, Orig) ->
	User = egs_db:users_select(GID),
	case Version of
		0 -> % AOTI v2.000
			<< _:416, Modifiers:128/bits, Message/bits >> = Orig;
		_ -> % Above
			<< _:416, Modifiers:128/bits, _:512, Message/bits >> = Orig
	end,
	[LogName|_] = re:split(User#users.charname, "\\0\\0", [{return, binary}]),
	[TmpMessage|_] = re:split(Message, "\\0\\0", [{return, binary}]),
	LogMessage = re:replace(TmpMessage, "\\n", " ", [global, {return, binary}]),
	log(GID, "chat from ~s: ~s", [[re:replace(LogName, "\\0", "", [global, {return, binary}])], [re:replace(LogMessage, "\\0", "", [global, {return, binary}])]]),
	lists:foreach(fun(X) -> X#users.pid ! {psu_chat, GID, User#users.charname, Modifiers, Message} end, egs_db:users_select_all());

%% @todo Handle this packet properly.
%% @todo Spawn cleared response event shouldn't be handled following this packet but when we see the spawn actually dead HP-wise.

handle(16#0402, CSocket, GID, _, Orig) ->
	<< _:352, SpawnID:32/little-unsigned-integer, _:64, Type:32/little-unsigned-integer, _:64 >> = Orig,
	case Type of
		7 -> % spawn cleared @todo 1201 sent back with same values apparently, but not always
			if	SpawnID =:= 70 ->
					send_1205(CSocket, GID, 53, 0);
				SpawnID =:= 100 ->
					send_1205(CSocket, GID, 55, 0);
				true ->
					ignore
			end;
		_ ->
			ignore
	end;

%% @todo Handle this packet.

handle(16#0404, CSocket, GID, _, Orig) ->
	<< _:352, A:32/little-unsigned-integer, B:32/little-unsigned-integer >> = Orig,
	log(GID, "unknown command 0404: ~b ~b", [A, B]),
	send_1205(CSocket, GID, A, B);

%% @doc Map change handler.
%%      Rooms are handled differently than normal lobbies.
%% @todo When changing lobby to the room, 0230 must also be sent. Same when going from room to lobby.

handle(16#0807, CSocket, GID, _, Orig) ->
	<< _:352, QuestID:32/little-unsigned-integer, ZoneID:16/little-unsigned-integer,
		MapID:16/little-unsigned-integer, EntryID:16/little-unsigned-integer, _/bits >> = Orig,
	log(GID, "map change (~b,~b,~b,~b)", [QuestID, ZoneID, MapID, EntryID]),
	area_load(CSocket, GID, QuestID, ZoneID, MapID, EntryID);

%% @doc Mission counter handler.

handle(16#0811, CSocket, GID, _, Orig) ->
	<< _:352, QuestID:32/little-unsigned-integer, ZoneID:16/little-unsigned-integer,
		MapID:16/little-unsigned-integer, EntryID:16/little-unsigned-integer, _/bits >> = Orig,
	log(GID, "mission counter (~b,~b,~b,~b)", [QuestID, ZoneID, MapID, EntryID]),
	counter_load(CSocket, GID, QuestID, ZoneID, MapID, EntryID);

%% @doc Leave mission counter handler. Lobby values depend on which counter was entered.

handle(16#0812, CSocket, GID, _, _) ->
	User = egs_db:users_select(GID),
	area_load(CSocket, GID, User#users.savedquestid, User#users.savedzoneid, User#users.zoneid, User#users.mapid);

%% @doc Item description request.
%% @todo Send something other than just "dammy".

handle(16#0a10, CSocket, GID, _, Orig) ->
	<< _:352, ItemID:32/unsigned-integer >> = Orig,
	send_0a11(CSocket, GID, ItemID, "dammy");

%% @doc Start mission handler.
%% @todo Forward the mission start to other players of the same party, whatever their location is.

handle(16#0c01, CSocket, GID, _, Orig) ->
	<< _:352, QuestID:32/little-unsigned-integer >> = Orig,
	log(GID, "start mission ~b", [QuestID]),
	send_170c(CSocket, GID),
	send_1020(CSocket, GID),
	send_1015(CSocket, GID, QuestID),
	send_0c02(CSocket, GID);

%% @doc Counter quests files request handler? Send huge number of quest files.
%% @todo Handle correctly.

handle(16#0c05, CSocket, GID, _, _) ->
	User = egs_db:users_select(GID),
	[{quests, Filename}, {bg, _}, {options, _}] = proplists:get_value(User#users.entryid, ?COUNTERS),
	send_0c06(CSocket, Filename);

%% @doc Lobby transport handler? Just ignore the meseta price for now and send the player where he wanna be!
%% @todo Handle correctly.

handle(16#0c07, CSocket, GID, _, _) ->
	send_0c08(CSocket, GID, ok);

%% @doc Abort mission handler.

handle(16#0c0e, CSocket, GID, _, _) ->
	send_1006(CSocket, GID, 11),
	User = egs_db:users_select(GID),
	if	User#users.areatype =:= mission ->
			area_load(CSocket, GID, User#users.savedquestid, User#users.savedzoneid, User#users.savedmapid, User#users.savedentryid);
		true -> ignore
	end;

%% @doc Counter available mission list request handler.
%% @todo Temporarily allow rare mission and LL all difficulties to all players.

handle(16#0c0f, CSocket, GID, _, _) ->
	User = egs_db:users_select(GID),
	[{quests, _}, {bg, _}, {options, Options}] = proplists:get_value(User#users.entryid, ?COUNTERS),
	send_0c10(CSocket, GID, Options);

%% @doc Set flag handler. Associate a new flag with the character.
%%      Just reply with a success value for now.
%% @todo God save the flags.

handle(16#0d04, CSocket, GID, _, Orig) ->
	<< _:352, Flag:128/bits, A:16/bits, _:8, B/bits >> = Orig,
	log(GID, "flag handler for ~s", [re:replace(Flag, "\\0+", "", [global, {return, binary}])]),
	Packet = << 16#0d040300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Flag/binary, A/binary, 1, B/binary >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc Options changes handler.

handle(16#0d07, _, GID, _, Orig) ->
	log(GID, "options changes"),
	<< _:352, Options/bits >> = Orig,
	User = egs_db:users_select(GID),
	file:write_file(io_lib:format("save/~s/~b-character.options", [User#users.folder, User#users.charnumber]), Options);

%% @doc Hit handler.
%% @todo Finish the work on it.
%% @todo First value at 2C is the number of hits. We don't need to know it though.

handle(16#0e00, CSocket, GID, _, Orig) ->
	<< _:448, Data/bits >> = Orig,
	handle_hits(CSocket, GID, Data);

%% @doc Object event handler.
%% @todo Handle all events appropriately.

handle(16#0f0a, CSocket, GID, _, Orig) ->
	<< _:448, A:32/little-unsigned-integer, _:64, B:32/little-unsigned-integer, _:272, Action:8, _/bits >> = Orig,
	case Action of
		0 -> % warp
			ignore;
		3 -> % crystal activation
			ignore;
		12 -> % key
			% it's more than one 0f0a event actually... @todo hack
			send_1205(CSocket, GID, 215, 0),
			send_1213(CSocket, GID, 8, 1),
			send_1205(CSocket, GID, 202, 0);
		13 -> % button on
			ignore;
		14 -> % button off
			ignore;
		%~ 19 -> % @todo (somewhere in phantom ruins block 4)
			%~ ignore;
		20 -> % enter counter/elevator/room/spaceport
			ignore;
		23 -> % key door activation (no key)
			ignore;
		24 -> % key activation (has key)
			% it's more than one 0f0a event actually... @todo hack
			send_1205(CSocket, GID, 244, 0),
			send_1205(CSocket, GID, 54, 0),
			send_1213(CSocket, GID, 0, 1);
		25 -> % sit on chair
			send_1211(CSocket, GID, A, B, 8, 0);
		26 -> % sit out of chair
			send_1211(CSocket, GID, A, B, 8, 2);
		%~ 30 -> % @todo (phantom ruins block 4)
			%~ ignore;
		_ ->
			log(GID, "object event ~b", [Action])
	end;

%% @doc Party information recap request.
%% @todo Handle when the party already exists! And stop doing it wrong.

handle(16#1705, CSocket, GID, _, _) ->
	User = egs_db:users_select(GID),
	send_1706(CSocket, GID, User#users.charname);

%% @doc Mission selected handler. Send the currently selected mission.
%% @todo Probably need to dispatch that info to other party members in the same counter.

handle(16#1707, _, _, _, _) ->
	ignore;

%% @doc Party settings request handler. Item distribution is random for now.
%% @todo Handle correctly.

handle(16#1709, CSocket, GID, _, _) ->
	send_170a(CSocket, GID);

%% @doc Counter-related handler.

handle(16#170b, CSocket, GID, _, _) ->
	send_170c(CSocket, GID);

%% @doc Counter initialization handler? Send the code for the background image to use.
%% @todo Handle correctly.

handle(16#1710, CSocket, GID, _, _) ->
	User = egs_db:users_select(GID),
	[{quests, _}, {bg, Background}, {options, _}] = proplists:get_value(User#users.entryid, ?COUNTERS),
	send_1711(CSocket, GID, Background);

%% @doc Dialog request handler. Do what we can.
%% @todo Handle correctly.

handle(16#1a01, CSocket, GID, _, Orig) ->
	<< _:384, A:8, B:8, _:16, C:8, _/bits >> = Orig,
	case [A, B, C] of
		[ 0, 0, 2] ->
			log(GID, "lumilass (and more?)"),
			send_1a03(CSocket, GID);
		[ 0, 0, 3] ->
			log(GID, "pp cube"),
			send_1a04(CSocket, GID);
		[ 0, 0, 9] ->
			log(GID, "types menu"),
			send_1a07(CSocket, GID);
		[80, 0, _] ->
			log(GID, "npc dialog choice"),
			send_1a02(CSocket, GID, 0, 17, 17, 3, 9);
		[90, 0, _] -> % All the replies from here are consistent but their effect is unknown.
			log(GID, "1a01 unknown (~b ~b ~b)", [A, B, C]),
			send_1a02(CSocket, GID, 0, 5, 1, 4, 5);
		[91, 0, _] ->
			log(GID, "1a01 unknown (~b ~b ~b)", [A, B, C]),
			send_1a02(CSocket, GID, 0, 5, 5, 4, 7);
		[92, 0, _] ->
			log(GID, "1a01 unknown (~b ~b ~b)", [A, B, C]),
			send_1a02(CSocket, GID, 0, 5, 0, 4, 0);
		[93, 0, _] ->
			log(GID, "1a01 unknown (~b ~b ~b)", [A, B, C]),
			send_1a02(CSocket, GID, 0, 5, 18, 4, 0);
		[ _, 2, _] ->
			log(GID, "1a01 unknown (~b ~b ~b)", [A, B, C]),
			send_1a02(CSocket, GID, 0, 0, 1, 0, 0);
		_ ->
			log(GID, "1a01 unknown (~b ~b ~b) - do nothing", [A, B, C])
	end;

%% @doc Unknown command handler. Do nothing.

handle(Command, _, GID, _, _) ->
	log(GID, "dismissed packet ~4.16.0b", [Command]).

%% @doc Handle all hits received.
%% @todo Finish the work on it.

%~ log_hits(GID, Data) ->
	%~ <<	A:32/unsigned-integer, B:32/unsigned-integer, C:32/unsigned-integer, D:32/unsigned-integer,
		%~ E:32/unsigned-integer, F:32/unsigned-integer, G:32/unsigned-integer, H:32/unsigned-integer,
		%~ I:32/unsigned-integer, J:32/unsigned-integer, K:32/unsigned-integer, L:32/unsigned-integer,
		%~ M:32/unsigned-integer, N:32/unsigned-integer, O:32/unsigned-integer, P:32/unsigned-integer,
		%~ Q:32/unsigned-integer, R:32/unsigned-integer, S:32/unsigned-integer, T:32/unsigned-integer, _/bits >> = Data,
	%~ log(GID, "hit!~n    ~8.16.0b ~8.16.0b ~8.16.0b ~8.16.0b~n    ~8.16.0b ~8.16.0b ~8.16.0b ~8.16.0b~n    ~8.16.0b ~8.16.0b ~8.16.0b ~8.16.0b~n    ~8.16.0b ~8.16.0b ~8.16.0b ~8.16.0b~n    ~8.16.0b ~8.16.0b ~8.16.0b ~8.16.0b", [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]).

handle_hits(_, _, << >>) ->
	ok;
handle_hits(CSocket, GID, Data) ->
	%~ log_hits(GID, Data),
	<< A:224/bits, B:128/bits, _:288/bits, Rest/bits >> = Data,
	PlayerHP = 4401,
	TargetHP = 0,
	Damage = 58008,
	Packet = << 16#0e070300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		1:32/little-unsigned-integer, 16#01050000:32, Damage:32/little-unsigned-integer,
		A/binary, 0:64, PlayerHP:32/little-unsigned-integer, 0:32, 16#01000200:32,
		0:32, TargetHP:32, 0:32, B/binary, 16#04320000:32, 16#80000000:32, 16#26030000:32, 16#89068d00:32, 16#0c1c0105:32 >>,
	egs_proto:packet_send(CSocket, Packet),
	handle_hits(CSocket, GID, Rest).

%% @todo Possibly related to 010d. Just send seemingly safe values.

send_0111(CSocket, GID) ->
	Packet = << 16#01110300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		GID:32/little-unsigned-integer, 0:32, 6:32/little-unsigned-integer, 0:32 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Types capability list.

send_0113(CSocket, GID) ->
	{ok, File} = file:read_file("p/typesinfo.bin"),
	Packet = << 16#01130300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, GID:32/little-unsigned-integer, File/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the zone initialization notification.

send_0200(CSocket, GID, ZoneType) ->
	case ZoneType of
		mission ->
			Var = << 16#06000500:32, 16#01000000:32, 0:64, 16#00040000:32, 16#00010000:32, 16#00140000:32 >>;
		myroom ->
			Var = << 16#06000000:32, 16#02000000:32, 0:64, 16#40000000:32, 16#00010000:32, 16#00010000:32 >>;
		_ ->
			Var = << 16#00040000:32, 0:160, 16#00140000:32 >>
	end,
	Packet = << 16#02000300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96, 16#01000000:32,
		16#ffffffff:32, Var/binary, 16#ffffffff:32, 16#ffffffff:32 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out what the other things are.

send_0201(CSocket, GID, User, Char) ->
	QuestID = User#users.questid,
	ZoneID = User#users.zoneid,
	MapID = User#users.mapid,
	EntryID = User#users.entryid,
	CharGID = User#users.gid,
	CharLID = User#users.lid,
	{ok, File} = file:read_file("p/packet0201.bin"),
	<< _:96, A:32/bits, _:96, B:32/bits, _:256, D:32/bits, _:2656, After/bits >> = File,
	Packet = << 16#02010300:32, 0:32, A/binary, CharGID:32/little-unsigned-integer, 0:64, B/binary, GID:32/little-unsigned-integer,
		0:64, CharLID:32/little-unsigned-integer, CharGID:32/little-unsigned-integer, 0:96, D/binary, QuestID:32/little-unsigned-integer,
		ZoneID:32/little-unsigned-integer, MapID:32/little-unsigned-integer, EntryID:32/little-unsigned-integer, 0:192, QuestID:32/little-unsigned-integer,
		ZoneID:32/little-unsigned-integer, MapID:32/little-unsigned-integer, EntryID:32/little-unsigned-integer, Char/binary, After/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Hello packet, always sent on client connection.

send_0202(CSocket) ->
	egs_proto:packet_send(CSocket, << 16#02020300:32, 0:352 >>).

%% @todo Not sure. Used for unspawning, and more.

send_0204(CSocket, GID, PlayerGID, PlayerLID, Action) ->
	Packet = << 16#02040300:32, 0:32, 16#00001200:32, PlayerGID:32/little-unsigned-integer, 0:64,
		16#00011300:32, GID:32/little-unsigned-integer, 0:64, PlayerGID:32/little-unsigned-integer,
		PlayerLID:32/little-unsigned-integer, Action:32/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the map ID to be loaded by the client.
%% @todo Last two values are unknown.

send_0205(CSocket, MapType, MapNumber, MapEntry, IsSeasonal) ->
	Packet = << 16#02050300:32, 0:288, 16#ffffffff:32, MapType:32/little-unsigned-integer,
		MapNumber:32/little-unsigned-integer, MapEntry:32/little-unsigned-integer, 0:56, IsSeasonal >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Indicate to the client that loading should finish.
%% @todo Last value seems to be 2 most of the time. Never 0 though. Apparently counters have it at 4.

send_0208(CSocket, GID) ->
	Packet = << 16#02080300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 2:32/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo No idea what this one does. For unknown reasons it uses channel 2.

send_020c(CSocket) ->
	Packet = << 16#020c020c:32, 16#fffff20c:32, 0:256 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the quest file to be loaded.
%% @todo Probably should try sending the checksum like value (right before the file) and see if it magically fixes anything.

send_020e(CSocket, Filename) ->
	{ok, File} = file:read_file(Filename),
	Size = byte_size(File),
	Packet = << 16#020e0300:32, 0:288, Size:32/little-unsigned-integer, 0:32, File/binary, 0:32 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the zone file to be loaded.

send_020f(CSocket, Filename, Layout, Season) ->
	{ok, File} = file:read_file(Filename),
	Size = byte_size(File),
	Packet = << 16#020f0300:32, 0:288, Layout, Season, 0:16, Size:32/little-unsigned-integer, File/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo No idea what this do. Nor why it's sent twice when loading a counter.

send_0215(CSocket, GID, N) ->
	Packet = << 16#02150300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, N:32/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo End of character loading. Just send it.

send_021b(CSocket, GID) ->
	Packet = << 16#021b0300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the list of available universes.

send_021e(CSocket) ->
	{ok, << File:1184/bits, _/bits >>} = file:read_file("p/unicube.bin"),
	[StrCount] = io_lib:format("~b", [egs_db:users_count()]),
	UCS2Count = << << X:8, 0:8 >> || X <- StrCount >>,
	PaddingSize = (12 - byte_size(UCS2Count)) * 8,
	Packet = << 16#021e0300:32, 0:288, File/binary, UCS2Count/binary, 0:PaddingSize >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the current universe name and number.
%% @todo Currently only have universe number 2, named EGS Test.

send_0222(CSocket, GID) ->
	Packet = << 16#02220300:32, 0:32, 16#00001200:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		2:32/little-unsigned-integer, 0:32, 16#45, 0, 16#47, 0, 16#53, 0, 16#20, 0, 16#54, 0, 16#65, 0, 16#73, 0, 16#74, 0:24 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Not sure. Sent when going to or from room.

send_0230(CSocket, GID) ->
	Packet = << 16#02300300:32, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out what the other things are.

send_0233(CSocket, GID, Users) ->
	NbUsers = length(Users),
	case NbUsers of
		0 ->
			ignore;
		_ ->
			Header = << 16#02330300:32, 0:32, 16#00001200:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32,
				GID:32/little-unsigned-integer, 0:64, NbUsers:32/little-unsigned-integer >>,
			Contents = build_0233_contents(Users),
			Packet = << Header/binary, Contents/binary >>,
			egs_proto:packet_send(CSocket, Packet)
	end.

build_0233_contents([]) ->
	<< >>;
build_0233_contents(Users) ->
	[User|Rest] = Users,
	{ok, File} = file:read_file("p/player.bin"),
	<< A:32/bits, _:32, B:64/bits, _:32, C:32/bits, _:256, E:64/bits, _:2336, F/bits >> = File,
	{ok, CharFile} = file:read_file(io_lib:format("save/~s/~b-character", [User#users.folder, User#users.charnumber])),
	CharGID = User#users.gid,
	LID = User#users.lid,
	% TODO: temporary? undefined handling
	#users{direction=Direction, coords=Coords, questid=QuestID, zoneid=ZoneID, mapid=MapID, entryid=EntryID} = case User#users.coords of
		undefined -> #users{direction= << 0:32 >>, coords= << 0:96 >>, questid=1100000, zoneid=0, mapid=1, entryid=0};
		_ -> User
	end,
	Chunk = << A/binary, CharGID:32/little-unsigned-integer, B/binary, LID:16/little-unsigned-integer, 16#0100:16, C/binary,
		QuestID:32/little-unsigned-integer, ZoneID:32/little-unsigned-integer, MapID:32/little-unsigned-integer, EntryID:32/little-unsigned-integer,
		Direction:32/bits, Coords:96/bits, E/binary, QuestID:32/little-unsigned-integer, ZoneID:32/little-unsigned-integer, MapID:32/little-unsigned-integer,
		EntryID:32/little-unsigned-integer, CharFile/binary, F/binary >>,
	Next = build_0233_contents(Rest),
	<< Chunk/binary, Next/binary >>.

%% @doc Center the camera on the player, if possible.
%% @todo Probably.

send_0236(CSocket, GID) ->
	Packet = << 16#02360300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send a chat command. AOTI v2.000 version of the command.

send_0304(CSocket, 0, FromGID, _, Modifiers, Message) ->
	Packet = << 16#03040300:32/unsigned-integer, 0:288, 16#00001200:32, FromGID:32/little-unsigned-integer, Modifiers:128/bits, Message/bits >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc Send a chat command. AOTI since an unknown version of the game.

send_0304(CSocket, _, FromGID, FromName, Modifiers, Message) ->
	Packet = << 16#03040300:32, 0:288, 16#00001200:32, FromGID:32/little-unsigned-integer, Modifiers:128/bits, FromName:512/bits, Message/bits >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Inventory related. No idea what it does.

send_0a05(CSocket, GID) ->
	Packet = << 16#0a050300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Inventory related. Figure out everything in this packet and handle it correctly.
%% @todo It sends 60 values so it's probably some kind of options for all 60 items in the inventory?

send_0a06(CSocket, GID) ->
	{ok, << _:32, A:96/bits, _:32, B:96/bits, _:32, C:1440/bits, _:32, D/bits >>} = file:read_file("p/packet0a06.bin"),
	egs_proto:packet_send(CSocket, << A/binary, GID:32/little-unsigned-integer, B/binary, GID:32/little-unsigned-integer, C/binary, GID:32/little-unsigned-integer, D/binary >>).

%% @todo Inventory. Figure out everything in this packet and handle it correctly.

send_0a0a(CSocket, GID) ->
	{ok, << _:32, A:224/bits, _:32, B/bits >>} = file:read_file("p/packet0a0a.bin"),
	egs_proto:packet_send(CSocket, << A/binary, GID:32/little-unsigned-integer, B/binary >>).

%% @doc Item description.

send_0a11(CSocket, GID, ItemID, ItemDesc) ->
	Size = 1 + length(ItemDesc),
	UCS2Desc = << << X:8, 0:8 >> || X <- ItemDesc >>,
	Packet = << 16#0a110300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		ItemID:32/unsigned-integer, Size:32/little-unsigned-integer, UCS2Desc/binary, 0:16 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Init quest.

send_0c00(CSocket, GID, QuestID) ->
	Packet = << 16#0c000300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, QuestID:32/little-unsigned-integer,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out last 4 bytes!

send_0c02(CSocket, GID) ->
	Packet = << 16#0c020300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the huge pack of quest files available in the counter.

send_0c06(CSocket, Filename) ->
	{ok, << File/bits >>} = file:read_file(Filename),
	Packet = << 16#0c060300:32, 0:288, 1:32/little-unsigned-integer, File/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Reply whether the player is allowed to use the transport option.
%%      Use 'ok' for allowing it, and 'error' otherwise.

send_0c08(CSocket, GID, Response) ->
	Value = if Response =:= ok -> 0; true -> 1 end,
	Packet = << 16#0c080300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Value:32 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the trial start notification.

send_0c09(CSocket, GID) ->
	Packet = << 16#0c090300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:128 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the counter's mission options (0 = invisible, 2 = disabled, 3 = available).

send_0c10(CSocket, GID, Options) ->
	Packet = << 16#0c100300:32, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		16#00011300:32, GID:32/little-unsigned-integer, 0:64, Options/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the data for the selected character.
%% @todo The large chunk of 0s can have some values set... but what are they used for?
%% @todo The values after the Char variable are the flags. Probably use bits to define what flag is and isn't set. Handle correctly.

send_0d01(CSocket, GID, Char, Options) ->
	Packet = << 16#0d010300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Char/binary,
		16#ffbbef1c:32, 16#f8ff0700:32, 16#fc810916:32, 16#7802134c:32,
		16#b0c0040f:32, 16#7cf0e583:32, 16#b7bce0c6:32, 16#7ff8f963:32,
		16#3fd7ffff:32, 16#fff7ffff:32, 16#f3ff63e0:32, 16#1fe00000:32,
		0:7744, Options/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the character list for selection.

send_0d03(CSocket, GID, Data0, Data1, Data2, Data3) ->
	[{status, Status0}, {char, Char0}|_] = Data0,
	[{status, Status1}, {char, Char1}|_] = Data1,
	[{status, Status2}, {char, Char2}|_] = Data2,
	[{status, Status3}, {char, Char3}|_] = Data3,
	Packet = << 16#0d030300:32/unsigned-integer, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:104,
		Status0:8/unsigned-integer, 0:48, Char0/binary, 0:520,
		Status1:8/unsigned-integer, 0:48, Char1/binary, 0:520,
		Status2:8/unsigned-integer, 0:48, Char2/binary, 0:520,
		Status3:8/unsigned-integer, 0:48, Char3/binary, 0:512 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the character flags list. This is the whole list of available values, not the character's.
%%      Sent without fragmentation on official for unknown reasons. Do the same here.

send_0d05(CSocket, GID) ->
	{ok, Flags} = file:read_file("p/flags.bin"),
	Packet = << 16#0d050300:32, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Flags/binary >>,
	Size = 4 + byte_size(Packet),
	ssl:send(CSocket, << Size:32/little-unsigned-integer, Packet/binary >>).

%% @todo Figure out what the packet is.

send_1005(CSocket, GID, Char) ->
	{ok, File} = file:read_file("p/packet1005.bin"),
	<< _:352, Before:160/bits, _:608, After/bits >> = File,
	<< Name:512/bits, _/bits >> = Char,
	Packet = << 16#10050300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Before/binary, GID:32/little-unsigned-integer, 0:64, Name/binary, After/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Party-related command probably controlling the party state.
%%      Value 11 aborts the mission.
%% @todo Figure out what the packet is.

send_1006(CSocket, GID, N) ->
	Packet = << 16#10060300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, N:32/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the player's current location.

send_100e(CSocket, GID, QuestID, ZoneID, MapID, Location, CounterID) ->
	UCS2Location = << << X:8, 0:8 >> || X <- Location >>,
	Packet = << 16#100e0300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		1:32/little-unsigned-integer, MapID:16/little-unsigned-integer, ZoneID:16/little-unsigned-integer,
		QuestID:32/little-unsigned-integer, UCS2Location/binary >>,
	PaddingSize = (128 - byte_size(Packet) - 8) * 8,
	case CounterID of
		16#ffffffff ->
			Footer = << CounterID:32/little-unsigned-integer, 0:32 >>;
		_ ->
			Footer = << CounterID:32/little-unsigned-integer, 1:32/little-unsigned-integer >>
	end,
	egs_proto:packet_send(CSocket, << Packet/binary, 0:PaddingSize, Footer/binary >>).

%% @doc Send the mission's quest file when starting a new mission.
%% @todo Handle correctly. 0:32 is actually a missing value. Value before that is unknown too.

send_1015(CSocket, GID, QuestID) ->
	[{type, _}, {file, QuestFile}|_] = proplists:get_value(QuestID, ?QUESTS),
	{ok, File} = file:read_file(QuestFile),
	Size = byte_size(File),
	Packet = << 16#10150300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		QuestID:32/little-unsigned-integer, 16#01010000:32, 0:32, Size:32/little-unsigned-integer, File/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Totally unknown.

send_1020(CSocket, GID) ->
	Packet = << 16#10200300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.

send_1202(CSocket, GID) ->
	Packet = << 16#12020300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96, 16#10000000:32, 0:64, 16#14000000:32, 0:32 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out what this packet does. Seems it's the same values all the time.

send_1204(CSocket, GID) ->
	Packet = << 16#12040300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96, 16#20000000:32, 0:256 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Object events response?
%% @todo Figure things out.

send_1205(CSocket, GID, A, B) ->
	Packet = << 16#12050300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, A:32/little-unsigned-integer, B:32/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.

send_1206(CSocket, GID) ->
	Packet = << 16#12060300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96, 16#80020000:32, 0:5120 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out what this packet does. Sane values for counter and missions for now.

send_1207(CSocket, GID) ->
	Chunk = << 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 0:224, 16#0000ffff:32, 16#ff000000:32, 16#64000a00:32 >>,
	Packet = << 16#12070300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary, Chunk/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Object interaction? Figure out. C probably the interaction type.

send_1211(CSocket, GID, A, B, C, D) ->
	Packet = << 16#12110300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, A:32/little-unsigned-integer, B:32/little-unsigned-integer, C:32/little-unsigned-integer, D:32/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Make the client load the quest previously sent.

send_1212(CSocket, GID) ->
	Packet = << 16#12120300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:19264 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Not sure. Related to keys.

send_1213(CSocket, GID, A, B) ->
	Packet = << 16#12120300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, A:32/little-unsigned-integer, B:32/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the player's partner card.

send_1500(CSocket, GID, Char, Number) ->
	<< CharInfo:576/bits, _/bits >> = Char,
	Packet = << 16#15000300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, CharInfo/binary, 0:3072, 16#010401:24, Number:8, 0:64 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Send an empty partner card list.

send_1501(CSocket, GID) ->
	Packet = << 16#15010300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Send an empty blacklist.

send_1512(CSocket, GID) ->
	Packet = << 16#15120300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:46144 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the player's NPC and PM information.

send_1602(CSocket, GID) ->
	{ok, File} = file:read_file("p/npc.bin"),
	Packet = << 16#16020300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96, File/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Party information.
%% @todo Handle existing parties.

send_1706(CSocket, GID, CharName) ->
	Packet = << 16#17060300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		16#00000300:32, 16#d5c0faff:32, 0:64, CharName/binary, 16#78000000:32, 16#01010000:32,
		0:1536, 16#0100c800:32, 16#0601010a:32, 16#ffffffff:32, 0:32 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Party settings. Item distribution is random for now.
%% @todo Handle correctly.

send_170a(CSocket, GID) ->
	Packet = << 16#170a0300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#01010c08:32 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Find what the heck this packet is.

send_170c(CSocket, GID) ->
	{ok, File} = file:read_file("p/packet170c.bin"),
	Packet = << 16#170c0300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, File/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Send the background to use for the counter.
%% @todo Background has more info past the first byte.

send_1711(CSocket, GID, Background) ->
	Packet = << 16#17110300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Background:32/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Unknown dialog-related handler.
%% @todo Everything!

send_1a02(CSocket, GID, A, B, C, D, E) ->
	Packet = << 16#1a020300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, A:32/little-unsigned-integer,
		B:16/little-unsigned-integer, C:16/little-unsigned-integer, D:16/little-unsigned-integer, E:16/little-unsigned-integer >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Lumilass handler. Possibly more.
%% @todo Figure out how Lumilass work exactly. The 4 bytes before the file may vary.

send_1a03(CSocket, GID) ->
	{ok, File} = file:read_file("p/lumilassA.bin"),
	Packet = << 16#1a030300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96, File/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc PP cube handler. 
%% @todo The 4 bytes before the file may vary. Everything past that is the same. Figure things out.

send_1a04(CSocket, GID) ->
	{ok, File} = file:read_file("p/ppcube.bin"),
	Packet = << 16#1a040300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96, File/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @doc Types menu handler.
%% @todo Handle correctly.

send_1a07(CSocket, GID) ->
	Packet = << 16#1a070300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#085b5d0a:32, 16#3a200000:32, 0:32,
		16#01010101:32, 16#01010101:32, 16#01010101:32, 16#01010101:32 >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out what the other things are and do it right.
%% @todo Temporarily send 233 until the correct process is figured out.
%%       Should be something along the lines of 203 201 204.

send_spawn(CSocket, GID, _) ->
	send_0233(CSocket, GID, egs_db:users_select_others_in_area(egs_db:users_select(GID))).

%% @doc Log message to the console.

log(GID, Message) ->
	io:format("game (~.10b): ~s~n", [GID, Message]).

log(GID, Message, Format) ->
	RealMessage = io_lib:format(Message, Format),
	log(GID, RealMessage).

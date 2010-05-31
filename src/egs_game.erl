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
-export([listen/0, accept/1, process/2, char_select/3, lobby_load/6, loop/3, loop/4]). % internal

-include("include/records.hrl").
-include("include/network.hrl").
-include("include/maps.hrl").

%% @doc Start the game server.

start() ->
	Pid = spawn_link(?MODULE, listen, []),
	Pid.

%% @doc Listen for connections.

listen() ->
	process_flag(trap_exit, true),
	{ok, LSocket} = ssl:listen(?GAME_PORT, ?GAME_LISTEN_OPTIONS),
	?MODULE:accept(LSocket).

%% @doc Accept connections.

accept(LSocket) ->
	case ssl:transport_accept(LSocket, 5000) of
		{ok, CSocket} ->
			ssl:ssl_accept(CSocket),
			try
				log(0, "hello (new connection)"),
				egs_proto:send_hello(CSocket),
				Pid = spawn_link(?MODULE, process, [CSocket, 0]),
				ssl:controlling_process(CSocket, Pid)
			catch
				_:_ ->
					reload
			end;
		_ ->
			reload
	end,
	?MODULE:accept(LSocket).

%% @doc Process the new connections.
%%      Send an hello packet, authenticate the user and send him to character select.

process(CSocket, Version) ->
	case egs_proto:packet_recv(CSocket, 5000) of
		{ok, Packet} ->
			<< _:32, Command:16/unsigned-integer, _/bits >> = Packet,
			process_handle(Command, CSocket, Version, Packet);
		{error, timeout} ->
			reload,
			?MODULE:process(CSocket, Version);
		{error, closed} ->
			log(0, "recv error, closing")
	end.

%% @doc Game server auth request handler.

process_handle(16#020d, CSocket, Version, Packet) ->
	[{gid, GID}, {auth, Auth}] = egs_proto:parse_game_auth(Packet),
	case egs_db:users_select(GID) of
		error ->
			log(GID, "can't find user, closing"),
			ssl:close(CSocket);
		User ->
			case User#users.auth of
				Auth ->
					log(GID, "good auth, proceed"),
					LID = 1 + egs_db:next(lobby) rem 1023,
					Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
					egs_db:users_insert(#users{gid=GID, pid=self(), socket=CSocket, auth=success, time=Time, folder=User#users.folder, lid=LID}),
					egs_proto:send_flags(CSocket, GID),
					?MODULE:char_select(CSocket, GID, Version);
				_ ->
					log(GID, "bad auth, closing"),
					egs_db:users_delete(GID),
					ssl:close(CSocket)
			end
	end;

%% @doc Platform information handler. Obtain the game version.

process_handle(16#080e, CSocket, _, Packet) ->
	[{version, RealVersion}] = egs_proto:parse_platform_info(Packet),
	?MODULE:process(CSocket, RealVersion);

%% @doc Unknown command handler. Do nothing.

process_handle(Command, CSocket, Version, _) ->
	log(0, io_lib:format("(process) dismissed packet ~4.16.0b", [Command])),
	?MODULE:process(CSocket, Version).

%% @doc Character selection screen loop.
%%      The default entry point currently is first floor, near the uni cube.

char_select(CSocket, GID, Version) ->
	case egs_proto:packet_recv(CSocket, 5000) of
		{ok, Packet} ->
			<< _:32, Command:16/unsigned-integer, _/bits >> = Packet,
			char_select_handle(Command, CSocket, GID, Version, Packet);
		{error, timeout} ->
			egs_proto:send_keepalive(CSocket, GID),
			reload,
			?MODULE:char_select(CSocket, GID, Version);
		{error, closed} ->
			log(GID, "recv error, closing"),
			egs_db:users_delete(GID)
	end.

%% @doc Character selection handler.

char_select_handle(16#020b, CSocket, GID, Version, Packet) ->
	log(GID, "character selection"),
	[{number, Number}] = egs_proto:parse_character_select(Packet),
	char_select_load(CSocket, GID, Version, Number);

%% @doc Character creation handler.

char_select_handle(16#0d02, CSocket, GID, Version, Packet) ->
	log(GID, "character creation"),
	User = egs_db:users_select(GID),
	[{number, Number}, {char, Char}] = egs_proto:parse_character_create(Packet),
	_ = file:make_dir(io_lib:format("save/~s", [User#users.folder])),
	file:write_file(io_lib:format("save/~s/~b-character", [User#users.folder, Number]), Char),
	file:write_file(io_lib:format("save/~s/~b-character.options", [User#users.folder, Number]), << 0:192 >>),
	char_select_load(CSocket, GID, Version, Number);

%% @doc Character selection screen request.

char_select_handle(16#0d06, CSocket, GID, Version, _) ->
	log(GID, "send character selection screen"),
	User = egs_db:users_select(GID),
	egs_proto:send_character_list(CSocket, GID,
		char_load(User#users.folder, 0),
		char_load(User#users.folder, 1),
		char_load(User#users.folder, 2),
		char_load(User#users.folder, 3)),
	?MODULE:char_select(CSocket, GID, Version);

%% @doc Unknown command handler. Do nothing.

char_select_handle(Command, CSocket, GID, Version, _) ->
	log(GID, io_lib:format("(char_select) dismissed packet ~4.16.0b", [Command])),
	?MODULE:char_select(CSocket, GID, Version).

%% @doc Load the given character's data.

char_load(Folder, Number) ->
	Filename = io_lib:format("save/~s/~b-character", [Folder, Number]),
	case file:read_file(Filename) of
		{ok, Char} ->
			{ok, Options} = file:read_file(io_lib:format("~s.options", [Filename])),
			[{status, 1}, {char, Char}, {options, Options}];
		{error, _} ->
			[{status, 0}, {char, << 0:2208 >>}]
	end.

%% @doc Load the selected character and start the main game's loop.

char_select_load(CSocket, GID, Version, Number) ->
	User = egs_db:users_select(GID),
	[{status, _}, {char, << Name:512/bits, _/bits >>}|_] = char_load(User#users.folder, Number),
	NewRow = User#users{charnumber=Number, charname=Name},
	egs_db:users_insert(NewRow),
	lobby_load(CSocket, GID, 1100000, 0, 1, 1),
	ssl:setopts(CSocket, [{active, true}]),
	?MODULE:loop(CSocket, GID, Version).

%% @doc Load the given map as a mission counter.

counter_load(CSocket, GID, Quest, MapType, MapNumber, MapEntry) ->
	OldUser = egs_db:users_select(GID),
	User = OldUser#users{quest=Quest, maptype=MapType, mapnumber=MapNumber, mapentry=MapEntry},
	egs_db:users_insert(User),
	[{status, 1}, {char, Char}, {options, _}] = char_load(User#users.folder, User#users.charnumber),
	[{name, CounterName}, {quest, QuestFile}, {zone, ZoneFile}, {entries, _}] =
		[{name, "LL counter"}, {quest, "data/lobby/counter.quest.nbl"}, {zone, "data/lobby/counter.zone.nbl"}, {entries, []}],
	try
		% 0c00
		egs_proto:send_quest(CSocket, QuestFile),
		% 0a05 010d
		egs_proto:send_zone_init(CSocket, GID, counter),
		egs_proto:send_zone(CSocket, ZoneFile),
		egs_proto:send_map(CSocket, Quest, MapType, MapNumber, MapEntry),
		egs_proto:send_location(CSocket, GID, Quest, MapType, MapNumber, CounterName),
		% 0215 0215 020c 1202 1204 1206 1207
		egs_proto:send_load_quest(CSocket, GID),
		send_packet_201(CSocket, GID, User, Char),
		% 0a06
		egs_proto:send_loading_end(CSocket, GID),
		egs_proto:send_camera_center(CSocket, GID)
	catch
		_ ->
			ssl:close(CSocket),
			log(GID, "send error, closing")
	end.

%% @doc Load the given map as a standard lobby.

lobby_load(CSocket, GID, Quest, MapType, MapNumber, MapEntry) ->
	OldUser = egs_db:users_select(GID),
	User = OldUser#users{quest=Quest, maptype=MapType, mapnumber=MapNumber, mapentry=MapEntry},
	egs_db:users_insert(User),
	[{status, 1}, {char, Char}, {options, Options}] = char_load(User#users.folder, User#users.charnumber),
	[{name, _}, {quest, QuestFile}, {zone, ZoneFile}, {entries, _}] = proplists:get_value([Quest, MapType, MapNumber], ?MAPS,
		[{name, "dammy"}, {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone.nbl"}, {entries, []}]),
	try
		% broadcast spawn to other people
		lists:foreach(fun(Other) -> Other#users.pid ! {psu_player_spawn, User} end, egs_db:users_select_others(GID)),
		% load lobby and character
		egs_proto:send_character_selected(CSocket, GID, Char, Options),
		% 0246
		send_packet_0a0a(CSocket, GID),
		% 1006
		send_packet_1005(CSocket, GID, Char),
		% 1006 0210
		egs_proto:send_universe_info(CSocket, GID),
		egs_proto:send_player_card(CSocket, GID, Char),
		% 1501 1512 0303
		egs_proto:send_npc_info(CSocket, GID),
		% 0c00
		egs_proto:send_quest(CSocket, QuestFile),
		% 0a05 0111 010d
		egs_proto:send_zone_init(CSocket, GID, lobby),
		egs_proto:send_zone(CSocket, ZoneFile),
		egs_proto:send_map(CSocket, Quest, MapType, MapNumber, MapEntry),
		% 100e 020c
		egs_proto:send_load_quest(CSocket, GID),
		send_packet_201(CSocket, GID, User, Char),
		send_packet_0a06(CSocket, GID),
		Users = egs_db:users_select_others(GID),
		send_packet_233(CSocket, GID, Users),
		egs_proto:send_loading_end(CSocket, GID),
		egs_proto:send_camera_center(CSocket, GID)
	catch
		_ ->
			ssl:close(CSocket),
			log(GID, "send error, closing")
	end.

%% @doc Load the given map as a mission.
%% @todo One of the silenced packets enable a 04xx command sent by the client and related to enemies sync. Probably 12xx.

mission_load(CSocket, GID, Quest, MapType, MapNumber, MapEntry) ->
	OldUser = egs_db:users_select(GID),
	User = OldUser#users{quest=Quest, maptype=MapType, mapnumber=MapNumber, mapentry=MapEntry},
	egs_db:users_insert(User),
	[{status, 1}, {char, Char}, {options, _}] = char_load(User#users.folder, User#users.charnumber),
	[{name, _}, {quest, QuestFile}, {zone, ZoneFile}, {entries, _}] = proplists:get_value([Quest, MapType, MapNumber], ?MAPS),
	try
		% 0c00
		egs_proto:send_quest(CSocket, QuestFile),
		% 0215 0a05 010d
		egs_proto:send_zone_init(CSocket, GID, mission),
		egs_proto:send_zone(CSocket, ZoneFile),
		egs_proto:send_map(CSocket, Quest, MapType, MapNumber, MapEntry),
		% 100e 0215 0215
		egs_proto:send_trial_start(CSocket, GID),
		% 020c

		% mandatory packet to make enemies appear
		{ok, << _:32, Packet/bits >>} = file:read_file("p/packet1202.bin"),
		egs_proto:packet_send(CSocket, Packet),

		% 1204 1206 1207
		egs_proto:send_load_quest(CSocket, GID),
		send_packet_201(CSocket, GID, User, Char),
		send_packet_0a06(CSocket, GID),
		egs_proto:send_loading_end(CSocket, GID),
		egs_proto:send_camera_center(CSocket, GID)
	catch
		_ ->
			ssl:close(CSocket),
			log(GID, "send error, closing")
	end.

%% @doc Load the given map as a player room.
%%      Always load the same room that isn't this player's room for now.
%% @todo Load 'Your room' correctly.

myroom_load(CSocket, GID, Quest, MapType, MapNumber, MapEntry) ->
	OldUser = egs_db:users_select(GID),
	User = OldUser#users{quest=Quest, maptype=MapType, mapnumber=MapNumber, mapentry=MapEntry},
	egs_db:users_insert(User),
	[{status, 1}, {char, Char}, {options, Options}] = char_load(User#users.folder, User#users.charnumber),
	[{name, _}, {quest, QuestFile}, {zone, ZoneFile}, {entries, _}] = 
		[{name, "dammy"}, {quest, "data/rooms/test.quest.nbl"}, {zone, "data/rooms/test.zone.nbl"}, {entries, []}],
	try
		% broadcast spawn to other people
		lists:foreach(fun(Other) -> Other#users.pid ! {psu_player_spawn, User} end, egs_db:users_select_others(GID)),
		% load lobby and character
		egs_proto:send_character_selected(CSocket, GID, Char, Options),
		% 0246 0a0a 1006
		send_packet_1005(CSocket, GID, Char),
		% 1006 0210
		egs_proto:send_universe_info(CSocket, GID),
		egs_proto:send_player_card(CSocket, GID, Char),
		% 1501 1512 0303
		egs_proto:send_npc_info(CSocket, GID),
		% 0c00
		egs_proto:send_quest(CSocket, QuestFile),
		% 0a05 0111 010d
		egs_proto:send_zone_init(CSocket, GID, myroom),
		egs_proto:send_zone(CSocket, ZoneFile),
		egs_proto:send_map(CSocket, Quest, MapType, MapNumber, MapEntry),
		myroom_send_packet(CSocket, "p/packet1332.bin"),
		% 130e(a) 130e(b) 1202 1204 1206
		egs_proto:send_load_quest(CSocket, GID),
		myroom_send_packet(CSocket, "p/packet1309.bin"),
		% 130a(removing moved the pm from shop to normal spot) 1318
		send_packet_201(CSocket, GID, User, Char),
		% 0a06 0233
		egs_proto:send_loading_end(CSocket, GID),
		egs_proto:send_camera_center(CSocket, GID)
	catch
		_ ->
			ssl:close(CSocket),
			log(GID, "send error, closing")
	end.

myroom_send_packet(CSocket, Filename) ->
	{ok, << _:32, File/bits >>} = file:read_file(Filename),
	egs_proto:packet_send(CSocket, File).

%% @doc Load the given map as a spaceport.

spaceport_load(CSocket, GID, Quest, MapType, MapNumber, MapEntry) ->
	OldUser = egs_db:users_select(GID),
	User = OldUser#users{quest=Quest, maptype=MapType, mapnumber=MapNumber, mapentry=MapEntry},
	egs_db:users_insert(User),
	[{status, 1}, {char, Char}, {options, _}] = char_load(User#users.folder, User#users.charnumber),
	[{name, _}, {quest, QuestFile}, {zone, ZoneFile}, {entries, _}] = proplists:get_value([Quest, MapType, MapNumber], ?MAPS),
	try
		% 0c00
		egs_proto:send_quest(CSocket, QuestFile),
		% 0a05
		egs_proto:send_zone_init(CSocket, GID, spaceport),
		egs_proto:send_zone(CSocket, ZoneFile),
		egs_proto:send_map(CSocket, Quest, MapType, MapNumber, MapEntry),
		% 100e 020c
		send_packet_201(CSocket, GID, User, Char),
		% 0a06
		egs_proto:send_loading_end(CSocket, GID),
		egs_proto:send_camera_center(CSocket, GID)
	catch
		_ ->
			ssl:close(CSocket),
			log(GID, "send error, closing")
	end.

%% @doc Alias for the game main's loop when the buffer is empty.

loop(CSocket, GID, Version) ->
	loop(CSocket, GID, Version, << >>).

%% @doc Game's main loop.

loop(CSocket, GID, Version, SoFar) ->
	receive
		{psu_broadcast, Packet} ->
			<< A:64/bits, _:32, B:96/bits, _:64, C/bits >> = Packet,
			Broadcast = << A/binary, 16#00011300:32, B/binary, 16#00011300:32, GID:32/little-unsigned-integer, C/binary >>,
			egs_proto:packet_send(CSocket, Broadcast),
			?MODULE:loop(CSocket, GID, Version, SoFar);
		{psu_chat, ChatGID, ChatName, ChatModifiers, ChatMessage} ->
			egs_proto:send_chat(CSocket, Version, ChatGID, ChatName, ChatModifiers, ChatMessage),
			?MODULE:loop(CSocket, GID, Version, SoFar);
		{psu_keepalive} ->
			egs_proto:send_keepalive(CSocket, GID),
			?MODULE:loop(CSocket, GID, Version, SoFar);
		{psu_player_spawn, PlayerGID} ->
			send_spawn(CSocket, GID, PlayerGID),
			?MODULE:loop(CSocket, GID, Version, SoFar);
		{ssl, _, Data} ->
			{Packets, Rest} = egs_proto:packet_split(<< SoFar/bits, Data/bits >>),
			[dispatch(CSocket, GID, Version, P) || P <- Packets],
			?MODULE:loop(CSocket, GID, Version, Rest);
		{ssl_closed, _} ->
			close(CSocket, GID);
		{ssl_error, _, _} ->
			close(CSocket, GID);
		_ ->
			?MODULE:loop(CSocket, GID, Version, SoFar)
	after 1000 ->
		reload,
		?MODULE:loop(CSocket, GID, Version, SoFar)
	end.

%% @doc Close the connection for the given user.

close(CSocket, GID) ->
	log(GID, "quit"),
	egs_db:users_delete(GID),
	ssl:close(CSocket).

%% @doc Dispatch the command to the right handler.

dispatch(CSocket, GID, Version, Packet) ->
	<< _:32, Command:16/unsigned-integer, Channel:8/little-unsigned-integer, _/bits >> = Packet,
	case [Command, Channel] of
		[16#0b05, _] ->
			% 0b05 uses the channel for something else, conflicts may occur
			handle(Command, CSocket, GID, Version, Packet);
		[_, 1] ->
			broadcast(Command, GID, Packet);
		_ ->
			handle(Command, CSocket, GID, Version, Packet)
	end.

%% @doc Position change broadcast handler. Save the position and then dispatch it.

broadcast(16#0503, GID, Packet) ->
	LID = 0, % TODO: handle the LID correctly
	<< 100:32/little-unsigned-integer, 16#050301:24/unsigned-integer, _:72, GID:32/little-unsigned-integer, _:192,
		GID:32/little-unsigned-integer, LID:32/little-unsigned-integer, Direction:32/bits, Coords:96/bits, _:96,
		Quest:32/little-unsigned-integer, MapType:32/little-unsigned-integer, MapNumber:32/little-unsigned-integer,
		MapEntry:32/little-unsigned-integer, _:32 >> = Packet,
	User = egs_db:users_select(GID),
	NewUser = User#users{direction=Direction, coords=Coords, quest=Quest, maptype=MapType, mapnumber=MapNumber, mapentry=MapEntry},
	egs_db:users_insert(NewUser),
	broadcast(default, GID, Packet);

%% @doc Stand still broadcast handler. Save the position and then dispatch it.

broadcast(16#0514, GID, Packet) ->
	<< _:320, _:96, Direction:32/bits, Coords:96/bits, Quest:32/little-unsigned-integer, MapType:32/little-unsigned-integer,
		MapNumber:32/little-unsigned-integer, MapEntry:32/little-unsigned-integer, _/bits >> = Packet,
	User = egs_db:users_select(GID),
	NewUser = User#users{direction=Direction, coords=Coords, quest=Quest, maptype=MapType, mapnumber=MapNumber, mapentry=MapEntry},
	egs_db:users_insert(NewUser),
	broadcast(default, GID, Packet);

%% @doc Default broadcast handler. Dispatch the command to everyone.
%%      We clean up the command and use the real GID and LID of the user, disregarding what was sent and possibly tampered with.
%%      Only a handful of commands are allowed to broadcast. An user tampering with it would get disconnected instantly.
%% @todo Don't query the user data everytime! Keep an User instead of a GID probably.

broadcast(Command, GID, Packet)
	when	Command =:= 16#0101;
			Command =:= 16#0102;
			Command =:= 16#0104;
			Command =:= 16#0107;
			Command =:= 16#010f;
			Command =:= 16#050f;
			Command =:= default ->
	<< _:32, A:64/bits, _:64, B:192/bits, _:64, C/bits >> = Packet,
	case egs_db:users_select(GID) of
		error ->
			ignore;
		Self ->
			LID = Self#users.lid,
			SafePacket = << A/binary, 16#00011300:32, GID:32/little-unsigned-integer, B/binary,
				GID:32/little-unsigned-integer, LID:32/little-unsigned-integer, C/binary >>,
			lists:foreach(fun(User) -> User#users.pid ! {psu_broadcast, SafePacket} end, egs_db:users_select_others(GID))
	end.

%% @doc Movement (non-broadcast) handler. Do nothing.

handle(16#0102, _, _, _, _) ->
	ignore;

%% @doc Weapon change handler. Fake it.
%% @todo Others probably want to see that you changed your weapon.

handle(16#0105, CSocket, GID, _, Orig) ->
	log(GID, "weapon change (and more probably)"),
	<< _:384, Rest/bits >> = Orig,
	Packet = << 16#01050300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer,
		0:64, GID:32/little-unsigned-integer, Rest/binary >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc Character death handler. Abort mission and redirect to 4th floor for now.
%% @todo Recover from death correctly.

handle(16#0110, CSocket, GID, _, _) ->
	log(GID, "death (and more probably)"),
	lobby_load(CSocket, GID, 1100000, 0, 4, 6);

%% @doc Keepalive handler. Do nothing.

handle(16#021c, _, _, _, _) ->
	ignore;

%% @doc Uni cube handler.

handle(16#021d, CSocket, GID, _, _) ->
	log(GID, "uni cube"),
	egs_proto:send_universe_cube(CSocket);

%% @doc Uni selection handler.
%%      When selecting 'Your room', load first floor for now.
%%      When selecting 'Reload', load first floor.
%% @todo Load 'Your room' correctly.

handle(16#021f, CSocket, GID, _, Packet) ->
	case egs_proto:parse_uni_select(Packet) of
		[{uni, 0}] ->
			log(GID, "uni selection cancelled");
		[{uni, 16#ffffffff}] ->
			log(GID, "uni selection (my room)"),
			% 0230 0220
			myroom_load(CSocket, GID, 1120000, 0, 423, 0);
		_ ->
			log(GID, "uni selection (reload)"),
			% 0230 0220
			lobby_load(CSocket, GID, 1100000, 0, 1, 1)
	end;

%% @doc Shortcut changes handler. Do nothing.
%% @todo Save it.

handle(16#0302, _, GID, _, _) ->
	log(GID, "dismissed shortcut changes");

%% @doc Chat broadcast handler. Dispatch the message to everyone (for now).
%%      We must take extra precautions to handle different versions of the game correctly.
%% @todo Only broadcast to people in the same map.

handle(16#0304, _, GID, Version, Packet) ->
	[{gid, _}, {name, ChatName}, {modifiers, ChatModifiers}, {message, ChatMessage}] = egs_proto:parse_chat(Version, Packet),
	case ChatName of
		missing ->
			case egs_db:users_select(GID) of
				error ->
					ActualName = ChatName;
				User ->
					ActualName = User#users.charname
			end;
		_ ->
			ActualName = ChatName
	end,
	[LogName|_] = re:split(ActualName, "\\0\\0", [{return, binary}]),
	[TmpMessage|_] = re:split(ChatMessage, "\\0\\0", [{return, binary}]),
	LogMessage = re:replace(TmpMessage, "\\n", " ", [global, {return, binary}]),
	log(GID, io_lib:format("chat from ~s: ~s", [[re:replace(LogName, "\\0", "", [global, {return, binary}])], [re:replace(LogMessage, "\\0", "", [global, {return, binary}])]])),
	lists:foreach(fun(User) -> User#users.pid ! {psu_chat, GID, ActualName, ChatModifiers, ChatMessage} end, egs_db:users_select_all());

%% @todo Handle this packet. Ignore for now.

handle(16#0402, _, _, _, _) ->
	ignore;

%% @doc Map change handler.
%%      Spaceports and my room are handled differently than normal lobbies.
%% @todo Load 'Your room' correctly.

handle(16#0807, CSocket, GID, _, Packet) ->
	[{quest, Quest}, {maptype, MapType}, {mapnumber, MapNumber}, {mapentry, MapEntry}] = egs_proto:parse_lobby_change(Packet),
	log(GID, io_lib:format("lobby change (~b,~b,~b,~b)", [Quest,MapType, MapNumber, MapEntry])),
	case {Quest, MapType, MapNumber, MapEntry} of
		{1104000, 0, 900, 0} ->
			spaceport_load(CSocket, GID, Quest, MapType, MapNumber, MapEntry);
		{1120000, _, _, _} ->
			myroom_load(CSocket, GID, Quest, MapType, 423, MapEntry);
		_ ->
			lobby_load(CSocket, GID, Quest, MapType, MapNumber, MapEntry)
	end;

%% @doc Mission counter handler.
%% @todo Make the egs_proto function name more clear. This isn't a lobby! It's just the same format.

handle(16#0811, CSocket, GID, _, Packet) ->
	[{quest, Quest}, {maptype, MapType}, {mapnumber, MapNumber}, {mapentry, MapEntry}] = egs_proto:parse_lobby_change(Packet),
	log(GID, io_lib:format("mission counter (~b,~b,~b,~b)", [Quest,MapType, MapNumber, MapEntry])),
	counter_load(CSocket, GID, Quest, MapType, MapNumber, MapEntry);

%% @doc Mission select handler? Packet contains the selected mission number.
%% @todo Load more than one mission.

handle(16#0c01, CSocket, GID, _, _) ->
	Packet = << 16#0c020300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96 >>,
	egs_proto:packet_send(CSocket, Packet),
	mission_load(CSocket, GID, 1000013, 0, 1121, 0); % load test mission!

%% @doc Counter quests files request handler? Send huge number of quest files.
%% @todo Handle correctly.

handle(16#0c05, CSocket, _, _, _) ->
	{ok, << _:32, Packet/bits >>} = file:read_file("p/packet0c06.bin"),
	egs_proto:packet_send(CSocket, Packet);

%% @doc Counter available mission list request handler.
%% @todo Temporarily allow rare mission and LL all difficulties to all players.

handle(16#0c0f, CSocket, GID, _, _) ->
	Packet = << 16#0c100300:32, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#01a92800:32, 3, 3, 0,
		3, 3, 3, 3, 0, 0:40, 3, 3, 3, 3, 3, 0:176 >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc Set flag handler. Associate a new flag with the character.
%%      Just reply with a success value for now.
%% @todo God save the flags.

handle(16#0d04, CSocket, GID, _, Orig) ->
	<< _:352, Flag:128/bits, A:16/bits, _:8, B/bits >> = Orig,
	log(GID, io_lib:format("flag handler for ~s", [re:replace(Flag, "\\0+", "", [global, {return, binary}])])),
	Packet = << 16#0d040300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Flag/binary, A/binary, 1, B/binary >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc Options changes handler.

handle(16#0d07, _, GID, _, Packet) ->
	log(GID, "options changes"),
	[{options, Options}] = egs_proto:parse_options_change(Packet),
	User = egs_db:users_select(GID),
	file:write_file(io_lib:format("save/~s/~b-character.options", [User#users.folder, User#users.charnumber]), Options);

%% @doc Hit handler.
%% @todo Finish the work on it.

handle(16#0e00, CSocket, GID, _, Orig) ->
	<< _:448, A:224/bits, B:128/bits, _/bits >> = Orig,
	PlayerHP = 4401,
	TargetHP = 0,
	Damage = 58008,
	Packet = << 16#0e070300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
		1:32/little-unsigned-integer, 16#01050000:32, Damage:32/little-unsigned-integer,
		A/binary, 0:64, PlayerHP:32/little-unsigned-integer, 0:32, 16#01000200:32,
		0:32, TargetHP:32, 0:32, B/binary, 16#04320000:32, 16#80000000:32, 16#26030000:32, 16#89068d00:32, 16#0c1c0105:32 >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc Lobby event handler. Handle chairs!
%%      Apparently used for elevator, sit on chairs, and more?
%% @todo Handle more than sit on chair.

handle(16#0f0a, CSocket, GID, _, Orig) ->
	<< _:448, A:32/little-unsigned-integer, _:64, B:32/little-unsigned-integer, _/bits >> = Orig,
	Packet = << 16#1211:16, 0:176, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, A:32/little-unsigned-integer, B:32/little-unsigned-integer, 8:32/little-unsigned-integer, 0:32 >>,
	egs_proto:packet_send(CSocket, Packet),
	log(GID, "lobby event (can only chair so far)");

%% @doc Counter initialization handler?
%% @todo Handle correctly.

handle(16#1710, CSocket, GID, _, _) ->
	Packet = << 16#17110300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:96 >>,
	egs_proto:packet_send(CSocket, Packet);

%% @doc Unknown command handler. Do nothing.

handle(Command, _, GID, _, _) ->
	log(GID, io_lib:format("(game) dismissed packet ~4.16.0b", [Command])).

%% @todo Figure out what the other things are.

send_packet_201(CSocket, GID, User, Char) ->
	Quest = User#users.quest,
	MapType = User#users.maptype,
	MapNumber = User#users.mapnumber,
	MapEntry = User#users.mapentry,
	CharGID = User#users.gid,
	CharLID = User#users.lid,
	{ok, File} = file:read_file("p/packet0201.bin"),
	<< _:96, A:32/bits, _:96, B:32/bits, _:256, D:32/bits, _:2656, After/bits >> = File,
	Packet = << 16#0201:16, 0:48, A/binary, CharGID:32/little-unsigned-integer, 0:64, B/binary, GID:32/little-unsigned-integer,
		0:64, CharLID:32/little-unsigned-integer, CharGID:32/little-unsigned-integer, 0:96, D/binary, Quest:32/little-unsigned-integer,
		MapType:32/little-unsigned-integer, MapNumber:32/little-unsigned-integer, MapEntry:32/little-unsigned-integer, 0:192, Quest:32/little-unsigned-integer,
		MapType:32/little-unsigned-integer, MapNumber:32/little-unsigned-integer, MapEntry:32/little-unsigned-integer, Char/binary, After/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out what the other things are.

send_packet_233(CSocket, GID, Users) ->
	NbUsers = length(Users),
	case NbUsers of
		0 ->
			ignore;
		_ ->
			Header = << 16#02330300:32, 0:32, 16#00001200:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32,
				GID:32/little-unsigned-integer, 0:64, NbUsers:32/little-unsigned-integer >>,
			Contents = build_packet_233_contents(Users),
			Packet = << Header/binary, Contents/binary >>,
			egs_proto:packet_send(CSocket, Packet)
	end.

build_packet_233_contents([]) ->
	<< >>;
build_packet_233_contents(Users) ->
	[User|Rest] = Users,
	{ok, File} = file:read_file("p/player.bin"),
	<< A:32/bits, _:32, B:64/bits, _:32, C:32/bits, _:256, E:64/bits, _:2336, F/bits >> = File,
	{ok, CharFile} = file:read_file(io_lib:format("save/~s/~b-character", [User#users.folder, User#users.charnumber])),
	CharGID = User#users.gid,
	LID = User#users.lid,
	case User#users.coords of % TODO: temporary? undefined handling
		undefined ->
			Direction = << 0:32 >>,
			Coords = << 0:96 >>,
			Quest = 1100000,
			MapType = 0,
			MapNumber = 1,
			MapEntry = 0;
		_ ->
			Direction = User#users.direction,
			Coords = User#users.coords,
			Quest = User#users.quest,
			MapType = User#users.maptype,
			MapNumber = User#users.mapnumber,
			MapEntry = User#users.mapentry
	end,
	Chunk = << A/binary, CharGID:32/little-unsigned-integer, B/binary, LID:16/little-unsigned-integer, 16#0100:16, C/binary,
		Quest:32/little-unsigned-integer, MapType:32/little-unsigned-integer, MapNumber:32/little-unsigned-integer, MapEntry:32/little-unsigned-integer,
		Direction:32/bits, Coords:96/bits, E/binary,  Quest:32/little-unsigned-integer, MapType:32/little-unsigned-integer, MapNumber:32/little-unsigned-integer,
		MapEntry:32/little-unsigned-integer, CharFile/binary, F/binary >>,
	Next = build_packet_233_contents(Rest),
	<< Chunk/binary, Next/binary >>.

%% @todo Inventory related. Figure out everything in this packet and handle it correctly.

send_packet_0a06(CSocket, GID) ->
	{ok, << _:32, A:96/bits, _:32, B:96/bits, _:32, C:1440/bits, _:32, D/bits >>} = file:read_file("p/packet0a06.bin"),
	egs_proto:packet_send(CSocket, << A/binary, GID:32/little-unsigned-integer, B/binary, GID:32/little-unsigned-integer, C/binary, GID:32/little-unsigned-integer, D/binary >>).

%% @todo Inventory. Figure out everything in this packet and handle it correctly.

send_packet_0a0a(CSocket, GID) ->
	{ok, << _:32, A:224/bits, _:32, B/bits >>} = file:read_file("p/packet0a0a.bin"),
	egs_proto:packet_send(CSocket, << A/binary, GID:32/little-unsigned-integer, B/binary >>).

%% @todo Figure out what the packet is.

send_packet_1005(CSocket, GID, Char) ->
	{ok, File} = file:read_file("p/packet1005.bin"),
	<< _:352, Before:160/bits, _:608, After/bits >> = File,
	<< Name:512/bits, _/bits >> = Char,
	Packet = << 16#1005:16, 0:208, GID:32/little-unsigned-integer, 0:64, Before/binary, GID:32/little-unsigned-integer, 0:64, Name/binary, After/binary >>,
	egs_proto:packet_send(CSocket, Packet).

%% @todo Figure out what the other things are and do it right.
%% @todo Temporarily send 233 until the correct process is figured out.
%%       Should be something along the lines of 203 201 204.

send_spawn(CSocket, GID, _) ->
	send_packet_233(CSocket, GID, egs_db:users_select_others(GID)).

%% @doc Log message to the console.

log(GID, Message) ->
	io:format("game (~.10b): ~s~n", [GID, Message]).

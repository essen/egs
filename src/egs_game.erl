%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Game callback module.
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

-module(egs_game).
-export([keepalive/1, info/2, cast/3, raw/3, event/2]).

-include("include/records.hrl").
-include("include/maps.hrl").
-include("include/psu/items.hrl").

%% @doc Send a keepalive.
keepalive(#state{socket=Socket}) ->
	psu_proto:send_keepalive(Socket).

%% @doc Forward the broadcasted command to the client.
info({egs, cast, Command}, #state{gid=GID}) ->
	<< A:64/bits, _:32, B:96/bits, _:64, C/bits >> = Command,
	psu_game:send(<< A/binary, 16#00011300:32, B/binary, 16#00011300:32, GID:32/little-unsigned-integer, C/binary >>);

%% @doc Forward the chat message to the client.
info({egs, chat, ChatTypeID, ChatGID, ChatName, ChatModifiers, ChatMessage}, _State) ->
	psu_game:send_0304(ChatTypeID, ChatGID, ChatName, ChatModifiers, ChatMessage);

%% @doc Inform the client that a player has spawn.
%% @todo Should be something along the lines of 010d 0205 203 201.
info({egs, player_spawn, _Player}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	{ok, SpawnList} = egs_user_model:select({neighbors, User}),
	psu_game:send_0233(SpawnList);

%% @doc Inform the client that a player has unspawn.
info({egs, player_unspawn, Player}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	psu_game:send_0204(User, Player, 5);

%% @doc Warp the player to the given location.
info({egs, warp, QuestID, ZoneID, MapID, EntryID}, State) ->
	event({area_change, QuestID, ZoneID, MapID, EntryID}, State).

%% Broadcasts.

%% @todo Handle broadcasting better than that. Review the commands at the same time.
%% @doc Position change. Save the position and then dispatch it.
cast(16#0503, Data, State=#state{gid=GID}) ->
	<< _:424, Dir:24/little-unsigned-integer, _PrevCoords:96, X:32/little-float, Y:32/little-float, Z:32/little-float,
		QuestID:32/little-unsigned-integer, ZoneID:32/little-unsigned-integer, MapID:32/little-unsigned-integer, EntryID:32/little-unsigned-integer, _:32 >> = Data,
	FloatDir = Dir / 46603.375,
	{ok, User} = egs_user_model:read(GID),
	NewUser = User#egs_user_model{pos=#pos{x=X, y=Y, z=Z, dir=FloatDir}, area=#psu_area{questid=QuestID, zoneid=ZoneID, mapid=MapID}, entryid=EntryID},
	egs_user_model:write(NewUser),
	cast(valid, Data, State);

%% @doc Stand still. Save the position and then dispatch it.
cast(16#0514, Data, State=#state{gid=GID}) ->
	<< _:424, Dir:24/little-unsigned-integer, X:32/little-float, Y:32/little-float, Z:32/little-float,
		QuestID:32/little-unsigned-integer, ZoneID:32/little-unsigned-integer,
		MapID:32/little-unsigned-integer, EntryID:32/little-unsigned-integer, _/bits >> = Data,
	FloatDir = Dir / 46603.375,
	{ok, User} = egs_user_model:read(GID),
	NewUser = User#egs_user_model{pos=#pos{x=X, y=Y, z=Z, dir=FloatDir}, area=#psu_area{questid=QuestID, zoneid=ZoneID, mapid=MapID}, entryid=EntryID},
	egs_user_model:write(NewUser),
	cast(valid, Data, State);

%% @doc Default broadcast handler. Dispatch the command to everyone.
%%      We clean up the command and use the real GID and LID of the user, disregarding what was sent and possibly tampered with.
%%      Only a handful of commands are allowed to broadcast. An user tampering with it would get disconnected instantly.
%% @todo Don't query the user data everytime! Keep the needed information in the State.
cast(Command, Data, #state{gid=GID})
	when	Command =:= 16#0101;
			Command =:= 16#0102;
			Command =:= 16#0104;
			Command =:= 16#0107;
			Command =:= 16#010f;
			Command =:= 16#050f;
			Command =:= valid ->
	<< _:32, A:64/bits, _:64, B:192/bits, _:64, C/bits >> = Data,
	case egs_user_model:read(GID) of
		{error, _Reason} ->
			ignore;
		{ok, Self} ->
			LID = Self#egs_user_model.lid,
			Packet = << A/binary, 16#00011300:32, GID:32/little-unsigned-integer, B/binary,
				GID:32/little-unsigned-integer, LID:32/little-unsigned-integer, C/binary >>,
			{ok, SpawnList} = egs_user_model:select({neighbors, Self}),
			lists:foreach(fun(User) -> User#egs_user_model.pid ! {egs, cast, Packet} end, SpawnList)
	end.

%% Raw commands.

%% @todo Handle this packet properly.
%% @todo Spawn cleared response event shouldn't be handled following this packet but when we see the spawn actually dead HP-wise.
%% @todo Type shouldn't be :32 but it seems when the later 16 have something it's not a spawn event.
raw(16#0402, << _:352, Data/bits >>, #state{gid=GID}) ->
	<< SpawnID:32/little-unsigned-integer, _:64, Type:32/little-unsigned-integer, _:64 >> = Data,
	case Type of
		7 -> % spawn cleared @todo 1201 sent back with same values apparently, but not always
			log("cleared spawn ~b", [SpawnID]),
			{ok, User} = egs_user_model:read(GID),
			{BlockID, EventID} = psu_instance:spawn_cleared_event(User#egs_user_model.instancepid, (User#egs_user_model.area)#psu_area.zoneid, SpawnID),
			if	EventID =:= false -> ignore;
				true -> psu_game:send_1205(EventID, BlockID, 0)
			end;
		_ ->
			ignore
	end;

%% @todo Handle this packet.
%% @todo 3rd Unsafe Passage C, EventID 10 BlockID 2 = mission cleared?
raw(16#0404, << _:352, Data/bits >>, _State) ->
	<< EventID:8, BlockID:8, _:16, Value:8, _/bits >> = Data,
	log("unknown command 0404: eventid ~b blockid ~b value ~b", [EventID, BlockID, Value]),
	psu_game:send_1205(EventID, BlockID, Value);

%% @todo Used in the tutorial. Not sure what it does. Give an item (the PA) maybe?
%% @todo Probably should ignore that until more is known.
raw(16#0a09, _Data, #state{gid=GID}) ->
	psu_game:send(<< 16#0a090300:32, 0:32, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, 16#00003300:32, 0:32 >>);

%% @todo Figure out this command.
raw(16#0c11, << _:352, A:32/little, B:32/little >>, #state{gid=GID}) ->
	log("0c11 ~p ~p", [A, B]),
	psu_game:send(<< 16#0c120300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, A:32/little, 1:32/little >>);

%% @doc Set flag handler. Associate a new flag with the character.
%%      Just reply with a success value for now.
%% @todo God save the flags.
raw(16#0d04, << _:352, Data/bits >>, #state{gid=GID}) ->
	<< Flag:128/bits, A:16/bits, _:8, B/bits >> = Data,
	log("flag handler for ~s", [re:replace(Flag, "\\0+", "", [global, {return, binary}])]),
	psu_game:send(<< 16#0d040300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64, Flag/binary, A/binary, 1, B/binary >>);

%% @doc Initialize a vehicle object.
%% @todo Find what are the many values, including the odd Whut value (and whether it's used in the reply).
%% @todo Separate the reply.
raw(16#0f00, << _:352, Data/bits >>, _State) ->
	<< A:32/little-unsigned-integer, 0:16, B:16/little-unsigned-integer, 0:16, C:16/little-unsigned-integer, 0, Whut:8, D:16/little-unsigned-integer, 0:16,
		E:16/little-unsigned-integer, 0:16, F:16/little-unsigned-integer, G:16/little-unsigned-integer, H:16/little-unsigned-integer, I:32/little-unsigned-integer >> = Data,
	log("init vehicle: ~b ~b ~b ~b ~b ~b ~b ~b ~b ~b", [A, B, C, Whut, D, E, F, G, H, I]),
	psu_game:send(<< (psu_game:header(16#1208))/binary, A:32/little-unsigned-integer, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32, 16#ffffffff:32,
		0:16, B:16/little-unsigned-integer, 0:16, C:16/little-unsigned-integer, 0:16, D:16/little-unsigned-integer, 0:112,
		E:16/little-unsigned-integer, 0:16, F:16/little-unsigned-integer, H:16/little-unsigned-integer, 1, 0, 100, 0, 10, 0, G:16/little-unsigned-integer, 0:16 >>);

%% @doc Enter vehicle.
%% @todo Separate the reply.
raw(16#0f02, << _:352, Data/bits >>, _State) ->
	<< A:32/little-unsigned-integer, B:32/little-unsigned-integer, C:32/little-unsigned-integer >> = Data,
	log("enter vehicle: ~b ~b ~b", [A, B, C]),
	HP = 100,
	psu_game:send(<< (psu_game:header(16#120a))/binary, A:32/little-unsigned-integer, B:32/little-unsigned-integer, C:32/little-unsigned-integer, HP:32/little-unsigned-integer >>);

%% @doc Sent right after entering the vehicle. Can't move without it.
%% @todo Separate the reply.
raw(16#0f07, << _:352, Data/bits >>, _State) ->
	<< A:32/little-unsigned-integer, B:32/little-unsigned-integer >> = Data,
	log("after enter vehicle: ~b ~b", [A, B]),
	psu_game:send(<< (psu_game:header(16#120f))/binary, A:32/little-unsigned-integer, B:32/little-unsigned-integer >>);

%% @todo Not sure yet.
raw(16#1019, _Data, _State) ->
	ignore;
	%~ psu_game:send(<< (psu_game:header(16#1019))/binary, 0:192, 16#00200000:32, 0:32 >>);

%% @todo Not sure about that one though. Probably related to 1112 still.
raw(16#1106, << _:352, Data/bits >>, _State) ->
	psu_game:send_110e(Data);

%% @doc Probably asking permission to start the video (used for syncing?).
raw(16#1112, << _:352, Data/bits >>, _State) ->
	psu_game:send_1113(Data);

%% @todo Not sure yet. Value is probably a TargetID. Used in Airboard Rally. Replying with the same value starts the race.
raw(16#1216, << _:352, Data/bits >>, _State) ->
	<< Value:32/little-unsigned-integer >> = Data,
	log("command 1216 with value ~b", [Value]),
	psu_game:send_1216(Value);

%% @doc Dismiss all unknown raw commands with a log notice.
%% @todo Have a log event handler instead.
raw(Command, _Data, State) ->
	io:format("~p (~p): dismissed command ~4.16.0b~n", [?MODULE, State#state.gid, Command]).

%% Events.

%% @todo When changing lobby to the room, 0230 must also be sent. Same when going from room to lobby.
%% @todo Probably move area_load inside the event and make other events call this one when needed.
event({area_change, QuestID, ZoneID, MapID, EntryID}, State) ->
	event({area_change, QuestID, ZoneID, MapID, EntryID, 16#ffffffff}, State);
event({area_change, QuestID, ZoneID, MapID, EntryID, PartyPos}, State) ->
	case PartyPos of
		16#ffffffff ->
			log("area change (~b,~b,~b,~b,~b)", [QuestID, ZoneID, MapID, EntryID, PartyPos]),
			psu_game:area_load(QuestID, ZoneID, MapID, EntryID, State);
		_Any -> %% @todo Handle area_change event for NPCs in story missions.
			ignore
	end;

%% @doc After the character has been (re)loaded, change the area he's in.
%% @todo The area_load function should probably not change the user's values.
%% @todo Remove that ugly code when the above is done.
event(char_load_complete, State=#state{gid=GID}) ->
	{ok, User=#egs_user_model{area=#psu_area{questid=QuestID, zoneid=ZoneID, mapid=MapID},
		entryid=EntryID}} = egs_user_model:read(GID),
	egs_user_model:write(User#egs_user_model{area=#psu_area{questid=0, zoneid=0, mapid=0}, entryid=0}),
	event({area_change, QuestID, ZoneID, MapID, EntryID}, State);

%% @doc Chat broadcast handler. Dispatch the message to everyone (for now).
%%      Disregard the name sent by the server. Use the name saved in memory instead, to prevent client-side editing.
%% @todo Only broadcast to people in the same map.
%% @todo In the case of NPC characters, when FromTypeID is 00001d00, check that the NPC is in the party and broadcast only to the party (probably).
%% @todo When the game doesn't find an NPC (probably) and forces it to talk like in the tutorial mission it seems FromTypeID, FromGID and Name are all 0.
%% @todo Make sure modifiers have correct values.
event({chat, _FromTypeID, FromGID, _FromName, Modifiers, ChatMsg}, #state{gid=UserGID}) ->
	[BcastTypeID, BcastGID, BcastName] = case FromGID of
		0 -> %% This probably shouldn't happen. Just make it crash on purpose.
			log("chat FromGID=0"),
			ignore;
		UserGID -> %% player chat: disregard whatever was sent except modifiers and message.
			{ok, User} = egs_user_model:read(UserGID),
			[16#00001200, User#egs_user_model.id, (User#egs_user_model.character)#characters.name];
		NPCGID -> %% npc chat: @todo Check that the player is the party leader and this npc is in his party.
			{ok, User} = egs_user_model:read(NPCGID),
			[16#00001d00, FromGID, (User#egs_user_model.character)#characters.name]
	end,
	%% log the message as ascii to the console
	[LogName|_] = re:split(BcastName, "\\0\\0", [{return, binary}]),
	[TmpMessage|_] = re:split(ChatMsg, "\\0\\0", [{return, binary}]),
	LogMessage = re:replace(TmpMessage, "\\n", " ", [global, {return, binary}]),
	log("chat from ~s: ~s", [[re:replace(LogName, "\\0", "", [global, {return, binary}])], [re:replace(LogMessage, "\\0", "", [global, {return, binary}])]]),
	%% broadcast
	{ok, List} = egs_user_model:select(all),
	lists:foreach(fun(X) -> X#egs_user_model.pid ! {egs, chat, BcastTypeID, BcastGID, BcastName, Modifiers, ChatMsg} end, List);

%% @todo There's at least 9 different sets of locations. Handle all of them correctly.
event(counter_background_locations_request, _State) ->
	psu_game:send_170c();

%% @todo Make sure non-mission counters follow the same loading process.
%% @todo Probably validate the From* values, to not send the player back inside a mission.
event({counter_enter, CounterID, FromZoneID, FromMapID, FromEntryID}, State=#state{gid=GID}) ->
	log("counter load ~b", [CounterID]),
	{ok, OldUser} = egs_user_model:read(GID),
	OldArea = OldUser#egs_user_model.area,
	FromArea = {psu_area, OldArea#psu_area.questid, FromZoneID, FromMapID},
	User = OldUser#egs_user_model{areatype=counter, area={psu_area, 16#7fffffff, 0, 0}, entryid=0, prev_area=FromArea, prev_entryid=FromEntryID},
	egs_user_model:write(User),
	QuestFile = "data/lobby/counter.quest.nbl",
	ZoneFile = "data/lobby/counter.zone.nbl",
	%% broadcast unspawn to other people
	{ok, UnspawnList} = egs_user_model:select({neighbors, OldUser}),
	lists:foreach(fun(Other) -> Other#egs_user_model.pid ! {egs, player_unspawn, User} end, UnspawnList),
	%% load counter
	psu_proto:send_0c00(User, State),
	psu_proto:send_020e(QuestFile, State),
	psu_proto:send_0a05(State),
	psu_proto:send_010d(User#egs_user_model{lid=0}, State),
	psu_proto:send_0200(0, mission, State),
	psu_proto:send_020f(ZoneFile, 0, 255, State),
	State2 = State#state{areanb=State#state.areanb + 1},
	psu_proto:send_0205(User, 0, State2),
	psu_proto:send_100e(CounterID, "Counter", State2),
	psu_proto:send_0215(0, State2),
	psu_proto:send_0215(0, State2),
	psu_proto:send_020c(State2),
	psu_game:send_1202(),
	psu_proto:send_1204(State2),
	psu_game:send_1206(),
	psu_game:send_1207(),
	psu_game:send_1212(),
	psu_proto:send_0201(User#egs_user_model{lid=0}, State2),
	psu_proto:send_0a06(User, State2),
	case User#egs_user_model.partypid of
		undefined -> ignore;
		_ -> psu_game:send_022c(0, 16#12)
	end,
	State3 = State2#state{areanb=State2#state.areanb + 1},
	psu_proto:send_0208(State3),
	psu_proto:send_0236(State3),
	{ok, State3};

%% @doc Leave mission counter handler.
event(counter_leave, State=#state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	PrevArea = User#egs_user_model.prev_area,
	event({area_change, PrevArea#psu_area.questid, PrevArea#psu_area.zoneid, PrevArea#psu_area.mapid, User#egs_user_model.prev_entryid}, State);

%% @doc Send the code for the background image to use. But there's more that should be sent though.
%% @todo Apparently background values 1 2 3 are never used on official servers. Find out why.
event({counter_options_request, CounterID}, _State) ->
	log("counter options request ~p", [CounterID]),
	[{quests, _}, {bg, Background}|_Tail] = proplists:get_value(CounterID, ?COUNTERS),
	psu_game:send_1711(Background);

%% @todo Handle when the party already exists! And stop doing it wrong.
event(counter_party_info_request, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	psu_game:send_1706((User#egs_user_model.character)#characters.name);

%% @todo Item distribution is always set to random for now.
event(counter_party_options_request, _State) ->
	psu_game:send_170a();

%% @doc Request the counter's quest files.
event({counter_quest_files_request, CounterID}, _State) ->
	log("counter quest files request ~p", [CounterID]),
	[{quests, Filename}|_Tail] = proplists:get_value(CounterID, ?COUNTERS),
	psu_game:send_0c06(Filename);

%% @doc Counter available mission list request handler.
event({counter_quest_options_request, CounterID}, _State) ->
	log("counter quest options request ~p", [CounterID]),
	[{quests, _}, {bg, _}, {options, Options}] = proplists:get_value(CounterID, ?COUNTERS),
	psu_game:send_0c10(Options);

%% @todo A and B are mostly unknown. Like most of everything else from the command 0e00...
event({hit, FromTargetID, ToTargetID, A, B}, State=#state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	%% hit!
	#hit_response{type=Type, user=NewUser, exp=HasEXP, damage=Damage, targethp=TargetHP, targetse=TargetSE, events=Events} = psu_instance:hit(User, FromTargetID, ToTargetID),
	case Type of
		box ->
			%% @todo also has a hit sent, we should send it too
			events(Events, State);
		_ ->
			PlayerHP = (NewUser#egs_user_model.character)#characters.currenthp,
			case lists:member(death, TargetSE) of
				true -> SE = 16#01000200;
				false -> SE = 16#01000000
			end,
			psu_game:send(<< 16#0e070300:32, 0:160, 16#00011300:32, GID:32/little-unsigned-integer, 0:64,
				1:32/little-unsigned-integer, 16#01050000:32, Damage:32/little-unsigned-integer,
				A/binary, 0:64, PlayerHP:32/little-unsigned-integer, 0:32, SE:32,
				0:32, TargetHP:32/little-unsigned-integer, 0:32, B/binary,
				16#04320000:32, 16#80000000:32, 16#26030000:32, 16#89068d00:32, 16#0c1c0105:32, 0:64 >>)
				% after TargetHP is SE-related too?
	end,
	%% exp
	if	HasEXP =:= true ->
			psu_proto:send_0115(NewUser#egs_user_model{lid=0}, ToTargetID, State);
		true -> ignore
	end,
	%% save
	egs_user_model:write(NewUser);

event({hits, Hits}, State) ->
	events(Hits, State);

event({item_description_request, ItemID}, State) ->
	psu_proto:send_0a11(ItemID, egs_items_db:desc(ItemID), State);

%% @todo A and B are unknown.
%%      Melee uses a format similar to: AAAA--BBCCCC----DDDDDDDDEE----FF with
%%      AAAA the attack sound effect, BB the range, CCCC and DDDDDDDD unknown but related to angular range or similar, EE number of targets and FF the model.
%%      Bullets and tech weapons formats are unknown but likely use a slightly different format.
%% @todo Others probably want to see that you changed your weapon.
%% @todo Apparently B is always ItemID+1. Not sure why.
%% @todo Currently use a separate file for the data sent for the weapons.
%% @todo TargetGID and TargetLID must be validated, they're either the player's or his NPC characters.
%% @todo Handle NPC characters properly.
event({item_equip, ItemIndex, TargetGID, TargetLID, A, B}, #state{gid=GID}) ->
	case egs_user_model:item_nth(GID, ItemIndex) of
		{ItemID, Variables} when element(1, Variables) =:= psu_special_item_variables ->
			<< Category:8, _:24 >> = << ItemID:32 >>,
			psu_game:send(<< 16#01050300:32, 0:64, TargetGID:32/little, 0:64, 16#00011300:32, GID:32/little, 0:64,
				TargetGID:32/little, TargetLID:32/little, ItemIndex:8, 1:8, Category:8, A:8, B:32/little >>);
		{ItemID, Variables} when element(1, Variables) =:= psu_striking_weapon_item_variables ->
			#psu_item{data=Constants} = egs_items_db:read(ItemID),
			#psu_striking_weapon_item{attack_sound=Sound, hitbox_a=HitboxA, hitbox_b=HitboxB,
				hitbox_c=HitboxC, hitbox_d=HitboxD, nb_targets=NbTargets, effect=Effect, model=Model} = Constants,
			<< Category:8, _:24 >> = << ItemID:32 >>,
			{SoundInt, SoundType} = case Sound of
				{default, Val} -> {Val, 0};
				{custom, Val} -> {Val, 8}
			end,
			psu_game:send(<< 16#01050300:32, 0:64, TargetGID:32/little, 0:64, 16#00011300:32, GID:32/little, 0:64,
				TargetGID:32/little, TargetLID:32/little, ItemIndex:8, 1:8, Category:8, A:8, B:32/little,
				SoundInt:32/little, HitboxA:16, HitboxB:16, HitboxC:16, HitboxD:16, SoundType:4, NbTargets:4, 0:8, Effect:8, Model:8 >>);
		{ItemID, Variables} when element(1, Variables) =:= psu_trap_item_variables ->
			#psu_item{data=#psu_trap_item{effect=Effect, type=Type}} = egs_items_db:read(ItemID),
			<< Category:8, _:24 >> = << ItemID:32 >>,
			Bin = case Type of
				damage   -> << Effect:8, 16#0c0a05:24, 16#20140500:32, 16#0001c800:32, 16#10000000:32 >>;
				damage_g -> << Effect:8, 16#2c0505:24, 16#0c000600:32, 16#00049001:32, 16#10000000:32 >>;
				trap     -> << Effect:8, 16#0d0a05:24, 16#61140000:32, 16#0001c800:32, 16#10000000:32 >>;
				trap_g   -> << Effect:8, 16#4d0505:24, 16#4d000000:32, 16#00049001:32, 16#10000000:32 >>;
				trap_ex  -> << Effect:8, 16#490a05:24, 16#4500000f:32, 16#4b055802:32, 16#10000000:32 >>
			end,
			psu_game:send(<< 16#01050300:32, 0:64, TargetGID:32/little, 0:64, 16#00011300:32, GID:32/little, 0:64,
				TargetGID:32/little, TargetLID:32/little, ItemIndex:8, 1:8, Category:8, A:8, B:32/little, Bin/binary >>);
		undefined ->
			%% @todo Shouldn't be needed later when NPCs are handled correctly.
			ignore
	end;

event({item_set_trap, ItemIndex, TargetGID, TargetLID, A, B}, #state{gid=GID}) ->
	{ItemID, _Variables} = egs_user_model:item_nth(GID, ItemIndex),
	egs_user_model:item_qty_add(GID, ItemIndex, -1),
	<< Category:8, _:24 >> = << ItemID:32 >>,
	psu_game:send(<< 16#01050300:32, 0:64, TargetGID:32/little, 0:64, 16#00011300:32, GID:32/little, 0:64,
		TargetGID:32/little, TargetLID:32/little, ItemIndex:8, 9:8, Category:8, A:8, B:32/little >>);

%% @todo A and B are unknown.
%% @see item_equip
event({item_unequip, ItemIndex, TargetGID, TargetLID, A, B}, #state{gid=GID}) ->
	Category = case ItemIndex of
		% units would be 8, traps would be 12
		19 -> 2; % armor
		Y when Y =:= 5; Y =:= 6; Y =:= 7 -> 0; % clothes
		_ -> 1 % weapons
	end,
	psu_game:send(<< 16#01050300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32, GID:32/little-unsigned-integer,
		0:64, TargetGID:32/little-unsigned-integer, TargetLID:32/little-unsigned-integer, ItemIndex, 2, Category, A, B:32/little-unsigned-integer >>);

%% @todo Just ignore the meseta price for now and send the player where he wanna be!
event(lobby_transport_request, _State) ->
	psu_game:send_0c08(true);

event(lumilass_options_request, State=#state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	psu_proto:send_1a03(User, State);

%% @todo Probably replenish the player HP when entering a non-mission area rather than when aborting the mission?
event(mission_abort, State=#state{gid=GID}) ->
	psu_proto:send_1006(11, State),
	{ok, User} = egs_user_model:read(GID),
	%% delete the mission
	if	User#egs_user_model.instancepid =:= undefined -> ignore;
		true -> psu_instance:stop(User#egs_user_model.instancepid)
	end,
	%% full hp
	Character = User#egs_user_model.character,
	MaxHP = Character#characters.maxhp,
	NewCharacter = Character#characters{currenthp=MaxHP},
	NewUser = User#egs_user_model{character=NewCharacter, setid=0, instancepid=undefined},
	egs_user_model:write(NewUser),
	%% map change
	if	User#egs_user_model.areatype =:= mission ->
			PrevArea = User#egs_user_model.prev_area,
			event({area_change, PrevArea#psu_area.questid, PrevArea#psu_area.zoneid, PrevArea#psu_area.mapid, User#egs_user_model.prev_entryid}, State);
		true -> ignore
	end;

%% @todo Forward the mission start to other players of the same party, whatever their location is.
event({mission_start, QuestID}, State) ->
	log("mission start ~b", [QuestID]),
	psu_proto:send_1020(State),
	psu_game:send_1015(QuestID),
	psu_game:send_0c02();

%% @doc Force the invite of an NPC character while inside a mission. Mostly used by story missions.
%%      Note that the NPC is often removed and reinvited between block/cutscenes.
event({npc_force_invite, NPCid}, State=#state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	%% Create NPC.
	log("npc force invite ~p", [NPCid]),
	TmpNPCUser = psu_npc:user_init(NPCid, ((User#egs_user_model.character)#characters.mainlevel)#level.number),
	%% Create and join party.
	case User#egs_user_model.partypid of
		undefined ->
			{ok, PartyPid} = psu_party:start_link(GID);
		PartyPid ->
			ignore
	end,
	{ok, PartyPos} = psu_party:join(PartyPid, npc, TmpNPCUser#egs_user_model.id),
	#egs_user_model{instancepid=InstancePid, area=Area, entryid=EntryID, pos=Pos} = User,
	NPCUser = TmpNPCUser#egs_user_model{lid=PartyPos, partypid=PartyPid, instancepid=InstancePid, areatype=mission, area=Area, entryid=EntryID, pos=Pos},
	egs_user_model:write(NPCUser),
	egs_user_model:write(User#egs_user_model{partypid=PartyPid}),
	%% Send stuff.
	Character = NPCUser#egs_user_model.character,
	SentNPCCharacter = Character#characters{gid=NPCid, npcid=NPCid},
	SentNPCUser = NPCUser#egs_user_model{character=SentNPCCharacter},
	psu_proto:send_010d(SentNPCUser, State),
	psu_proto:send_0201(SentNPCUser, State),
	psu_proto:send_0215(0, State),
	psu_game:send_0a04(SentNPCUser#egs_user_model.id),
	psu_game:send_022c(0, 16#12),
	psu_game:send_1004(npc_mission, SentNPCUser, PartyPos),
	psu_game:send_100f((SentNPCUser#egs_user_model.character)#characters.npcid, PartyPos),
	psu_game:send_1601(PartyPos);

%% @todo Also at the end send a 101a (NPC:16, PartyPos:16, ffffffff). Not sure about PartyPos.
event({npc_invite, NPCid}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	%% Create NPC.
	log("invited npcid ~b", [NPCid]),
	TmpNPCUser = psu_npc:user_init(NPCid, ((User#egs_user_model.character)#characters.mainlevel)#level.number),
	%% Create and join party.
	case User#egs_user_model.partypid of
		undefined ->
			{ok, PartyPid} = psu_party:start_link(GID),
			psu_game:send_022c(0, 16#12);
		PartyPid ->
			ignore
	end,
	{ok, PartyPos} = psu_party:join(PartyPid, npc, TmpNPCUser#egs_user_model.id),
	NPCUser = TmpNPCUser#egs_user_model{lid=PartyPos, partypid=PartyPid},
	egs_user_model:write(NPCUser),
	egs_user_model:write(User#egs_user_model{partypid=PartyPid}),
	%% Send stuff.
	Character = NPCUser#egs_user_model.character,
	SentNPCCharacter = Character#characters{gid=NPCid, npcid=NPCid},
	SentNPCUser = NPCUser#egs_user_model{character=SentNPCCharacter},
	psu_game:send_1004(npc_invite, SentNPCUser, PartyPos),
	psu_game:send_101a(NPCid, PartyPos);

%% @todo Should be 0115(money) 010a03(confirm sale).
event({npc_shop_buy, ShopItemIndex, QuantityOrColor}, State=#state{gid=GID}) ->
	ShopID = egs_user_model:shop_get(GID),
	ItemID = lists:nth(ShopItemIndex + 1, proplists:get_value(ShopID, ?SHOPS)),
	log("npc shop ~p buy itemid ~8.16.0b quantity/color+1 ~p", [ShopID, ItemID, QuantityOrColor]),
	#psu_item{name=Name, rarity=Rarity, buy_price=BuyPrice, sell_price=SellPrice, data=Constants} = egs_items_db:read(ItemID),
	Variables = case element(1, Constants) of
		psu_clothing_item ->
			if	QuantityOrColor >= 1, QuantityOrColor =< 10 ->
				#psu_clothing_item_variables{color=QuantityOrColor - 1}
			end;
		psu_consumable_item ->
			#psu_consumable_item_variables{quantity=QuantityOrColor};
		psu_parts_item ->
			#psu_parts_item_variables{};
		psu_special_item ->
			#psu_special_item_variables{};
		psu_striking_weapon_item ->
			#psu_striking_weapon_item{pp=PP, shop_element=Element} = Constants,
			#psu_striking_weapon_item_variables{current_pp=PP, max_pp=PP, element=Element};
		psu_trap_item ->
			#psu_trap_item_variables{quantity=QuantityOrColor}
	end,
	egs_user_model:money_add(GID, -1 * BuyPrice),
	ItemUUID = egs_user_model:item_add(GID, ItemID, Variables),
	{ok, User} = egs_user_model:read(GID),
	psu_proto:send_0115(User#egs_user_model{lid=0}, State), %% @todo This one is apparently broadcast to everyone in the same zone.
	%% @todo Following command isn't done 100% properly.
	UCS2Name = << << X:8, 0:8 >> || X <- Name >>,
	NamePadding = 8 * (46 - byte_size(UCS2Name)),
	<< Category:8, _:24 >> = << ItemID:32 >>,
	RarityInt = Rarity - 1,
	psu_game:send(<< 16#010a0300:32, 0:64, GID:32/little, 0:64, 16#00011300:32, GID:32/little, 0:64,
		GID:32/little, 0:32, 2:16/little, 0:16, (psu_game:build_item_variables(ItemID, ItemUUID, Variables))/binary,
		UCS2Name/binary, 0:NamePadding, RarityInt:8, Category:8, SellPrice:32/little, (psu_game:build_item_constants(Constants))/binary >>);

%% @todo Currently send the normal items shop for all shops, differentiate.
event({npc_shop_enter, ShopID}, #state{gid=GID}) ->
	log("npc shop enter ~p", [ShopID]),
	egs_user_model:shop_enter(GID, ShopID),
	psu_game:send_010a(proplists:get_value(ShopID, ?SHOPS));

event({npc_shop_leave, ShopID}, #state{gid=GID}) ->
	log("npc shop leave ~p", [ShopID]),
	egs_user_model:shop_leave(GID),
	psu_game:send(<< 16#010a0300:32, 0:64, GID:32/little-unsigned-integer, 0:64, 16#00011300:32,
		GID:32/little-unsigned-integer, 0:64, GID:32/little-unsigned-integer, 0:32 >>);

%% @todo Should be 0115(money) 010a03(confirm sale).
event({npc_shop_sell, InventoryItemIndex, Quantity}, _State) ->
	log("npc shop sell itemindex ~p quantity ~p", [InventoryItemIndex, Quantity]);

%% @todo First 1a02 value should be non-0.
%% @todo Could the 2nd 1a02 parameter simply be the shop type or something?
%% @todo Although the values replied should be right, they seem mostly ignored by the client.
event({npc_shop_request, ShopID}, State) ->
	log("npc shop request ~p", [ShopID]),
	case ShopID of
		80 -> psu_proto:send_1a02(17, 17, 3, 9, State); %% lumilass
		90 -> psu_proto:send_1a02(5, 1, 4, 5, State);   %% parum weapon grinding
		91 -> psu_proto:send_1a02(5, 5, 4, 7, State);   %% tenora weapon grinding
		92 -> psu_proto:send_1a02(5, 8, 4, 0, State);   %% yohmei weapon grinding
		93 -> psu_proto:send_1a02(5, 18, 4, 0, State);  %% kubara weapon grinding
		_  -> psu_proto:send_1a02(0, 1, 0, 0, State)
	end;

%% @todo Not sure what are those hardcoded values.
event({object_boss_gate_activate, ObjectID}, _State) ->
	psu_game:send_1213(ObjectID, 0),
	psu_game:send_1215(2, 16#7008),
	%% @todo Following sent after the warp?
	psu_game:send_1213(37, 0),
	%% @todo Why resend this?
	psu_game:send_1213(ObjectID, 0);

event({object_boss_gate_enter, ObjectID}, _State) ->
	psu_game:send_1213(ObjectID, 1);

%% @todo Do we need to send something back here?
event({object_boss_gate_leave, _ObjectID}, _State) ->
	ignore;

event({object_box_destroy, ObjectID}, _State) ->
	psu_game:send_1213(ObjectID, 3);

%% @todo Second send_1211 argument should be User#egs_user_model.lid. Fix when it's correctly handled.
event({object_chair_sit, ObjectTargetID}, _State) ->
	%~ {ok, User} = egs_user_model:read(get(gid)),
	psu_game:send_1211(ObjectTargetID, 0, 8, 0);

%% @todo Second psu_game:send_1211 argument should be User#egs_user_model.lid. Fix when it's correctly handled.
event({object_chair_stand, ObjectTargetID}, _State) ->
	%~ {ok, User} = egs_user_model:read(get(gid)),
	psu_game:send_1211(ObjectTargetID, 0, 8, 2);

event({object_crystal_activate, ObjectID}, _State) ->
	psu_game:send_1213(ObjectID, 1);

%% @doc Server-side event.
event({object_event_trigger, BlockID, EventID}, _State) ->
	psu_game:send_1205(EventID, BlockID, 0);

event({object_goggle_target_activate, ObjectID}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	{BlockID, EventID} = psu_instance:std_event(User#egs_user_model.instancepid, (User#egs_user_model.area)#psu_area.zoneid, ObjectID),
	psu_game:send_1205(EventID, BlockID, 0),
	psu_game:send_1213(ObjectID, 8);

%% @todo Make NPC characters heal too.
event({object_healing_pad_tick, [_PartyPos]}, State=#state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	Character = User#egs_user_model.character,
	if	Character#characters.currenthp =:= Character#characters.maxhp -> ignore;
		true ->
			NewHP = Character#characters.currenthp + Character#characters.maxhp div 10,
			NewHP2 = if NewHP > Character#characters.maxhp -> Character#characters.maxhp; true -> NewHP end,
			User2 = User#egs_user_model{character=Character#characters{currenthp=NewHP2}},
			egs_user_model:write(User2),
			psu_proto:send_0117(User2#egs_user_model{lid=0}, State),
			psu_proto:send_0111(User2#egs_user_model{lid=0}, 4, State)
	end;

event({object_key_console_enable, ObjectID}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	{BlockID, [EventID|_]} = psu_instance:std_event(User#egs_user_model.instancepid, (User#egs_user_model.area)#psu_area.zoneid, ObjectID),
	psu_game:send_1205(EventID, BlockID, 0),
	psu_game:send_1213(ObjectID, 1);

event({object_key_console_init, ObjectID}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	{BlockID, [_, EventID, _]} = psu_instance:std_event(User#egs_user_model.instancepid, (User#egs_user_model.area)#psu_area.zoneid, ObjectID),
	psu_game:send_1205(EventID, BlockID, 0);

event({object_key_console_open_gate, ObjectID}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	{BlockID, [_, _, EventID]} = psu_instance:std_event(User#egs_user_model.instancepid, (User#egs_user_model.area)#psu_area.zoneid, ObjectID),
	psu_game:send_1205(EventID, BlockID, 0),
	psu_game:send_1213(ObjectID, 1);

%% @todo Now that it's separate from object_key_console_enable, handle it better than that, don't need a list of events.
event({object_key_enable, ObjectID}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	{BlockID, [EventID|_]} = psu_instance:std_event(User#egs_user_model.instancepid, (User#egs_user_model.area)#psu_area.zoneid, ObjectID),
	psu_game:send_1205(EventID, BlockID, 0),
	psu_game:send_1213(ObjectID, 1);

%% @todo Some switch objects apparently work differently, like the light switch in Mines in MAG'.
event({object_switch_off, ObjectID}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	{BlockID, EventID} = psu_instance:std_event(User#egs_user_model.instancepid, (User#egs_user_model.area)#psu_area.zoneid, ObjectID),
	psu_game:send_1205(EventID, BlockID, 1),
	psu_game:send_1213(ObjectID, 0);

event({object_switch_on, ObjectID}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	{BlockID, EventID} = psu_instance:std_event(User#egs_user_model.instancepid, (User#egs_user_model.area)#psu_area.zoneid, ObjectID),
	psu_game:send_1205(EventID, BlockID, 0),
	psu_game:send_1213(ObjectID, 1);

event({object_vehicle_boost_enable, ObjectID}, _State) ->
	psu_game:send_1213(ObjectID, 1);

event({object_vehicle_boost_respawn, ObjectID}, _State) ->
	psu_game:send_1213(ObjectID, 0);

%% @todo Second send_1211 argument should be User#egs_user_model.lid. Fix when it's correctly handled.
event({object_warp_take, BlockID, ListNb, ObjectNb}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	Pos = psu_instance:warp_event(User#egs_user_model.instancepid, (User#egs_user_model.area)#psu_area.zoneid, BlockID, ListNb, ObjectNb),
	NewUser = User#egs_user_model{pos=Pos},
	egs_user_model:write(NewUser),
	psu_game:send_0503(User#egs_user_model.pos),
	psu_game:send_1211(16#ffffffff, 0, 14, 0);

event({party_remove_member, PartyPos}, State=#state{gid=GID}) ->
	log("party remove member ~b", [PartyPos]),
	{ok, DestUser} = egs_user_model:read(GID),
	{ok, RemovedGID} = psu_party:get_member(DestUser#egs_user_model.partypid, PartyPos),
	psu_party:remove_member(DestUser#egs_user_model.partypid, PartyPos),
	{ok, RemovedUser} = egs_user_model:read(RemovedGID),
	case (RemovedUser#egs_user_model.character)#characters.type of
		npc -> egs_user_model:delete(RemovedGID);
		_ -> ignore
	end,
	psu_proto:send_1006(8, PartyPos, State),
	psu_game:send_0204(DestUser, RemovedUser, 1),
	psu_proto:send_0215(0, State);

event({player_options_change, Options}, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	file:write_file(io_lib:format("save/~s/~b-character.options", [User#egs_user_model.folder, (User#egs_user_model.character)#characters.slot]), Options);

%% @todo If the player has a scape, use it! Otherwise red screen.
%% @todo Right now we force revive with a dummy HP value.
event(player_death, State=#state{gid=GID}) ->
	% @todo send_0115(get(gid), 16#ffffffff, LV=1, EXP=idk, Money=1000), % apparently sent everytime you die...
	%% use scape:
	NewHP = 10,
	{ok, User} = egs_user_model:read(GID),
	Char = User#egs_user_model.character,
	User2 = User#egs_user_model{character=Char#characters{currenthp=NewHP}},
	egs_user_model:write(User2),
	psu_proto:send_0117(User2#egs_user_model{lid=0}, State),
	psu_proto:send_1022(User2, State);
	%% red screen with return to lobby choice:
	%~ psu_proto:send_0111(User2, 3, 1, State);

%% @todo Refill the player's HP to maximum, remove SEs etc.
event(player_death_return_to_lobby, State=#state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	PrevArea = User#egs_user_model.prev_area,
	event({area_change, PrevArea#psu_area.questid, PrevArea#psu_area.zoneid, PrevArea#psu_area.mapid, User#egs_user_model.prev_entryid}, State);

event(player_type_availability_request, State) ->
	psu_proto:send_1a07(State);

event(player_type_capabilities_request, _State) ->
	psu_game:send_0113();

event(ppcube_request, _State) ->
	psu_game:send_1a04();

%% @doc Uni cube handler.
event(unicube_request, _State) ->
	psu_game:send_021e();

%% @doc Uni selection handler. Selecting anything reloads the character which will then be sent to its associated area.
%% @todo When selecting 'Your room', load a default room.
%% @todo When selecting 'Reload', reload the character in the current lobby.
%% @todo Delete NPC characters and stop the party on entering myroom too.
event({unicube_select, Selection, EntryID}, #state{gid=GID}) ->
	case Selection of
		cancel -> ignore;
		16#ffffffff ->
			log("uni selection (my room)"),
			psu_game:send_0230(),
			% 0220
			{ok, User} = egs_user_model:read(GID),
			User2 = User#egs_user_model{area=#psu_area{questid=1120000, zoneid=0, mapid=100}, entryid=0},
			egs_user_model:write(User2),
			psu_game:char_load(User2);
		_UniID ->
			log("uni selection (reload)"),
			psu_game:send_0230(),
			% 0220
			{ok, User} = egs_user_model:read(GID),
			case User#egs_user_model.partypid of
				undefined ->
					ignore;
				PartyPid ->
					%% @todo Replace stop by leave when leaving stops the party correctly when nobody's there anymore.
					%~ psu_party:leave(User#egs_user_model.partypid, User#egs_user_model.id)
					{ok, NPCList} = psu_party:get_npc(PartyPid),
					[egs_user_model:delete(NPCGID) || {_Spot, NPCGID} <- NPCList],
					psu_party:stop(PartyPid)
			end,
			User2 = User#egs_user_model{entryid=EntryID},
			egs_user_model:write(User2),
			psu_game:char_load(User2)
	end.

%% Internal.

%% @doc Trigger many events.
events(Events, State) ->
	[event(Event, State) || Event <- Events],
	ok.

%% @doc Log message to the console.
log(Message) ->
	io:format("~p: ~s~n", [get(gid), Message]).

log(Message, Format) ->
	FormattedMessage = io_lib:format(Message, Format),
	log(FormattedMessage).

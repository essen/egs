%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Handle game clients.
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

-module(psu_game).
-compile(export_all). %% @todo Temporarily export all until send_xxxx functions are moved to psu_proto.

-include("include/records.hrl").

%% @doc Load and send the character information to the client.
%% @todo Move this whole function directly to psu_proto, probably.
char_load(User, Client) ->
	psu_proto:send_0d01(User#users.character, Client),
	%% 0246
	psu_proto:send_0a0a((User#users.character)#characters.inventory, Client),
	psu_proto:send_1006(5, 0, Client), %% @todo The 0 here is PartyPos, save it in User.
	psu_proto:send_1005(User#users.character, Client),
	psu_proto:send_1006(12, Client),
	psu_proto:send_0210(Client),
	psu_proto:send_0222(User#users.uni, Client),
	psu_proto:send_1500(User#users.character, Client),
	psu_proto:send_1501(Client),
	psu_proto:send_1512(Client),
	%% 0303
	psu_proto:send_1602(Client),
	psu_proto:send_021b(Client).

%% @doc Load the given map as a standard lobby.
area_load(QuestID, ZoneID, MapID, EntryID, Client) ->
	{ok, OldUser} = egs_users:read(Client#client.gid),
	{OldQuestID, OldZoneID, _OldMapID} = OldUser#users.area,
	QuestChange = OldQuestID /= QuestID,
	ZoneChange = if OldQuestID =:= QuestID, OldZoneID =:= ZoneID -> false; true -> true end,
	AreaType = egs_quests_db:area_type(QuestID, ZoneID),
	AreaShortName = "dammy", %% @todo Load the short name from egs_quests_db.
	{IsSeasonal, SeasonID} = egs_seasons:read(QuestID),
	User = OldUser#users{areatype=AreaType, area={QuestID, ZoneID, MapID}, entryid=EntryID},
	egs_users:write(User), %% @todo Booh ugly! But temporary.
	%% Load the quest.
	User2 = if QuestChange ->
			psu_proto:send_0c00(User, Client),
			psu_proto:send_020e(egs_quests_db:quest_nbl(QuestID), Client),
			User#users{questpid=egs_universes:lobby_pid(User#users.uni, QuestID)};
		true -> User
	end,
	%% Load the zone.
	Client1 = if ZoneChange ->
			ZonePid = egs_quests:zone_pid(User2#users.questpid, ZoneID),
			egs_zones:leave(User2#users.zonepid, User2#users.gid),
			NewLID = egs_zones:enter(ZonePid, User2#users.gid),
			NewClient = Client#client{lid=NewLID},
			{ok, User3} = egs_users:read(User2#users.gid),
			psu_proto:send_0a05(NewClient),
			psu_proto:send_0111(User3, 6, NewClient),
			psu_proto:send_010d(User3, NewClient),
			psu_proto:send_0200(ZoneID, AreaType, NewClient),
			psu_proto:send_020f(egs_quests_db:zone_nbl(QuestID, ZoneID), egs_zones:setid(ZonePid), SeasonID, NewClient),
			NewClient;
		true ->
			User3 = User2,
			Client
	end,
	%% Save the user.
	egs_users:write(User3),
	%% Load the player location.
	Client2 = Client1#client{areanb=Client#client.areanb + 1},
	psu_proto:send_0205(User3, IsSeasonal, Client2),
	psu_proto:send_100e(User3#users.area, User3#users.entryid, AreaShortName, Client2),
	%% Load the zone objects.
	if ZoneChange ->
			psu_proto:send_1212(Client2); %% @todo Only sent if there is a set file.
		true -> ignore
	end,
	%% Load the player.
	psu_proto:send_0201(User3, Client2),
	if ZoneChange ->
			psu_proto:send_0a06(User3, Client2),
			%% Load the other players in the zone.
			OtherPlayersGID = egs_zones:get_all_players(User3#users.zonepid, User3#users.gid),
			if	OtherPlayersGID =:= [] -> ignore;
				true ->
					OtherPlayers = egs_users:select(OtherPlayersGID),
					psu_proto:send_0233(OtherPlayers, Client)
			end;
		true -> ignore
	end,
	%% End of loading.
	Client3 = Client2#client{areanb=Client2#client.areanb + 1},
	psu_proto:send_0208(Client3),
	psu_proto:send_0236(Client3),
	%% @todo Load APC characters.
	{ok, Client3}.

%% @todo Don't change the NPC info unless you are the leader!
npc_load(_Leader, [], _Client) ->
	ok;
npc_load(Leader, [{PartyPos, NPCGID}|NPCList], Client) ->
	{ok, OldNPCUser} = egs_users:read(NPCGID),
	#users{instancepid=InstancePid, area=Area, entryid=EntryID, pos=Pos} = Leader,
	NPCUser = OldNPCUser#users{lid=PartyPos, instancepid=InstancePid, areatype=mission, area=Area, entryid=EntryID, pos=Pos},
	%% @todo This one on mission end/abort?
	%~ OldNPCUser#users{lid=PartyPos, instancepid=undefined, areatype=AreaType, area={0, 0, 0}, entryid=0, pos={0.0, 0.0, 0.0, 0}}
	egs_users:write(NPCUser),
	psu_proto:send_010d(NPCUser, Client),
	psu_proto:send_0201(NPCUser, Client),
	psu_proto:send_0215(0, Client),
	psu_proto:send_0a04(NPCUser#users.gid, Client),
	psu_proto:send_1004(npc_mission, NPCUser, PartyPos, Client),
	psu_proto:send_100f((NPCUser#users.character)#characters.npcid, PartyPos, Client),
	psu_proto:send_1601(PartyPos, Client),
	psu_proto:send_1016(PartyPos, Client),
	npc_load(Leader, NPCList, Client).

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

%% @doc Enter all maps event. Set the right music for the season.
event entr_unit ->
	obj.get_eventflag,
	case
		      0 -> push 0, push  46, sound.play_bgm; %% party
		      1 -> push 0, push  47, sound.play_bgm; %% new year
		      2 -> push 0, push  48, sound.play_bgm; %% valentine
		      3 -> push 0, push  49, sound.play_bgm; %% white day
		      4 -> push 0, push  50, sound.play_bgm; %% spring
		      5 -> push 0, push  47, sound.play_bgm; %% easter
%%		      6 -> push 0, push  12, sound.play_bgm; %% parum unification
		      7 -> push 0, push 121, sound.play_bgm; %% sonic
%%		      8 -> push 0, push  14, sound.play_bgm; %% holy light
%%		      9 -> push 0, push  50, sound.play_bgm; %% fireworks
		     10 -> push 0, push  50, sound.play_bgm; %% autumn
		     11 -> push 0, push  51, sound.play_bgm; %% halloween
%%		     12 -> push 0, push  16, sound.play_bgm; %% native
		     13 -> push 0, push  52, sound.play_bgm; %% christmas
		     14 -> push 0, push  52, sound.play_bgm; %% winter
		     15 -> push 0, push  44, sound.play_bgm; %% wedding
		default -> push 0, push  40, sound.play_bgm  %% clyez city
	end.

%% @doc Enter map 1 event. Initialize the labels.
event entr_unit0001 ->
	push  0, push 0, obj.set_caption, %% Room entrances.
	push  2, push 1, obj.set_caption, %% Exits to 2nd floor.
	push  6, push 2, obj.set_caption. %% Elevators.

%% @doc Enter map 2 event. Initialize the labels.
event entr_unit0002 ->
	push  1, push 0, obj.set_caption, %% Exits to 1st floor.
	push  3, push 1, obj.set_caption, %% Exits to 3rd floor.
	push  6, push 2, obj.set_caption, %% Elevators.
	push  7, push 3, obj.set_caption, %% Grind shop.
	push  8, push 4, obj.set_caption, %% Materials shop.
	push  9, push 5, obj.set_caption, %% Variety shop.
	push 10, push 6, obj.set_caption, %% Armors shop.
	push 11, push 7, obj.set_caption, %% Weapons shop.
	push 12, push 8, obj.set_caption. %% Items shop.

%% @doc Enter map 3 event. Initialize the labels.
event entr_unit0003 ->
	push  2, push 0, obj.set_caption, %% Exits to 2nd floor.
	push  4, push 1, obj.set_caption, %% Exits to 4th floor.
	push  6, push 2, obj.set_caption, %% Elevators.
	push 13, push 3, obj.set_caption, %% Lumilass.
	push 14, push 4, obj.set_caption, %% Clothes shop.
	push 15, push 5, obj.set_caption, %% Parts shop.
	push 16, push 6, obj.set_caption, %% Tenants wanted.
	push 17, push 7, obj.set_caption. %% Club.

%% @doc Enter map 4 event. Initialize the labels and NPCs.
event entr_unit0004 ->
	push  3, push 0, obj.set_caption, %% Exits to 3rd floor.
	push  5, push 1, obj.set_caption, %% Exits to 5th floor.
	push  6, push 2, obj.set_caption, %% Elevators.
	push 18, push 3, obj.set_caption, %% Linear Line.
	push 19, push 4, obj.set_caption, %% Space Docks.
	push 20, push 5, obj.set_caption, %% Neudaiz Spaceport.
	push 21, push 6, obj.set_caption, %% Moatoob Spaceport.
	push 22, push 7, obj.set_caption, %% Parum Spaceport.
	push 0, npc.talk_on, %% Linear Line NPC.
	push 1, npc.talk_on. %% Space Docks NPC.

%% @doc Parum Spaceport exit.
event goal_unit0004_num002 ->
	push "SV_WK_LC_RETURN", push 1, work.chrwork_set.

%% @doc Moatoob Spaceport exit.
event goal_unit0004_num003 ->
	push "SV_WK_LC_RETURN", push 3, work.chrwork_set.

%% @doc Neudaiz Spaceport exit.
event goal_unit0004_num004 ->
	push "SV_WK_LC_RETURN", push 2, work.chrwork_set.

%% Elevators.

num_var nElevatorEntry.

%% Map 1.

function coli_unit0001_elevator ->
	player.pad_off,
	push 39, %% return
	push 30, %% 1st floor
	push 31, %% 2nd floor
	push 33, %% 3rd floor
	push 35, %% 4th floor
	push 37, %% 5th floor
	push 24, %% stringid question
	push 6,  %% number of options
	push 4,  %% selected option: 1st floor
	mes.select_win_b,
	case
		0 -> push 11, push 5, push 0, player.change_unit;
		1 -> push 0, push 4, num_get nElevatorEntry, player.change_unit;
		2 -> push 0, push 3, num_get nElevatorEntry, player.change_unit;
		3 -> push 0, push 2, num_get nElevatorEntry, player.change_unit
	end,
	player.pad_on.

event coli_unit0001_obje020 ->
	push 20,
	num_set nElevatorEntry,
	coli_unit0001_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0001_obje021 ->
	push 21,
	num_set nElevatorEntry,
	coli_unit0001_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

%% Map 2.

function coli_unit0002_elevator ->
	player.pad_off,
	push 39, %% return
	push 29, %% 1st floor
	push 32, %% 2nd floor
	push 33, %% 3rd floor
	push 35, %% 4th floor
	push 37, %% 5th floor
	push 25, %% stringid question
	push 6,  %% number of options
	push 3,  %% selected option: 2nd floor
	mes.select_win_b,
	case
		0 -> push 11, push 5, push 0, player.change_unit;
		1 -> push 0, push 4, num_get nElevatorEntry, player.change_unit;
		2 -> push 0, push 3, num_get nElevatorEntry, player.change_unit;
		4 -> push 0, push 1, num_get nElevatorEntry, player.change_unit
	end,
	player.pad_on.

event coli_unit0002_obje020 ->
	push 20,
	num_set nElevatorEntry,
	coli_unit0002_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0002_obje021 ->
	push 21,
	num_set nElevatorEntry,
	coli_unit0002_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0002_obje022 ->
	push 22,
	num_set nElevatorEntry,
	coli_unit0002_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0002_obje023 ->
	push 23,
	num_set nElevatorEntry,
	coli_unit0002_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

%% Map 3.

function coli_unit0003_elevator ->
	player.pad_off,
	push 39, %% return
	push 29, %% 1st floor
	push 31, %% 2nd floor
	push 34, %% 3rd floor
	push 35, %% 4th floor
	push 37, %% 5th floor
	push 26, %% stringid question
	push 6,  %% number of options
	push 2,  %% selected option: 3rd floor
	mes.select_win_b,
	case
		0 -> push 11, push 5, push 0, player.change_unit;
		1 -> push 0, push 4, num_get nElevatorEntry, player.change_unit;
		3 -> push 0, push 2, num_get nElevatorEntry, player.change_unit;
		4 -> push 0, push 1, num_get nElevatorEntry, player.change_unit
	end,
	player.pad_on.

event coli_unit0003_obje020 ->
	push 20,
	num_set nElevatorEntry,
	coli_unit0003_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0003_obje021 ->
	push 21,
	num_set nElevatorEntry,
	coli_unit0003_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0003_obje022 ->
	push 22,
	num_set nElevatorEntry,
	coli_unit0003_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0003_obje023 ->
	push 23,
	num_set nElevatorEntry,
	coli_unit0003_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

%% Map 4.

function coli_unit0004_elevator ->
	player.pad_off,
	push 39, %% return
	push 29, %% 1st floor
	push 31, %% 2nd floor
	push 33, %% 3rd floor
	push 36, %% 4th floor
	push 37, %% 5th floor
	push 27, %% stringid question
	push 6,  %% number of options
	push 1,  %% selected option: 4th floor
	mes.select_win_b,
	case
		0 -> push 11, push 5, push 0, player.change_unit;
		2 -> push 0, push 3, num_get nElevatorEntry, player.change_unit;
		3 -> push 0, push 2, num_get nElevatorEntry, player.change_unit;
		4 -> push 0, push 1, num_get nElevatorEntry, player.change_unit
	end,
	player.pad_on.

event coli_unit0004_obje020 ->
	push 20,
	num_set nElevatorEntry,
	coli_unit0004_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0004_obje021 ->
	push 21,
	num_set nElevatorEntry,
	coli_unit0004_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0004_obje022 ->
	push 22,
	num_set nElevatorEntry,
	coli_unit0004_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

event coli_unit0004_obje023 ->
	push 23,
	num_set nElevatorEntry,
	coli_unit0004_elevator,
	num_get nElevatorEntry,
	obj.coli_end.

%% NPCs.

num_var nTransportNPC.

function coli_unit0004_transport_npc ->
	player.pad_off,
	%% Welcome!
	push 0,    %% probably number of embedded int vars
	push 0,    %% probably number of embedded float vars
	push 0,    %% probably number of embedded string vars
	push 40,   %% stringid
	push 0.50, %% max horizontal proportion?
	push 0.25, %% max vertical proportion?
	num_get nTransportNPC,
	mes.fukidasi_npc,
	%% Where do you want to go?
	push 39, %% return
	push 47, %% Falz Memoria
	push 46, %% Rykros
	push 45, %% Dallgun Viewing Plaza
	push 44, %% HIVE No. 3
	push 43, %% Aurorey Viewing Plaza
	push 42, %% Linear Line: Transfer Terminal
	push 41, %% stringid question
	push 7,  %% number of options
	push 0,  %% selected option: who cares
	mes.select_win_b,
	case
		      0 -> push 1, push 9001, push 0, player.change_unit;
		      1 -> push 2, push 9000, push 0, player.change_unit;
		      2 -> push 3, push 9102, push 0, player.change_unit;
		      3 -> push 4, push 9010, push 0, player.change_unit;
		      4 -> push 7, push 9200, push 0, player.change_unit;
		      5 -> push 7, push 9202, push 0, player.change_unit;
		default -> %% Please come back later.
			push 0,
			push 0,
			push 0,
			push 48,
			push 0.50,
			push 0.25,
			num_get nTransportNPC,
			mes.fukidasi_npc
	end,
	player.pad_on.

event coli_unit0004_npc000 ->
	push 0,
	num_set nTransportNPC,
	coli_unit0004_transport_npc,
	num_get nTransportNPC,
	npc.coli_end.

event coli_unit0004_npc001 ->
	push 1,
	num_set nTransportNPC,
	coli_unit0004_transport_npc,
	num_get nTransportNPC,
	npc.coli_end.

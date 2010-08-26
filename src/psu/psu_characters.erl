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

-module(psu_characters).
-export([
	character_tuple_to_binary/1, character_user_to_binary/1, class_atom_to_binary/1, class_binary_to_atom/1,
	gender_atom_to_binary/1, gender_binary_to_atom/1, options_binary_to_tuple/1, options_tuple_to_binary/1,
	race_atom_to_binary/1, race_binary_to_atom/1, se_list_to_binary/1, stats_tuple_to_binary/1, validate_name/1
]).

-include("include/records.hrl").

%% @doc Convert a character tuple into a binary to be sent to clients.
%%      Only contains the actually saved data, not the stats and related information.

character_tuple_to_binary(Tuple) ->
	#characters{type=Type, name=Name, race=Race, gender=Gender, class=Class, appearance=Appearance,
		mainlevel=Level, blastbar=BlastBar, luck=Luck, money=Money, playtime=PlayTime} = Tuple,
	#level{number=LV, exp=EXP} = Level,
	RaceBin = race_atom_to_binary(Race),
	GenderBin = gender_atom_to_binary(Gender),
	ClassBin = class_atom_to_binary(Class),
	AppearanceBin = psu_appearance:tuple_to_binary(Race, Appearance),
	FooterBin = case Type of
		npc ->
			<<	16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32,
				16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32,
				16#4e4f4630:32, 16#08000000:32, 0:32, 0:32, 16#4e454e44:32 >>;
		_ -> %% @todo Handle classes.
			<<	0:160,
				16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32,
				16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32, 16#01000000:32 >>
	end,
	<<	Name/binary, RaceBin:8, GenderBin:8, ClassBin:8, AppearanceBin/binary, LV:32/little-unsigned-integer, BlastBar:16/little-unsigned-integer,
		Luck:8, 0:40, EXP:32/little-unsigned-integer, 0:32, Money:32/little-unsigned-integer, PlayTime:32/little-unsigned-integer, FooterBin/binary >>.

%% @doc Convert a character tuple into a binary to be sent to clients.
%%      Contains everything from character_tuple_to_binary/1 along with location, stats, SE and more.
%% @todo The second StatsBin seems unused. Not sure what it's for.
%% @todo Find out what the big block of 0 is at the end.
%% @todo The value before IntDir seems to be the player's current animation. 01 stand up, 08 ?, 17 normal sit

character_user_to_binary(User) ->
	#egs_user_model{id=CharGID, lid=CharLID, character=Character, pos=#pos{x=X, y=Y, z=Z, dir=Dir}, area={psu_area, QuestID, ZoneID, MapID}, entryid=EntryID,
		prev_area={psu_area, PrevQuestID, PrevZoneID, PrevMapID}, prev_entryid=PrevEntryID} = User,
	#characters{type=Type, mainlevel=Level, stats=Stats, se=SE, currenthp=CurrentHP, maxhp=MaxHP} = Character,
	#level{number=LV} = Level,
	CharBin = psu_characters:character_tuple_to_binary(Character),
	StatsBin = psu_characters:stats_tuple_to_binary(Stats),
	SEBin = psu_characters:se_list_to_binary(SE),
	EXPNextLevel = 100,
	EXPPreviousLevel = 0,
	IntDir = trunc(Dir * 182.0416),
	TypeID = case Type of npc -> 16#00001d00; _ -> 16#00001200 end,
	NPCStuff = case Type of npc -> 16#01ff0700; _ -> 16#0000ffff end,
	<<	TypeID:32, CharGID:32/little-unsigned-integer, 0:64, CharLID:32/little-unsigned-integer, NPCStuff:32, QuestID:32/little-unsigned-integer,
		ZoneID:32/little-unsigned-integer, MapID:32/little-unsigned-integer, EntryID:32/little-unsigned-integer,
		16#0100:16, IntDir:16/little-unsigned-integer, X:32/little-float, Y:32/little-float, Z:32/little-float, 0:64,
		PrevQuestID:32/little-unsigned-integer, PrevZoneID:32/little-unsigned-integer, PrevMapID:32/little-unsigned-integer, PrevEntryID:32/little-unsigned-integer,
		CharBin/binary, EXPNextLevel:32/little-unsigned-integer, EXPPreviousLevel:32/little-unsigned-integer, MaxHP:32/little-unsigned-integer, % not sure if this one is current or max
		StatsBin/binary, 0:32, SEBin/binary, 0:32, LV:32/little-unsigned-integer, StatsBin/binary, CurrentHP:32/little-unsigned-integer, MaxHP:32/little-unsigned-integer,
		0:1344, 16#0000803f:32, 0:64, 16#0000803f:32, 0:64, 16#0000803f:32, 0:64, 16#0000803f:32, 0:64, 16#0000803f:32, 0:160, 16#0000803f:32, 0:352 >>.

%% @doc Convert a class atom into a binary to be sent to clients.

class_atom_to_binary(Class) ->
	case Class of
		hunter			-> 0;
		ranger			-> 1;
		force			-> 2;
		fighgunner		-> 3;
		guntecher		-> 4;
		wartecher		-> 5;
		fortefighter	-> 6;
		fortegunner		-> 7;
		fortetecher		-> 8;
		protranser		-> 9;
		acrofighter		-> 10;
		acrotecher		-> 11;
		fighmaster		-> 12;
		gunmaster		-> 13;
		masterforce		-> 14;
		acromaster		-> 15
	end.

%% @doc Convert the binary class to an atom.
%% @todo Probably can make a list and use that list for both functions.

class_binary_to_atom(ClassBin) ->
	case ClassBin of
		 0 -> hunter;
		 1 -> ranger;
		 2 -> force;
		 3 -> fighgunner;
		 4 -> guntecher;
		 5 -> wartecher;
		 6 -> fortefighter;
		 7 -> fortegunner;
		 8 -> fortetecher;
		 9 -> protranser;
		10 -> acrofighter;
		11 -> acrotecher;
		12 -> fighmaster;
		13 -> gunmaster;
		14 -> masterforce;
		15 -> acromaster
	end.

%% @doc Convert a gender atom into a binary to be sent to clients.

gender_atom_to_binary(Gender) ->
	case Gender of
		male	-> 0;
		female	-> 1
	end.

%% @doc Convert the binary gender into an atom.

gender_binary_to_atom(GenderBin) ->
	case GenderBin of
		0 -> male;
		1 -> female
	end.

%% @doc Convert the binary options data into a tuple.
%%      The few unknown values are probably PS2 or 360 only.

options_binary_to_tuple(Binary) ->
	<<	TextDisplaySpeed:8, Sound:8, MusicVolume:8, SoundEffectVolume:8, Vibration:8, RadarMapDisplay:8,
		CutInDisplay:8, MainMenuCursorPosition:8, _:8, Camera3rdY:8, Camera3rdX:8, Camera1stY:8, Camera1stX:8,
		Controller:8, WeaponSwap:8, LockOn:8, Brightness:8, FunctionKeySetting:8, _:8, ButtonDetailDisplay:8, _:32 >> = Binary,
	{options, TextDisplaySpeed, Sound, MusicVolume, SoundEffectVolume, Vibration, RadarMapDisplay,
		CutInDisplay, MainMenuCursorPosition, Camera3rdY, Camera3rdX, Camera1stY, Camera1stX,
		Controller, WeaponSwap, LockOn, Brightness, FunctionKeySetting, ButtonDetailDisplay}.

%% @doc Convert a tuple of options data into a binary to be sent to clients.

options_tuple_to_binary(Tuple) ->
	{options, TextDisplaySpeed, Sound, MusicVolume, SoundEffectVolume, Vibration, RadarMapDisplay,
		CutInDisplay, MainMenuCursorPosition, Camera3rdY, Camera3rdX, Camera1stY, Camera1stX,
		Controller, WeaponSwap, LockOn, Brightness, FunctionKeySetting, ButtonDetailDisplay} = Tuple,
	<<	TextDisplaySpeed, Sound, MusicVolume, SoundEffectVolume, Vibration, RadarMapDisplay,
		CutInDisplay, MainMenuCursorPosition, 0, Camera3rdY, Camera3rdX, Camera1stY, Camera1stX,
		Controller, WeaponSwap, LockOn, Brightness, FunctionKeySetting, 0, ButtonDetailDisplay, 0:32 >>.

%% @doc Convert a race atom into a binary to be sent to clients.

race_atom_to_binary(Race) ->
	case Race of
		human	-> 0;
		newman	-> 1;
		cast	-> 2;
		beast	-> 3
	end.

%% @doc Convert the binary race into an atom.

race_binary_to_atom(RaceBin) ->
	case RaceBin of
		0 -> human;
		1 -> newman;
		2 -> cast;
		3 -> beast
	end.

%% @doc Convert a list of status effects into a binary to be sent to clients.
%% @todo Do it for real.

se_list_to_binary(_List) ->
	<< 0:32 >>.

%% @doc Convert the tuple of stats data into a binary to be sent to clients.

stats_tuple_to_binary(Tuple) ->
	{stats, ATP, ATA, TP, DFP, EVP, MST, STA} = Tuple,
	<<	ATP:16/little-unsigned-integer, DFP:16/little-unsigned-integer, ATA:16/little-unsigned-integer, EVP:16/little-unsigned-integer, 
		STA:16/little-unsigned-integer, 0:16, TP:16/little-unsigned-integer, MST:16/little-unsigned-integer >>.

%% @doc Validate the character's name.
%%      00F7 is the RGBA color control character.
%%      03F7 is the RGB color control character.
%%      Trigger an exception rather than handling errors.

validate_name(_Name) ->
	%~ Something like that probably: << true = X =/= 16#00F7 andalso X =/= 16#03F7 || X:16 <- Name>>.
	ok.

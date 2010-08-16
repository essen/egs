%%	EGS: Erlang Game Server
%%	Copyright (C) 2010  Loic Hoguin
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

-record(psu_npc, {has_card, name, race, gender, class, level, appearance}).

-define(NPC, [
	%~ { 0, #psu_npc{has_card=false, name="Ethan Waber", level=+0}},

	%~ { 1, #psu_npc{has_card=true, name="Hyuga Ryght", race=human, gender=male, class=hunter, level=+3,
		%~ appearance=#flesh_appearance{voicetype=54, jacket=16#00860300, pants=16#00860301, shoes=16#00860302, ears=16#00860303, face=16#00860304, hairstyle=16#00860305}
	%~ }},

	%~ { 2, #psu_npc{has_card=true, name="Karen Erra", level=+0}}, %% normal
	%~ { 3, #psu_npc{has_card=true, name="Leogini Berafort", level=+0}},
	%~ { 4, #psu_npc{has_card=true, name="Lucaim Nav", level=+0}},
	%~ { 5, #psu_npc{has_card=true, name="Maya Shidow", level=+0}},
	%~ { 6, #psu_npc{has_card=true, name="Tonnio Rhima", level=+0}},

	{ 7, #psu_npc{has_card=true, name="Lou", race=cast, gender=female, class=hunter, level=+3,
		appearance=#metal_appearance{voicetype=59, torso=16#008b1300, legs=16#008b1301, arms=16#008b1302, ears=16#008b1303, face=16#008b1304, headtype=16#008b1305}
	}}%,

	%~ { 8, #psu_npc{has_card=true, name="Mirei Mikuna", level=+0}},
	%~ { 9, #psu_npc{has_card=true, name="Hiru Vol", level=+0}},
	%~ {10, #psu_npc{has_card=true, name="No Vol", level=+0}},
	%~ {11, #psu_npc{has_card=true, name="Do Vol", level=+0}},
	%~ {12, #psu_npc{has_card=true, name="Liina Sukaya", level=+0}},
	%~ {13, #psu_npc{has_card=true, name="Alfort Tylor", level=+0}},
	%~ {14, #psu_npc{has_card=true, name="Obel Dallgun", level=+0}},
	%~ {15, #psu_npc{has_card=true, name="Ethan Waber", level=+0}}, %% EP1
	%~ {16, #psu_npc{has_card=true, name="Fulyen Curtz", level=+0}},
	%~ {17, #psu_npc{has_card=true, name="Renvolt Magashi", level=+0}},
	%~ {18, #psu_npc{has_card=false, name="Lumia Waber", level=+0}},
	%~ {19, #psu_npc{has_card=true, name="Remlia Norphe", level=+0}},
	%~ {20, #psu_npc{has_card=false, name="Clamp Maniel", level=+0}},
	%~ {21, #psu_npc{has_card=false, name="Kanal Tomrain", level=+0}},

	%~ {22, #psu_npc{has_card=false, name="Mina", race=human, gender=female, class=hunter, level=+0,
		%~ appearance=#flesh_appearance{voicetype=87, jacket=16#009C1300, pants=16#009C1301, shoes=16#009C1302, ears=16#009C1303, face=16#009C1304, hairstyle=16#009C1305}
	%~ }},

	%~ {23, #psu_npc{has_card=true, name="Hal", level=+0}},
	%~ {24, #psu_npc{has_card=false, name="Fulyen Curtz", level=+0}},
	%~ {25, #psu_npc{has_card=true, name="Laia Martinez", level=+0}}, %% EP2
	%~ {26, #psu_npc{has_card=true, name="Karen Erra", level=+0}}, %% maiden
	%~ {27, #psu_npc{has_card=false, name="Mirei Mikuna", level=+0}},
	%~ {28, #psu_npc{has_card=false, name="Obel Dallgun", level=+0}},
	%~ {29, #psu_npc{has_card=false, name="Maira Klein", level=+0}},
	%~ {30, #psu_npc{has_card=true, name="Orson Waber", level=+0}},
	%~ {31, #psu_npc{has_card=false, name="Fulyen Curtz", level=+0}},
	%~ {32, #psu_npc{has_card=true, name="Bruce Boyde", level=+0}},
	%~ {33, #psu_npc{has_card=true, name="Ethan Waber", level=+0}}, %% rogue
	%~ {34, #psu_npc{has_card=false, name="Vivienne", level=+0}},
	%~ {35, #psu_npc{has_card=false, name="Helga", level=+0}},
	%~ {36, #psu_npc{has_card=false, name="Hakana Kutanami", level=+0}},
	%~ {37, #psu_npc{has_card=false, name="Liche Baratse", level=+0}},
	%~ {38, #psu_npc{has_card=true, name="Howzer", level=+0}},
	%~ {39, #psu_npc{has_card=true, name="Rutsu", level=+0}},
	%~ {40, #psu_npc{has_card=true, name="Lumia Waber", level=+0}}, %% EP2
	%~ {41, #psu_npc{has_card=true, name="Laia Martinez", level=+0}}, %% president
	%~ {42, #psu_npc{has_card=true, name="My PM", level=+0}}
]).

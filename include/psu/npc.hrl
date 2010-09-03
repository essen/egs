%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc NPC characters definitions.
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

-record(psu_npc, {has_card, name, race, gender, class, level, appearance}).

-define(NPC, [
	%% This card doesn't actually exist.
	%% { 0, #psu_npc{has_card=false, name="Ethan Waber", level=+0}},

	{ 1, #psu_npc{has_card=true, name="Hyuga Ryght", race=human, gender=male, class=hunter, level=+3,
		appearance=#flesh_appearance{voicetype=54, jacket=16#00860300, pants=16#00860301, shoes=16#00860302, ears=16#00860303, face=16#00860304, hairstyle=16#00860305}
	}},
	{ 2, #psu_npc{has_card=true, name="Karen Erra", race=newman, gender=female, class=hunter, level=+3,
		appearance=#flesh_appearance{voicetype=52, jacket=16#00851300, pants=16#00851301, shoes=16#00851302, ears=16#00851303, face=16#00851304, hairstyle=16#00851305}
	}},
	{ 3, #psu_npc{has_card=true, name="Leogini Berafort", race=beast, gender=male, class=hunter, level=+3,
		appearance=#flesh_appearance{voicetype=55, jacket=16#00870300, pants=16#00870301, shoes=16#00870302, ears=16#00870303, face=16#00870304, hairstyle=16#00870305}
	}},
	{ 4, #psu_npc{has_card=true, name="Lucaim Nav", race=cast, gender=male, class=hunter, level=+5,
		appearance=#metal_appearance{voicetype=57, torso=16#00880300, legs=16#00880301, arms=16#00880302, ears=16#00880303, face=16#00880304, headtype=16#00880305}
	}},
	{ 5, #psu_npc{has_card=true, name="Maya Shidow", race=newman, gender=female, class=force, level=+3,
		appearance=#flesh_appearance{voicetype=58, jacket=16#00891300, pants=16#00891301, shoes=16#00891302, ears=16#00891303, face=16#00891304, hairstyle=16#00891305}
	}},
	{ 6, #psu_npc{has_card=true, name="Tonnio Rhima", race=beast, gender=male, class=hunter, level=+3,
		appearance=#flesh_appearance{voicetype=56, jacket=16#008a0300, pants=16#008a0301, shoes=16#008a0302, ears=16#008a0303, face=16#008a0304, hairstyle=16#008a0305}
	}},
	{ 7, #psu_npc{has_card=true, name="Lou", race=cast, gender=female, class=hunter, level=+3,
		appearance=#metal_appearance{voicetype=59, torso=16#008b1300, legs=16#008b1301, arms=16#008b1302, ears=16#008b1303, face=16#008b1304, headtype=16#008b1305}
	}},

	%~ { 8, #psu_npc{has_card=true, name="Mirei Mikuna", level=+0}}, %% @todo Seems to be Hatless Lou instead; 59 b013

	{ 9, #psu_npc{has_card=true, name="Hiru Vol", race=newman, gender=male, class=force, level=+3,
		appearance=#flesh_appearance{voicetype=65, jacket=16#008d0300, pants=16#008d0301, shoes=16#008d0302, ears=16#008d0303, face=16#008d0304, hairstyle=16#008d0305}
	}},
	{10, #psu_npc{has_card=true, name="No Vol", race=human, gender=male, class=ranger, level=+3,
		appearance=#flesh_appearance{voicetype=66, jacket=16#008e0300, pants=16#008e0301, shoes=16#008e0302, ears=16#008e0303, face=16#008e0304, hairstyle=16#008e0305}
	}},
	{11, #psu_npc{has_card=true, name="Do Vol", race=beast, gender=male, class=hunter, level=+3,
		appearance=#flesh_appearance{voicetype=67, jacket=16#008f0300, pants=16#008f0301, shoes=16#008f0302, ears=16#008f0303, face=16#008f0304, hairstyle=16#008f0305}
	}},
	{12, #psu_npc{has_card=true, name="Liina Sukaya", race=beast, gender=female, class=guntecher, level=+3,
		appearance=#flesh_appearance{voicetype=64, jacket=16#00921300, pants=16#00921301, shoes=16#00921302, ears=16#00921303, face=16#00921304, hairstyle=16#00921305}
	}},
	{13, #psu_npc{has_card=true, name="Alfort Tylor", race=beast, gender=male, class=fortefighter, level=+5,
		appearance=#flesh_appearance{voicetype=63, jacket=16#00910300, pants=16#00910301, shoes=16#00910302, ears=16#00910303, face=16#00910304, hairstyle=16#00910305}
	}},
	{14, #psu_npc{has_card=true, name="Obel Dallgun", race=human, gender=male, class=fortefighter, level=+5,
		appearance=#flesh_appearance{voicetype=60, jacket=16#008c0300, pants=16#008c0301, shoes=16#008c0302, ears=16#008c0303, face=16#008c0304, hairstyle=16#008c0305}
	}},
	{15, #psu_npc{has_card=true, name="Ethan Waber", race=human, gender=male, class=hunter, level=+5,
		appearance=#flesh_appearance{voicetype=51, jacket=16#00840300, pants=16#00840301, shoes=16#00840302, ears=16#00840303, face=16#00840304, hairstyle=16#00840305}
	}},
	{16, #psu_npc{has_card=true, name="Fulyen Curtz", race=cast, gender=male, class=hunter, level=+5,
		appearance=#metal_appearance{voicetype=68, torso=16#00950300, legs=16#00950301, arms=16#00950302, ears=16#00950303, face=16#00950304, headtype=16#00950305}
	}},
	{17, #psu_npc{has_card=true, name="Renvolt Magashi", race=cast, gender=male, class=hunter, level=+10,
		appearance=#metal_appearance{voicetype=69, torso=16#00900300, legs=16#00900301, arms=16#00900302, ears=16#00900303, face=16#00900304, headtype=16#00900305}
	}},
	{18, #psu_npc{has_card=true, name="Lumia Waber", race=human, gender=female, class=guntecher, level=-5,
		appearance=#flesh_appearance{voicetype=61, jacket=16#00931300, pants=16#00931301, shoes=16#00931302, ears=16#00931303, face=16#00931304, hairstyle=16#00931305}
	}},

	%~ {19, #psu_npc{has_card=true, name="Remlia Norphe", level=+0}}, %% @todo Missing data.

	{20, #psu_npc{has_card=true, name="Clamp Manyel", race=human, gender=male, class=hunter, level=-3,
		appearance=#flesh_appearance{voicetype=6, jacket=16#00960300, pants=16#00960301, shoes=16#00960302, ears=16#00960303, face=16#00960304, hairstyle=16#00960305}
	}},
	{21, #psu_npc{has_card=true, name="Kanal Tomrain", race=human, gender=male, class=hunter, level=-3,
		appearance=#flesh_appearance{voicetype=72, jacket=16#009d0300, pants=16#009d0301, shoes=16#009d0302, ears=16#009d0303, face=16#009d0304, hairstyle=16#009d0305}
	}},
	{22, #psu_npc{has_card=true, name="Mina", race=human, gender=female, class=hunter, level=+0,
		appearance=#flesh_appearance{voicetype=87, jacket=16#009c1300, pants=16#009c1301, shoes=16#009c1302, ears=16#009c1303, face=16#009c1304, hairstyle=16#009c1305}
	}},

	%~ {23, #psu_npc{has_card=true, name="Hal", level=+0}}, %% @todo Missing data.

	{24, #psu_npc{has_card=true, name="Fulyen Curtz", race=cast, gender=male, class=hunter, level=+5,
		appearance=#metal_appearance{voicetype=68, torso=16#00b80300, legs=16#00b80301, arms=16#00b80302, ears=16#00b80303, face=16#00b80304, headtype=16#00b80305}
	}},
	{25, #psu_npc{has_card=true, name="Laia Martinez", race=beast, gender=female, class=hunter, level=+3,
		appearance=#flesh_appearance{voicetype=102, jacket=16#00c41300, pants=16#00c41301, shoes=16#00c41302, ears=16#00c41303, face=16#00c41304, hairstyle=16#00c41305}
	}},
	{26, #psu_npc{has_card=true, name="Karen Erra", race=newman, gender=female, class=wartecher, level=+3,
		appearance=#flesh_appearance{voicetype=52, jacket=16#00ad1300, pants=16#00ad1301, shoes=16#00ad1302, ears=16#00ad1303, face=16#00ad1304, hairstyle=16#00ad1305}
	}},

	%~ {27, #psu_npc{has_card=false, name="Mirei Mikuna", level=+0}}, %% @todo Missing data.
	%~ {28, #psu_npc{has_card=false, name="Obel Dallgun", level=+0}}, %% @todo Orson Waber is found here. But for now Orson Waber is put on spot 30 instead.

	{29, #psu_npc{has_card=true, name="Maira Klein", race=human, gender=female, class=hunter, level=-10,
		appearance=#flesh_appearance{voicetype=18, jacket=16#00a91300, pants=16#00a91301, shoes=16#00a91302, ears=16#00a91303, face=16#00a91304, hairstyle=16#00a91305}
	}},
	{30, #psu_npc{has_card=true, name="Orson Waber", race=human, gender=male, class=protranser, level=+5,
		appearance=#flesh_appearance{voicetype=103, jacket=16#00b20300, pants=16#00b20301, shoes=16#00b20302, ears=16#00b20303, face=16#00b20304, hairstyle=16#00b20305}
	}},

	%~ {31, #psu_npc{has_card=false, name="Fulyen Curtz", level=+0}}, %% @todo Missing data.

	{32, #psu_npc{has_card=true, name="Bruce Boyde", race=human, gender=male, class=hunter, level=+3,
		appearance=#flesh_appearance{voicetype=74, jacket=16#00bb0300, pants=16#00bb0301, shoes=16#00bb0302, ears=16#00bb0303, face=16#00bb0304, hairstyle=16#00bb0305}
	}},
	{33, #psu_npc{has_card=true, name="Ethan Waber", race=human, gender=male, class=hunter, level=+5,
		appearance=#flesh_appearance{voicetype=51, jacket=16#00c50300, pants=16#00c50301, shoes=16#00c50302, ears=16#00c50303, face=16#00c50304, hairstyle=16#00c50305}
	}},
	{34, #psu_npc{has_card=true, name="Vivienne", race=cast, gender=female, class=hunter, level=-5,
		appearance=#metal_appearance{voicetype=0, torso=16#009a1300, legs=16#009a1301, arms=16#009a1302, ears=16#009a1303, face=16#009a1304, headtype=16#009a1305}
	}},

	%~ {35, #psu_npc{has_card=false, name="Helga", level=+0}}, %% @todo Missing data. Voice 51?
	%~ {36, #psu_npc{has_card=false, name="Hakana Kutanami", level=+0}}, %% @todo Find the right appearance.
	%~ {37, #psu_npc{has_card=false, name="Liche Baratse", level=+0}}, %% @todo Find the right appearance.

	{38, #psu_npc{has_card=true, name="Howzer", race=human, gender=male, class=fortefighter, level=+0,
		appearance=#flesh_appearance{voicetype=69, jacket=16#00b10300, pants=16#00b10301, shoes=16#00b10302, ears=16#00b10303, face=16#00b10304, hairstyle=16#00b10305}
	}},
	{39, #psu_npc{has_card=true, name="Rutsu", race=newman, gender=male, class=fortetecher, level=+0,
		appearance=#flesh_appearance{voicetype=7, jacket=16#00b40300, pants=16#00b40301, shoes=16#00b40302, ears=16#00b40303, face=16#00b40304, hairstyle=16#00b40305}
	}},
	{40, #psu_npc{has_card=true, name="Lumia Waber", race=human, gender=female, class=guntecher, level=-5,
		appearance=#flesh_appearance{voicetype=61, jacket=16#00b31300, pants=16#00b31301, shoes=16#00b31302, ears=16#00b31303, face=16#00b31304, hairstyle=16#00b31305}
	}},
	{41, #psu_npc{has_card=true, name="Laia Martinez", race=beast, gender=female, class=fortefighter, level=+3,
		appearance=#flesh_appearance{voicetype=102, jacket=16#00b51300, pants=16#00b51301, shoes=16#00b51302, ears=16#00b51303, face=16#00b51304, hairstyle=16#00b51305}
	}}

	%~ {42, #psu_npc{has_card=true, name="My PM", level=+0}} %% @todo Not sure how to handle those yet. Also missing data for a bunch of them.
]).

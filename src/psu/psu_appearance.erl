%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Character appearance functions.
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

-module(psu_appearance).
-export([binary_to_tuple/2, tuple_to_binary/2, validate_char_create/3]).

-include("include/records.hrl").

%% @doc Convert the binary character creation appearance data into a tuple.
%%      The lineshield color is ignored and set to 0 (neutral) by default instead.
%%      The badge is always set to 0 (none). Only beasts can later change it.
%%      The lips color and intensity is ignored and set to the default values {32767, 32767, 0} (flesh races only).
binary_to_tuple(cast, Binary) ->
	<<	VoiceType:8, VoicePitch:8, _:24, Torso:32/unsigned-integer, Legs:32/unsigned-integer, Arms:32/unsigned-integer,
		Ears:32/unsigned-integer, Face:32/unsigned-integer, HeadType:32/unsigned-integer, MainColor:8, _:24,
		_:8, Eyebrows:8, Eyelashes:8, EyesGroup:8, Eyes:8, _:24, EyesColorY:32/little-unsigned-integer, EyesColorX:32/little-unsigned-integer,
		_:96, BodyColor:32/little-unsigned-integer, SubColor:32/little-unsigned-integer, HairstyleColorY:32/little-unsigned-integer,
		HairstyleColorX:32/little-unsigned-integer, Proportion:32/little-unsigned-integer, ProportionBoxX:32/little-unsigned-integer,
		ProportionBoxY:32/little-unsigned-integer, FaceBoxX:32/little-unsigned-integer, FaceBoxY:32/little-unsigned-integer >> = Binary,
	{metal_appearance, VoiceType, VoicePitch, Torso, Legs, Arms, Ears, Face, HeadType, MainColor, 0,
		Eyebrows, Eyelashes, EyesGroup, Eyes, EyesColorY, EyesColorX, BodyColor, SubColor, HairstyleColorY, HairstyleColorX,
		Proportion, ProportionBoxX, ProportionBoxY, FaceBoxX, FaceBoxY};

binary_to_tuple(_, Binary) ->
	<<	VoiceType:8, VoicePitch:8, _:24, Jacket:32/unsigned-integer, Pants:32/unsigned-integer, Shoes:32/unsigned-integer,
		Ears:32/unsigned-integer, Face:32/unsigned-integer, Hairstyle:32/unsigned-integer, JacketColor:8, PantsColor:8, ShoesColor:8, _:8,
		_:8, Eyebrows:8, Eyelashes:8, EyesGroup:8, Eyes:8, BodySuit:8, _:16, EyesColorY:32/little-unsigned-integer, EyesColorX:32/little-unsigned-integer,
		_:96, SkinColor:32/little-unsigned-integer, _:32, HairstyleColorY:32/little-unsigned-integer,
		HairstyleColorX:32/little-unsigned-integer, Proportion:32/little-unsigned-integer, ProportionBoxX:32/little-unsigned-integer,
		ProportionBoxY:32/little-unsigned-integer, FaceBoxX:32/little-unsigned-integer, FaceBoxY:32/little-unsigned-integer >> = Binary,
	{flesh_appearance, VoiceType, VoicePitch, Jacket, Pants, Shoes, Ears, Face, Hairstyle, JacketColor, PantsColor, ShoesColor,
		0, 0, Eyebrows, Eyelashes, EyesGroup, Eyes, BodySuit, EyesColorY, EyesColorX, 32767, 32767, 0, SkinColor, HairstyleColorY,
		HairstyleColorX, Proportion, ProportionBoxX, ProportionBoxY, FaceBoxX, FaceBoxY}.

%% @doc Convert a tuple of appearance data into a binary to be sent to clients.
tuple_to_binary(cast, Tuple) ->
	{metal_appearance, VoiceType, VoicePitch, Torso, Legs, Arms, Ears, Face, HeadType, MainColor, LineshieldColor,
		Eyebrows, Eyelashes, EyesGroup, Eyes, EyesColorY, EyesColorX, BodyColor, SubColor, HairstyleColorY, HairstyleColorX,
		Proportion, ProportionBoxX, ProportionBoxY, FaceBoxX, FaceBoxY} = Tuple,
	<<	VoiceType:8, VoicePitch:8, 0:24, Torso:32/unsigned-integer, Legs:32/unsigned-integer, Arms:32/unsigned-integer,
		Ears:32/unsigned-integer, Face:32/unsigned-integer, HeadType:32/unsigned-integer, MainColor:8, 0:16, LineshieldColor:8,
		0:8, Eyebrows:8, Eyelashes:8, EyesGroup:8, Eyes:8, 0:24, EyesColorY:32/little-unsigned-integer, EyesColorX:32/little-unsigned-integer,
		16#ff7f0000:32, 16#ff7f0000:32, 0:32, BodyColor:32/little-unsigned-integer, SubColor:32/little-unsigned-integer, HairstyleColorY:32/little-unsigned-integer,
		HairstyleColorX:32/little-unsigned-integer, Proportion:32/little-unsigned-integer, ProportionBoxX:32/little-unsigned-integer,
		ProportionBoxY:32/little-unsigned-integer, FaceBoxX:32/little-unsigned-integer, FaceBoxY:32/little-unsigned-integer >>;

tuple_to_binary(_, Tuple) ->
	{flesh_appearance, VoiceType, VoicePitch, Jacket, Pants, Shoes, Ears, Face, Hairstyle, JacketColor, PantsColor, ShoesColor,
		LineshieldColor, Badge, Eyebrows, Eyelashes, EyesGroup, Eyes, BodySuit, EyesColorY, EyesColorX, LipsIntensity, LipsColorY, LipsColorX,
		SkinColor, HairstyleColorY, HairstyleColorX, Proportion, ProportionBoxX, ProportionBoxY, FaceBoxX, FaceBoxY} = Tuple,
	<<	VoiceType:8, VoicePitch:8, 0:24, Jacket:32/unsigned-integer, Pants:32/unsigned-integer, Shoes:32/unsigned-integer,
		Ears:32/unsigned-integer, Face:32/unsigned-integer, Hairstyle:32/unsigned-integer, JacketColor:8, PantsColor:8, ShoesColor:8, LineshieldColor:8,
		Badge:8, Eyebrows:8, Eyelashes:8, EyesGroup:8, Eyes:8, BodySuit:8, 0:16, EyesColorY:32/little-unsigned-integer, EyesColorX:32/little-unsigned-integer,
		LipsIntensity:32/little-unsigned-integer, LipsColorY:32/little-unsigned-integer, LipsColorX:32/little-unsigned-integer,
		SkinColor:32/little-unsigned-integer, 16#ffff0200:32, HairstyleColorY:32/little-unsigned-integer,
		HairstyleColorX:32/little-unsigned-integer, Proportion:32/little-unsigned-integer, ProportionBoxX:32/little-unsigned-integer,
		ProportionBoxY:32/little-unsigned-integer, FaceBoxX:32/little-unsigned-integer, FaceBoxY:32/little-unsigned-integer >>.

%% @doc Validate the character creation appearance data.
%%      Trigger an exception rather than handling errors.
validate_char_create(cast, male, Tuple) ->
	#metal_appearance{voicetype=VoiceType, torso=Torso, legs=Legs, arms=Arms, ears=Ears, face=Face, headtype=HeadType, eyelashes=Eyelashes, eyesgroup=EyesGroup, eyescolorx=EyesColorX} = Tuple,
	validate_char_create_common_metal(Tuple),
	true = (VoiceType >= 27 andalso VoiceType =< 38) orelse (VoiceType >= 89 andalso VoiceType =< 96),
	true = Torso =:= 16#00F70100 orelse Torso =:= 16#00F90100 orelse Torso =:= 16#00FC0100,
	true = Legs =:= 16#00F70101 orelse Legs =:= 16#00F90101 orelse Legs =:= 16#00FC0101,
	true = Arms =:= 16#00F70102 orelse Arms =:= 16#00F90102 orelse Arms =:= 16#00FC0102,
	if	Face =:= 16#00040004 orelse Face =:= 16#0A040004 orelse Face =:= 16#14040004 orelse Face =:= 16#1E040004 orelse Face =:= 16#28040004 orelse Face =:= 16#000E0004 ->
			true = Ears =:= 16#001E0003 orelse Ears =:= 16#001F0003 orelse Ears =:= 16#00200003 orelse Ears =:= 16#00210003 orelse Ears =:= 16#00220003,
			true = EyesColorX =< 327679,
			validate_char_create_male_hairstyle(HeadType);
		Face =:= 16#00F40104 orelse Face =:= 16#00F50104 orelse Face =:= 16#00F60104 orelse Face =:= 16#00F70104 orelse Face =:= 16#00F80104 orelse Face =:= 16#00F90104 orelse
		Face =:= 16#00FA0104 orelse Face =:= 16#00FD0104 orelse Face =:= 16#00020204 orelse Face =:= 16#00030204 orelse Face =:= 16#00040204 orelse Face =:= 16#00060204 orelse Face =:= 16#00070204 ->
			Ears = 16#FFFFFFFF,
			true = EyesColorX =< 458751,
			true = HeadType =:= 16#00F40105 orelse HeadType =:= 16#00F50105 orelse HeadType =:= 16#00F60105 orelse HeadType =:= 16#00F70105 orelse HeadType =:= 16#00F80105 orelse
				   HeadType =:= 16#00F90105 orelse HeadType =:= 16#00FA0105 orelse HeadType =:= 16#00FB0105 orelse HeadType =:= 16#00FD0105 orelse HeadType =:= 16#00020205 orelse
				   HeadType =:= 16#00030205 orelse HeadType =:= 16#00040205 orelse HeadType =:= 16#00060205 orelse HeadType =:= 16#00070205
	end,
	true = Eyelashes =< 2,
	EyesGroup = 4;

validate_char_create(cast, female, Tuple) ->
	#metal_appearance{voicetype=VoiceType, torso=Torso, legs=Legs, arms=Arms, ears=Ears, face=Face, headtype=HeadType, eyelashes=Eyelashes, eyesgroup=EyesGroup, eyescolorx=EyesColorX} = Tuple,
	validate_char_create_common_metal(Tuple),
	true = (VoiceType >= 39 andalso VoiceType =< 50) orelse (VoiceType >= 97 andalso VoiceType =< 101),
	true = Torso =:= 16#00F51100 orelse Torso =:= 16#00F91100 orelse Torso =:= 16#00FA1100,
	true = Legs =:= 16#00F51101 orelse Legs =:= 16#00F91101 orelse Legs =:= 16#00FA1101,
	true = Arms =:= 16#00F51102 orelse Arms =:= 16#00F91102 orelse Arms =:= 16#00F61102,
	if	Face =:= 16#00041004 orelse Face =:= 16#0A041004 orelse Face =:= 16#14041004 orelse Face =:= 16#1E041004 orelse Face =:= 16#3C041004 ->
			true = Ears =:= 16#001E1003 orelse Ears =:= 16#001F1003 orelse Ears =:= 16#00201003 orelse Ears =:= 16#00211003 orelse Ears =:= 16#00221003,
			true = EyesColorX =< 327679,
			validate_char_create_female_hairstyle(HeadType);
		Face =:= 16#00F41104 orelse Face =:= 16#00F51104 orelse Face =:= 16#00F61104 orelse Face =:= 16#00F71104 orelse Face =:= 16#00F81104 orelse Face =:= 16#00F91104 orelse
		Face =:= 16#00FA1104 orelse Face =:= 16#00FD1104 orelse Face =:= 16#00031204 orelse Face =:= 16#00041204 orelse Face =:= 16#00051204 orelse Face =:= 16#00061204 orelse Face =:= 16#00081204 ->
			Ears = 16#FFFFFFFF,
			true = EyesColorX =< 458751,
			true = HeadType =:= 16#00F41105 orelse HeadType =:= 16#00F51105 orelse HeadType =:= 16#00F61105 orelse HeadType =:= 16#00F71105 orelse HeadType =:= 16#00F81105 orelse
				   HeadType =:= 16#00F91105 orelse HeadType =:= 16#00FA1105 orelse HeadType =:= 16#00FB1105 orelse HeadType =:= 16#00FD1105 orelse HeadType =:= 16#00031205 orelse
				   HeadType =:= 16#00041205 orelse HeadType =:= 16#00051205 orelse HeadType =:= 16#00061205 orelse HeadType =:= 16#00081205
	end,
	true = Eyelashes =< 12,
	EyesGroup = 5;

validate_char_create(human, male, Tuple) ->
	#flesh_appearance{ears=Ears, face=Face, eyesgroup=EyesGroup, eyes=Eyes} = Tuple,
	validate_char_create_common_flesh(Tuple),
	validate_char_create_common_male_flesh(Tuple),
	true = Ears =:= 16#00000003 orelse Ears =:= 16#00010003,
	true = Face =:= 16#00010004 orelse Face =:= 16#01010004 orelse Face =:= 16#14010004 orelse Face =:= 16#15010004 orelse Face =:= 16#16010004 orelse Face =:= 16#17010004 orelse
		   Face =:= 16#18010004 orelse Face =:= 16#19010004 orelse Face =:= 16#1A010004 orelse Face =:= 16#1E010004 orelse Face =:= 16#1F010004 orelse Face =:= 16#20010004 orelse
		   Face =:= 16#21010004 orelse Face =:= 16#22010004 orelse Face =:= 16#23010004 orelse Face =:= 16#24010004 orelse Face =:= 16#28010004 orelse Face =:= 16#29010004 orelse
		   Face =:= 16#2A010004 orelse Face =:= 16#2B010004 orelse Face =:= 16#2C010004 orelse Face =:= 16#2D010004 orelse Face =:= 16#2E010004 orelse Face =:= 16#000B0004,
	EyesGroup = 0,
	true = Eyes =< 5;

validate_char_create(newman, male, Tuple) ->
	#flesh_appearance{ears=Ears, face=Face, eyesgroup=EyesGroup, eyes=Eyes} = Tuple,
	validate_char_create_common_flesh(Tuple),
	validate_char_create_common_male_flesh(Tuple),
	true = Ears =:= 16#00030003 orelse Ears =:= 16#00650003 orelse Ears =:= 16#00660003,
	true = Face =:= 16#00020004 orelse Face =:= 16#01020004 orelse Face =:= 16#14020004 orelse Face =:= 16#15020004 orelse Face =:= 16#16020004 orelse Face =:= 16#17020004 orelse
		   Face =:= 16#18020004 orelse Face =:= 16#19020004 orelse Face =:= 16#1A020004 orelse Face =:= 16#1E020004 orelse Face =:= 16#1F020004 orelse Face =:= 16#20020004 orelse
		   Face =:= 16#21020004 orelse Face =:= 16#22020004 orelse Face =:= 16#23020004 orelse Face =:= 16#24020004 orelse Face =:= 16#28020004 orelse Face =:= 16#29020004 orelse
		   Face =:= 16#2A020004 orelse Face =:= 16#2B020004 orelse Face =:= 16#2C020004 orelse Face =:= 16#2D020004 orelse Face =:= 16#2E020004 orelse Face =:= 16#000C0004,
	EyesGroup = 2,
	true = Eyes =< 5;

validate_char_create(beast, male, Tuple) ->
	#flesh_appearance{ears=Ears, face=Face, eyesgroup=EyesGroup, eyes=Eyes} = Tuple,
	validate_char_create_common_flesh(Tuple),
	validate_char_create_common_male_flesh(Tuple),
	true = Ears =:= 16#00020003 orelse Ears =:= 16#00CD0003 orelse Ears =:= 16#00CE0003,
	true = Face =:= 16#00030004 orelse Face =:= 16#0A030004 orelse Face =:= 16#14030004 orelse Face =:= 16#15030004 orelse Face =:= 16#16030004 orelse Face =:= 16#17030004 orelse
		   Face =:= 16#18030004 orelse Face =:= 16#19030004 orelse Face =:= 16#1A030004 orelse Face =:= 16#1E030004 orelse Face =:= 16#1F030004 orelse Face =:= 16#20030004 orelse
		   Face =:= 16#21030004 orelse Face =:= 16#22030004 orelse Face =:= 16#23030004 orelse Face =:= 16#24030004 orelse Face =:= 16#28030004 orelse Face =:= 16#29030004 orelse
		   Face =:= 16#2A030004 orelse Face =:= 16#2B030004 orelse Face =:= 16#2C030004 orelse Face =:= 16#2D030004 orelse Face =:= 16#2E030004 orelse Face =:= 16#000D0004,
	EyesGroup = 6,
	true = Eyes =< 6;

validate_char_create(human, female, Tuple) ->
	#flesh_appearance{ears=Ears, face=Face, eyesgroup=EyesGroup, eyes=Eyes} = Tuple,
	validate_char_create_common_flesh(Tuple),
	validate_char_create_common_female_flesh(Tuple),
	true = Ears =:= 16#00001003 orelse Ears =:= 16#00011003,
	true = Face =:= 16#00011004 orelse Face =:= 16#0A011004 orelse Face =:= 16#14011004 orelse Face =:= 16#1E011004 orelse Face =:= 16#3C011004,
	EyesGroup = 1,
	true = Eyes =< 5;

validate_char_create(newman, female, Tuple) ->
	#flesh_appearance{ears=Ears, face=Face, eyesgroup=EyesGroup, eyes=Eyes} = Tuple,
	validate_char_create_common_flesh(Tuple),
	validate_char_create_common_female_flesh(Tuple),
	true = Ears =:= 16#00031003 orelse Ears =:= 16#00651003 orelse Ears =:= 16#00661003,
	true = Face =:= 16#00021004 orelse Face =:= 16#0A021004 orelse Face =:= 16#14021004 orelse Face =:= 16#1E021004 orelse Face =:= 16#3C021004,
	EyesGroup = 3,
	true = Eyes =< 5;

validate_char_create(beast, female, Tuple) ->
	#flesh_appearance{ears=Ears, face=Face, eyesgroup=EyesGroup, eyes=Eyes} = Tuple,
	validate_char_create_common_flesh(Tuple),
	validate_char_create_common_female_flesh(Tuple),
	true = Ears =:= 16#00021003 orelse Ears =:= 16#00CD1003 orelse Ears =:= 16#00CE1003 orelse Ears =:= 16#00CF1003,
	true = Face =:= 16#00031004 orelse Face =:= 16#0A031004 orelse Face =:= 16#14031004 orelse Face =:= 16#1E031004 orelse Face =:= 16#3C031004,
	EyesGroup = 7,
	true = Eyes =< 6.

%% @doc Validate the common settings for all metal characters.
validate_char_create_common_metal(Tuple) ->
	#metal_appearance{maincolor=MainColor, eyebrows=Eyebrows, eyes=Eyes, eyescolory=EyesColorY, bodycolor=BodyColor, subcolor=SubColor,
		hairstylecolory=HairstyleColorY, hairstylecolorx=HairstyleColorX, proportion=Proportion, proportionboxx=ProportionBoxX, proportionboxy=ProportionBoxY,
		faceboxx=FaceBoxX, faceboxy=FaceBoxY} = Tuple,
	true = MainColor =< 7,
	true = Eyebrows =< 18,
	true = Eyes =< 2,
	true = EyesColorY =< 65535,
	true = BodyColor =< 131071,
	true = SubColor =< 393215,
	true = HairstyleColorY =< 65535,
	true = HairstyleColorX =< 327679,
	true = Proportion =< 131071,
	true = ProportionBoxX =< 131071,
	true = ProportionBoxY =< 131071,
	true = FaceBoxX =< 131071,
	true = FaceBoxY =< 131071.

%% @doc Validate the common settings for all flesh characters.
validate_char_create_common_flesh(Tuple) ->
	#flesh_appearance{jacketcolor=JacketColor, pantscolor=PantsColor, shoescolor=ShoesColor, eyebrows=Eyebrows, bodysuit=BodySuit,
		eyescolory=EyesColorY, eyescolorx=EyesColorX, skincolor=SkinColor, hairstylecolory=HairstyleColorY, hairstylecolorx=HairstyleColorX,
		proportion=Proportion, proportionboxx=ProportionBoxX, proportionboxy=ProportionBoxY, faceboxx=FaceBoxX, faceboxy=FaceBoxY} = Tuple,
	true = JacketColor =< 4,
	true = PantsColor =< 4,
	true = ShoesColor =< 4,
	true = Eyebrows =< 18,
	true = BodySuit =< 4,
	true = EyesColorY =< 65535,
	true = EyesColorX =< 327679,
	true = SkinColor =< 131071,
	true = HairstyleColorY =< 65535,
	true = HairstyleColorX =< 327679,
	true = Proportion =< 131071,
	true = ProportionBoxX =< 131071,
	true = ProportionBoxY =< 131071,
	true = FaceBoxX =< 131071,
	true = FaceBoxY =< 131071.

%% @doc Validate the common settings for all male flesh characters.
validate_char_create_common_male_flesh(Tuple) ->
	#flesh_appearance{voicetype=VoiceType, jacket=Jacket, pants=Pants, shoes=Shoes, hairstyle=Hairstyle, eyelashes=Eyelashes} = Tuple,
	true = (VoiceType >= 1 andalso VoiceType =< 14) orelse (VoiceType >= 76 andalso VoiceType =< 83),
	true = Jacket =:= 16#00060000 orelse Jacket =:= 16#00020000 orelse Jacket =:= 16#00030000,
	true = Pants =:= 16#00060001 orelse Pants =:= 16#000B0001 orelse Pants =:= 16#00030001,
	true = Shoes =:= 16#00060002 orelse Shoes =:= 16#00020002 orelse Shoes =:= 16#00040002,
	validate_char_create_male_hairstyle(Hairstyle),
	true = Eyelashes =< 2.

%% @doc Validate the common settings for all female flesh characters.
validate_char_create_common_female_flesh(Tuple) ->
	#flesh_appearance{voicetype=VoiceType, jacket=Jacket, pants=Pants, shoes=Shoes, hairstyle=Hairstyle, eyelashes=Eyelashes} = Tuple,
	true = (VoiceType >= 15 andalso VoiceType =< 26) orelse (VoiceType >= 84 andalso VoiceType =< 88),
	true = Jacket =:= 16#00011000 orelse Jacket =:= 16#00021000 orelse Jacket =:= 16#00031000,
	true = Pants =:= 16#00011001 orelse Pants =:= 16#00021001 orelse Pants =:= 16#00031001,
	true = Shoes =:= 16#00091002 orelse Shoes =:= 16#00071002 orelse Shoes =:= 16#00031002,
	validate_char_create_female_hairstyle(Hairstyle),
	true = Eyelashes =< 12.

%% @doc Validate the hairstyle for all male characters.
validate_char_create_male_hairstyle(Hairstyle) ->
	true = Hairstyle =:= 16#00000005 orelse Hairstyle =:= 16#000A0005 orelse Hairstyle =:= 16#00140005 orelse Hairstyle =:= 16#001E0005 orelse Hairstyle =:= 16#00280005 orelse Hairstyle =:= 16#00320005 orelse
		   Hairstyle =:= 16#003C0005 orelse Hairstyle =:= 16#00460005 orelse Hairstyle =:= 16#00500005 orelse Hairstyle =:= 16#005A0005 orelse Hairstyle =:= 16#00640005 orelse Hairstyle =:= 16#006E0005 orelse
		   Hairstyle =:= 16#00780005 orelse Hairstyle =:= 16#00820005 orelse Hairstyle =:= 16#008C0005 orelse Hairstyle =:= 16#00960005 orelse Hairstyle =:= 16#00A00005 orelse Hairstyle =:= 16#00AA0005.

%% @doc Validate the hairstyle for all female characters.
validate_char_create_female_hairstyle(Hairstyle) ->
	true = Hairstyle =:= 16#00001005 orelse Hairstyle =:= 16#000A1005 orelse Hairstyle =:= 16#00141005 orelse Hairstyle =:= 16#001E1005 orelse Hairstyle =:= 16#00281005 orelse Hairstyle =:= 16#00321005 orelse
		   Hairstyle =:= 16#003C1005 orelse Hairstyle =:= 16#00461005 orelse Hairstyle =:= 16#00501005 orelse Hairstyle =:= 16#005A1005 orelse Hairstyle =:= 16#00641005 orelse Hairstyle =:= 16#006E1005 orelse
		   Hairstyle =:= 16#00781005 orelse Hairstyle =:= 16#00821005 orelse Hairstyle =:= 16#008C1005 orelse Hairstyle =:= 16#00961005 orelse Hairstyle =:= 16#00A01005.

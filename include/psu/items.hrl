%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Items definitions.
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

-record(psu_item, {name, description, rarity, buy_price, sell_price, data}).
-record(psu_clothing_item, {appearance, type, manufacturer, overlap, gender, colors}).
-record(psu_consumable_item, {max_quantity, pt_diff, status_effect, target, use_condition, item_effect}).
-record(psu_trap_item, {max_quantity}).

%% Items.

-define(ITEMS, [
	%% Consumables.
	%% item_effect: 0x20: give points! fixed pt_diff
	%%              0x22: give SE! ignore pt_diff
	%%              0x30: give points! percent pt_diff
	%%              0x60: remove SEs! ignore pt_diff
	%%              0xB0: revive! percent pt_diff
	%%              0xD0: auto-revive! percent pt_diff

	{16#03010000, #psu_item{name="Monomate",
		description="HP recovery item.\nRestores a small amount of HP for the one who uses it.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_consumable_item{
		max_quantity=20, pt_diff=+100, status_effect=0, target=0, use_condition=0, item_effect=16#20
	}}},
	{16#03010100, #psu_item{name="Dimate",
		description="HP recovery item.\nRestores a large amount of HP for the one who uses it.",
		rarity=2, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=15, pt_diff=+1000, status_effect=0, target=0, use_condition=0, item_effect=16#20
	}}},
	{16#03010200, #psu_item{name="Trimate",
		description="HP recovery item.\nCompletely restores HP for the one who uses it.",
		rarity=3, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=10, pt_diff=+100, status_effect=0, target=0, use_condition=0, item_effect=16#30
	}}},
	{16#03010300, #psu_item{name="Star Atomizer",
		description="HP recovery item.\nRestores all HP for the one who uses it, and nearby allies.",
		rarity=6, buy_price=1500, sell_price=150, data=#psu_consumable_item{
		max_quantity=5, pt_diff=+100, status_effect=0, target=1, use_condition=0, item_effect=16#30
	}}},
	%% @todo Missing 03010400.
	{16#03010500, #psu_item{name="Antimate",
		description="Cures all status abnormalities for the one who uses it.",
		rarity=1, buy_price=100, sell_price=10, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=0, target=0, use_condition=0, item_effect=16#60
	}}},
	{16#03010600, #psu_item{name="Sol Atomizer",
		description="Cures all status abnormalities for the one who uses it, and nearby allies.",
		rarity=4, buy_price=300, sell_price=30, data=#psu_consumable_item{
		max_quantity=5, pt_diff=0, status_effect=0, target=1, use_condition=0, item_effect=16#60
	}}},
	{16#03010700, #psu_item{name="Moon Atomizer",
		description="Revives any incapacitated allies near the one who uses it.",
		rarity=5, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=5, pt_diff=+25, status_effect=0, target=1, use_condition=0, item_effect=16#B0
	}}},
	%% @todo Missing 03010800. Cosmo Atomizer?
	{16#03010900, #psu_item{name="Scape Doll",
		description="Automatically revives and cures the holder when incapacitated.",
		rarity=8, buy_price=10000, sell_price=10, data=#psu_consumable_item{
		max_quantity=1, pt_diff=+100, status_effect=0, target=0, use_condition=0, item_effect=16#D0
	}}},
	{16#03010a00, #psu_item{name="Agtaride",
		description="Status enhancement item.\nBoosts ATP (attack power) for the one who uses it.",
		rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=17, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010b00, #psu_item{name="Defbaride",
		description="Status enhancement item.\nBoosts DFP (defense) for the one who uses it.",
		rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=19, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010c00, #psu_item{name="Retaride",
		description="Status enhancement item.\nBoosts TP (TECHNIC points) and MST for the one who uses it.",
		rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=21, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010d00, #psu_item{name="Zodiaride",
		description="Status enhancement item.\nBoosts ATA (accuracy) and EVP (evasion) for the one who uses it.",
		rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=23, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010e00, #psu_item{name="Megistaride",
		description="Status enhancement item.\nBoosts all abilities for the one who uses it.",
		rarity=4, buy_price=300, sell_price=30, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=26, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03020000, #psu_item{name="Photon Charge",
		description="PP recovery item.\nRestores PP for the selected weapon.",
		rarity=3, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=10, pt_diff=+100, status_effect=0, target=4, use_condition=4, item_effect=16#30
	}}},
	%% @todo Missing 03020100. Cosmo Charge?

	%% Clothes.

	{16#09010000, #psu_item{name="Braves Jacket",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0100, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#cecdae98:32, 16#cfa33aca:32, 16#8d5d:16 >>
	}}},
	{16#09010100, #psu_item{name="Seyagya Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0200, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#8a5aa26a:32, 16#f6b6dce9:32, 16#afd8:16 >>
	}}},
	{16#09010200, #psu_item{name="Gojgoj Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0300, type=1, manufacturer=7, overlap=16#02, gender=male, colors= << 16#cd2a7c56:32, 16#eacae887:32, 16#c2da:16 >>
	}}},
	{16#09010300, #psu_item{name="Innocent Jacket",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0400, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#cfc29b69:32, 16#9212f9b7:32, 16#e68b:16 >>
	}}},
	{16#09010400, #psu_item{name="Necnec Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0500, type=1, manufacturer=7, overlap=16#01, gender=male, colors= << 16#923db538:32, 16#63a2afb3:32, 16#ebd9:16 >>
	}}},
	{16#09010500, #psu_item{name="Speeders Jersey",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0600, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#28efc2de:32, 16#95d7cd5f:32, 16#fa8b:16 >>
	}}},
	{16#09010600, #psu_item{name="AMF Army Jacket",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0700, type=1, manufacturer=0, overlap=16#02, gender=male, colors= << 16#9732f96e:32, 16#cabd8ff5:32, 16#cbfc:16 >>
	}}},
	{16#09010700, #psu_item{name="Braves Coat",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0800, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#5a89a256:32, 16#da4ca9c7:32, 16#8fde:16 >>
	}}},
	{16#09010800, #psu_item{name="Nobles Long Coat",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0900, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#aca598a2:32, 16#fd9f6bca:32, 16#1a8d:16 >>
	}}},
	{16#09010900, #psu_item{name="Wakakusa Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0a00, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#8fe923c9:32, 16#d38ca67f:32, 16#cbfc:16 >>
	}}},
	{16#09010a00, #psu_item{name="Kusatarika Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0b00, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#f3d92acf:32, 16#998215db:32, 16#1f89:16 >>
	}}},
	{16#09010b00, #psu_item{name="Flaxo Jacket",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0c00, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#8239c589:32, 16#93d968fa:32, 16#9cea:16 >>
	}}},
	{16#09010c00, #psu_item{name="Classica Shirt",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0d00, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#835329e3:32, 16#c8c2d6fc:32, 16#6da4:16 >>
	}}},
	{16#09010d00, #psu_item{name="Vigor Coat",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0e00, type=1, manufacturer=7, overlap=16#02, gender=male, colors= << 16#ac3aad83:32, 16#a8875a3d:32, 16#9fc5:16 >>
	}}},
	{16#09010e00, #psu_item{name="Roar Roars Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0f00, type=1, manufacturer=7, overlap=16#01, gender=male, colors= << 16#2353d999:32, 16#c986fce5:32, 16#ce5f:16 >>
	}}},
	{16#09010f00, #psu_item{name="Boaboa Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1000, type=1, manufacturer=7, overlap=16#02, gender=male, colors= << 16#6cc9dc3c:32, 16#a32cfaec:32, 16#73ab:16 >>
	}}},

%% female
%~ 09011000                    Braves Jacket 0100 12 02 2b cec4c7cd abfcc2c1 cb58
%~ 09011100                     Seyagya Vest 0200 15 02 2b ffc93d54 abaec178 7df2
%~ 09011200                     Gojgoj Tunic 0300 18 02 2b 283a8aba c7f27d1c ed74
%~ 09011300                 Guardless Jacket 0400 12 01 2b a2b4a8a3 cdb13fc7 abc4
%~ 09011400                     Phanis Tunic 0500 12 02 2b 5c27dc8c ffcfacea 1cf8
%~ 09011500                   Classica Tunic 0600 15 02 2b f8adc82d 8cec5dbf 323c
%~ 09011600                Karawai One-piece 0700 15 02 2b 2d1cc481 1bd4bfe8 5df1
%~ 09011700                    Smartia Shirt 0800 12 02 2b c7afc4c8 a3ce1cfc ea7d
%~ 09011800                 Pritia One-piece 0900 12 02 2b a427adf1 71edc81c 474f
%~ 09011900                  AMF Army Jacket 0a00 10 02 2b af3da3b8 6efb9598 cb9e
%~ 09011a00                     Flaxo Jacket 0b00 15 02 2b d35fc37c 2c1fcf97 e5f8
%~ 09011b00                Yorokatabra Armor 0c00 15 01 2b be938a39 9af9329f b689
%~ 09011c00                      Boaboa Vest 0d00 18 01 2b 8b53873a c91adef8 63c7
%~ 09011d00                      Bunbun Vest 0e00 18 01 2b 9f7c2cdc 5287dea7 1f2d
%~ 09011e00                     Bombom Smock 0f00 18 02 2b da13caf9 79532aee 46d3
%~ 09011f00                     Phasnis Vest 1000 18 02 2b b6d2c158 3c361fae 82fc

	{16#09012000, #psu_item{name="Goshkaria Top",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1200, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#47718bfd:32, 16#2a62e8c2:32, 16#fe9c:16 >>
	}}},
	{16#09012100, #psu_item{name="Storia Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1300, type=1, manufacturer=1, overlap=16#01, gender=male, colors= << 16#fe9fa5c2:32, 16#8de82aae:32, 16#4e52:16 >>
	}}},
	{16#09012200, #psu_item{name="Fujifiji Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1400, type=1, manufacturer=7, overlap=16#01, gender=male, colors= << 16#a2ada72a:32, 16#e79eeacd:32, 16#a19c:16 >>
	}}},
	{16#09012300, #psu_item{name="Kouze Jacket",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1600, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#9762b25d:32, 16#919f93c2:32, 16#9e29:16 >>
	}}},
	{16#09012400, #psu_item{name="Kabgara Dougi",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1700, type=1, manufacturer=4, overlap=16#01, gender=male, colors= << 16#f329d9e6:32, 16#bda2712e:32, 16#fcb2:16 >>
	}}},
	{16#09012500, #psu_item{name="SPF Top",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1800, type=1, manufacturer=3, overlap=16#01, gender=male, colors= << 16#a45ecdac:32, 16#b39329f5:32, 16#e5df:16 >>
	}}},
	{16#09012600, #psu_item{name="Waiwad Vest",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1900, type=1, manufacturer=7, overlap=16#01, gender=male, colors= << 16#f7bef134:32, 16#7e737d91:32, 16#35d8:16 >>
	}}},
	{16#09012700, #psu_item{name="HUmar Top",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1100, type=1, manufacturer=0, overlap=16#01, gender=male, colors= << 16#2d5d87db:32, 16#abf7cd8b:32, 16#bcb5:16 >>
	}}},
	{16#09012800, #psu_item{name="RAmar Jacket",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1c00, type=1, manufacturer=0, overlap=16#01, gender=male, colors= << 16#8c2cad5c:32, 16#fcbccace:32, 16#ea8a:16 >>
	}}},
	{16#09012900, #psu_item{name="Men's Swim Top",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2200, type=1, manufacturer=0, overlap=16#01, gender=male, colors= << 16#aaacda5c:32, 16#9288ad7a:32, 16#2de2:16 >>
	}}},

%% female
%~ 09012a00                      Hanaura Top 1300 15 01 2b 1f72432c 91bde53d 71a2
%~ 09012b00                   Hulauna Jacket 1400 15 02 2b 417dd1f7 14c2c1bd a191
%~ 09012c00                     Fujifiji Top 1500 18 01 2b 177deaa1 8d31cde1 46f6
%~ 09012d00                    Storia Jacket 1600 12 01 2b 7c1c5b8b d241ea39 71ab
%~ 09012e00                    Vatavara Vest 1a00 18 01 2b 91c1318c 9fa114be 9d27
%~ 09012f00                       HUnewe Top 1200 10 01 2b a32a8fde cf78cb5d 1c97
%~ 09013000                       FOnewe Top 1100 10 01 2b fa2a8a5a a21cdac5 4cea
%~ 09013100                    RAmarl Jacket 2500 10 01 2b 53befbb1 75d3cd23 a68a
%~ 09013200                  Normal Swim Top 1b00 10 01 2b 8f5dacf1 7c4d1322 ffec

	{16#09020000, #psu_item{name="Braves ST Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0100, type=2, manufacturer=1, overlap=16#12, gender=male, colors= << 16#a2adcec8:32, 16#afc2faca:32, 16#aecd:16 >>
	}}},
	{16#09020100, #psu_item{name="Seyagya Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0200, type=2, manufacturer=4, overlap=16#11, gender=male, colors= << 16#32922aad:32, 16#6b9fce97:32, 16#f285:16 >>
	}}},
	{16#09020200, #psu_item{name="Gojgoj Shorts",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0300, type=2, manufacturer=7, overlap=16#11, gender=male, colors= << 16#3d2a9c56:32, 16#baafe979:32, 16#a3bd:16 >>
	}}},
	{16#09020300, #psu_item{name="Innocent Slacks",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0400, type=2, manufacturer=1, overlap=16#12, gender=male, colors= << 16#fb2b8bca:32, 16#9212797a:32, 16#39b8:16 >>
	}}},
	{16#09020400, #psu_item{name="Necnec Shorts",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0500, type=2, manufacturer=7, overlap=16#11, gender=male, colors= << 16#b3bdb539:32, 16#e3a2cf63:32, 16#8b3a:16 >>
	}}},
	{16#09020500, #psu_item{name="Speeders Shorts",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0600, type=2, manufacturer=1, overlap=16#11, gender=male, colors= << 16#b8bfacf2:32, 16#9597b35d:32, 16#8bba:16 >>
	}}},
	{16#09020600, #psu_item{name="Braves Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0800, type=2, manufacturer=1, overlap=16#12, gender=male, colors= << 16#c5c8a26b:32, 16#9d97aa48:32, 16#872d:16 >>
	}}},
	{16#09020700, #psu_item{name="Wakakusa Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0a00, type=2, manufacturer=4, overlap=16#12, gender=male, colors= << 16#fc879198:32, 16#64ed6dd3:32, 16#f3cd:16 >>
	}}},
	{16#09020800, #psu_item{name="Kusatarika Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0b00, type=2, manufacturer=4, overlap=16#12, gender=male, colors= << 16#ad6d979d:32, 16#f7cdfb2b:32, 16#8b5d:16 >>
	}}},
	{16#09020900, #psu_item{name="Flaxo Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0c00, type=2, manufacturer=4, overlap=16#12, gender=male, colors= << 16#872dc58d:32, 16#9dd458fb:32, 16#95e3:16 >>
	}}},
	{16#09020a00, #psu_item{name="Classica Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0d00, type=2, manufacturer=4, overlap=16#11, gender=male, colors= << 16#92fb993d:32, 16#8b285ec8:32, 16#d64c:16 >>
	}}},
	{16#09020b00, #psu_item{name="Boaboa Shorts",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1000, type=2, manufacturer=7, overlap=16#11, gender=male, colors= << 16#bab9ca56:32, 16#f31a6aa3:32, 16#4eda:16 >>
	}}},

%% female
%~ 09020c00                    Braves Shorts 0100 22 11 2b e74d74d4 adf1cd17 bc85
%~ 09020d00                   Seyagya Shorts 0200 25 11 2b f29b3d54 adebc1c9 7d3f
%~ 09020e00                    Gojgoj Shorts 0300 28 11 2b 2c33caab 9cfcdc1c d35c
%~ 09020f00                  Guardless Pants 0400 22 12 2b a264a8a3 adc13f97 ba94
%~ 09021000                     Phanis Pants 0500 22 12 2b fcbc5487 ff8bccab c198
%~ 09021100                  Classica Shorts 0600 25 11 2b faff372d cc3cd4af c2cb
%~ 09021200                   Karawai Shorts 0700 25 11 2b d3c15c18 a24dbf8d d51f
%~ 09021300                    Pritia Shorts 0900 22 11 2b a589aef1 c1ecc711 ae99
%~ 09021400                      Flaxo Pants 0b00 25 11 2b 9c6cfc7c 8c63f2c7 56fb
%~ 09021500               Yorokatabra Shorts 0c00 25 11 2b ae93abb9 aabb32fa 6aa9
%~ 09021600                     Boaboa Pants 0d00 28 21 2b 8b539b3a b9a13df8 63c9
%~ 09021700                   Bunbun Bottoms 0e00 28 11 2b 9f79e2ec 56f7d5a5 1fed
%~ 09021800                    Bombom Shorts 0f00 28 11 2b c1d3ac9d 7d3d12ef c751

	{16#09021900, #psu_item{name="Goshkaria Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1200, type=2, manufacturer=1, overlap=16#11, gender=male, colors= << 16#af31e79d:32, 16#ca61986b:32, 16#d9ae:16 >>
	}}},
	{16#09021a00, #psu_item{name="Storia Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1300, type=2, manufacturer=1, overlap=16#22, gender=male, colors= << 16#fe9fa5c2:32, 16#8de82aab:32, 16#5b6c:16 >>
	}}},
	{16#09021b00, #psu_item{name="Fujifiji Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1400, type=2, manufacturer=7, overlap=16#22, gender=male, colors= << 16#a2ada72b:32, 16#e79ebe7e:32, 16#b1b2:16 >>
	}}},
	{16#09021c00, #psu_item{name="Kouze Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1600, type=2, manufacturer=4, overlap=16#12, gender=male, colors= << 16#9762295d:32, 16#91a839cd:32, 16#9ef7:16 >>
	}}},
	{16#09021d00, #psu_item{name="Kabgara Hakama",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1700, type=2, manufacturer=4, overlap=16#22, gender=male, colors= << 16#f329d9e6:32, 16#bdd2833d:32, 16#f9c2:16 >>
	}}},
	{16#09021e00, #psu_item{name="Waiwad Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1900, type=2, manufacturer=7, overlap=16#21, gender=male, colors= << 16#f7bef134:32, 16#7e377dc9:32, 16#338d:16 >>
	}}},
	{16#09021f00, #psu_item{name="HUmar Bottoms",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1100, type=2, manufacturer=0, overlap=16#11, gender=male, colors= << 16#2d5d87db:32, 16#abf7cd8b:32, 16#bcb5:16 >>
	}}},
	{16#09022000, #psu_item{name="Bikini Swim Pants",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2300, type=2, manufacturer=0, overlap=16#11, gender=male, colors= << 16#aaacda5c:32, 16#9288ad7a:32, 16#2de9:16 >>
	}}},
	{16#09022100, #psu_item{name="Swimming Trunks",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2200, type=2, manufacturer=0, overlap=16#11, gender=male, colors= << 16#8b2cadab:32, 16#de54fa9a:32, 16#ce41:16 >>
	}}},

%% female
%~ 09022200                  Hulauna Bottoms 1400 25 12 2b 919d61fd 642cf1db c3a9
%~ 09022300                 Fujifiji Bottoms 1500 28 21 2b 165de3a1 7db2c492 4ef4
%~ 09022400                   Storia Bottoms 1600 22 22 2b 7c1a56b8 c143dbe9 71ab
%~ 09022500                 Vatavara Bottoms 1a00 28 21 2b 171fe8c8 fda141eb d47e
%~ 09022600                   HUnewe Bottoms 1200 20 11 2b ab2a8fde cf78cb5d fc9b
%~ 09022700                   FOnewe Bottoms 1100 20 21 2b fa2a8a5a a21cdac5 5def
%~ 09022800              Normal Swim Bottoms 1b00 20 21 2b 8f5dacf1 7c4d1322 ffec

	{16#09030000, #psu_item{name="Braves Sneakers",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0100, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#c5cdcec8:32, 16#cfc2ccca:32, 16#c1bd:16 >>
	}}},
	{16#09030100, #psu_item{name="Seyagya Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0200, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#8bdba2b6:32, 16#fac6dc2c:32, 16#afd9:16 >>
	}}},
	{16#09030200, #psu_item{name="Gojgoj Boots",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0300, type=3, manufacturer=9, overlap=16#20, gender=male, colors= << 16#8c2c5cdc:32, 16#accfecfc:32, 16#c2a7:16 >>
	}}},
	{16#09030300, #psu_item{name="Innocent Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0400, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#c5c8a76d:32, 16#ac1cfda8:32, 16#ed8c:16 >>
	}}},
	{16#09030400, #psu_item{name="Necnec Short Boots",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0500, type=3, manufacturer=9, overlap=16#20, gender=male, colors= << 16#c9c3cb33:32, 16#e6aacadc:32, 16#8eed:16 >>
	}}},
	{16#09030500, #psu_item{name="Speeders Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0600, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#d9e93979:32, 16#29d8caf4:32, 16#978c:16 >>
	}}},
	{16#09030600, #psu_item{name="Braves Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0800, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#a2db9aa5:32, 16#abc5aab8:32, 16#8fb1:16 >>
	}}},
	{16#09030700, #psu_item{name="Wakakusa Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0a00, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#f88929ad:32, 16#56aca2c8:32, 16#a8fc:16 >>
	}}},
	{16#09030800, #psu_item{name="Kusatarika Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0b00, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#3a892af3:32, 16#a32c5ad2:32, 16#fdc7:16 >>
	}}},
	{16#09030900, #psu_item{name="Flaxo Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0c00, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#8728ce89:32, 16#93d767fb:32, 16#ac1a:16 >>
	}}},
	{16#09030a00, #psu_item{name="Classica Boots",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0d00, type=3, manufacturer=6, overlap=16#20, gender=male, colors= << 16#23799fd3:32, 16#9c8cec8a:32, 16#6aca:16 >>
	}}},
	{16#09030b00, #psu_item{name="Boaboa Boots",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1000, type=3, manufacturer=9, overlap=16#20, gender=male, colors= << 16#ec7cdcfc:32, 16#ac29fae3:32, 16#73ad:16 >>
	}}},

%% female
%~ 09030c00               Braves Short Boots 0100 33 20 2b c7cdcec4 aefdbecf cb4d
%~ 09030d00              Seyagya Short Boots 0200 36 10 2b ff9e3e64 aaaec1c8 7df3
%~ 09030e00                     Gojgoj Boots 0300 39 20 2b cc22eeaa 33ff7711 dd88
%~ 09030f00              GuardlessShortBoots 0400 33 10 2b fb6bda8a ecc7a7cf a44d
%~ 09031000                     Phanis Shoes 0500 33 10 2b c17845cd fc7fc24b 1ff1
%~ 09031100                   Classica Boots 0600 36 20 2b caaf1c32 7838defa cd8c
%~ 09031200                    Karawai Shoes 0700 36 10 2b aeaca5a2 aa64b19c ada1
%~ 09031300               Pritia Short Boots 0900 33 20 2b 2c8cda1c bc7ee8f1 45d4
%~ 09031400                      Flaxo Shoes 0b00 36 10 2b a2aea3a8 abcbc3cf e3fb
%~ 09031500                Yorokatabra Boots 0c00 36 20 2b 3e29ab93 9ebf2e98 a587
%~ 09031600                     Boaboa Boots 0d00 39 20 2b 8353983b c31cd9fa 5cc7
%~ 09031700                     Robrob Boots 0e00 39 20 2b c9c1c4a3 edc7b487 fb2e
%~ 09031800                     Bombom Boots 0f00 39 20 2b c8dea79f 8f311dfe c75d

	{16#09031900, #psu_item{name="Goshkaria Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1200, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#9c3c829b:32, 16#cb638deb:32, 16#d9ae:16 >>
	}}},
	{16#09031a00, #psu_item{name="Storia Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1300, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#fe9fa5c2:32, 16#9de82aab:32, 16#5b6e:16 >>
	}}},
	{16#09031b00, #psu_item{name="Fujifiji Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1400, type=3, manufacturer=9, overlap=16#10, gender=male, colors= << 16#2aada7a3:32, 16#e73ccbef:32, 16#aea2:16 >>
	}}},
	{16#09031c00, #psu_item{name="Kouze Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1600, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#9c62929d:32, 16#c18b39c2:32, 16#d229:16 >>
	}}},
	{16#09031d00, #psu_item{name="Kabgara Clogs",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1700, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#dfd2cdde:32, 16#c73219c3:32, 16#9fc2:16 >>
	}}},
	{16#09031e00, #psu_item{name="Waiwad Boots",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1900, type=3, manufacturer=9, overlap=16#20, gender=male, colors= << 16#f7bef134:32, 16#7e329dcb:32, 16#3839:16 >>
	}}},
	{16#09031f00, #psu_item{name="HUmar Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1100, type=3, manufacturer=0, overlap=16#20, gender=male, colors= << 16#2d5d87db:32, 16#abf7cd8b:32, 16#bcb5:16 >>
	}}},
	{16#09032000, #psu_item{name="FOnewm Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1b00, type=3, manufacturer=0, overlap=16#20, gender=male, colors= << 16#9ded3d2c:32, 16#c1defb1b:32, 16#a7eb:16 >>
	}}},
	{16#09032100, #psu_item{name="Bikini Swim Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2300, type=3, manufacturer=0, overlap=16#10, gender=male, colors= << 16#aaacda5c:32, 16#9288ad7a:32, 16#2de9:16 >>
	}}},
	{16#09032200, #psu_item{name="Normal Swim Shoes",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2200, type=3, manufacturer=0, overlap=16#20, gender=male, colors= << 16#8b2cadab:32, 16#de54fa9a:32, 16#ce41:16 >>
	}}},

%% female
%~ 09032300                    Hulauna Shoes 1400 36 10 2b 93396dfd 26cf31ae 92fc
%~ 09032400                   Fujifiji Shoes 1500 39 20 2b 316efbab b7a1eb39 e4f4
%~ 09032500                     Storia Shoes 1600 33 10 2b 2a9ada8a 1c93eab3 71ab
%~ 09032600                      Maydi Shoes 1700 33 10 2b ac8c2cec 5c7c4cfc bc9c
%~ 09032700                   Amorosso Boots 1900 33 20 2b f1c8fd82 beac51e3 379e
%~ 09032800                   Vatavara Boots 1a00 39 20 2b c11b378d 9da141ae 9562
%~ 09032900                     HUnewe Boots 1200 30 20 2b a32a8f3d c178cb36 1c97
%~ 09032a00                     FOnewe Shoes 1100 30 20 2b fa2a8a5a a21c4ac5 45ce
%~ 09032b00              Normal Swim Sandals 1b00 30 10 2b 8f5dacf1 7c4d1322 ffec

	{16#09040000, #psu_item{name="FOnewm Jacket",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1b00, type=4, manufacturer=0, overlap=16#01, gender=male, colors= << 16#fb8d6e23:32, 16#71daf41d:32, 16#a7e6:16 >>
	}}},

%% female
%~ 09040100                       Maydi Suit 1700 43 01 2b ac8c2cec 5c7c4cfc bc9c
%~ 09040200                     Amorosso Top 1900 42 01 2b f1e4df71 7ea251ce 8db7

	{16#09050000, #psu_item{name="AMF ArmyBottomsSet",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0700, type=5, manufacturer=0, overlap=16#10, gender=male, colors= << 16#9832f96d:32, 16#babd8ff5:32, 16#cbfc:16 >>
	}}},
	{16#09050100, #psu_item{name="Nobles Bottoms",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0900, type=5, manufacturer=1, overlap=16#10, gender=male, colors= << 16#ac379d99:32, 16#fb9c6cca:32, 16#1c8c:16 >>
	}}},
	{16#09050200, #psu_item{name="Western Bottoms M",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0e00, type=5, manufacturer=7, overlap=16#10, gender=male, colors= << 16#283ff2b2:32, 16#8bc3635c:32, 16#7a3d:16 >>
	}}},
	{16#09050300, #psu_item{name="Roar Roars Bottoms",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0f00, type=5, manufacturer=7, overlap=16#10, gender=male, colors= << 16#2353d999:32, 16#7986fce6:32, 16#ce4f:16 >>
	}}},

%% female
%~ 09050400                  Smartia Bottoms 0800 52 10 2b 7cfa4d89 2adec1c2 be75
%~ 09050500               AMF ArmyBottomsSet 0a00 50 10 2b af3d9fb7 6efca598 ca9e
%~ 09050600                Western Bottoms W 1000 58 10 2b 289dc1eb a39ef43e 82bf

	{16#09050700, #psu_item{name="SPF Bottoms",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1800, type=5, manufacturer=3, overlap=16#20, gender=male, colors= << 16#a45ecdac:32, 16#b39329f5:32, 16#e5df:16 >>
	}}},
	{16#09050800, #psu_item{name="RAmar Bottoms Set",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1c00, type=5, manufacturer=0, overlap=16#20, gender=male, colors= << 16#862ada5a:32, 16#fabaaaeb:32, 16#ea8a:16 >>
	}}},

%% female
%~ 09050900                  Hanaura Bottoms 1300 55 20 2b f9824f2f 919d5e92 1cca
%~ 09050a00               RAmarl Bottoms Set 2500 50 20 2b 5debbf1b 74debd23 658b

	{16#09060000, #psu_item{name="Men's Swimwear",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1e00, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#8b2cadab:32, 16#de54fa9a:32, 16#ce41:16 >>
	}}},
	{16#09060100, #psu_item{name="Bikini Swimwear",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1f00, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#aaacda5c:32, 16#9288ad7a:32, 16#2de9:16 >>
	}}},
	{16#09060200, #psu_item{name="Rappy Suit",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2000, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#dc7cfcd8:32, 16#5d8cf84c:32, 16#7f2e:16 >>
	}}},
	{16#09060300, #psu_item{name="Formalwear",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2100, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#c7abc5ac:32, 16#cebdfb7b:32, 16#8c6b:16 >>
	}}},

%% female
%~ 09060400                  Bikini Swimwear 1e00 60 00 2b d7fcab11 7cf4c53b 442c
%~ 09060500                 Regular Swimwear 1f00 60 00 2b 8f5dacf1 7c4d1322 ffec
%~ 09060600               One-piece Swimwear 2000 60 00 2b 8cc1744d 2c1f2df7 a2d1
%~ 09060700                     Formal Dress 2100 60 00 2b f278dc14 cd1fdeec 89f3
%~ 09060800                       Rappy Suit 2200 60 00 2b de7cfcd8 5d8cf84c 7f2e

	{16#09060a00, #psu_item{name="Illuminus Coat",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1500, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#8792917c:32, 16#beb3822b:32, 16#5f32:16 >>
	}}},
	{16#09060b00, #psu_item{name="Miyabi-kata",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1a00, type=6, manufacturer=4, overlap=0, gender=male, colors= << 16#7d27d4b3:32, 16#f55e92fc:32, 16#2c3f:16 >>
	}}},
	{16#09060c00, #psu_item{name="FOmar Set",
		description="",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1d00, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#8c2c5cfa:32, 16#a7c3da39:32, 16#7ffc:16 >>
	}}},

%% female
%~ 09060d00                 GUARDIANS Formal 1800 60 00 2b 38368d2a dfc1ba9d 1c5a
%~ 09060e00                      Miyabi-kata 2300 65 00 2b c81ad378 f845aec3 4e18
%~ 09060f00                      Mikunas Set 1c00 60 00 2b f77d4ee7 fa1af1ce 2eda
%~ 09061000                       FOmarl Set 2400 60 00 2b dc2c8714 2afcc147 1137
%~ 09061100                     Mikumiko Set 2700 65 00 2b 2a2e126e d5b6de58 c8c2
%~ 09061200                      Voloyal Set 2600 68 00 2b 5dd782cf a478c7a2 f52d

	%% Traps.
	%% @todo Handle type/race/gender restrictions.

	{16#0c010000, #psu_item{name="Damage Trap",
		description="Automatically explodes a few seconds after being set. Multiple traps can be set at once.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010100, #psu_item{name="Burn Trap",
		description="Automatically explodes a few seconds after being set. Adds burn effects.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010200, #psu_item{name="Freeze Trap",
		description="Automatically explodes a few seconds after being set. Adds freezing effects.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010300, #psu_item{name="Poison Trap",
		description="Automatically explodes a few seconds after being set. Adds poison effects.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010400, #psu_item{name="Confusion Trap",
		description="Automatically explodes a few seconds after being set. Adds confusion effects.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010500, #psu_item{name="Sleep Trap",
		description="Automatically explodes a few seconds after being set. Adds sleep effects.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010600, #psu_item{name="Virus Trap",
		description="Automatically explodes a few seconds after being set. Adds virus effects.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010700, #psu_item{name="Shock Trap",
		description="Automatically explodes a few seconds after being set. Adds shock effects.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010800, #psu_item{name="Silence Trap",
		description="Automatically explodes a few seconds after being set. Adds silence effects.",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},

	{16#0c020000, #psu_item{name="Damage Trap G",
		description="A trap that can be set off at any time after it's set.",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020100, #psu_item{name="Burn Trap G",
		description="A trap that can be set off at any time after it's set. Adds burn effects.",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020200, #psu_item{name="Freeze Trap G",
		description="A trap that can be set off at any time after it's set. Adds freeze effects.",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020300, #psu_item{name="Poison Trap G",
		description="A trap that can be set off at any time after it's set. Adds poison effects.",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020400, #psu_item{name="Confusion Trap G",
		description="A trap that can be set off at any time after it's set. Adds confusion effects.",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020500, #psu_item{name="Sleep Trap G",
		description="A trap that can be set off at any time after it's set. Adds sleep effects.",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020600, #psu_item{name="Virus Trap G",
		description="A trap that can be set off at any time after it's set. Adds virus effects.",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020700, #psu_item{name="Shock Trap G",
		description="A trap that can be set off at any time after it's set. Adds shock effects.",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020800, #psu_item{name="Silence Trap G",
		description="A trap that can be set off at any time after it's set. Adds silence effects.",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},

	{16#0c020900, #psu_item{name="Burn Trap EX",
		description="A new trap model that activates for a set period of time. Adds burn effects.",
		rarity=7, buy_price=650, sell_price=65, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020a00, #psu_item{name="Freeze Trap EX",
		description="A new trap model that activates for a set period of time. Adds freezing effects.",
		rarity=7, buy_price=650, sell_price=65, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020b00, #psu_item{name="Stun Trap EX",
		description="A new trap model that activates for a set period of time. Adds stun effects.",
		rarity=7, buy_price=650, sell_price=65, data=#psu_trap_item{max_quantity=10}}}
]).

%% Shops.

-define(MALE_CLOTHES, [16#09010000, 16#09010100, 16#09010200, 16#09010300, 16#09010400, 16#09010500, 16#09010600, 16#09010700,
	16#09010800, 16#09010900, 16#09010a00, 16#09010b00, 16#09010c00, 16#09010d00, 16#09010e00, 16#09010f00, 16#09012000,
	16#09012100, 16#09012200, 16#09012300, 16#09012400, 16#09012500, 16#09012600, 16#09012700, 16#09012800, 16#09012900,
	16#09020000, 16#09020100, 16#09020200, 16#09020300, 16#09020400, 16#09020500, 16#09020600, 16#09020700, 16#09020800,
	16#09020900, 16#09020a00, 16#09020b00, 16#09021900, 16#09021a00, 16#09021b00, 16#09021c00, 16#09021d00, 16#09021e00,
	16#09021f00, 16#09022000, 16#09022100, 16#09030000, 16#09030100, 16#09030200, 16#09030300, 16#09030400, 16#09030500,
	16#09030600, 16#09030700, 16#09030800, 16#09030900, 16#09030a00, 16#09030b00, 16#09031900, 16#09031a00, 16#09031b00,
	16#09031c00, 16#09031d00, 16#09031e00, 16#09031f00, 16#09032000, 16#09032100, 16#09032200, 16#09040000, 16#09050000,
	16#09050100, 16#09050200, 16#09050300, 16#09050700, 16#09050800, 16#09060000, 16#09060100, 16#09060200, 16#09060300,
	16#09060a00, 16#09060b00, 16#09060c00]).
-define(STD_CONSUMABLES, [16#03010000, 16#03010100, 16#03010200, 16#03010300, 16#03010500, 16#03010600,
	16#03010700, 16#03010900, 16#03010a00, 16#03010b00, 16#03010c00, 16#03010d00, 16#03010e00, 16#03020000,
	16#0c010000, 16#0c010100, 16#0c010200, 16#0c010300, 16#0c010400, 16#0c010500, 16#0c010600, 16#0c010700, 16#0c010800,
	16#0c020000, 16#0c020100, 16#0c020200, 16#0c020300, 16#0c020400, 16#0c020500, 16#0c020600, 16#0c020700, 16#0c020800,
	16#0c020900, 16#0c020a00, 16#0c020b00]).

-define(SHOPS, [
	{515, ?MALE_CLOTHES}, %% Parum vendor.
	{552, ?MALE_CLOTHES}, %% Moatoob vendor.
	{589, ?MALE_CLOTHES}, %% Neudaiz vendor.
	{623, ?MALE_CLOTHES}, %% Colony left vendor.
	{624, ?MALE_CLOTHES}, %% Colony right vendor.

	{512, ?STD_CONSUMABLES}, %% Parum right vendor, Parum v1 field lobbies vendors. Parum GUARDIANS vendor.
	{513, ?STD_CONSUMABLES}, %% Parum left vendor.
	{549, ?STD_CONSUMABLES}, %% Moatoob right vendor, Moatoob v1 field lobbies vendors. Moatoob GUARDIANS vendor.
	{550, ?STD_CONSUMABLES}, %% Moatoob left vendor.
	{586, ?STD_CONSUMABLES}, %% Neudaiz only vendor, Neudaiz v1 field lobbies vendors. Neudaiz GUARDIANS vendor.
	{620, ?STD_CONSUMABLES}, %% Colony right vendor, Colony v1 field lobbies vendors. Colony GUARDIANS vendor.
	{621, ?STD_CONSUMABLES}, %% Colony left vendor.
	{622, ?STD_CONSUMABLES}  %% v2 field lobbies vendors.
]).

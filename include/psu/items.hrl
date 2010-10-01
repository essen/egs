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

-record(psu_element, {type, percent}).
-record(psu_pa, {type, level}).

-record(psu_item, {name, rarity, buy_price, sell_price, data}).
-record(psu_clothing_item, {appearance, type, manufacturer, overlap, gender, colors}).
-record(psu_consumable_item, {max_quantity, pt_diff, status_effect, action, target, use_condition, item_effect}).
-record(psu_parts_item, {appearance, type, manufacturer, overlap, gender}).
-record(psu_special_item, {}).
-record(psu_striking_weapon_item, {pp, atp, ata, atp_req, shop_element, hand, max_upgrades, attack_label,
	attack_sound, hitbox_a, hitbox_b, hitbox_c, hitbox_d, nb_targets, effect, model}).
-record(psu_trap_item, {max_quantity, effect, type}).

-record(psu_clothing_item_variables, {color}).
-record(psu_consumable_item_variables, {quantity}).
-record(psu_parts_item_variables, {}).
-record(psu_special_item_variables, {}).
-record(psu_striking_weapon_item_variables, {is_active=0, slot=0, current_pp, max_pp, element, pa=#psu_pa{type=0, level=0}}).
-record(psu_trap_item_variables, {quantity}).

%% Items.

-define(ITEMS, [
	%% Swords.
	%% @todo Do the shop for these.

	{16#01010900, #psu_item{name="Huge Cutter",
		rarity=15, buy_price=10, sell_price=1, data=#psu_striking_weapon_item{
			pp=100, atp=200, ata=300, atp_req=1, shop_element=#psu_element{type=0, percent=0}, hand=both, max_upgrades=50, attack_label=0,
			attack_sound={default, 1961}, hitbox_a=16#9f01, hitbox_b=16#76fd, hitbox_c=16#3f29, hitbox_d=16#2222, nb_targets=5, effect=0, model=9
	}}},
	{16#01010a00, #psu_item{name="Kan Yu",
		rarity=15, buy_price=10, sell_price=1, data=#psu_striking_weapon_item{
			pp=100, atp=200, ata=300, atp_req=1, shop_element=#psu_element{type=0, percent=0}, hand=both, max_upgrades=50, attack_label=0,
			attack_sound={default, 1961}, hitbox_a=16#9f01, hitbox_b=16#76fd, hitbox_c=16#3f29, hitbox_d=16#2222, nb_targets=5, effect=1, model=10
	}}},
	{16#01010b00, #psu_item{name="De Ragan Slayer",
		rarity=15, buy_price=10, sell_price=1, data=#psu_striking_weapon_item{
			pp=100, atp=200, ata=300, atp_req=1, shop_element=#psu_element{type=0, percent=0}, hand=both, max_upgrades=50, attack_label=0,
			attack_sound={custom, 0}, hitbox_a=16#9f01, hitbox_b=16#76fd, hitbox_c=16#3f29, hitbox_d=16#2222, nb_targets=133, effect=0, model=11
	}}},

	%% Consumables.
	%% item_effect: 0x20: give points! fixed pt_diff
	%%              0x22: give SE! ignore pt_diff
	%%              0x30: give points! percent pt_diff
	%%              0x60: remove SEs! ignore pt_diff
	%%              0xB0: revive! percent pt_diff
	%%              0xD0: auto-revive! percent pt_diff

	{16#03010000, #psu_item{name="Monomate",
		rarity=1, buy_price=50, sell_price=5, data=#psu_consumable_item{
		max_quantity=20, pt_diff=+100, status_effect=0, action=0, target=0, use_condition=0, item_effect=16#20
	}}},
	{16#03010100, #psu_item{name="Dimate",
		rarity=2, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=15, pt_diff=+1000, status_effect=0, action=0, target=0, use_condition=0, item_effect=16#20
	}}},
	{16#03010200, #psu_item{name="Trimate",
		rarity=3, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=10, pt_diff=+100, status_effect=0, action=0, target=0, use_condition=0, item_effect=16#30
	}}},
	{16#03010300, #psu_item{name="Star Atomizer",
		rarity=6, buy_price=1500, sell_price=150, data=#psu_consumable_item{
		max_quantity=5, pt_diff=+100, status_effect=0, action=0, target=1, use_condition=0, item_effect=16#30
	}}},
	%% @todo Missing 03010400.
	{16#03010500, #psu_item{name="Antimate",
		rarity=1, buy_price=100, sell_price=10, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=0, action=4, target=0, use_condition=0, item_effect=16#60
	}}},
	{16#03010600, #psu_item{name="Sol Atomizer",
		rarity=4, buy_price=300, sell_price=30, data=#psu_consumable_item{
		max_quantity=5, pt_diff=0, status_effect=0, action=4, target=1, use_condition=0, item_effect=16#60
	}}},
	{16#03010700, #psu_item{name="Moon Atomizer",
		rarity=5, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=5, pt_diff=+25, status_effect=0, action=5, target=1, use_condition=0, item_effect=16#B0
	}}},
	%% @todo Missing 03010800. Cosmo Atomizer?
	{16#03010900, #psu_item{name="Scape Doll",
		rarity=8, buy_price=10000, sell_price=10, data=#psu_consumable_item{
		max_quantity=1, pt_diff=+100, status_effect=0, action=2, target=0, use_condition=0, item_effect=16#D0
	}}},
	{16#03010a00, #psu_item{name="Agtaride",
		rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=17, action=3, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010b00, #psu_item{name="Defbaride",
		rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=19, action=3, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010c00, #psu_item{name="Retaride",
		rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=21, action=3, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010d00, #psu_item{name="Zodiaride",
		rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=23, action=3, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010e00, #psu_item{name="Megistaride",
		rarity=4, buy_price=300, sell_price=30, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=26, action=3, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03020000, #psu_item{name="Photon Charge",
		rarity=3, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=10, pt_diff=+100, status_effect=0, action=1, target=4, use_condition=4, item_effect=16#30
	}}},
	%% @todo Missing 03020100. Cosmo Charge?

	%% Clothes.

	{16#09010000, #psu_item{name="Braves Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0100, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#cecdae98:32, 16#cfa33aca:32, 16#8d5d:16 >>
	}}},
	{16#09010100, #psu_item{name="Seyagya Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0200, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#8a5aa26a:32, 16#f6b6dce9:32, 16#afd8:16 >>
	}}},
	{16#09010200, #psu_item{name="Gojgoj Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0300, type=1, manufacturer=7, overlap=16#02, gender=male, colors= << 16#cd2a7c56:32, 16#eacae887:32, 16#c2da:16 >>
	}}},
	{16#09010300, #psu_item{name="Innocent Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0400, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#cfc29b69:32, 16#9212f9b7:32, 16#e68b:16 >>
	}}},
	{16#09010400, #psu_item{name="Necnec Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0500, type=1, manufacturer=7, overlap=16#01, gender=male, colors= << 16#923db538:32, 16#63a2afb3:32, 16#ebd9:16 >>
	}}},
	{16#09010500, #psu_item{name="Speeders Jersey",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0600, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#28efc2de:32, 16#95d7cd5f:32, 16#fa8b:16 >>
	}}},
	{16#09010600, #psu_item{name="AMF Army Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0700, type=1, manufacturer=0, overlap=16#02, gender=male, colors= << 16#9732f96e:32, 16#cabd8ff5:32, 16#cbfc:16 >>
	}}},
	{16#09010700, #psu_item{name="Braves Coat",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0800, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#5a89a256:32, 16#da4ca9c7:32, 16#8fde:16 >>
	}}},
	{16#09010800, #psu_item{name="Nobles Long Coat",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0900, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#aca598a2:32, 16#fd9f6bca:32, 16#1a8d:16 >>
	}}},
	{16#09010900, #psu_item{name="Wakakusa Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0a00, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#8fe923c9:32, 16#d38ca67f:32, 16#cbfc:16 >>
	}}},
	{16#09010a00, #psu_item{name="Kusatarika Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0b00, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#f3d92acf:32, 16#998215db:32, 16#1f89:16 >>
	}}},
	{16#09010b00, #psu_item{name="Flaxo Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0c00, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#8239c589:32, 16#93d968fa:32, 16#9cea:16 >>
	}}},
	{16#09010c00, #psu_item{name="Classica Shirt",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0d00, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#835329e3:32, 16#c8c2d6fc:32, 16#6da4:16 >>
	}}},
	{16#09010d00, #psu_item{name="Vigor Coat",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0e00, type=1, manufacturer=7, overlap=16#02, gender=male, colors= << 16#ac3aad83:32, 16#a8875a3d:32, 16#9fc5:16 >>
	}}},
	{16#09010e00, #psu_item{name="Roar Roars Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0f00, type=1, manufacturer=7, overlap=16#01, gender=male, colors= << 16#2353d999:32, 16#c986fce5:32, 16#ce5f:16 >>
	}}},
	{16#09010f00, #psu_item{name="Boaboa Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1000, type=1, manufacturer=7, overlap=16#02, gender=male, colors= << 16#6cc9dc3c:32, 16#a32cfaec:32, 16#73ab:16 >>
	}}},
	{16#09011000, #psu_item{name="Braves Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0100, type=1, manufacturer=2, overlap=16#02, gender=female, colors= << 16#cec4c7cd:32, 16#abfcc2c1:32, 16#cb58:16 >>
	}}},
	{16#09011100, #psu_item{name="Seyagya Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0200, type=1, manufacturer=5, overlap=16#02, gender=female, colors= << 16#ffc93d54:32, 16#abaec178:32, 16#7df2:16 >>
	}}},
	{16#09011200, #psu_item{name="Gojgoj Tunic",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0300, type=1, manufacturer=8, overlap=16#02, gender=female, colors= << 16#283a8aba:32, 16#c7f27d1c:32, 16#ed74:16 >>
	}}},
	{16#09011300, #psu_item{name="Guardless Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0400, type=1, manufacturer=2, overlap=16#01, gender=female, colors= << 16#a2b4a8a3:32, 16#cdb13fc7:32, 16#abc4:16 >>
	}}},
	{16#09011400, #psu_item{name="Phanis Tunic",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0500, type=1, manufacturer=2, overlap=16#02, gender=female, colors= << 16#5c27dc8c:32, 16#ffcfacea:32, 16#1cf8:16 >>
	}}},
	{16#09011500, #psu_item{name="Classica Tunic",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0600, type=1, manufacturer=5, overlap=16#02, gender=female, colors= << 16#f8adc82d:32, 16#8cec5dbf:32, 16#323c:16 >>
	}}},
	{16#09011600, #psu_item{name="Karawai One-piece",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0700, type=1, manufacturer=5, overlap=16#02, gender=female, colors= << 16#2d1cc481:32, 16#1bd4bfe8:32, 16#5df1:16 >>
	}}},
	{16#09011700, #psu_item{name="Smartia Shirt",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0800, type=1, manufacturer=2, overlap=16#02, gender=female, colors= << 16#c7afc4c8:32, 16#a3ce1cfc:32, 16#ea7d:16 >>
	}}},
	{16#09011800, #psu_item{name="Pritia One-piece",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0900, type=1, manufacturer=2, overlap=16#02, gender=female, colors= << 16#a427adf1:32, 16#71edc81c:32, 16#474f:16 >>
	}}},
	{16#09011900, #psu_item{name="AMF Army Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0a00, type=1, manufacturer=0, overlap=16#02, gender=female, colors= << 16#af3da3b8:32, 16#6efb9598:32, 16#cb9e:16 >>
	}}},
	{16#09011a00, #psu_item{name="Flaxo Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0b00, type=1, manufacturer=5, overlap=16#02, gender=female, colors= << 16#d35fc37c:32, 16#2c1fcf97:32, 16#e5f8:16 >>
	}}},
	{16#09011b00, #psu_item{name="Yorokatabra Armor",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0c00, type=1, manufacturer=5, overlap=16#01, gender=female, colors= << 16#be938a39:32, 16#9af9329f:32, 16#b689:16 >>
	}}},
	{16#09011c00, #psu_item{name="Boaboa Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0d00, type=1, manufacturer=8, overlap=16#01, gender=female, colors= << 16#8b53873a:32, 16#c91adef8:32, 16#63c7:16 >>
	}}},
	{16#09011d00, #psu_item{name="Bunbun Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0e00, type=1, manufacturer=8, overlap=16#01, gender=female, colors= << 16#9f7c2cdc:32, 16#5287dea7:32, 16#1f2d:16 >>
	}}},
	{16#09011e00, #psu_item{name="Bombom Smock",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0f00, type=1, manufacturer=8, overlap=16#02, gender=female, colors= << 16#da13caf9:32, 16#79532aee:32, 16#46d3:16 >>
	}}},
	{16#09011f00, #psu_item{name="Phasnis Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1000, type=1, manufacturer=8, overlap=16#02, gender=female, colors= << 16#b6d2c158:32, 16#3c361fae:32, 16#82fc:16 >>
	}}},
	{16#09012000, #psu_item{name="Goshkaria Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1200, type=1, manufacturer=1, overlap=16#02, gender=male, colors= << 16#47718bfd:32, 16#2a62e8c2:32, 16#fe9c:16 >>
	}}},
	{16#09012100, #psu_item{name="Storia Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1300, type=1, manufacturer=1, overlap=16#01, gender=male, colors= << 16#fe9fa5c2:32, 16#8de82aae:32, 16#4e52:16 >>
	}}},
	{16#09012200, #psu_item{name="Fujifiji Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1400, type=1, manufacturer=7, overlap=16#01, gender=male, colors= << 16#a2ada72a:32, 16#e79eeacd:32, 16#a19c:16 >>
	}}},
	{16#09012300, #psu_item{name="Kouze Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1600, type=1, manufacturer=4, overlap=16#02, gender=male, colors= << 16#9762b25d:32, 16#919f93c2:32, 16#9e29:16 >>
	}}},
	{16#09012400, #psu_item{name="Kabgara Dougi",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1700, type=1, manufacturer=4, overlap=16#01, gender=male, colors= << 16#f329d9e6:32, 16#bda2712e:32, 16#fcb2:16 >>
	}}},
	{16#09012500, #psu_item{name="SPF Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1800, type=1, manufacturer=3, overlap=16#01, gender=male, colors= << 16#a45ecdac:32, 16#b39329f5:32, 16#e5df:16 >>
	}}},
	{16#09012600, #psu_item{name="Waiwad Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1900, type=1, manufacturer=7, overlap=16#01, gender=male, colors= << 16#f7bef134:32, 16#7e737d91:32, 16#35d8:16 >>
	}}},
	{16#09012700, #psu_item{name="HUmar Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1100, type=1, manufacturer=0, overlap=16#01, gender=male, colors= << 16#2d5d87db:32, 16#abf7cd8b:32, 16#bcb5:16 >>
	}}},
	{16#09012800, #psu_item{name="RAmar Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1c00, type=1, manufacturer=0, overlap=16#01, gender=male, colors= << 16#8c2cad5c:32, 16#fcbccace:32, 16#ea8a:16 >>
	}}},
	{16#09012900, #psu_item{name="Men's Swim Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2200, type=1, manufacturer=0, overlap=16#01, gender=male, colors= << 16#aaacda5c:32, 16#9288ad7a:32, 16#2de2:16 >>
	}}},
	{16#09012a00, #psu_item{name="Hanaura Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1300, type=1, manufacturer=5, overlap=16#01, gender=female, colors= << 16#1f72432c:32, 16#91bde53d:32, 16#71a2:16 >>
	}}},
	{16#09012b00, #psu_item{name="Hulauna Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1400, type=1, manufacturer=5, overlap=16#02, gender=female, colors= << 16#417dd1f7:32, 16#14c2c1bd:32, 16#a191:16 >>
	}}},
	{16#09012c00, #psu_item{name="Fujifiji Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1500, type=1, manufacturer=8, overlap=16#01, gender=female, colors= << 16#177deaa1:32, 16#8d31cde1:32, 16#46f6:16 >>
	}}},
	{16#09012d00, #psu_item{name="Storia Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1600, type=1, manufacturer=2, overlap=16#01, gender=female, colors= << 16#7c1c5b8b:32, 16#d241ea39:32, 16#71ab:16 >>
	}}},
	{16#09012e00, #psu_item{name="Vatavara Vest",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1a00, type=1, manufacturer=8, overlap=16#01, gender=female, colors= << 16#91c1318c:32, 16#9fa114be:32, 16#9d27:16 >>
	}}},
	{16#09012f00, #psu_item{name="HUnewe Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1200, type=1, manufacturer=0, overlap=16#01, gender=female, colors= << 16#a32a8fde:32, 16#cf78cb5d:32, 16#1c97:16 >>
	}}},
	{16#09013000, #psu_item{name="FOnewe Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1100, type=1, manufacturer=0, overlap=16#01, gender=female, colors= << 16#fa2a8a5a:32, 16#a21cdac5:32, 16#4cea:16 >>
	}}},
	{16#09013100, #psu_item{name="RAmarl Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2500, type=1, manufacturer=0, overlap=16#01, gender=female, colors= << 16#53befbb1:32, 16#75d3cd23:32, 16#a68a:16 >>
	}}},
	{16#09013200, #psu_item{name="Normal Swim Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1b00, type=1, manufacturer=0, overlap=16#01, gender=female, colors= << 16#8f5dacf1:32, 16#7c4d1322:32, 16#ffec:16 >>
	}}},
	{16#09013300, #psu_item{name="Bikini Swim Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1d00, type=1, manufacturer=0, overlap=16#01, gender=female, colors= << 16#d7fcab11:32, 16#7cf4c53b:32, 16#442c:16 >>
	}}},
	{16#09020000, #psu_item{name="Braves ST Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0100, type=2, manufacturer=1, overlap=16#12, gender=male, colors= << 16#a2adcec8:32, 16#afc2faca:32, 16#aecd:16 >>
	}}},
	{16#09020100, #psu_item{name="Seyagya Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0200, type=2, manufacturer=4, overlap=16#11, gender=male, colors= << 16#32922aad:32, 16#6b9fce97:32, 16#f285:16 >>
	}}},
	{16#09020200, #psu_item{name="Gojgoj Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0300, type=2, manufacturer=7, overlap=16#11, gender=male, colors= << 16#3d2a9c56:32, 16#baafe979:32, 16#a3bd:16 >>
	}}},
	{16#09020300, #psu_item{name="Innocent Slacks",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0400, type=2, manufacturer=1, overlap=16#12, gender=male, colors= << 16#fb2b8bca:32, 16#9212797a:32, 16#39b8:16 >>
	}}},
	{16#09020400, #psu_item{name="Necnec Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0500, type=2, manufacturer=7, overlap=16#11, gender=male, colors= << 16#b3bdb539:32, 16#e3a2cf63:32, 16#8b3a:16 >>
	}}},
	{16#09020500, #psu_item{name="Speeders Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0600, type=2, manufacturer=1, overlap=16#11, gender=male, colors= << 16#b8bfacf2:32, 16#9597b35d:32, 16#8bba:16 >>
	}}},
	{16#09020600, #psu_item{name="Braves Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0800, type=2, manufacturer=1, overlap=16#12, gender=male, colors= << 16#c5c8a26b:32, 16#9d97aa48:32, 16#872d:16 >>
	}}},
	{16#09020700, #psu_item{name="Wakakusa Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0a00, type=2, manufacturer=4, overlap=16#12, gender=male, colors= << 16#fc879198:32, 16#64ed6dd3:32, 16#f3cd:16 >>
	}}},
	{16#09020800, #psu_item{name="Kusatarika Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0b00, type=2, manufacturer=4, overlap=16#12, gender=male, colors= << 16#ad6d979d:32, 16#f7cdfb2b:32, 16#8b5d:16 >>
	}}},
	{16#09020900, #psu_item{name="Flaxo Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0c00, type=2, manufacturer=4, overlap=16#12, gender=male, colors= << 16#872dc58d:32, 16#9dd458fb:32, 16#95e3:16 >>
	}}},
	{16#09020a00, #psu_item{name="Classica Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0d00, type=2, manufacturer=4, overlap=16#11, gender=male, colors= << 16#92fb993d:32, 16#8b285ec8:32, 16#d64c:16 >>
	}}},
	{16#09020b00, #psu_item{name="Boaboa Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1000, type=2, manufacturer=7, overlap=16#11, gender=male, colors= << 16#bab9ca56:32, 16#f31a6aa3:32, 16#4eda:16 >>
	}}},
	{16#09020c00, #psu_item{name="Braves Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0100, type=2, manufacturer=2, overlap=16#11, gender=female, colors= << 16#e74d74d4:32, 16#adf1cd17:32, 16#bc85:16 >>
	}}},
	{16#09020d00, #psu_item{name="Seyagya Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0200, type=2, manufacturer=5, overlap=16#11, gender=female, colors= << 16#f29b3d54:32, 16#adebc1c9:32, 16#7d3f:16 >>
	}}},
	{16#09020e00, #psu_item{name="Gojgoj Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0300, type=2, manufacturer=8, overlap=16#11, gender=female, colors= << 16#2c33caab:32, 16#9cfcdc1c:32, 16#d35c:16 >>
	}}},
	{16#09020f00, #psu_item{name="Guardless Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0400, type=2, manufacturer=2, overlap=16#12, gender=female, colors= << 16#a264a8a3:32, 16#adc13f97:32, 16#ba94:16 >>
	}}},
	{16#09021000, #psu_item{name="Phanis Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0500, type=2, manufacturer=2, overlap=16#12, gender=female, colors= << 16#fcbc5487:32, 16#ff8bccab:32, 16#c198:16 >>
	}}},
	{16#09021100, #psu_item{name="Classica Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0600, type=2, manufacturer=5, overlap=16#11, gender=female, colors= << 16#faff372d:32, 16#cc3cd4af:32, 16#c2cb:16 >>
	}}},
	{16#09021200, #psu_item{name="Karawai Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0700, type=2, manufacturer=5, overlap=16#11, gender=female, colors= << 16#d3c15c18:32, 16#a24dbf8d:32, 16#d51f:16 >>
	}}},
	{16#09021300, #psu_item{name="Pritia Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0900, type=2, manufacturer=2, overlap=16#11, gender=female, colors= << 16#a589aef1:32, 16#c1ecc711:32, 16#ae99:16 >>
	}}},
	{16#09021400, #psu_item{name="Flaxo Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0b00, type=2, manufacturer=5, overlap=16#11, gender=female, colors= << 16#9c6cfc7c:32, 16#8c63f2c7:32, 16#56fb:16 >>
	}}},
	{16#09021500, #psu_item{name="Yorokatabra Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0c00, type=2, manufacturer=5, overlap=16#11, gender=female, colors= << 16#ae93abb9:32, 16#aabb32fa:32, 16#6aa9:16 >>
	}}},
	{16#09021600, #psu_item{name="Boaboa Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0d00, type=2, manufacturer=8, overlap=16#21, gender=female, colors= << 16#8b539b3a:32, 16#b9a13df8:32, 16#63c9:16 >>
	}}},
	{16#09021700, #psu_item{name="Bunbun Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0e00, type=2, manufacturer=8, overlap=16#11, gender=female, colors= << 16#9f79e2ec:32, 16#56f7d5a5:32, 16#1fed:16 >>
	}}},
	{16#09021800, #psu_item{name="Bombom Shorts",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0f00, type=2, manufacturer=8, overlap=16#11, gender=female, colors= << 16#c1d3ac9d:32, 16#7d3d12ef:32, 16#c751:16 >>
	}}},
	{16#09021900, #psu_item{name="Goshkaria Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1200, type=2, manufacturer=1, overlap=16#11, gender=male, colors= << 16#af31e79d:32, 16#ca61986b:32, 16#d9ae:16 >>
	}}},
	{16#09021a00, #psu_item{name="Storia Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1300, type=2, manufacturer=1, overlap=16#22, gender=male, colors= << 16#fe9fa5c2:32, 16#8de82aab:32, 16#5b6c:16 >>
	}}},
	{16#09021b00, #psu_item{name="Fujifiji Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1400, type=2, manufacturer=7, overlap=16#22, gender=male, colors= << 16#a2ada72b:32, 16#e79ebe7e:32, 16#b1b2:16 >>
	}}},
	{16#09021c00, #psu_item{name="Kouze Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1600, type=2, manufacturer=4, overlap=16#12, gender=male, colors= << 16#9762295d:32, 16#91a839cd:32, 16#9ef7:16 >>
	}}},
	{16#09021d00, #psu_item{name="Kabgara Hakama",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1700, type=2, manufacturer=4, overlap=16#22, gender=male, colors= << 16#f329d9e6:32, 16#bdd2833d:32, 16#f9c2:16 >>
	}}},
	{16#09021e00, #psu_item{name="Waiwad Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1900, type=2, manufacturer=7, overlap=16#21, gender=male, colors= << 16#f7bef134:32, 16#7e377dc9:32, 16#338d:16 >>
	}}},
	{16#09021f00, #psu_item{name="HUmar Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1100, type=2, manufacturer=0, overlap=16#11, gender=male, colors= << 16#2d5d87db:32, 16#abf7cd8b:32, 16#bcb5:16 >>
	}}},
	{16#09022000, #psu_item{name="Bikini Swim Pants",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2300, type=2, manufacturer=0, overlap=16#11, gender=male, colors= << 16#aaacda5c:32, 16#9288ad7a:32, 16#2de9:16 >>
	}}},
	{16#09022100, #psu_item{name="Swimming Trunks",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2200, type=2, manufacturer=0, overlap=16#11, gender=male, colors= << 16#8b2cadab:32, 16#de54fa9a:32, 16#ce41:16 >>
	}}},
	{16#09022200, #psu_item{name="Hulauna Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1400, type=2, manufacturer=5, overlap=16#12, gender=female, colors= << 16#919d61fd:32, 16#642cf1db:32, 16#c3a9:16 >>
	}}},
	{16#09022300, #psu_item{name="Fujifiji Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1500, type=2, manufacturer=8, overlap=16#21, gender=female, colors= << 16#165de3a1:32, 16#7db2c492:32, 16#4ef4:16 >>
	}}},
	{16#09022400, #psu_item{name="Storia Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1600, type=2, manufacturer=2, overlap=16#22, gender=female, colors= << 16#7c1a56b8:32, 16#c143dbe9:32, 16#71ab:16 >>
	}}},
	{16#09022500, #psu_item{name="Vatavara Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1a00, type=2, manufacturer=8, overlap=16#21, gender=female, colors= << 16#171fe8c8:32, 16#fda141eb:32, 16#d47e:16 >>
	}}},
	{16#09022600, #psu_item{name="HUnewe Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1200, type=2, manufacturer=0, overlap=16#11, gender=female, colors= << 16#ab2a8fde:32, 16#cf78cb5d:32, 16#fc9b:16 >>
	}}},
	{16#09022700, #psu_item{name="FOnewe Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1100, type=2, manufacturer=0, overlap=16#21, gender=female, colors= << 16#fa2a8a5a:32, 16#a21cdac5:32, 16#5def:16 >>
	}}},
	{16#09022800, #psu_item{name="Normal Swim Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1b00, type=2, manufacturer=0, overlap=16#21, gender=female, colors= << 16#8f5dacf1:32, 16#7c4d1322:32, 16#ffec:16 >>
	}}},
	{16#09022900, #psu_item{name="Bikini Swim Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1d00, type=2, manufacturer=0, overlap=16#21, gender=female, colors= << 16#d7fcab11:32, 16#7cf4c53b:32, 16#442c:16 >>
	}}},
	{16#09030000, #psu_item{name="Braves Sneakers",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0100, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#c5cdcec8:32, 16#cfc2ccca:32, 16#c1bd:16 >>
	}}},
	{16#09030100, #psu_item{name="Seyagya Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0200, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#8bdba2b6:32, 16#fac6dc2c:32, 16#afd9:16 >>
	}}},
	{16#09030200, #psu_item{name="Gojgoj Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0300, type=3, manufacturer=9, overlap=16#20, gender=male, colors= << 16#8c2c5cdc:32, 16#accfecfc:32, 16#c2a7:16 >>
	}}},
	{16#09030300, #psu_item{name="Innocent Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0400, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#c5c8a76d:32, 16#ac1cfda8:32, 16#ed8c:16 >>
	}}},
	{16#09030400, #psu_item{name="Necnec Short Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0500, type=3, manufacturer=9, overlap=16#20, gender=male, colors= << 16#c9c3cb33:32, 16#e6aacadc:32, 16#8eed:16 >>
	}}},
	{16#09030500, #psu_item{name="Speeders Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0600, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#d9e93979:32, 16#29d8caf4:32, 16#978c:16 >>
	}}},
	{16#09030600, #psu_item{name="Braves Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0800, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#a2db9aa5:32, 16#abc5aab8:32, 16#8fb1:16 >>
	}}},
	{16#09030700, #psu_item{name="Wakakusa Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0a00, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#f88929ad:32, 16#56aca2c8:32, 16#a8fc:16 >>
	}}},
	{16#09030800, #psu_item{name="Kusatarika Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0b00, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#3a892af3:32, 16#a32c5ad2:32, 16#fdc7:16 >>
	}}},
	{16#09030900, #psu_item{name="Flaxo Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0c00, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#8728ce89:32, 16#93d767fb:32, 16#ac1a:16 >>
	}}},
	{16#09030a00, #psu_item{name="Classica Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0d00, type=3, manufacturer=6, overlap=16#20, gender=male, colors= << 16#23799fd3:32, 16#9c8cec8a:32, 16#6aca:16 >>
	}}},
	{16#09030b00, #psu_item{name="Boaboa Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1000, type=3, manufacturer=9, overlap=16#20, gender=male, colors= << 16#ec7cdcfc:32, 16#ac29fae3:32, 16#73ad:16 >>
	}}},
	{16#09030c00, #psu_item{name="Braves Short Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0100, type=3, manufacturer=3, overlap=16#20, gender=female, colors= << 16#c7cdcec4:32, 16#aefdbecf:32, 16#cb4d:16 >>
	}}},
	{16#09030d00, #psu_item{name="Seyagya Short Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0200, type=3, manufacturer=6, overlap=16#10, gender=female, colors= << 16#ff9e3e64:32, 16#aaaec1c8:32, 16#7df3:16 >>
	}}},
	{16#09030e00, #psu_item{name="Gojgoj Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0300, type=3, manufacturer=9, overlap=16#20, gender=female, colors= << 16#cc22eeaa:32, 16#33ff7711:32, 16#dd88:16 >>
	}}},
	{16#09030f00, #psu_item{name="GuardlessShortBoots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0400, type=3, manufacturer=3, overlap=16#10, gender=female, colors= << 16#fb6bda8a:32, 16#ecc7a7cf:32, 16#a44d:16 >>
	}}},
	{16#09031000, #psu_item{name="Phanis Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0500, type=3, manufacturer=3, overlap=16#10, gender=female, colors= << 16#c17845cd:32, 16#fc7fc24b:32, 16#1ff1:16 >>
	}}},
	{16#09031100, #psu_item{name="Classica Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0600, type=3, manufacturer=6, overlap=16#20, gender=female, colors= << 16#caaf1c32:32, 16#7838defa:32, 16#cd8c:16 >>
	}}},
	{16#09031200, #psu_item{name="Karawai Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0700, type=3, manufacturer=6, overlap=16#10, gender=female, colors= << 16#aeaca5a2:32, 16#aa64b19c:32, 16#ada1:16 >>
	}}},
	{16#09031300, #psu_item{name="Pritia Short Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0900, type=3, manufacturer=3, overlap=16#20, gender=female, colors= << 16#2c8cda1c:32, 16#bc7ee8f1:32, 16#45d4:16 >>
	}}},
	{16#09031400, #psu_item{name="Flaxo Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0b00, type=3, manufacturer=6, overlap=16#10, gender=female, colors= << 16#a2aea3a8:32, 16#abcbc3cf:32, 16#e3fb:16 >>
	}}},
	{16#09031500, #psu_item{name="Yorokatabra Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0c00, type=3, manufacturer=6, overlap=16#20, gender=female, colors= << 16#3e29ab93:32, 16#9ebf2e98:32, 16#a587:16 >>
	}}},
	{16#09031600, #psu_item{name="Boaboa Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0d00, type=3, manufacturer=9, overlap=16#20, gender=female, colors= << 16#8353983b:32, 16#c31cd9fa:32, 16#5cc7:16 >>
	}}},
	{16#09031700, #psu_item{name="Robrob Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0e00, type=3, manufacturer=9, overlap=16#20, gender=female, colors= << 16#c9c1c4a3:32, 16#edc7b487:32, 16#fb2e:16 >>
	}}},
	{16#09031800, #psu_item{name="Bombom Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0f00, type=3, manufacturer=9, overlap=16#20, gender=female, colors= << 16#c8dea79f:32, 16#8f311dfe:32, 16#c75d:16 >>
	}}},
	{16#09031900, #psu_item{name="Goshkaria Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1200, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#9c3c829b:32, 16#cb638deb:32, 16#d9ae:16 >>
	}}},
	{16#09031a00, #psu_item{name="Storia Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1300, type=3, manufacturer=3, overlap=16#10, gender=male, colors= << 16#fe9fa5c2:32, 16#9de82aab:32, 16#5b6e:16 >>
	}}},
	{16#09031b00, #psu_item{name="Fujifiji Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1400, type=3, manufacturer=9, overlap=16#10, gender=male, colors= << 16#2aada7a3:32, 16#e73ccbef:32, 16#aea2:16 >>
	}}},
	{16#09031c00, #psu_item{name="Kouze Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1600, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#9c62929d:32, 16#c18b39c2:32, 16#d229:16 >>
	}}},
	{16#09031d00, #psu_item{name="Kabgara Clogs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1700, type=3, manufacturer=6, overlap=16#10, gender=male, colors= << 16#dfd2cdde:32, 16#c73219c3:32, 16#9fc2:16 >>
	}}},
	{16#09031e00, #psu_item{name="Waiwad Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1900, type=3, manufacturer=9, overlap=16#20, gender=male, colors= << 16#f7bef134:32, 16#7e329dcb:32, 16#3839:16 >>
	}}},
	{16#09031f00, #psu_item{name="HUmar Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1100, type=3, manufacturer=0, overlap=16#20, gender=male, colors= << 16#2d5d87db:32, 16#abf7cd8b:32, 16#bcb5:16 >>
	}}},
	{16#09032000, #psu_item{name="FOnewm Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1b00, type=3, manufacturer=0, overlap=16#20, gender=male, colors= << 16#9ded3d2c:32, 16#c1defb1b:32, 16#a7eb:16 >>
	}}},
	{16#09032100, #psu_item{name="Bikini Swim Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2300, type=3, manufacturer=0, overlap=16#10, gender=male, colors= << 16#aaacda5c:32, 16#9288ad7a:32, 16#2de9:16 >>
	}}},
	{16#09032200, #psu_item{name="Normal Swim Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2200, type=3, manufacturer=0, overlap=16#20, gender=male, colors= << 16#8b2cadab:32, 16#de54fa9a:32, 16#ce41:16 >>
	}}},
	{16#09032300, #psu_item{name="Hulauna Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1400, type=3, manufacturer=6, overlap=16#10, gender=female, colors= << 16#93396dfd:32, 16#26cf31ae:32, 16#92fc:16 >>
	}}},
	{16#09032400, #psu_item{name="Fujifiji Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1500, type=3, manufacturer=9, overlap=16#20, gender=female, colors= << 16#316efbab:32, 16#b7a1eb39:32, 16#e4f4:16 >>
	}}},
	{16#09032500, #psu_item{name="Storia Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1600, type=3, manufacturer=3, overlap=16#10, gender=female, colors= << 16#2a9ada8a:32, 16#1c93eab3:32, 16#71ab:16 >>
	}}},
	{16#09032600, #psu_item{name="Maydi Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1700, type=3, manufacturer=3, overlap=16#10, gender=female, colors= << 16#ac8c2cec:32, 16#5c7c4cfc:32, 16#bc9c:16 >>
	}}},
	{16#09032700, #psu_item{name="Amorosso Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1900, type=3, manufacturer=3, overlap=16#20, gender=female, colors= << 16#f1c8fd82:32, 16#beac51e3:32, 16#379e:16 >>
	}}},
	{16#09032800, #psu_item{name="Vatavara Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1a00, type=3, manufacturer=9, overlap=16#20, gender=female, colors= << 16#c11b378d:32, 16#9da141ae:32, 16#9562:16 >>
	}}},
	{16#09032900, #psu_item{name="HUnewe Boots",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1200, type=3, manufacturer=0, overlap=16#20, gender=female, colors= << 16#a32a8f3d:32, 16#c178cb36:32, 16#1c97:16 >>
	}}},
	{16#09032a00, #psu_item{name="FOnewe Shoes",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1100, type=3, manufacturer=0, overlap=16#20, gender=female, colors= << 16#fa2a8a5a:32, 16#a21c4ac5:32, 16#45ce:16 >>
	}}},
	{16#09032b00, #psu_item{name="Normal Swim Sandals",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1b00, type=3, manufacturer=0, overlap=16#10, gender=female, colors= << 16#8f5dacf1:32, 16#7c4d1322:32, 16#ffec:16 >>
	}}},
	{16#09032c00, #psu_item{name="Bikini Swim Sandals",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1d00, type=3, manufacturer=0, overlap=16#10, gender=female, colors= << 16#d7fcab11:32, 16#7cf4c53b:32, 16#442c:16 >>
	}}},
	{16#09040000, #psu_item{name="FOnewm Jacket",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1b00, type=4, manufacturer=0, overlap=16#01, gender=male, colors= << 16#fb8d6e23:32, 16#71daf41d:32, 16#a7e6:16 >>
	}}},
	{16#09040100, #psu_item{name="Maydi Suit",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1700, type=4, manufacturer=3, overlap=16#01, gender=female, colors= << 16#ac8c2cec:32, 16#5c7c4cfc:32, 16#bc9c:16 >>
	}}},
	{16#09040200, #psu_item{name="Amorosso Top",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1900, type=4, manufacturer=2, overlap=16#01, gender=female, colors= << 16#f1e4df71:32, 16#7ea251ce:32, 16#8db7:16 >>
	}}},
	{16#09050000, #psu_item{name="AMF ArmyBottomsSet",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0700, type=5, manufacturer=0, overlap=16#10, gender=male, colors= << 16#9832f96d:32, 16#babd8ff5:32, 16#cbfc:16 >>
	}}},
	{16#09050100, #psu_item{name="Nobles Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0900, type=5, manufacturer=1, overlap=16#10, gender=male, colors= << 16#ac379d99:32, 16#fb9c6cca:32, 16#1c8c:16 >>
	}}},
	{16#09050200, #psu_item{name="Western Bottoms M",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0e00, type=5, manufacturer=7, overlap=16#10, gender=male, colors= << 16#283ff2b2:32, 16#8bc3635c:32, 16#7a3d:16 >>
	}}},
	{16#09050300, #psu_item{name="Roar Roars Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0f00, type=5, manufacturer=7, overlap=16#10, gender=male, colors= << 16#2353d999:32, 16#7986fce6:32, 16#ce4f:16 >>
	}}},
	{16#09050400, #psu_item{name="Smartia Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0800, type=5, manufacturer=2, overlap=16#10, gender=female, colors= << 16#7cfa4d89:32, 16#2adec1c2:32, 16#be75:16 >>
	}}},
	{16#09050500, #psu_item{name="AMF ArmyBottomsSet",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#0a00, type=5, manufacturer=0, overlap=16#10, gender=female, colors= << 16#af3d9fb7:32, 16#6efca598:32, 16#ca9e:16 >>
	}}},
	{16#09050600, #psu_item{name="Western Bottoms W",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1000, type=5, manufacturer=8, overlap=16#10, gender=female, colors= << 16#289dc1eb:32, 16#a39ef43e:32, 16#82bf:16 >>
	}}},
	{16#09050700, #psu_item{name="SPF Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1800, type=5, manufacturer=3, overlap=16#20, gender=male, colors= << 16#a45ecdac:32, 16#b39329f5:32, 16#e5df:16 >>
	}}},
	{16#09050800, #psu_item{name="RAmar Bottoms Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1c00, type=5, manufacturer=0, overlap=16#20, gender=male, colors= << 16#862ada5a:32, 16#fabaaaeb:32, 16#ea8a:16 >>
	}}},
	{16#09050900, #psu_item{name="Hanaura Bottoms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1300, type=5, manufacturer=5, overlap=16#20, gender=female, colors= << 16#f9824f2f:32, 16#919d5e92:32, 16#1cca:16 >>
	}}},
	{16#09050a00, #psu_item{name="RAmarl Bottoms Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2500, type=5, manufacturer=0, overlap=16#20, gender=female, colors= << 16#5debbf1b:32, 16#74debd23:32, 16#658b:16 >>
	}}},
	{16#09060000, #psu_item{name="Men's Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1e00, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#8b2cadab:32, 16#de54fa9a:32, 16#ce41:16 >>
	}}},
	{16#09060100, #psu_item{name="Bikini Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1f00, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#aaacda5c:32, 16#9288ad7a:32, 16#2de9:16 >>
	}}},
	{16#09060200, #psu_item{name="Rappy Suit",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2000, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#dc7cfcd8:32, 16#5d8cf84c:32, 16#7f2e:16 >>
	}}},
	{16#09060300, #psu_item{name="Formalwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2100, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#c7abc5ac:32, 16#cebdfb7b:32, 16#8c6b:16 >>
	}}},
	{16#09060400, #psu_item{name="Bikini Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1e00, type=6, manufacturer=0, overlap=16#00, gender=female, colors= << 16#d7fcab11:32, 16#7cf4c53b:32, 16#442c:16 >>
	}}},
	{16#09060500, #psu_item{name="Regular Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1f00, type=6, manufacturer=0, overlap=16#00, gender=female, colors= << 16#8f5dacf1:32, 16#7c4d1322:32, 16#ffec:16 >>
	}}},
	{16#09060600, #psu_item{name="One-piece Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2000, type=6, manufacturer=0, overlap=16#00, gender=female, colors= << 16#8cc1744d:32, 16#2c1f2df7:32, 16#a2d1:16 >>
	}}},
	{16#09060700, #psu_item{name="Formal Dress",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2100, type=6, manufacturer=0, overlap=16#00, gender=female, colors= << 16#f278dc14:32, 16#cd1fdeec:32, 16#89f3:16 >>
	}}},
	{16#09060800, #psu_item{name="Rappy Suit",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2200, type=6, manufacturer=0, overlap=16#00, gender=female, colors= << 16#de7cfcd8:32, 16#5d8cf84c:32, 16#7f2e:16 >>
	}}},
	{16#09060a00, #psu_item{name="Illuminus Coat",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1500, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#8792917c:32, 16#beb3822b:32, 16#5f32:16 >>
	}}},
	{16#09060b00, #psu_item{name="Miyabi-kata",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1a00, type=6, manufacturer=4, overlap=0, gender=male, colors= << 16#7d27d4b3:32, 16#f55e92fc:32, 16#2c3f:16 >>
	}}},
	{16#09060c00, #psu_item{name="FOmar Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1d00, type=6, manufacturer=0, overlap=0, gender=male, colors= << 16#8c2c5cfa:32, 16#a7c3da39:32, 16#7ffc:16 >>
	}}},
	{16#09060d00, #psu_item{name="GUARDIANS Formal",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1800, type=6, manufacturer=0, overlap=16#00, gender=female, colors= << 16#38368d2a:32, 16#dfc1ba9d:32, 16#1c5a:16 >>
	}}},
	{16#09060e00, #psu_item{name="Miyabi-kata",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2300, type=6, manufacturer=5, overlap=16#00, gender=female, colors= << 16#c81ad378:32, 16#f845aec3:32, 16#4e18:16 >>
	}}},
	{16#09060f00, #psu_item{name="Mikunas Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#1c00, type=6, manufacturer=0, overlap=16#00, gender=female, colors= << 16#f77d4ee7:32, 16#fa1af1ce:32, 16#2eda:16 >>
	}}},
	{16#09061000, #psu_item{name="FOmarl Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2400, type=6, manufacturer=0, overlap=16#00, gender=female, colors= << 16#dc2c8714:32, 16#2afcc147:32, 16#1137:16 >>
	}}},
	{16#09061100, #psu_item{name="Mikumiko Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2700, type=6, manufacturer=5, overlap=16#00, gender=female, colors= << 16#2a2e126e:32, 16#d5b6de58:32, 16#c8c2:16 >>
	}}},
	{16#09061200, #psu_item{name="Voloyal Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_clothing_item{
			appearance=16#2600, type=6, manufacturer=8, overlap=16#00, gender=female, colors= << 16#5dd782cf:32, 16#a478c7a2:32, 16#f52d:16 >>
	}}},

	%% Parts.

	{16#0a010000, #psu_item{name="Revsys Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f501, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a010100, #psu_item{name="Boktos Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f601, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a010200, #psu_item{name="Raptus Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f701, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a010300, #psu_item{name="Vilogis Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f801, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a010400, #psu_item{name="Lobas Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f901, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a010500, #psu_item{name="Evors Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fa01, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a010600, #psu_item{name="Hounds Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fb01, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a010700, #psu_item{name="Gimnas Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fc01, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a010800, #psu_item{name="Elaciel Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f501, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a010900, #psu_item{name="Batrelle Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f601, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a010a00, #psu_item{name="Redmiel Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f701, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a010b00, #psu_item{name="Digiel Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f801, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a010c00, #psu_item{name="Agriel Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f901, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a010d00, #psu_item{name="Epicarel Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fa01, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a010e00, #psu_item{name="Gimnael Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fb01, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a010f00, #psu_item{name="Amorel Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fc01, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a011000, #psu_item{name="Granadas Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fd01, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a011100, #psu_item{name="Apollos Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0202, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a011200, #psu_item{name="Musagante Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0302, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a011300, #psu_item{name="Subarga Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0402, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a011400, #psu_item{name="HUcas Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0702, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a011500, #psu_item{name="RAcas Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0602, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a011600, #psu_item{name="Valatines Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fd01, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a011700, #psu_item{name="Bacolone Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0302, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a011800, #psu_item{name="Lucaral Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0402, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a011900, #psu_item{name="Jenkel Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0502, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a011a00, #psu_item{name="RAcase Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0602, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a011b00, #psu_item{name="HUcase Torso",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0802, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a011c00, #psu_item{name="Revsys Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5902, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a011d00, #psu_item{name="Boktos Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5a02, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a011e00, #psu_item{name="Raptus Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5b02, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a011f00, #psu_item{name="Vilogis Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5c02, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a012000, #psu_item{name="Lobas Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5d02, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a012100, #psu_item{name="Evors Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5e02, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a012200, #psu_item{name="Hounds Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5f02, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a012300, #psu_item{name="Gimnas Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6002, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a012400, #psu_item{name="Elaciel Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5902, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a012500, #psu_item{name="Batrelle Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5a02, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a012600, #psu_item{name="Redmiel Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5b02, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a012700, #psu_item{name="Digiel Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5c02, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a012800, #psu_item{name="Agriel Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5d02, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a012900, #psu_item{name="Epicarel Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5e02, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a012a00, #psu_item{name="Gimnael Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5f02, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a012b00, #psu_item{name="Amorel Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6002, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a012c00, #psu_item{name="Granadas Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6102, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a012d00, #psu_item{name="Apollos Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6602, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a012e00, #psu_item{name="Musagante Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6702, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a012f00, #psu_item{name="Subarga Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6802, type=1, manufacturer=1, overlap=16#01, gender=male
	}}},
	{16#0a013000, #psu_item{name="Valatines Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6102, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a013100, #psu_item{name="Bacolone Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6702, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a013200, #psu_item{name="Lucaral Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6802, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a013300, #psu_item{name="Jenkel Torso CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6902, type=1, manufacturer=2, overlap=16#01, gender=female
	}}},
	{16#0a020000, #psu_item{name="Revsys Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f501, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a020100, #psu_item{name="Boktos Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f601, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a020200, #psu_item{name="Raptus Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f701, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a020300, #psu_item{name="Vilogis Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f801, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a020400, #psu_item{name="Lobas Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f901, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a020500, #psu_item{name="Evors Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fa01, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a020600, #psu_item{name="Hounds Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fb01, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a020700, #psu_item{name="Gimnas Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fc01, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a020800, #psu_item{name="Elaciel Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f501, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a020900, #psu_item{name="Batrelle Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f601, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a020a00, #psu_item{name="Redmiel Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f701, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a020b00, #psu_item{name="Digiel Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f801, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a020c00, #psu_item{name="Agriel Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f901, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a020d00, #psu_item{name="Epicarel Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fa01, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a020e00, #psu_item{name="Gimnael Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fb01, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a020f00, #psu_item{name="Amorel Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fc01, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a021000, #psu_item{name="Granadas Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fd01, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a021100, #psu_item{name="Apollos Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0202, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a021200, #psu_item{name="Musagante Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0302, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a021300, #psu_item{name="Subarga Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0402, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a021400, #psu_item{name="HUcas Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0702, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a021500, #psu_item{name="RAcas Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0602, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a021600, #psu_item{name="Valatines Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fd01, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a021700, #psu_item{name="Bacolone Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0302, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a021800, #psu_item{name="Lucaral Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0402, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a021900, #psu_item{name="Jenkel Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0502, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a021a00, #psu_item{name="RAcase Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0602, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a021b00, #psu_item{name="HUcase Legs",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0802, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a021c00, #psu_item{name="Revsys Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5902, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a021d00, #psu_item{name="Boktos Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5a02, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a021e00, #psu_item{name="Raptus Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5b02, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a021f00, #psu_item{name="Vilogis Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5c02, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a022000, #psu_item{name="Lobas Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5d02, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a022100, #psu_item{name="Evors Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5e02, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a022200, #psu_item{name="Hounds Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5f02, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a022300, #psu_item{name="Gimnas Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6002, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a022400, #psu_item{name="Elaciel Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5902, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a022500, #psu_item{name="Batrelle Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5a02, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a022600, #psu_item{name="Redmiel Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5b02, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a022700, #psu_item{name="Digiel Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5c02, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a022800, #psu_item{name="Agriel Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5d02, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a022900, #psu_item{name="Epicarel Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5e02, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a022a00, #psu_item{name="Gimnael Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5f02, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a022b00, #psu_item{name="Amorel Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6002, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a022c00, #psu_item{name="Granadas Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6102, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a022d00, #psu_item{name="Apollos Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6602, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a022e00, #psu_item{name="Musagante Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6702, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a022f00, #psu_item{name="Subarga Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6802, type=2, manufacturer=1, overlap=16#11, gender=male
	}}},
	{16#0a023000, #psu_item{name="Valatines Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6102, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a023100, #psu_item{name="Bacolone Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6702, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a023200, #psu_item{name="Lucaral Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6802, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a023300, #psu_item{name="Jenkel Legs CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6902, type=2, manufacturer=2, overlap=16#11, gender=female
	}}},
	{16#0a030000, #psu_item{name="Revsys Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f501, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a030100, #psu_item{name="Boktos Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f601, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a030200, #psu_item{name="Raptus Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f701, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a030300, #psu_item{name="Vilogis Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f801, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a030400, #psu_item{name="Lobas Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f901, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a030500, #psu_item{name="Evors Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fa01, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a030600, #psu_item{name="Hounds Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fb01, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a030700, #psu_item{name="Gimnas Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fc01, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a030800, #psu_item{name="Elaciel Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f501, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a030900, #psu_item{name="Batrelle Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f601, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a030a00, #psu_item{name="Redmiel Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f701, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a030b00, #psu_item{name="Digiel Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f801, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a030c00, #psu_item{name="Agriel Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#f901, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a030d00, #psu_item{name="Epicarel Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fa01, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a030e00, #psu_item{name="Gimnael Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fb01, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a030f00, #psu_item{name="Amorel Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fc01, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a031000, #psu_item{name="Granadas Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fd01, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a031100, #psu_item{name="Apollos Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0202, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a031200, #psu_item{name="Musagante Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0302, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a031300, #psu_item{name="Subarga Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0402, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a031400, #psu_item{name="HUcas Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0702, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a031500, #psu_item{name="RAcas Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0602, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a031600, #psu_item{name="Valatines Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fd01, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a031700, #psu_item{name="Bacolone Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0302, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a031800, #psu_item{name="Lucaral Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0402, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a031900, #psu_item{name="Jenkel Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0502, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a031a00, #psu_item{name="RAcase Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0602, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a031b00, #psu_item{name="HUcase Arms",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0802, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a031c00, #psu_item{name="Revsys Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5902, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a031d00, #psu_item{name="Boktos Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5a02, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a031e00, #psu_item{name="Raptus Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5b02, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a031f00, #psu_item{name="Vilogis Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5c02, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a032000, #psu_item{name="Lobas Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5d02, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a032100, #psu_item{name="Evors Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5e02, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a032200, #psu_item{name="Hounds Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5f02, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a032300, #psu_item{name="Gimnas Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6002, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a032400, #psu_item{name="Elaciel Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5902, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a032500, #psu_item{name="Batrelle Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5a02, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a032600, #psu_item{name="Redmiel Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5b02, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a032700, #psu_item{name="Digiel Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5c02, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a032800, #psu_item{name="Agriel Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5d02, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a032900, #psu_item{name="Epicarel Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5e02, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a032a00, #psu_item{name="Gimnael Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#5f02, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a032b00, #psu_item{name="Amorel Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6002, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a032c00, #psu_item{name="Granadas Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6102, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a032d00, #psu_item{name="Apollos Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6602, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a032e00, #psu_item{name="Musagante Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6702, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a032f00, #psu_item{name="Subarga Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6802, type=3, manufacturer=1, overlap=16#10, gender=male
	}}},
	{16#0a033000, #psu_item{name="Valatines Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6102, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a033100, #psu_item{name="Bacolone Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6702, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a033200, #psu_item{name="Lucaral Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6802, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a033300, #psu_item{name="Jenkel Arms CV",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#6902, type=3, manufacturer=2, overlap=16#10, gender=female
	}}},
	{16#0a060000, #psu_item{name="Men's Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fe01, type=6, manufacturer=0, overlap=16#00, gender=male
	}}},
	{16#0a060100, #psu_item{name="Bikini Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#ff01, type=6, manufacturer=0, overlap=16#00, gender=male
	}}},
	{16#0a060200, #psu_item{name="Rappy Suit",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0002, type=6, manufacturer=0, overlap=16#00, gender=male
	}}},
	{16#0a060300, #psu_item{name="Formalwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0102, type=6, manufacturer=0, overlap=16#00, gender=male
	}}},
	{16#0a060400, #psu_item{name="Bikini Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#fe01, type=6, manufacturer=0, overlap=16#00, gender=female
	}}},
	{16#0a060500, #psu_item{name="Regular Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#ff01, type=6, manufacturer=0, overlap=16#00, gender=female
	}}},
	{16#0a060600, #psu_item{name="One-piece Swimwear",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0002, type=6, manufacturer=0, overlap=16#00, gender=female
	}}},
	{16#0a060700, #psu_item{name="Rappy Suit",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0102, type=6, manufacturer=0, overlap=16#00, gender=female
	}}},
	{16#0a060800, #psu_item{name="Formal Dress",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0202, type=6, manufacturer=0, overlap=16#00, gender=female
	}}},
	{16#0a060900, #psu_item{name="CAS-yukata",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0502, type=6, manufacturer=1, overlap=16#00, gender=male
	}}},
	{16#0a060a00, #psu_item{name="CAS-yukata",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0702, type=6, manufacturer=2, overlap=16#00, gender=female
	}}},
	{16#0a060b00, #psu_item{name="Mikunas Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0b02, type=6, manufacturer=2, overlap=16#00, gender=female
	}}},
	{16#0a060c00, #psu_item{name="CAS-miko Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0902, type=6, manufacturer=2, overlap=16#00, gender=female
	}}},
	{16#0a060d00, #psu_item{name="CAS-oyal Set",
		rarity=5, buy_price=10, sell_price=1, data=#psu_parts_item{
			appearance=16#0a02, type=6, manufacturer=2, overlap=16#00, gender=female
	}}},

	%% Traps.
	%% @todo Handle type/race/gender restrictions.

	{16#0c010000, #psu_item{name="Damage Trap",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10, effect=0, type=damage}}},
	{16#0c010100, #psu_item{name="Burn Trap",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10, effect=2, type=trap}}},
	{16#0c010200, #psu_item{name="Freeze Trap",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10, effect=3, type=trap}}},
	{16#0c010300, #psu_item{name="Poison Trap",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10, effect=4, type=trap}}},
	{16#0c010400, #psu_item{name="Confusion Trap",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10, effect=5, type=trap}}},
	{16#0c010500, #psu_item{name="Sleep Trap",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10, effect=6, type=trap}}},
	{16#0c010600, #psu_item{name="Virus Trap",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10, effect=8, type=trap}}},
	{16#0c010700, #psu_item{name="Shock Trap",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10, effect=16, type=trap}}},
	{16#0c010800, #psu_item{name="Silence Trap",
		rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10, effect=9, type=trap}}},
	%% @todo Missing 0c010900 and 0c010a00.

	{16#0c020000, #psu_item{name="Damage Trap G",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10, effect=1, type=damage_g}}},
	{16#0c020100, #psu_item{name="Burn Trap G",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10, effect=2, type=trap_g}}},
	{16#0c020200, #psu_item{name="Freeze Trap G",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10, effect=3, type=trap_g}}},
	{16#0c020300, #psu_item{name="Poison Trap G",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10, effect=4, type=trap_g}}},
	{16#0c020400, #psu_item{name="Confusion Trap G",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10, effect=5, type=trap_g}}},
	{16#0c020500, #psu_item{name="Sleep Trap G",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10, effect=6, type=trap_g}}},
	{16#0c020600, #psu_item{name="Virus Trap G",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10, effect=8, type=trap_g}}},
	{16#0c020700, #psu_item{name="Shock Trap G",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10, effect=16, type=trap_g}}},
	{16#0c020800, #psu_item{name="Silence Trap G",
		rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10, effect=9, type=trap_g}}},

	{16#0c020900, #psu_item{name="Burn Trap EX",
		rarity=7, buy_price=650, sell_price=65, data=#psu_trap_item{max_quantity=10, effect=10, type=trap_ex}}},
	{16#0c020a00, #psu_item{name="Freeze Trap EX",
		rarity=7, buy_price=650, sell_price=65, data=#psu_trap_item{max_quantity=10, effect=11, type=trap_ex}}},
	{16#0c020b00, #psu_item{name="Stun Trap EX",
		rarity=7, buy_price=650, sell_price=65, data=#psu_trap_item{max_quantity=10, effect=12, type=trap_ex}}},

	%% Special items.

	{16#11010000, #psu_item{name="Goggles",
		rarity=1, buy_price=none, sell_price=16#fa0a1f00, data=#psu_special_item{}}},
	{16#11020000, #psu_item{name="Photon Eraser",
		rarity=1, buy_price=none, sell_price=16#fa0a1f00, data=#psu_special_item{}}},
	{16#11020100, #psu_item{name="Photon Reflector",
		rarity=1, buy_price=none, sell_price=16#fa0a1f00, data=#psu_special_item{}}},
	{16#11020200, #psu_item{name="Photon Breaker",
		rarity=1, buy_price=none, sell_price=16#fa0a1f00, data=#psu_special_item{}}}
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

-define(FEMALE_CLOTHES, [16#09011000, 16#09011100, 16#09011200, 16#09011300, 16#09011400, 16#09011500, 16#09011600, 16#09011700,
	16#09011800, 16#09011900, 16#09011a00, 16#09011b00, 16#09011c00, 16#09011d00, 16#09011e00, 16#09011f00, 16#09012a00,
	16#09012b00, 16#09012c00, 16#09012d00, 16#09012e00, 16#09012f00, 16#09013000, 16#09013100, 16#09013200, 16#09013300,
	16#09020c00, 16#09020d00, 16#09020e00, 16#09020f00, 16#09021000, 16#09021100, 16#09021200, 16#09021300, 16#09021400,
	16#09021500, 16#09021600, 16#09021700, 16#09021800, 16#09022200, 16#09022300, 16#09022400, 16#09022500, 16#09022600,
	16#09022700, 16#09022800, 16#09022900, 16#09030c00, 16#09030d00, 16#09030e00, 16#09030f00, 16#09031000, 16#09031100,
	16#09031200, 16#09031300, 16#09031400, 16#09031500, 16#09031600, 16#09031700, 16#09031800, 16#09032300, 16#09032400,
	16#09032500, 16#09032600, 16#09032700, 16#09032800, 16#09032900, 16#09032a00, 16#09032b00, 16#09032c00, 16#09040100,
	16#09040200, 16#09050400, 16#09050500, 16#09050600, 16#09050900, 16#09050a00, 16#09060400, 16#09060500, 16#09060600,
	16#09060700, 16#09060800, 16#09060d00, 16#09060e00, 16#09060f00, 16#09061000, 16#09061100, 16#09061200]).

-define(MALE_PARTS, [16#0a010000, 16#0a010100, 16#0a010200, 16#0a010300, 16#0a010400, 16#0a010500, 16#0a010600, 16#0a010700,
	16#0a011000, 16#0a011100, 16#0a011200, 16#0a011300, 16#0a011400, 16#0a011500, 16#0a011c00, 16#0a011d00, 16#0a011e00,
	16#0a011f00, 16#0a012000, 16#0a012100, 16#0a012200, 16#0a012300, 16#0a012c00, 16#0a012d00, 16#0a012e00, 16#0a012f00,
	16#0a020000, 16#0a020100, 16#0a020200, 16#0a020300, 16#0a020400, 16#0a020500, 16#0a020600, 16#0a020700, 16#0a021000,
	16#0a021100, 16#0a021200, 16#0a021300, 16#0a021400, 16#0a021500, 16#0a021c00, 16#0a021d00, 16#0a021e00, 16#0a021f00,
	16#0a022000, 16#0a022100, 16#0a022200, 16#0a022300, 16#0a022c00, 16#0a022d00, 16#0a022e00, 16#0a022f00, 16#0a030000,
	16#0a030100, 16#0a030200, 16#0a030300, 16#0a030400, 16#0a030500, 16#0a030600, 16#0a030700, 16#0a031000, 16#0a031100,
	16#0a031200, 16#0a031300, 16#0a031400, 16#0a031500, 16#0a031c00, 16#0a031d00, 16#0a031e00, 16#0a031f00, 16#0a032000,
	16#0a032100, 16#0a032200, 16#0a032300, 16#0a032c00, 16#0a032d00, 16#0a032e00, 16#0a032f00, 16#0a060000, 16#0a060100,
	16#0a060200, 16#0a060300, 16#0a060900]).

-define(FEMALE_PARTS, [16#0a010800, 16#0a010900, 16#0a010a00, 16#0a010b00, 16#0a010c00, 16#0a010d00, 16#0a010e00, 16#0a010f00,
	16#0a011600, 16#0a011700, 16#0a011800, 16#0a011900, 16#0a011a00, 16#0a011b00, 16#0a012400, 16#0a012500, 16#0a012600,
	16#0a012700, 16#0a012800, 16#0a012900, 16#0a012a00, 16#0a012b00, 16#0a013000, 16#0a013100, 16#0a013200, 16#0a013300,
	16#0a020800, 16#0a020900, 16#0a020a00, 16#0a020b00, 16#0a020c00, 16#0a020d00, 16#0a020e00, 16#0a020f00, 16#0a021600,
	16#0a021700, 16#0a021800, 16#0a021900, 16#0a021a00, 16#0a021b00, 16#0a022400, 16#0a022500, 16#0a022600, 16#0a022700,
	16#0a022800, 16#0a022900, 16#0a022a00, 16#0a022b00, 16#0a023000, 16#0a023100, 16#0a023200, 16#0a023300, 16#0a030800,
	16#0a030900, 16#0a030a00, 16#0a030b00, 16#0a030c00, 16#0a030d00, 16#0a030e00, 16#0a030f00, 16#0a031600, 16#0a031700,
	16#0a031800, 16#0a031900, 16#0a031a00, 16#0a031b00, 16#0a032400, 16#0a032500, 16#0a032600, 16#0a032700, 16#0a032800,
	16#0a032900, 16#0a032a00, 16#0a032b00, 16#0a033000, 16#0a033100, 16#0a033200, 16#0a033300, 16#0a060400, 16#0a060500,
	16#0a060600, 16#0a060700, 16#0a060800, 16#0a060a00, 16#0a060b00, 16#0a060c00, 16#0a060d00]).

-define(CONSUMABLES, [16#03010000, 16#03010100, 16#03010200, 16#03010300, 16#03010500, 16#03010600,
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

	{518, ?FEMALE_CLOTHES}, %% Parum vendor.
	{555, ?FEMALE_CLOTHES}, %% Moatoob vendor.
	{589, ?FEMALE_CLOTHES}, %% Neudaiz vendor.
	{626, ?FEMALE_CLOTHES}, %% Colony right vendor.
	{627, ?FEMALE_CLOTHES}, %% Colony left vendor.

	{521, ?MALE_PARTS}, %% Parum vendor.
	{558, ?MALE_PARTS}, %% Moatoob vendor.
	{595, ?MALE_PARTS}, %% Neudaiz vendor.
	{629, ?MALE_PARTS}, %% Colony left vendor.
	{630, ?MALE_PARTS}, %% Colony right vendor.

	{524, ?FEMALE_PARTS}, %% Parum vendor.
	{561, ?FEMALE_PARTS}, %% Moatoob vendor.
	{598, ?FEMALE_PARTS}, %% Neudaiz vendor.
	{632, ?FEMALE_PARTS}, %% Colony right vendor.
	{633, ?FEMALE_PARTS}, %% Colony left vendor.

	{512, ?CONSUMABLES}, %% Parum right vendor, Parum v1 field lobbies vendors. Parum GUARDIANS vendor.
	{513, ?CONSUMABLES}, %% Parum left vendor.
	{549, ?CONSUMABLES}, %% Moatoob right vendor, Moatoob v1 field lobbies vendors. Moatoob GUARDIANS vendor.
	{550, ?CONSUMABLES}, %% Moatoob left vendor.
	{586, ?CONSUMABLES}, %% Neudaiz only vendor, Neudaiz v1 field lobbies vendors. Neudaiz GUARDIANS vendor.
	{620, ?CONSUMABLES}, %% Colony right vendor, Colony v1 field lobbies vendors. Colony GUARDIANS vendor.
	{621, ?CONSUMABLES}, %% Colony left vendor.
	{622, ?CONSUMABLES}, %% v2 field lobbies vendors.

	%% @todo The following shops need items.

	{500, [16#03010000]}, %% Parum weapons left vendor.
	{501, [16#03010000]}, %% Parum weapons right vendor.
	{503, [16#03010000]}, %% Parum armors only vendor.
	{506, [16#03010000]}, %% Parum units only vendor.
	{509, [16#03010000]}, %% Parum discs left vendor.
	{510, [16#03010000]}, %% Parum discs center vendor.
	{511, [16#03010000]}, %% Parum discs right vendor.
	{527, [16#03010000]}, %% Parum materials center left vendor.
	{528, [16#03010000]}, %% Parum materials left vendor.
	{532, [16#03010000]}, %% Parum materials center vendor.
	{533, [16#03010000]}, %% Parum materials center right vendor.
	{534, [16#03010000]}, %% Parum materials right vendor.

	{537, [16#03010000]}, %% Neudaiz weapons left vendor.
	{538, [16#03010000]}, %% Neudaiz weapons right vendor.
	{540, [16#03010000]}, %% Neudaiz armors only vendor.
	{543, [16#03010000]}, %% Neudaiz units only vendor.
	{546, [16#03010000]}, %% Neudaiz discs center vendor.
	{547, [16#03010000]}, %% Neudaiz discs left vendor.
	{548, [16#03010000]}, %% Neudaiz discs right vendor.
	{564, [16#03010000]}, %% Neudaiz materials left vendor.
	{565, [16#03010000]}, %% Neudaiz materials center left vendor.
	{569, [16#03010000]}, %% Neudaiz materials right vendor.
	{570, [16#03010000]}, %% Neudaiz materials center right vendor.
	{571, [16#03010000]}, %% Neudaiz materials center vendor.

	{574, [16#03010000]}, %% Neudaiz weapons left vendor.
	{575, [16#03010000]}, %% Neudaiz weapons right vendor.
	{577, [16#03010000]}, %% Neudaiz armors only vendor.
	{580, [16#03010000]}, %% Neudaiz units only vendor.
	{583, [16#03010000]}, %% Neudaiz discs left vendor.
	{584, [16#03010000]}, %% Neudaiz discs center vendor.
	{585, [16#03010000]}, %% Neudaiz discs right vendor.
	{601, [16#03010000]}, %% Neudaiz materials left vendor.
	{602, [16#03010000]}, %% Neudaiz materials center left vendor.
	{606, [16#03010000]}, %% Neudaiz materials center vendor.
	{607, [16#03010000]}, %% Neudaiz materials center right vendor.
	{608, [16#03010000]}, %% Neudaiz materials right vendor.

	{611, [16#03010000]}, %% Colony weapons left vendor.
	{612, [16#03010000]}, %% Colony weapons center vendor.
	{613, [16#03010000]}, %% Colony weapons right vendor.
	{614, [16#03010000]}, %% Colony armors only vendor.
	{617, [16#03010000]}, %% Colony discs right vendor.
	{618, [16#03010000]}, %% Colony discs center vendor.
	{619, [16#03010000]}, %% Colony discs left vendor.
	{635, [16#03010000]}, %% Colony materials right vendor.
	{636, [16#03010000]}, %% Colony materials center right vendor.
	{640, [16#03010000]}, %% Colony materials center left vendor.
	{641, [16#03010000]}, %% Colony materials left vendor.
	{644, [16#03010000]}, %% KUBARA SPREAD.
	{645, [16#03010000]}, %% Colony units left vendor.
	{646, [16#03010000]}, %% Colony units right vendor.
	{648, [16#03010000]}, %% Colony deco right vendor.
	{649, [16#03010000]}, %% Colony deco center vendor.
	{650, [16#03010000]}  %% Colony deco left vendor.
]).

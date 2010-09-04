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

-record(psu_item, {name, rarity, buy_price, sell_price, data}).
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

	{16#03010000, #psu_item{name="Monomate", rarity=1, buy_price=50, sell_price=5, data=#psu_consumable_item{
		max_quantity=20, pt_diff=+100, status_effect=0, target=0, use_condition=0, item_effect=16#20
	}}},
	{16#03010100, #psu_item{name="Dimate", rarity=2, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=15, pt_diff=+1000, status_effect=0, target=0, use_condition=0, item_effect=16#20
	}}},
	{16#03010200, #psu_item{name="Trimate", rarity=3, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=10, pt_diff=+100, status_effect=0, target=0, use_condition=0, item_effect=16#30
	}}},
	{16#03010300, #psu_item{name="Star Atomizer", rarity=6, buy_price=1500, sell_price=150, data=#psu_consumable_item{
		max_quantity=5, pt_diff=+100, status_effect=0, target=1, use_condition=0, item_effect=16#30
	}}},
	%% @todo Missing 03010400.
	{16#03010500, #psu_item{name="Antimate", rarity=1, buy_price=100, sell_price=10, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=0, target=0, use_condition=0, item_effect=16#60
	}}},
	{16#03010600, #psu_item{name="Sol Atomizer", rarity=4, buy_price=300, sell_price=30, data=#psu_consumable_item{
		max_quantity=5, pt_diff=0, status_effect=0, target=1, use_condition=0, item_effect=16#60
	}}},
	{16#03010700, #psu_item{name="Moon Atomizer", rarity=5, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=5, pt_diff=+25, status_effect=0, target=1, use_condition=0, item_effect=16#B0
	}}},
	%% @todo Missing 03010800. Cosmo Atomizer?
	{16#03010900, #psu_item{name="Scape Doll", rarity=8, buy_price=10000, sell_price=10, data=#psu_consumable_item{
		max_quantity=1, pt_diff=+100, status_effect=0, target=0, use_condition=0, item_effect=16#D0
	}}},
	{16#03010a00, #psu_item{name="Agtaride", rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=17, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010b00, #psu_item{name="Defbaride", rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=19, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010c00, #psu_item{name="Retaride", rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=21, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010d00, #psu_item{name="Zodiaride", rarity=3, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=23, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03010e00, #psu_item{name="Megistaride", rarity=4, buy_price=300, sell_price=30, data=#psu_consumable_item{
		max_quantity=10, pt_diff=0, status_effect=26, target=0, use_condition=0, item_effect=16#22
	}}},
	{16#03020000, #psu_item{name="Photon Charge", rarity=3, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=10, pt_diff=+100, status_effect=0, target=4, use_condition=4, item_effect=16#30
	}}},
	%% @todo Missing 03020100. Cosmo Charge?

	%% Traps.
	%% @todo Handle type/race/gender restrictions.

	{16#0c010000, #psu_item{name="Damage Trap", rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010100, #psu_item{name="Burn Trap", rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010200, #psu_item{name="Freeze Trap", rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010300, #psu_item{name="Poison Trap", rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010400, #psu_item{name="Confusion Trap", rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010500, #psu_item{name="Sleep Trap", rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010600, #psu_item{name="Virus Trap", rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010700, #psu_item{name="Shock Trap", rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},
	{16#0c010800, #psu_item{name="Silence Trap", rarity=1, buy_price=50, sell_price=5, data=#psu_trap_item{max_quantity=10}}},

	{16#0c020000, #psu_item{name="Damage Trap G", rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020100, #psu_item{name="Burn Trap G", rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020200, #psu_item{name="Freeze Trap G", rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020300, #psu_item{name="Poison Trap G", rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020400, #psu_item{name="Confusion Trap G", rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020500, #psu_item{name="Sleep Trap G", rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020600, #psu_item{name="Virus Trap G", rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020700, #psu_item{name="Shock Trap G", rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020800, #psu_item{name="Silence Trap G", rarity=4, buy_price=350, sell_price=35, data=#psu_trap_item{max_quantity=10}}},

	{16#0c020900, #psu_item{name="Burn Trap EX", rarity=7, buy_price=650, sell_price=65, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020a00, #psu_item{name="Freeze Trap EX", rarity=7, buy_price=650, sell_price=65, data=#psu_trap_item{max_quantity=10}}},
	{16#0c020b00, #psu_item{name="Stun Trap EX", rarity=7, buy_price=650, sell_price=65, data=#psu_trap_item{max_quantity=10}}}
]).

%% Shops.

-define(STD_CONSUMABLES, [16#03010000, 16#03010100, 16#03010200, 16#03010300, 16#03010500, 16#03010600,
	16#03010700, 16#03010900, 16#03010a00, 16#03010b00, 16#03010c00, 16#03010d00, 16#03010e00, 16#03020000,
	16#0c010000, 16#0c010100, 16#0c010200, 16#0c010300, 16#0c010400, 16#0c010500, 16#0c010600, 16#0c010700, 16#0c010800,
	16#0c020000, 16#0c020100, 16#0c020200, 16#0c020300, 16#0c020400, 16#0c020500, 16#0c020600, 16#0c020700, 16#0c020800,
	16#0c020900, 16#0c020a00, 16#0c020b00]).

-define(SHOPS, [
	{512, ?STD_CONSUMABLES}, %% Parum right vendor, Parum v1 field lobbies vendors. Parum GUARDIANS vendor.
	{513, ?STD_CONSUMABLES}, %% Parum left vendor.
	{549, ?STD_CONSUMABLES}, %% Moatoob right vendor, Moatoob v1 field lobbies vendors. Moatoob GUARDIANS vendor.
	{550, ?STD_CONSUMABLES}, %% Moatoob left vendor.
	{586, ?STD_CONSUMABLES}, %% Neudaiz only vendor, Neudaiz v1 field lobbies vendors. Neudaiz GUARDIANS vendor.
	{620, ?STD_CONSUMABLES}, %% Colony right vendor, Colony v1 field lobbies vendors. Colony GUARDIANS vendor.
	{621, ?STD_CONSUMABLES}, %% Colony left vendor.
	{622, ?STD_CONSUMABLES}  %% v2 field lobbies vendors.
]).

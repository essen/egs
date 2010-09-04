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
-record(psu_consumable_item, {max_quantity, hp_diff, status_effect, target, use_condition, item_effect}).

%% Items.

-define(ITEMS, [
	%% Consumables.

	{16#03010000, #psu_item{name="Monomate", rarity=1, buy_price=50, sell_price=5, data=#psu_consumable_item{
		max_quantity=20, hp_diff=+100, status_effect=0, target=0, use_condition=0, item_effect=16#20
	}}},
	{16#03010100, #psu_item{name="Dimate", rarity=2, buy_price=150, sell_price=15, data=#psu_consumable_item{
		max_quantity=15, hp_diff=+500, status_effect=0, target=0, use_condition=0, item_effect=16#20
	}}},
	{16#03010200, #psu_item{name="Trimate", rarity=3, buy_price=500, sell_price=50, data=#psu_consumable_item{
		max_quantity=10, hp_diff=+2500, status_effect=0, target=0, use_condition=0, item_effect=16#30
	}}}
]).

%% Shops.

-define(STD_CONSUMABLES, [16#03010000, 16#03010100, 16#03010200]).

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

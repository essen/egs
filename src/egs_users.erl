%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Users handling.
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

-module(egs_users).
-export([read/1, select/1, write/1, delete/1, item_nth/2, item_add/3, item_qty_add/3, shop_enter/2, shop_leave/1, shop_get/1, money_add/2]).

-define(TABLE, users).

-include("include/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @spec do(Q) -> Record
%% @doc Perform a mnesia transaction using a QLC query.
do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

%% --

%% @spec read({pid, Pid}) -> {ok, User} | {error, badarg}
%% @spec read(ID) -> {ok, User} | {error, badarg}
read({pid, Pid}) ->
	List = do(qlc:q([X || X <- mnesia:table(?TABLE), X#?TABLE.pid =:= Pid])),
	case List of
		[] ->		{error, badarg};
		[User] ->	{ok, User}
	end;
read(ID) ->
	case mnesia:transaction(fun() -> mnesia:read({?TABLE, ID}) end) of
		{atomic, []} ->		{error, badarg};
		{atomic, [Val]} ->	{ok, Val}
	end.

%% @spec select(all) -> {ok, List}
%% @spec select({neighbors, User}) -> {ok, List}
%% @todo state = undefined | {wait_for_authentication, Key} | authenticated | online
select(all) ->
	List = do(qlc:q([X || X <- mnesia:table(?TABLE),
		X#?TABLE.pid /= undefined
	])),
	{ok, List};
select({neighbors, User}) ->
	List = do(qlc:q([X || X <- mnesia:table(?TABLE),
		X#?TABLE.id /= User#?TABLE.id,
		X#?TABLE.pid /= undefined,
		X#?TABLE.instancepid =:= User#?TABLE.instancepid,
		X#?TABLE.area =:= User#?TABLE.area
	])),
	{ok, List}.

%% @spec write(User) -> ok
write(User) ->
	mnesia:transaction(fun() -> mnesia:write(User) end).

%% @spec delete(ID) -> ok
delete(ID) ->
	mnesia:transaction(fun() -> mnesia:delete({?TABLE, ID}) end).

item_nth(GID, ItemIndex) ->
	{atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	lists:nth(ItemIndex + 1, (User#users.character)#characters.inventory).

item_add(GID, ItemID, Variables) ->
	{atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	Character = User#users.character,
	Inventory = Character#characters.inventory,
	Inventory2 = case Variables of
		#psu_consumable_item_variables{quantity=Quantity} ->
			#psu_item{data=#psu_consumable_item{max_quantity=MaxQuantity}} = egs_items_db:read(ItemID),
			{ItemID, #psu_consumable_item_variables{quantity=Quantity2}} = case lists:keyfind(ItemID, 1, Inventory) of
				false -> New = true, {ItemID, #psu_consumable_item_variables{quantity=0}};
				Tuple -> New = false, Tuple
			end,
			Quantity3 = Quantity + Quantity2,
			if	Quantity3 =< MaxQuantity ->
				lists:keystore(ItemID, 1, Inventory, {ItemID, #psu_consumable_item_variables{quantity=Quantity3}})
			end;
		#psu_trap_item_variables{quantity=Quantity} ->
			#psu_item{data=#psu_trap_item{max_quantity=MaxQuantity}} = egs_items_db:read(ItemID),
			{ItemID, #psu_trap_item_variables{quantity=Quantity2}} = case lists:keyfind(ItemID, 1, Inventory) of
				false -> New = true, {ItemID, #psu_trap_item_variables{quantity=0}};
				Tuple -> New = false, Tuple
			end,
			Quantity3 = Quantity + Quantity2,
			if	Quantity3 =< MaxQuantity ->
				lists:keystore(ItemID, 1, Inventory, {ItemID, #psu_trap_item_variables{quantity=Quantity3}})
			end;
		_ ->
			New = true,
			if	length(Inventory) < 60 ->
				Inventory ++ [{ItemID, Variables}]
			end
	end,
	Character2 = Character#characters{inventory=Inventory2},
	mnesia:transaction(fun() -> mnesia:write(User#users{character=Character2}) end),
	if New =:= false -> 16#ffffffff;
		true -> length(Inventory2)
	end.

%% @todo Consumable items.
item_qty_add(GID, ItemIndex, QuantityDiff) ->
	{atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	Character = User#users.character,
	Inventory = Character#characters.inventory,
	{ItemID, Variables} = lists:nth(ItemIndex + 1, Inventory),
	case Variables of
		#psu_trap_item_variables{quantity=Quantity} ->
			#psu_item{data=#psu_trap_item{max_quantity=MaxQuantity}} = egs_items_db:read(ItemID),
			Quantity2 = Quantity + QuantityDiff,
			if	Quantity2 =:= 0 ->
					Inventory2 = string:substr(Inventory, 1, ItemIndex) ++ string:substr(Inventory, ItemIndex + 2);
				Quantity2 > 0, Quantity2 =< MaxQuantity ->
					Variables2 = Variables#psu_trap_item_variables{quantity=Quantity2},
					Inventory2 = string:substr(Inventory, 1, ItemIndex) ++ [{ItemID, Variables2}] ++ string:substr(Inventory, ItemIndex + 2)
			end
	end,
	Character2 = Character#characters{inventory=Inventory2},
	mnesia:transaction(fun() -> mnesia:write(User#users{character=Character2}) end).

shop_enter(GID, ShopID) ->
	mnesia:transaction(fun() ->
		[User] = mnesia:wread({?TABLE, GID}),
		mnesia:write(User#users{shopid=ShopID})
	end).

shop_leave(GID) ->
	mnesia:transaction(fun() ->
		[User] = mnesia:wread({?TABLE, GID}),
		mnesia:write(User#users{shopid=undefined})
	end).

shop_get(GID) ->
	{atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	User#users.shopid.

money_add(GID, MoneyDiff) ->
	mnesia:transaction(fun() ->
		[User] = mnesia:wread({?TABLE, GID}),
		Character = User#users.character,
		Money = Character#characters.money + MoneyDiff,
		if Money >= 0 ->
			Character2 = Character#characters{money=Money},
			mnesia:write(User#users{character=Character2})
		end
	end).

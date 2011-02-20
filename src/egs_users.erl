%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
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
-behaviour(gen_server).

-export([start_link/0, stop/0, broadcast/2, set_zone/3]). %% API.
-export([read/1, select/1, write/1, delete/1, item_nth/2, item_add/3, item_qty_add/3,
		 shop_enter/2, shop_leave/1, shop_get/1, money_add/2]). %% Deprecated API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-define(SERVER, ?MODULE).

-include("include/records.hrl").

%% @todo We have a conflict with the global state record. Solve it then rename this to state.
-record(stateu, {
	users = [] :: list({GID::integer(), #users{}})
}).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
	gen_server:call(?SERVER, stop).

broadcast(Message, PlayersGID) ->
	gen_server:cast(?SERVER, {broadcast, Message, PlayersGID}).

set_zone(GID, ZonePid, LID) ->
	gen_server:call(?SERVER, {set_zone, GID, ZonePid, LID}).

%% Deprecated API.

%% @spec read({pid, Pid}) -> {ok, User} | {error, badarg}
%% @spec read(ID) -> {ok, User} | {error, badarg}
read(ID) ->
	gen_server:call(?SERVER, {read, ID}).

select(GIDsList) ->
	gen_server:call(?SERVER, {select, GIDsList}).

%% @spec write(User) -> ok
write(User) ->
	gen_server:call(?SERVER, {write, User}).

%% @spec delete(GID) -> ok
delete(GID) ->
	gen_server:call(?SERVER, {delete, GID}).

item_nth(GID, ItemIndex) ->
	gen_server:call(?SERVER, {item_nth, GID, ItemIndex}).

item_add(GID, ItemID, Variables) ->
	gen_server:call(?SERVER, {item_add, GID, ItemID, Variables}).

%% @todo Consumable items.
item_qty_add(GID, ItemIndex, QuantityDiff) ->
	gen_server:call(?SERVER, {item_qty_add, GID, ItemIndex, QuantityDiff}).

shop_enter(GID, ShopID) ->
	gen_server:call(?SERVER, {shop_enter, GID, ShopID}).

shop_leave(GID) ->
	gen_server:call(?SERVER, {shop_leave, GID}).

shop_get(GID) ->
	gen_server:call(?SERVER, {shop_get, GID}).

money_add(GID, MoneyDiff) ->
	gen_server:call(?SERVER, {money_add, GID, MoneyDiff}).

%% gen_server.

init([]) ->
	{ok, #stateu{}}.

handle_call({set_zone, GID, ZonePid, LID}, _From, State) ->
	{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
	Users2 = lists:delete({GID, User}, State#stateu.users),
	{reply, ok, State#stateu{users=[{GID, User#users{zonepid=ZonePid, lid=LID}}|Users2]}};

handle_call({read, {pid, Pid}}, _From, State) ->
	[User] = [User || {_GID, User} <- State#stateu.users, User#users.pid =:= Pid],
	{reply, {ok, User}, State};
handle_call({read, GID}, _From, State) ->
	{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
	{reply, {ok, User}, State};

handle_call({select, UsersGID}, _From, State) ->
	Users = [begin
		{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
		User
	 end || GID <- UsersGID],
	{reply, Users, State};

handle_call({write, User}, _From, State) ->
	Users2 = lists:keydelete(User#users.gid, 1, State#stateu.users),
	{reply, ok, State#stateu{users=[{User#users.gid, User}|Users2]}};

handle_call({delete, GID}, _From, State) ->
	Users2 = lists:keydelete(GID, 1, State#stateu.users),
	{reply, ok, State#stateu{users=Users2}};

handle_call({item_nth, GID, ItemIndex}, _From, State) ->
	{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
	Item = lists:nth(ItemIndex + 1, (User#users.character)#characters.inventory),
	{reply, Item, State};

handle_call({item_add, GID, ItemID, Variables}, _From, State) ->
	{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
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
	Users2 = lists:keydelete(User#users.gid, 1, State#stateu.users),
	State2 = State#stateu{users=[{GID, User#users{character=Character2}}|Users2]},
	case New of
		false -> {reply, 16#ffffffff, State2};
		true  -> {reply, length(Inventory2), State2}
	end;

handle_call({item_qty_add, GID, ItemIndex, QuantityDiff}, _From, State) ->
	{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
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
	Users2 = lists:keydelete(User#users.gid, 1, State#stateu.users),
	{reply, ok, State#stateu{users=[{GID, User#users{character=Character2}}|Users2]}};

handle_call({shop_enter, GID, ShopID}, _From, State) ->
	{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
	Users2 = lists:delete({GID, User}, State#stateu.users),
	{reply, ok, State#stateu{users=[{GID, User#users{shopid=ShopID}}|Users2]}};

handle_call({shop_leave, GID}, _From, State) ->
	{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
	Users2 = lists:delete({GID, User}, State#stateu.users),
	{reply, ok, State#stateu{users=[{GID, User#users{shopid=undefined}}|Users2]}};

handle_call({shop_get, GID}, _From, State) ->
	{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
	{reply, User#users.shopid, State};

handle_call({money_add, GID, MoneyDiff}, _From, State) ->
	{GID, User} = lists:keyfind(GID, 1, State#stateu.users),
	Character = User#users.character,
	Money = Character#characters.money + MoneyDiff,
	if Money >= 0 ->
		Character2 = Character#characters{money=Money},
		Users2 = lists:delete({GID, User}, State#stateu.users),
		{reply, ok, [{GID, User#users{character=Character2}}|Users2]}
	end;

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({broadcast, Message, PlayersGID}, State) ->
	[begin	{GID, #users{pid=Pid}} = lists:keyfind(GID, 1, State#stateu.users),
			Pid ! Message
	 end || GID <- PlayersGID],
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

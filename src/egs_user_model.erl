%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc User domain model.
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

-module(egs_user_model).
-behavior(gen_server).
-export([start_link/0, stop/0, read/1, select/1, write/1, delete/1, item_nth/2, item_add/3, item_qty_add/3, shop_enter/2, shop_leave/1, shop_get/1, money_add/2]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name and for the table name.
-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-include("include/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @spec do(Q) -> Record
%% @doc Perform a mnesia transaction using a QLC query.
do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

%% API

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec read({pid, Pid}) -> {ok, User} | {error, badarg}
%% @spec read(ID) -> {ok, User} | {error, badarg}
read(ID) ->
	gen_server:call(?SERVER, {read, ID}).

%% @spec select(all) -> {ok, List}
%% @spec select({neighbors, User}) -> {ok, List}
select(Type) ->
	gen_server:call(?SERVER, {select, Type}).

%% @spec write(User) -> ok
write(User) ->
	gen_server:cast(?SERVER, {write, User}).

%% @spec delete(ID) -> ok
delete(ID) ->
	gen_server:cast(?SERVER, {delete, ID}).

item_nth(GID, ItemIndex) ->
	gen_server:call(?SERVER, {item_nth, GID, ItemIndex}).

item_add(GID, ItemID, Variables) ->
	gen_server:call(?SERVER, {item_add, GID, ItemID, Variables}).

item_qty_add(GID, ItemIndex, QuantityDiff) ->
	gen_server:cast(?SERVER, {item_qty_add, GID, ItemIndex, QuantityDiff}).

shop_enter(GID, ShopID) ->
	gen_server:cast(?SERVER, {shop_enter, GID, ShopID}).

shop_leave(GID) ->
	gen_server:cast(?SERVER, {shop_leave, GID}).

shop_get(GID) ->
	gen_server:call(?SERVER, {shop_get, GID}).

money_add(GID, MoneyDiff) ->
	gen_server:cast(?SERVER, {money_add, GID, MoneyDiff}).

%% gen_server

init([]) ->
	error_logger:info_report("egs_user_model started"),
	{ok, undefined}.

handle_call({read, {pid, Pid}}, _From, State) ->
	List = do(qlc:q([X || X <- mnesia:table(?TABLE), X#?TABLE.pid =:= Pid])),
	case List of
		[] -> {reply, {error, badarg}, State};
		[User] -> {reply, {ok, User}, State}
	end;

handle_call({read, ID}, _From, State) ->
	case mnesia:transaction(fun() -> mnesia:read({?TABLE, ID}) end) of
		{atomic, []} -> {reply, {error, badarg}, State};
		{atomic, [Val]} -> {reply, {ok, Val}, State}
	end;

%% @todo state = undefined | {wait_for_authentication, Key} | authenticated | online
handle_call({select, all}, _From, State) ->
	List = do(qlc:q([X || X <- mnesia:table(?TABLE),
		X#?TABLE.pid /= undefined
	])),
	{reply, {ok, List}, State};

handle_call({select, {neighbors, User}}, _From, State) ->
	List = do(qlc:q([X || X <- mnesia:table(?TABLE),
		X#?TABLE.id /= User#?TABLE.id,
		X#?TABLE.pid /= undefined,
		X#?TABLE.instancepid =:= User#?TABLE.instancepid,
		X#?TABLE.area =:= User#?TABLE.area
	])),
	{reply, {ok, List}, State};

handle_call({item_nth, GID, ItemIndex}, _From, State) ->
	{atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	{reply, lists:nth(ItemIndex + 1, (User#egs_user_model.character)#characters.inventory), State};

handle_call({item_add, GID, ItemID, Variables}, _From, State) ->
	{atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	Character = User#egs_user_model.character,
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
	mnesia:transaction(fun() -> mnesia:write(User#egs_user_model{character=Character2}) end),
	if New =:= false -> {reply, 16#ffffffff, State};
		true -> {reply, length(Inventory2), State}
	end;

handle_call({shop_get, GID}, _From, State) ->
	{atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	{reply, User#egs_user_model.shopid, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({write, User}, State) ->
	mnesia:transaction(fun() -> mnesia:write(User) end),
	{noreply, State};

handle_cast({delete, ID}, State) ->
	mnesia:transaction(fun() -> mnesia:delete({?TABLE, ID}) end),
	{noreply, State};

%% @todo Consumable items.
handle_cast({item_qty_add, GID, ItemIndex, QuantityDiff}, State) ->
	{atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({?TABLE, GID}) end),
	Character = User#egs_user_model.character,
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
	mnesia:transaction(fun() -> mnesia:write(User#egs_user_model{character=Character2}) end),
	{noreply, State};

handle_cast({shop_enter, GID, ShopID}, State) ->
	mnesia:transaction(fun() ->
		[User] = mnesia:wread({?TABLE, GID}),
		mnesia:write(User#egs_user_model{shopid=ShopID})
	end),
	{noreply, State};

handle_cast({shop_leave, GID}, State) ->
	mnesia:transaction(fun() ->
		[User] = mnesia:wread({?TABLE, GID}),
		mnesia:write(User#egs_user_model{shopid=undefined})
	end),
	{noreply, State};

handle_cast({money_add, GID, MoneyDiff}, State) ->
	mnesia:transaction(fun() ->
		[User] = mnesia:wread({?TABLE, GID}),
		Character = User#egs_user_model.character,
		Money = Character#characters.money + MoneyDiff,
		if Money >= 0 ->
			Character2 = Character#characters{money=Money},
			mnesia:write(User#egs_user_model{character=Character2})
		end
	end),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

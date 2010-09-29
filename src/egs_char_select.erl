%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Character selection callback module.
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

-module(egs_char_select).
-export([keepalive/1, info/2, cast/3, raw/3, event/2]).

-include("include/records.hrl").
-include("include/psu/items.hrl").

%% @doc Send a keepalive.
keepalive(#state{socket=Socket}) ->
	psu_proto:send_keepalive(Socket).

%% @doc We don't expect any message here.
%% @todo Throw an error instead?
info(_Msg, _State) ->
	ok.

%% @doc Nothing to broadcast.
%% @todo Throw an error instead?
cast(_Command, _Data, _State) ->
	ok.

%% @doc Dismiss all raw commands with a log notice.
%% @todo Have a log event handler instead.
raw(Command, _Data, State) ->
	io:format("~p (~p): dismissed command ~4.16.0b~n", [?MODULE, State#state.gid, Command]).

%% Events.

%% @doc Character screen selection request and delivery.
event(char_select_request, #state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	psu_game:send_0d03(data_load(User#egs_user_model.folder, 0), data_load(User#egs_user_model.folder, 1), data_load(User#egs_user_model.folder, 2), data_load(User#egs_user_model.folder, 3));

%% @doc The options default to 0 for everything except brightness to 4.
%% @todo Don't forget to check for the character's name.
event({char_select_create, Slot, CharBin}, #state{gid=GID}) ->
	%% check for valid character appearance
	%~ << _Name:512, RaceID:8, GenderID:8, _TypeID:8, AppearanceBin:776/bits, _/bits >> = CharBin,
	%~ Race = proplists:get_value(RaceID, [{0, human}, {1, newman}, {2, cast}, {3, beast}]),
	%~ Gender = proplists:get_value(GenderID, [{0, male}, {1, female}]),
	%~ Appearance = psu_appearance:binary_to_tuple(Race, AppearanceBin),
	%~ psu_characters:validate_name(Name),
	%~ psu_appearance:validate_char_create(Race, Gender, Appearance),
	%% end of check, continue doing it wrong past that point for now
	{ok, User} = egs_user_model:read(GID),
	Dir = io_lib:format("save/~s", [User#egs_user_model.folder]),
	File = io_lib:format("~s/~b-character", [Dir, Slot]),
	_ = file:make_dir(Dir),
	file:write_file(File, CharBin),
	file:write_file(io_lib:format("~s.options", [File]), << 0:128, 4, 0:56 >>);

%% @doc Load the selected character into the game's universe.
event({char_select_enter, Slot, _BackToPreviousField}, State=#state{gid=GID}) ->
	{ok, User} = egs_user_model:read(GID),
	[{status, 1}, {char, CharBin}, {options, OptionsBin}] = data_load(User#egs_user_model.folder, Slot),
	<< Name:512/bits, RaceBin:8, GenderBin:8, ClassBin:8, AppearanceBin:776/bits, _/bits >> = CharBin,
	Race = psu_characters:race_binary_to_atom(RaceBin),
	Gender = psu_characters:gender_binary_to_atom(GenderBin),
	Class = psu_characters:class_binary_to_atom(ClassBin),
	Appearance = psu_appearance:binary_to_tuple(Race, AppearanceBin),
	Options = psu_characters:options_binary_to_tuple(OptionsBin),
	Character = #characters{slot=Slot, name=Name, race=Race, gender=Gender, class=Class, appearance=Appearance, options=Options, % TODO: temporary set the slot here, won't be needed later
		inventory= [{16#11010000, #psu_special_item_variables{}}, {16#11020000, #psu_special_item_variables{}}, {16#11020100, #psu_special_item_variables{}}, {16#11020200, #psu_special_item_variables{}},
		{16#03010000, #psu_consumable_item_variables{quantity=10}},
		{16#03020000, #psu_consumable_item_variables{quantity=3}},
		{16#03010900, #psu_consumable_item_variables{quantity=1}},
		{16#0c020900, #psu_trap_item_variables{quantity=10}},
		{16#0c020a00, #psu_trap_item_variables{quantity=10}},
		{16#0c020b00, #psu_trap_item_variables{quantity=10}},
		{16#09061000, #psu_clothing_item_variables{color=0}},
		{16#09061000, #psu_clothing_item_variables{color=1}},
		{16#09061000, #psu_clothing_item_variables{color=2}},
		{16#09061000, #psu_clothing_item_variables{color=3}},
		{16#09061000, #psu_clothing_item_variables{color=4}},
		{16#09061000, #psu_clothing_item_variables{color=5}},
		{16#09061000, #psu_clothing_item_variables{color=6}},
		{16#09061000, #psu_clothing_item_variables{color=7}},
		{16#09061000, #psu_clothing_item_variables{color=8}},
		{16#09061000, #psu_clothing_item_variables{color=9}},
		{16#0a060c00, #psu_parts_item_variables{}},
		{16#0a060d00, #psu_parts_item_variables{}},
		{16#01010900, #psu_striking_weapon_item_variables{current_pp=99, max_pp=100, element=#psu_element{type=1, percent=50}}},
		{16#01010a00, #psu_striking_weapon_item_variables{current_pp=99, max_pp=100, element=#psu_element{type=2, percent=50}}},
		{16#01010b00, #psu_striking_weapon_item_variables{current_pp=99, max_pp=100, element=#psu_element{type=3, percent=50}}}]},
	User2 = User#egs_user_model{state=online, character=Character, area=#psu_area{questid=1100000, zoneid=0, mapid=4}, entryid=5},
	egs_user_model:write(User2),
	psu_game:char_load(User2),
	{ok, egs_game, State}.

%% Internal.

%% @doc Load the given character's data.
%% @todo This function is temporary until we get permanent mnesia accounts.
data_load(Folder, Number) ->
	Filename = io_lib:format("save/~s/~b-character", [Folder, Number]),
	case file:read_file(Filename) of
		{ok, Char} ->
			{ok, Options} = file:read_file(io_lib:format("~s.options", [Filename])),
			[{status, 1}, {char, Char}, {options, Options}];
		{error, _Reason} ->
			[{status, 0}, {char, << 0:2208 >>}]
	end.

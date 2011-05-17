%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
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

-include("include/types.hrl").
-include("include/records.hrl").

%% @doc Send a keepalive.
keepalive(Client) ->
	egs_proto:send_keepalive(Client).

%% @doc We don't expect any message here.
info(_Msg, _Client) ->
	ok.

%% @doc Nothing to broadcast.
cast(_Command, _Data, _Client) ->
	ok.

%% @doc Dismiss all raw commands with a log notice.
%% @todo Have a log event handler instead.
raw(Command, _Data, Client) ->
	io:format("~p (~p): dismissed command ~4.16.0b~n", [?MODULE, Client#client.gid, Command]).

%% Events.

%% @doc Character screen selection request and delivery.
event(char_select_request, Client=#client{gid=GID}) ->
	Folder = egs_accounts:get_folder(GID),
	egs_proto:send_0d03(data_load(Folder, 0), data_load(Folder, 1), data_load(Folder, 2), data_load(Folder, 3), Client);

%% @doc The options default to 0 for everything except brightness to 4.
%% @todo Don't forget to check for the character's name.
%%       00F7 is the RGBA color control character.
%%       03F7 is the RGB color control character.
event({char_select_create, Slot, CharBin}, #client{gid=GID}) ->
	%% check for valid character appearance
	%~ << _Name:512, RaceID:8, GenderID:8, _TypeID:8, AppearanceBin:776/bits, _/bits >> = CharBin,
	%~ Race = proplists:get_value(RaceID, [{0, human}, {1, newman}, {2, cast}, {3, beast}]),
	%~ Gender = proplists:get_value(GenderID, [{0, male}, {1, female}]),
	%~ Appearance = psu_appearance:binary_to_tuple(Race, AppearanceBin),
	%~ psu_appearance:validate_char_create(Race, Gender, Appearance),
	%% end of check, continue doing it wrong past that point for now
	Folder = egs_accounts:get_folder(GID),
	Dir = io_lib:format("save/~s", [Folder]),
	File = io_lib:format("~s/~b-character", [Dir, Slot]),
	_ = file:make_dir(Dir),
	file:write_file(File, CharBin),
	file:write_file(io_lib:format("~s.options", [File]), << 0:128, 4, 0:56 >>);

%% @doc Load the selected character into the game's default universe.
%% @todo Isn't very pretty to call egs_game from here but that will do for now.
event({char_select_enter, Slot, _BackToPreviousField}, Client=#client{gid=GID}) ->
	Folder = egs_accounts:get_folder(GID),
	[{status, 1}, {char, CharBin}, {options, OptionsBin}] = data_load(Folder, Slot),
	<< Name:512/bits, RaceBin:8, GenderBin:8, ClassBin:8, AppearanceBin:776/bits, _/bits >> = CharBin,
	Race = psu_characters:race_binary_to_atom(RaceBin),
	Gender = psu_characters:gender_binary_to_atom(GenderBin),
	Class = psu_characters:class_binary_to_atom(ClassBin),
	Appearance = psu_appearance:binary_to_tuple(Race, AppearanceBin),
	Options = psu_characters:options_binary_to_tuple(OptionsBin),
	UniID = egs_universes:defaultid(),
	egs_universes:enter(UniID),
	User = #users{gid=GID, pid=self(), uni=UniID, slot=Slot, name=Name, race=Race, gender=Gender,
		class=Class, appearance=Appearance, options=Options, area={1100000, 0, 4}, entryid=0},
	egs_users:write(User),
	egs_game_server:link_exit(),
	egs_users:item_add(GID, 16#11010000, #psu_special_item_variables{}),
	egs_users:item_add(GID, 16#11020000, #psu_special_item_variables{}),
	egs_users:item_add(GID, 16#11020100, #psu_special_item_variables{}),
	egs_users:item_add(GID, 16#11020200, #psu_special_item_variables{}),
	egs_users:item_add(GID, 16#01010900, #psu_striking_weapon_item_variables{current_pp=99, max_pp=100, element=#psu_element{type=1, percent=50}}),
	egs_users:item_add(GID, 16#01010a00, #psu_striking_weapon_item_variables{current_pp=99, max_pp=100, element=#psu_element{type=2, percent=50}}),
	egs_users:item_add(GID, 16#01010b00, #psu_striking_weapon_item_variables{current_pp=99, max_pp=100, element=#psu_element{type=3, percent=50}}),
	{ok, User2} = egs_users:read(GID),
	Client2 = Client#client{slot=Slot},
	egs_game:char_load(User2, Client2),
	{ok, egs_game, Client2}.

%% Internal.

%% @doc Load the given character's data.
%% @todo This function is temporary until we get permanent accounts.
data_load(Folder, Number) ->
	Filename = io_lib:format("save/~s/~b-character", [Folder, Number]),
	case file:read_file(Filename) of
		{ok, Char} ->
			{ok, Options} = file:read_file(io_lib:format("~s.options", [Filename])),
			[{status, 1}, {char, Char}, {options, Options}];
		{error, _Reason} ->
			[{status, 0}, {char, << 0:2208 >>}]
	end.

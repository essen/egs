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
-export([info/2, cast/3, event/2]).

-include_lib("erlson/include/erlson.hrl").
-include("include/records.hrl").

%% @doc We don't expect any message here.
info(_Msg, _Client) ->
	ok.

%% @doc Nothing to broadcast.
cast(_Command, _Data, _Client) ->
	ok.

%% Events.

%% @doc Character screen selection request and delivery.
event(account_characters_request, Client) ->
	{ok, Characters}
		= egs_store:load_characters(egs_net:get_gid(Client), [0, 1, 2, 3]),
	egs_net:account_characters_response(Characters, Client);

%% @todo Don't forget to check for the character's name (in egs_net).
%%       00F7 is the RGBA color control character.
%%       03F7 is the RGB color control character.
event({account_create_character, Slot, Name, Race, Gender, Class, Appearance},
		Client) ->
	Character = egs_char:new(Slot, Name, Race, Gender, Class, Appearance),
	egs_store:save_character(egs_net:get_gid(Client), Slot, Character);

%% @doc Load the selected character into the game's default universe.
%% @todo Isn't very pretty to call egs_game from here but that will do for now.
event({system_character_select, Slot, _BackToPreviousField}, Client) ->
	GID = egs_net:get_gid(Client),
	{ok, 1, Char} = egs_store:load_character(GID, Slot),
	UniID = egs_universes:defaultid(),
	egs_universes:enter(UniID),
	%% @todo Handle users properly, just giving Character directly.
	Name = Char.name,
	NameBin = << Name/binary, 0:(512 - bit_size(Name)) >>,
	Race = Char.race,
	Gender = Char.gender,
	Class = Char.class,
	Appearance = Char.appearance,
	Options = Char.options,
	User = #users{gid=GID, pid=self(), uni=UniID, slot=Slot,
		name=NameBin, race=Race, gender=Gender, class=Class,
		appearance=Appearance, options=Options,
		area={1100000, 0, 4}, entryid=0, char=Char},
	egs_users:write(User),
	egs_users:item_add(GID, 16#11010000, #psu_special_item_variables{}),
	egs_users:item_add(GID, 16#11020000, #psu_special_item_variables{}),
	egs_users:item_add(GID, 16#11020100, #psu_special_item_variables{}),
	egs_users:item_add(GID, 16#11020200, #psu_special_item_variables{}),
	egs_users:item_add(GID, 16#01010900, #psu_striking_weapon_item_variables{
		current_pp=99, max_pp=100, element=#psu_element{type=1, percent=50}}),
	egs_users:item_add(GID, 16#01010a00, #psu_striking_weapon_item_variables{
		current_pp=99, max_pp=100, element=#psu_element{type=2, percent=50}}),
	egs_users:item_add(GID, 16#01010b00, #psu_striking_weapon_item_variables{
		current_pp=99, max_pp=100, element=#psu_element{type=3, percent=50}}),
	{ok, User2} = egs_users:read(GID),
	egs_game:char_load(User2, Client),
	Client2 = egs_net:set_handler(egs_game, Client),
	{ok, Client2};

event({client_hardware, GPU, CPU}, Client) ->
	ok.

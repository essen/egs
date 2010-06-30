%	EGS: Erlang Game Server
%	Copyright (C) 2010  Loic Hoguin
%
%	This file is part of EGS.
%
%	EGS is free software: you can redistribute it and/or modify
%	it under the terms of the GNU General Public License as published by
%	the Free Software Foundation, either version 3 of the License, or
%	(at your option) any later version.
%
%	EGS is distributed in the hope that it will be useful,
%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%	GNU General Public License for more details.
%
%	You should have received a copy of the GNU General Public License
%	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

-module(psu_characters).
-export([options_binary_to_tuple/1, options_tuple_to_binary/1, validate_options/1]).

-include("include/records.hrl").

%% @doc Convert the binary options data into a tuple.
%%      The few unknown values are probably PS2 or 360 only.

options_binary_to_tuple(Binary) ->
	<<	TextDisplaySpeed:8, Sound:8, MusicVolume:8, SoundEffectVolume:8, Vibration:8, RadarMapDisplay:8,
		CutInDisplay:8, MainMenuCursorPosition:8, _:8, Camera3rdY:8, Camera3rdX:8, Camera1stY:8, Camera1stX:8,
		Controller:8, WeaponSwap:8, LockOn:8, Brightness:8, FunctionKeySetting:8, _:8, ButtonDetailDisplay:8, _:32 >> = Binary,
	{options, TextDisplaySpeed, Sound, MusicVolume, SoundEffectVolume, Vibration, RadarMapDisplay,
		CutInDisplay, MainMenuCursorPosition, Camera3rdY, Camera3rdX, Camera1stY, Camera1stX,
		Controller, WeaponSwap, LockOn, Brightness, FunctionKeySetting, ButtonDetailDisplay}.

%% @doc Convert a tuple of options data into a binary to be sent to clients.
%% @todo Write the function body!

options_tuple_to_binary(_Tuple) ->
	{error, todo}.

%% @doc Validate the options data.
%%      Trigger an exception rather than handling errors.

validate_options(Tuple) ->
	{options, TextDisplaySpeed, Sound, MusicVolume, SoundEffectVolume, Vibration, RadarMapDisplay,
		CutInDisplay, MainMenuCursorPosition, Camera3rdY, Camera3rdX, Camera1stY, Camera1stX,
		Controller, WeaponSwap, LockOn, Brightness, FunctionKeySetting, ButtonDetailDisplay} = Tuple,
	true = TextDisplaySpeed =< 1,
	true = Sound =< 1,
	true = MusicVolume =< 9,
	true = SoundEffectVolume =< 9,
	true = Vibration =< 1,
	true = RadarMapDisplay =< 1,
	true = CutInDisplay =< 1,
	true = MainMenuCursorPosition =< 1,
	true = Camera3rdY =< 1,
	true = Camera3rdX =< 1,
	true = Camera1stY =< 1,
	true = Camera1stX =< 1,
	true = Controller =< 1,
	true = WeaponSwap =< 1,
	true = LockOn =< 1,
	true = Brightness =< 4,
	true = FunctionKeySetting =< 1,
	true = ButtonDetailDisplay =< 2.

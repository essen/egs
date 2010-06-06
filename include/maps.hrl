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

%% EGS maps settings.

-define(QUESTS, [
	% Current test mission

	{1000013, [{type, mission},   {file, "data/missions/test.quest.nbl"}]},

	% Planetary lobbies

	{1100000, [{type, lobby},     {file, "data/lobby/colony.quest.nbl"}]},
	{1101000, [{type, lobby},     {file, "data/lobby/parum.quest.nbl"}]},
	{1102000, [{type, lobby},     {file, "data/lobby/neudaiz.quest.nbl"}]},
	{1103000, [{type, lobby},     {file, "data/lobby/moatoob.quest.nbl"}]},
	{1104000, [{type, spaceport}, {file, "data/lobby/spaceport.quest.nbl"}]}
]).

-define(ZONES, [
	% Current test mission

	{[1000013, 0], [{file, "data/missions/test.zone.nbl"}]},

	% Colony

	{[1100000, 0], [{file, "data/lobby/colony.zone-0.nbl"}]},
	{[1100000, 1], [{file, "data/lobby/colony.zone-1.nbl"}]},
	{[1100000, 2], [{file, "data/lobby/colony.zone-2.nbl"}]},
	{[1100000, 3], [{file, "data/lobby/colony.zone-3.nbl"}]},
	{[1100000, 4], [{file, "data/lobby/colony.zone-4.nbl"}]},
	{[1100000, 7], [{file, "data/lobby/colony.zone-7.nbl"}]},
	{[1100000,11], [{file, "data/lobby/colony.zone-11.nbl"}]},
	{[1100000,12], [{file, "data/lobby/colony.zone-12.nbl"}]},
	{[1100000,13], [{file, "data/lobby/colony.zone-13.nbl"}]},

	% Parum

	{[1101000, 0], [{file, "data/lobby/parum.zone-0.nbl"}]},
	{[1101000, 1], [{file, "data/lobby/parum.zone-1.nbl"}]},
	{[1101000, 2], [{file, "data/lobby/parum.zone-2.nbl"}]},
	{[1101000, 3], [{file, "data/lobby/parum.zone-3.nbl"}]},
	{[1101000, 4], [{file, "data/lobby/parum.zone-4.nbl"}]},
	{[1101000, 5], [{file, "data/lobby/parum.zone-5.nbl"}]},
	{[1101000, 7], [{file, "data/lobby/parum.zone-7.nbl"}]},
	{[1101000,11], [{file, "data/lobby/parum.zone-11.nbl"}]},
	{[1101000,12], [{file, "data/lobby/parum.zone-12.nbl"}]},
	{[1101000,13], [{file, "data/lobby/parum.zone-13.nbl"}]},

	% Neudaiz

	{[1102000, 0], [{file, "data/lobby/neudaiz.zone-0.nbl"}]},
	{[1102000, 1], [{file, "data/lobby/neudaiz.zone-1.nbl"}]},
	{[1102000, 2], [{file, "data/lobby/neudaiz.zone-2.nbl"}]},
	{[1102000, 3], [{file, "data/lobby/neudaiz.zone-3.nbl"}]},
	{[1102000, 4], [{file, "data/lobby/neudaiz.zone-4.nbl"}]},
	{[1102000, 7], [{file, "data/lobby/neudaiz.zone-7.nbl"}]},
	{[1102000,11], [{file, "data/lobby/neudaiz.zone-11.nbl"}]},
	{[1102000,12], [{file, "data/lobby/neudaiz.zone-12.nbl"}]},
	{[1102000,13], [{file, "data/lobby/neudaiz.zone-13.nbl"}]},

	% Moatoob

	{[1103000, 0], [{file, "data/lobby/moatoob.zone-0.nbl"}]},
	{[1103000, 1], [{file, "data/lobby/moatoob.zone-1.nbl"}]},
	{[1103000, 3], [{file, "data/lobby/moatoob.zone-3.nbl"}]},
	{[1103000, 4], [{file, "data/lobby/moatoob.zone-4.nbl"}]},
	{[1103000, 5], [{file, "data/lobby/moatoob.zone-5.nbl"}]},
	{[1103000, 6], [{file, "data/lobby/moatoob.zone-6.nbl"}]},
	{[1103000, 7], [{file, "data/lobby/moatoob.zone-7.nbl"}]},
	{[1103000,11], [{file, "data/lobby/moatoob.zone-11.nbl"}]},
	{[1103000,12], [{file, "data/lobby/moatoob.zone-12.nbl"}]},
	{[1103000,13], [{file, "data/lobby/moatoob.zone-13.nbl"}]},

	% Spaceport

	{[1104000, 0], [{file, "data/lobby/spaceport.zone.nbl"}]}
]).

-define(MAPS, [
	% Colony

	{[1100000,   1], [{name, "Colony 1st Floor"}]},
	{[1100000,   2], [{name, "Colony 2nd Floor"}]},
	{[1100000,   3], [{name, "Colony 3rd Floor"}]},
	{[1100000,   4], [{name, "Colony 4th Floor"}]},
	{[1100000,   5], [{name, "Colony GUARDIANS"}]},
	{[1100000, 100], [{name, "Colony 2nd, Grind Shop"}]},
	{[1100000, 100], [{name, "Colony 2nd, Synth Shop"}]},
	{[1100000, 100], [{name, "Colony 2nd, Decos Shop"}]},
	{[1100000, 101], [{name, "Colony 2nd, Items Shop"}]},
	{[1100000, 101], [{name, "Colony 2nd, Weapons Shop"}]},
	{[1100000, 101], [{name, "Colony 2nd, Armors Shop"}]},
	{[1100000, 102], [{name, "Colony 3rd, Lumilass"}]},
	{[1100000, 102], [{name, "Colony 3rd, Clothes Shop"}]},
	{[1100000, 102], [{name, "Colony 3rd, Parts Shop"}]},
	{[1100000, 103], [{name, "Colony Club"}]},
	{[1100000, 110], [{name, "Colony R&D"}]},
	{[1100000,9000], [{name, "Colony Aurorey"}]},
	{[1100000,9001], [{name, "Colony Transfer Terminal"}]},
	{[1100000,9010], [{name, "Colony Dallgun"}]},
	{[1100000,9102], [{name, "Colony HIVE"}]},
	{[1100000,9200], [{name, "Colony Rykros"}]},
	{[1100000,9202], [{name, "Colony Falz Memoria"}]},

	% Parum

	{[1101000,   1], [{name, "Parum City Central"}]},
	{[1101000,   2], [{name, "Parum City West"}]},
	{[1101000,   3], [{name, "Parum City East"}]},
	{[1101000,   4], [{name, "Parum GUARDIANS"}]},
	{[1101000, 100], [{name, "Parum Synth Shop"}]},
	{[1101000, 100], [{name, "Parum Clothes Shop"}]},
	{[1101000, 100], [{name, "Parum Parts Shop"}]},
	{[1101000, 200], [{name, "Parum GRM"}]},
	{[1101000,9000], [{name, "Parum Raffon"}]},
	{[1101000,9010], [{name, "Parum Lakeshore"}]},
	{[1101000,9030], [{name, "Parum Waterfall"}]},
	{[1101000,9100], [{name, "Parum Denes"}]},
	{[1101000,9101], [{name, "Parum Underground"}]},
	{[1101000,9200], [{name, "Parum Beach"}]},
	{[1101000,9201], [{name, "Parum Rozenom"}]},
	{[1101000,9203], [{name, "Parum Subway"}]},
	{[1101000,9209], [{name, "Parum AMF"}]},

	% Neudaiz

	{[1102000,   1], [{name, "Neudaiz City"}]},
	{[1102000,   3], [{name, "Neudaiz GUARDIANS"}]},
	{[1102000, 100], [{name, "Neudaiz Synth Shop"}]},
	{[1102000, 100], [{name, "Neudaiz Clothes Shop"}]},
	{[1102000, 100], [{name, "Neudaiz Parts Shop"}]},
	{[1102000, 200], [{name, "Neudaiz Yohmei"}]},
	{[1102000,9000], [{name, "Neudaiz Islands"}]},
	{[1102000,9010], [{name, "Neudaiz Relics"}]},
	{[1102000,9100], [{name, "Neudaiz Mizuraki"}]},
	{[1102000,9120], [{name, "Neudaiz Hot Springs"}]},
	{[1102000,9300], [{name, "Neudaiz Temple"}]},
	{[1102000,9301], [{name, "Neudaiz Pavilion"}]},
	{[1102000,9302], [{name, "Neudaiz Habirao"}]},
	{[1102000,9305], [{name, "Neudaiz Saguraki"}]},

	% Moatoob

	{[1103000,   1], [{name, "Moatoob City"}]},
	{[1103000,   2], [{name, "Moatoob GUARDIANS"}]},
	{[1103000, 100], [{name, "Moatoob Parts Shop"}]},
	{[1103000, 100], [{name, "Moatoob Clothes Shop"}]},
	{[1103000, 100], [{name, "Moatoob Synth Shop"}]},
	{[1103000, 101], [{name, "Moatoob Pub"}]},
	{[1103000, 200], [{name, "Moatoob Tenora"}]},
	{[1103000,9010], [{name, "Moatoob Desert"}]},
	{[1103000,9030], [{name, "Moatoob Oasis"}]},
	{[1103000,9040], [{name, "Moatoob Glacier"}]},
	{[1103000,9101], [{name, "Moatoob Basin"}]},
	{[1103000,9202], [{name, "Moatoob Underground Lake"}]},
	{[1103000,9300], [{name, "Moatoob Casino"}]},
	{[1103000,9302], [{name, "Moatoob Il Cabo"}]},
	{[1103000,9304], [{name, "Moatoob Granigs"}]},

	% Spaceports

	{[1104000,900], [{name, "Spaceport"}]}
]).

%% EGS counters settings.

-define(COUNTERS, [
	% Colony mission counters
	{0, [{lobby, [1100000, 0]}, {filename, "data/missions/colony.counter.ll.pack"},    {options, << 16#01a92800:32, 3, 3, 0,             3, 3, 3, 3, 0:48, 3, 3, 3, 3, 3, 0:176 >>}]},
	{1, [{lobby, [1100000, 0]}, {filename, "data/missions/colony.counter.docks.pack"}, {options, << 16#01805400:32, 3, 3, 0, 0, 0, 0,    3, 3, 3, 3, 0:40, 3, 3, 3, 3, 0:40, 3, 0, 3, 0, 0:448 >>}]},
	% Lumilass
	{ 731394, [{lobby, [1100000,11]}, {data, none}] }
]).

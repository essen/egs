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

-define(MAPS, [
	% Current test mission

	{ [1000013, 0, 1121], [{type, mission}, {name, "Linear Line Test"}, {quest, "data/missions/test.quest.nbl"}, {zone, "data/missions/test.zone.nbl"},  {entries, []}] },

	% Colony

	{ [1100000, 0,   1], [{type, lobby}, {name, "Colony 1st Floor"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-0.nbl"},  {entries, [1,2,11,12,13,14,20,21]}] },
	{ [1100000, 0,   2], [{type, lobby}, {name, "Colony 2nd Floor"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,7,8,20,21,22,23]}] },
	{ [1100000, 0,   3], [{type, lobby}, {name, "Colony 3rd Floor"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-0.nbl"},  {entries, [0,1,2,3,4,5,20,21,22,23]}] },
	{ [1100000, 0,   4], [{type, lobby}, {name, "Colony 4th Floor"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,20,21,22,23]}] },
	{ [1100000,11,   5], [{type, lobby}, {name, "Colony GUARDIANS"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [0,1,2]}] },
	{ [1100000,11, 100], [{type, lobby}, {name, "Colony 2nd, Grind Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [2]}] },
	{ [1100000,12, 100], [{type, lobby}, {name, "Colony 2nd, Synth Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-12.nbl"}, {entries, [1]}] },
	{ [1100000,13, 100], [{type, lobby}, {name, "Colony 2nd, Decos Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-13.nbl"}, {entries, [0]}] },
	{ [1100000,11, 101], [{type, lobby}, {name, "Colony 2nd, Items Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [0]}] },
	{ [1100000,12, 101], [{type, lobby}, {name, "Colony 2nd, Weapons Shop"}, {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-12.nbl"}, {entries, [1]}] },
	{ [1100000,13, 101], [{type, lobby}, {name, "Colony 2nd, Armors Shop"},  {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-13.nbl"}, {entries, [2]}] },
	{ [1100000,11, 102], [{type, lobby}, {name, "Colony 3rd, Lumilass"},     {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [2,7,8]}] },
	{ [1100000,12, 102], [{type, lobby}, {name, "Colony 3rd, Clothes Shop"}, {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-12.nbl"}, {entries, [1]}] },
	{ [1100000,13, 102], [{type, lobby}, {name, "Colony 3rd, Parts Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-13.nbl"}, {entries, [0]}] },
	{ [1100000,11, 103], [{type, lobby}, {name, "Colony Club"},              {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [0,1]}] },
	{ [1100000,11, 110], [{type, lobby}, {name, "Colony R&D"},               {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [0,1]}] },
	{ [1100000, 2,9000], [{type, lobby}, {name, "Colony Aurorey"},           {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-2.nbl"},  {entries, [0]}] },
	{ [1100000, 1,9001], [{type, lobby}, {name, "Colony Transfer Terminal"}, {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-1.nbl"},  {entries, [0]}] }, % probably also 1 leaving next counter
	{ [1100000, 4,9010], [{type, lobby}, {name, "Colony Dallgun"},           {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-4.nbl"},  {entries, [0]}] }, % probably also 1 leaving next counter
	{ [1100000, 3,9102], [{type, lobby}, {name, "Colony HIVE"},              {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-3.nbl"},  {entries, [0,1]}] },
	{ [1100000, 7,9200], [{type, lobby}, {name, "Colony Rykros"},            {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-7.nbl"},  {entries, [0,1]}] },
	{ [1100000, 7,9202], [{type, lobby}, {name, "Colony Falz Memoria"},      {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-7.nbl"},  {entries, [0]}] },

	% Parum

	{ [1101000, 0,   1], [{type, lobby}, {name, "Parum City Central"}, {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-0.nbl"},  {entries, [0,1,2]}] },
	{ [1101000, 0,   2], [{type, lobby}, {name, "Parum City West"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,7,8]}] },
	{ [1101000, 0,   3], [{type, lobby}, {name, "Parum City East"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-0.nbl"},  {entries, [0,1,2]}] },
	{ [1101000,11,   4], [{type, lobby}, {name, "Parum GUARDIANS"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-11.nbl"}, {entries, [0,1,2]}] },
	{ [1101000,11, 100], [{type, lobby}, {name, "Parum Synth Shop"},   {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-11.nbl"}, {entries, [2]}] },
	{ [1101000,12, 100], [{type, lobby}, {name, "Parum Clothes Shop"}, {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-12.nbl"}, {entries, [1]}] },
	{ [1101000,13, 100], [{type, lobby}, {name, "Parum Parts Shop"},   {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-13.nbl"}, {entries, [0]}] },
	{ [1101000,11, 200], [{type, lobby}, {name, "Parum GRM"},          {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-11.nbl"}, {entries, [0]}] },
	{ [1101000, 1,9000], [{type, lobby}, {name, "Parum Raffon"},       {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-1.nbl"},  {entries, [0,1]}] },
	{ [1101000, 2,9010], [{type, lobby}, {name, "Parum Lakeshore"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-2.nbl"},  {entries, [0,1]}] },
	{ [1101000, 5,9030], [{type, lobby}, {name, "Parum Waterfall"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-5.nbl"},  {entries, [0]}] },
	{ [1101000, 4,9100], [{type, lobby}, {name, "Parum Denes"},        {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-4.nbl"},  {entries, [0,1]}] },
	{ [1101000, 3,9101], [{type, lobby}, {name, "Parum Underground"},  {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-3.nbl"},  {entries, [0,1,2]}] },
	{ [1101000, 7,9200], [{type, lobby}, {name, "Parum Beach"},        {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-7.nbl"},  {entries, [0,1]}] },
	{ [1101000, 7,9201], [{type, lobby}, {name, "Parum Rozenom"},      {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-7.nbl"},  {entries, [0]}] }, % probably also 1
	{ [1101000, 7,9203], [{type, lobby}, {name, "Parum Subway"},       {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-7.nbl"},  {entries, [0]}] }, % probably also 1
	{ [1101000, 7,9209], [{type, lobby}, {name, "Parum AMF"},          {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-7.nbl"},  {entries, [0,1]}] },

	% Neudaiz

	{ [1102000, 0,   1], [{type, lobby}, {name, "Neudaiz City"},         {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,7]}] },
	{ [1102000,11,   3], [{type, lobby}, {name, "Neudaiz GUARDIANS"},    {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-11.nbl"}, {entries, [0,1,2]}] },
	{ [1102000,11, 100], [{type, lobby}, {name, "Neudaiz Synth Shop"},   {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-11.nbl"}, {entries, [0]}] },
	{ [1102000,12, 100], [{type, lobby}, {name, "Neudaiz Clothes Shop"}, {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-12.nbl"}, {entries, [1]}] },
	{ [1102000,13, 100], [{type, lobby}, {name, "Neudaiz Parts Shop"},   {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-13.nbl"}, {entries, [2]}] },
	{ [1102000,11, 200], [{type, lobby}, {name, "Neudaiz Yohmei"},       {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-11.nbl"}, {entries, [0]}] },
	{ [1102000, 1,9000], [{type, lobby}, {name, "Neudaiz Islands"},      {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-1.nbl"},  {entries, [0,1,2]}] },
	{ [1102000, 2,9010], [{type, lobby}, {name, "Neudaiz Relics"},       {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-2.nbl"},  {entries, [0,1]}] },
	{ [1102000, 3,9100], [{type, lobby}, {name, "Neudaiz Mizuraki"},     {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-3.nbl"},  {entries, [0,1,2]}] },
	{ [1102000, 4,9120], [{type, lobby}, {name, "Neudaiz Hot Springs"},  {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-4.nbl"},  {entries, [0]}] }, % maybe also 1 clothes
	{ [1102000, 7,9300], [{type, lobby}, {name, "Neudaiz Temple"},       {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-7.nbl"},  {entries, [0]}] },
	{ [1102000, 7,9301], [{type, lobby}, {name, "Neudaiz Pavilion"},     {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-7.nbl"},  {entries, [0,2]}] }, % probably also unused 1
	{ [1102000, 7,9302], [{type, lobby}, {name, "Neudaiz Habirao"},      {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-7.nbl"},  {entries, [0,1]}] },
	{ [1102000, 7,9305], [{type, lobby}, {name, "Neudaiz Saguraki"},     {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-7.nbl"},  {entries, [0,1,2]}] },

	% Moatoob

	{ [1103000, 0,   1], [{type, lobby}, {name, "Moatoob City"},             {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,7]}] },
	{ [1103000,11,   2], [{type, lobby}, {name, "Moatoob GUARDIANS"},        {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-11.nbl"}, {entries, [0,1,2]}] },
	{ [1103000,11, 100], [{type, lobby}, {name, "Moatoob Parts Shop"},       {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-11.nbl"}, {entries, [0]}] },
	{ [1103000,12, 100], [{type, lobby}, {name, "Moatoob Clothes Shop"},     {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-12.nbl"}, {entries, [1]}] },
	{ [1103000,13, 100], [{type, lobby}, {name, "Moatoob Synth Shop"},       {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-13.nbl"}, {entries, [2]}] },
	{ [1103000,11, 101], [{type, lobby}, {name, "Moatoob Pub"},              {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-11.nbl"}, {entries, [0,1]}] },
	{ [1103000,11, 200], [{type, lobby}, {name, "Moatoob Tenora"},           {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-11.nbl"}, {entries, [0]}] },
	{ [1103000, 1,9010], [{type, lobby}, {name, "Moatoob Desert"},           {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-1.nbl"},  {entries, [0,1]}] },
	{ [1103000, 5,9030], [{type, lobby}, {name, "Moatoob Oasis"},            {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-5.nbl"},  {entries, [0]}] }, % maybe also 1 clothes
	{ [1103000, 6,9040], [{type, lobby}, {name, "Moatoob Glacier"},          {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-6.nbl"},  {entries, [0]}] }, % probably also 1
	{ [1103000, 3,9101], [{type, lobby}, {name, "Moatoob Basin"},            {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-3.nbl"},  {entries, [0,1]}] },
	{ [1103000, 4,9202], [{type, lobby}, {name, "Moatoob Underground Lake"}, {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-4.nbl"},  {entries, [0,1,2]}] }, % maybe also 3
	{ [1103000, 7,9300], [{type, lobby}, {name, "Moatoob Casino"},           {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-7.nbl"},  {entries, [0]}] },
	{ [1103000, 7,9302], [{type, lobby}, {name, "Moatoob Il Cabo"},          {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-7.nbl"},  {entries, [0]}] }, % probably also 1
	{ [1103000, 7,9304], [{type, lobby}, {name, "Moatoob Granigs"},          {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-7.nbl"},  {entries, [0,1,2]}] },

	% Spaceports

	{ [1104000,0,900], [{type, spaceport}, {name, "Spaceport"}, {quest, "data/lobby/spaceport.quest.nbl"}, {zone, "data/lobby/spaceport.zone.nbl"}, {entries, [0]}] }
]).

%% EGS counters settings.

-define(COUNTERS, [
	% Colony mission counters
	{0, [{lobby, [1100000, 0]}, {filename, "data/missions/colony.counter.ll.pack"},    {options, << 16#01a92800:32, 3, 3, 0,             3, 3, 3, 3, 0:48, 3, 3, 3, 3, 3, 0:176 >>}]},
	{1, [{lobby, [1100000, 0]}, {filename, "data/missions/colony.counter.docks.pack"}, {options, << 16#01805400:32, 3, 3, 0, 0, 0, 0,    3, 3, 3, 3, 0:40, 3, 3, 3, 3, 0:40, 3, 0, 3, 0, 0:448 >>}]},
	% Lumilass
	{ 731394, [{lobby, [1100000,11]}, {data, none}] }
]).

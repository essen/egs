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
	% Colony

	{ [1100000, 0,   1], [{name, "Colony 1st Floor"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-0.nbl"},  {entries, [1,2,11,12,13,14,20,21]}] },
	{ [1100000, 0,   2], [{name, "Colony 2nd Floor"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,7,8,20,21,22,23]}] },
	{ [1100000, 0,   3], [{name, "Colony 3rd Floor"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-0.nbl"},  {entries, [0,1,2,3,4,5,20,21,22,23]}] },
	{ [1100000, 0,   4], [{name, "Colony 4th Floor"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,20,21,22,23]}] },
	{ [1100000,11,   5], [{name, "Colony GUARDIANS"},         {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [0,1,2]}] },
	{ [1100000,11, 100], [{name, "Colony 2nd, Grind Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [2]}] },
	{ [1100000,12, 100], [{name, "Colony 2nd, Synth Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-12.nbl"}, {entries, [1]}] },
	{ [1100000,13, 100], [{name, "Colony 2nd, Decos Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-13.nbl"}, {entries, [0]}] },
	{ [1100000,11, 101], [{name, "Colony 2nd, Items Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [0]}] },
	{ [1100000,12, 101], [{name, "Colony 2nd, Weapons Shop"}, {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-12.nbl"}, {entries, [1]}] },
	{ [1100000,13, 101], [{name, "Colony 2nd, Armors Shop"},  {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-13.nbl"}, {entries, [2]}] },
	{ [1100000,11, 102], [{name, "Colony 3rd, Lumilass"},     {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [2]}] },
	{ [1100000,12, 102], [{name, "Colony 3rd, Clothes Shop"}, {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-12.nbl"}, {entries, [1]}] },
	{ [1100000,13, 102], [{name, "Colony 3rd, Parts Shop"},   {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-13.nbl"}, {entries, [0]}] },
	{ [1100000,11, 103], [{name, "Colony Club"},              {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [0,1]}] },
	{ [1100000,11, 110], [{name, "Colony R&D"},               {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-11.nbl"}, {entries, [0,1]}] },
	{ [1100000, 2,9000], [{name, "Colony Aurorey"},           {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-2.nbl"},  {entries, [0]}] },
	{ [1100000, 1,9001], [{name, "Colony Transfer Terminal"}, {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-1.nbl"},  {entries, [0]}] }, % probably also 1 leaving next counter
	{ [1100000, 4,9010], [{name, "Colony Dallgun"},           {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-4.nbl"},  {entries, [0]}] }, % probably also 1 leaving next counter
	{ [1100000, 3,9102], [{name, "Colony HIVE"},              {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-3.nbl"},  {entries, [0,1]}] },
	{ [1100000, 7,9200], [{name, "Colony Rykros"},            {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-7.nbl"},  {entries, [0,1]}] },
	{ [1100000, 7,9202], [{name, "Colony Falz Memoria"},      {quest, "data/lobby/colony.quest.nbl"}, {zone, "data/lobby/colony.zone-7.nbl"},  {entries, [0]}] },

	% Parum

	{ [1101000, 0,   1], [{name, "Parum City Central"}, {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-0.nbl"},  {entries, [0,1,2]}] },
	{ [1101000, 0,   2], [{name, "Parum City West"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,7,8]}] },
	{ [1101000, 0,   3], [{name, "Parum City East"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-0.nbl"},  {entries, [0,1,2]}] },
	{ [1101000,11,   4], [{name, "Parum GUARDIANS"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-11.nbl"}, {entries, [0,1,2]}] },
	{ [1101000,11, 100], [{name, "Parum Synth Shop"},   {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-11.nbl"}, {entries, [2]}] },
	{ [1101000,12, 100], [{name, "Parum Clothes Shop"}, {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-12.nbl"}, {entries, [1]}] },
	{ [1101000,13, 100], [{name, "Parum Parts Shop"},   {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-13.nbl"}, {entries, [0]}] },
	{ [1101000,11, 200], [{name, "Parum GRM"},          {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-11.nbl"}, {entries, [0]}] },
	{ [1101000, 1,9000], [{name, "Parum Raffon"},       {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-1.nbl"},  {entries, [0,1]}] },
	{ [1101000, 2,9010], [{name, "Parum Lakeshore"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-2.nbl"},  {entries, [0,1]}] },
	{ [1101000, 5,9030], [{name, "Parum Waterfall"},    {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-5.nbl"},  {entries, [0]}] },
	{ [1101000, 4,9100], [{name, "Parum Denes"},        {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-4.nbl"},  {entries, [0,1]}] },
	{ [1101000, 3,9101], [{name, "Parum Underground"},  {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-3.nbl"},  {entries, [0,1,2]}] },
	{ [1101000, 7,9200], [{name, "Parum Beach"},        {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-7.nbl"},  {entries, [0,1]}] },
	{ [1101000, 7,9201], [{name, "Parum Rozenom"},      {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-7.nbl"},  {entries, [0]}] }, % probably also 1
	{ [1101000, 7,9203], [{name, "Parum Subway"},       {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-7.nbl"},  {entries, [0]}] }, % probably also 1
	{ [1101000, 7,9209], [{name, "Parum AMF"},          {quest, "data/lobby/parum.quest.nbl"}, {zone, "data/lobby/parum.zone-7.nbl"},  {entries, [0,1]}] },

	% Neudaiz

	{ [1102000, 0,   1], [{name, "Neudaiz City"},         {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,7]}] },
	{ [1102000,11,   3], [{name, "Neudaiz GUARDIANS"},    {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-11.nbl"}, {entries, [0,1,2]}] },
	{ [1102000,11, 100], [{name, "Neudaiz Synth Shop"},   {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-11.nbl"}, {entries, [0]}] },
	{ [1102000,12, 100], [{name, "Neudaiz Clothes Shop"}, {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-12.nbl"}, {entries, [1]}] },
	{ [1102000,13, 100], [{name, "Neudaiz Parts Shop"},   {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-13.nbl"}, {entries, [2]}] },
	{ [1102000,11, 200], [{name, "Neudaiz Yohmei"},       {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-11.nbl"}, {entries, [0]}] },
	{ [1102000, 1,9000], [{name, "Neudaiz Islands"},      {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-1.nbl"},  {entries, [0,1,2]}] },
	{ [1102000, 2,9010], [{name, "Neudaiz Relics"},       {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-2.nbl"},  {entries, [0,1]}] },
	{ [1102000, 3,9100], [{name, "Neudaiz Mizuraki"},     {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-3.nbl"},  {entries, [0,1,2]}] },
	{ [1102000, 4,9120], [{name, "Neudaiz Hot Springs"},  {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-4.nbl"},  {entries, [0]}] }, % maybe also 1 clothes
	{ [1102000, 7,9300], [{name, "Neudaiz Temple"},       {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-7.nbl"},  {entries, [0]}] },
	{ [1102000, 7,9301], [{name, "Neudaiz Pavilion"},     {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-7.nbl"},  {entries, [0,2]}] }, % probably also unused 1
	{ [1102000, 7,9302], [{name, "Neudaiz Habirao"},      {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-7.nbl"},  {entries, [0,1]}] },
	{ [1102000, 7,9305], [{name, "Neudaiz Saguraki"},     {quest, "data/lobby/neudaiz.quest.nbl"}, {zone, "data/lobby/neudaiz.zone-7.nbl"},  {entries, [0,1,2]}] },

	% Moatoob

	{ [1103000, 0,   1], [{name, "Moatoob City"},             {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-0.nbl"},  {entries, [0,1,2,3,4,5,6,7]}] },
	{ [1103000,11,   2], [{name, "Moatoob GUARDIANS"},        {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-11.nbl"}, {entries, [0,1,2]}] },
	{ [1103000,11, 100], [{name, "Moatoob Parts Shop"},       {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-11.nbl"}, {entries, [0]}] },
	{ [1103000,12, 100], [{name, "Moatoob Clothes Shop"},     {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-12.nbl"}, {entries, [1]}] },
	{ [1103000,13, 100], [{name, "Moatoob Synth Shop"},       {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-13.nbl"}, {entries, [2]}] },
	{ [1103000,11, 101], [{name, "Moatoob Pub"},              {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-11.nbl"}, {entries, [0,1]}] },
	{ [1103000,11, 200], [{name, "Moatoob Tenora"},           {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-11.nbl"}, {entries, [0]}] },
	{ [1103000, 1,9010], [{name, "Moatoob Desert"},           {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-1.nbl"},  {entries, [0,1]}] },
	{ [1103000, 5,9030], [{name, "Moatoob Oasis"},            {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-5.nbl"},  {entries, [0]}] }, % maybe also 1 clothes
	{ [1103000, 6,9040], [{name, "Moatoob Glacier"},          {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-6.nbl"},  {entries, [0]}] }, % probably also 1
	{ [1103000, 3,9101], [{name, "Moatoob Basin"},            {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-3.nbl"},  {entries, [0,1]}] },
	{ [1103000, 4,9202], [{name, "Moatoob Underground Lake"}, {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-4.nbl"},  {entries, [0,1,2]}] }, % maybe also 3
	{ [1103000, 7,9300], [{name, "Moatoob Casino"},           {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-7.nbl"},  {entries, [0]}] },
	{ [1103000, 7,9302], [{name, "Moatoob Il Cabo"},          {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-7.nbl"},  {entries, [0]}] }, % probably also 1
	{ [1103000, 7,9304], [{name, "Moatoob Granigs"},          {quest, "data/lobby/moatoob.quest.nbl"}, {zone, "data/lobby/moatoob.zone-7.nbl"},  {entries, [0,1,2]}] },

	% Spaceports

	{ [1104000,0,900], [{name, "Spaceport"}, {quest, "data/lobby/spaceport.quest.nbl"}, {zone, "data/lobby/spaceport.zone.nbl"}, {entries, [0]}] }
]).

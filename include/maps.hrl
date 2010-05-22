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
	{    1, [{name, "GC Floor 1"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/zone.gc1.nbl"}] },
	{    2, [{name, "GC Floor 2"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/zone.gc1.nbl"}] },
	{    3, [{name, "GC Floor 3"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/zone.gc1.nbl"}] },
	{    4, [{name, "GC Floor 4"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/zone.gc1.nbl"}] },
%	{    5, [{name, "GC Floor 5"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/TODO"}] },
%	{  100, [{name, "GC Floor 2 Shops R"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/TODO"}] }, % entries 0 1 2
%	{  101, [{name, "GC Floor 2 Shops L"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/TODO"}] }, % entries 0 1 2
%	{  102, [{name, "GC Floor 3 Shops R"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/TODO"}] }, % entries 0 1 2
	{  103, [{name, "GC Club Commune"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/zone.club.nbl"}] } % entries 0 1
%	{  900, [{name, "GC Space Port"}, {quest, "p/quest.gc1.nbl"}, {zone, "p/TODO"}] },
%	{ 16#a701, [{quest, "p/quest.myroom.nbl"}, {zone, "p/zone.myroom.nbl"}] } % TODO: 6400 too? on US through a door?
]).

% TODO:
% 100 101 102 103 104 105 106 107 108 109 111 112 113 117 119 120 121 122 123 124 130 131 140
% 200 201 202 203 204 205 206 208 210 211 212 213 217 219 220 221 222 223 224 230 231
% 300 301 302 303 304 305 306 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322 323 324 325
% 400 401 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417 418 419 420 421 422 423 424 425
% 800 801 802
% 900
% 1002 1104 1105 1120 1121 1200 1301
% 2100 2200 2202
% 3002 3100 3101 3110 3112 3120 3132 3160 3161 3300 3301 3400 3801 3803
% 4100? 4221 4250 4300 4301 4802 4803 4804
% 5000 5101 5112 5113 5114 5200 5201 5202 5203 5204 5300 5301 5302 5303 5304 5402 5403 5801 5802 5803 5804 5805 5806 5807 5808
% 6000 6301 6302 6303 6304 6803
% 7000
% 8000 8002
% 9000 9001 9010 9011 9012 9020 9030 9040 9100 9101 9102 9103 9120 9130 9200 9201 9202 9203 9207 9208 9209 9300 9301 9302 9304 9305 9306 9307 9308
% and more...

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
%	gasetools is distributed in the hope that it will be useful,
%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%	GNU General Public License for more details.
%
%	You should have received a copy of the GNU General Public License
%	along with gasetools.  If not, see <http://www.gnu.org/licenses/>.

%% EGS maps settings.

-define(MAPS, [
	{ 16#0100, [{quest, "p/quest.gc1.nbl"}, {zone, "p/zone.gc1.nbl"}] },
	{ 16#0200, [{quest, "p/quest.gc1.nbl"}, {zone, "p/zone.gc1.nbl"}] },
	{ 16#0300, [{quest, "p/quest.gc1.nbl"}, {zone, "p/zone.gc1.nbl"}] },
	{ 16#0400, [{quest, "p/quest.gc1.nbl"}, {zone, "p/zone.gc1.nbl"}] },
% TODO: 0500 (0000): 5th floor
% TODO: 6400 (0000 0100 0200): clyez 2nd floor shops, right
% TODO: 6500 (0000 0100 0200): clyez 2nd floor shops, left
% TODO: 6600 (0000 0100 0200): clyez 3rd floor shops, right
% TODO: 6700 (0000): club commune
% TODO: 8403 (0000): parum, neudaiz, moatoob spaceports
	{ 16#a701, [{quest, "p/quest.myroom.nbl"}, {zone, "p/zone.myroom.nbl"}] } % TODO: 6400 too? on US through a door?
]).

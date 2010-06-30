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

%% EGS database schema.

-record(ids, {type, id}).
-record(users, {gid, pid, socket, auth, time, folder, charnumber, charname, lid, instanceid, areatype, questid, zoneid, mapid, entryid, savedquestid, savedzoneid, savedmapid, savedentryid, direction, coords}).

-record(flesh_appearance, {voicetype, voicepitch, jacket, pants, shoes, ears, face, hairstyle, jacketcolor, pantscolor, shoescolor,
	lineshieldcolor, badge, eyebrows, eyelashes, eyesgroup, eyes, bodysuit, eyescolory, eyescolorx, lipsintensity, lipscolory, lipscolorx,
	skincolor, hairstylecolory, hairstylecolorx, proportion, proportionboxx, proportionboxy, faceboxx, faceboxy}).
-record(metal_appearance, {voicetype, voicepitch, torso, legs, arms, ears, face, headtype, maincolor, lineshieldcolor,
	eyebrows, eyelashes, eyesgroup, eyes, eyescolory, eyescolorx, bodycolor, subcolor, hairstylecolory, hairstylecolorx,
	proportion, proportionboxx, proportionboxy, faceboxx, faceboxy}).

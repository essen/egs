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

%% EGS network settings.

-define(PATCH_PORT_JP, 11030).
-define(PATCH_PORT_US, 11230).
-define(PATCH_LISTEN_OPTIONS, [binary, {send_timeout, 5000}, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(LOGIN_PORT_JP_ONE, 12030).
-define(LOGIN_PORT_JP_TWO, 12031).
-define(LOGIN_PORT_US, 12230).
-define(LOGIN_LISTEN_OPTIONS, [binary, {active, false}, {certfile, "ssl/servercert.pem"}, {keyfile, "ssl/serverkey.pem"}, {password, "alpha"}]).

-define(GAME_IP, << 188, 165, 56, 119 >>).
-define(GAME_PORT, 12061).
-define(GAME_LISTEN_OPTIONS, [binary, {active, false}, {certfile, "ssl/servercert.pem"}, {keyfile, "ssl/serverkey.pem"}, {password, "alpha"}]).

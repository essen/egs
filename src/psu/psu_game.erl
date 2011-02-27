%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Handle game clients.
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

-module(psu_game).
-compile(export_all). %% @todo Temporarily export all until send_xxxx functions are moved to psu_proto.

-include("include/records.hrl").

%% @doc Load and send the character information to the client.
%% @todo Move this whole function directly to psu_proto, probably.
char_load(User, Client) ->
	psu_proto:send_0d01(User#users.character, Client),
	%% 0246
	psu_proto:send_0a0a((User#users.character)#characters.inventory, Client),
	psu_proto:send_1006(5, 0, Client), %% @todo The 0 here is PartyPos, save it in User.
	psu_proto:send_1005(User#users.character, Client),
	psu_proto:send_1006(12, Client),
	psu_proto:send_0210(Client),
	psu_proto:send_0222(User#users.uni, Client),
	psu_proto:send_1500(User#users.character, Client),
	psu_proto:send_1501(Client),
	psu_proto:send_1512(Client),
	%% 0303
	psu_proto:send_1602(Client),
	psu_proto:send_021b(Client).

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2011 Loïc Hoguin.
%% @doc Project-wide Erlang types.
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

%% Standard library types.

-opaque sslsocket() :: any().

%% EGS types.

-type gid() :: 0..16#ffffffff.
-type lid() :: 0..1023 | 16#ffff.

-type questid()	:: 0..16#ffffffff. %% @todo What's the real max?
-type zoneid()	:: 0..16#ffff. %% @todo What's the real max?
-type mapid()	:: 0..9999.
-type entryid()	:: 0..16#ffff. %% @todo What's the real max?

-type area() :: {questid(), zoneid(), mapid()}.
-type position() :: {X :: float(), Y :: float(), Z :: float(), Dir :: float()}.

-type character_slot() :: 0..3.

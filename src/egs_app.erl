%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc Callbacks for the egs application.
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

-module(egs_app).
-behaviour(application).
-export([start/2, stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for egs.
start(_Type, _StartArgs) ->
	egs_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for egs.
stop(_State) ->
	ok.

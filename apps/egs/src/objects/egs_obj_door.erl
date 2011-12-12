%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2011 Loïc Hoguin.
%% @doc Door object.
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

-module(egs_obj_door).
-export([init/3]).

%% @todo Real values.
-is_indexed(true).
-is_target(false).

-record(egs_obj_door, {
	pos :: {X :: float(), Y :: float(), Z :: float()},
	rot :: {Rx :: float(), Ry :: float(), Zy :: float()}
}).

init(Pos, Rot, Params) ->
	#egs_obj_door{pos=Pos, rot=Rot}.

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
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

-include("include/records.hrl").

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for egs.
start(_Type, _StartArgs) ->
	case is_fresh_startup() of
		true ->
			db_init();
		{exists, Tables} ->
			ok = mnesia:wait_for_tables(Tables, 20000)
	end,
	egs_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for egs.
stop(_State) ->
	ok.

%% @spec is_fresh_startup() -> true | false
%% @doc Returns true if mnesia has not been initialized with the egs schema.
%% Thanks to Dale Harvey for this function posted to the erlang questions mailing list.
is_fresh_startup() ->
	Node = node(),
	case mnesia:system_info(tables) of
		[schema] -> true;
		Tables ->
			case mnesia:table_info(schema, cookie) of
				{_, Node} -> {exists, Tables};
				_ -> true
			end
	end.

%% @spec db_init() -> ok
%% @doc Initialize the database.
db_init() ->
	Nodes = [node()],
	case mnesia:system_info(is_running) of
		yes ->
			error_logger:info_report("stopping mnesia"),
			mnesia:stop();
		_ -> pass
	end,
	mnesia:create_schema(Nodes),
	error_logger:info_report("mnesia schema created"),
	error_logger:info_report("starting mnesia"),
	mnesia:start(),
	mnesia:create_table(users, [{attributes, record_info(fields, users)}]),
	error_logger:info_report("mnesia tables created"),
	ok.

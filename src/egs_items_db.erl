%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc EGS items database.
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

-module(egs_items_db).
-behavior(gen_server).

-export([start_link/0, stop/0, desc/1, read/1, reload/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

-include("include/types.hrl").
-include("include/records.hrl").
-include("priv/items.hrl").

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec desc(ItemID) -> string()
desc(ItemID) ->
	gen_server:call(?SERVER, {desc, ItemID}).

%% @spec read(ItemID) -> term() | undefined
read(ItemID) ->
	gen_server:call(?SERVER, {read, ItemID}).

%% @spec reload() -> ok
reload() ->
	gen_server:cast(?SERVER, reload).

%% gen_server.

init([]) ->
	error_logger:info_report("egs_items_db started"),
	{ok, undefined}.

handle_call({desc, ItemID}, _From, State) ->
	Filename = io_lib:format("priv/item_descs/~8.16.0b.txt", [ItemID]),
	Desc = case filelib:is_regular(Filename) of
		false -> << << X:8, 0:8 >> || X <- "Always bet on Dammy." >>;
		true -> {ok, File} = file:read_file(Filename), File
	end,
	{reply, Desc, State};

handle_call({read, ItemID}, _From, State) ->
	{reply, proplists:get_value(ItemID, ?ITEMS), State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%% @doc Compile this file here and let the reloader reload the code properly.
handle_cast(reload, State) ->
	compile:file(?FILE, [verbose, report_errors, report_warnings, {outdir, "ebin/"}]),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

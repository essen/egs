%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc EGS shops database.
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

-module(egs_shops_db).
-behavior(gen_server).

-export([start_link/0, stop/0, nth/2, read/1, reload/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

-include("include/types.hrl").

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec nth(ShopID, N) -> ItemID | undefined
nth(ShopID, N) ->
	gen_server:call(?SERVER, {nth, ShopID, N}).

%% @spec read(ShopID) -> list() | undefined
read(ShopID) ->
	gen_server:call(?SERVER, {read, ShopID}).

%% @spec reload() -> ok
reload() ->
	gen_server:cast(?SERVER, reload).

%% gen_server.

init([]) ->
	case file:consult("priv/shops.conf") of
		{ok, Terms} ->
			error_logger:info_report("egs_shops_db started"),
			{ok, Terms};
		Error ->
			error_logger:error_report(["An error occurred when trying to load the shops configuration file:", Error]),
			Error
	end.

handle_call({nth, ShopID, N}, _From, State) ->
	ListingID = proplists:get_value({shop, ShopID}, State),
	List = proplists:get_value({listing, ListingID}, State),
	{reply, lists:nth(N, List), State};

handle_call({read, ShopID}, _From, State) ->
	ListingID = proplists:get_value({shop, ShopID}, State),
	{reply, proplists:get_value({listing, ListingID}, State), State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(reload, _State) ->
	{ok, Terms} = file:consult("priv/shops.conf"),
	{noreply, Terms};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc EGS counters database and cache manager.
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

-module(egs_counters_db).
-behavior(gen_server).

-export([start_link/0, stop/0, bg/1, opts/1, pack/1, reload/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec bg(CounterID) -> integer()
bg(CounterID) ->
	gen_server:call(?SERVER, {bg, CounterID}).

%% @spec opts(CounterID) -> binary()
opts(CounterID) ->
	gen_server:call(?SERVER, {opts, CounterID}).

%% @spec pack(CounterID) -> binary()
pack(CounterID) ->
	gen_server:call(?SERVER, {pack, CounterID}).

%% @spec reload() -> ok
reload() ->
	gen_server:cast(?SERVER, reload).

%% gen_server.

init([]) ->
	{ok, []}.

%% @doc Possible keys: bg, opts, pack.
handle_call({Key, CounterID}, _From, State) ->
	{Counter, State2} = get_counter(CounterID, State),
	{reply, proplists:get_value(Key, Counter), State2};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(reload, _State) ->
	{noreply, []};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

%% @doc Return a counter information either from the cache or from the configuration file,
%% in which case it gets added to the cache for subsequent attempts.
get_counter(CounterID, Cache) ->
	case proplists:get_value(CounterID, Cache) of
		undefined ->
			Dir = io_lib:format("priv/counters/~b/", [CounterID]),
			ConfFilename = Dir ++ "counter.conf",
			{TableRelData, TableRelPtrs} = egs_files:load_table_rel(ConfFilename),
			TextBinData = egs_files:load_text_bin(Dir ++ "text.bin.en_US.txt"),
			CounterNbl = egs_files:nbl_pack([{files, [
				{data, "table.rel", TableRelData, TableRelPtrs},
				{data, "text.bin", TextBinData, []}
			]}]),
			Counter = egs_files:load_counter_pack(ConfFilename, CounterNbl),
			Cache2 = [{CounterID, Counter}|Cache],
			{Counter, Cache2};
		Counter ->
			{Counter, Cache}
	end.

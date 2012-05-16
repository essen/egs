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

-module(egs_store).
-behaviour(gen_server).

-export([start_link/0,
	load_character/2, load_characters/2, save_character/3]). %% API.
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, terminate/2, code_change/3]). %% gen_server.

-define(SERVER, ?MODULE).
-define(ACCOUNTS_TBL, accounts_tbl).
-define(ACCOUNTS_VSN, 1).
-define(CHARACTERS_TBL, characters_tbl).
-define(CHARACTERS_VSN, 1).

%% API.

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

load_character(GID, Slot) ->
	gen_server:call(?SERVER, {load_character, GID, Slot}).

load_characters(GID, Slots) ->
	gen_server:call(?SERVER, {load_characters, GID, Slots}).

save_character(GID, Slot, Data) ->
	gen_server:call(?SERVER, {save_character, GID, Slot, Data}).

%% gen_server.

init([]) ->
	{ok, App} = application:get_application(),
	PrivDir = code:priv_dir(App),
	AccountsFile = PrivDir ++ "/accounts.tbl",
	CharactersFile = PrivDir ++ "/characters.tbl",
	{ok, ?ACCOUNTS_TBL} = dets:open_file(?ACCOUNTS_TBL,
		[{file, AccountsFile}]),
	io:format("accounts tbl:~n~p~n~n", [dets:info(?ACCOUNTS_TBL)]),
	{ok, ?CHARACTERS_TBL} = dets:open_file(?CHARACTERS_TBL,
		[{file, CharactersFile}]),
	io:format("characters tbl:~n~p~n~n", [dets:info(?CHARACTERS_TBL)]),
	{ok, undefined}.

handle_call({load_character, GID, Slot}, _From, State) ->
	case dets:lookup(?CHARACTERS_TBL, {GID, Slot}) of
		[{{GID, Slot}, Version, Data}] ->
			{reply, {ok, Version, Data}, State};
		[] ->
			{reply, {error, notfound}, State}
	end;
handle_call({load_characters, GID, Slots}, _From, State) ->
	Characters = lists:map(fun(Slot) ->
		case dets:lookup(?CHARACTERS_TBL, {GID, Slot}) of
			[{{GID, Slot}, Version, Data}] ->
				{Version, Data};
			[] ->
				notfound
		end
	end, Slots),
	{reply, {ok, Characters}, State};
handle_call({save_character, GID, Slot, Data}, _From, State) ->
	ok = dets:insert(?CHARACTERS_TBL, {{GID, Slot}, ?CHARACTERS_VSN, Data}),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Login server module.
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

-module(egs_login_server).
-export([start_link/1, on_exit/1, init/1]).

-include("include/records.hrl").

%% @spec start_link(Port) -> {ok,Pid::pid()}
%% @doc Start the login server.
start_link(Port) ->
	Pid = spawn(egs_network, listen, [Port, ?MODULE]),
	{ok, Pid}.

%% @spec on_exit(Pid) -> ok
%% @doc Nothing to do for the login server.
on_exit(_Pid) ->
	ok.

%% @doc Initialize the game state and start receiving messages.
init(Socket) ->
	TmpGID = 16#ff000000 + mnesia:dirty_update_counter(counters, tmpgid, 1),
	State = #state{socket=Socket, gid=TmpGID},
	psu_proto:send_0202(State),
	egs_network:recv(<< >>, egs_login, State).

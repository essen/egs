%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc Cowboy protocol module for the login server.
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

-module(egs_login_protocol).
-export([start_link/4, init/2]).

-include("include/records.hrl").

-spec start_link(pid(), ssl:sslsocket(), module(), []) -> {ok, pid()}.
start_link(_ListenerPid, Socket, Transport, []) ->
	Pid = spawn_link(?MODULE, init, [Socket, Transport]),
	{ok, Pid}.

-spec init(ssl:sslsocket(), module()) -> ok | closed.
init(Socket, Transport) ->
	Client = #client{socket=Socket, transport=Transport,
		gid=egs_accounts:tmp_gid()},
	egs_proto:send_0202(Client),
	egs_network:recv(<<>>, egs_login, Client).

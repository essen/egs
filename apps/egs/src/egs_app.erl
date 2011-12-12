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
-export([start/2, stop/1]). %% API.

-type application_start_type()
	:: normal | {takeover, node()} | {failover, node()}.

-define(SSL_OPTIONS, [{certfile, "priv/ssl/servercert.pem"},
	{keyfile, "priv/ssl/serverkey.pem"}, {password, "alpha"}]).

%% API.

-spec start(application_start_type(), term()) -> {ok, pid()}.
start(_Type, _StartArgs) ->
	{ok, Pid} = egs_sup:start_link(),
	start_patch_listeners(egs_conf:read(patch_ports)),
	start_login_listeners(egs_conf:read(login_ports)),
	{_ServerIP, GamePort} = egs_conf:read(game_server),
	{ok, _GamePid} = cowboy:start_listener({game, GamePort}, 10,
		cowboy_ssl_transport, [{port, GamePort}] ++ ?SSL_OPTIONS,
		egs_game_protocol, []),
	{ok, Pid}.

-spec stop(term()) -> ok.
stop(_State) ->
	ok.

%% Internal.

-spec start_patch_listeners([inet:ip_port()]) -> ok.
start_patch_listeners([]) ->
	ok;
start_patch_listeners([Port|Tail]) ->
	{ok, _Pid} = cowboy:start_listener({patch, Port}, 10,
		cowboy_tcp_transport, [{port, Port}],
		egs_patch_protocol, []),
	start_patch_listeners(Tail).

-spec start_login_listeners([inet:ip_port()]) -> ok.
start_login_listeners([]) ->
	ok;
start_login_listeners([Port|Tail]) ->
	{ok, _Pid} = cowboy:start_listener({login, Port}, 10,
		cowboy_ssl_transport, [{port, Port}] ++ ?SSL_OPTIONS,
		egs_login_protocol, []),
	start_login_listeners(Tail).

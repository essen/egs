%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2011 Loïc Hoguin.
%% @doc Callbacks for the egs_patch application.
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

-module(egs_patch_app).
-behaviour(application).
-export([start/2, stop/1]). %% API.

-type application_start_type()
	:: normal | {takeover, node()} | {failover, node()}.

%% API.

-spec start(application_start_type(), term()) -> {ok, pid()}.
start(_Type, _StartArgs) ->
	{ok, PatchPorts} = application:get_env(patch_ports),
	start_listeners(PatchPorts),
	egs_patch_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
	ok.

%% Internal.

-spec start_listeners([inet:ip_port()]) -> ok.
start_listeners([]) ->
	ok;
start_listeners([Port|Tail]) ->
	{ok, _Pid} = cowboy:start_listener({patch, Port}, 10,
		cowboy_tcp_transport, [{port, Port}],
		egs_patch_protocol, []),
	start_listeners(Tail).

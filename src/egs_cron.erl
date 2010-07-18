%	EGS: Erlang Game Server
%	Copyright (C) 2010  Loic Hoguin
%
%	This file is part of EGS.
%
%	EGS is free software: you can redistribute it and/or modify
%	it under the terms of the GNU General Public License as published by
%	the Free Software Foundation, either version 3 of the License, or
%	(at your option) any later version.
%
%	EGS is distributed in the hope that it will be useful,
%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%	GNU General Public License for more details.
%
%	You should have received a copy of the GNU General Public License
%	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

-module(egs_cron).
-export([start/0]). % external
-export([cleanup/0]). % internal

-include("include/records.hrl").

%% @doc Start the cron processes.

start() ->
	Pid = spawn_link(?MODULE, cleanup, []),
	{ok, Pid}.

%% @doc Cleanup the users table of failures to log into the game server.

cleanup() ->
	receive
		_ ->
			?MODULE:cleanup()
	after 300000 ->
		egs_db:users_cleanup(),
		reload,
		?MODULE:cleanup()
	end.

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc General purpose module for monitoring exit signals of linked processes.
%%
%%	This file is part of EGS.
%%
%%	EGS is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU General Public License as published by
%%	the Free Software Foundation, either version 3 of the License, or
%%	(at your option) any later version.
%%
%%	EGS is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU General Public License for more details.
%%
%%	You should have received a copy of the GNU General Public License
%%	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

-module(egs_exit_mon).
-export([start_link/1]). %% External.
-export([start/1, loop/1]). %% Internal.

%% @spec start_link(CleanupFn) -> {ok,Pid::pid()}
%% @doc Start the monitor and return the process' Pid.
start_link(CallbackFn) ->
	Pid = spawn(?MODULE, start, [CallbackFn]),
	{ok, Pid}.

%% @spec start(CallbackFn) -> ok
%% @doc Start the main loop.
start(CallbackFn) ->
	error_logger:info_report(io_lib:format("egs_exit_mon started with callback ~p", [CallbackFn])),
	process_flag(trap_exit, true),
	?MODULE:loop(CallbackFn).

%% @spec loop(CallbackFn) -> ok
%% @doc Main loop, trap exit messages and call the callback function.
loop(CallbackFn = {Module, Function}) ->
	receive
		{'EXIT', Pid, _} ->
			spawn(Module, Function, [Pid]);
		_ ->
			reload
	after 5000 ->
		reload
	end,
	?MODULE:loop(CallbackFn).

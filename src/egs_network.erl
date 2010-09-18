%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Login and game servers low-level network handling.
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

-module(egs_network).
-export([listen/2, recv/3]). %% API.
-export([accept/2]). %% Internal.

-define(OPTIONS, [binary, {active, true}, {reuseaddr, true}, {certfile, "priv/ssl/servercert.pem"}, {keyfile, "priv/ssl/serverkey.pem"}, {password, "alpha"}]).

%% @doc Listen for connections.
listen(Port, CallbackMod) ->
	error_logger:info_report(io_lib:format("listener started for ~p on port ~b", [CallbackMod, Port])),
	{ok, LSocket} = ssl:listen(Port, ?OPTIONS),
	?MODULE:accept(LSocket, CallbackMod).

%% @doc Accept connections.
accept(LSocket, CallbackMod) ->
	case ssl:transport_accept(LSocket, 5000) of
		{ok, CSocket} ->
			case ssl:ssl_accept(CSocket, 5000) of
				ok ->
					Pid = spawn(CallbackMod, init, [CSocket]),
					ssl:controlling_process(CSocket, Pid);
				{error, _Reason} ->
					ignore
			end;
		{error, _Reason} ->
			ignore
	end,
	?MODULE:accept(LSocket, CallbackMod).

%% @doc Main loop for the network stack. Receive and handle messages.
recv(SoFar, CallbackMod, State) ->
	receive
		{ssl, _Any, Data} ->
			{Commands, Rest} = split(<< SoFar/bits, Data/bits >>, []),
			case dispatch(Commands, CallbackMod, CallbackMod, State) of
				{ok, NextCallbackMod, NewState} ->
					?MODULE:recv(Rest, NextCallbackMod, NewState);
				closed -> closed
			end;
		{ssl_closed, _} ->
			ssl_closed; %% exit
		{ssl_error, _, _} ->
			ssl_error; %% exit
		{egs, keepalive} ->
			CallbackMod:keepalive(State),
			?MODULE:recv(SoFar, CallbackMod, State);
		Tuple when element(1, Tuple) =:= egs ->
			case CallbackMod:info(Tuple, State) of
				{ok, NewState} -> ?MODULE:recv(SoFar, CallbackMod, NewState);
				_Any -> ?MODULE:recv(SoFar, CallbackMod, State)
			end;
		_ ->
			?MODULE:recv(SoFar, CallbackMod, State)
	end.

%% @doc Dispatch the commands received to the right handler.
dispatch([], _CallbackMod, NextMod, State) ->
	{ok, NextMod, State};
dispatch([Data|Tail], CallbackMod, NextMod, State) ->
	Ret = case psu_proto:parse(Data) of
		{command, Command, Channel} ->
			case Channel of
				1 -> CallbackMod:cast(Command, Data, State);
				_ -> CallbackMod:raw(Command, Data, State)
			end;
		ignore ->
			ignore;
		Event ->
			CallbackMod:event(Event, State)
	end,
	case Ret of
		{ok, NewMod, NewState} ->
			dispatch(Tail, CallbackMod, NewMod, NewState);
		{ok, NewState} ->
			dispatch(Tail, CallbackMod, NextMod, NewState);
		closed ->
			closed;
		_Any ->
			dispatch(Tail, CallbackMod, NextMod, State)
	end.

%% @doc Split the network data received into commands.
split(Data, Acc) when byte_size(Data) < 4 ->
	{lists:reverse(Acc), Data};
split(<< Size:32/little, _/bits >> = Data, Acc) when Size > byte_size(Data) ->
	{lists:reverse(Acc), Data};
split(<< Size:32/little, _/bits >> = Data, Acc) ->
	BitSize = Size * 8,
	<< Split:BitSize/bits, Rest/bits >> = Data,
	split(Rest, [Split|Acc]).

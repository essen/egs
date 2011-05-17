%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
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
-export([recv/3]). %% API.

-include("include/types.hrl").
-include("include/records.hrl").

%% API.

%% @doc Main loop for the network stack. Receive and handle messages.
recv(SoFar, CallbackMod, Client=#client{socket=Socket, transport=Transport}) ->
	Transport:setopts(Socket, [{active, once}]),
	{OK, Closed, Error} = Transport:messages(),
	receive
		{OK, _Any, Data} ->
			{Commands, Rest} = split(<< SoFar/bits, Data/bits >>, []),
			case dispatch(Commands, CallbackMod, CallbackMod, Client) of
				{ok, NextCallbackMod, NewClient} ->
					?MODULE:recv(Rest, NextCallbackMod, NewClient);
				closed -> closed
			end;
		{Closed, _} -> closed;
		{Error, _, _} -> closed;
		{egs, keepalive} ->
			CallbackMod:keepalive(Client),
			?MODULE:recv(SoFar, CallbackMod, Client);
		Tuple when element(1, Tuple) =:= egs ->
			case CallbackMod:info(Tuple, Client) of
				{ok, NewClient} -> ?MODULE:recv(SoFar, CallbackMod, NewClient);
				_Any -> ?MODULE:recv(SoFar, CallbackMod, Client)
			end
	end.

%% Internal.

%% @doc Dispatch the commands received to the right handler.
dispatch([], _CallbackMod, NextMod, Client) ->
	{ok, NextMod, Client};
dispatch([Data|Tail], CallbackMod, NextMod, Client) ->
	Ret = case egs_proto:parse(Data) of
		{command, Command, Channel} ->
			case Channel of
				1 -> CallbackMod:cast(Command, Data, Client);
				_ -> CallbackMod:raw(Command, Data, Client)
			end;
		ignore ->
			ignore;
		Event ->
			CallbackMod:event(Event, Client)
	end,
	case Ret of
		{ok, NewMod, NewClient} ->
			dispatch(Tail, CallbackMod, NewMod, NewClient);
		{ok, NewClient} ->
			dispatch(Tail, CallbackMod, NextMod, NewClient);
		closed ->
			closed;
		_Any ->
			dispatch(Tail, CallbackMod, NextMod, Client)
	end.

%% @doc Split the network data received into commands.
split(Data, Acc) when byte_size(Data) < 4 ->
	{lists:reverse(Acc), Data};
split(<< Size:32/little, _/bits >> = Data, Acc) when Size > byte_size(Data) ->
	{lists:reverse(Acc), Data};
split(<< Size:32/little, _/bits >> = Data, Acc) ->
	<< Split:Size/binary, Rest/bits >> = Data,
	split(Rest, [Split|Acc]).

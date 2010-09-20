%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Game server module.
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

-module(egs_game_server).
-export([start_link/1, on_exit/1, init/1]).

-include("include/records.hrl").

%% @spec start_link(Port) -> {ok,Pid::pid()}
%% @doc Start the game server.
start_link(Port) ->
	{ok, MPid} = egs_exit_mon:start_link({?MODULE, on_exit}),
	register(egs_game_server_exit_mon, MPid),
	LPid = spawn(egs_network, listen, [Port, ?MODULE]),
	{ok, LPid}.

%% @spec on_exit(Pid) -> ok
%% @doc Cleanup the data associated with the failing process.
%% @todo Cleanup the instance process if there's nobody in it anymore.
%% @todo Leave party instead of stopping it.
on_exit(Pid) ->
	case egs_user_model:read({pid, Pid}) of
		{ok, User} ->
			case User#egs_user_model.partypid of
				undefined ->
					ignore;
				PartyPid ->
					{ok, NPCList} = psu_party:get_npc(PartyPid),
					[egs_user_model:delete(NPCGID) || {_Spot, NPCGID} <- NPCList],
					psu_party:stop(PartyPid)
			end,
			egs_user_model:delete(User#egs_user_model.id),
			{ok, List} = egs_user_model:select({neighbors, User}),
			lists:foreach(fun(Other) -> Other#egs_user_model.pid ! {egs, player_unspawn, User} end, List),
			io:format("game (~p): quit~n", [User#egs_user_model.id]);
		{error, _Reason} ->
			ignore
	end.

%% @doc Initialize the game state and start receiving messages.
%% @todo Handle keepalive messages globally?
init(Socket) ->
	egs_game_server_exit_mon ! {link, self()},
	timer:send_interval(5000, {egs, keepalive}),
	TmpGID = 16#ff000000 + mnesia:dirty_update_counter(counters, tmpgid, 1),
	psu_proto:send_0202(Socket, TmpGID),
	egs_network:recv(<< >>, egs_login, #state{socket=Socket, gid=TmpGID}).

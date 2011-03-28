%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc Patch server module.
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

-module(egs_patch_server).
-export([start_link/1]). %% API.
-export([listen/1, accept/1, init/1, recv/3]). %% Internal

-include("include/types.hrl").

-define(OPTIONS, [binary, {active, true}, {packet, 0}, {reuseaddr, true}, {send_timeout, 5000}]).

-record(state, {step=start, files=[]}).

%% @spec start_link(Port) -> {ok,Pid::pid()}
%% @doc Start the PSU patch server for inclusion in a supervisor tree.
start_link(Port) ->
	Pid = spawn(?MODULE, listen, [Port]),
	{ok, Pid}.

%% @spec listen(Port) -> ok
%% @doc Listen for connections.
listen(Port) ->
	error_logger:info_report(io_lib:format("listener started for ~p on port ~b", [?MODULE, Port])),
	{ok, LSocket} = gen_tcp:listen(Port, ?OPTIONS),
	?MODULE:accept(LSocket).

%% @spec accept(LSocket) -> ok
%% @doc Accept connections.
accept(LSocket) ->
	case gen_tcp:accept(LSocket, 5000) of
		{ok, CSocket} ->
			Pid = spawn(?MODULE, init, [CSocket]),
			gen_tcp:controlling_process(CSocket, Pid);
		{error, timeout} ->
			reload
	end,
	?MODULE:accept(LSocket).

%% @spec init(CSocket) -> ok
%% @doc Send the hello packet and move on to the main loop.
init(CSocket) ->
	send_01(CSocket),
	recv(CSocket, << >>, #state{}).

%% @spec recv(CSocket, SoFar, State) -> ok
%% @doc Receive commands from the client and process them.
recv(CSocket, SoFar, State) ->
	receive
		{tcp, CSocket, Data} ->
			{Commands, Rest} = split(<< SoFar/bits, Data/bits >>, []),
			case handle(CSocket, Commands, State) of
				closed -> closed;
				State2 -> ?MODULE:recv(CSocket, Rest, State2)
			end;
		{tcp_closed, CSocket} ->
			tcp_closed;
		{tcp_error, CSocket, _Any} ->
			tcp_error;
		_ ->
			?MODULE:recv(CSocket, SoFar, State)
	end.

%% @spec split(Bin, Acc) -> {Commands, Rest}
%% @doc Split the given binary into a list of commands.
split(<< >>, Acc) ->
	{lists:reverse(Acc), << >>};
split(Rest = << Size:32/little, Data/bits >>, Acc) when byte_size(Data) + 4 < Size ->
	{lists:reverse(Acc), Rest};
split(<< Size:32/little, Cmd:16/little, _Junk:16, Rest/bits >>, Acc) ->
	BitSize = 8 * Size - 64,
	<< Data:BitSize/bits, Rest2/bits >> = Rest,
	split(Rest2, [{command, Cmd, Size, Data}|Acc]).

%% @spec handle(CSocket, CommandsList) -> closed | State
%% @doc Handle the given commands.
handle(_CSocket, [], State) ->
	State;
%% Start of file info reply.
handle(CSocket, [{command, 16#0c, 8, << >>}|Tail], State=#state{step=waitfileinfo}) ->
	handle(CSocket, Tail, State#state{step=recvfileinfo});
%% File info.
handle(CSocket, [{command, 16#0d, 20, << FileNumber:32/little, CRC:32/little, Size:32/little >>}|Tail], State=#state{step=recvfileinfo}) ->
	State2 = case egs_patch_files_db:check(FileNumber, CRC, Size) of
		ok -> State;
		invalid -> State#state{files=[FileNumber|State#state.files]}
	end,
	handle(CSocket, Tail, State2);
%% End of file info reply. Nothing expected from the client afterward.
handle(CSocket, [{command, 16#0e, 8, << >>}], #state{step=recvfileinfo, files=Files}) ->
	case Files of
		[] -> ok; %% No files to update.
		_List -> update(CSocket, lists:reverse(Files))
	end,
	send_13(CSocket),
	closed;
%% Hello reply.
%% @todo Figure out the remaining unknown values.
handle(CSocket, [{command, 16#14, 52, << 16#e44c0915:32, UnknownA:32/little, UnknownB:32/little, UnknownC:32/little,
		UnknownD:32/little, _GameVersion:32/little, UnknownE:32/little, 0:128 >>}|Tail], State=#state{step=start}) ->
	io:format("patch #14: ~p ~p ~p ~p ~p~n", [UnknownA, UnknownB, UnknownC, UnknownD, UnknownE]),
	ListBin = egs_patch_files_db:list(),
	gen_tcp:send(CSocket, ListBin),
	handle(CSocket, Tail, State#state{step=waitfileinfo});
%% Unknown command.
handle(_CSocket, [{command, Cmd, _Size, Data}|_Tail], State) ->
	io:format("~p: dismissed command ~2.16.0b - ~p - ~p~n", [?MODULE, Cmd, Data, State]),
	closed.

%% @spec update(CSocket, Files) -> ok
%% @doc Update the invalid client files.
update(CSocket, Files) ->
	Size = update_size(Files),
	send_0f(CSocket, Size, length(Files)),
	update_files(CSocket, root, Files).

%% @spec update_files(CSocket, Files) -> ok
%% @doc Send all the files the client needs to update.
update_files(_CSocket, _CurrentFolder, []) ->
	ok;
update_files(CSocket, CurrentFolder, [FileNumber|Tail]) ->
	{file, _CRC, Size, Folder, FilenameBin, FullFilename} = egs_patch_files_db:get_info(FileNumber),
	case CurrentFolder of
		Folder -> ok;
		_Any ->
			if CurrentFolder =/= root ->
				send_0a(CSocket);
				true -> ok
			end,
			if Folder =/= root ->
				send_09(CSocket, Folder);
				true -> ok
			end
	end,
	send_10(CSocket, Size, FilenameBin),
	update_send_file(CSocket, FullFilename),
	send_12(CSocket),
	update_files(CSocket, Folder, Tail).

%% @spec update_send_file(CSocket, Filename) -> ok
%% @doc Send a file by fragmenting it into many chunks of fixed size.
update_send_file(CSocket, Filename) ->
	{ok, IoDevice} = file:open(Filename, [read, raw, binary]),
	update_send_file(CSocket, IoDevice, 0).
update_send_file(CSocket, IoDevice, N) ->
	case file:read(IoDevice, 24576) of
		{ok, Data} ->
			send_11(CSocket, Data, N),
			update_send_file(CSocket, IoDevice, N + 1);
		eof ->
			file:close(IoDevice)
	end.

%% @spec update_size(Files) -> Size
%% @doc Return the total size for all the files given.
update_size(Files) ->
	update_size(Files, 0).
update_size([], Size) ->
	Size;
update_size([FileNumber|Tail], Size) ->
	FileSize = egs_patch_files_db:get_size(FileNumber),
	update_size(Tail, Size + FileSize).

%% @spec send_01(CSocket) -> ok
%% @doc Hello command sent when a client connects to the server. Encryption is disabled.
send_01(CSocket) ->
	Bin = << 16#28:32/little, 16#01:32/little, 16#8b9f2dfa:32, 0:96, 1:32/little, 0:96 >>,
	gen_tcp:send(CSocket, Bin).

%% @spec send_09(CSocket, Folder) -> ok
%% @doc Change folder command.
send_09(CSocket, Folder) ->
	FolderBin = list_to_binary(Folder),
	Padding = 8 * (64 - length(Folder)),
	Bin = << 16#48:32/little, 16#09:32/little, FolderBin/binary, 0:Padding >>,
	gen_tcp:send(CSocket, Bin).

%% @spec send_0a(CSocket) -> ok
%% @doc Back to root folder command.
send_0a(CSocket) ->
	Bin = << 16#8:32/little, 16#0a:32/little >>,
	gen_tcp:send(CSocket, Bin).

%% @spec send_0f(CSocket, TotalSize, NbFiles) -> ok
%% @doc General update information command. Prepare the update screen.
send_0f(CSocket, Size, NbFiles) ->
	Bin = << 16#10:32/little, 16#0f:32/little, Size:32/little, NbFiles:32/little >>,
	gen_tcp:send(CSocket, Bin).

%% @spec send_10(CSocket, Size, FilenameBin) -> ok
%% @doc File update begin command. Prepare sending an individual file.
send_10(CSocket, Size, FilenameBin) ->
	Bin = << 16#50:32/little, 16#10:32/little, 0:32, Size:32/little, FilenameBin/binary >>,
	gen_tcp:send(CSocket, Bin).

%% @spec send_11(CSocket, Data, N) -> ok
%% @doc Command to send a file fragment.
send_11(CSocket, Data, N) ->
	DataSize = byte_size(Data),
	Padding = case DataSize rem 4 of
		0 -> 0;
		Rem -> 8 * (4 - Rem)
	end,
	Data2 = << Data/binary, 0:Padding >>,
	DataSize2 = DataSize + Padding div 8,
	Size = DataSize2 + 16#14,
	CRC = erlang:crc32(Data2),
	Bin = << Size:32/little, 16#11:32/little, N:32/little, CRC:32/little, DataSize:32/little, Data2/binary >>,
	gen_tcp:send(CSocket, Bin).

%% @spec send_12(CSocket) -> ok
%% @doc File update end command.
send_12(CSocket) ->
	Bin = << 16#8:32/little, 16#12:32/little >>,
	gen_tcp:send(CSocket, Bin).

%% @spec send_13(CSocket) -> ok
%% @doc Update complete command. Usually followed by the server closing the connection.
send_13(CSocket) ->
	Bin = << 16#8:32/little, 16#13:32/little >>,
	gen_tcp:send(CSocket, Bin).

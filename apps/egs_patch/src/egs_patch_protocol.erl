%%	Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

%% @doc Cowboy protocol module for the patch server.
-module(egs_patch_protocol).

-export([start_link/4, init/2]).

%% @todo Move that in a configuration file.
-define(TIMEOUT, 5000).

-record(state, {
	socket :: inet:socket(),
	transport :: module(),
	buffer = <<>> :: binary(),
	files = [] :: list(integer())
}).

-type state() :: #state{}.
-type cmd() :: 0..16#14.
-type cmd_size() :: 0..16#ffffffff.

-spec start_link(pid(), inet:socket(), module(), []) -> {ok, pid()}.
start_link(_ListenerPid, Socket, Transport, []) ->
	Pid = spawn_link(?MODULE, init, [Socket, Transport]),
	{ok, Pid}.

-spec init(inet:socket(), module()) -> ok | closed.
init(Socket, Transport) ->
	State = #state{socket=Socket, transport=Transport},
	send_01(State),
	wait_hello(State).

-spec next(state()) -> {ok, cmd(), cmd_size(), binary(), state()} | closed.
next(State=#state{buffer= << Size:32/little, Cmd:16/little, _:16, Rest/bits >>})
		when byte_size(Rest) + 8 >= Size ->
	Size2 = Size - 8,
	<< Data:Size2/binary, Buffer/bits >> = Rest,
	{ok, Cmd, Size, Data, State#state{buffer=Buffer}};
next(State=#state{socket=Socket, transport=Transport, buffer=Buffer}) ->
	Transport:setopts(Socket, [{active, once}]),
	{OK, Closed, Error} = Transport:messages(),
	receive
		{OK, Socket, Data} -> next(State#state{
			buffer= << Buffer/binary, Data/binary >>});
		{Closed, Socket} -> closed;
		{Error, Socket, _Reason} -> closed
	after ?TIMEOUT ->
		closed
	end.

-spec wait_hello(state()) -> ok | closed.
wait_hello(State) ->
	case next(State) of
		{ok, 16#14, 52, Data, State2} -> handle_hello(State2, Data);
		closed -> closed
	end.

-spec handle_hello(state(), binary()) -> ok | closed.
handle_hello(State=#state{socket=Socket, transport=Transport}, Data) ->
	<< 16#e44c0915:32, UnknownA:32/little,
		UnknownB:32/little, UnknownC:32/little, UnknownD:32/little,
		_GameVersion:32/little, UnknownE:32/little, 0:128 >> = Data,
	io:format("patch hello: ~p ~p ~p ~p ~p~n",
		[UnknownA, UnknownB, UnknownC, UnknownD, UnknownE]),
	ListBin = egs_patch_files_db:list(),
	Transport:send(Socket, ListBin),
	wait_fileinfo_begin(State).

-spec wait_fileinfo_begin(state()) -> ok | closed.
wait_fileinfo_begin(State) ->
	case next(State) of
		{ok, 16#0c, 8, <<>>, State2} -> wait_fileinfo(State2);
		closed -> closed
	end.

-spec wait_fileinfo(state()) -> ok | closed.
wait_fileinfo(State) ->
	case next(State) of
		{ok, 16#0d, 20, Data, State2} -> handle_fileinfo(State2, Data);
		{ok, 16#0e, 8, <<>>, State2} -> handle_fileinfo_end(State2);
		closed -> closed
	end.

-spec handle_fileinfo(state(), binary()) -> ok | closed.
handle_fileinfo(State=#state{files=Files}, Data) ->
	<< FileNumber:32/little, CRC:32/little, Size:32/little >> = Data,
	case egs_patch_files_db:check(FileNumber, CRC, Size) of
		ok -> wait_fileinfo(State);
		invalid -> wait_fileinfo(State#state{files=[FileNumber|Files]})
	end.

-spec handle_fileinfo_end(state()) -> ok.
handle_fileinfo_end(State=#state{files=[]}) ->
	handle_update_complete(State);
handle_fileinfo_end(State=#state{files=Files}) ->
	Files2 = lists:reverse(Files),
	State2 = State#state{files=Files2},
	send_0f(State2),
	handle_update(State2, root, Files2).

-spec handle_update(state(), root | string(), list(integer())) -> ok.
handle_update(State, _CurrentDir, []) ->
	handle_update_complete(State);
handle_update(State=#state{}, CurrentDir, [FileNumber|Tail]) ->
	{file, _CRC, Size, Dir, FilenameBin, FullFilename}
		= egs_patch_files_db:get_info(FileNumber),
	change_directory(State, CurrentDir, Dir),
	send_10(State, Size, FilenameBin),
	sendfile(State, FullFilename),
	send_12(State),
	handle_update(State, Dir, Tail).

-spec change_directory(state(), root | string(), root | string()) -> ok.
change_directory(_State, CurrentDir, CurrentDir) ->
	ok;
change_directory(State, _CurrentDir, root) ->
	send_0a(State);
change_directory(State, root, Dir) ->
	send_09(State, Dir).

-spec sendfile(state(), string()) -> ok.
sendfile(State, Filename) ->
	{ok, IoDevice} = file:open(Filename, [read, raw, binary]),
	sendfile(State, IoDevice, 0).
sendfile(State, IoDevice, N) ->
	case file:read(IoDevice, 24576) of
		{ok, Data} ->
			send_11(State, Data, N),
			sendfile(State, IoDevice, N + 1);
		eof ->
			ok = file:close(IoDevice)
	end.

-spec handle_update_complete(state()) -> ok.
handle_update_complete(State=#state{socket=Socket, transport=Transport}) ->
	send_13(State),
	ok = Transport:close(Socket).

-spec send_01(state()) -> ok.
%% @doc Hello command sent on connect. Encryption is disabled.
send_01(#state{socket=Socket, transport=Transport}) ->
	Bin = << 16#28:32/little, 16#01:32/little,
		16#8b9f2dfa:32, 0:96, 1:32/little, 0:96 >>,
	ok = Transport:send(Socket, Bin).

-spec send_09(state(), string()) -> ok.
%% @doc Change folder command.
send_09(#state{socket=Socket, transport=Transport}, Folder) ->
	FolderBin = list_to_binary(Folder),
	Padding = 8 * (64 - length(Folder)),
	Bin = << 16#48:32/little, 16#09:32/little, FolderBin/binary, 0:Padding >>,
	ok = Transport:send(Socket, Bin).

-spec send_0a(state()) -> ok.
%% @doc Back to root folder command.
send_0a(#state{socket=Socket, transport=Transport}) ->
	Bin = << 16#8:32/little, 16#0a:32/little >>,
	ok = Transport:send(Socket, Bin).

-spec send_0f(state()) -> ok.
%% @doc General update information command. Prepare the update screen.
send_0f(#state{socket=Socket, transport=Transport, files=Files}) ->
	Size = lists:foldl(
		fun(N, Acc) -> Acc + egs_patch_files_db:get_size(N) end, 0, Files),
	NbFiles = length(Files),
	Bin = << 16#10:32/little, 16#0f:32/little,
		Size:32/little, NbFiles:32/little >>,
	ok = Transport:send(Socket, Bin).

-spec send_10(state(), non_neg_integer(), binary()) -> ok.
%% @doc File update begin command. Prepare sending an individual file.
send_10(#state{socket=Socket, transport=Transport}, Size, FilenameBin) ->
	Bin = << 16#50:32/little, 16#10:32/little, 0:32,
		Size:32/little, FilenameBin/binary >>,
	ok = Transport:send(Socket, Bin).

-spec send_11(state(), binary(), non_neg_integer()) -> ok.
%% @doc Command to send a file fragment.
send_11(#state{socket=Socket, transport=Transport}, Data, N) ->
	DataSize = byte_size(Data),
	Padding = case DataSize rem 4 of
		0 -> 0;
		Rem -> 8 * (4 - Rem)
	end,
	Data2 = << Data/binary, 0:Padding >>,
	DataSize2 = DataSize + Padding div 8,
	Size = DataSize2 + 16#14,
	CRC = erlang:crc32(Data2),
	Bin = << Size:32/little, 16#11:32/little, N:32/little,
		CRC:32/little, DataSize:32/little, Data2/binary >>,
	ok = Transport:send(Socket, Bin).

-spec send_12(state()) -> ok.
%% @doc File update end command.
send_12(#state{socket=Socket, transport=Transport}) ->
	Bin = << 16#8:32/little, 16#12:32/little >>,
	ok = Transport:send(Socket, Bin).

-spec send_13(state()) -> ok.
%% @doc Update complete command. Followed by the server closing the connection.
send_13(#state{socket=Socket, transport=Transport}) ->
	Bin = << 16#8:32/little, 16#13:32/little >>,
	ok = Transport:send(Socket, Bin).

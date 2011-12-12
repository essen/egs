%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc EGS patch files database and cache manager.
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

-module(egs_patch_files_db).
-behavior(gen_server).

-export([start_link/0, stop/0, list/0, check/3, get_size/1, get_info/1, reload/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

-include_lib("kernel/include/file.hrl").

-record(state, {list_bin=[], files=[]}).
-record(file, {crc, size, folder, filename_bin, full_filename}).

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec list() -> binary()
list() ->
	gen_server:call(?SERVER, list).

%% @spec check(FileNumber, CRC, Size) -> ok | invalid
check(FileNumber, CRC, Size) ->
	gen_server:call(?SERVER, {check, FileNumber, CRC, Size}).

%% @spec get_size(FileNumber) -> Size
get_size(FileNumber) ->
	gen_server:call(?SERVER, {get_size, FileNumber}).

%% @spec get_info(FileNumber) -> {CRC, Size, FilenameBin, FullFilename}
get_info(FileNumber) ->
	gen_server:call(?SERVER, {get_info, FileNumber}).

%% @spec reload() -> ok
reload() ->
	gen_server:cast(?SERVER, reload).

%% gen_server.

init([]) ->
	{ok, build_state()}.

handle_call(list, _From, State=#state{list_bin=Bin}) ->
	{reply, Bin, State};

handle_call({check, FileNumber, CRC, Size}, _From, State=#state{files=Files}) ->
	File = proplists:get_value(FileNumber, Files),
	case File of
		#file{crc=CRC, size=Size} -> {reply, ok, State};
		_Any -> {reply, invalid, State}
	end;

handle_call({get_size, FileNumber}, _From, State=#state{files=Files}) ->
	File = proplists:get_value(FileNumber, Files),
	{reply, File#file.size, State};

handle_call({get_info, FileNumber}, _From, State=#state{files=Files}) ->
	{reply, proplists:get_value(FileNumber, Files), State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(reload, _State) ->
	{noreply, build_state()};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

build_state() ->
	{ok, App} = application:get_application(),
	{ok, Terms} = file:consult([code:priv_dir(App), "/patch.conf"]),
	Folders = proplists:get_value(folders, Terms),
	{ListBin, Files} = build_list_bin(Folders, Terms),
	#state{list_bin=ListBin, files=Files}.

%% The file number must start at 0.
build_list_bin(Folders, Terms) ->
	build_list_bin(Folders, Terms, 0, [], []).
build_list_bin([], _Terms, _N, Acc, FilesAcc) ->
	Bin = list_to_binary(lists:reverse(Acc)),
	Bin2 = << 16#08:32/little, 16#06:32/little, Bin/binary, 16#08:32/little, 16#08:32/little >>,
	{Bin2, lists:flatten(FilesAcc)};
build_list_bin([Folder|Tail], Terms, N, Acc, FilesAcc) ->
	Filenames = proplists:get_value({folder, Folder}, Terms),
	{BinFiles, Files, N2} = build_files_bin(Folder, Filenames, N),
	BinFiles2 = case Folder of
		root -> BinFiles;
		_Any ->
			FolderBin = list_to_binary(Folder),
			Padding = 8 * (64 - length(Folder)),
			<<	16#48:32/little, 16#09:32/little, FolderBin/binary, 0:Padding,
				BinFiles/binary, 16#08:32/little, 16#0a:32/little >>
	end,
	build_list_bin(Tail, Terms, N2, [BinFiles2|Acc], [Files|FilesAcc]).

build_files_bin(Folder, Filenames, N) ->
	build_files_bin(Folder, Filenames, N, [], []).
build_files_bin(_Folder, [], N, Acc, FilesAcc) ->
	Bin = list_to_binary(lists:reverse(Acc)),
	{Bin, FilesAcc, N};
build_files_bin(Folder, [Filename|Tail], N, Acc, FilesAcc) ->
	FullFilename = case Folder of
		root -> ["priv/patch/"|Filename];
		_Any -> ["priv/patch/",Folder,"/"|Filename]
	end,
	Size = file_get_size(FullFilename),
	CRC = file_get_crc(FullFilename),
	FilenameBin = list_to_binary(Filename),
	Padding = 8 * (64 - length(Filename)),
	FilenameBin2 = << FilenameBin/binary, 0:Padding >>,
	Bin = << 16#4c:32/little, 16#07:32/little, N:32/little, FilenameBin2/binary >>,
	build_files_bin(Folder, Tail, N + 1, [Bin|Acc], [{N, #file{crc=CRC, size=Size, folder=Folder, filename_bin=FilenameBin2, full_filename=FullFilename}}|FilesAcc]).

file_get_size(Filename) ->
	{ok, FileInfo} = file:read_file_info(Filename),
	FileInfo#file_info.size.

file_get_crc(Filename) ->
	{ok, IoDevice} = file:open(Filename, [read, raw, binary]),
	case file:read(IoDevice, 524288) of
		eof -> 0;
		{ok, Data} ->
			CRC = erlang:crc32(Data),
			file_get_crc(IoDevice, CRC)
	end.
file_get_crc(IoDevice, CRC) ->
	case file:read(IoDevice, 524288) of
		{ok, Data} ->
			CRC2 = erlang:crc32(CRC, Data),
			file_get_crc(IoDevice, CRC2);
		eof ->
			file:close(IoDevice),
			CRC
	end.

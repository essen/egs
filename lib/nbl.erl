%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Library for packing and unpacking NBL files.
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

-module(nbl).
-export([pack/1]).

%% @doc Pack an nbl file according to the given Options.
%%      Example usage: nbl:pack([{files, [{file, "table.rel", [16#184, 16#188, 16#1a0]}, {file, "text.bin", []}]}]).
pack(Options) ->
	OutFilename = proplists:get_value(out, Options, "unnamed.nbl"),
	Files = proplists:get_value(files, Options),
	{Header, Data, DataSize, PtrArray, PtrArraySize} = pack_files(Files),
	NbFiles = length(Files),
	HeaderSize = 16#30 + 16#60 * NbFiles,
	CompressedDataSize = 0,
	EncryptSeed = 0,
	NBL = << $N, $M, $L, $L, 2:16/little, 16#1300:16, HeaderSize:32/little, NbFiles:32/little,
		DataSize:32/little, CompressedDataSize:32/little, PtrArraySize:32/little, EncryptSeed:32/little,
		0:128, Header/binary, Data/binary, PtrArray/binary >>,
	file:write_file(OutFilename, NBL).

%% @doc Pack a list of files and return the header, data and pointer array parts.
pack_files(Files) ->
	pack_files(Files, {[], [], [], 0, 0}).
pack_files([], {AccH, AccD, AccP, _FilePos, _PtrIndex}) ->
	BinH = iolist_to_binary(lists:reverse(AccH)),
	PaddingH = 8 * (16#7d0 - (byte_size(BinH) rem 16#800)),
	PaddingH2 = if PaddingH =< 0 -> 16#800 + PaddingH; true -> PaddingH end,
	BinD = iolist_to_binary(lists:reverse(AccD)),
	PaddingD = 8 * (16#800 - (byte_size(BinD) rem 16#800)),
	BinP = iolist_to_binary(lists:reverse(AccP)),
	PtrSize = byte_size(BinP),
	PtrArray = case PtrSize of
		0 -> << >>;
		_ ->
			PaddingP = 8 * (16#800 - (byte_size(BinP) rem 16#800)),
			<< BinP/binary, 0:PaddingP >>
	end,
	{<< BinH/binary, 0:PaddingH2 >>,
	 << BinD/binary, 0:PaddingD >>, byte_size(BinD),
	 PtrArray, PtrSize};
pack_files([{data, Filename, Data, PtrList}|Tail], {AccH, AccD, AccP, FilePos, PtrIndex}) ->
	ID = case filename:extension(Filename) of
		".bin" -> << $S, $T, $D, 0 >>;
		[$.|String] -> list_to_binary(string:to_upper(String ++ [0]))
	end,
	FilenamePaddingBits = 8 * (32 - length(Filename)),
	DataSize = byte_size(Data),
	DataPadding = 16#20 - (DataSize rem 16#20),
	DataPaddingBits = 8 * DataPadding,
	DataSizeWithPadding = DataSize + DataPadding,
	PtrSize = 4 * length(PtrList),
	BinH = << ID/binary, 16#60000000:32, 0:64, (list_to_binary(Filename))/binary, 0:FilenamePaddingBits,
		FilePos:32/little, DataSize:32/little, PtrIndex:32/little, PtrSize:32/little >>,
	NXIF = case filename:extension(Filename) of
		".bin" -> << 0:256 >>;
		_ -> pack_nxif(DataSize, PtrSize)
	end,
	BinH2 = << BinH/binary, NXIF/binary >>,
	BinD = << Data/binary, 0:DataPaddingBits >>,
	BinP = iolist_to_binary([ << Ptr:32/little >> || Ptr <- PtrList]),
	pack_files(Tail, {[BinH2|AccH], [BinD|AccD], [BinP|AccP], FilePos + DataSizeWithPadding, PtrIndex + PtrSize});
pack_files([{file, Filename, PtrList}|Tail], Acc) ->
	{ok, Data} = file:read_file(Filename),
	pack_files([{data, Filename, Data, PtrList}|Tail], Acc).

%% @doc Return an NXIF chunk data for a specific data and pointer array size.
pack_nxif(DataSize, PtrSize) ->
	DataSize2 = DataSize + 16#20,
	PtrSize2 = PtrSize + 16#20 - (PtrSize rem 16#20),
	<< $N, $X, $I, $F, 16#18000000:32, 16#01000000:32, 16#20000000:32,
		DataSize:32/little, DataSize2:32/little, PtrSize2:32/little, 16#01000000:32 >>.

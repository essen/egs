%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc EGS script compiler.
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

-module(egs_script_compiler).
-export([compile/1]).

%% @doc Compile a script parsed using egs_script_lexer and egs_script_parser.
compile(ParseTree) ->
	{RootBin, Funcs} = root(ParseTree),
	FuncsPos = byte_size(RootBin),
	FuncsBin = funcs(Funcs),
	FuncsSize = byte_size(FuncsBin),
	<< $T, $S, $B, $2, FuncsPos:32/little, FuncsSize:32/little, RootBin/binary, FuncsBin/binary >>.

root(Routines) ->
	root(Routines, 0, [], []).
root(nil, _Pos, Funcs, Acc) ->
	{iolist_to_binary(lists:reverse(Acc)), Funcs};
root({Routine, Next}, Pos, Funcs, Acc) ->
	{Bin, Funcs2} = routine(Routine, Funcs),
	Pos2 = case Next of nil -> 0; _ -> Pos + byte_size(Bin) + 4 end,
	root(Next, Pos2, Funcs2, [<< Pos2:32/little, Bin/binary >>|Acc]).

routine({event_def, {Name, _Type}, Instrs}, Funcs) ->
	NameBin = routine_name_to_binary([$E, $V, $E, $N, $T, $.|Name]),
	{BodyBin, Funcs2} = routine_body_to_binary(Instrs, Funcs),
	{<< NameBin/binary, BodyBin/binary >>, Funcs2};
routine({function_def, {Name, _Type}, Instrs}, Funcs) ->
	NameBin = routine_name_to_binary(Name),
	{BodyBin, Funcs2} = routine_body_to_binary(Instrs, Funcs),
	{<< NameBin/binary, BodyBin/binary >>, [Name|Funcs2]};
routine({num_var, {Name, _Type}}, Funcs) ->
	NameBin = routine_name_to_binary(Name),
	{<< NameBin/binary, 0:32/little, 16#3c:32/little, 0:32 >>, [Name|Funcs]};
routine({str_var, {Name, _Type}, Length}, Funcs) ->
	NameBin = routine_name_to_binary(Name),
	{<< NameBin/binary, 0:32/little, 16#49:32/little, Length:32/little >>, [Name|Funcs]}.

routine_body_to_binary(Instrs, Funcs) ->
	{_Inc, InstrsBin, VarsList, Funcs2} = instructions(Instrs, Funcs),
	InstrsSize = byte_size(InstrsBin) + 4,
	NbVars = length(VarsList),
	VarsBin = iolist_to_binary([<< VarN:32/little, VarPos:32/little >> || {VarN, VarPos} <- VarsList]),
	{<< InstrsSize:32/little, 16#4c:32/little, NbVars:32/little, InstrsBin/binary, 0:32, VarsBin/binary >>, Funcs2}.

routine_name_to_binary(Name) ->
	NameBin = list_to_binary(Name),
	Padding = 8 * (32 - byte_size(NameBin)),
	<< NameBin/binary, 0:Padding >>.

instructions(Instrs, Funcs) ->
	instructions(Instrs, Funcs, 0, [], []).
instructions(Instrs, Funcs, Pos) ->
	instructions(Instrs, Funcs, Pos, [], []).
instructions(nil, Funcs, Pos, Acc, VarsAcc) ->
	{Pos, iolist_to_binary(lists:reverse(Acc)), lists:reverse(lists:flatten(VarsAcc)), Funcs};
instructions({Instr, Next}, Funcs, Pos, Acc, VarsAcc) ->
	{Inc, Bin, Vars, Funcs2} = instruction(Instr, Funcs, Pos),
	instructions(Next, Funcs2, Pos + Inc, [Bin|Acc], [Vars|VarsAcc]).

%% High level constructs.
instruction({'case', Tests}, Funcs, Pos) ->
	{Pos2, Bin, Vars, Funcs2} = case_tests(Tests, Funcs, Pos),
	{Pos2 - Pos, Bin, Vars, Funcs2};
%% Functions and syscalls.
instruction({function, {Name, Type}}, Funcs, Pos) ->
	{N2, Funcs2} = case find_func(Name, Funcs) of
		{{error, undefined}, N} -> {N, [Name|Funcs]};
		{ok, N} -> {N, Funcs}
	end,
	SyncBin = case Type of sync -> << 16#56:32/little >>; _ -> << >> end,
	{2 + byte_size(SyncBin) div 4, << 16#60:32/little, 16#ffffffff:32, SyncBin/binary >>, [{N2, Pos + 1}], Funcs2};
instruction({syscall, {N, async}}, Funcs, _Pos) ->
	{2, << 16#61:32/little, N:32/little >>, [], Funcs};
instruction({syscall, {N, sync}}, Funcs, _Pos) ->
	{3, << 16#61:32/little, N:32/little, 16#56:32/little >>, [], Funcs};
%% Low level instructions.
instruction('abs', Funcs, _Pos) ->
	{1, << 16#11:32/little >>, [], Funcs};
instruction(add, Funcs, _Pos) ->
	{1, << 16#04:32/little >>, [], Funcs};
instruction('band', Funcs, _Pos) ->
	{1, << 16#0b:32/little >>, [], Funcs};
instruction('bor', Funcs, _Pos) ->
	{1, << 16#0c:32/little >>, [], Funcs};
instruction('bxor', Funcs, _Pos) ->
	{1, << 16#0d:32/little >>, [], Funcs};
instruction(dec, Funcs, _Pos) ->
	{1, << 16#0f:32/little >>, [], Funcs};
instruction('div', Funcs, _Pos) ->
	{1, << 16#07:32/little >>, [], Funcs};
instruction(inc, Funcs, _Pos) ->
	{1, << 16#0e:32/little >>, [], Funcs};
instruction(is_eq, Funcs, _Pos) ->
	{1, << 16#12:32/little >>, [], Funcs};
instruction(is_gt, Funcs, _Pos) ->
	{1, << 16#15:32/little >>, [], Funcs};
instruction(is_gteq, Funcs, _Pos) ->
	{1, << 16#14:32/little >>, [], Funcs};
instruction(is_lt, Funcs, _Pos) ->
	{1, << 16#17:32/little >>, [], Funcs};
instruction(is_lteq, Funcs, _Pos) ->
	{1, << 16#16:32/little >>, [], Funcs};
instruction(is_neq, Funcs, _Pos) ->
	{1, << 16#13:32/little >>, [], Funcs};
instruction({jmp, N}, Funcs, _Pos) ->
	{2, << 16#2c:32/little, N:32/little-signed >>, [], Funcs};
instruction({jnz, N}, Funcs, _Pos) ->
	{2, << 16#2e:32/little, N:32/little-signed >>, [], Funcs};
instruction({jz, N}, Funcs, _Pos) ->
	{2, << 16#2d:32/little, N:32/little-signed >>, [], Funcs};
instruction(land, Funcs, _Pos) ->
	{1, << 16#18:32/little >>, [], Funcs};
instruction(lor, Funcs, _Pos) ->
	{1, << 16#19:32/little >>, [], Funcs};
instruction(lshift, Funcs, _Pos) ->
	{1, << 16#09:32/little >>, [], Funcs};
instruction(mod, Funcs, _Pos) ->
	{1, << 16#08:32/little >>, [], Funcs};
instruction(mul, Funcs, _Pos) ->
	{1, << 16#06:32/little >>, [], Funcs};
instruction(neg, Funcs, _Pos) ->
	{1, << 16#10:32/little >>, [], Funcs};
instruction(nop, Funcs, _Pos) ->
	{1, << 16#01:32/little >>, [], Funcs};
instruction({num_get, {Name, _Type}}, Funcs, Pos) ->
	{ok, N} = find_func(Name, Funcs),
	{2, << 16#3c:32/little, 16#ffffffff:32 >>, [{N, Pos + 1}], Funcs};
instruction({num_set, {Name, _Type}}, Funcs, Pos) ->
	{ok, N} = find_func(Name, Funcs),
	{2, << 16#3d:32/little, 16#ffffffff:32 >>, [{N, Pos + 1}], Funcs};
instruction({push, I}, Funcs, _Pos) when is_integer(I) ->
	{2, << 16#02:32/little, I:32/little-signed >>, [], Funcs};
instruction({push, F}, Funcs, _Pos) when is_float(F) ->
	{2, << 16#03:32/little, F:32/little-signed-float >>, [], Funcs};
instruction({push, Str}, Funcs, _Pos) when is_list(Str) ->
	StrBin = list_to_binary(Str),
	L = length(Str),
	Padding = 8 * (4 - L rem 4),
	StrBin2 = << StrBin/binary, 0:Padding >>,
	Size = byte_size(StrBin2) div 4,
	{2 + Size, << 16#46:32/little, Size:32/little, StrBin2/binary >>, [], Funcs};
instruction(restore, Funcs, _Pos) ->
	{1, << 16#1b:32/little >>, [], Funcs};
instruction(return, Funcs, _Pos) ->
	{1, << 16#00:32/little >>, [], Funcs};
instruction(rshift, Funcs, _Pos) ->
	{1, << 16#0a:32/little >>, [], Funcs};
instruction(save, Funcs, _Pos) ->
	{1, << 16#1a:32/little >>, [], Funcs};
instruction(savep, Funcs, _Pos) ->
	{1, << 16#1d:32/little >>, [], Funcs};
instruction({str_get, {Name, _Type}}, Funcs, Pos) ->
	{ok, N} = find_func(Name, Funcs),
	{2, << 16#49:32/little, 16#ffffffff:32 >>, [{N, Pos + 1}], Funcs};
instruction({str_set, {Name, _Type}}, Funcs, Pos) ->
	{ok, N} = find_func(Name, Funcs),
	{2, << 16#4a:32/little, 16#ffffffff:32 >>, [{N, Pos + 1}], Funcs};
instruction(sub, Funcs, _Pos) ->
	{1, << 16#05:32/little >>, [], Funcs};
%% Debug instruction. Do nothing by default. Change it to whatever needs testing.
instruction(debug, Funcs, _Pos) ->
	Bin = << 16#01:32/little >>,
	{byte_size(Bin) div 4, Bin, [], Funcs}.

case_tests(Tests, Funcs, Pos) ->
	case_tests(Tests, Funcs, Pos, [], []).
case_tests(nil, Funcs, Pos, Acc, VarsAcc) ->
	{Pos, case_tests_end(lists:reverse(Acc)), VarsAcc, Funcs};
case_tests({{case_default, Instrs}, Next}, Funcs, Pos, Acc, VarsAcc) ->
	{Pos2, InstrsBin, VarsList, Funcs2} = instructions(Instrs, Funcs, Pos),
	case_tests(Next, Funcs2, Pos2, [InstrsBin|Acc], [VarsList|VarsAcc]);
case_tests({{case_test, N, Instrs}, Next}, Funcs, Pos, Acc, VarsAcc) ->
	{Pos2, InstrsBin, VarsList, Funcs2} = instructions(Instrs, Funcs, Pos + 7),
	Jump = Pos2 - Pos - 4,
	TestBin = << 16#02:32/little, N:32/little-signed, 16#1d:32/little, 16#12:32/little,
		16#2d:32/little, Jump:32/little, 16#1b:32/little, InstrsBin/binary >>,
	case_tests(Next, Funcs2, Pos2 + 2, [TestBin|Acc], [VarsList|VarsAcc]).

case_tests_end(Tests) ->
	case_tests_end(Tests, []).
case_tests_end([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
case_tests_end([TestBin|Tail], Acc) ->
	TailBin = iolist_to_binary(Tail),
	Jump = byte_size(TailBin) div 4 + 2 * length(Tail),
	case_tests_end(Tail, [<< TestBin/binary, 16#2c:32/little, Jump:32/little >>|Acc]).

funcs(Funcs) ->
	funcs(Funcs, []).
funcs([], Acc) ->
	iolist_to_binary(Acc);
funcs([Name|Tail], Acc) ->
	NameBin = list_to_binary(Name),
	Padding = 8 * (32 - byte_size(NameBin)),
	funcs(Tail, [<< NameBin/binary, 0:Padding >>|Acc]).

find_func(Name, Funcs) ->
	find_func(Name, lists:reverse(Funcs), 0).
find_func(_Name, [], N) ->
	{{error, undefined}, N};
find_func(Name, [Func|_Tail], N) when Name =:= Func ->
	{ok, N};
find_func(Name, [_Func|Tail], N) ->
	find_func(Name, Tail, N + 1).

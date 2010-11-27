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
root({{external, Name}, Next}, Pos, Funcs, Acc) ->
	root(Next, Pos, [Name|Funcs], Acc);
root({Routine, Next}, Pos, Funcs, Acc) ->
	{Bin, Funcs2} = routine(Routine, Funcs),
	Pos2 = case Next of nil -> 0; _ -> Pos + byte_size(Bin) + 4 end,
	root(Next, Pos2, Funcs2, [<< Pos2:32/little, Bin/binary >>|Acc]).

routine({Type, Name, Instrs}, Funcs) ->
	{TypeBin, Funcs2} = case Type of
		event -> {<< $E, $V, $E, $N, $T, $. >>, Funcs};
		function -> {<< >>, [Name|Funcs]}
	end,
	NameBin = list_to_binary(Name),
	Padding = 8 * (32 - byte_size(TypeBin) - byte_size(NameBin)),
	{_Inc, InstrsBin, VarsList} = instructions(Instrs, Funcs),
	InstrsSize = byte_size(InstrsBin) + 4,
	NbVars = length(VarsList),
	VarsBin = iolist_to_binary([<< VarN:32/little, VarPos:32/little >> || {VarN, VarPos} <- VarsList]),
	{<<	TypeBin/binary, NameBin/binary, 0:Padding, InstrsSize:32/little,
		76:32/little, NbVars:32/little, InstrsBin/binary, 0:32, VarsBin/binary >>, Funcs2}.

instructions(Instrs, Funcs) ->
	instructions(Instrs, Funcs, 0, [], []).
instructions(Instrs, Funcs, Pos) ->
	instructions(Instrs, Funcs, Pos, [], []).
instructions(nil, _Funcs, Pos, Acc, VarsAcc) ->
	{Pos, iolist_to_binary(lists:reverse(Acc)), lists:reverse(lists:flatten(VarsAcc))};
instructions({Instr, Next}, Funcs, Pos, Acc, VarsAcc) ->
	{Inc, Bin, Vars} = instruction(Instr, Funcs, Pos),
	instructions(Next, Funcs, Pos + Inc, [Bin|Acc], [Vars|VarsAcc]).

instruction({call, Name}, Funcs, Pos) ->
	N = find_func(Name, Funcs),
	{2, << 96:32/little, 16#ffffffff:32 >>, [{N, Pos + 1}]};
instruction({'case', Tests}, Funcs, Pos) ->
	{Pos2, Bin, Vars} = case_tests(Tests, Funcs, Pos),
	{Pos2 - Pos, Bin, Vars};
instruction({push, N}, _Funcs, _Pos) when is_integer(N) ->
	{2, << 2:32/little, N:32/little >>, []};
instruction({push, Str}, _Funcs, _Pos) when is_list(Str) ->
	L = length(Str),
	L2 = L + 4 - L rem 4,
	L3 = L2 div 4,
	Padding = 8 * (L2 - L),
	StrBin = list_to_binary(Str),
	{3 + L3, << 70:32/little, L3:32/little, StrBin/binary, 0:Padding >>, []};
instruction({subcall, Name}, Funcs, Pos) ->
	N = find_func(Name, Funcs),
	{2, << 76:32/little, 16#ffffffff:32 >>, [{N, Pos + 1}]};
instruction({syscall, N}, _Funcs, _Pos) ->
	{2, << 97:32/little, N:32/little >>, []}.

case_tests(Tests, Funcs, Pos) ->
	case_tests(Tests, Funcs, Pos, [], []).
case_tests(nil, _Funcs, Pos, Acc, VarsAcc) ->
	{Pos, case_tests_end(lists:reverse(Acc)), VarsAcc};
case_tests({{case_default, Instrs}, Next}, Funcs, Pos, Acc, VarsAcc) ->
	{Pos2, InstrsBin, VarsList} = instructions(Instrs, Funcs, Pos),
	case_tests(Next, Funcs, Pos2, [InstrsBin|Acc], [VarsList|VarsAcc]);
case_tests({{case_test, N, Instrs}, Next}, Funcs, Pos, Acc, VarsAcc) ->
	{Pos2, InstrsBin, VarsList} = instructions(Instrs, Funcs, Pos + 7),
	Jump = Pos2 - Pos - 4,
	TestBin = << 2:32/little, N:32/little, 29:32/little, 18:32/little,
		45:32/little, Jump:32/little, 27:32/little, InstrsBin/binary >>,
	case_tests(Next, Funcs, Pos2, [TestBin|Acc], [VarsList|VarsAcc]).

case_tests_end(Tests) ->
	case_tests_end(Tests, []).
case_tests_end([], Acc) ->
	iolist_to_binary(lists:reverse(Acc));
case_tests_end([TestBin|Tail], Acc) ->
	TailBin = iolist_to_binary(Tail),
	Jump = byte_size(TailBin) div 4 + 2 * length(Tail),
	case_tests_end(Tail, [<< TestBin/binary, 44:32/little, Jump:32/little >>|Acc]).

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
find_func(Name, [Func|_Tail], N) when Name =:= Func ->
	N;
find_func(Name, [_Func|Tail], N) ->
	find_func(Name, Tail, N + 1).

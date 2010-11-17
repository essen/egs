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
	RootBin = root(ParseTree),
	FooterPos = byte_size(RootBin),
	FooterSize = 0, %% @todo
	<< $T, $S, $B, $2, FooterPos:32/little, FooterSize:32/little, RootBin/binary >>.

root(Routines) ->
	root(Routines, 0, []).
root(nil, _Pos, Acc) ->
	iolist_to_binary(lists:reverse(Acc));
root({Routine, Next}, Pos, Acc) ->
	Bin = routine(Routine),
	Pos2 = case Next of nil -> 0; _ -> Pos + byte_size(Bin) + 4 end,
	root(Next, Pos2, [<< Pos2:32/little, Bin/binary >>|Acc]).

routine({Type, Name, Instructions}) ->
	TypeBin = case Type of event -> << $E, $V, $E, $N, $T, $. >>; function -> << >> end,
	NameBin = list_to_binary(Name),
	Padding = 8 * (32 - byte_size(TypeBin) - byte_size(NameBin)),
	InstrsBin = instructions(Instructions),
	InstrsSize = byte_size(InstrsBin) + 4,
	VarsBin = << 76:32/little, 0:32/little >>, %% @todo 0 vars for now.
	<< TypeBin/binary, NameBin/binary, 0:Padding,
		InstrsSize:32/little, VarsBin/binary, InstrsBin/binary, 0:32 >>.

instructions(Instrs) ->
	instructions(Instrs, []).
instructions(nil, Acc) ->
	iolist_to_binary(lists:reverse(Acc));
instructions({Instr, Next}, Acc) ->
	instructions(Next, [instruction(Instr)|Acc]).

instruction({push, N}) when is_integer(N) ->
	<< 2:32/little, N:32/little >>;
instruction({syscall, N}) when is_integer(N) ->
	<< 97:32/little, N:32/little >>.

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc EGS script lexer.
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

Definitions.

D	= [0-9]
L	= [a-z]
N	= ({L}|{D}|_)
WS	= ([\000-\s]|%.*)

Rules.

{D}+	:	{token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{N}+	:	{token, case reserved_word(TokenChars) of
				false   -> case syscall(TokenChars) of
						false   -> {name, TokenLine, TokenChars};
						Syscall -> {syscall, TokenLine, Syscall}
					end;
				KeyWord -> {KeyWord, TokenLine}
			end}.
->		:	{token,{'->', TokenLine}}.
[}{,]	:	{token, {list_to_atom(TokenChars), TokenLine}}.
\.{WS}	:	{end_token, {dot, TokenLine}}.
{WS}+	:	skip_token.

Erlang code.

reserved_word("event")    -> event;
reserved_word("function") -> function;
reserved_word("push")     -> push;
reserved_word(_)          -> false.

syscall("play_music") -> 253;
syscall(_)            -> false.

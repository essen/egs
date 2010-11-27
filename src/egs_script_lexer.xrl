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
"(\\\^.|\\.|[^"])*" : %% Strip quotes.
			S = lists:sublist(TokenChars, 2, TokenLen - 2),
			{token, {string, TokenLine, string_gen(S)}}.
->		:	{token, {'->', TokenLine}}.
[}{,;]	:	{token, {list_to_atom(TokenChars), TokenLine}}.
\.{WS}	:	{end_token, {dot, TokenLine}}.
{WS}+	:	skip_token.

Erlang code.

reserved_word("call")     -> call;
reserved_word("case")     -> 'case';
reserved_word("default")  -> default;
reserved_word("end")      -> 'end';
reserved_word("event")    -> event;
reserved_word("external") -> external;
reserved_word("function") -> function;
reserved_word("push")     -> push;
reserved_word("subcall")  -> subcall;
reserved_word(_)          -> false.

syscall("disable_gamepad") -> 158;
syscall("enable_gamepad")  -> 159;
syscall("play_music")      -> 253;
syscall("set_value_flag")  -> 285;
syscall(_)                 -> false.

string_gen([$\\|Cs]) ->
	string_escape(Cs);
string_gen([C|Cs]) ->
	[C|string_gen(Cs)];
string_gen([]) ->
	[].

string_escape([O1,O2,O3|S]) when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
	[(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
	[C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= $\000, C =< $\s ->
	string_gen(Cs);
string_escape([C|Cs]) ->
	[escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n; %% \n = LF
escape_char($r) -> $\r; %% \r = CR
escape_char($t) -> $\t; %% \t = TAB
escape_char($v) -> $\v; %% \v = VT
escape_char($b) -> $\b; %% \b = BS
escape_char($f) -> $\f; %% \f = FF
escape_char($e) -> $\e; %% \e = ESC
escape_char($s) -> $\s; %% \s = SPC
escape_char($d) -> $\d; %% \d = DEL
escape_char(C) -> C.

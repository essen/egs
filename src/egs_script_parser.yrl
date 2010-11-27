%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc EGS script parser.
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

Nonterminals declarations declaration instructions instruction case_tests case_test case_default.
Terminals integer name string call case default end event external function push subcall syscall '->' ',' ';' dot.
Rootsymbol declarations.

declarations -> declaration declarations : {'$1', '$2'}.
declarations -> '$empty'                 : nil.

declaration  -> event name '->' instructions dot    : {event, unwrap('$2'), '$4'}.
declaration  -> external string dot                 : {external, unwrap('$2')}.
declaration  -> function name '->' instructions dot : {function, unwrap('$2'), '$4'}.

instructions -> instruction ',' instructions : {'$1', '$3'}.
instructions -> instruction                  : {'$1', nil}.

instruction  -> call string             : {call, unwrap('$2')}.
instruction  -> 'case' case_tests 'end' : {'case', '$2'}.
instruction  -> push integer            : {push, unwrap('$2')}.
instruction  -> push string             : {push, unwrap('$2')}.
instruction  -> subcall string          : {subcall, unwrap('$2')}.
instruction  -> syscall                 : {syscall, unwrap('$1')}.

case_tests   -> case_test ';' case_tests   : {'$1', '$3'}.
case_tests   -> case_test ';' case_default : {'$1', {'$3', nil}}.
case_tests   -> case_test                  : {'$1', nil}.
case_test    -> integer '->' instructions  : {case_test, unwrap('$1'), '$3'}.
case_default -> default '->' instructions  : {case_default, '$3'}.

Erlang code.

unwrap({_,_,V}) -> V.

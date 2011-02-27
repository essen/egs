%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
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
Terminals
	integer float string function syscall debug event_def function_def num_var str_var
	abs add band bor bxor dec div inc is_eq is_gt is_gteq is_lt is_lteq is_neq
	jmp jnz jz land lor lshift mod mul neg nop num_get num_set push restore return
	rshift save savep str_get str_set sub case default end '->' ',' ';' dot.
Rootsymbol declarations.

declarations -> declaration declarations : {'$1', '$2'}.
declarations -> '$empty'                 : nil.

declaration  -> event_def function '->' instructions dot    : {event_def, unwrap('$2'), '$4'}.
declaration  -> function_def function '->' instructions dot : {function_def, unwrap('$2'), '$4'}.
declaration  -> num_var function dot : {num_var, unwrap('$2')}.
declaration  -> str_var function integer dot : {str_var, unwrap('$2'), unwrap('$3')}.

instructions -> instruction ',' instructions : {'$1', '$3'}.
instructions -> instruction                  : {'$1', nil}.

instruction  -> abs              : abs.
instruction  -> add              : add.
instruction  -> band             : 'band'.
instruction  -> bor              : 'bor'.
instruction  -> bxor             : 'bxor'.
instruction  -> dec              : dec.
instruction  -> div              : 'div'.
instruction  -> inc              : inc.
instruction  -> is_eq            : is_eq.
instruction  -> is_gt            : is_gt.
instruction  -> is_gteq          : is_gteq.
instruction  -> is_lt            : is_lt.
instruction  -> is_lteq          : is_lteq.
instruction  -> is_neq           : is_neq.
instruction  -> jmp integer      : {jmp, unwrap('$2')}.
instruction  -> jnz integer      : {jnz, unwrap('$2')}.
instruction  -> jz integer       : {jz, unwrap('$2')}.
instruction  -> land             : land.
instruction  -> lor              : lor.
instruction  -> lshift           : lshift.
instruction  -> mod              : mod.
instruction  -> mul              : mul.
instruction  -> neg              : neg.
instruction  -> nop              : nop.
instruction  -> num_get function : {num_get, unwrap('$2')}.
instruction  -> num_set function : {num_set, unwrap('$2')}.
instruction  -> push integer     : {push, unwrap('$2')}.
instruction  -> push float       : {push, unwrap('$2')}.
instruction  -> push string      : {push, unwrap('$2')}.
instruction  -> restore          : restore.
instruction  -> return           : return.
instruction  -> rshift           : rshift.
instruction  -> save             : save.
instruction  -> savep            : savep.
instruction  -> str_get function : {str_get, unwrap('$2')}.
instruction  -> str_set function : {str_set, unwrap('$2')}.
instruction  -> sub              : sub.

instruction  -> debug            : debug.
instruction  -> function         : {function, unwrap('$1')}.
instruction  -> syscall          : {syscall, unwrap('$1')}.

instruction  -> 'case' case_tests 'end'    : {'case', '$2'}.
case_tests   -> case_test ';' case_tests   : {'$1', '$3'}.
case_tests   -> case_test ';' case_default : {'$1', {'$3', nil}}.
case_tests   -> case_test                  : {'$1', nil}.
case_test    -> integer '->' instructions  : {case_test, unwrap('$1'), '$3'}.
case_default -> default '->' instructions  : {case_default, '$3'}.

Erlang code.

unwrap({_,_,V}) -> V.

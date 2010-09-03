#	EGS: Erlang Game Server
#	Copyright (C) 2010  Loic Hoguin
#
#	This file is part of EGS.
#
#	EGS is free software: you can redistribute it and/or modify
#	it under the terms of the GNU Affero General Public License as
#	published by the Free Software Foundation, either version 3 of the
#	License, or (at your option) any later version.
#
#	EGS is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU Affero General Public License for more details.
#
#	You should have received a copy of the GNU Affero General Public License
#	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

ERL ?= erl
ERLC ?= erlc
APP := egs

all: clean missions server

server:
	@./rebar compile

missions:
	$(ERLC) src/psu/psu_parser.erl
	$(ERL) -noshell -noinput -sname missions -pa ebin -run psu_parser run -run init stop
	rm psu_parser.beam

clean:
	@./rebar clean
	rm -f erl_crash.dump

fclean: clean
	rm -rf Mnesia.egs*

run:
	@echo "EGS is free software available under the GNU GPL version 3"
	@echo "Copyright (C) 2010  Loic Hoguin"
	@echo 
	$(ERL) -ssl protocol_version '{sslv3}' -sname egs -pa ebin -boot start_sasl -s reloader -s egs

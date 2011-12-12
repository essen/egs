#!/bin/sh
echo "EGS is free software available under the GNU GPL version 3"
echo "Copyright (C) 2010-2011  Loic Hoguin"
echo 
erl -sname egs -pa apps/*/ebin -pa deps/*/ebin -boot start_sasl -s ex_reloader -s egs

#!/bin/sh
echo "EGS is free software available under the GNU GPL version 3"
echo "Copyright (C) 2010  Loic Hoguin"
echo 
erl -sname egs -pa ebin -boot start_sasl -s reloader -s egs

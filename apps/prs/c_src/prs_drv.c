/*
	@author Loïc Hoguin <essen@dev-extend.eu>
	@copyright 2010-2011 Loïc Hoguin.
	@doc PRS Erlang driver for EGS.

	This file is part of EGS.

	EGS is free software: you can redistribute it and/or modify
	it under the terms of the GNU Affero General Public License as
	published by the Free Software Foundation, either version 3 of the
	License, or (at your option) any later version.

	EGS is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Affero General Public License for more details.

	You should have received a copy of the GNU Affero General Public License
	along with EGS.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <erl_nif.h>

extern unsigned long prs_compress(unsigned char* source, unsigned char* dest, unsigned long size);

static ERL_NIF_TERM compress_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary srcbin;
	ErlNifBinary destbin;
	unsigned int size;

	if (argc != 1)
		return enif_make_badarg(env);

	if (!enif_is_binary(env, argv[0]))
		return enif_make_badarg(env);

	if (!enif_inspect_binary(env, argv[0], &srcbin))
		return enif_make_badarg(env);

	if (!enif_alloc_binary((9 * srcbin.size) / 8 + 2, &destbin))
		return enif_make_badarg(env);

	size = prs_compress(srcbin.data, destbin.data, srcbin.size);
	enif_realloc_binary(&destbin, size);

	return enif_make_binary(env, &destbin);
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
	return 0;
}

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
	return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
	return 0;
}

static void unload(ErlNifEnv* env, void* priv)
{
	return;
}

static ErlNifFunc nif_funcs[] = {
	{"compress", 1, compress_nif}
};

ERL_NIF_INIT(prs, nif_funcs, load, reload, upgrade, unload)

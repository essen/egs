%%-*- mode: erlang -*-
{application, egs, [
	{description, "EGS online action-RPG game server"},
	{vsn, "0.1"},
	{modules, [
		egs,
		egs_app,
		egs_sup,
		egs_exit_mon,
		reloader,
		egs_cron,
		egs_db,
		psu_game,
		psu_login,
		psu_patch,
		egs_proto,
		psu_appearance,
		psu_characters,
		psu_missions,
		psu_parser
	]},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		crypto,
		ssl,
		mnesia
	]},
	{mod, {egs_app, []}},
	{env, []}
]}.

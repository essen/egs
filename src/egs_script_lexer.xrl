%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
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
L	= [a-zA-Z]
N	= ({L}|{D}|_)
WS	= ([\000-\s]|%.*)

Rules.

-*{D}+				:	{token, {integer, TokenLine, list_to_integer(TokenChars)}}.
-*{D}+\.{D}+		:	{token, {float, TokenLine, list_to_float(TokenChars)}}.
{N}+\.*{N}+			:	{token, case reserved_word(TokenChars) of
							false   -> case syscall(TokenChars) of
									false   -> {function, TokenLine, function(TokenChars)};
									Syscall -> {syscall, TokenLine, Syscall}
								end;
							KeyWord -> {KeyWord, TokenLine}
						end}.
"(\\\^.|\\.|[^"])*"	:	%% Strip quotes.
						S = lists:sublist(TokenChars, 2, TokenLen - 2),
						{token, {string, TokenLine, string_gen(S)}}.
->					:	{token, {'->', TokenLine}}.
[}{,;]				:	{token, {list_to_atom(TokenChars), TokenLine}}.
\.{WS}				:	{end_token, {dot, TokenLine}}.
{WS}+				:	skip_token.

Erlang code.

%% Reserved words.
reserved_word("debug")    -> debug;
%% Definitions.
reserved_word("event")    -> event_def;
reserved_word("function") -> function_def;
reserved_word("num_var")  -> num_var;
reserved_word("str_var")  -> str_var;
%% Low level opcodes.
reserved_word("abs")      -> abs;
reserved_word("add")      -> add;
reserved_word("band")     -> 'band';
reserved_word("bor")      -> 'bor';
reserved_word("bxor")     -> 'bxor';
reserved_word("dec")      -> dec;
reserved_word("div")      -> 'div';
reserved_word("inc")      -> inc;
reserved_word("is_eq")    -> is_eq;
reserved_word("is_gt")    -> is_gt;
reserved_word("is_gteq")  -> is_gteq;
reserved_word("is_lt")    -> is_lt;
reserved_word("is_lteq")  -> is_lteq;
reserved_word("is_neq")   -> is_neq;
reserved_word("jmp")      -> jmp;
reserved_word("jnz")      -> jnz;
reserved_word("jz")       -> jz;
reserved_word("land")     -> land;
reserved_word("lor")      -> lor;
reserved_word("lshift")   -> lshift;
reserved_word("mod")      -> mod;
reserved_word("mul")      -> mul;
reserved_word("neg")      -> neg;
reserved_word("nop")      -> nop;
reserved_word("num_get")  -> num_get;
reserved_word("num_set")  -> num_set;
reserved_word("push")     -> push;
reserved_word("restore")  -> restore;
reserved_word("return")   -> return;
reserved_word("rshift")   -> rshift;
reserved_word("save")     -> save;
reserved_word("savep")    -> savep;
reserved_word("str_get")  -> str_get;
reserved_word("str_set")  -> str_set;
reserved_word("sub")      -> sub;
%% Case statement.
reserved_word("case")     -> 'case';
reserved_word("default")  -> default;
reserved_word("end")      -> 'end';
%% Otherwise isn't a reserved word.
reserved_word(_)          -> false.

%% Syscalls.
syscall("mes.fukidasi_pc")                  -> {100,  sync};
syscall("mes.fukidasi_apc")                 -> {101,  sync};
syscall("mes.fukidasi_npc")                 -> {102,  sync};
syscall("mes.fukidasi_pc_type")             -> {103,  sync};
syscall("mes.fukidasi_apc_type")            -> {104,  sync};
syscall("mes.fukidasi_npc_type")            -> {105,  sync};
syscall("mes.fukidasi_custom")              -> {106,  sync};
syscall("mes.erase_fukidasi")               -> {107, async};
syscall("mes.chat_pc")                      -> {108, async};
syscall("mes.cut_in_chat")                  -> {109, async};
syscall("mes.cut_in_chat_custom")           -> {110, async};
syscall("mes.system_win")                   -> {111,  sync};
syscall("mes.custom_win")                   -> {112,  sync};
syscall("mes.scroll_win")                   -> {113,  sync};
syscall("mes.win_move")                     -> {114, async};
syscall("mes.erase_system_win")             -> {115, async};
syscall("mes.select_win")                   -> {116,  sync};
syscall("mes.select_win_set")               -> {117,  sync};
syscall("mes.select_win_single")            -> {118,  sync};
syscall("mes.select_win_custom")            -> {119,  sync};
syscall("mes.select_win_fukidasi")          -> {120,  sync};
syscall("mes.select_win_b")                 -> {121,  sync};
syscall("mes.select_win_set_b")             -> {122,  sync};
syscall("mes.select_win_single_b")          -> {123,  sync};
syscall("mes.select_win_custom_b")          -> {124,  sync};
syscall("mes.select_win_fukidasi_b")        -> {125,  sync};
syscall("mes.erase_select_win")             -> {126,  sync};
syscall("mes.message_end")                  -> {127, async};
syscall("mes.set_name_npc")                 -> {128, async};
syscall("mes.set_color_npc")                -> {129, async};
syscall("mes.line_window")                  -> {130, async};
syscall("adv.cut_in")                       -> {131, async};
syscall("adv.cut_in_custom")                -> {132, async};
syscall("adv.window_disable")               -> {133, async};
syscall("adv.window_enable")                -> {134, async};
syscall("adv.fukidasi_disable")             -> {135, async};
syscall("adv.fukidasi_enable")              -> {136, async};
syscall("adv.player_disable")               -> {137, async};
syscall("adv.player_enable")                -> {138, async};
syscall("adv.chat_on")                      -> {139, async};
syscall("adv.chat_off")                     -> {140, async};
syscall("adv.mainmenu_disable")             -> {141, async};
syscall("adv.mainmenu_enable")              -> {142, async};
syscall("unit_flag.on")                     -> {143, async};
syscall("unit_flag.off")                    -> {144, async};
syscall("unit_flag.reverse")                -> {145, async};
syscall("unit_flag.delay_on")               -> {146, async};
syscall("unit_flag.delay_off")              -> {147, async};
syscall("unit_flag.delay_reverse")          -> {148, async};
syscall("unit_flag.chk")                    -> {149, async};
syscall("player.get_my_id")                 -> {150, async};
syscall("player.set_pos")                   -> {151, async};
syscall("player.change_unit")               -> {152, async};
syscall("player.turn")                      -> {153, async};
syscall("player.walk")                      -> {154, async};
syscall("player.run")                       -> {155, async};
syscall("player.pad_off")                   -> {158, async};
syscall("player.pad_on")                    -> {159, async};
syscall("player.get_pos")                   -> {160, async};
syscall("player.get_ang")                   -> {161, async};
syscall("player.turn_coord")                -> {162, async};
syscall("player.turn_member")               -> {163, async};
syscall("plymotion.item_take_off")          -> {173, async};
syscall("plymotion.set_pack")               -> {174,  sync};
syscall("plymotion.release_pack")           -> {176, async};
syscall("plymotion.set_loop")               -> {177, async};
syscall("plymotion.play_one_shot")          -> {178,  sync};
syscall("plymotion.cancel_one_shot")        -> {179, async};
syscall("plymotion.restart_one_shot")       -> {180, async};
syscall("apc.create")                       -> {183,  sync};
syscall("apc.delete")                       -> {184, async};
syscall("apc.delete_inx")                   -> {185, async};
syscall("apc.team_into")                    -> {186, async};
syscall("apc.team_remove")                  -> {187, async};
syscall("apc.set_pos")                      -> {188, async};
syscall("apc.turn")                         -> {189, async};
syscall("apc.walk")                         -> {190, async};
syscall("apc.run")                          -> {191, async};
syscall("apc.think_off")                    -> {194, async};
syscall("apc.think_on")                     -> {195, async};
syscall("apc.get_pos")                      -> {196, async};
syscall("apc.get_ang")                      -> {197, async};
syscall("apc.reserve")                      -> {205, async};
syscall("apc.reserve_cancel")               -> {206, async};
syscall("apc.is_reserve")                   -> {207, async};
syscall("apc.create_lv")                    -> {208,  sync};
syscall("apc.reserve_lv")                   -> {209, async};
syscall("apc.target_guard")                 -> {210, async};
syscall("apc.target_guard_pid")             -> {211, async};
syscall("apc.target_guard_aid")             -> {212, async};
syscall("apc.cancel_target_guard")          -> {213, async};
syscall("apc.set_move_out_party")           -> {214, async};
syscall("apc.apc_set_ban")                  -> {215, async};
syscall("apc.get_aid_from_pid")             -> {216, async};
syscall("apc.is_apc2")                      -> {217, async};
syscall("apc.is_alive")                     -> {218, async};
syscall("camera.set")                       -> {219, async};
syscall("camera.move")                      -> {220, async};
syscall("camera.release")                   -> {221, async};
syscall("camera.get_pos")                   -> {222, async};
syscall("camera.cam_rot")                   -> {223, async};
syscall("camera.target_rot")                -> {224, async};
syscall("camera.dist_move")                 -> {225, async};
syscall("camera.get_cam_angle")             -> {226, async};
syscall("camera.get_target_angle")          -> {227, async};
syscall("camera.get_dist")                  -> {228, async};
syscall("camera.set_quake")                 -> {229, async};
syscall("camera.stop_quake")                -> {230, async};
syscall("direction.fade_in")                -> {232, async};
syscall("direction.fade_out")               -> {233, async};
syscall("direction.fade_out_rgba")          -> {234, async};
syscall("direction.cinema_on")              -> {235, async};
syscall("direction.cinema_off")             -> {236, async};
syscall("direction.vib")                    -> {237, async};
syscall("wait.abort_cancel")                -> {238, async};
syscall("temp.exit_game")                   -> {239, async};
syscall("movie.play_prm")                   -> {240,  sync};
syscall("movie.play_rtm")                   -> {242,  sync};
syscall("movie.play_event")                 -> {245,  sync};
syscall("movie.play_sub_title")             -> {246,  sync};
syscall("movie.telop")                      -> {247,  sync};
syscall("sound.play_se")                    -> {251, async};
syscall("sound.play_bgm")                   -> {253, async}; %% Unofficial name.
syscall("sound.stop_bgm")                   -> {256, async};
syscall("render.scene_off")                 -> {257, async};
syscall("render.scene_on")                  -> {258, async};
syscall("render.npc_all_off")               -> {259, async};
syscall("render.npc_all_on")                -> {260, async};
syscall("render.npc_off")                   -> {261, async};
syscall("render.npc_on")                    -> {262, async};
syscall("render.player_off")                -> {263, async};
syscall("render.player_on")                 -> {264, async};
syscall("render.enemy_off")                 -> {265, async};
syscall("render.enemy_on")                  -> {266, async};
syscall("render.obj_off")                   -> {267, async};
syscall("render.obj_on")                    -> {268, async};
syscall("seq.get_quest")                    -> {271, async};
syscall("seq.is_online")                    -> {272, async};
syscall("seq.save_qst")                     -> {273,  sync};
syscall("seq.get_area")                     -> {274, async};
syscall("seq.get_zone")                     -> {275, async};
syscall("seq.get_unit")                     -> {276, async};
syscall("seq.get_item")                     -> {280,  sync};
syscall("work.accountwork_get")             -> {282,  sync};
syscall("work.accountwork_set")             -> {283,  sync};
syscall("work.chrwork_get")                 -> {284,  sync};
syscall("work.chrwork_set")                 -> {285,  sync};
syscall("work.chrflag_get")                 -> {286,  sync};
syscall("work.chrflag_set")                 -> {287,  sync};
syscall("work.partyflag_on")                -> {288,  sync};
syscall("work.partyflag_off")               -> {289,  sync};
syscall("work.qstwork_get")                 -> {290, async};
syscall("work.qstwork_get_member")          -> {291, async};
syscall("work.qstwork_set")                 -> {292, async};
syscall("work.partyflag_get")               -> {297, async};
syscall("work.partywork_get")               -> {298, async};
syscall("work.partywork_set")               -> {299, async};
syscall("work.zone_cndflag_get")            -> {300, async};
syscall("work.zone_cndflag_on")             -> {301, async};
syscall("work.zone_cndflag_off")            -> {302, async};
syscall("work.zone_sendwork_get")           -> {303, async};
syscall("work.zone_sendwork_set")           -> {304, async};
syscall("work.party_rand_get")              -> {305,  sync};
syscall("party.get_member")                 -> {306, async};
syscall("party.get_leader")                 -> {307, async};
syscall("party.chk_exist")                  -> {308, async};
syscall("party.sync_wait")                  -> {309,  sync};
syscall("party.join_on")                    -> {310, async};
syscall("party.join_off")                   -> {311, async};
syscall("party.get_enemy_kill")             -> {312, async};
syscall("ptcl.create")                      -> {315, async};
syscall("ptcl.move")                        -> {316, async};
syscall("ptcl.delete")                      -> {317, async};
syscall("col.create")                       -> {318, async};
syscall("col.create_lock_on")               -> {319, async};
syscall("col.create_attack")                -> {320, async};
syscall("col.move")                         -> {321, async};
syscall("col.scale")                        -> {322, async};
syscall("col.delete")                       -> {323, async};
syscall("npc.talk_on")                      -> {324, async};
syscall("npc.talk_off")                     -> {325, async};
syscall("npc.think_on")                     -> {326, async};
syscall("npc.think_off")                    -> {327, async};
syscall("npc.set_pos")                      -> {328, async};
syscall("npc.turn")                         -> {329, async};
syscall("npc.gaze")                         -> {330, async};
syscall("npc.walk")                         -> {331, async};
syscall("npc.run")                          -> {332, async};
syscall("npc.walk_path")                    -> {335, async};
syscall("npc.run_path")                     -> {336, async};
syscall("npc.get_pos")                      -> {338, async};
syscall("npc.get_ang")                      -> {339, async};
syscall("npc.cng_motion")                   -> {340, async};
syscall("npc.cancel_motion")                -> {341, async};
syscall("npc.get_motion")                   -> {342, async};
syscall("npc.coli_end")                     -> {343, async};
syscall("obj.coli_end")                     -> {347, async}; %% Unofficial name.
syscall("status.get_posflag")               -> {354, async};
syscall("lobby.start")                      -> {356,  sync};
syscall("lobby.end")                        -> {357, async};
syscall("lobby.select_planet")              -> {358,  sync};
syscall("camera.def_cng")                   -> {359, async};
syscall("camera.def_remove")                -> {360, async};
syscall("camera.def_move_back")             -> {361, async};
syscall("camera.def_set_back")              -> {362, async};
syscall("camera.def_set_quake")             -> {364, async};
syscall("camera.def_stop_qake")             -> {365, async};
syscall("calc.get_rot")                     -> {373, async};
syscall("sjis.fukidasi_pc")                 -> {389,  sync};
syscall("sjis.fukidasi_apc")                -> {390,  sync};
syscall("sjis.fukidasi_npc")                -> {391,  sync};
syscall("sjis.fukidasi_pc_type")            -> {392,  sync};
syscall("sjis.fukidasi_apc_type")           -> {393,  sync};
syscall("sjis.fukidasi_npc_type")           -> {394,  sync};
syscall("sjis.fukidasi_custom")             -> {395,  sync};
syscall("sjis.chat_pc")                     -> {396, async};
syscall("sjis.cut_in_chat")                 -> {397, async};
syscall("sjis.cut_in_chat_custom")          -> {398, async};
syscall("sjis.system_win")                  -> {399,  sync};
syscall("sjis.custom_win")                  -> {400,  sync};
syscall("sjis.scroll_win")                  -> {401,  sync};
syscall("sjis.select_win")                  -> {402,  sync};
syscall("sjis.select_win_set")              -> {403,  sync};
syscall("sjis.select_win_single")           -> {404,  sync};
syscall("sjis.select_win_custom")           -> {405,  sync};
syscall("sjis.select_win_fukidasi")         -> {406,  sync};
syscall("sjis.select_win_b")                -> {407,  sync};
syscall("sjis.select_win_set_b")            -> {408,  sync};
syscall("sjis.select_win_single_b")         -> {409,  sync};
syscall("sjis.select_win_custom_b")         -> {410,  sync};
syscall("sjis.select_win_fukidasi_b")       -> {411,  sync};
syscall("sjis.line_window")                 -> {412, async};
syscall("temp.line")                        -> {413, async};
syscall("temp.sline")                       -> {414, async};
syscall(_)                                  -> false.

%% Functions.
function("adv.cut_in_load")                 -> {"q.adv.cut_in_load",              async};
function("adv.cut_in_noise")                -> {"q.adv.cut_in_noise",             async};
function("adv.cut_in_release")              -> {"q.adv.cut_in_release",           async};
function("adv.cut_in_stop")                 -> {"q.adv.cut_in_stop",              async};
%% @todo apc.apc_sp_exec
%% @todo q.apc.create_ex
function("apc.get_reserve_aid_all")         -> {"apc.get_reserve_aid_all",        async};
function("apc.get_reserve_count")           -> {"apc.get_reserve_count",          async};
function("apc.guildcard_disable")           -> {"apc.guildcard_disable",          async};
function("apc.guildcard_enable")            -> {"apc.guildcard_enable",           async};
function("apc.impossible_join_pt")          -> {"apc.impossible_join_pt",         async};
function("apc.is_set_ban")                  -> {"apc.is_set_ban",                 async};
%% @todo apc.move
%% @todo apc.move_xz
function("apc.possible_join_pt")            -> {"apc.possible_join_pt",           async};
%% @todo apc.set_goal_out_party
function("apc.set_sp_ban")                  -> {"apc.set_sp_ban",                 async};
function("apc.set_talk_ban")                -> {"apc.set_talk_ban",               async};
function("counter.create_dc_map")           -> {"counter.create_dc_map",          async};
function("counter.delete_dc_map")           -> {"counter.delete_dc_map",          async};
function("counter.set_arrow_pos")           -> {"counter.set_arrow_pos",          async};
function("counter.set_lookon_pos")          -> {"counter.set_lookon_pos",         async};
function("direction.fade_out_nowloading")   -> {"direction.fade_out_nowloading",  async};
function("font.set_party_member_id")        -> {"font.set_party_member_id",       async};
function("item.exchange")                   -> {"q.seq.exchange_item",             sync};
function("item.get_num")                    -> {"q.seq.get_item_num",              sync};
function("item.get_slot_num_req")           -> {"q.item.get_slot_num_req",         sync};
function("item.get_slot_num_ret")           -> {"q.item.get_slot_num_ret",        async};
%% @todo item.set_ability
function("mes.mes_broadcast_clear")         -> {"mes.mes_broadcast_clear",        async};
%% @todo mes.mes_broadcast_clear_all
function("mes.mes_broadcast_set")           -> {"mes.mes_broadcast_set",          async};
function("mes.line_window_time")            -> {"q.mes.line_window_line_time",    async};
function("mes.select_win_number")           -> {"q.mes.select_win_number",         sync};
function("movie.text_prm_delay")            -> {"movie.text_prm_delay",           async};
function("myroom.check_data_load")          -> {"myroom.check_data_load",         async};
function("myroom.disable_obj_ex")           -> {"myroom.disable_obj_ex",          async};
function("myroom.get_move_planet")          -> {"myroom.get_move_planet",         async};
function("myroom.get_probo_ang")            -> {"myroom.get_probo_ang",           async};
function("myroom.get_probo_pos")            -> {"myroom.get_probo_pos",           async};
function("myroom.get_probo_rank")           -> {"myroom.get_probo_rank",          async};
function("myroom.news_enable")              -> {"myroom.news_enable",             async};
%% @todo myroom.get_probo_type
function("npc.gaze_cancel")                 -> {"npc.gaze_cancel",                async};
%% @todo npc.get_head_y
function("npc.is_load_comp")                -> {"npc.is_load_comp",               async};
%% @todo q.npc.run_fast
%% @todo npc.run_xz_fast
%% @todo q.npc.walk_fast
%% @todo npc.walk_xz_fast
%% @todo obj.chair_disable
%% @todo obj.chair_enable
function("obj.get_eventflag")               -> {"obj.get_eventflag",              async};
function("obj.set_caption")                 -> {"obj.set_caption",                async};
function("obj.vehicle_resetpos")            -> {"obj.vehicle_resetpos",           async};
function("player.display")                  -> {"player.display",                 async};
function("player.disp_off_except_party")    -> {"player.disp_off_except_party",   async};
function("player.disp_on_except_party")     -> {"player.disp_on_except_party",    async};
function("player.face_set")                 -> {"player.face_set",                async};
%% @todo player.gaze_body
%% @todo player.get_action
function("player.get_level")                -> {"player.get_level",               async};
function("player.get_my_pmid")              -> {"q.player.get_my_pmid",           async};
function("player.get_occupation")           -> {"player.get_occupation",          async};
function("player.get_off_vehicle")          -> {"player.get_off_vehicle",         async};
%% @todo player.get_on_vehicle
function("player.get_race")                 -> {"player.get_race",                async};
function("player.get_sex")                  -> {"player.get_sex",                 async};
function("player.hide")                     -> {"player.hide",                    async};
function("player.is_dead")                  -> {"player.is_dead",                 async};
%% @todo player.is_equipping_item
function("player.is_in_sp")                 -> {"player.is_in_sp",                async};
function("player.is_on_vehicle")            -> {"player.is_on_vehicle",           async};
%% @todo player.move
%% @todo player.move_xz
%% @todo player.set_vehicle_breakpos
function("player.sp_cancel")                -> {"player.sp_cancel",               async};
%% @todo player.turn_round
%% @todo player.turn_round_coord_xz
function("plymotion.can_i_set_event_pack")  -> {"plymotion.can_i_set_event_pack", async};
%% @todo plymotion.get_pack
function("plymotion.is_ready")              -> {"plymotion.is_ready",             async};
function("plymotion.item_re_equip")         -> {"plymotion.item_re_equip",        async};
%% @todo radar.display
%% @todo radar.fullclose
function("radar.fullopen")                  -> {"radar.fullopen",                 async};
%% @todo radar.hide
function("seq.endroll_play")                -> {"q.seq.endroll_play",              sync};
%% @todo seq.get_death_count
%% @todo seq.get_death_count_apc
function("seq.get_destroy_enemy_count")     -> {"seq.get_destroy_enemy_count",    async};
function("seq.get_elapsed_time")            -> {"seq.get_elapsed_time",           async};
function("seq.get_judgement")               -> {"seq.get_judgement",              async};
function("seq.get_mission_step")            -> {"seq.get_mission_step",           async};
function("seq.get_rank")                    -> {"seq.get_rank",                   async};
%% @todo seq.get_remaining_time
%% @todo seq.get_setdata_inx
%% @todo seq.goto_title
function("seq.has_finished_result")         -> {"seq.has_finished_result",        async};
function("seq.open_ex_mode")                -> {"seq.open_ex_mode",               async};
function("seq.preview_network")             -> {"q.seq.preview_network",           sync};
function("seq.preview_play")                -> {"q.seq.preview_play",              sync};
function("seq.result")                      -> {"q.seq.result",                   async};
function("seq.save_menu")                   -> {"q.seq.save_menu",                 sync};
%% @todo seq.set_chapter_no
function("seq.set_ex_chapter_no")           -> {"seq.set_ex_chapter_no",          async};
%% @todo seq.set_story_end_flag
function("seq.staffroll_play")              -> {"q.seq.staffroll_play",            sync};
%% @todo q.seq.unify_counter
function("shop.open")                       -> {"shop.open",                      async};
%% @todo shop.req_load_data
function("sjis.line_window_time")           -> {"q.sjis.line_window_time",        async};
function("sound.check_jingle")              -> {"sound.check_jingle",             async};
%% @todo sound.get_player_pack
function("sound.load_enemy_pack")           -> {"sound.load_enemy_pack",          async};
%% @todo sound.load_enemy_port_pack
function("sound.load_event_pack")           -> {"sound.load_event_pack",          async};
function("sound.load_player_pack")          -> {"sound.load_player_pack",         async};
%% @todo sound.pause_adx_voice
function("sound.pause_bgm")                 -> {"sound.pause_bgm",                async};
function("sound.play_jingle")               -> {"sound.play_jingle",              async};
function("sound.play_se3d_loop_stop")       -> {"sound.play_se3d_loop_stop",      async};
function("sound.play_se3d_loop_vol")        -> {"sound.play_se3d_loop_vol",       async};
function("sound.play_player")               -> {"q.sound.play_player",             sync};
function("sound.play_se3d")                 -> {"q.sound.play_se3d",               sync};
%% @todo sound.play_se3d_loop
function("sound.play_se3d_vol")             -> {"q.sound.play_se3d_vol",           sync};
function("sound.play_technique")            -> {"sound.play_technique",           async};
function("sound.play_vehicle")              -> {"sound.play_vehicle",             async};
function("sound.stop_adx_voice")            -> {"sound.stop_adx_voice",           async};
function("system.frame_adjust")             -> {"system.frame_adjust",            async};
%% @todo system.frame_default
function("system.frame_get")                -> {"system.frame_get",               async};
%% @todo system.frame_set
function("system.get_episode")              -> {"system.get_episode",             async};
%% @todo system.get_language
function("system.get_platform")             -> {"system.get_platform",            async};
function("system.get_resolution")           -> {"system.get_resolution",          async};
function("work.get_stamp_bonus")            -> {"q.work.get_stamp_bonus",          sync};
function("work.get_stamp_num")              -> {"q.work.get_stamp_num",            sync};
function("work.get_stamp_quest")            -> {"q.work.get_stamp_quest",          sync};
%% @todo q.work.on_accountwork_get
function("work.partyworknum_get")           -> {"q.work.partyworknum_get",        async};
function("work.partyworknum_set")           -> {"q.work.partyworknum_set",        async};
%% @todo q.work.stamp_command
function(Name)                              -> {Name,                             local}.

%% Strings.
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

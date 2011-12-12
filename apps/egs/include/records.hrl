%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2011 Loïc Hoguin.
%% @doc Project-wide Erlang records.
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

%% Records.

%% @doc Client state. One per connected client.
-record(client, {
	socket			:: ssl:sslsocket(),
	transport		:: module(),
	gid = 0			:: egs:gid(),
	slot = 0		:: egs:character_slot(), %% @todo Probably should remove this one from the state.
	lid = 16#ffff	:: egs:lid(),
	areanb = 0		:: non_neg_integer()
}).

%% @doc Table containing the users currently logged in.
%% @todo Probably can use a "param" or "extra" field to store the game-specific information (for things that don't need to be queried).
-record(users, {
	%% General information.
	gid				:: egs:gid(),
	lid = 16#ffff	:: egs:lid(),
	pid				:: pid(),
	%% Character information.
	%% @todo Specs it.
	type = white,
	slot,
	npcid = 16#ffff,
	name,
	race,
	gender,
	class,
	level = 1,
	exp = 0,
	currenthp = 100,
	maxhp = 100,
	stats = {stats, 1000, 2000, 3000, 4000, 5000, 6000, 7000},
	money = 1000000,
	blastbar = 0,
	luck = 3,
	appearance,
	onlinestatus = 0,
	options = {options, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0},
	inventory = [],
	%% Location/state related information.
	uni			:: integer(),
	questpid	:: pid(),
	zonepid		:: pid(),
	partypid	:: pid(),
	areatype	:: counter | mission | lobby | myroom,
	area		:: egs:area(),
	entryid		:: egs:entryid(),
	pos = {0.0, 0.0, 0.0, 0.0} :: egs:position(),
	shopid		:: integer(),
	prev_area = {0, 0, 0} :: egs:area(),
	prev_entryid = 0 :: egs:entryid(),
	%% To be moved or deleted later on.
	instancepid	:: pid()
}).

%% Past this point needs to be reviewed.

%% @doc NPC configuration data.
%% @todo Add inventory, AI parameters.
-record(npc, {name, race, gender, class, level_diff, appearance}).

%% @doc Character appearance data structure, flesh version.
-record(flesh_appearance, {
	voicetype, voicepitch=127,
	jacket, pants, shoes, ears, face, hairstyle,
	jacketcolor=0, pantscolor=0, shoescolor=0, lineshieldcolor=0, badge=0,
	eyebrows=0, eyelashes=0, eyesgroup=0, eyes=0,
	bodysuit=0,
	eyescolory=32767, eyescolorx=0,
	lipsintensity=32767, lipscolory=32767, lipscolorx=0,
	skincolor=65535,
	hairstylecolory=32767, hairstylecolorx=0,
	proportion=65535, proportionboxx=65535, proportionboxy=65535,
	faceboxx=65535, faceboxy=65535
}).

%% @doc Character appearance data structure, metal version.
-record(metal_appearance, {
	voicetype, voicepitch=127,
	torso, legs, arms, ears, face, headtype,
	maincolor=0, lineshieldcolor=0,
	eyebrows=0, eyelashes=0, eyesgroup=0, eyes=0,
	eyescolory=32767, eyescolorx=0,
	bodycolor=65535, subcolor=196607,
	hairstylecolory=32767, hairstylecolorx=0,
	proportion=65535, proportionboxx=65535, proportionboxy=65535,
	faceboxx=65535, faceboxy=65535
}).

%% @doc Character stats data structure.

-record(stats, {atp, ata, tp, dfp, evp, mst, sta}).

%% @doc Character options data structure.

-record(options, {textdisplayspeed, sound, musicvolume, soundeffectvolume, vibration, radarmapdisplay,
	cutindisplay, mainmenucursorposition, camera3y, camera3x, camera1y, camera1x, controller, weaponswap,
	lockon, brightness, functionkeysetting, buttondetaildisplay}).

%% @doc Hit response data.

-record(hit_response, {type, user, exp, damage, targethp, targetse, events}).

%% @doc Items.

-record(psu_element, {type, percent}).
-record(psu_pa, {type, level}).

-record(psu_item, {name, rarity, buy_price, sell_price, data}).
-record(psu_clothing_item, {appearance, type, manufacturer, overlap, gender, colors}).
-record(psu_consumable_item, {max_quantity, pt_diff, status_effect, action, target, use_condition, item_effect}).
-record(psu_parts_item, {appearance, type, manufacturer, overlap, gender}).
-record(psu_special_item, {}).
-record(psu_striking_weapon_item, {pp, atp, ata, atp_req, shop_element, hand, max_upgrades, attack_label,
	attack_sound, hitbox_a, hitbox_b, hitbox_c, hitbox_d, nb_targets, effect, model}).
-record(psu_trap_item, {max_quantity, effect, type}).

-record(psu_clothing_item_variables, {color}).
-record(psu_consumable_item_variables, {quantity}).
-record(psu_parts_item_variables, {}).
-record(psu_special_item_variables, {}).
-record(psu_striking_weapon_item_variables, {is_active=0, slot=0, current_pp, max_pp, element, pa=#psu_pa{type=0, level=0}}).
-record(psu_trap_item_variables, {quantity}).

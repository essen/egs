%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010-2012 Loïc Hoguin.
%% @doc Login and game servers network handling.
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

-module(egs_net).

%% Client state manipulation API.
-export([init/4]).
-export([terminate/1]).
-export([get_gid/1]).
-export([set_gid/2]).
-export([set_handler/2]).
-export([set_keepalive/1]).

%% Receive loop.
-export([loop/1]).

%% Response API.
-export([account_character/2]).
-export([account_characters_response/2]).
-export([account_flags/4]).
-export([comm_own_card/2]).
-export([system_auth_error/2]).
-export([system_game_server_response/3]).
-export([system_hello/1]).
-export([system_key_auth_info/3]).
-export([system_motd_response/3]).
-export([system_open_url/2]).

%% @todo Temporary, remove.
-export([character_appearance_to_binary/2]).

-include_lib("erlson/include/erlson.hrl").

%% Network state.

-record(egs_net, {
	socket :: ssl:sslsocket(),
	transport :: module(),
	handler :: module(),
	buffer = <<>> :: binary(),
	keepalive = false :: boolean(),

	gid = 0 :: egs:gid(),
	targetid = 16#ffff :: egs:targetid(),
	slot = 0 :: 0..3, %% @todo Remove.
	areanb = 0 :: non_neg_integer()
}).

%% Little-endian macros.

-define(l16, :16/little).
-define(l32, :32/little).
-define(l32f, :32/little-float).

%% Client state manipulation API.

init(Socket, Transport, Handler, GID) ->
	#egs_net{socket=Socket, transport=Transport,
		handler=Handler, gid=GID}.

terminate(#egs_net{socket=Socket, transport=Transport}) ->
	Transport:close(Socket).

get_gid(#egs_net{gid=GID}) ->
	GID.

set_gid(GID, State) ->
	State#egs_net{gid=GID}.

set_handler(Handler, State) ->
	State#egs_net{handler=Handler}.

set_keepalive(State) ->
	State#egs_net{keepalive=true}.

%% Receive loop.

loop(State=#egs_net{socket=Socket, transport=Transport}) ->
	Transport:setopts(Socket, [{active, once}]),
	{OK, Closed, Error} = Transport:messages(),
	receive
		{OK, _, Data} -> handle(State, Data);
		{Closed, _} -> closed;
		{Error, _, _} -> closed;
		{egs, keepalive} -> keepalive(State);
		Info when element(1, Info) =:= egs -> info(State, Info)
	end.

handle(State=#egs_net{buffer=Buffer}, Data) ->
	{Commands, Rest} = split(<< Buffer/binary, Data/binary >>),
	dispatch(State#egs_net{buffer=Rest}, Commands).

dispatch(State, []) ->
	?MODULE:loop(State);
dispatch(State, [Data|Tail]) ->
	case parse(Data) of
		ignore ->
			dispatch(State, Tail);
		{Type, Event} ->
			case call(State, Event, Type) of
				closed -> closed;
				ok -> dispatch(State, Tail);
				{ok, State2} -> dispatch(State2, Tail)
			end
	end.

%% If keepalive is enabled we just send an empty packet since the
%% real keepalive packet is managed by Gameguard which is better disabled.
keepalive(State=#egs_net{keepalive=false}) ->
	?MODULE:loop(State);
keepalive(State=#egs_net{keepalive=true}) ->
	send_packet(<< 8?l32, 0:32 >>, State),
	?MODULE:loop(State).

info(State, Info) ->
	case call(State, Info, info) of
		closed -> closed;
		ok -> ?MODULE:loop(State);
		{ok, State2} -> ?MODULE:loop(State2)
	end.

%% @todo The try..catch should be only enabled for debug.
call(State=#egs_net{handler=Handler, gid=GID}, Data, Name) ->
	try
		%% @todo Make this io:format optional.
		io:format("~b -> ~p ~p~n", [GID, Name, Data]),
		Handler:Name(Data, State)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler error in ~p for ~p:~n"
			"   ~p~n"
			"   for the reason ~p:~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, Name, Data, Class, Reason, erlang:get_stacktrace()])
	end.

%% Packet parsing code.

split(Data) ->
	split(Data, []).
split(Data, Acc) when byte_size(Data) < 4 ->
	{lists:reverse(Acc), Data};
split(Data = << Size:32/little, _/bits >>, Acc) when Size > byte_size(Data) ->
	{lists:reverse(Acc), Data};
split(Data = << Size:32/little, _/bits >>, Acc) ->
	<< Packet:Size/binary, Rest/bits >> = Data,
	split(Rest, [Packet|Acc]).

%% Completely ignore the fragmented packet replies.
parse(<< 8:32/little, 16#0b05:16, _:16 >>) ->
	ignore;
%% Catch parse errors and prints a dump of the packet with useful info.
parse(<< Size:32/little, Category:8, Sub:8, Channel:8, _:8, Data/bits >>) ->
	try begin
		Event = parse(Size, Category * 256 + Sub, Channel, Data),
		case {Event, Channel} of
			{ignore, _} -> ignore; %% @todo Always return something.
			{Event, 1} -> {cast, Event};
			{Event, _} -> {event, Event}
		end
	end catch Class:Reason ->
		error_logger:error_msg(lists:flatten([
			"** Parse error in with reason ~p:~p~n"
			"   for command #~2.16.0b~2.16.0b of size ~b on channel ~b~n~n",
			binary_to_dump(Data), "~n~n"]),
			[Class, Reason, Category, Sub, Size, Channel]),
		ignore
	end.

%% @todo Documentation.
%% @todo Probably shouldn't be ignored.
parse(92, 16#0102, 2, Data) ->
	<<	DestTargetID?l16, _:16,
		CharActType:32, CharGID?l32, 0:32, 0:32,
		DestActType:32, DestGID?l32, 0:32, 0:32,
		CharGID?l32, CharTargetID?l16, _:16,
		AnimType:8, AnimState:8, Dir?l16, 0:32,
		X?l32f, Y?l32f, Z?l32f, QuestID?l32,
		ZoneID?l16, _:16, MapID?l16, _:16, EntryID?l16, _:16, 0:32
		>> = Data,
	ignore;

%% @todo Documentation.
%% @todo A B
parse(Size, 16#0105, 2, Data) ->
	<<	DestTargetID?l16, _:16,
		0:32, CharGID?l32, 0:32, 0:32,
		DestActType:32, DestGID?l32, 0:32, 0:32,
		CharGID?l32, CharTargetID?l16, _:16,
		ItemIndex:8, EventID:8, PAID:8, A:8,
		B?l32, Rest/binary
		>> = Data,
	Event = item_eventid_to_atom(EventID),
	case Event of
		item_drop ->
			Size = 76,
			<< Quantity?l32, X?l32f, Y?l32f, Z?l32f >> = Rest,
			{Event, ItemIndex, CharGID, CharTargetID, A, B,
				Quantity, X, Y, Z};
		Event ->
			Size = 60,
			<<>> = Rest,
			{Event, ItemIndex, CharGID, CharTargetID, A, B}
	end;

%% @todo Documentation.
%% @todo A _B
parse(60, 16#010a, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		DestGID?l32, DestTargetID?l16, _:16,
		EventID?l16, QuantityOrColor:8, A:8, Param:32/bits
		>> = Data,
	Event = npc_shop_eventid_to_atom(EventID),
	case Event of
		Event when Event =:= npc_shop_enter; Event =:= npc_shop_leave ->
			<< ShopID?l16, 0:16 >> = Param,
			QuantityOrColor = 0,
			A = 0,
			{Event, ShopID};
		npc_shop_buy ->
			<< ItemIndex?l16, 0:16 >> = Param,
			QuantityOrColor = A,
			{Event, ItemIndex, QuantityOrColor};
		npc_shop_sell ->
			<< ItemIndex:8, _B:8, 0:16 >> = Param,
			A = 0,
			{Event, ItemIndex, QuantityOrColor}
	end;

%% @todo Documentation.
%% @todo Probably shouldn't be ignored.
%% @todo We should send the spawn to everyone in this command,
%%       rather than in area_change.
parse(92, 16#010b, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:32, CharGID?l32, 0:64, 0:128,
		CharGID?l32, CharTargetID?l16, _:16, 0:16, Dir?l16,
		X?l32f, Y?l32f, Z?l32f, 0:64,
		QuestID?l32, ZoneID?l16, _:16, MapID?l16, _:16, EntryID?l16, _:16
		>> = Data,
	DestTargetID = CharTargetID,
	ignore; %% @todo character_enter_area

%% @todo Documentation.
parse(60, 16#0110, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:32, CharGID?l32, 0:64, 0:128,
		CharGID?l32, CharTargetID?l16, _:16, EventID?l32, Param?l32
		>> = Data,
	Event = character_eventid_to_atom(EventID),
	case Event of
		character_type_change ->
			{character_type_change, Param};
		character_status_change ->
			{character_status_change, Param};
		Event when Event =/= unknown ->
			Param = 0,
			Event
	end;

parse(52, 16#020b, 2, Data) ->
	<<	16#ffff:16, _:16, 0:128, 0:128,
		Slot:32/little, 0:8, BackToField:8, 0:16
		>> = Data,
	BackToFieldAtom = case BackToField of 0 -> false; 1 -> true end,
	{system_character_select, Slot, BackToFieldAtom};

parse(60, 16#020d, 2, Data) ->
	<<	16#ffff:16, _:16, 0:128, 0:128,
		AuthGID:32/little, AuthKey:32/bits, 0:64
		>> = Data,
	{system_key_auth, AuthGID, AuthKey};

parse(44, 16#0217, 2, Data) ->
	<<	16#ffff:16, _:16, 0:128, 0:128
		>> = Data,
	system_game_server_request;

%% @todo _A _B
parse(100, 16#0219, 2, Data) ->
	<<	16#ffff:16, _:16, 0:128, 0:128,
		Username:192/bits,
		Password:192/bits,
		_A?l32, _B?l32
		>> = Data,
	Username2 = iolist_to_binary(
		re:split(Username, "\\0", [{return, binary}])),
	Password2 = iolist_to_binary(
		re:split(Password, "\\0", [{return, binary}])),
	{system_login_auth, Username2, Password2};

parse(44, 16#021c, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	system_character_load_complete;

parse(48, 16#021d, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		EntryID?l32
		>> = Data,
	unicube_request;

parse(52, 16#021f, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		UniID?l32, EntryID?l32
		>> = Data,
	case UniID of
		0 -> ignore;
		UniID -> {unicube_select, UniID, EntryID}
	end;

%% Same as 023f, except for the odd channel, and that only JP clients use it.
parse(48, 16#0226, 3, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		Page:8, Language:8, 0:16
		>> = Data,
	{system_motd_request, Page, language_to_atom(Language)};

%% Whether the MOTD is accepted.
parse(48, 16#0227, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		AcceptMOTD?l32
		>> = Data,
	ignore;

%% Same as 0226, except for the odd channel, and that only US clients use it.
parse(48, 16#023f, 2, Data) ->
	<<	16#ffff:16, _:16, 0:128, 0:128,
		Page:8, Language:8, 0:16
		>> = Data,
	{system_motd_request, Page, language_to_atom(Language)};

%% @todo Check Size properly.
%% @todo _A
parse(Size, 16#0304, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		CharAccType?l32, CharGID?l32, 0:64,
		ChatType:8, ChatCutIn:8, ChatCutInAngle:8, ChatLength:8,
		ChatChannel:8, ChatCharType:8, 0:8, _A:8,
		CharName:512/bits, ChatMsg/binary
		>> = Data,
	ChatTypeA = chat_type_to_atom(ChatType),
	ChatCutInA = chat_cutin_to_atom(ChatCutIn),
	ChatChannelA = chat_channel_to_atom(ChatChannel),
	ChatCharTypeA = chat_character_type_to_atom(ChatCharType),
	Modifiers = {chat_modifiers, ChatTypeA,
		ChatCutInA, ChatCutInAngle, ChatChannelA, ChatCharTypeA},
	{chat, CharAccType, CharGID, CharName, Modifiers, ChatLength, ChatMsg};

%% @todo AreaNb should be the same that was sent with 0205 apparently.
parse(48, 16#0806, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		AreaNb?l32
		>> = Data,
	ignore;

%% @todo Check if NbAreaChanges is related to AreaNb or something.
parse(60, 16#0807, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		QuestID?l32, ZoneID?l16, MapID?l16, EntryID?l16,
		NbAreaChanges?l16, PartyPos?l32
		>> = Data,
	{area_change, QuestID, ZoneID, MapID, EntryID, PartyPos};

%% @todo AreaNb should be the same that was sent with 0208 apparently.
parse(48, 16#0808, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128, AreaNb?l32
		>> = Data,
	ignore;

parse(648, 16#080c, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		16#ffffffff:32, APCid?l16, _A?l16,
		16#ffffffff:32, 16#ffffffff:32, 0:16, _B?l16,
		0:4736
		>> = Data,
	{apc_force_invite, APCid};

%% @todo Probably indicates a successful area change.
parse(44, 16#080d, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	ignore;

parse(68, 16#080e, 2, Data) ->
	<<	16#ffff:16, _:16, 0:128, 0:128,
		0:8, Language:8, 1:8, Entrance:8, Platform:8, 0:24,
		Revision:8, Minor:4, _A:12, Major:4, _B:4,
		0:96
		>> = Data,
	LanguageA = language_to_atom(Language),
	PlatformA = platform_to_atom(Platform),
	Version = Major * 1000000 + Minor * 1000 + Revision,
	{client_version, LanguageA, Entrance, PlatformA, Version};

%% @todo No idea what this packet is about.
parse(48, 16#080f, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		PartyPos?l32
		>> = Data,
	ignore;

parse(60, 16#0811, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		CounterType:8, 41:8, FromZoneID?l16, FromMapID?l16, FromEntryID?l16,
		CounterID?l32, 16#ffffffff:32
		>> = Data,
	{counter_enter, CounterID, FromZoneID, FromMapID, FromEntryID};

parse(44, 16#0812, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	counter_leave;

parse(52, 16#0813, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		16#ffffffff:32, APCid?l32
		>> = Data,
	{npc_invite, APCid};

%% @todo Probably indicates a successful mission block change.
parse(44, 16#0814, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	ignore;

%% @todo Probably indicates a successful area change.
parse(44, 16#0815, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	ignore;

parse(160, 16#0818, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		GPU:512/bits,
		CPU:384/bits,
		_A?l32
		>> = Data,
	GPU2 = iolist_to_binary(re:split(GPU, "\\0", [{return, binary}])),
	CPU2 = iolist_to_binary(re:split(CPU, "\\0", [{return, binary}])),
	{client_hardware, GPU2, CPU2};

parse(48, 16#0a10, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		ItemID?l32
		>> = Data,
	{item_description_request, ItemID};

parse(48, 16#0c01, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		QuestID?l32
		>> = Data,
	{mission_start, QuestID};

parse(48, 16#0c05, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		CounterID?l32
		>> = Data,
	{counter_quest_files_request, CounterID};

%% On official, Price = Rate * 200.
parse(52, 16#0c07, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		QuestID?l32, Rate?l32
		>> = Data,
	lobby_transport_request;

%% Probably indicates a successful mission block change.
parse(44, 16#0c0d, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	ignore;

parse(44, 16#0c0e, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	mission_abort;

parse(48, 16#0c0f, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		CounterID?l32
		>> = Data,
	{counter_quest_options_request, CounterID};

%% We completely skip the class given and set one depending on the
%% race we received.
%%
%% We completely skip the class levels part as we can't trust what the
%% client tells us, and they should be set to 1 everywhere anyway.
parse(324, 16#0d02, 2, Data) ->
	<<	0:32, 0:128, 0:128,
		Slot?l32,
		NameBin:512/bits,
		Race:8, Gender:8, _:8,
		Infos/bits
		>> = Data,
	Name = iolist_to_binary(
		re:split(NameBin, "\\0\\0", [{return, binary}])),
	RaceAtom = race_to_atom(Race),
	GenderAtom = gender_to_atom(Gender),
	ClassAtom = case RaceAtom of
		human  -> acro;
		newman -> force;
		cast   -> ranger;
		beast  -> hunter
	end,
	Appearance = case RaceAtom of
		cast ->
			<<	VoiceType:8, VoicePitch:8, _:24,
				Torso:32, Legs:32, Arms:32,
				Ears:32, Face:32, HeadType:32,
				MainColor:8, _:32,
				Eyebrows:8, Eyelashes:8, EyesGroup:8, Eyes:8,
				_:24, EyesColorY?l32, EyesColorX?l32,
				_:96, BodyColor?l32, SubColor?l32,
				HairstyleColorY?l32, HairstyleColorX?l32,
				Proportion?l32, ProportionBoxX?l32, ProportionBoxY?l32,
				FaceBoxX?l32, FaceBoxY?l32,
				_/binary
				>> = Infos,
			#{
				arms=Arms,
				body_color=BodyColor,
				ears=Ears,
				eyebrows=Eyebrows,
				eyelashes=Eyelashes,
				eyes_group=EyesGroup,
				eyes=Eyes,
				eyes_color_x=EyesColorX,
				eyes_color_y=EyesColorY,
				face=Face,
				face_box_x=FaceBoxX,
				face_box_y=FaceBoxY,
				hairstyle_color_x=HairstyleColorX,
				hairstyle_color_y=HairstyleColorY,
				head_type=HeadType,
				legs=Legs,
				main_color=MainColor,
				proportion=Proportion,
				proportion_box_x=ProportionBoxX,
				proportion_box_y=ProportionBoxY,
				shield_color=0,
				sub_color=SubColor,
				torso=Torso,
				voice_pitch=VoicePitch,
				voice_type=VoiceType
			};
		RaceAtom ->
			<<	VoiceType:8, VoicePitch:8, _:24,
				Jacket:32, Pants:32, Shoes:32,
				Ears:32, Face:32, Hairstyle:32,
				JacketColor:8, PantsColor:8, ShoesColor:8,
				_:16, Eyebrows:8, Eyelashes:8, EyesGroup:8, Eyes:8,
				BodySuit:8, _:16, EyesColorY?l32, EyesColorX?l32,
				_:96, SkinColor?l32, _:32,
				HairstyleColorY?l32, HairstyleColorX?l32,
				Proportion?l32, ProportionBoxX?l32, ProportionBoxY?l32,
				FaceBoxX?l32, FaceBoxY?l32,
				_/binary
				>> = Infos,
			#{
				blast_badge=0,
				body_suit=BodySuit,
				ears=Ears,
				eyebrows=Eyebrows,
				eyelashes=Eyelashes,
				eyes_group=EyesGroup,
				eyes=Eyes,
				eyes_color_x=EyesColorX,
				eyes_color_y=EyesColorY,
				face=Face,
				face_box_x=FaceBoxX,
				face_box_y=FaceBoxY,
				hairstyle=Hairstyle,
				hairstyle_color_x=HairstyleColorX,
				hairstyle_color_y=HairstyleColorY,
				jacket=Jacket,
				jacket_color=JacketColor,
				lips_color_x=0,
				lips_color_y=32767,
				lips_intensity=32767,
				pants=Pants,
				pants_color=PantsColor,
				proportion=Proportion,
				proportion_box_x=ProportionBoxX,
				proportion_box_y=ProportionBoxY,
				shield_color=0,
				shoes=Shoes,
				shoes_color=ShoesColor,
				skin_color=SkinColor,
				voice_pitch=VoicePitch,
				voice_type=VoiceType
			}
	end,
	validate_new_character(RaceAtom, GenderAtom, Appearance),
	{account_create_character, Slot, Name,
		RaceAtom, GenderAtom, ClassAtom, Appearance};

parse(44, 16#0d06, 2, Data) ->
	<<	0:32, 0:128, 0:128
		>> = Data,
	account_characters_request;

parse(68, 16#0d07, 2, Data) ->
	<<	0:32, 0:128, 0:128,
		TextDisplaySpeed:8, Sound:8, MusicVolume:8, SoundEffectVolume:8,
		Vibration:8, RadarMapDisplay:8, CutInDisplay:8, MainMenuCursorPos:8,
		0:8, Camera3rdY:8, Camera3rdX:8, Camera1stY:8,
		Camera1stX:8, Controller:8, WeaponSwap:8, LockOn:8,
		Brightness:8, FunctionKeySetting:8, 0:8, ButtonDetailDisplay:8,
		0:32
		>> = Data,
	%% Make sure the options are valid.
	true = TextDisplaySpeed =< 1,
	true = Sound =< 1,
	true = MusicVolume =< 9,
	true = SoundEffectVolume =< 9,
	true = Vibration =< 1,
	true = RadarMapDisplay =< 1,
	true = CutInDisplay =< 1,
	true = MainMenuCursorPos =< 1,
	true = Camera3rdY =< 1,
	true = Camera3rdX =< 1,
	true = Camera1stY =< 1,
	true = Camera1stX =< 1,
	true = Controller =< 1,
	true = WeaponSwap =< 1,
	true = LockOn =< 1,
	true = Brightness =< 4,
	true = FunctionKeySetting =< 1,
	true = ButtonDetailDisplay =< 2,
	%% Options are considered safe past this point.
	Options = {options,
		TextDisplaySpeed, Sound, MusicVolume, SoundEffectVolume,
		Vibration, RadarMapDisplay, CutInDisplay, MainMenuCursorPos,
		Camera3rdY, Camera3rdX, Camera1stY, Camera1stX,
		Controller, WeaponSwap, LockOn, Brightness,
		FunctionKeySetting, ButtonDetailDisplay
	},
	{account_set_options, Options};

%% @todo Maybe the first 32+128+128 bits contain useful info?
%% @todo HitNb is an auto incremented hit number.
parse(Size, 16#0e00, 2, Data) ->
	<<	_:288,
		NbHits?l32, PartyPos?l32, HitNb?l32,
		HitsBin/binary
		>> = Data,
	Size = 56 + NbHits * 80,
	Hits = [{hit, FromTargetID, ToTargetID}
		|| <<	X1?l32f, Y1?l32f, Z1?l32f, FromTargetID?l32, ToTargetID?l32,
				_:64,
				_:128, %% probably anim+dir followed by x,y,z
				_:128, %% probably the same
				_:128, _:32,
				Rest/binary >>
		<= HitsBin],
	{hits, Hits};

%% ObjectBaseTargetID is ObjectTargetID - 1024 or 16#ffff.
%% All the ffffffff after PartyPos are other PartyPos values too.
%% @todo Those aren't PartyPos but TargetID of the party.
parse(112, 16#0f0a, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		BlockID?l16, GroupNb?l16, ObjectNb?l16, MapID?l16,
		ObjectID?l16, A?l16, ObjectTargetID?l32,
		ObjectType?l16, 0:16, ObjectBaseTargetID?l16, B?l16,
		PartyPos?l32, C?l32, D?l32, 16#ffffffff:32,
		16#ffffffff:32, 16#ffffffff:32, 0:32, 0:32,
		0:32, ObjectType?l16, EventID:8, NbTargets:8,
		E?l32
		>> = Data,
	case {ObjectType, EventID} of
		{ 5, 13} ->
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_switch_on, ObjectID};
		{ 5, 14} ->
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_switch_off, ObjectID};
		{ 9, 20} ->
			ignore; %% @todo object_sensor_trigger
		{14,  0} ->
			ObjectID = 16#ffff,
			ObjectTargetID = 16#ffffffff,
			ObjectBaseTargetID = 16#ffff,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			{object_warp_enter, BlockID, GroupNb, ObjectNb};
		{22, 12} ->
			A = 134,
			ObjectTargetID = 16#ffffffff,
			ObjectBaseTargetID = 16#ffff,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_key_console_enable, ObjectID};
		{22, 23} ->
			A = 134,
			ObjectTargetID = 16#ffffffff,
			ObjectBaseTargetID = 16#ffff,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_key_console_init, ObjectID};
		{22, 24} ->
			A = 134,
			ObjectTargetID = 16#ffffffff,
			ObjectBaseTargetID = 16#ffff,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_key_console_open_gate, ObjectID};
		{31, 12} ->
			A = 134,
			ObjectTargetID = 16#ffffffff,
			ObjectBaseTargetID = 16#ffff,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_key_enable, ObjectID};
		{48,  4} ->
			A = 134,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_boss_gate_enter, ObjectID};
		{48,  5} ->
			A = 134,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_boss_gate_leave, ObjectID};
		{48,  6} ->
			A = 134,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_boss_gate_activate, ObjectID};
		{48,  7} ->
			A = 134,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			unknown; %% @todo object_boss_gate_??
		{49,  3} ->
			A = 134,
			ObjectTargetID = 16#ffffffff,
			ObjectBaseTargetID = 16#ffff,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_crystal_activate, ObjectID};
		{50,  9} ->
			%% @todo Handle more than one PartyPos.
			A = 134,
			ObjectTargetID = 16#ffffffff,
			ObjectBaseTargetID = 16#ffff,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_healing_pad_tick, [PartyPos]};
		{51,  1} ->
			B = 116,
			C = ObjectTargetID,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_goggle_target_activate, ObjectID};
		{56, 25} ->
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			%% @todo Do we only have the ObjectTargetID here?
			{object_chair_sit, ObjectTargetID};
		{56, 26} ->
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			%% @todo Do we only have the ObjectTargetID here?
			{object_chair_stand, ObjectTargetID};
		{57, 12} ->
			A = 134,
			ObjectTargetID = 16#ffffffff,
			ObjectBaseTargetID = 16#ffff,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			{object_vehicle_boost_enable, ObjectID};
		{57, 28} ->
			A = 134,
			ObjectTargetID = 16#ffffffff,
			ObjectBaseTargetID = 16#ffff,
			B = 116,
			C = 16#ffffffff,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			{object_vehicle_boost_respawn, ObjectID};
		{71, 27} ->
			A = 134,
			C = ObjectTargetID,
			D = 16#ffffffff,
			NbTargets = 1,
			E = 0,
			unknown %% @todo object_trap3_??
	end;

parse(112, 16#1007, 2, Data) ->
	<<	0:32, 0:128, 0:128,
		PartyPos?l32, CharName:512/bits
		>> = Data,
	{party_remove_member, PartyPos};

parse(52, 16#1701, 2, Data) ->
	<<	0:32, 0:128, 0:128,
		0:32, 16#ffffffff
		>> = Data,
	counter_party_list_request;

parse(44, 16#1705, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	counter_party_info_request;

%% @todo Probably needs to be broadcasted to other players in the party.
parse(48, 16#1707, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		QuestID?l32
		>> = Data,
	ignore; %% @todo {counter_quest_select, QuestID}

parse(44, 16#1709, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	counter_party_options_request;

parse(44, 16#170b, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128
		>> = Data,
	counter_background_locations_request;

parse(48, 16#1710, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		CounterID?l32
		>> = Data,
	{counter_options_request, CounterID};

parse(64, 16#1a01, 2, Data) ->
	<<	DestTargetID?l16, _:16, 0:128, 0:128,
		DestTargetID?l16, _:16, ShopID?l32, EventID?l32,
		A?l32, B?l32
		>> = Data,
	Event = dialog_eventid_to_atom(EventID),
	case Event of
		npc_shop_request ->
			A = 0,
			{Event, ShopID};
		lumilass_options_request ->
			ShopID = 0,
			A = 0,
			Event;
		ppcube_request ->
			ShopID = 0,
			A = 0,
			Event;
		ppcube_charge_all ->
			ShopID = 0,
			unknown; %% @todo
		ppcube_charge_one ->
			ShopID = 0,
			unknown; %% @todo
		put_on_outfit ->
			ShopID = 0,
			A = 0,
			unknown; %% @todo
		remove_outfit ->
			ShopID = 0,
			unknown; %% @todo
		player_type_availability_request ->
			ShopID = 0,
			A = 0,
			B = 0,
			Event
	end;

%% @todo Proper cast parsing.
parse(_, Command, 1, Data) ->
	{Command, << 0:32, Command:16, 1:8, 0:8, Data/binary >>}.

%% Data validation.

validate_new_character(cast, male, Appearance) ->
	validate_new_metal_character(Appearance),
	VoiceType = Appearance.voice_type,
	true = (VoiceType >= 27 andalso VoiceType =< 38)
		orelse (VoiceType >= 89 andalso VoiceType =< 96),
	true = Appearance.eyelashes =< 2,
	4 = Appearance.eyes_group,
	true = lists:member(Appearance.torso,
		[16#00F70100, 16#00F90100, 16#00FC0100]),
	true = lists:member(Appearance.legs,
		[16#00F70101, 16#00F90101, 16#00FC0101]),
	true = lists:member(Appearance.arms,
		[16#00F70102, 16#00F90102, 16#00FC0102]),
	case lists:member(Appearance.face, [16#00040004, 16#0A040004,
			16#14040004, 16#1E040004, 16#28040004, 16#000E0004]) of
		true ->
			true = lists:member(Appearance.ears, [16#001E0003,
				16#001F0003, 16#00200003, 16#00210003, 16#00220003]),
			true = Appearance.eyes_color_x =< 327679,
			validate_new_male_hairstyle(Appearance.head_type);
		false ->
			true = lists:member(Appearance.face, [16#00F40104,
				16#00F50104, 16#00F60104, 16#00F70104, 16#00F80104,
				16#00F90104, 16#00FA0104, 16#00FD0104, 16#00020204,
				16#00030204, 16#00040204, 16#00060204, 16#00070204]),
			16#FFFFFFFF = Appearance.ears,
			true = Appearance.eyes_color_x =< 458751,
			true = lists:member(Appearance.head_type, [16#00F40105,
				16#00F50105, 16#00F60105, 16#00F70105, 16#00F80105,
				16#00F90105, 16#00FA0105, 16#00FB0105, 16#00FD0105,
				16#00020205, 16#00030205, 16#00040205, 16#00060205,
				16#00070205])
	end;

validate_new_character(cast, female, Appearance) ->
	validate_new_metal_character(Appearance),
	VoiceType = Appearance.voice_type,
	true = (VoiceType >= 39 andalso VoiceType =< 50)
		orelse (VoiceType >= 97 andalso VoiceType =< 101),
	true = Appearance.eyelashes =< 12,
	5 = Appearance.eyes_group,
	true = lists:member(Appearance.torso,
		[16#00F51100, 16#00F91100, 16#00FA1100]),
	true = lists:member(Appearance.legs,
		[16#00F51101, 16#00F91101, 16#00FA1101]),
	true = lists:member(Appearance.arms,
		[16#00F51102, 16#00F91102, 16#00F61102]),
	case lists:member(Appearance.face, [16#00041004, 16#0A041004,
			16#14041004, 16#1E041004, 16#3C041004]) of
		true ->
			true = lists:member(Appearance.ears, [16#001E1003,
				16#001F1003, 16#00201003, 16#00211003, 16#00221003]),
			true = Appearance.eyes_color_x =< 327679,
			validate_new_female_hairstyle(Appearance.head_type);
		false ->
			true = lists:member(Appearance.face, [16#00F41104,
				16#00F51104, 16#00F61104, 16#00F71104, 16#00F81104,
				16#00F91104, 16#00FA1104, 16#00FD1104, 16#00031204,
				16#00041204, 16#00051204, 16#00061204, 16#00081204]),
			16#FFFFFFFF = Appearance.ears,
			true = Appearance.eyes_color_x =< 458751,
			true = lists:member(Appearance.head_type, [16#00F41105,
				16#00F51105, 16#00F61105, 16#00F71105, 16#00F81105,
				16#00F91105, 16#00FA1105, 16#00FB1105, 16#00FD1105,
				16#00031205, 16#00041205, 16#00051205, 16#00061205,
				16#00081205])
	end;

validate_new_character(human, male, Appearance) ->
	validate_new_fleshy_character(Appearance),
	validate_new_fleshy_male_character(Appearance),
	true = lists:member(Appearance.ears, [16#00000003, 16#00010003]),
	true = lists:member(Appearance.face, [16#00010004, 16#01010004,
		16#14010004, 16#15010004, 16#16010004, 16#17010004, 16#18010004,
		16#19010004, 16#1A010004, 16#1E010004, 16#1F010004, 16#20010004,
		16#21010004, 16#22010004, 16#23010004, 16#24010004, 16#28010004,
		16#29010004, 16#2A010004, 16#2B010004, 16#2C010004, 16#2D010004,
		16#2E010004, 16#000B0004]),
	0 = Appearance.eyes_group,
	true = Appearance.eyes =< 5;

validate_new_character(newman, male, Appearance) ->
	validate_new_fleshy_character(Appearance),
	validate_new_fleshy_male_character(Appearance),
	true = lists:member(Appearance.ears,
		[16#00030003, 16#00650003, 16#00660003]),
	true = lists:member(Appearance.face, [16#00020004, 16#01020004,
		16#14020004, 16#15020004, 16#16020004, 16#17020004, 16#18020004,
		16#19020004, 16#1A020004, 16#1E020004, 16#1F020004, 16#20020004,
		16#21020004, 16#22020004, 16#23020004, 16#24020004, 16#28020004,
		16#29020004, 16#2A020004, 16#2B020004, 16#2C020004, 16#2D020004,
		16#2E020004, 16#000C0004]),
	2 = Appearance.eyes_group,
	true = Appearance.eyes =< 5;

validate_new_character(beast, male, Appearance) ->
	validate_new_fleshy_character(Appearance),
	validate_new_fleshy_male_character(Appearance),
	true = lists:member(Appearance.ears,
		[16#00020003, 16#00CD0003, 16#00CE0003]),
	true = lists:member(Appearance.face, [16#00030004, 16#0A030004,
		16#14030004, 16#15030004, 16#16030004, 16#17030004, 16#18030004,
		16#19030004, 16#1A030004, 16#1E030004, 16#1F030004, 16#20030004,
		16#21030004, 16#22030004, 16#23030004, 16#24030004, 16#28030004,
		16#29030004, 16#2A030004, 16#2B030004, 16#2C030004, 16#2D030004,
		16#2E030004, 16#000D0004]),
	6 = Appearance.eyes_group,
	true = Appearance.eyes =< 6;

validate_new_character(human, female, Appearance) ->
	validate_new_fleshy_character(Appearance),
	validate_new_fleshy_female_character(Appearance),
	true = lists:member(Appearance.ears, [16#00001003, 16#00011003]),
	true = lists:member(Appearance.face, [16#00011004, 16#0A011004,
		16#14011004, 16#1E011004, 16#3C011004]),
	1 = Appearance.eyes_group,
	true = Appearance.eyes =< 5;

validate_new_character(newman, female, Appearance) ->
	validate_new_fleshy_character(Appearance),
	validate_new_fleshy_female_character(Appearance),
	true = lists:member(Appearance.ears,
		[16#00031003, 16#00651003, 16#00661003]),
	true = lists:member(Appearance.face, [16#00021004, 16#0A021004,
		16#14021004, 16#1E021004, 16#3C021004]),
	3 = Appearance.eyes_group,
	true = Appearance.eyes =< 5;

validate_new_character(beast, female, Appearance) ->
	validate_new_fleshy_character(Appearance),
	validate_new_fleshy_female_character(Appearance),
	true = lists:member(Appearance.ears,
		[16#00021003, 16#00CD1003, 16#00CE1003, 16#00CF1003]),
	true = lists:member(Appearance.face, [16#00031004, 16#0A031004,
		16#14031004, 16#1E031004, 16#3C031004]),
	7 = Appearance.eyes_group,
	true = Appearance.eyes =< 6.

validate_new_metal_character(Appearance) ->
	true = Appearance.body_color =< 131071,
	true = Appearance.eyes =< 2,
	true = Appearance.eyes_color_y =< 65535,
	true = Appearance.main_color =< 7,
	true = Appearance.sub_color =< 393215,
	validate_new_common_character(Appearance).

validate_new_fleshy_character(Appearance) ->
	true = Appearance.body_suit =< 4,
	true = Appearance.eyes_color_x =< 327679,
	true = Appearance.jacket_color =< 4,
	true = Appearance.pants_color =< 4,
	true = Appearance.shoes_color =< 4,
	true = Appearance.skin_color =< 131071,
	validate_new_common_character(Appearance).

validate_new_common_character(Appearance) ->
	true = Appearance.eyebrows =< 18,
	true = Appearance.eyes_color_y =< 65535,
	true = Appearance.face_box_x =< 131071,
	true = Appearance.face_box_y =< 131071,
	true = Appearance.hairstyle_color_x =< 327679,
	true = Appearance.hairstyle_color_y =< 65535,
	true = Appearance.proportion =< 131071,
	true = Appearance.proportion_box_x =< 131071,
	true = Appearance.proportion_box_y =< 131071.

validate_new_fleshy_male_character(Appearance) ->
	VoiceType = Appearance.voice_type,
	true = (VoiceType >= 1 andalso VoiceType =< 14)
		orelse (VoiceType >= 76 andalso VoiceType =< 83),
	true = lists:member(Appearance.jacket,
		[16#00060000, 16#00020000, 16#00030000]),
	true = lists:member(Appearance.pants,
		[16#00060001, 16#000B0001, 16#00030001]),
	true = lists:member(Appearance.shoes,
		[16#00060002, 16#00020002, 16#00040002]),
	validate_new_male_hairstyle(Appearance.hairstyle),
	true = Appearance.eyelashes =< 2.

validate_new_fleshy_female_character(Appearance) ->
	VoiceType = Appearance.voice_type,
	true = (VoiceType >= 15 andalso VoiceType =< 26)
		orelse (VoiceType >= 84 andalso VoiceType =< 88),
	true = lists:member(Appearance.jacket,
		[16#00011000, 16#00021000, 16#00031000]),
	true = lists:member(Appearance.pants,
		[16#00011001, 16#00021001, 16#00031001]),
	true = lists:member(Appearance.shoes,
		[16#00091002, 16#00071002, 16#00031002]),
	validate_new_female_hairstyle(Appearance.hairstyle),
	true = Appearance.eyelashes =< 12.

validate_new_male_hairstyle(Hairstyle) ->
	true = lists:member(Hairstyle, [16#00000005, 16#000A0005, 16#00140005,
		16#001E0005, 16#00280005, 16#00320005, 16#003C0005, 16#00460005,
		16#00500005, 16#005A0005, 16#00640005, 16#006E0005, 16#00780005,
		16#00820005, 16#008C0005, 16#00960005, 16#00A00005, 16#00AA0005]).

validate_new_female_hairstyle(Hairstyle) ->
	true = lists:member(Hairstyle, [16#00001005, 16#000A1005, 16#00141005,
		16#001E1005, 16#00281005, 16#00321005, 16#003C1005, 16#00461005,
		16#00501005, 16#005A1005, 16#00641005, 16#006E1005, 16#00781005,
		16#00821005, 16#008C1005, 16#00961005, 16#00A01005]).

%% Response API.
%% @todo We need to be able to optionally output commands sent. How?

%% @doc Send the general data and flags for the selected character.
%% @todo Handle bitflags and value flags properly.
%% @todo Check that DestTargetID is ffff here as it should be.
account_character(Char, State=#egs_net{gid=DestGID, targetid=DestTargetID}) ->
	CharBin = character_to_binary(Char),
	OptionsBin = character_options_to_binary(Char.options),
	send(16#0d01, <<
		DestTargetID?l16, 0:144,
		16#00011300:32, DestGID?l32, 0:64,
		CharBin/binary,
		0:8128, %% bit flags followed directly by value flags
		OptionsBin/binary
	>>, State).

character_options_to_binary(Opts) ->
	Brightness = Opts.brightness,
	ButtonHelp = Opts.buttonhelp,
	Cam1stX = Opts.cam1stx,
	Cam1stY = Opts.cam1sty,
	Cam3rdX = Opts.cam3rdx,
	Cam3rdY = Opts.cam3rdy,
	Controller = Opts.controller,
	CursorPos = Opts.cursorpos,
	CutIn = Opts.cutin,
	FnKeys = Opts.fnkeys,
	LockOn = Opts.lockon,
	MusicVolume = Opts.musicvolume,
	RadarMap = Opts.radarmap,
	SfxVolume = Opts.sfxvolume,
	Sound = Opts.sound,
	TextSpeed = Opts.textspeed,
	Vibration = Opts.vibration,
	WeaponSwap = Opts.weaponswap,
	<<	TextSpeed, Sound, MusicVolume, SfxVolume,
		Vibration, RadarMap, CutIn, CursorPos,
		0, Cam3rdY, Cam3rdX, Cam1stY,
		Cam1stX, Controller, WeaponSwap, LockOn,
		Brightness, FnKeys, 0, ButtonHelp >>.

%% @doc Send the character list for the selection screen.
%% @todo Some values aren't handled yet, like the previous location.
account_characters_response(Characters, State=#egs_net{gid=DestGID}) ->
	[Char1, Char2, Char3, Char4] = Characters,
	Char1Bin = account_character_to_binary(Char1),
	Char2Bin = account_character_to_binary(Char2),
	Char3Bin = account_character_to_binary(Char3),
	Char4Bin = account_character_to_binary(Char4),
	send(16#0d03, <<
		0:32,
		16#00011300:32, DestGID?l32, 0:64,
		16#00011300:32, DestGID?l32, 0:64,
		0:32,
		Char1Bin/binary, Char2Bin/binary,
		Char3Bin/binary, Char4Bin/binary
	>>, State).

%% @todo We shouldn't care about the version *here*, but rather in egs_store.
account_character_to_binary(notfound) ->
	<< 0:2784 >>;
account_character_to_binary({1, Char}) ->
	CharBin = character_to_binary(Char),
	<< 0:8, 1:8, 0:48, CharBin/binary, 0:512 >>.

character_to_binary(notfound) ->
	<< 0:2208 >>;
character_to_binary(Char) ->
	Name = Char.name,
	NameBin = << Name/binary, 0:(512 - bit_size(Name)) >>,
	Race = atom_to_race(Char.race),
	Gender = atom_to_gender(Char.gender),
	Class = atom_to_class(Char.class),
	AppearanceBin = character_appearance_to_binary(Char.race, Char.appearance),
	LevelsBin = character_levels_to_binary(Char),
	<<	NameBin/binary, Race:8, Gender:8, Class:8,
		AppearanceBin/binary, LevelsBin/binary >>.

character_levels_to_binary(Char) ->
	Level = Char.level,
	BlastBar = Char.blast_bar,
	Luck = Char.luck,
	EXP = Char.exp,
	Money = Char.money,
	Playtime = Char.playtime,
	ClassesBin = character_classes_to_binary(Char.type,
		Char.hunter_level, Char.ranger_level,
		Char.force_level, Char.acro_level),
	<<	Level?l32, BlastBar?l16,
		Luck:8, 0:40, EXP?l32, 0:32, Money?l32,
		Playtime?l32, ClassesBin/binary >>.

%% @todo Figure out what the extra values for NPC are.
character_classes_to_binary(player, HunterLv, RangerLv, ForceLv, AcroLv) ->
	<<	0:160,
		1?l32, 1?l32, 1?l32, 1?l32,
		1?l32, 1?l32, 1?l32, 1?l32,
		1?l32, 1?l32, 1?l32, 1?l32,
		HunterLv?l32, RangerLv?l32,
		ForceLv?l32, AcroLv?l32 >>;
character_classes_to_binary(npc, HunterLv, RangerLv, ForceLv, AcroLv) ->
	<<	1?l32, 1?l32, 1?l32, 1?l32,
		1?l32, 1?l32, 1?l32, 1?l32,
		1?l32, 1?l32, 1?l32, 1?l32,
		HunterLv?l32, RangerLv?l32,
		ForceLv?l32, AcroLv?l32,
		16#4e4f4630:32, 16#08000000:32, 0:32, 0:32, 16#4e454e44:32 >>.

character_appearance_to_binary(cast, Appearance) ->
	Arms = Appearance.arms,
	BodyColor = Appearance.body_color,
	Ears = Appearance.ears,
	Eyebrows = Appearance.eyebrows,
	Eyelashes = Appearance.eyelashes,
	EyesColorX = Appearance.eyes_color_x,
	EyesColorY = Appearance.eyes_color_y,
	EyesGroup = Appearance.eyes_group,
	Eyes = Appearance.eyes,
	Face = Appearance.face,
	FaceBoxX = Appearance.face_box_x,
	FaceBoxY = Appearance.face_box_y,
	HairstyleColorX = Appearance.hairstyle_color_x,
	HairstyleColorY = Appearance.hairstyle_color_y,
	HeadType = Appearance.head_type,
	Legs = Appearance.legs,
	MainColor = Appearance.main_color,
	Proportion = Appearance.proportion,
	ProportionBoxX = Appearance.proportion_box_x,
	ProportionBoxY = Appearance.proportion_box_y,
	ShieldColor = Appearance.shield_color,
	SubColor = Appearance.sub_color,
	Torso = Appearance.torso,
	VoicePitch = Appearance.voice_pitch,
	VoiceType = Appearance.voice_type,
	<<	VoiceType:8, VoicePitch:8, 0:24,
		Torso:32, Legs:32, Arms:32, Ears:32, Face:32, HeadType:32,
		MainColor:8, 0:16, ShieldColor:8,
		0:8, Eyebrows:8, Eyelashes:8, EyesGroup:8, Eyes:8,
		0:24, EyesColorY?l32, EyesColorX?l32,
		16#ff7f0000:32, 16#ff7f0000:32, 0:32,
		BodyColor?l32, SubColor?l32,
		HairstyleColorY?l32, HairstyleColorX?l32,
		Proportion?l32, ProportionBoxX?l32, ProportionBoxY?l32,
		FaceBoxX?l32, FaceBoxY?l32 >>;
character_appearance_to_binary(_, Appearance) ->
	Badge = Appearance.blast_badge,
	BodySuit = Appearance.body_suit,
	Ears = Appearance.ears,
	Eyebrows = Appearance.eyebrows,
	Eyelashes = Appearance.eyelashes,
	EyesColorX = Appearance.eyes_color_x,
	EyesColorY = Appearance.eyes_color_y,
	EyesGroup = Appearance.eyes_group,
	Eyes = Appearance.eyes,
	Face = Appearance.face,
	FaceBoxX = Appearance.face_box_x,
	FaceBoxY = Appearance.face_box_y,
	Hairstyle = Appearance.hairstyle,
	HairstyleColorX = Appearance.hairstyle_color_x,
	HairstyleColorY = Appearance.hairstyle_color_y,
	Jacket = Appearance.jacket,
	JacketColor = Appearance.jacket_color,
	LipsColorX = Appearance.lips_color_x,
	LipsColorY = Appearance.lips_color_y,
	LipsIntensity = Appearance.lips_intensity,
	Pants = Appearance.pants,
	PantsColor = Appearance.pants_color,
	Proportion = Appearance.proportion,
	ProportionBoxX = Appearance.proportion_box_x,
	ProportionBoxY = Appearance.proportion_box_y,
	ShieldColor = Appearance.shield_color,
	Shoes = Appearance.shoes,
	ShoesColor = Appearance.shoes_color,
	SkinColor = Appearance.skin_color,
	VoicePitch = Appearance.voice_pitch,
	VoiceType = Appearance.voice_type,
	<<	VoiceType:8, VoicePitch:8, 0:24,
		Jacket:32, Pants:32, Shoes:32, Ears:32, Face:32, Hairstyle:32,
		JacketColor:8, PantsColor:8, ShoesColor:8, ShieldColor:8,
		Badge:8, Eyebrows:8, Eyelashes:8, EyesGroup:8, Eyes:8,
		BodySuit:8, 0:16, EyesColorY?l32, EyesColorX?l32,
		LipsIntensity?l32, LipsColorY?l32, LipsColorX?l32, SkinColor?l32,
		16#ffff0200:32, HairstyleColorY?l32, HairstyleColorX?l32,
		Proportion?l32, ProportionBoxX?l32, ProportionBoxY?l32,
		FaceBoxX?l32, FaceBoxY?l32 >>.

%% @doc Send the defined account flags for the server.
account_flags(ValueFlags, BoolFlags, TempFlags, State=#egs_net{gid=DestGID}) ->
	NbValue = length(ValueFlags),
	NbBool = length(BoolFlags),
	NbTemp = length(TempFlags),
	F = fun(Flag) ->
		FlagBin = list_to_binary(Flag),
		Padding = 8 * (16 - byte_size(FlagBin)),
		<< FlagBin/binary, 0:Padding >>
	end,
	ValueFlagsBin = iolist_to_binary(lists:map(F, ValueFlags)),
	BoolFlagsBin = iolist_to_binary(lists:map(F, BoolFlags)),
	TempFlagsBin = iolist_to_binary(lists:map(F, TempFlags)),
	send(16#0d05, <<
		0:32,
		16#00011300:32, DestGID?l32, 0:64,
		16#00011300:32, DestGID?l32, 0:64,
		ValueFlagsBin/binary, BoolFlagsBin/binary, TempFlagsBin/binary,
		NbValue?l32, NbBool?l32, NbTemp?l32
	>>, State).

%% @doc Send the player's own partner card.
comm_own_card(Char, State=#egs_net{gid=DestGID, targetid=DestTargetID}) ->
	Slot = Char.slot,
	Name = Char.name,
	NameBin = << Name/binary, 0:(512 - bit_size(Name)) >>,
	Race = atom_to_race(Char.race),
	Gender = atom_to_gender(Char.gender),
	Class = atom_to_class(Char.class),
	VoiceType = Char.appearance.voice_type,
	VoicePitch = Char.appearance.voice_pitch,
	Comment = Char.card_comment,
	CommentBin = << Comment/binary, 0:(2816 - bit_size(Comment)) >>,
	send(16#1500, <<
		DestTargetID?l32, 0:144,
		16#00011300:32, DestGID?l32, 0:64,
		NameBin/binary,
		Race:8, Gender:8, Class:8, VoiceType:8, VoicePitch:8, 0:24,
		DestGID?l32, 0:224,
		CommentBin/binary,
		1:8, 4:8, 1:8, Slot:8,
		0:64 >>, State).

%% @doc Display an error to the client.
system_auth_error(Error, State=#egs_net{gid=DestGID}) ->
	Length = byte_size(Error) div 2 + 2,
	send(16#0223, <<
		0:160,
		16#00000f00:32, DestGID?l32, 0:64,
		0:64,
		3?l32, 0:48, Length?l16,
		Error/binary, 0:16
	>>, State).

%% @doc Send the game server's IP and port that the client will connect to.
%% @todo Take IP as a list, not a binary.
system_game_server_response(ServerIP, ServerPort,
		State=#egs_net{gid=DestGID, targetid=DestTargetID}) ->
	send(16#0216, <<
		DestTargetID?l16, 0:16,
		0:128,
		16#00000f00:32, DestGID?l32, 0:64,
		ServerIP/binary, ServerPort?l16, 0:16
	>>, State).

%% @doc Say hello to a newly connected client.
system_hello(State=#egs_net{gid=DestGID, targetid=DestTargetID}) ->
	send(16#0202, << DestTargetID?l16, 0:272, DestGID?l32, 0:1024 >>, State).

%% @doc Send the authentication information for key-based authentication.
system_key_auth_info(AuthGID, AuthKey, State=#egs_net{gid=DestGID}) ->
	send(16#0223, <<
		0:160,
		16#00000f00:32, DestGID?l32, 0:64,
		AuthGID?l32, AuthKey/binary
	>>, State).

%% @doc Send the given MOTD page to the client for display.
%%
%% The full MOTD is expected as this function will only take the
%% page it needs, automatically.
system_motd_response(MOTD, Page, State=#egs_net{targetid=DestTargetID}) ->
	Lines = re:split(MOTD, "\n\\0"),
	NbPages = 1 + length(Lines) div 15,
	true = Page >= 0,
	true = Page < NbPages,
	Text = << << Line/binary, "\n", 0 >>
		|| Line <- lists:sublist(Lines, 1 + Page * 15, 15) >>,
	Length = byte_size(Text) div 2 + 2,
	send(16#0225, <<
		DestTargetID?l16, 0:272,
		NbPages:8, Page:8, Length?l16,
		Text/binary, 0:16
	>>, State).

%% @doc Make the client open the given URL in a browser, after the game closes.
%% @todo Take URL as a list, not a binary.
system_open_url(URL, State=#egs_net{gid=DestGID, targetid=DestTargetID}) ->
	Length = byte_size(URL) + 1,
	Padding = 8 * (512 - Length - 1),
	send(16#0231, <<
		DestTargetID?l16, 0:16,
		16#00000f00:32, DestGID?l32, 0:64,
		16#00000f00:32, DestGID?l32, 0:64,
		Length?l32, URL/binary, 0:Padding
	>>, State).

%% Response primitives.

send(Command, Data, State) ->
	send(Command, 3, Data, State).

%% @todo We may also optionally output packets sent here.
send(Command, Channel, Data, State) ->
	Size = 8 + byte_size(Data),
	{Size2, Padding} = case Size rem 4 of
		0 -> {Size, <<>>};
		2 -> {Size + 2, << 0:16 >>}
	end,
	send_packet(<< Size2?l32, Command:16, Channel:8, 0:8,
		Data/binary, Padding/binary >>, State).

send_packet(Packet, #egs_net{socket=Socket, transport=Transport})
		when byte_size(Packet) =< 16#4000 ->
	Transport:send(Socket, Packet);
send_packet(Packet, State) ->
	send_fragments(Packet, byte_size(Packet), 0, State).

send_fragments(Packet, Size, Current, #egs_net{
		socket=Socket, transport=Transport})
		when Size - Current =< 16#4000 ->
	FragmentSize = 16#10 + byte_size(Packet),
	Transport:send(Socket, << FragmentSize?l32, 16#0b030000:32,
		Size?l32, Current?l32, Packet/binary >>);
send_fragments(Packet, Size, Current, State=#egs_net{
		socket=Socket, transport=Transport}) ->
	<< Fragment:16#4000/binary, Rest/binary >> = Packet,
	Transport:send(Socket, << 16#10400000:32, 16#0b030000:32,
		Size?l32, Current?l32, Fragment/binary >>),
	send_fragments(Rest, Size, Current + 16#4000, State).

%% Data conversion.

atom_to_class(hunter) -> 12;
atom_to_class(ranger) -> 13;
atom_to_class(force ) -> 14;
atom_to_class(acro  ) -> 15.

atom_to_gender(male  ) -> 0;
atom_to_gender(female) -> 1.

atom_to_race(human ) -> 0;
atom_to_race(newman) -> 1;
atom_to_race(cast  ) -> 2;
atom_to_race(beast ) -> 3.

character_eventid_to_atom( 1) -> unknown; %% @todo
character_eventid_to_atom( 2) -> character_type_capabilities_request;
character_eventid_to_atom( 3) -> character_type_change;
character_eventid_to_atom( 4) -> unknown; %% @todo
character_eventid_to_atom( 6) -> unknown; %% @todo
character_eventid_to_atom( 7) -> character_death;
character_eventid_to_atom( 8) -> character_death_return_to_lobby;
character_eventid_to_atom( 9) -> unknown; %% @todo
character_eventid_to_atom(10) -> character_status_change.

chat_type_to_atom(0) -> speak;
chat_type_to_atom(1) -> shout;
chat_type_to_atom(2) -> whisper.

chat_cutin_to_atom( 0) -> none;
chat_cutin_to_atom( 1) -> laugh;
chat_cutin_to_atom( 2) -> smile;
chat_cutin_to_atom( 3) -> wry_smile;
chat_cutin_to_atom( 4) -> surprised;
chat_cutin_to_atom( 5) -> confused;
chat_cutin_to_atom( 6) -> disappointed;
chat_cutin_to_atom( 7) -> deep_in_thought;
chat_cutin_to_atom( 8) -> sneer;
chat_cutin_to_atom( 9) -> dissatisfied;
chat_cutin_to_atom(10) -> angry.

chat_channel_to_atom(0) -> public;
chat_channel_to_atom(1) -> private.

chat_character_type_to_atom(0) -> player;
chat_character_type_to_atom(2) -> apc. %% @todo Check that this is right.

class_to_atom(12) -> hunter;
class_to_atom(13) -> ranger;
class_to_atom(14) -> force;
class_to_atom(15) -> acro.

dialog_eventid_to_atom(0) -> npc_shop_request;
dialog_eventid_to_atom(2) -> lumilass_options_request;
dialog_eventid_to_atom(3) -> ppcube_request;
dialog_eventid_to_atom(4) -> ppcube_charge_all;
dialog_eventid_to_atom(5) -> ppcube_charge_one;
dialog_eventid_to_atom(6) -> put_on_outfit;
dialog_eventid_to_atom(7) -> remove_outfit;
dialog_eventid_to_atom(9) -> player_type_availability_request.

gender_to_atom(0) -> male;
gender_to_atom(1) -> female.

item_eventid_to_atom( 1) -> item_equip;
item_eventid_to_atom( 2) -> item_unequip;
item_eventid_to_atom( 3) -> item_link_pa;
item_eventid_to_atom( 4) -> item_unlink_pa;
item_eventid_to_atom( 5) -> item_drop;
item_eventid_to_atom( 7) -> item_learn_pa;
item_eventid_to_atom( 8) -> item_use;
item_eventid_to_atom( 9) -> item_set_trap;
item_eventid_to_atom(18) -> item_unlearn_pa.

language_to_atom(0) -> japanese;
language_to_atom(1) -> american_english;
language_to_atom(2) -> british_english;
language_to_atom(3) -> french;
language_to_atom(4) -> german;
language_to_atom(5) -> spanish;
language_to_atom(6) -> italian;
language_to_atom(7) -> korean;
language_to_atom(8) -> simplified_chinese;
language_to_atom(9) -> traditional_chinese.

npc_shop_eventid_to_atom(1) -> npc_shop_enter;
npc_shop_eventid_to_atom(2) -> npc_shop_buy;
npc_shop_eventid_to_atom(3) -> npc_shop_sell;
npc_shop_eventid_to_atom(4) -> unknown; %% @todo npc_shop_gift_wrap
npc_shop_eventid_to_atom(5) -> npc_shop_leave;
npc_shop_eventid_to_atom(6) -> unknown. %% @todo

platform_to_atom(0) -> ps2;
platform_to_atom(1) -> pc.

race_to_atom(0) -> human;
race_to_atom(1) -> newman;
race_to_atom(2) -> cast;
race_to_atom(3) -> beast.

%% Debug.

binary_to_dump(Data) ->
	binary_to_dump(Data, 4, []).
binary_to_dump(<<>>, _, Acc) ->
	lists:reverse(Acc);
binary_to_dump(Data, 0, Acc) ->
	binary_to_dump(Data, 4, ["~n"|Acc]);
binary_to_dump(<< A, B, C, D, Rest/binary >>, N, Acc) ->
	Str = io_lib:format("~2.16.0b ~2.16.0b ~2.16.0b ~2.16.0b  ", [A, B, C, D]),
	binary_to_dump(Rest, N - 1, [Str|Acc]).

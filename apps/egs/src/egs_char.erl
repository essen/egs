-module(egs_char).

-export([new/6]).

-include_lib("erlson/include/erlson.hrl").

%% @todo Add the current location for backtopreviousfield
new(Slot, Name, Race, Gender, Class, Appearance) ->
	#{
		type=player,

		slot=Slot,
		name=Name,
		race=Race,
		gender=Gender,
		class=Class,
		appearance=Appearance,

		level=1,
		exp=0,

		hunter_level=1,
		hunter_exp=0,
		ranger_level=1,
		ranger_exp=0,
		force_level=1,
		force_exp=0,
		acro_level=1,
		acro_level=0,

		blast_bar=0,
		luck=3,
		playtime=0,

		%% current_uni,
		%% current location + entryid
		%% previous location + entryid
		%% pids for such

		money=1000000,
		inventory=[],

		card_comment= <<>>,

		options=#{
			brightness=4,
			buttonhelp=0,
			cam1stx=0,
			cam1sty=0,
			cam3rdx=0,
			cam3rdy=0,
			controller=0,
			cursorpos=0,
			cutin=0,
			fnkeys=0,
			lockon=0,
			musicvolume=0,
			radarmap=0,
			sfxvolume=0,
			sound=0,
			textspeed=0,
			vibration=0,
			weaponswap=0
	}}.

%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2010 Loïc Hoguin.
%% @doc Items definitions.
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

%% Shops.

-define(MALE_CLOTHES, [16#09010000, 16#09010100, 16#09010200, 16#09010300, 16#09010400, 16#09010500, 16#09010600, 16#09010700,
	16#09010800, 16#09010900, 16#09010a00, 16#09010b00, 16#09010c00, 16#09010d00, 16#09010e00, 16#09010f00, 16#09012000,
	16#09012100, 16#09012200, 16#09012300, 16#09012400, 16#09012500, 16#09012600, 16#09012700, 16#09012800, 16#09012900,
	16#09020000, 16#09020100, 16#09020200, 16#09020300, 16#09020400, 16#09020500, 16#09020600, 16#09020700, 16#09020800,
	16#09020900, 16#09020a00, 16#09020b00, 16#09021900, 16#09021a00, 16#09021b00, 16#09021c00, 16#09021d00, 16#09021e00,
	16#09021f00, 16#09022000, 16#09022100, 16#09030000, 16#09030100, 16#09030200, 16#09030300, 16#09030400, 16#09030500,
	16#09030600, 16#09030700, 16#09030800, 16#09030900, 16#09030a00, 16#09030b00, 16#09031900, 16#09031a00, 16#09031b00,
	16#09031c00, 16#09031d00, 16#09031e00, 16#09031f00, 16#09032000, 16#09032100, 16#09032200, 16#09040000, 16#09050000,
	16#09050100, 16#09050200, 16#09050300, 16#09050700, 16#09050800, 16#09060000, 16#09060100, 16#09060200, 16#09060300,
	16#09060a00, 16#09060b00, 16#09060c00]).

-define(FEMALE_CLOTHES, [16#09011000, 16#09011100, 16#09011200, 16#09011300, 16#09011400, 16#09011500, 16#09011600, 16#09011700,
	16#09011800, 16#09011900, 16#09011a00, 16#09011b00, 16#09011c00, 16#09011d00, 16#09011e00, 16#09011f00, 16#09012a00,
	16#09012b00, 16#09012c00, 16#09012d00, 16#09012e00, 16#09012f00, 16#09013000, 16#09013100, 16#09013200, 16#09013300,
	16#09020c00, 16#09020d00, 16#09020e00, 16#09020f00, 16#09021000, 16#09021100, 16#09021200, 16#09021300, 16#09021400,
	16#09021500, 16#09021600, 16#09021700, 16#09021800, 16#09022200, 16#09022300, 16#09022400, 16#09022500, 16#09022600,
	16#09022700, 16#09022800, 16#09022900, 16#09030c00, 16#09030d00, 16#09030e00, 16#09030f00, 16#09031000, 16#09031100,
	16#09031200, 16#09031300, 16#09031400, 16#09031500, 16#09031600, 16#09031700, 16#09031800, 16#09032300, 16#09032400,
	16#09032500, 16#09032600, 16#09032700, 16#09032800, 16#09032900, 16#09032a00, 16#09032b00, 16#09032c00, 16#09040100,
	16#09040200, 16#09050400, 16#09050500, 16#09050600, 16#09050900, 16#09050a00, 16#09060400, 16#09060500, 16#09060600,
	16#09060700, 16#09060800, 16#09060d00, 16#09060e00, 16#09060f00, 16#09061000, 16#09061100, 16#09061200]).

-define(MALE_PARTS, [16#0a010000, 16#0a010100, 16#0a010200, 16#0a010300, 16#0a010400, 16#0a010500, 16#0a010600, 16#0a010700,
	16#0a011000, 16#0a011100, 16#0a011200, 16#0a011300, 16#0a011400, 16#0a011500, 16#0a011c00, 16#0a011d00, 16#0a011e00,
	16#0a011f00, 16#0a012000, 16#0a012100, 16#0a012200, 16#0a012300, 16#0a012c00, 16#0a012d00, 16#0a012e00, 16#0a012f00,
	16#0a020000, 16#0a020100, 16#0a020200, 16#0a020300, 16#0a020400, 16#0a020500, 16#0a020600, 16#0a020700, 16#0a021000,
	16#0a021100, 16#0a021200, 16#0a021300, 16#0a021400, 16#0a021500, 16#0a021c00, 16#0a021d00, 16#0a021e00, 16#0a021f00,
	16#0a022000, 16#0a022100, 16#0a022200, 16#0a022300, 16#0a022c00, 16#0a022d00, 16#0a022e00, 16#0a022f00, 16#0a030000,
	16#0a030100, 16#0a030200, 16#0a030300, 16#0a030400, 16#0a030500, 16#0a030600, 16#0a030700, 16#0a031000, 16#0a031100,
	16#0a031200, 16#0a031300, 16#0a031400, 16#0a031500, 16#0a031c00, 16#0a031d00, 16#0a031e00, 16#0a031f00, 16#0a032000,
	16#0a032100, 16#0a032200, 16#0a032300, 16#0a032c00, 16#0a032d00, 16#0a032e00, 16#0a032f00, 16#0a060000, 16#0a060100,
	16#0a060200, 16#0a060300, 16#0a060900]).

-define(FEMALE_PARTS, [16#0a010800, 16#0a010900, 16#0a010a00, 16#0a010b00, 16#0a010c00, 16#0a010d00, 16#0a010e00, 16#0a010f00,
	16#0a011600, 16#0a011700, 16#0a011800, 16#0a011900, 16#0a011a00, 16#0a011b00, 16#0a012400, 16#0a012500, 16#0a012600,
	16#0a012700, 16#0a012800, 16#0a012900, 16#0a012a00, 16#0a012b00, 16#0a013000, 16#0a013100, 16#0a013200, 16#0a013300,
	16#0a020800, 16#0a020900, 16#0a020a00, 16#0a020b00, 16#0a020c00, 16#0a020d00, 16#0a020e00, 16#0a020f00, 16#0a021600,
	16#0a021700, 16#0a021800, 16#0a021900, 16#0a021a00, 16#0a021b00, 16#0a022400, 16#0a022500, 16#0a022600, 16#0a022700,
	16#0a022800, 16#0a022900, 16#0a022a00, 16#0a022b00, 16#0a023000, 16#0a023100, 16#0a023200, 16#0a023300, 16#0a030800,
	16#0a030900, 16#0a030a00, 16#0a030b00, 16#0a030c00, 16#0a030d00, 16#0a030e00, 16#0a030f00, 16#0a031600, 16#0a031700,
	16#0a031800, 16#0a031900, 16#0a031a00, 16#0a031b00, 16#0a032400, 16#0a032500, 16#0a032600, 16#0a032700, 16#0a032800,
	16#0a032900, 16#0a032a00, 16#0a032b00, 16#0a033000, 16#0a033100, 16#0a033200, 16#0a033300, 16#0a060400, 16#0a060500,
	16#0a060600, 16#0a060700, 16#0a060800, 16#0a060a00, 16#0a060b00, 16#0a060c00, 16#0a060d00]).

-define(CONSUMABLES, [16#03010000, 16#03010100, 16#03010200, 16#03010300, 16#03010500, 16#03010600,
	16#03010700, 16#03010900, 16#03010a00, 16#03010b00, 16#03010c00, 16#03010d00, 16#03010e00, 16#03020000,
	16#0c010000, 16#0c010100, 16#0c010200, 16#0c010300, 16#0c010400, 16#0c010500, 16#0c010600, 16#0c010700, 16#0c010800,
	16#0c020000, 16#0c020100, 16#0c020200, 16#0c020300, 16#0c020400, 16#0c020500, 16#0c020600, 16#0c020700, 16#0c020800,
	16#0c020900, 16#0c020a00, 16#0c020b00]).

-define(SHOPS, [
	{515, ?MALE_CLOTHES}, %% Parum vendor.
	{552, ?MALE_CLOTHES}, %% Moatoob vendor.
	{589, ?MALE_CLOTHES}, %% Neudaiz vendor.
	{623, ?MALE_CLOTHES}, %% Colony left vendor.
	{624, ?MALE_CLOTHES}, %% Colony right vendor.

	{518, ?FEMALE_CLOTHES}, %% Parum vendor.
	{555, ?FEMALE_CLOTHES}, %% Moatoob vendor.
	{589, ?FEMALE_CLOTHES}, %% Neudaiz vendor.
	{626, ?FEMALE_CLOTHES}, %% Colony right vendor.
	{627, ?FEMALE_CLOTHES}, %% Colony left vendor.

	{521, ?MALE_PARTS}, %% Parum vendor.
	{558, ?MALE_PARTS}, %% Moatoob vendor.
	{595, ?MALE_PARTS}, %% Neudaiz vendor.
	{629, ?MALE_PARTS}, %% Colony left vendor.
	{630, ?MALE_PARTS}, %% Colony right vendor.

	{524, ?FEMALE_PARTS}, %% Parum vendor.
	{561, ?FEMALE_PARTS}, %% Moatoob vendor.
	{598, ?FEMALE_PARTS}, %% Neudaiz vendor.
	{632, ?FEMALE_PARTS}, %% Colony right vendor.
	{633, ?FEMALE_PARTS}, %% Colony left vendor.

	{512, ?CONSUMABLES}, %% Parum right vendor, Parum v1 field lobbies vendors. Parum GUARDIANS vendor.
	{513, ?CONSUMABLES}, %% Parum left vendor.
	{549, ?CONSUMABLES}, %% Moatoob right vendor, Moatoob v1 field lobbies vendors. Moatoob GUARDIANS vendor.
	{550, ?CONSUMABLES}, %% Moatoob left vendor.
	{586, ?CONSUMABLES}, %% Neudaiz only vendor, Neudaiz v1 field lobbies vendors. Neudaiz GUARDIANS vendor.
	{620, ?CONSUMABLES}, %% Colony right vendor, Colony v1 field lobbies vendors. Colony GUARDIANS vendor.
	{621, ?CONSUMABLES}, %% Colony left vendor.
	{622, ?CONSUMABLES}, %% v2 field lobbies vendors.

	%% @todo The following shops need items.

	{500, [16#03010000]}, %% Parum weapons left vendor.
	{501, [16#03010000]}, %% Parum weapons right vendor.
	{503, [16#03010000]}, %% Parum armors only vendor.
	{506, [16#03010000]}, %% Parum units only vendor.
	{509, [16#03010000]}, %% Parum discs left vendor.
	{510, [16#03010000]}, %% Parum discs center vendor.
	{511, [16#03010000]}, %% Parum discs right vendor.
	{527, [16#03010000]}, %% Parum materials center left vendor.
	{528, [16#03010000]}, %% Parum materials left vendor.
	{532, [16#03010000]}, %% Parum materials center vendor.
	{533, [16#03010000]}, %% Parum materials center right vendor.
	{534, [16#03010000]}, %% Parum materials right vendor.

	{537, [16#03010000]}, %% Neudaiz weapons left vendor.
	{538, [16#03010000]}, %% Neudaiz weapons right vendor.
	{540, [16#03010000]}, %% Neudaiz armors only vendor.
	{543, [16#03010000]}, %% Neudaiz units only vendor.
	{546, [16#03010000]}, %% Neudaiz discs center vendor.
	{547, [16#03010000]}, %% Neudaiz discs left vendor.
	{548, [16#03010000]}, %% Neudaiz discs right vendor.
	{564, [16#03010000]}, %% Neudaiz materials left vendor.
	{565, [16#03010000]}, %% Neudaiz materials center left vendor.
	{569, [16#03010000]}, %% Neudaiz materials right vendor.
	{570, [16#03010000]}, %% Neudaiz materials center right vendor.
	{571, [16#03010000]}, %% Neudaiz materials center vendor.

	{574, [16#03010000]}, %% Neudaiz weapons left vendor.
	{575, [16#03010000]}, %% Neudaiz weapons right vendor.
	{577, [16#03010000]}, %% Neudaiz armors only vendor.
	{580, [16#03010000]}, %% Neudaiz units only vendor.
	{583, [16#03010000]}, %% Neudaiz discs left vendor.
	{584, [16#03010000]}, %% Neudaiz discs center vendor.
	{585, [16#03010000]}, %% Neudaiz discs right vendor.
	{601, [16#03010000]}, %% Neudaiz materials left vendor.
	{602, [16#03010000]}, %% Neudaiz materials center left vendor.
	{606, [16#03010000]}, %% Neudaiz materials center vendor.
	{607, [16#03010000]}, %% Neudaiz materials center right vendor.
	{608, [16#03010000]}, %% Neudaiz materials right vendor.

	{611, [16#03010000]}, %% Colony weapons left vendor.
	{612, [16#03010000]}, %% Colony weapons center vendor.
	{613, [16#03010000]}, %% Colony weapons right vendor.
	{614, [16#03010000]}, %% Colony armors only vendor.
	{617, [16#03010000]}, %% Colony discs right vendor.
	{618, [16#03010000]}, %% Colony discs center vendor.
	{619, [16#03010000]}, %% Colony discs left vendor.
	{635, [16#03010000]}, %% Colony materials right vendor.
	{636, [16#03010000]}, %% Colony materials center right vendor.
	{640, [16#03010000]}, %% Colony materials center left vendor.
	{641, [16#03010000]}, %% Colony materials left vendor.
	{644, [16#03010000]}, %% KUBARA SPREAD.
	{645, [16#03010000]}, %% Colony units left vendor.
	{646, [16#03010000]}, %% Colony units right vendor.
	{648, [16#03010000]}, %% Colony deco right vendor.
	{649, [16#03010000]}, %% Colony deco center vendor.
	{650, [16#03010000]}  %% Colony deco left vendor.
]).

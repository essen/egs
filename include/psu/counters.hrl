%~ -define(COUNTERS, [{0, {counter, colony_bg, "Clyez City\nLinear Line Platform", "Take the Linear\nLine from this\nplatform for\nmissions inside the\nGUARDIANS colony.", [
	%~ {group, "Phantom Ruins", "Special Mission\nParty size:\n  (4+ recommended)\nNo join mid-mission.\nAOTI exclusive", [1060301, 1060303]},
	%~ {group, "Unsafe Passage", "Normal Free Mssn.", [1000000, 1000001, 1000002, 1000003, 1000004, 1000010, 1000011, 1000012, 1000013, 1000014, 1000020, 1000021, 1000022, 1000023, 1000024]}
%~ ]}}]).


%~ text.bin:
%~ 4 bytes: File length
%~ 4 bytes: Pointer to next 4 bytes
%~ 4 bytes: Pointer to list of text strings

%~ List of strings:
%~ 4 bytes: Absolute pointer to string

%~ String:
%~ (x) bytes: Text, unicode format, ending in \0

%~ General order:
%~ Header
%~ List of strings
%~ Strings


%~ counter_to_text(Counter) ->
	

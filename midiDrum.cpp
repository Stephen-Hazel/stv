// midiDrum.cpp - strings for all those dang midi drum #s

#include "midi.h"

MDGrpDef MDGrp [] = {
   {"Kick", 3},
   {"Snar", 9},
   {"HHat", 3},
   {"Cymb", 11},
   {"Toms", 6},
   {"Latn", 22},
   {"Misc", 19},
   {"X",    0}
};
ubyte   NMDGrp = BITS (MDGrp);


MDrumDef MDrum [] = {
   {"2C",  "",    "Kick", 0, "BassDrum1Electric"},
   {"1B",  "",    "Kik2", 0, "BassDrum2Acoustic"},
   {"1A",  "XG",  "KikS", 0, "KickSoft(XG-ONLY)"},

   {"2D",  "",    "Snar", 1, "Snare1Acoustic"},
   {"2E",  "",    "Snr2", 1, "Snare2Electric"},
   {"1F",  "1Db", "SnRl", 1, "SnareRoll(XG)"},
   {"1G",  "1C",  "SnrS", 1, "SnareSoft(XG)"},
   {"1Bb", "XG",  "RimO", 1, "RimShotOpen(XG-ONLY)"},
   {"2Db", "",    "SStk", 1, "SideStick"},
   {"1Ab", "1G",  "Stik", 1, "Sticks(XG)"},
   {"2Eb", "",    "Clap", 1, "HandClap"},
   {"0G",  "1D",  "Snap", 1, "FingerSnap(XG)"},

   {"2Gb", "",    "HHCl", 2, "HiHatClosed"},
   {"2Ab", "",    "HHPd", 2, "HiHatPedal"},
   {"2Bb", "",    "HHOp", 2, "HiHatOpen"},

   {"3Eb", "",    "Ride", 3, "CymbalRide1Edge"},
   {"3B",  "",    "Rid2", 3, "CymbalRide2"},
   {"3F",  "",    "RdBl", 3, "CymbalRideBell"},
   {"3Db", "",    "Cras", 3, "CymbalCrash1"},
   {"3A",  "",    "Cra2", 3, "CymbalCrash2"},
   {"3G",  "",    "Spla", 3, "CymbalSplash"},
   {"3E",  "",    "Chin", 3, "CymbalChinese"},
   {"1Db", "XG",  "BrTp", 3, "BrushTap(XG-ONLY)"},
   {"1D",  "XG",  "BrSw", 3, "BrushSwirl(XG-ONLY)"},
   {"1Eb", "XG",  "BrSl", 3, "BrushSlap(XG-ONLY)"},
   {"1E",  "XG",  "BrTS", 3, "BrushTapSwirl(XG-ONLY)"},

   {"2F",  "",    "TFlL", 4, "TomFloorLo(1)"},
   {"2G",  "",    "TFlH", 4, "TomFloorHi(2)"},
   {"2A",  "",    "TomL", 4, "TomLo(3)"},
   {"2B",  "",    "TMdL", 4, "TomMidLo(4)"},
   {"3C",  "",    "TMdH", 4, "TomMidHi(5)"},
   {"3D",  "",    "TomH", 4, "TomHi(6)"},

   {"4Db", "",    "BonL", 5, "BongoLo"},
   {"4C",  "",    "BonH", 5, "BongoHi"},
   {"4E",  "",    "ConL", 5, "CongaLo"},
   {"4Eb", "",    "ConO", 5, "CongaHiOpen"},
   {"4D",  "",    "ConM", 5, "CongaHiMute"},
   {"4Gb", "",    "TimL", 5, "TimbaleLo"},
   {"4F",  "",    "TimH", 5, "TimbaleHi"},
   {"4Ab", "",    "AgoL", 5, "AgogoLo"},
   {"4G",  "",    "AgoH", 5, "AgogoHi"},
   {"4A",  "",    "Caba", 5, "Cabasa"},
   {"4Bb", "",    "Mara", 5, "Maracas"},
   {"4B",  "",    "WhiS", 5, "WhistleShort"},
   {"5C",  "",    "WhiL", 5, "WhistleLong"},
   {"5D",  "",    "GuiL", 5, "GuiroLong"},
   {"5Db", "",    "GuiS", 5, "GuiroShort"},
   {"5Eb", "",    "Clav", 5, "Claves"},
   {"5F",  "",    "BlkL", 5, "WoodBlockLo"},
   {"5E",  "",    "BlkH", 5, "WoodBlockHi"},
   {"5G",  "",    "CuiL", 5, "CuicaLo"},
   {"5Gb", "",    "CuiH", 5, "CuicaHi"},
   {"0Db", "6D",  "SurM", 5, "SurdoMute(XG)"},
   {"0D",  "6Eb", "SurO", 5, "SurdoOpen(XG)"},

   {"3Ab", "",    "CowB", 6, "Cowbell"},
   {"3Gb", "",    "Tamb", 6, "Tambourine"},
   {"5Ab", "",    "TriM", 6, "TriangleMute"},
   {"5A",  "",    "TriO", 6, "TriangleOpen"},
   {"5B",  "",    "BelJ", 6, "BellJingle"},
   {"6C",  "",    "BelT", 6, "BellTree"},
   {"0Eb", "1Eb", "HiQu", 6, "HighQ(XG)"},
   {"0E",  "1E",  "Slap", 6, "Slap(XG)"},
   {"3Bb", "",    "Vbra", 6, "Vibraslap"},
   {"5Bb", "",    "Shak", 6, "Shaker"},
   {"1Gb", "6Db", "Cast", 6, "Castanets(XG)"},
   {"0F",  "1F",  "ScPs", 6, "ScratchPush(XG)"},
   {"0Gb", "1Gb", "ScPl", 6, "ScratchPull(XG)"},
   {"0Ab", "1Ab", "MtSq", 6, "MetronomeSquare(XG)"},
   {"0A",  "1A",  "MtCl", 6, "MetronomeClick(XG)"},
   {"0Bb", "1Bb", "MtBl", 6, "MetronomeBell(XG)"},
   {"0B",  "0Bb", "ClkL", 6, "ClickLo(XG)"},
   {"1C",  "0B",  "ClkH", 6, "ClickHi(XG)"},
   {"6E",  "",    "Appl", 6, "Applause(GS-ONLY)"}
};
ubyte NMDrum = BITS (MDrum);


ubyte MDrm     (char *s)               // str to key int
{  if (CHDN (*s) == 'x')  return (ubyte)Str2Int (& s [1]);
   for (ubyte i = 0;  i < NMDrum;  i++)  if (! MemCm    (MDrum [i].sym, s, 4))
                                            return MKey (MDrum [i].key);
   return (ubyte)128;                  // eh?
}

char *MDrm2Str (char *s, ubyte b)      // key int to str 'Kik2'
{  *s = '\0';
   for (ubyte i = 0;  i < NMDrum;  i++)  if (MKey (MDrum [i].key) == b)
      {StrCp (s, MDrum [i].sym);   break;}
   if (! *s)  StrFmt (s, "x`03d", b);
   return s;
}

char *MDrm2StG (char *s, ubyte b)      // key int to str w grp  'Drum/Kick_Kik2'
{  StrFmt (s, "Drum/x_`03d", b);       // default it
   for (ubyte i = 0;  i < NMDrum;  i++)  if (MKey (MDrum [i].key) == b)
      {StrFmt (s, "Drum/`s_`s", MDGrp [MDrum [i].grp].sym, MDrum [i].sym);
       break;}
   return s;
}

ubyte MDrm2Grp (ubyte b)               // key int to grp #
{  for (ubyte i = 0;  i < NMDrum;  i++)  if (MKey (MDrum [i].key) == b)
      return MDrum [i].grp;
   return 7;  // x
}


/*
________________________________________________________________________________
XG-Yamaha                              GX-Roland
   2C  Kick BassDrum1Electric             2C  Kick BassDrum1Electric
   1B  Kik2 BassDrum2Acoustic             1B  Kik2 BassDrum2Acoustic
   1A  KikS KickSoft(XG-ONLY)

   2D  Snar Snare1Acoustic                2D  Snar Snare1Acoustic
   2E  Snr2 Snare2Electric                2E  Snr2 Snare2Electric
   1F  SnRl SnareRoll(XG)                 1Db SnRl SnareRoll(GS)
   1G  SnrS SnareSoft(XG)                 1C  SnrS SnareSoft(GS)
   1Bb RimO RimShotOpen(XG-ONLY)

   2Gb HHCl HiHatClosed                   2Gb HHCl HiHatClosed
   2Ab HHPd HiHatPedal                    2Ab HHPd HiHatPedal
   2Bb HHOp HiHatOpen                     2Bb HHOp HiHatOpen

   3Eb Ride CymbalRide1Edge               3Eb Ride CymbalRide1Edge
   3B  Rid2 CymbalRide2                   3B  Rid2 CymbalRide2
   3F  RdBl CymbalRideBell                3F  RdBl CymbalRideBell
   3Db Cras CymbalCrash1                  3Db Cras CymbalCrash1
   3A  Cra2 CymbalCrash2                  3A  Cra2 CymbalCrash2
   3G  Spla CymbalSplash                  3G  Spla CymbalSplash
   3E  Chin CymbalChinese                 3E  Chin CymbalChinese
   1Db BrTp BrushTap(XG-ONLY)
   1D  BrSw BrushSwirl(XG-ONLY)
   1Eb BrSl BrushSlap(XG-ONLY)
   1E  BrTS BrushTapSwirl(XG-ONLY)

   2F  TFlL TomFloorLo(1)                 2F  TFlL TomFloorLo(1)
   2G  TFlH TomFloorHi(2)                 2G  TFlH TomFloorHi(2)
   2A  TomL TomLo(3)                      2A  TomL TomLo(3)
   2B  TMdL TomMidLo(4)                   2B  TMdL TomMidLo(4)
   3C  TMdH TomMidHi(5)                   3C  TMdH TomMidHi(5)
   3D  TomH TomHi(6)                      3D  TomH TomHi(6)

   4Db BonL BongoLo                       4Db BonL BongoLo
   4C  BonH BongoHi                       4C  BonH BongoHi
   4E  ConL CongaLo                       4E  ConL CongaLo
   4Eb ConO CongaHiOpen                   4Eb ConO CongaHiOpen
   4D  ConM CongaHiMute                   4D  ConM CongaHiMute
   4Gb TimL TimbaleLo                     4Gb TimL TimbaleLo
   4F  TimH TimbaleHi                     4F  TimH TimbaleHi
   4Ab AgoL AgogoLo                       4Ab AgoL AgogoLo
   4G  AgoH AgogoHi                       4G  AgoH AgogoHi
   4A  Caba Cabasa                        4A  Caba Cabasa
   4Bb Mara Maracas                       4Bb Mara Maracas
   4B  WhiS WhistleShort                  4B  WhiS WhistleShort
   5C  WhiL WhistleLong                   5C  WhiL WhistleLong
   5D  GuiL GuiroLong                     5D  GuiL GuiroLong
   5Db GuiS GuiroShort                    5Db GuiS GuiroShort
   5Eb Clav Claves                        5Eb Clav Claves
   5F  BlkL WoodBlockLo                   5F  BlkL WoodBlockLo
   5E  BlkH WoodBlockHi                   5E  BlkH WoodBlockHi
   5G  CuiL CuicaLo                       5G  CuiL CuicaLo
   5Gb CuiH CuicaHi                       5Gb CuiH CuicaHi
   0Db SurM SurdoMute(XG)                 6D  SurM SurdoMute(GS)
   0D  SurO SurdoOpen(XG)                 6Eb SurO SurdoOpen(GS)

   3Ab CowB Cowbell                       3Ab CowB Cowbell
   3Gb Tamb Tambourine                    3Gb Tamb Tambourine
   5Ab TriM TriangleMute                  5Ab TriM TriangleMute
   5A  TriO TriangleOpen                  5A  TriO TriangleOpen
   5B  BelJ BellJingle                    5B  BelJ BellJingle
   6C  BelT BellTree                      6C  BelT BellTree
   0Eb HiQu HighQ(XG)                     1Eb HiQu HighQ(GS)
   2Db SStk SideStick                     2Db SStk SideStick
   1Ab Stik Sticks(XG)                    1G  Stik Sticks(GS)
   2Eb Clap HandClap                      2Eb Clap HandClap
   0G  Snap FingerSnap(XG)                1D  Snap FingerSnap(GS)
   0E  Slap Slap(XG)                      1E  Slap Slap(GS)
   3Bb Vbra Vibraslap                     3Bb Vbra Vibraslap
   5Bb Shak Shaker                        5Bb Shak Shaker
   1Gb Cast Castanets(XG)                 6Db Cast Castanets(GS)
   0F  ScPs ScratchPush(XG)               1F  ScPs ScratchPush(GS)
   0Gb ScPl ScratchPull(XG)               1Gb ScPl ScratchPull(GS)
   0Ab MtSq MetronomeSquare(XG)           1Ab MtSq MetronomeSquare(GS)
   0A  MtCl MetronomeClick(XG)            1A  MtCl MetronomeClick(GS)
   0Bb MtBl MetronomeBell(XG)             1Bb MtBl MetronomeBell(GS)
   0B  ClkL ClickLo(XG)                   0Bb ClkL ClickLo(GS)
   1C  ClkH ClickHi(XG)                   0B  ClkH ClickHi(GS)
                                          6E  Appl Applause(GS-ONLY)


Standard:  GM map or XG,GS match
   2C  Kick BassDrum1Electric             4Db BonL BongoLo
   1B  Kik2 BassDrum2Acoustic             4C  BonH BongoHi
                                          4E  ConL CongaLo
   2D  Snar Snare1Acoustic                4Eb ConO CongaHiOpen
   2E  Snr2 Snare2Electric                4D  ConM CongaHiMute
                                          4Gb TimL TimbaleLo
   2Gb HHCl HiHatClosed                   4F  TimH TimbaleHi
   2Ab HHPd HiHatPedal                    4Ab AgoL AgogoLo
   2Bb HHOp HiHatOpen                     4G  AgoH AgogoHi
                                          4A  Caba Cabasa
   3Eb Ride CymbalRide1Edge               4Bb Mara Maracas
   3B  Rid2 CymbalRide2                   4B  WhiS WhistleShort
   3F  RdBl CymbalRideBell                5C  WhiL WhistleLong
   3Db Cras CymbalCrash1                  5D  GuiL GuiroLong
   3A  Cra2 CymbalCrash2                  5Db GuiS GuiroShort
   3G  Spla CymbalSplash                  5Eb Clav Claves
   3E  Chin CymbalChinese                 5F  BlkL WoodBlockLo
                                          5E  BlkH WoodBlockHi
   2F  TFlL TomFloorLo(1)                 5G  CuiL CuicaLo
   2G  TFlH TomFloorHi(2)                 5Gb CuiH CuicaHi
   2A  TomL TomLo(3)
   2B  TMdL TomMidLo(4)                   3Ab CowB Cowbell
   3C  TMdH TomMidHi(5)                   3Gb Tamb Tambourine
   3D  TomH TomHi(6)                      5Ab TriM TriangleMute
                                          5A  TriO TriangleOpen
                                          5B  BelJ BellJingle
                                          6C  BelT BellTree
                                          2Db SStk SideStick
                                          2Eb Clap HandClap
                                          3Bb Vbra Vibraslap
                                          5Bb Shak Shaker


Diffs:  XG versus GS
...any of these means it's definitely XG
   0Db SurM SurdoMute(XG)
   0D  SurO SurdoOpen(XG)
   0Eb HiQu HighQ(XG)
   0E  Slap Slap(XG)
   0F  ScPs ScratchPush(XG)
   0Gb ScPl ScratchPull(XG)
   0G  Snap FingerSnap(XG)
   0Ab MtSq MetronomeSquare(XG)
   0A  MtCl MetronomeClick(XG)

...These mean you gotta listen sigh...
   0Bb MtBl MetronomeBell(XG)             ClkL ClickLo(GS)
   0B  ClkL ClickLo(XG)                   ClkH ClickHi(GS)
   1C  ClkH ClickHi(XG)                   SnrS SnareSoft(GS)
   1Db BrTp BrushTap(XG-ONLY)             SnRl SnareRoll(GS)
   1D  BrSw BrushSwirl(XG-ONLY)           Snap FingerSnap(GS)
   1Eb BrSl BrushSlap(XG-ONLY)            HiQu HighQ(GS)
   1E  BrTS BrushTapSwirl(XG-ONLY)        Slap Slap(GS)
   1F  SnRl SnareRoll(XG)                 ScPs ScratchPush(GS)
   1Gb Cast Castanets(XG)                 ScPl ScratchPull(GS)
   1G  SnrS SnareSoft(XG)                 Stik Sticks(GS)
   1Ab Stik Sticks(XG)                    MtSq MetronomeSquare(GS)
   1A  KikS KickSoft(XG-ONLY)             MtCl MetronomeClick(GS)
   1Bb RimO RimShotOpen(XG-ONLY)          MtBl MetronomeBell(GS)

...any of these means it's definitely GS
   6Db                                    Cast Castanets(GS)
   6D                                     SurM SurdoMute(GS)
   6Eb                                    SurO SurdoOpen(GS)
   6E                                     Appl Applause(GS-ONLY)



MidiNoteNumber = (octave+1)*12 + [C  Db D  Eb E  F  Gb G  Ab A  Bb B]
                                  0  1  2  3  4  5  6  7  8  9  10 11

                 middle C is 4c => (4+1)*12 + 0 => 60
________________________________________________________________________________
*/

// uiKey.cpp

#include "uiKey.h"

typedef struct {int in;   char str [6];   key k;} kmap;

static kmap KMap [] = {
   {Qt::Key_Return,    "ret", RET_KEY},  {Qt::Key_Tab,      "tab", TAB_KEY},
   {Qt::Key_Insert,    "ins", INS_KEY},  {Qt::Key_Delete,   "del", DEL_KEY},
   {Qt::Key_Backspace, "bsp", BSP_KEY},  {Qt::Key_Escape,   "esc", ESC_KEY},
   {Qt::Key_Up,        "up",  UP_KEY},   {Qt::Key_Down,     "dn",  DN_KEY},
   {Qt::Key_Left,      "lft", LFT_KEY},  {Qt::Key_Right,    "rit", RIT_KEY},
   {Qt::Key_PageUp,    "pup", PUP_KEY},  {Qt::Key_PageDown, "pdn", PDN_KEY},
   {Qt::Key_Home,      "hom", HOM_KEY},  {Qt::Key_End,      "end", END_KEY},
   {Qt::Key_Clear,     "clr", CLR_KEY},
   {Qt::Key_Print,     "prt", PRT_KEY},  {Qt::Key_Pause,    "brk", BRK_KEY},

   {Qt::Key_F1, "f01", F01_KEY}, {Qt::Key_F2, "f02", F02_KEY},
   {Qt::Key_F3, "f03", F03_KEY}, {Qt::Key_F4, "f04", F04_KEY},
   {Qt::Key_F5, "f05", F05_KEY}, {Qt::Key_F6, "f06", F06_KEY},
   {Qt::Key_F7, "f07", F07_KEY}, {Qt::Key_F8, "f08", F08_KEY},
   {Qt::Key_F9, "f09", F09_KEY}, {Qt::Key_F10,"f10", F10_KEY},
   {Qt::Key_F11,"f11", F11_KEY}, {Qt::Key_F12,"f12", F12_KEY}
};

key   KeyMap::Map (Qt::KeyboardModifiers mo, int ky)
{ char  c;
  ulong i;
  key   k = 0;
   if ((ky >= ' ') && (ky <= '~')) {   // is it ascii?
      c = SC(char,ky);   if (! mo.testFlag (Qt::ShiftModifier))  c = CHDN (c);
      k = SC(key,c);                   // toss out shift bit (EXCEpt for spc :/)
      if ((c == ' ') && mo.testFlag (Qt::ShiftModifier))  k |= SHF;
   }
   else {
      for (i = 0;  i < BITS (KMap);  i++)  if (ky == KMap [i].in)
         {k = KMap [i].k;   break;}
      if (mo.testFlag (Qt::ShiftModifier))    k |= SHF;
   }
   if (mo.testFlag    (Qt::ControlModifier))  k |= CTL;
   if (mo.testFlag    (Qt::AltModifier))      k |= ALT;
   if (mo.testFlag    (Qt::MetaModifier))     k |= MET;
   return k;
}

char *KeyMap::Str (key k)
{ static char buf [2];
   if (k & 0x80)  return KMap [k & 0x7F].str;
   buf [0] = k & 0x7F;   buf [1] = '\0';   return buf;
}

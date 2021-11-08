//      uiKey.h
#ifndef UIKEY_H
#define UIKEY_H

#include "os.h"
#include <QKeyEvent>

typedef ubyt2 key;

#define SHF      SC(key,0x8000)
#define CTL      SC(key,0x4000)
#define ALT      SC(key,0x2000)
#define MET      SC(key,0x1000)

#define RET_KEY  SC(key,0x80)
#define TAB_KEY  SC(key,0x81)
#define INS_KEY  SC(key,0x82)
#define DEL_KEY  SC(key,0x83)
#define BSP_KEY  SC(key,0x84)
#define ESC_KEY  SC(key,0x85)
#define UP_KEY   SC(key,0x86)
#define DN_KEY   SC(key,0x87)
#define LFT_KEY  SC(key,0x88)
#define RIT_KEY  SC(key,0x89)
#define PUP_KEY  SC(key,0x8A)
#define PDN_KEY  SC(key,0x8B)
#define HOM_KEY  SC(key,0x8C)
#define END_KEY  SC(key,0x8D)
#define CLR_KEY  SC(key,0x8E)
#define PRT_KEY  SC(key,0x8F)
#define BRK_KEY  SC(key,0x90)
#define F01_KEY  SC(key,0x91)
#define F02_KEY  SC(key,0x92)
#define F03_KEY  SC(key,0x93)
#define F04_KEY  SC(key,0x94)
#define F05_KEY  SC(key,0x95)
#define F06_KEY  SC(key,0x96)
#define F07_KEY  SC(key,0x97)
#define F08_KEY  SC(key,0x98)
#define F09_KEY  SC(key,0x99)
#define F10_KEY  SC(key,0x9A)
#define F11_KEY  SC(key,0x9B)
#define F12_KEY  SC(key,0x9C)

class KeyMap {
public:
   KeyMap ()  {}
   key   Map (Qt::KeyboardModifiers mo, int ky);
   char *Str (key);
   QKeySequence UnStr (char *s);
};

#endif

// midi.h - deal with the midi i/o stuph...

#ifndef MIDI_H
#define MIDI_H

#include "timer.h"                     // os.h incl by parent
#include "alsa/asoundlib.h"
#include <QThread>


const ubyte MAX_DEV = 40;              // max i+o devs we can deal with

const ubyt4 MAX_DSC = 256;             // .song file max recs (etc)
const ubyt4 MAX_TRK = 128;
const ubyt4 MAX_DRM = 128;
const ubyt4 MAX_LYR = 32768;
const ubyt4 MAX_EVT = 512000;

const ubyte M_NOFF    = 0x80;  // 3       std midi opcode defs (status)
const ubyte M_NOTE    = 0x90;  // 3
const ubyte M_NPRS    = 0xA0;  // 3
const ubyte M_CTRL    = 0xB0;  // 3
const ubyte M_PROG    = 0xC0;  // 2
const ubyte M_PRSS    = 0xD0;  // 2
const ubyte M_PBND    = 0xE0;  // 3

const ubyte M_SYSX    = 0xF0;
const ubyte M_EOX     = 0xF7;
                                       // i rarely if ever use these
const ubyte M_QFRAME  = 0xF1;          // +1 byte  midi time code quarter frame
const ubyte M_SONGPOS = 0xF2;          // +2 bytes like pbnd
const ubyte M_SONGSEL = 0xF3;          // +1 byte    (no F4,F5)
const ubyte M_TUNEREQ = 0xF6;          // no extra bytes
const ubyte M_CLOCK   = 0xF8;          // no bytes   (pesky)
const ubyte M_TICK    = 0xF9;          // no bytes   (not in midi 1.0?)
const ubyte M_START   = 0xFA;          // no "
const ubyte M_CONT    = 0xFB;          // no
const ubyte M_STOP    = 0xFC;          // no "       (no FD)
const ubyte M_SENSE   = 0xFE;          // no         (pesky)
const ubyte M_RESET   = 0xFF;          // no

const ubyte M_BANK  = 0;               // the more popular midi CCs
const ubyte M_BNKL  = 32;
const ubyte M_MOD   = 1;
const ubyte M_BRTH  = 2;
const ubyte M_PEDL  = 4;
const ubyte M_VOL   = 7;
const ubyte M_EXPR  = 11;
const ubyte M_PAN   = 10;
const ubyte M_BAL   = 8;
const ubyte M_HOLD  = 64;
const ubyte M_HLD2  = 69;
const ubyte M_SOFT  = 67;
const ubyte M_SUST  = 66;
const ubyte M_LEGA  = 68;
const ubyte M_RVRB  = 91;
const ubyte M_CHOR  = 93;
const ubyte M_ASOFF = 120;             // all sound off
const ubyte M_ACOFF = 121;             // reset all controls
const ubyte M_LOCAL = 122;             // 0 means local control off;  127=on
const ubyte M_ANOFF = 123;             // all notes off

const ubyte M_NRPNL = 98;              // to do Non Registered Params via M_CTRL
const ubyte M_NRPNH = 99;
const ubyte M_RPNL  = 100;             // to do Registered Params via M_CTRL
const ubyte M_RPNH  = 101;
const ubyte M_DATH  = 6;               // to set one of these ctrl's value
const ubyte M_DATL  = 38;

const ubyte M_RP_PBRN = 0;
const ubyte M_RP_TUNE = 4;
const ubyte M_RP_TUNL = 5;

// my mapping of raw midi - into ubyt2 .ctrl
// nt  0..127       note
// std 128          prog
//     129          prss
//     130          pbnd
//     131          tmpo               // ...never go out midi
//     132          tsig
//     133          ksig
//     134..255         (hole)
// cc  256..383     cc  (hole for datahi,datalo,nrpnlo,nrpnhi,rpnlo,rpnhi)
// us  384..16383   univSysEx (mainly outbound only?)
// rp  16384,16385  pbRngStep(0-127), pbRngCent(0-99)
//     16386,16387  tuneFine(hi*128+lo => +-8192 cent)
//     16388,16389  tune(+-64 step)   -- 137 unused
//     16390,16391  tuneProg  (rare)
//     16392,16393  tuneBank  (rare)
//     16394,16395  modDepthRng  (??)
//     16396..32767 rpn6..8191 -- if ever used
// np  32768..65535 nrpn  (hole for NULLhi,lo - 65534,65535)
//                  (16k*2 for hi,lo)
const ubyt2 MC_PROG = 0x0080;          // std midi control mapping area
const ubyt2 MC_PRSS = 0x0081;
const ubyt2 MC_PBND = 0x0082;
const ubyt2 MC_TMPO = 0x0083;          // just for Txt2Song/Mid2Song
const ubyt2 MC_TSIG = 0x0084;
const ubyt2 MC_KSIG = 0x0085;

const ubyt2 MC_CC   = 0x0100;          // control change mapping area
const ubyt2 MC_US   = 0x0180;          // universal sysex
const ubyt2 MC_RP   = 0x4000;          // registered params
const ubyt2 MC_NP   = 0x8000;          // nonregistered params

const ubyt2 MC_PBNR = MC_RP|0;         // pbend range (halfsteps only)
const ubyt2 MC_TUNE = MC_RP|4;         // tune (halfsteps signed)
const ubyt2 MC_MVOL = MC_US|0;         // master volume
const ubyt2 MC_MBAL = MC_US|2;         // master balance


// raw midi ev w ubyt2 ctrl and chan (comin from a midiin)
typedef struct {ubyt4 time,  msec;
                ubyt2 ctrl;  ubyte valu, val2, chan;}  MidiEv;

struct MidiDevLst {
public:                                // only MidiCfg app should reach in herez
   struct {char io;   TStr name, type, desc, dev;}  _lst [MAX_DEV];
   ubyte                                            _len;
   void Load (), Dump ();
// get the dev by name (can't be type=OFF)
   bool Get    (char io, char *name, char *type, char *desc, char *dev);
// rifle thru whole list by pos including type=OFF devs
   bool GetPos (char io, ubyte pos,
                         char *name, char *type, char *desc, char *dev);
private:
   void InsDev (char io, char *dev, char *desc);
   static char *DoRec (char *buf, ubyt2 len, ubyt4 pos, void *ptr);
};
extern MidiDevLst Midi;


// MCC stuff...
struct MCCRow {TStr s;  char typ;  ubyt2 dflt, raw;};
extern MCCRow MCC [];
extern ubyte NMCC;
void MCCDump ();


// convert absolute time (int 2 str and back) ignoring any timesig stuff
char *TmS (char *s, ubyt4 t);
ubyt4 TmI (char *s);


//______________________________________________________________________________
class MidiI: public QThread {
   Q_OBJECT

   void run () override;

public:
   MidiI (char *name, Timer *tmr = nullptr);
  ~MidiI ();
   bool  Dead ()  {return (_hnd == nullptr) ? true : false;}
   char *Name ()  {return _name;}
   char *Type ()  {return _type;}
   char *Desc ()  {return _desc;}
   char *Dev  ()  {return _dev;}
   bool  Get  (MidiEv *ev);
   void  SetTmr (Timer *t)  {_timer = t;}
   void  BufAdj (sbyt4 tm);

public slots:
   void EvIns (ubyte s, ubyte c, ubyte v, ubyte v2);

signals:
   void Event (ubyte s, ubyte c, ubyte v, ubyte v2);
   void MidiIEv ();

private:
   snd_rawmidi_t *_hnd;
   TStr   _name, _type, _desc, _dev;
   Timer *_timer;
   ubyt2  _rpn;
   MidiEv _buf [128];
   ubyte  _bAdd, _bRmv;
   bool   _bErr, _run;
};


class MidiO {
public:
   MidiO (char *name, char noinit = '\0');
  ~MidiO ();
   bool  Dead ()  {return (_hnd == nullptr) ? true : false;}
   char *Name ()  {return _name;}
   char *Type ()  {return _type;}
   char *Desc ()  {return _desc;}
   char *Dev  ()  {return _dev;}
   void  NotesOff (),  DumpOns ();
   void  GMInit ();
   void  PutMEv (ubyte *mev, ubyte len = 3);
   void  Put    (ubyte chan, ubyt2 ctrl, ubyte valu = 0, ubyte val2 = 0);
   ubyte _MVol, _MBal;
private:
   snd_rawmidi_t *_hnd;
   TStr  _name, _type, _desc, _dev;
   ubyt4 _ntOn [16*4];
};


// my TrkEv format...
typedef struct {ubyt4 time;  ubyte ctrl, valu, val2, x;} TrkEv;

// ctrl - hi bit clear for noteOn/Prs/Off;  set for controls
// for notes: on  - valu hi bit set, val2 hi bit clear, val2 bits0-4=fingering
//            prs - valu hi bit set, val2 hi bit set  (some day?)
//            off - valu hi bit clear
// for ctrls: usually just use valu, some (PBnd) use val2
// Tmpo - =1..32767;   =valu|val2<<8 (vals are 0..255, not 0..127)
// TSig - =num/den[/subBt 1 default]
//        numer=valu, denom=1<<(val2 & 0x0F), subBt=val2>>4+1
// KSig - =key[m] with PROPER #/b if non cdefgab  WATCH OUT FER B = Cb !!
//        valu=key(0-11), val2=hi bit means spelled b (else #);  lo bit=minor
// Prog - =*     valu,val2=0  (always)                             else major

#define ECTRL(e)  ((e)->ctrl & 0x80)
#define EDOWN(e)  ((e)->valu & 0x80)
#define EPRSS(e)  ((e)->val2 & 0x80)
#define ENOTE(e)  (! ECTRL(e))         // PAY ATTENTION !!
#define EUP(e)    (! EDOWN(e))         // must be used SEQUENTIALLY !!
#define EDN(e)    (! EPRSS(e))         // or ...faaailure
// if checkin all, do...  if      ECTRL {got ctrl}
//                        else if EUP   {got NtUp}
//                        else if EDN   {got NtDn}
//                        else          {got NPrs}
#define ENTDN(e)  (ENOTE(e) && EDOWN(e) && (! EPRSS(e)))
#define ENPRS(e)  (ENOTE(e) && EDOWN(e) &&    EPRSS(e) )
#define ENTUP(e)  (ENOTE(e) && EUP(e))

#define MCTRL(m)  ((m)->ctrl & 0xFF80)      // MidiEv is a lil diff cuz
#define MDOWN(m)  ((m)->valu & 0x80)        // ctrl is ubyt2
#define MPRSS(m)  ((m)->val2 & 0x80)
#define MNOTE(m)  (! MCTRL(m))
#define MUP(m)    (! MDOWN(m))
#define MDN(m)    (! MPRSS(m))
#define MNTDN(m)  (MNOTE(m) && MDOWN(m) && (! MPRSS(m)))
#define MNPRS(m)  (MNOTE(m) && MDOWN(m) &&    MPRSS(m) )
#define MNTUP(m)  (MNOTE(m) && MUP(m))


//______________________________________________________________________________
extern ubyte MNt      (char *s);
extern ubyte MKey     (char *s, char **news = nullptr);
extern char *MKey2Str (char *s, ubyte b, char fl = '#');

typedef struct {char sym [5];  ubyte nDr;}  MDGrpDef;
typedef struct {char key [4], gs [4], sym [5];   ubyte grp;  WStr dsc;}
                                            MDrumDef;
extern char     MKeyStr [12][3], MKeyStrB [12][3];
extern char    *MProg [128];
extern char     MFing [31][3];
extern MDGrpDef MDGrp [];
extern ubyte   NMDGrp;
extern MDrumDef MDrum [];
extern ubyte   NMDrum;

extern ubyte MDrm     (char *s);            // str to key
extern char *MDrm2Str (char *s, ubyte b);   // key to str
extern char *MDrm2StG (char *s, ubyte b);   // key to Drum\grp\str
extern ubyte MDrm2Grp (ubyte b);            // key to grp #

extern ubyt2 MCtl     (char *s);
extern char *MCtl2Str (char *s, ubyt2 c, char raw = '\0');
extern char *MNt2Str  (char *s, MidiEv *e);
extern char *CtlX2Str (char *s, char *cs, TrkEv *in);
extern void  CtlX2Val (TrkEv *e, char *cs, char *s);

// MidiChd.cpp stuff...
// for chord calcs  chordType label, offset set, use in Calc, yamaha chord ID#
struct MChdDef {char const *lbl;  char tmp [8];  
                char calc;  ubyte yId;  char const *etc;};
const  ubyte   MAJ_CHD = 3;            // 0=off,1=oct,2=5,3=maj
extern MChdDef MChd [];
extern ubyte  NMChd;
extern ubyte    ChdPos (char *chd, ubyte *root = nullptr, ubyte *bass = nullptr,
                                                           char *sty = nullptr);

#endif  // MIDI

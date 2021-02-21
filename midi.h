// midi.h - deal with the midi i/o stuph...

#ifndef MIDI_H
#define MIDI_H

#include <os.h>

const ubyte MAX_DEV = 40;              // max i+o devs we can deal with

const ulong MAX_DSC = 256;             // .song file max recs (etc)
const ulong MAX_TRK = 128;
const ulong MAX_DRM = 128;
const ulong MAX_LYR = 32768;
const ulong MAX_EVT = 512000;

const ubyte M_NOFF    = 0x80;  // 3       std midi opcode defs
const ubyte M_NOTE    = 0x90;  // 3
const ubyte M_NPRS    = 0xA0;  // 3
const ubyte M_CTRL    = 0xB0;  // 3
const ubyte M_PROG    = 0xC0;  // 2
const ubyte M_PRSS    = 0xD0;  // 2
const ubyte M_PBND    = 0xE0;  // 3

const ubyte M_SYSX    = 0xF0;
const ubyte M_EOX     = 0xF7;
                                       // i rarely if ever use these
const ubyte M_QFRAME  = 0xF1;          // +1 byte
const ubyte M_SONGPOS = 0xF2;          // +2 bytes like pbnd
const ubyte M_SONGSEL = 0xF3;          // +1 byte    (no F4,F5)
const ubyte M_TUNEREQ = 0xF6;          // no extra bytes
const ubyte M_CLOCK   = 0xF8;          // no bytes   (pesky)
const ubyte M_TICK    = 0xF9;          // no bytes
const ubyte M_START   = 0xFA;          // no "
const ubyte M_CONT    = 0xFB;          // no
const ubyte M_STOP    = 0xFC;          // no "       (no FD)
const ubyte M_SENSE   = 0xFE;          // no         (pesky)
const ubyte M_RESET   = 0xFF;          // no

const ubyte M_C  = 0;                  // to code note numbers
const ubyte M_Cs = 1;
const ubyte M_Db = 1;
const ubyte M_D  = 2;
const ubyte M_Ds = 3;
const ubyte M_Eb = 3;
const ubyte M_E  = 4;
const ubyte M_F  = 5;
const ubyte M_Fs = 6;
const ubyte M_Gb = 6;
const ubyte M_G  = 7;
const ubyte M_Gs = 8;
const ubyte M_Ab = 8;
const ubyte M_A  = 9;
const ubyte M_As = 10;
const ubyte M_Bb = 10;
const ubyte M_B  = 11;
const ubyte M_Cb = 11;
#define M_NT(n,o)  (ubyte)((n)+(o+1)*12)

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

// my mapping of raw midi - into uword .ctrl
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
const uword MC_PROG = 0x0080;          // std midi control mapping area
const uword MC_PRSS = 0x0081;
const uword MC_PBND = 0x0082;
const uword MC_TMPO = 0x0083;          // just for Txt2Song/Mid2Song
const uword MC_TSIG = 0x0084;
const uword MC_KSIG = 0x0085;

const uword MC_CC   = 0x0100;          // control change mapping area
const uword MC_US   = 0x0180;          // universal sysex
const uword MC_RP   = 0x4000;          // registered params
const uword MC_NP   = 0x8000;          // nonregistered params

const uword MC_PBNR = MC_RP|0;         // pbend range (halfsteps only)
const uword MC_TUNE = MC_RP|4;         // tune (halfsteps signed)
const uword MC_MVOL = MC_US|0;         // master volume
const uword MC_MBAL = MC_US|2;         // master balance


// raw midi ev w uword ctrl and chan (comin from a midiin)
typedef struct {ulong time,  msec;
                uword ctrl;  ubyte valu, val2, chan;}  MidiEv;

struct MidiDevLst {
public:
   void Load ();
// get the dev by name (can't be type=OFF)
   bool Get    (char io,            char *name, char *type, char *desc);
// rifle thru whole list by pos including type=OFF devs
   bool GetPos (char io, ubyte pos, char *name, char *type, char *desc);
private:
   static char *DoRec (char *buf, uword len, ulong pos, void *ptr);
   struct {char io;   TStr name, type, ccmap, desc;}  _lst [MAX_DEV];
   ubyte                                              _len;
};
extern MidiDevLst Midi;


// MCC stuff...
struct MCCRow {TStr s;  char typ;  uword dflt, raw;};
extern MCCRow MCC [];
extern ubyte NMCC;
void MCCDump ();


// convert absolute time (int 2 str and back) ignoring any timesig stuff
char *TmS (char *s, ulong t);
ulong TmI (char *s);
                               // len

class MidiI {
public:
   MidiI (char *name, Timer *tmr = NULL, HWND wnd = NULL, char raw = '\0');
  ~MidiI ();
   char *Name   ()          {return _name;}
   char *Type   ()          {return _type;}
   char *Map    ()          {return _map;}
   char *Desc   ()          {return _desc;}
   ubyte MmId   ()          {return _mmId;}
   void  SetTmr (Timer *t)  {_timer = t;}
   bool  Get    (MidiEv *ev);
   void  BufAdj (slong tm);
   void  GetSx  (ubyte *strm, ulong len);
   void  SxBuf  (ubyte num, ulong siz, ubyte *p);
   void  SxRet  (ubyte *p);
   bool  Dead   ()          {return (_hnd == NULL) ? true : false;}
private:
   static void CALLBACK Handler (HMIDIIN h, UINT msg, DWORD user,
                                 DWORD l1, DWORD l2);
   void Chk (UINT err, char *f, char x = '\0');
   TStr       _name, _type, _desc, _map;
   ubyte      _mmId;
   MIDIINCAPS _cap;
   HMIDIIN    _hnd;
   MIDIHDR    _hdr, _hdrB [10];
   bool       _hdrUsed, _hdrDone;
   ubyte      _numB;
   ulong      _sizB;
   DWORD      _sigThrd;
   HWND       _sigWndo;
   Timer     *_timer;
   uword      _rpn;
   MidiEv     _buf [128];
   ubyte      _bAdd, _bRmv;
   bool       _bErr;
   char       _raw;
   char       _syX;
   ulong      _tmX;
};


class MidiO {
public:
   MidiO (char *name, bool noreset = false);
  ~MidiO ();
   bool  Dead   ()  {return ((_hnd == NULL) && (! _syn)) ? true : false;}
   char *Name   ()  {return _name;}
   char *Type   ()  {return _type;}
   char *Desc   ()  {return _desc;}
   ubyte MmId   ()  {return _mmId;}
   ubyte Syn    ()  {return _syn;}
   void  PutSh  (ubyte *sh);
   void  NotesOff (),  DumpOns ();
   void  GMInit ();
   void  PutMEv (ubyte *mev);
   void  Put    (ubyte chan, uword ctrl, ubyte valu = 0, ubyte val2 = 0);
   void  PutSx  (ubyte *strm, ulong len);
   ubyte _MVol, _MBal;
private:
   static void CALLBACK Handler (HMIDIOUT h, UINT msg, DWORD user,
                                 DWORD l1, DWORD l2);
   void Chk (UINT err, char *f);
   TStr        _name, _type, _desc;
   ubyte       _syn;
   ubyte       _mmId;
   MIDIOUTCAPS _cap;
   HMIDIOUT    _hnd;
   MIDIHDR     _hdr;
   bool        _noSx, _hdrUsed, _hdrDone;
   DWORD       _sigThrd, _synThrd;
   ulong       _ntOn [16*4];
   bool        _noRs;                  // midiOutReset can wreck local control
};


// my TrkEv format...
typedef struct {ulong time;  ubyte ctrl, valu, val2, x;} TrkEv;

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
#define MDOWN(m)  ((m)->valu & 0x80)        // ctrl is uword
#define MPRSS(m)  ((m)->val2 & 0x80)
#define MNOTE(m)  (! MCTRL(m))
#define MUP(m)    (! MDOWN(m))
#define MDN(m)    (! MPRSS(m))
#define MNTDN(m)  (MNOTE(m) && MDOWN(m) && (! MPRSS(m)))
#define MNPRS(m)  (MNOTE(m) && MDOWN(m) &&    MPRSS(m) )
#define MNTUP(m)  (MNOTE(m) && MUP(m))


// MidiStr.cpp stuff...
typedef struct {char sym [5];  ubyte nDr;}  MDGrpDef;
typedef struct {ubyte ctl, gs;  char sym [5];  ubyte grp;  WStr dsc;}  MDrumDef;

extern char     MKeyStr [12][3], MKeyStrB [12][3];
extern char    *MProg [128];
extern char     MFing [31][3];
extern MDGrpDef MDGrp [];
extern ubyte   NMDGrp;
extern MDrumDef MDrum [];
extern ubyte   NMDrum;

extern ubyte MDrm2Int (char *s);            // str to ctl
extern char *MDrm2Str (char *s, ubyte b);   // ctl to str
extern char *MDrm2StG (char *s, ubyte b);   // ctl to Drum\grp\str
extern ubyte MDrm2Grp (ubyte b);            // ctl to grp #
extern ubyte MNt2Int  (char *s);
extern ubyte MKey2Int (char *s, char **news = NULL);
extern char *MKey2Str (char *s, ubyte b, char fl = '#');
extern uword MCtl2Int (char *s);
extern char *MCtl2Str (char *s, uword c, char raw = '\0');
extern char *MNt2Str  (char *s, MidiEv *e);
extern char *CtlX2Str (char *s, char *cs, TrkEv *in);
extern void  CtlX2Val (TrkEv *e, char *cs, char *s);


// MidiChd.cpp stuff...

// for chord calcs  chordType label, offset set, use in Calc, yamaha chord ID#
struct MChdDef {char *lbl;  char tmp [8];  char calc; ubyte yId; char *etc;};
const  ubyte   MAJ_CHD = 3;            // 0=off,1=oct,2=5,3=maj
extern MChdDef MChd [];
extern ubyte  NMChd;
extern ubyte    ChdPos (char *chd, ubyte *root = NULL, ubyte *bass = NULL,
                                                        char *sty = NULL);

#endif  // MIDI

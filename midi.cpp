// midi.cpp


#include <midi.h>
#include <midiDrum.cpp>                // eh, just always...

char MKeyStr  [12][3] =
        {"c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"},
     MKeyStrB [12][3] =
        {"c", "db", "d", "eb", "e", "f", "gb", "g", "ab", "a", "bb", "b"};

char MFing [31][3] = {
   "1",  "2",  "3",  "4",  "5",
         "12", "13", "14", "15",
   "21",       "23", "24", "25",
   "31", "32",       "34", "35",
   "41", "42", "43",       "45",
   "51", "52", "53", "54",
   "lh", "rh", "lf", "rf", ">", "v"
};


// MidiDevLst ------------------------------------------------------------------
MidiDevLst Midi;

char *MidiDevLst::DoRec (char *buf, uword len, ulong pos, void *ptr)
{ static char spot;
  static TStr err;
  MidiDevLst *md = (MidiDevLst *)ptr;
  ubyte       nCo;
// find initial, MidiIn: n MidiOut: spots
   if (pos == 0)                          spot = ' ';
   if (MemCm (buf, "MidiIn:",  7) == 0)  {spot = 'i';  return NULL;}
   if (MemCm (buf, "MidiOut:", 8) == 0)  {spot = 'o';  return NULL;}
// skip comments n blank lines
   if ((spot == ' ') || (buf [0] == '\0') || (buf [0] == '#'))  return NULL;
   if (md->_len >= MAX_DEV)  return "tooo many devices in device.txt :(";
  SpaceSep ss (buf, nCo = 2);
   *err = '\0';
   if (StrLn (ss.Col [0])   > MAXTSTR)  StrCp (err, "name too long");
   if (StrLn (ss.Col [1])   > MAXTSTR)  StrCp (err, "type too long");
   if (StrLn (ss.Col [nCo]) > MAXTSTR)  StrCp (err, "description too long");
   if (*err)  {StrFmt (& err [StrLn (err)], " in device.txt line `d", pos+1);
               return err;}
          md->_lst [md->_len].io =   spot;    // fill in stuff
   StrCp (md->_lst [md->_len].name,  ss.Col [0]);
   StrCp (md->_lst [md->_len].type,  ss.Col [1]);
   StrCp (md->_lst [md->_len].desc,  ss.Col [nCo]);
// if ya can't open it, it ain't there - turn it to OFF

   md->_len++;
   return NULL;
}


void MidiDevLst::Load ()
{ TStr  fn;
  File  f;
   _len = 0;
TRC("{ MidiDevLst::Load");
   App.Path (fn, 'd');   StrAp (fn, "\\Device\\device.txt");
// gotta ignore errors here...:/  cuzu SETUP=>MidiConfig use me :/
   f.DoText (fn, this, DoRec);
TRC("} MidiDevLst::Load");
}


bool MidiDevLst::Get    (char io,            char *name, char *type, char *desc)
// find io n name w type!=OFF,  fill in other stuff
{  for (ubyte         i = 0;  i < _len;  i++)
      if ( (io == _lst [i].io) && (! StrCm (name, _lst [i].name)) &&
                                     StrCm (_lst [i].type, "OFF") ) {
            StrCp (type, _lst [i].type);
            StrCp (desc, _lst [i].desc);
            return true;
      }
   return false;                       // ...nope, not therez :(
}


bool MidiDevLst::GetPos (char io, ubyte pos, char *name, char *type, char *desc)
// find io n num, fill in other stuff
{  for (ubyte n = 0,  i = 0;  i < _len;  i++)
      if (io == _lst [i].io) {
         if (n != pos)  n++;
         else {
            StrCp (name, _lst [i].name);
            StrCp (type, _lst [i].type);
            StrCp (desc, _lst [i].desc);
            return true;
         }
      }
   return false;                       // ...hit end of list
}


// MCC -------------------------------------------------------------------------
MCCRow MCC [] = { // types are u=0-127(default)
                            // o=0,127(split at 64)
                            // s=0-127=>-64-63
                            // x=tmpo,tsig,ksig,prog
   {"Tmpo",    'x',120,MC_TMPO},  // tempo (scaled unsigned)  (non midi/mapd)
   {"TSig",    'x',260,MC_TSIG},  // time signature  (non midi/mapd)
   {"KSig",    'x',0,  MC_KSIG},  // key signature C(0b,Maj)=0  (non midi/mapd)
   {"Prog",    'x',0,  MC_PROG},  // set track's sound on it's dev/chn
   {"MTun",    's',64, MC_US+3},  // master transpose/tune
   {"Tune",    's',64, MC_RP+4*2},// track transpose
   {"PBnR",    'u',2,  MC_RP},    // pitchbend range
   {"PBnd",    's',64, MC_PBND},  // pitchbend tuning
   {"MVol",    'u',127,MC_US},    // master volume
   {"Vol",     'u',100,MC_CC+7},  // track overall volume
   {"Expr",    'u',127,MC_CC+11}, // track volume adjustment
   {"MBal",    's',64, MC_US+1},  // master balance
   {"Pan",     's',64, MC_CC+10}, // track pan position
   {"Bal",     's',64, MC_CC+8},  // track pan adjustment
   {"Hold",    'o',0,  MC_CC+64}, // sustain/damper pedal
   {"Soft",    'o',0,  MC_CC+67}, // soft pedal
   {"Sust",    'o',0,  MC_CC+66}, // sustenuto pedal
   {"Lega",    'o',0,  MC_CC+68}, // legato pedal
   {"Rvrb",    'u',0,  MC_CC+91}, // reverb fx amount
   {"Chor",    'u',0,  MC_CC+93}, // chorus fx amount
   {"Portamto",'u',0,  MC_CC+65}, // portamento on/off
   {"PrtTime", 'u',0,  MC_CC+5},  // portamento time hi
   {"PrtTimeL",'u',0,  MC_CC+37}, // portamento time lo
   {"PrtNote", 'u',0,  MC_CC+84}, // portamento note
   {"SnOff",   'u',0,  MC_CC+120},// sounds off
   {"CtRst",   'u',0,  MC_CC+121},// control reset
   {"NtOff",   'u',0,  MC_CC+123},// notes off
   {"LocCtrl", 'u',0,  MC_CC+122},
   {"OmniOff", 'u',0,  MC_CC+124},
   {"OmniOn",  'u',0,  MC_CC+125},
   {"Mono",    'u',0,  MC_CC+126},
   {"Poly",    'u',0,  MC_CC+127},
   {"Prss",    'u',0,  MC_PRSS},  // (usually mapped)
   {"Mod",     'u',0,  MC_CC+1},  // (usually mapped)
   {"Brth",    'u',0,  MC_CC+2},  // (usually mapped)
   {"Pedl",    'u',0,  MC_CC+4},  // (usually mapped)
   {"Hld2",    'o',0,  MC_CC+69}  // sustain/damper pedal 2 (usually mapped)
};
ubyte NMCC = BITS (MCC);

void MCCDump ()
{ TStr s;
   for (ulong i = 0;  i < NMCC;  i++)  DBG("s=`<8s typ=`c dflt=`3d raw=`s",
MCC [i].s, MCC [i].typ, MCC [i].dflt, MCtl2Str (s, MCC [i].raw));
}


// util funcs ------------------------------------------------------------------

char *TmS (char *st, ulong t)
// convert absolute time int to str (ignoring any timesig stuff)
{ ulong b, q, s;
   b = t /  M_WHOLE + 1;       t %=  M_WHOLE;    // 1 based :/
   q = t / (M_WHOLE/4) + 1;    t %= (M_WHOLE/4);
   s = t / (M_WHOLE/16) + 1;   t %= (M_WHOLE/16);
   if (b > 99999)  return "SKIP";
   return StrFmt (st, "`04d.`d`d.`02d", b, q, s, t);
}

ulong TmI (char *st)
// convert absolute time str 2 int (ignoring any timesig stuff)
{ char *p;
  ulong b, q = 0, s = 0, t = 0;
   b = Str2Int (st, & p);   if (b)  --b;
   if (*p == '.') {
      p++;   q = (*p - '0');  if (q)  --q;
      p++;   s = (*p - '0');  if (s)  --s;
      p++;
      if (*p == '.')  t = Str2Int (++p);
   }
   return b*M_WHOLE + q*(M_WHOLE/4) + s*(M_WHOLE/16) + t;
}

ubyte MNt2Int (char *s)
{ TStr  t;
  ubyte n;
   StrCp (t, s);   if (StrLn (t) && (t [StrLn (t)-1] == 'm'))  StrAp (t, "", 1);
   for (n = 0;    n < 12; n++)  if (! StrCm (t, MKeyStr  [n]))  break;
   if (n >= 12)
      for (n = 0; n < 12; n++)  if (! StrCm (t, MKeyStrB [n]))  break;
   if (! StrCm (s, "Cb"))  return 11;  // handlin dumb ksigs sigh
   return n;
}

ubyte MKey2Int (char *s, char **news)
{ ubyte n;
  sbyte f = 0;
   if (news) *news = s;
   if ((*s < '0') || (*s > '9'))          return (ubyte)0;
   for (n = 0; n < 12; n++)  if (CHDN (s [1]) == MKeyStr [n][0])  break;
   if (n >= 12)  {if (news) *news += 1;   return (ubyte)0;}
   if (CHDN (s [2]) == 'b')  f = -1;
   if (      s [2]  == '#')  f =  1;
   if (news) *news += (f ? 3 : 2);
   return (ubyte)((*s-'0'+1)*12  +  n  +  f);
}

char *MKey2Str (char *s, ubyte b, char fl)
{  if (b < M_NT(M_C,0))  b = M_NT(M_C,0);
   if (fl == 'b')  StrFmt (s, "`d`s", b/12-1, MKeyStrB [b%12]);
   else            StrFmt (s, "`d`s", b/12-1, MKeyStr  [b%12]);
   return s;
}

uword MCtl2Int (char *s)              // cc raw str to uword
{ uword rc;
  char *s2;
   if      (! StrCm (s, "prog"))   return MC_PROG;
   else if (! StrCm (s, "prss"))   return MC_PRSS;
   else if (! StrCm (s, "pbnd"))   return MC_PBND;
   else if (! StrCm (s, "tmpo"))   return MC_TMPO;
   else if (! StrCm (s, "tsig"))   return MC_TSIG;
   else if (! StrCm (s, "ksig"))   return MC_KSIG;
   else if (! MemCm (s, "cc", 2))  return MC_CC + (uword)Str2Int (s+2);
   else if (! MemCm (s, "us", 2))  return MC_US + (uword)Str2Int (s+2);
   else if (! MemCm (s, "rp", 2))  rc = MC_RP;
   else if (! MemCm (s, "np", 2))  rc = MC_NP;
   else                            return 0;
   rc += (uword)(Str2Int (s+2, & s2) * 2);
   return rc + (uword)((CHUP (*s2) == 'L') ? 1 : 0);
}

char *MCtl2Str (char *s, uword c, char raw)      // cc uword to raw str
{  *s = '\0';
   if (c < 128)  return MKey2Str (s, (ubyte)c);
   if (c < MC_CC) {
      if      (c == MC_PROG)  StrCp (s, "Prog");
      else if (c == MC_PRSS)  StrCp (s, "Prss");
      else if (c == MC_PBND)  StrCp (s, "PBnd");
      else if (c == MC_TMPO)  StrCp (s, "Tmpo");
      else if (c == MC_TSIG)  StrCp (s, "TSig");
      else if (c == MC_KSIG)  StrCp (s, "KSig");
   }
   else if (c < MC_US) {
      StrFmt (s, "cc`d", c - MC_CC);
      if (raw)  for (ubyte i = 0;  i < NMCC;  i++)  if (c == MCC [i].raw)
         StrFmt (& s [StrLn (s)], "(`s)", MCC [i].s);
   }
   else if (c < MC_RP) {
      StrFmt (s, "us`d", c - MC_US);
   }
   else if (c < MC_NP) {
      StrFmt (s, "rp`d`s", (c - MC_RP) >> 1, c & 0x01 ? "L" : "");
   }
   else
      StrFmt (s, "np`d`s", (c - MC_NP) >> 1, c & 0x01 ? "L" : "");
   return s;
}

char *MNt2Str (char *o, MidiEv *e)
{ TStr s;
   return StrFmt (o, "`s`c`d",
      ((e->chan % 16) == 9) ? MDrm2Str (s, (ubyte)e->ctrl)
                            : MKey2Str (s, (ubyte)e->ctrl),
      (e->valu & 0x0080) ? ((e->val2 & 0x80) ? '~' : '_') : '^',
      e->valu & 0x007F);
}

char *CtlX2Str (char *s, char *cs, TrkEv *in)
{ TrkEv e;
  ubyte r;
   if (in)  MemCp (& e, in, sizeof (TrkEv));
   else {
      for (r = 0;  r < NMCC;  r++)  if (! StrCm (cs, MCC [r].s))  break;
      if (r >= NMCC)  Die ("CtlX2Str not an x ctl");
      e.valu = MCC [r].dflt & 0x7F;
      e.val2 = MCC [r].dflt >> 7;
   }
   if      (! StrCm (cs, "tmpo"))
      StrFmt (s, "`d",  e.valu + (e.val2 << 8));
   else if (! StrCm (cs, "tsig"))
      if (e.val2 >> 4)
            StrFmt (s, "`d/`d/`d", e.valu, 1 << (e.val2 & 0x0F),
                                           1 +  (e.val2 >> 4)  );
      else  StrFmt (s, "`d/`d",    e.valu, 1 << (e.val2 & 0x0F));
   else if (! StrCm (cs, "ksig")) {
      if   (! (e.val2 & 0x80))  StrCp (s, MKeyStr  [e.valu]);
      else if (e.valu != 11)    StrCp (s, MKeyStrB [e.valu]);
      else                      StrCp (s, "Cb");
      if (e.val2 & 0x01)  StrAp (s, "m");
      *s = CHUP (*s);
   }             // else "prog"
   else
      StrCp (s, "*");
   return s;
}

void CtlX2Val (TrkEv *e, char *cs, char *s)
{ ubyte r;
  uword w;
   if (*s == '\0') {
      for (r = 0;  r < NMCC;  r++)  if (! StrCm (cs, MCC [r].s))  break;
      if (r >= NMCC)  Die ("CtlX2Val not an x ctl");
      e->valu = MCC [r].dflt & 0x7F;
      e->val2 = MCC [r].dflt >> 7;
   }
   else if (! StrCm (cs, "tmpo")) {
      w = (uword)Str2Int (s);   e->valu = (ubyte)(w & 0xFF);
                                e->val2 = (ubyte)(w >> 8);
   }
   else if (! StrCm (cs, "tsig")) {
      e->valu = (ubyte)Str2Int (s, & s);   if (*s == '/')  s++;
      w       = (uword)Str2Int (s, & s);
      for (r = 0;  r < 16;  r++)  if ((1 << r) == w)  {e->val2 = r;   break;}
      if (*s == '/')  {w = (uword)Str2Int (++s);   e->val2 |= ((w-1) << 4);}
   }
   else if (! StrCm (cs, "ksig")) {
      e->val2 = (s [StrLn (s)-1] == 'm') ? 1 : 0;
      e->valu = MNt2Int (s);
      if (s [1] == 'b')  e->val2 |= 0x80;
   }             // else "prog"
   else
      e->valu = e->val2 = 0;
}


// MidiI -----------------------------------------------------------------------
void MidiI::Chk (UINT err, char *f, char x)
{ static char buf [161];
  TSt2 b2;
   if (err != MMSYSERR_NOERROR) {
      ::midiInGetErrorText (err, b2, MAX_PATH);
      if (x)  {TRC ("`s `s", f, StrCvt (buf, b2));}
      else    Die (StrCvt (buf, b2), f);
   }
}


MidiI::MidiI (char *name, Timer *tmr, HWND wnd, char raw)
// timer needed to stamp MidiEv.time
: _hnd (NULL), _timer (tmr),   _bAdd (0), _bRmv (0), _bErr (false),
  _hdrUsed (false), _hdrDone (false), _numB (0), _raw (raw)
{ UINT rc;
  TStr p;
TRC("{ MidiI `s", name);
   MemSet (_buf, 0, sizeof (_buf));   _syX = 'n';   _tmX = 0;
   StrCp (_name, name);
   if (wnd) {_sigThrd = NULL;  _sigWndo = wnd;}
   else     {_sigWndo = NULL;  _sigThrd = ::GetCurrentThreadId ();}
   if (! Midi.Get ('i', _name, _type, _desc))
      Die ("MidiI::MidiI  unknown device name", _name);
  ubyte max = ::midiInGetNumDevs ();
   for (_mmId = 0;  _mmId < max;  _mmId++) {
      Chk (::midiInGetDevCaps (_mmId, & _cap, sizeof (_cap)),
            "midiInGetDevCaps");
      if (StrCm (StrCvt (p, _cap.szPname), _desc) == 0)  break;
   }
   if (_mmId >= max) {
      _hnd = NULL;
TRC("} MidiIn device `s not connected", _name);
      return;
   }
   rc = ::midiInOpen (& _hnd, _mmId, (DWORD_PTR)Handler, (DWORD_PTR)this,
                      CALLBACK_FUNCTION);
   if (rc != MMSYSERR_NOERROR) {
      _hnd = NULL;
TRC("} MidiIn device `s ALREADY grabbed by another app", _name);
      return;
   }
   Chk (::midiInStart (_hnd),  "midiInStart");
   if (_raw == 's')  _timer->SetTempo (1);
TRC("} MidiI");
}


MidiI::~MidiI (void)
{
TRC("{ ~MidiI `s", (*_name) ? _name : "?");
   _timer = NULL;   _name [0] = '\0';
   if (_hnd == NULL)  {TRC ("} ~MidiI");   return;}
   if (_hdrUsed) {
      if (! _hdrDone)  Chk (::midiInReset(_hnd), "midiInResetX");
      Chk (::midiInUnprepareHeader (_hnd, & _hdr, sizeof (_hdr)),
            "midiInUnprepareHeader");
   }
   if (_numB) {
      Chk (::midiInReset (_hnd), "midiInResetX");
      for (ubyte i = 0; i < _numB; i++)
      Chk (::midiInUnprepareHeader (_hnd, & _hdrB [i], sizeof (_hdrB [0])),
            "midiInUnprepareHeader");
   }
   Chk (::midiInStop  (_hnd), "midiInStop", '-');
   Chk (::midiInReset (_hnd), "midiInReset");
   Chk (::midiInClose (_hnd), "midiInClose");
   _hnd = NULL;
TRC("} ~MidiI");
}


bool MidiI::Get (MidiEv *ev)
{  if (_hnd == NULL)    return false;
//TRC("{ MidiI::Get");
   if (_bRmv == _bAdd)  {
//TRC("} MidiI::Get - empty");
      return false;  // nothin there
   }
   MemCp (ev, & _buf [_bRmv], sizeof (MidiEv));
  ubyte p = _bRmv;
   if (++p == BITS (_buf))  p = 0;
   _bRmv = p;
//TRC("} MidiI::Get");
   return true;
}


void MidiI::BufAdj (slong tm)
{ slong t;
   if (_hnd)  for (ubyte p = _bRmv;  p != _bAdd;)
                  {t = (slong)_buf [p].time + tm;   _buf [p].time = (ulong)t;
                   if (++p == BITS (_buf))  p = 0;}
}


void MidiI::GetSx (ubyte *strm, DWORD len)
{  if (_hnd == NULL)  return;
   if (_hdrUsed) {
      if (! _hdrDone)  Chk (::midiInReset (_hnd), "MidiI::GetSx  midiInReset");
      _hdrUsed = _hdrDone = false;
      Chk (::midiInUnprepareHeader (_hnd, & _hdr, sizeof (_hdr)),
           "MidiI::GetSx  midiInUnprepareHeader");
   }
   _hdr.lpData = (char *)strm;  _hdr.dwBufferLength = len;  _hdr.dwFlags = 0;
   Chk (::midiInPrepareHeader (_hnd, & _hdr, sizeof (_hdr)),
        "MidiI::GetSx  midiInPrepareHeader");
   _hdrUsed = true;
   Chk (::midiInAddBuffer (_hnd, & _hdr, sizeof (_hdr)),
        "MidiI::GetSx  midiInAddBuffer");
}


void MidiI::SxBuf (ubyte num, ulong siz, ubyte *p)
{  if (_hnd == NULL)  return;
   _numB = num;  _sizB = siz;
   for (ubyte i = 0; i < num; i++) {
      MemSet (& _hdrB [i], 0, sizeof (_hdrB [0]));
      _hdrB [i].lpData = (char *)(& p [i*siz]);
      _hdrB [i].dwBufferLength = siz;
      Chk (::midiInPrepareHeader (_hnd, & _hdrB [i], sizeof (_hdrB [0])),
           "MidiI::SxSet  midiInPrepareHeader");
      Chk (::midiInAddBuffer (_hnd, & _hdrB [i], sizeof (_hdrB [0])),
           "MidiI::GetSx  midiInAddBuffer");
   }
}


void MidiI::SxRet (ubyte *p)
{  if (_hnd == NULL)  return;
   for (ubyte i = 0; i < _numB; i++)  if ((char *)p == _hdrB [i].lpData) {
      _hdrB [i].dwBytesRecorded = 0;  _hdrB [i].dwFlags &= ~MHDR_DONE;
      Chk (::midiInAddBuffer (_hnd, & _hdrB [i], sizeof (_hdrB [0])),
           "MidiI::SxRet  midiInAddBuffer");
      return;
   }
}


void CALLBACK MidiI::Handler
   (HMIDIIN h, UINT msg, DWORD user, DWORD l1, DWORD l2)
{ MidiI *m = (MidiI *)user;
  ubyte  p, pnew,   s, v, v2 = 0,   t;
  uword                c;
  bool              ok = false;
   if (msg == MIM_CLOSE) {
//DBG("MidiI::Handler  MIM_CLOSE on `s", m?m->_name:"?");
      return;      // he's dead, Jim...
   }
   if ((m == NULL) || (! m->_name [0])) {
//DBG("MidiI::Handler  m=`08x _name=`s", m, m?m->_name:"?");
      return;      // ...he's also dead, Jim
   }
   switch (msg) {
   case MIM_LONGERROR: m->_bErr    = true;
//DBG("MidiI::Handler  MIM_LONGERROR on `s", m?m->_name:"?");
   case MIM_LONGDATA:  m->_hdrDone = true;
      break;
   case MIM_ERROR:     m->_bErr    = true;
//DBG("MidiI::Handler  MIM_ERROR on `s", m?m->_name:"?");
   case MIM_DATA:
      pnew = p = m->_bAdd;  if (++pnew == BITS (m->_buf))  pnew = 0;
      if (pnew == m->_bRmv)  m->_bErr = true;
      else {
         s = (ubyte) l1 & 0x0FF;   c = (uword)(l1 >>  8) & 0x0FF;
                                   v = (ubyte)(l1 >> 16) & 0x0FF;
         if ( (! m->_raw) && ((s == M_CLOCK) || (s == M_SENSE)) )  return;

         if (m->_timer) {m->_buf [p].time = m->_timer->Get ();
                         m->_buf [p].msec = m->_timer->GetMS ();}
         else            m->_buf [p].time = m->_buf [p].msec = l2;
//if ((s != M_CLOCK) && (s != M_SENSE)) DBG("`02x `02x `02x\n",s,c,v);
         if (m->_raw != 'y')  switch (s & 0xF0) {
         case M_NPRS: v2 = 0x80;
         case M_NOTE: if (v)  v |= 0x80;
         case M_NOFF: if ((c >= M_NT(M_C,0)) && (c <= M_NT(M_B,8)))  ok = true;
                                                                 break;
         case M_PROG: v = (ubyte)c;   c = MC_PROG;   ok = true;  break;
         case M_PRSS: v = (ubyte)c;   c = MC_PRSS;   ok = true;  break;
         case M_PBND: v2 = (ubyte)c;  c = MC_PBND;   ok = true;  break;
         case M_CTRL:
            switch (c) {
            case M_NRPNL:
               m->_rpn = 0x8000 | (m->_rpn & 0x7F00) | (v<<1);     break;
            case M_NRPNH:
               m->_rpn = 0x8000 | (v << 8) | (m->_rpn & 0x00FF);   break;
            case M_RPNL:
               m->_rpn =          (m->_rpn & 0x7F00) | (v<<1);     break;
            case M_RPNH:
               m->_rpn =          (v << 8) | (m->_rpn & 0x00FF);   break;
            case M_DATL:
            case M_DATH:
               t = (ubyte)c;   c = m->_rpn;
               if (c & 0x8000) {if (c < 65534)               ok = true; }
               else            {if (c < 16384) {c += MC_NP;  ok = true;}}
               if (t == M_DATH)  c++;
               break;
            default:
               c |= MC_CC;  ok = true;
            }
            ok = true;  break;
         default: /*0xF0:*/            // filter M_CLOCK and M_SENSE
            if (! m->_raw)
                 {if ((s != M_CLOCK) && (s != M_SENSE))  ok = true;}
            else {
            // _raw == 's' means sync timer to clock/start/stop/cont/songpos
               switch (s) {
               case M_START:   m->_tmX = 0;
               case M_CONT:    m->_syX = 'y';                  break;
               case M_STOP:    m->_syX = 'n';                  break;
               case M_SONGPOS: m->_tmX = 6*8 * (c << 7 | v);   break;
               case M_CLOCK:   m->_timer->Set (m->_tmX);   m->_syX = 'y';
                               m->_tmX +=  8;                  break;
               case M_SENSE:   break;
               default:        ok = true;
               }
               if (! ok)  return;
            }
         }
      // _raw == 'y' means store all minus M_SENSE in buffer
         if      (m->_raw == 'y') {
            if (s == M_SENSE)  return;
            m->_buf [p].chan = 0;
            m->_buf [p].ctrl = s;
            m->_buf [p].valu = (ubyte)c;
            m->_buf [p].val2 = v;
            m->_bAdd = pnew;
         }
         else if (ok) {
            m->_buf [p].chan = s & 0x0F;
            m->_buf [p].ctrl = c;
            m->_buf [p].valu = v;
            m->_buf [p].val2 = v2;
//DBG("dev=`s chan=`02x ctrl=`02x valu=`02x val2=`02x",
//m->_name, s & 0x0F, c, v, v2);
            m->_bAdd = pnew;
         }
         else return;                  // not bufferin?  no MIM_DATA msg
      }
      break;
   default:
      return;                          // don't send other MIMs thru
   }
   if (msg == MIM_LONGDATA) {
      MIDIHDR *mh = (MIDIHDR *)l1;
      if (m->_sigThrd)
           {if (::PostThreadMessage (m->_sigThrd, msg,
                (WPARAM)mh->lpData, (LPARAM)mh->dwBytesRecorded) != TRUE)
               DieWn ("MidiI::Handler  PostThrdMsgX died");}
      else {
      if (::PostMessage       (m->_sigWndo, msg,
                (WPARAM)mh->lpData, (LPARAM)mh->dwBytesRecorded) != TRUE)
               DieWn ("MidiI::Handler  PostMsgX died");}
   }
   else {
//DBG("MidiI::Handler got `s", m?m->_name:"?");
      if (m->_sigThrd)
           {if (::PostThreadMessage (m->_sigThrd, msg, l1, l2) != TRUE) {
              static TStr rats;
               StrFmt (rats, "MidiInHandler  PostThrdMsg died  PLEASE CHECK "
                             "device `s  (type=`s  Description=`s)",
                       m->_name, m->_type, m->_desc);
               DieWn (rats);
            }
           }
      else {if (::PostMessage       (m->_sigWndo, msg, l1, l2) != TRUE)
               DieWn ("MidiI::Handler  PostThrdMsg died");}
   }
}


// MidiO -----------------------------------------------------------------------
void MidiO::Chk (UINT err, char *f)
{ static TStr buf;
  TSt2 b2;
   if (err != MMSYSERR_NOERROR)
      {::midiOutGetErrorText (err, b2, MAX_PATH);   Die (StrCvt (buf, b2), f);}
}

const ulong MSG_BANK = MSG_CLOSE+1;
const ulong MSG_MIDI = MSG_CLOSE+2;

MidiO::MidiO (char *name, bool noreset)
: _hnd (NULL), _noSx (false), _hdrUsed (false), _hdrDone (false),
  _sigThrd (::GetCurrentThreadId ())
{ TStr m, buf, p;
  TSt2 b2;
  UINT rc;
TRC("{ MidiO `s noreset=`b", name, noreset);
   MemSet (_ntOn, 0, sizeof (_ntOn));
   StrCp (_name, name);
   _syn = 0;   _noRs = noreset;
   if (! Midi.Get ('o', _name, _type, _desc))
      Die ("MidiO::MidiO  don't know device name:", name);
TRC("type=`s desc=`s", _type, _desc);

// got syn?
   if (! StrCm (_type, "syn")) {
     char *p;
      _syn = 1;
      if (p = StrCh (_desc, '#'))  _syn = (ubyte) Str2Int (p+1);     // 1..8
     ShMem  sh;
     DWORD *b;
      if (! (b = (DWORD *)sh.Open ("syn.exe", 4)))  Die ("can't open syn.exe");

      _synThrd = *b;
      sh.Shut ();
      GMInit ();
TRC("} MidiO - syn");
      return;
   }

// look up regular midi device
  ubyte max = ::midiOutGetNumDevs ();
   for (_mmId = 0; _mmId < max; _mmId++) {
      rc = ::midiOutGetDevCaps (_mmId, & _cap, sizeof (_cap));
      if (rc != MMSYSERR_NOERROR) {
         ::midiOutGetErrorText (rc, b2, MAX_PATH);
         StrFmt (m, "Can't check a midiout device\r\n"
                    "name=`s type=`s desc=`s\r\n`s",
                 name, _type, _desc, StrCvt (buf, b2));
         Die (m);
      }
      if (StrCm (StrCvt (p, _cap.szPname), _desc) == 0)  break;
   }
   if (_mmId >= max) {                 // device could be off...
TRC("} MidiO - device not on=`s type=`s desc=`s", name, _type, _desc);
      _hnd = NULL;
      return;
   }
TRC("mmid=`d", _mmId);
   if ((rc = ::midiOutOpen (& _hnd, _mmId, (DWORD_PTR)Handler, (DWORD_PTR)this,
                            CALLBACK_FUNCTION) != MMSYSERR_NOERROR)) {
      ::midiOutGetErrorText (rc, b2, MAX_PATH);
TRC("device won't open rc=`d err=`s", rc, StrCvt (buf, b2));
   // try again without the callback biz (can't do sysex now)
      _hnd = NULL;   _noSx = true;
      if ((rc = ::midiOutOpen (& _hnd, _mmId, 0, 0, 0)) != MMSYSERR_NOERROR) {
         ::midiOutGetErrorText (rc, b2, MAX_PATH);   StrCvt (buf, b2);
TRC("won't open even with no callback rc=`d err=`s", rc, buf);
         StrFmt (m, "Can't open midiout device=`s type=`s desc=`s\r\nerror=`s",
                 name, _type, _desc, buf);
         Die (m);
      }
   }
   GMInit ();
TRC("} MidiO");
}


MidiO::~MidiO (void)
// toss the header thing;  shush the notes left on;  reset n close
{
TRC("{ ~MidiO `s", (*_name) ? _name : "?");
   if (_syn)  {NotesOff ();   _syn = 0;   TRC("} ~MidiO - syn");        return;}

   if (_hnd == NULL)                     {TRC("} ~MidiO - was dead");   return;}

   if ((! _noSx) && _hdrUsed) {
      while (! _hdrDone)  Sleep (1);   // not nice...:/
      Chk (::midiOutUnprepareHeader (_hnd, & _hdr, sizeof (_hdr)),
                                     "midiOutUnprepareHeader");
   }
   NotesOff ();                        // does midiOutReset too
   Chk (::midiOutClose (_hnd), "midiOutClose");
TRC("} ~MidiO");
}


void MidiO::PutMEv (ubyte *mev)
{  if (_syn)           return;         // none of this for syn.exe (all in Put)
   if (_hnd == NULL)  {DBG("PutMEv `s but _hnd is NULL", _name);   return;}

  TSt2 b2;
  TStr s;
  UINT rc;
   if ((! _noSx) && _hdrUsed)  {       // not nice :/
DBG("await sysex");
      while (! _hdrDone)  Sleep (1);
DBG("wait done");
   }
//TRC("mosm a");
   if (MMSYSERR_NOERROR != (rc = ::midiOutShortMsg (_hnd, *((DWORD *)mev)))) {
TRC("mosm c");
      ::midiOutGetErrorText (rc, b2, MAX_PATH);
DBG("couldn't midiOutShortMsg `s/`s/`s rc=`d err=`s.  nulling _hnd",
_name, _type, _desc, rc, StrCvt (b2, s));
      _hnd = NULL;
   }
//TRC("mosm b");

/* for INEVitable tracing...:/
DBG("PutMEv on `s/`s/`s", _name, _type, _desc);
   switch (mev[0] & 0xF0) {
      case M_NOTE:
         DBG(" ch=`d Note `s=`d", mev[0] & 0x0F, MKey2Str (s, mev[1]), mev [2]);
         break;
      case M_NOFF:
         DBG(" ch=`d NOff `s=`d", mev[0] & 0x0F, MKey2Str (s, mev[1]), mev [2]);
         break;
      case M_NPRS:
         DBG(" ch=`d NPrs `s=`d", mev[0] & 0x0F, MKey2Str (s, mev[1]), mev [2]);
         break;
      case M_PROG:
         DBG(" ch=`d Prog=`s.`d", mev[0] & 0x0F, MProg [mev[1]], mev[1]);
         break;
      case M_PBND:
         DBG(" ch=`d PBnd=`d",    mev[0] & 0x0F, 8192-(mev[1] + mev[2]*128));
         break;
      case M_PRSS:
         DBG(" ch=`d Prss=`d",    mev[0] & 0x0F, mev[1]);
         break;
      case M_CTRL:
         switch (mev[1]) {
            case M_NRPNL:  StrCp (s, "NRPNL.");  break;
            case M_NRPNH:  StrCp (s, "NRPN.");   break;
            case M_RPNL:   StrCp (s, "RPNL.");   break;
            case M_RPNH:   StrCp (s, "RPNH.");   break;
            case M_DATH:   StrCp (s, "DatH.");   break;
            case M_DATL:   StrCp (s, "DatL.");   break;
            case M_BANK:   StrCp (s, "Bank.");   break;
            case M_BNKL:   StrCp (s, "BnkL.");   break;
            case M_MOD:    StrCp (s, "Mod.");    break;
            case M_BRTH:   StrCp (s, "Brth.");   break;
            case M_PEDL:   StrCp (s, "Pedl.");   break;
            case M_VOL:    StrCp (s, "Vol.");    break;
            case M_EXPR:   StrCp (s, "Expr.");   break;
            case M_PAN:    StrCp (s, "Pan.");    break;
            case M_BAL:    StrCp (s, "Bal.");    break;
            case M_HOLD:   StrCp (s, "Hold.");   break;
            case M_HLD2:   StrCp (s, "Hld2.");   break;
            case M_SOFT:   StrCp (s, "Soft.");   break;
            case M_SUST:   StrCp (s, "Sust.");   break;
            case M_LEGA:   StrCp (s, "Lega.");   break;
            default:       StrCp (s, "");        break;
         }
         DBG(" ch=`d Ctrl(`s`d)=`d",  mev[0] & 0x0F, s, mev[1], mev[2]);
         break;
      default:
         DBG(" ??? `d `d `d `d (x`02x `02x `02x `02x)",
              mev[0], mev[1], mev[2], mev[3],
              mev[0], mev[1], mev[2], mev[3]);
         break;
   }
*/
}


/*
void MidiO::PutSx (ubyte *strm, DWORD len)
{  if (_syn || _noSx)  return;         // none of this for syn.exe

   if (_hnd == NULL)  return;//Die ("MidiO::PutSx  _hnd is NULL");
   if (_hdrUsed) {
      while (! _hdrDone)  Sleep (1);   // not nice...:/
      _hdrUsed = _hdrDone = false;
      Chk (::midiOutUnprepareHeader (_hnd, & _hdr, sizeof (_hdr)),
           "MidiO::PutSx  midiOutUnprepareHeader");
   }
   _hdr.lpData = (char *)strm;  _hdr.dwBufferLength = len;  _hdr.dwFlags = 0;
   Chk (::midiOutPrepareHeader (_hnd, & _hdr, sizeof (_hdr)),
        "MidiO::PutSx  midiOutPrepareHeader");
   _hdrUsed = true;
   Chk (::midiOutLongMsg (_hnd, & _hdr, sizeof (_hdr)),
        "MidiOut::PutSx  midiOutLongMsg");
}
*/


void MidiO::Put (ubyte ch, uword c, ubyte v, ubyte v2)
// build a midi event given args
// do notes (note/nprs/noff - keepin track of which chan/notes are on),
{  if (_syn)  {if (! (ch & 0x80))  ch |= ((_syn-1)<<4);
//DBG("MidiO::Put to syn ch=`d c=`d v=`d v2=`d",  ch, c, v, v2);
               PosTM (_synThrd, MSG_MIDI, ch, (c<<16)|(v<<8)|v2);
               return;}
//DBG("MidiO::Put on `s ch=`d c=`d v=`d v2=`d", _name, ch, c, v, v2);
  ubyte mev [4], p;
  ulong m;
  ubyte mvol [8] = {0xF0,0x7F,0x7F,0x04,0x01,0,0,0xF7};
  ubyte mbal [8] = {0xF0,0x7F,0x7F,0x04,0x02,0,0,0xF7};
//ubyte mtun [8] = {0xF0,0x7F,0x7F,0x04,0x04,0,0,0xF7}; 3,4 are +-cent/8192,cent
   mev [0] = ch;  mev [1] = (ubyte)c;  mev [2] = 0x7F & v;
   if      (c < MC_PROG) {             // just a note
      p = (ch << 2) | (c >> 5);  m = 1 << (c & 0x1F);
      if (v & 0x80)  //if ((_ntOn [p] & m) == 0)
                         {mev [0] |= M_NOTE;  _ntOn [p] |= ( m);}
                     //else mev [0] |= M_NPRS;
      else               {mev [0] |= M_NOFF;  _ntOn [p] &= (~m);}
   }
   else if (c < MC_CC) {               // std midi
      if      (c == MC_PROG) {mev [0] |= M_PROG;  mev [1] = v;}
      else if (c == MC_PRSS) {mev [0] |= M_PRSS;  mev [1] = v;}
      else if (c == MC_PBND) {mev [0] |= M_PBND;  mev [1] = v2;}
      else return;
   }
   else if (c < MC_US) {               // cc
      mev [0] |= M_CTRL;  mev [1] = c - MC_CC;
   }
   else if (c < MC_RP) {               // cc
      if (c == MC_MVOL) {
         mvol [6] = _MVol = v;  mvol [5] = v ? 0x7F : 0;   // PutSx (mvol, 8);
      }
      if (c == MC_MBAL) {
         mbal [6] = _MBal = v;  // PutSx (mbal, 8);
      }
      return;
   }
   else if (c < MC_NP) {
      mev [0] |= M_CTRL;
      mev [1] = M_RPNH;    mev [2] = ((c - MC_RP) >> 8) & 0x7F;   PutMEv (mev);
      mev [1] = M_RPNL;    mev [2] = ((c - MC_RP) >> 1) & 0x7F;   PutMEv (mev);
      mev [1] = (c & 0x0001) ? M_DATL : M_DATH;   mev [2] = v;
   }
   else {
      mev [0] |= M_CTRL;
      mev [1] = M_NRPNH;   mev [2] = ((c - MC_NP) >> 8) & 0x7F;   PutMEv (mev);
      mev [1] = M_NRPNL;   mev [2] = ((c - MC_NP) >> 1) & 0x7F;   PutMEv (mev);
      mev [1] = (c & 0x0001) ? M_DATL : M_DATH;   mev [2] = v;
   }
   PutMEv (mev);
}


void MidiO::NotesOff ()
{
TRC("{ MidiO::NotesOff on `s/`s/`s", _name, _type, _desc);
   if (_syn)  {
      for (ubyte i = 0; i < 16; i++)
         {PosTM (_synThrd, MSG_MIDI, ((_syn-1)<<4)|i, (MC_CC|M_ASOFF)<<16);
          PosTM (_synThrd, MSG_MIDI, ((_syn-1)<<4)|i, (MC_CC|M_HOLD )<<16);}
TRC("} MidiO::NotesOff");
      return;
   }
  ubyte p, mev [4], hoff [4];
  ulong m;
  TStr  ts;
   hoff [1] = M_HOLD;  hoff [2] = 0;
   for (ubyte ch = 0; ch < 16; ch++) {
      for (ubyte nt = 0; nt < 128; nt++) {
         p = (ch << 2) | (nt >> 5);   m = 1 << (nt & 0x1F);
         if (_ntOn [p] & m) {
TRC("ch=`d nt=`s", ch, MKey2Str(ts, nt));
            mev [0] = M_NOFF | ch;   mev [1] = nt;  mev [2] = 0;
            PutMEv (mev);
         }
      }
      hoff [0] = M_CTRL | ch;   PutMEv (hoff);     // hold to OFF, too
   }
   MemSet (_ntOn, 0, sizeof (_ntOn));
   ::Sleep (5);   // ...so last noteoff doesn't get wrecked by this reset...
   if (! _noRs)  if (MMSYSERR_NOERROR != ::midiOutReset (_hnd))  _hnd = NULL;
TRC("} MidiO::NotesOff");
}


void MidiO::DumpOns ()
{ ubyte p;
  ulong m;
  TStr  ts;
TRC("{ MidiO::DumpOns on `s/`s/`s", _name, _type, _desc);
   for (ubyte ch = 0; ch < 16; ch++)  for (ubyte nt = 0; nt < 128; nt++) {
         p = (ch << 2) | (nt >> 5);   m = 1 << (nt & 0x1F);
         if (_ntOn [p] & m)
TRC("ch=`d nt=`s", ch, MKey2Str(ts, nt));
   }
TRC("} MidiO::DumpOns");
}


void MidiO::GMInit ()
{
TRC("{ MidiO::GMInit on `s/`s/`s", _name, _type, _desc);
   if (_syn)  {
      for (ubyte i = 0; i < 16; i++) {
         if (i != 9)
         PosTM (_synThrd, MSG_MIDI, ((_syn-1)<<4)|i,  MC_PROG<<16);
         PosTM (_synThrd, MSG_MIDI, ((_syn-1)<<4)|i, (MC_CC|M_ACOFF)<<16);
      }
TRC("} MidiO::GMInit");
      return;
   }
   Put (0, MC_MVOL, 127);
   Put (0, MC_MBAL, 64);         // non chan
   for (ubyte c = 0; c < 16; c++) {
      if (c != 9)  Put (c, MC_PROG);
      Put (c, MC_CC|M_BANK);           Put (c, MC_CC|M_BNKL);
      Put (c, MC_PRSS);                Put (c, MC_CC|M_MOD);    // valu=0
      Put (c, MC_CC|M_HOLD);           Put (c, MC_CC|M_HLD2);
      Put (c, MC_CC|M_SOFT);           Put (c, MC_CC|M_SUST);
      Put (c, MC_CC|M_LEGA);
      Put (c, MC_PBNR,        2);                                    // 2
      Put (c, MC_PBND,       64);      Put (c, MC_TUNE,     64);     // 64
      Put (c, MC_CC|M_PAN,   64);      Put (c, MC_CC|M_BAL, 64);
      Put (c, MC_CC|M_VOL,  100);
      Put (c, MC_CC|M_EXPR, 127);                                    // 127
   }
TRC("} MidiO::GMInit");
}


void CALLBACK MidiO::Handler (HMIDIOUT h, UINT msg, DWORD user,
                              DWORD l1, DWORD l2)
{ MidiO *m = (MidiO *)user;
//DBG("in MidiO::Handler");
   if (msg != MOM_DONE)  return;  // skip any other MOM msgs
   m->_hdrDone = true;
   if (m->_sigThrd)
      if (::PostThreadMessage (m->_sigThrd, MOM_DONE, l1, l2) != TRUE)
         Die ("MidiO::Handler  PostThrdMsg died");
}

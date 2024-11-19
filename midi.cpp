// midi.cpp

#include "midi.h"
#include "midiProg.cpp"                // eh, just always...
#include "midiDrum.cpp"
#ifdef USE_SYN
#include "syn.h"
#endif

char MKeyStr  [12][3] =
        {"c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"},
     MKeyStrB [12][3] =
        {"c", "db", "d", "eb", "e", "f", "gb", "g", "ab", "a", "bb", "b"};

//______________________________________________________________________________
MidiDevLst Midi;

char *MidiDevLst::DoRec (char *buf, ubyt2 len, ubyt4 pos, void *ptr)
{ static char spot;
  static TStr err;
  MidiDevLst *m = (MidiDevLst *)ptr;
   (void)len;
// find initial, MidiIn: n MidiOut: spots
   if (pos == 0)                          spot = ' ';
   if (MemCm (buf, CC("MidiIn:"),  7) == 0)  {spot = 'i';   return NULL;}
   if (MemCm (buf, CC("MidiOut:"), 8) == 0)  {spot = 'o';   return NULL;}
// skip comments n blank lines
   if ((spot == ' ') || (*buf == '\0') || (*buf == '#'))    return NULL;
  ColSep ss (buf, 2);
   *err = '\0';
   if (StrLn (ss.Col [0]) > MAXTSTR)  StrCp (err, CC("name too long"));
   if (StrLn (ss.Col [1]) > MAXTSTR)  StrCp (err, CC("type too long"));
   if (StrLn (ss.Col [2]) > MAXTSTR)  StrCp (err, CC("description too long"));
   if (m->_len >= MAX_DEV)            StrCp (err, CC("too many devices"));
   if (*err) {
      StrFmt (& err [StrLn (err)], " in device.txt line `d", pos+1);
      DBG (err);   return err;
   }
          m->_lst [m->_len].io =   spot;    // fill in stuff
   StrCp (m->_lst [m->_len].name,  ss.Col [0]);
   StrCp (m->_lst [m->_len].type,  ss.Col [1]);
   StrCp (m->_lst [m->_len].desc,  ss.Col [2]);
   StrCp (m->_lst [m->_len].dev, CC(StrCm (ss.Col [1], CC("syn")) ? "?" : "!"));
   m->_len++;                          // syn needs no IS IT ON check
   return NULL;
}


void MidiDevLst::InsDev (char io, char *desc, char *dev)
// update/insert dev into _lst matching on desc
{ ubyte p, q, no;
  TStr  nm;
  bool  got, dup;
   for (got = false, p = 0;  p < _len;  p++)
      if ( (! StrCm (_lst [p].desc, desc)) && (_lst [p].io == io) &&
                                              (_lst [p].dev [0] == '?') )
         {StrCp (_lst [p].dev, dev);   got = true;   break;}
   if (! got) {                        // dagnabitt - gotta insert
      if (_len >= MAX_DEV)  {DBG("too many midi devices");   return;}
      if (io == 'i') {                 // gotta find ins spot
         for (p = 0;  p < _len;  p++)  if (_lst [p].io != 'i')  break;
         RecIns (_lst, _len+1, sizeof (_lst [0]), p);
      }
      else  p = _len;                  // ez - tack it onto end
      dup = true;                      // got i with same desc?  use name
      if (io == 'o')  for (q = 0;  q < _len;  q++)
         if ((_lst [q].io == 'i') && (! StrCm (_lst [q].desc, desc)))
            {StrCp (nm, _lst [q].name);   dup = false;   break;}
      for (no = 1;  dup;  no++) {      // need new name thing1, thing2, etc
         dup = false;   StrFmt (nm, "thing`d", no);
         for (q = 0;  q < _len;  q++)  if (! StrCm (nm, _lst [q].name))
                                          {dup = true;   break;}
      }
             _lst [p].io =  io;
      StrCp (_lst [p].name, nm);
      StrCp (_lst [p].type, CC("DEFAULT"));
      StrCp (_lst [p].desc, desc);
      StrCp (_lst [p].dev,  dev);
      _len++;
   }
}


void MidiDevLst::Dump ()               // test Load()
{  for (ubyte i = 0;  i < _len;  i++)  DBG("`d: `c `s `s `s `s",
     i, _lst [i].io, _lst [i].name, _lst [i].type, _lst [i].desc, _lst [i].dev);
}


void MidiDevLst::Load ()
{ TStr  fn, name, sdev;
  File  f;
  int   card, dev, sub, nsub, isub, osub, err;
  snd_ctl_t          *ctl;
  snd_rawmidi_info_t *info;
  const char         *desc, *desc2;
TRC("MidiDevLst::Load");
   _len = 0;
   App.Path (fn, 'd');   StrAp (fn, CC("/device/device.txt"));
   f.DoText (fn, this, DoRec);

// map desc to alsa's "rawmidi" hw:9,9[,9] names (a bitt painful) - into .dev
   snd_rawmidi_info_alloca (& info);        // sheesh :/
   for (card = -1;;) {
      if ((err = ::snd_card_next (& card))) {
         DBG ("snd_card_next error: `s", ::snd_strerror (err));
         break;
      }
      if (card < 0)  break;

//DBG("card#=`d", card);
      StrFmt (name, "hw:`d", card);
      if ((err = ::snd_ctl_open (& ctl, name, 0)) < 0) {
         DBG ("snd_ctl_open error for card `d: `s", card, ::snd_strerror (err));
         break;
      }
      for (dev = -1;;) {
         if ((err = ::snd_ctl_rawmidi_next_device (ctl, & dev)) < 0) {
            DBG ("snd_ctl_rm_next_device error: `s", ::snd_strerror (err));
            break;
         }
         if (dev < 0)  break;

//DBG(" dev=`d", dev);
         isub = osub = 0;
         ::snd_rawmidi_info_set_device (info, dev);
         ::snd_rawmidi_info_set_stream (info, SND_RAWMIDI_STREAM_INPUT);
         if (::snd_ctl_rawmidi_info (ctl, info) >= 0)
            isub = ::snd_rawmidi_info_get_subdevices_count (info);
         ::snd_rawmidi_info_set_stream (info, SND_RAWMIDI_STREAM_OUTPUT);
         if (::snd_ctl_rawmidi_info (ctl, info) >= 0)
            osub = ::snd_rawmidi_info_get_subdevices_count (info);
         nsub = isub > osub ? isub : osub;
//DBG("  nsub=`d isub=`d osub=`d", nsub, isub, osub);
         for (sub = 0;  sub < nsub;  sub++) {
            ::snd_rawmidi_info_set_stream (
               info, sub < isub ? SND_RAWMIDI_STREAM_INPUT
                                : SND_RAWMIDI_STREAM_OUTPUT);
            ::snd_rawmidi_info_set_subdevice (info, sub);
            if ((err = ::snd_ctl_rawmidi_info (ctl, info))) {
               DBG ("snd_rm_info_set_subdevice error: `s\n",
                    ::snd_strerror (err));
               break;
            }
            desc  = ::snd_rawmidi_info_get_name           (info);
            desc2 = ::snd_rawmidi_info_get_subdevice_name (info);
            if (nsub == 1) {           // use hw:9,9   and desc
               StrFmt (sdev, "hw:`d,`d", card, sub);
               if (sub < isub)  InsDev ('i', CC(desc), sdev);
               if (sub < osub)  InsDev ('o', CC(desc), sdev);
            }
            else {                     // use hw:9,9,9 and desc2
               StrFmt (sdev, "hw:`d,`d,`d", card, dev, sub);
               if (sub < isub)  InsDev ('i', CC(desc2), sdev);
               if (sub < osub)  InsDev ('o', CC(desc2), sdev);
            }
         }
      }
      ::snd_ctl_close (ctl);
   }
// Dump ();
}


bool MidiDevLst::Get    (char io, char *name, char *type, char *desc, char *dev)
// find io n name w type!=OFF,  fill in other stuff
{  for (ubyte        i = 0;  i < _len;  i++)
      if ( (io == _lst [i].io) && (! StrCm (name, _lst [i].name)) &&
                                     StrCm (_lst [i].type, CC("OFF")) ) {
            StrCp (type, _lst [i].type);
            StrCp (desc, _lst [i].desc);
            StrCp (dev,  _lst [i].dev);
            return true;
      }
   return false;                       // ...nope, not therez :(
}


bool MidiDevLst::GetPos (char io, ubyte pos,
                                  char *name, char *type, char *desc, char *dev)
// find io n num, fill in other stuff
{  for (ubyte n = 0, i = 0;  i < _len;  i++)
      if (io == _lst [i].io) {
         if (n != pos)  n++;
         else {
            StrCp (name, _lst [i].name);
            StrCp (type, _lst [i].type);
            StrCp (desc, _lst [i].desc);
            StrCp (dev,  _lst [i].dev);
            return true;
         }
      }
   return false;                       // ...hit end of list
}


//______________________________________________________________________________
MCCRow MCC [] = { // types are u=0-127(default)
                            // o=0,127(split at 64)
                            // s=0-127=>-64-63
                            // x=tmpo,tsig,ksig,prog
   {"Tmpo",    'x',120,MC_TMPO},  // tempo (scaled unsigned)    (non midi/mapd)
   {"TSig",    'x',260,MC_TSIG},  // time signature             (non midi/mapd)
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
   for (ubyt4 i = 0;  i < NMCC;  i++)  DBG("s=`<8s typ=`c dflt=`3d raw=`s",
MCC [i].s, MCC [i].typ, MCC [i].dflt, MCtl2Str (s, MCC [i].raw));
}


// utils _______________________________________________________________________
ubyt4 Tm (char *st)
// convert 2 int - absolute time (no timesig stuff)
{ char *p;
  ubyt4 b, q = 0, x = 0;
   b = Str2Int (st, & p);      if (b)  --b;      // cuz 1 based
   if (*p == '.') {
      p++;   q = (*p - '0');   if (q)  --q;      // samez
      if (*p == '.')  x = Str2Int (++p);         // x are 0 based tho !
   }
   return b*M_WHOLE + q*(M_WHOLE/4) + x;
}

char *TmS (char *st, ubyt4 t)
// convert 2 str - absolute time (no timesig stuff)
{ ubyt4 b, q, x;
   b = t /  M_WHOLE + 1;       t %=  M_WHOLE;    // 1 based :/
   q = t / (M_WHOLE/4) + 1;    t %= (M_WHOLE/4); // 1 based
   x = t % (M_WHOLE/4);                          // 0 based !!
   if (b > 99999)  return CC("SKIP");
   return StrFmt (st, "`04d.`d.`03d", b, q, x);
}

ubyte MNt (char *s)
{ TStr  t;
  ubyte n;
   StrCp (t, s);
   if (StrLn (t) && (t [StrLn (t)-1] == 'm'))  StrAp (t, CC(""), 1);
   for (n = 0;     n < 12;  n++)  if (! StrCm (t, MKeyStr  [n]))  break;
   if (n >= 12)
      for (n = 0;  n < 12;  n++)  if (! StrCm (t, MKeyStrB [n]))  break;
   if (! StrCm (s, CC("Cb")))  return 11;   // handlin dumb ksigs sigh
   return n;
}

ubyte MKey (char *s, char **news)
{ ubyte n;
  sbyte a = 0;
   if (news)  *news = s;
   if ((*s < '0') || (*s > '9'))          return (ubyte)0;
   for (n = 0;  n < 12;  n++)  if (CHDN (s [1]) == MKeyStr [n][0])  break;
   if (n >= 12)  {if (news) *news += 1;   return (ubyte)0;}
   if (CHDN (s [2]) == 'b')  a = -1;
   if (      s [2]  == '#')  a =  1;
   if (news)  *news += (a ? 3 : 2);
   return (ubyte)((*s-'0'+1)*12 + n + a);
}

char *MKey2Str (char *s, ubyte b, char fl)
{  if (b < MKey (CC("0C")))  b = MKey (CC("0C"));
   if (fl == 'b')  StrFmt (s, "`d`s", b/12-1, MKeyStrB [b%12]);
   else            StrFmt (s, "`d`s", b/12-1, MKeyStr  [b%12]);
   return s;
}

ubyt2 MCtl (char *s)                   // cc raw str to ubyt2
{ ubyt2 rc;
  char *s2;
   if      (! StrCm (s, CC("prog")))   return MC_PROG;
   else if (! StrCm (s, CC("prss")))   return MC_PRSS;
   else if (! StrCm (s, CC("pbnd")))   return MC_PBND;
   else if (! StrCm (s, CC("tmpo")))   return MC_TMPO;
   else if (! StrCm (s, CC("tsig")))   return MC_TSIG;
   else if (! StrCm (s, CC("ksig")))   return MC_KSIG;
   else if (! MemCm (s, CC("cc"), 2))  return MC_CC + (ubyt2)Str2Int (s+2);
   else if (! MemCm (s, CC("us"), 2))  return MC_US + (ubyt2)Str2Int (s+2);
   else if (! MemCm (s, CC("rp"), 2))  rc = MC_RP;
   else if (! MemCm (s, CC("np"), 2))  rc = MC_NP;
   else                                return 0;
   rc += (ubyt2)(Str2Int (s+2, & s2) * 2);
   return rc + (ubyt2)((CHUP (*s2) == 'L') ? 1 : 0);
}

char *MCtl2Str (char *s, ubyt2 c, char raw)      // cc ubyt2 to raw str
{  *s = '\0';
   if (c < 128)  return MKey2Str (s, (ubyte)c);
   if (c < MC_CC) {
      if      (c == MC_PROG)  StrCp (s, CC("Prog"));
      else if (c == MC_PRSS)  StrCp (s, CC("Prss"));
      else if (c == MC_PBND)  StrCp (s, CC("PBnd"));
      else if (c == MC_TMPO)  StrCp (s, CC("Tmpo"));
      else if (c == MC_TSIG)  StrCp (s, CC("TSig"));
      else if (c == MC_KSIG)  StrCp (s, CC("KSig"));
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
      if (r >= NMCC)  {e.valu = e.val2 = 0;   DBG ("CtlX2Str not an x ctl");}
      else            {e.valu = MCC [r].dflt & 0x7F;
                       e.val2 = MCC [r].dflt >> 7;}
   }
   if      (! StrCm (cs, CC("tmpo")))
      StrFmt (s, "`d",  e.valu + (e.val2 << 8));
   else if (! StrCm (cs, CC("tsig")))
      if (e.val2 >> 4)
            StrFmt (s, "`d/`d/`d", e.valu, 1 << (e.val2 & 0x0F),
                                           1 +  (e.val2 >> 4)  );
      else  StrFmt (s, "`d/`d",    e.valu, 1 << (e.val2 & 0x0F));
   else if (! StrCm (cs, CC("ksig"))) {
      if   (! (e.val2 & 0x80))  StrCp (s, MKeyStr  [e.valu]);
      else if (e.valu != 11)    StrCp (s, MKeyStrB [e.valu]);
      else                      StrCp (s, CC("Cb"));
      if (e.val2 & 0x01)  StrAp (s, CC("m"));
      *s = CHUP (*s);
   }             // else "prog"
   else
      StrCp (s, CC("*"));
   return s;
}

void CtlX2Val (TrkEv *e, char *cs, char *s)
{ ubyte r;
  ubyt2 w;
   if (*s == '\0') {
      for (r = 0;  r < NMCC;  r++)  if (! StrCm (cs, MCC [r].s))  break;
      if (r >= NMCC)  {e->valu = e->val2 = 0;   DBG ("CtlX2Val not an x ctl");}
      else            {e->valu = MCC [r].dflt & 0x7F;
                       e->val2 = MCC [r].dflt >> 7;}
   }
   else if (! StrCm (cs, CC("tmpo"))) {
      w = (ubyt2)Str2Int (s);   e->valu = (ubyte)(w & 0xFF);
                                e->val2 = (ubyte)(w >> 8);
   }
   else if (! StrCm (cs, CC("tsig"))) {
      e->valu = (ubyte)Str2Int (s, & s);   if (*s == '/')  s++;
      w       = (ubyt2)Str2Int (s, & s);
      for (r = 0;  r < 16;  r++)  if ((1 << r) == w)  {e->val2 = r;   break;}
      if (*s == '/')  {w = (ubyt2)Str2Int (++s);   e->val2 |= ((w-1) << 4);}
   }
   else if (! StrCm (cs, CC("ksig"))) {
      e->val2 = (s [StrLn (s)-1] == 'm') ? 1 : 0;
      e->valu = MNt (s);
      if (s [1] == 'b')  e->val2 |= 0x80;
   }
   else                  // "prog"
      e->valu = e->val2 = 0;
}


//______________________________________________________________________________
MidiO::MidiO (char *name, char noinit)
{ int err;
   _hnd = nullptr;   StrCp (_name, name);   MemSet (_ntOn, 0, sizeof (_ntOn));
   _syn = false;
   if (! Midi.Get ('o', _name, _type, _desc, _dev))
      {DBG("MidiO no device name=`s",  _name);    return;}
   if (*_dev == '?')
      {DBG("MidiO device `s isn't on", _name);    return;}
//TRC("MidiO::MidiO `s.`s.`s  dev=`s", _name, _type, _desc, _dev);
   if (! StrCm (_type, CC("syn"))) {   // fake handle just so not Dead()
      _syn = true;   _hnd = (snd_rawmidi_t *)1;
      return;                          // GMInit in SynBnk ();
   }
   if ((err = ::snd_rawmidi_open (nullptr, & _hnd, _dev, SND_RAWMIDI_NONBLOCK)))
      {DBG("snd_rawmidi_open o `s failed: `s", _name, ::snd_strerror (err));
       _hnd = nullptr;   return;}
   if (! noinit)  GMInit ();
}


MidiO::~MidiO (void)
// toss the header thing;  shush the notes left on;  reset n close
{ int err;
//TRC("MidiO::~MidiO `s", (*_name) ? _name : "?");
   if (Dead ())  {TRC("...was dead");   return;}
   for (ubyte c = 0;  c < 16;  c++)  Put (c, MC_CC|M_ASOFF);
   if (! StrCm (_type, CC("syn")))
      {_hnd = nullptr;   return;}
   if ((err = ::snd_rawmidi_drain (_hnd)))
      DBG("snd_rawmidi_drain o `s failed: `s", _name, ::snd_strerror (err));
   if ((err = ::snd_rawmidi_close (_hnd)))
      DBG("snd_rawmidi_close o `s failed: `s", _name, ::snd_strerror (err));
   _hnd = nullptr;
}


void MidiO::PutMEv (ubyte *mev, ubyte ln)
{ int err;
   if (Dead ())  {DBG("PutMEv `s but Dead :(", _name);   return;}
// for INEVitable tracing...:/
//DBG("MidiO::PutMEv on `s/`s/`s ln=`d", _name, _type, _desc, ln);
/*
  TStr s, dc;
  char ch;
   StrFmt (dc, " `s.`d ", _name, (mev[0] & 0x0F)+1);
   switch (mev [0] & 0xF0) {
      case M_NOTE:
      case M_NOFF:
      case M_NPRS:                       ch = '~';
         if ((mev[0] & 0xF0) == M_NOTE)  ch = '_';
         if ((mev[0] & 0xF0) == M_NOFF)  ch = '^';
         if ((mev[0] & 0x0F) == 9)  MDrm2Str (s, mev [1]);
         else                       MKey2Str (s, mev [1]);
         DBG("`s`s`c`d",     dc, s, ch, mev [2]);
         break;
      case M_PROG:
         DBG("`sProg=`s.`d", dc, MProg [mev[1]], mev[1]);
         break;
      case M_PBND:
         DBG("`sPBnd=`d",    dc, 8192-(mev[1] + mev[2]*128));
         break;
      case M_PRSS:
         DBG("`sPrss=`d",    dc, mev[1]);
         break;
      case M_CTRL:
         switch (mev [1]) {
            case M_NRPNL:  StrCp (s, CC("NRPL."));   break;
            case M_NRPNH:  StrCp (s, CC("NRPH."));   break;
            case M_RPNL:   StrCp (s, CC("RPL."));    break;
            case M_RPNH:   StrCp (s, CC("RPH."));    break;
            case M_DATH:   StrCp (s, CC("DatH."));   break;
            case M_DATL:   StrCp (s, CC("DatL."));   break;
            case M_BANK:   StrCp (s, CC("Bank."));   break;
            case M_BNKL:   StrCp (s, CC("BnkL."));   break;
            case M_MOD:    StrCp (s, CC("Mod."));    break;
            case M_BRTH:   StrCp (s, CC("Brth."));   break;
            case M_PEDL:   StrCp (s, CC("Pedl."));   break;
            case M_VOL:    StrCp (s, CC("Vol."));    break;
            case M_EXPR:   StrCp (s, CC("Expr."));   break;
            case M_PAN:    StrCp (s, CC("Pan."));    break;
            case M_BAL:    StrCp (s, CC("Bal."));    break;
            case M_HOLD:   StrCp (s, CC("Hold."));   break;
            case M_HLD2:   StrCp (s, CC("Hld2."));   break;
            case M_SOFT:   StrCp (s, CC("Soft."));   break;
            case M_SUST:   StrCp (s, CC("Sust."));   break;
            case M_LEGA:   StrCp (s, CC("Lega."));   break;
            case M_RVRB:   StrCp (s, CC("Rvrb."));   break;
            case M_CHOR:   StrCp (s, CC("Chor."));   break;
            case M_ASOFF:  StrCp (s, CC("ASOff."));  break;
            case M_ACOFF:  StrCp (s, CC("ACOff."));  break;
            case M_LOCAL:  StrCp (s, CC("Local."));  break;
            case M_ANOFF:  StrCp (s, CC("ANOff."));  break;

            default:       StrCp (s, CC("?."));      break;
         }
         DBG("`s`s`d=`d",    dc, s, mev[1], mev[2]);
         break;
      default:
         DBG("`sln=`d ??? `d `d `d `d (x`02x `02x `02x `02x)",
             dc, ln, mev[0], mev[1], mev[2], mev[3],
                     mev[0], mev[1], mev[2], mev[3]);
   }
*/
   if ((err = ::snd_rawmidi_write (_hnd, mev, ln)) != ln)
      DBG("snd_rawmidi_write o `s failed: rc=`d <> ln=`d", _name, err, ln);
}


void MidiO::Put (ubyte ch, ubyt2 c, ubyte v, ubyte v2)
// build a midi event given args
// do notes (note/nprs/noff - keepin track of which chan/notes are on),
{
//DBG("MidiO::Put on `s.`s ch=`d c=`d v=`d v2=`d", _name, _type, ch, c, v, v2);
#ifdef USE_SYN
   if (_syn)  return Sy.Put (ch, c, v, v2);
#endif

  ubyte mev [4], p, ln = 3;
  ubyt4 m;
  ubyte mvol [8] = {0xF0,0x7F,0x7F,0x04,0x01,0,0,0xF7};
  ubyte mbal [8] = {0xF0,0x7F,0x7F,0x04,0x02,0,0,0xF7};
//ubyte mtun [8] = {0xF0,0x7F,0x7F,0x04,0x04,0,0,0xF7}; 3,4 are +-cent/8192,cent
   mev [0] = ch;  mev [1] = (ubyte)c;  mev [2] = 0x7F & v;
   if      (c < MC_PROG) {             // just a note
      p = (ch << 2) | (c >> 5);   m = 1 << (c & 0x1F);
      if (v & 0x80) // {if ((_ntOn [p] & m) == 0)
                            {mev [0] |= M_NOTE;  _ntOn [p] |= ( m);}
                    //  else mev [0] |= M_NPRS;}
      else                  {mev [0] |= M_NOFF;  _ntOn [p] &= (~m);}
   }
   else if (c < MC_CC) {               // std midi
      if      (c == MC_PROG) {mev [0] |= M_PROG;  mev [1] = v;}
      else if (c == MC_PRSS) {mev [0] |= M_PRSS;  mev [1] = v;}
      else if (c == MC_PBND) {mev [0] |= M_PBND;  mev [1] = v2;}
      else return;
      ln = 2;
   }
   else if (c < MC_US) {               // regular cc
      mev [0] |= M_CTRL;  mev [1] = c - MC_CC;
   }
   else if (c < MC_RP) {               // univ sysex
      if (c == MC_MVOL) {
         mvol [6] = _MVol = v;  mvol [5] = v ? 0x7F : 0;   PutMEv (mvol, 8);
      }
      if (c == MC_MBAL) {
         mbal [6] = _MBal = v;                             PutMEv (mbal, 8);
      }
//TODO: MC_MTUN
      return;
   }
   else if (c < MC_NP) {               // reg param
      mev [0] |= M_CTRL;
      mev [1] = M_RPNH;    mev [2] = ((c - MC_RP) >> 8) & 0x7F;   PutMEv (mev);
      mev [1] = M_RPNL;    mev [2] = ((c - MC_RP) >> 1) & 0x7F;   PutMEv (mev);
      mev [1] = (c & 0x0001) ? M_DATL : M_DATH;   mev [2] = v;
   }
   else {                              // nonreg param
      mev [0] |= M_CTRL;
      mev [1] = M_NRPNH;   mev [2] = ((c - MC_NP) >> 8) & 0x7F;   PutMEv (mev);
      mev [1] = M_NRPNL;   mev [2] = ((c - MC_NP) >> 1) & 0x7F;   PutMEv (mev);
      mev [1] = (c & 0x0001) ? M_DATL : M_DATH;   mev [2] = v;
   }
   PutMEv (mev, ln);
}


void MidiO::DumpOns ()
{ ubyte p;
  ubyt4 m;
  TStr  ts;
TRC("DumpOns on `s.`s", _name, _type);
   for (ubyte ch = 0;  ch < 16;  ch++)  for (ubyte nt = 0;  nt < 128;  nt++) {
      p = (ch << 2) | (nt >> 5);   m = 1 << (nt & 0x1F);
      if (_ntOn [p] & m)
TRC("   ch=`d nt=`s", ch, MKey2Str(ts, nt));
   }
}


void MidiO::NotesOff (ubyte nch)
{ ubyte c = 9;
TRC("NotesOff on `s.`s", _name, _type);
   Put     (c, MC_CC|M_ASOFF, 0, 0);   Put (c, MC_CC|M_HOLD, 0, 0);
   for (ubyte c = 0;  c < nch;  c++)
      {Put (c, MC_CC|M_ASOFF, 0, 0);   Put (c, MC_CC|M_HOLD, 0, 0);}
   MemSet (_ntOn, 0, sizeof (_ntOn));
}


void MidiO::GMInit (ubyte nch)
{ ubyte c = 9;
TRC("GMInit on `s.`s nch=`d", _name, _type, nch);
   Put (0, MC_MVOL, 127);
   Put (0, MC_MBAL, 64);         // non chan
   for (ubyte c = 0;  c < nch;  c++) {
      if (c != 9)
      {Put (c, MC_PROG);
      Put (c, MC_CC|M_BANK);           Put (c, MC_CC|M_BNKL);}
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
   c = 9;
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


//______________________________________________________________________________
void MidiI::run ()                     // poll loop runnin in sep thread
{ ubyte buf [256];
  ubyt2 re;
  int   i, ln, err, npf;
  TStr  x;
  struct pollfd *pf;
StrFmt(x, "MidiI_`s", _name);   DBGTH(x);
TRC("run  type=`s desc=`s", _type, _desc);
   snd_rawmidi_read (_hnd, nullptr, 0);      // trigger reading
   npf = snd_rawmidi_poll_descriptors_count (_hnd);
   pf  = (struct pollfd *)alloca (npf * sizeof (struct pollfd));
   snd_rawmidi_poll_descriptors (_hnd, pf, npf);
   while (_run) {
      err = poll (pf, npf, 500);       // timeout at 1/2 sec (500 millisec)
      if (err < 0)
         {DBG ("poll failed: `s", strerror (errno));   break;}

      err = snd_rawmidi_poll_descriptors_revents (_hnd, pf, npf, & re);
      if (err < 0)
         {DBG("s_r_poll_d_revents failed: `s", snd_strerror (err));   break;}
      if (re & (POLLERR | POLLHUP))
         {DBG("s_r_poll_d_revents ERR/HUP");   break;}

      err = snd_rawmidi_read (_hnd, buf, sizeof (buf));
      if (err == -EAGAIN)  continue;   // just ain't nothin therez

      if (err < 0)
         {DBG ("s_r_read failed: `s", snd_strerror (err));   break;}
      for (ln = i = 0;  i < err;  i++)
         if (buf [i] != MIDI_CMD_COMMON_CLOCK &&
             buf [i] != MIDI_CMD_COMMON_SENSING) {
            buf [ln++] = buf [i];
//DBG("   `s: `02x", _name, buf [i]);
//not sure bout sysex or other devices n stuff - weeee'll seee
         }
      if (ln == 0)  continue;
      if (ln >  4)  continue;
      emit Event (buf [0], (ln > 1) ? buf [1] : 0,
                           (ln > 2) ? buf [2] : 0,
                           (ln > 3) ? buf [3] : 0);
   }
   _run = false;
TRC("run end");
}


void MidiI::EvIns (ubyte s, ubyte ci, ubyte v, ubyte v2)
{ ubyte p, pnew, t;
  ubyt2 c;
  bool  ok = false;
   c = ci;
//DBG("EvIns `02x `02x `02x `02x", s, c, v, v2);
   pnew = p = _bAdd;  if (++pnew == BITS (_buf))  pnew = 0;
   if (pnew == _bRmv)  {_bErr = true;   DBG("MidiI::EvIns FULL");   return;}

   if (_timer) {_buf [p].time = _timer->Get ();
                _buf [p].msec = _timer->MS ();}
   else         _buf [p].time = _buf [p].msec = 0;
   switch (s & 0xF0) {
      case M_NPRS: v2 = 0x80;
      case M_NOTE: if (v)  v |= 0x80;
      case M_NOFF: if ((c >= MKey (CC("0C"))) &&
                       (c <= MKey (CC("8B"))))    ok = true;
                                                               break;
      case M_PROG: v = (ubyte)c;   c = MC_PROG;   ok = true;   break;
      case M_PRSS: v = (ubyte)c;   c = MC_PRSS;   ok = true;   break;
      case M_PBND: v2 = (ubyte)c;  c = MC_PBND;   ok = true;   break;
      case M_CTRL:
         switch (c) {
            case M_NRPNL:
               _rpn = 0x8000 | (_rpn & 0x7F00) | (v<<1);     break;
            case M_NRPNH:
               _rpn = 0x8000 | (v << 8) | (_rpn & 0x00FF);   break;
            case M_RPNL:
               _rpn =          (_rpn & 0x7F00) | (v<<1);     break;
            case M_RPNH:
               _rpn =          (v << 8) | (_rpn & 0x00FF);   break;
            case M_DATL:
            case M_DATH:
               t = (ubyte)c;   c = _rpn;
               if (! (c & 0x8000))  if (c < 16384)  c += MC_NP;
               if (t == M_DATH)  c++;
               break;
            default:
               c |= MC_CC;
         }
         ok = true;  break;
   }
   if (ok) {
      _buf [p].chan = s & 0x0F;
      _buf [p].ctrl = c;
      _buf [p].valu = v;
      _buf [p].val2 = v2;
// cuz you know...
//DBG("MidiIEvIns `s.`d `04x `02x `02x",_name,(s & 0x0F)+1,c,v,v2);
      _bAdd = pnew;
      emit MidiIEv ();
   }
}


bool MidiI::Get (MidiEv *ev)
{  if (Dead () || (_bRmv == _bAdd))  return false;
   MemCp (ev, & _buf [_bRmv], sizeof (MidiEv));
  ubyte p = _bRmv;
   if (++p == BITS (_buf))  p = 0;
   _bRmv = p;
   return true;
}


void MidiI::BufAdj (sbyt4 tm)          // offset all our buffered times :/
{ sbyt4 t;
  ubyte p;
   if (Dead ())  return;
   for (p = _bRmv;  p != _bAdd;) {
      t = (sbyt4)_buf [p].time + tm;   _buf [p].time = (ulong)t;
      if (++p == BITS (_buf))  p = 0;
   }
}


MidiI::MidiI (char *name, Timer *tmr)
// timer needed to stamp MidiEv.time
{ int err;
   _hnd = nullptr;   _timer = tmr;   _bAdd = _bRmv = 0;   _bErr = false;
   StrCp (_name, name);
//TRC("MidiI `s", _name);
   if (! Midi.Get ('i', _name, _type, _desc, _dev))
      {DBG("MidiI no device name=`s",  _name);   return;}
   if (*_dev == '?')
      {DBG("MidiI device `s isn't on", _name);   return;}
//TRC("   `s.`s.`s  dev=`s", _name, _type, _desc, _dev);
   if ((err = ::snd_rawmidi_open (& _hnd, nullptr, _dev, SND_RAWMIDI_NONBLOCK)))
      {DBG("snd_rawmidi_open i `s failed: `s", _name, ::snd_strerror (err));
       _hnd = nullptr;   return;}
   connect (this, & MidiI::Event,    this, & MidiI::EvIns);
   connect (this, & MidiI::finished, this, & QObject::deleteLater);
   _run = true;
   start ();
}


MidiI::~MidiI (void)
{ int err;
//TRC("~MidiI `s", *_name ? _name : "?");
   if (Dead ())  {TRC("...was dead");   return;}
   if (_run)  {_run = false;   wait ();}
   if ((err = ::snd_rawmidi_close (_hnd)))
      DBG("snd_rawmidi_close i `s failed: `s", _name, ::snd_strerror (err));
   _hnd = nullptr;
}

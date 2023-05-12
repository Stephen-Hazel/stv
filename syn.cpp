// syn.cpp - easy(ish) softsynth based on plain jane .WAVs

#include "syn.h"

SndO Sn;
Syn  Sy;                               // welp, that's everything...


char *R2Str (real f, char *s)          // fer dbg
{ bool neg;
  int  i, ln;
  TStr t;
   neg = (f < 0.) ? true : false;   if (neg) f = -f;
   i = (int)f;   f -= (real)i;
   StrFmt (t, "`09d", (int)(f*1000000000.));
   while ((ln = StrLn (t)))  {if (t [--ln] == '0')  t [ln] = '\0';  else break;}
   return StrFmt (s, "`s`d`s`s", neg?"-":"", i, *t?".":"", t);
}


//______________________________________________________________________________
// lookup stuffsss
real     Interp [MAX_INTERP][7];       // interpolation coefficients
void InitInterp ()
{ real v, iShf;
   for (ubyte i = 0; i < 7; i++)
      for (ubyt2 j = 0; j < MAX_INTERP; j++) {
         iShf = (real)i - (7. / 2.) + (real)j / (real)MAX_INTERP;
         if (fabs (iShf) <= 0.000001)  v = 1.;
         else {v = sin (iShf * M_PI) / (M_PI * iShf);
               v *= 0.5 * (1. + cos (2.*M_PI * iShf / 7.));}
         Interp [MAX_INTERP-1 - j][i] = v;
      }
}

real     Dither [2][MAX_DITHER];       // per l/r channel
void InitDither (void)                 // rand real btw -.999 and +.999
{ real  d, dp;
  sbyt4 c, i;
   for (c = 0; c < 2; c++) {
      dp = 0;
      for (i = 0; i < MAX_DITHER-1; i++) {
         d             = rand () / (real)RAND_MAX - 0.5f;
         Dither [c][i] = d - dp;
         dp            = d;
      }
      Dither [c][MAX_DITHER-1] = 0 - dp;
   }
}
                                       // conversion tables
real Cnv_ct2hz [1200];                 // cents  2 hz
real Cnv_pan   [127];                  // 128    2 pan

real Ct2Hz (real ct)
{  if (ct <     0.) return (real)    1.;
   if (ct <   900.) return (real)  6.875 * Cnv_ct2hz [(sbyt4)(ct +   300)];
   if (ct <  2100.) return (real)  13.75 * Cnv_ct2hz [(sbyt4)(ct -   900)];
   if (ct <  3300.) return (real)   27.5 * Cnv_ct2hz [(sbyt4)(ct -  2100)];
   if (ct <  4500.) return (real)   55.  * Cnv_ct2hz [(sbyt4)(ct -  3300)];
   if (ct <  5700.) return (real)  110.  * Cnv_ct2hz [(sbyt4)(ct -  4500)];
   if (ct <  6900.) return (real)  220.  * Cnv_ct2hz [(sbyt4)(ct -  5700)];
   if (ct <  8100.) return (real)  440.  * Cnv_ct2hz [(sbyt4)(ct -  6900)];
   if (ct <  9300.) return (real)  880.  * Cnv_ct2hz [(sbyt4)(ct -  8100)];
   if (ct < 10500.) return (real) 1760.  * Cnv_ct2hz [(sbyt4)(ct -  9300)];
   if (ct < 11700.) return (real) 3520.  * Cnv_ct2hz [(sbyt4)(ct - 10500)];
   if (ct < 12900.) return (real) 7040.  * Cnv_ct2hz [(sbyt4)(ct - 11700)];
   if (ct < 14100.) return (real)14080.  * Cnv_ct2hz [(sbyt4)(ct - 12900)];
   return                  (real)    1.;
}

real Pan (ubyte c, ubyte lr)
{  if (c ==  0)  c =   1;              // so -64 => -63
   if (c > 127)  c = 127;              // limit at -63 .. 63
   c--;   if (lr == 0)  c = 126 - c;
   return Cnv_pan [c-1];
}

void InitLookup (void)
{ sbyt4 i;
  real  x;
   for (i = 0;  i < BITS (Cnv_ct2hz);  i++)
      Cnv_ct2hz [i] = (real) pow (2., (real) i / 1200.);
   x = M_PI/2. / (BITS (Cnv_pan) - 1.);
   for (i = 0;  i < BITS (Cnv_pan);  i++)  Cnv_pan [i] = (real) sin (i * x);
//TStr ts;
//for (i = 0; i < BITS (Cnv_pan); i++)
//DBG("pan `d `s", i, R2Str (Cnv_pan[i],ts));
   InitInterp ();   InitDither ();
}


//______________________________________________________________________________
// sound (.WAV) part of syn
#define EVEN(n)     ((n)&0xFFFFFFFE)
#define EVEN_UP(n)  EVEN((n)+1)

const  real   MAXS4 = 2147483648.;

static TStr   WavFn [MAX_SAMP*2];      // hold sounds' wavs while loading
static Sample TSmp  [MAX_SAMP];        // sample is mostly just a data struct
                                       // (little code to it)

void Sample::Dump (bool dr)
{ TStr s1, s2;
   DBG("   smp=`s", fn);
   DBG("       mnKey=`d/`s mxKey=`d/`s mnVel=`d mxVel=`d",
       mnKey, dr ? MDrm2Str(s1,mnKey) : MKey2Str(s1,mnKey),
       mxKey, dr ? MDrm2Str(s2,mxKey) : MKey2Str(s2,mxKey), mnVel, mxVel);
   DBG("       len=`d lpBgn=`d lr=`d pos=`d",
       len, lpBgn, lr, pos);
   DBG("       frq=`s key=`d/`s cnt=`d bytes=`d chans=`d",
       R2Str (frq, s1), key, dr ? MDrm2Str(s2,key) : MKey2Str(s2,key),
       cnt, bytes, chans);
}


static int TSmCmp (void *p1, void *p2)
// find first numeric char and get oct,note from there to sort by
{ Sample *s1 = (Sample *)p1, *s2 = (Sample *)p2;
  int i;
   if ((i = s1->mxKey - s2->mxKey))  return i;
   if ((i = s1->mxVel - s2->mxVel))  return i;
   return  s1->lr    - s2->lr;
}


//______________________________________________________________________________
void Sound::Dump ()
{  DBG("   snd=`s nSmp=`d xFrq=`b xRls=`b siz=`d mxDat=`d",
       _nm, _nSmp, _xFrq, _xRls, _siz, _mxDat);
//    DBG("      dir=`s", _pa);
}


ubyt4 Sound::LoadDat (ubyte *dat, ubyt4 pos)
// actually load each .WAV we found into smp buffer n offset our poss by len
{ TStr  fn;
  File  f;
  sbyt4 smp;
  ubyt4 lenD, s, p1 = pos;
  ubyt2 i;
  ubyte nch, nby, flo, *p;
  real  sr;
  bool  got;
  struct {char tag [4];  ubyt4 siz;} ch;
  char          id [4];
TRC("{ Sound::LoadDat  pos=`d", pos);
   for (i = 0;  i < _nSmp;  i++) {
      _max = 0.0;
      got = false;
      StrCp (fn, _pa);   StrAp (fn, _smp [i].fn);  // already know it opens
//TRC("i=`d/`d fn=.../`s'", i, _nSmp, _smp [i].fn);
      f.Open (fn, "r");   f.Get (& ch, sizeof (ch));
                          f.Get (id,   sizeof (id));
      while (f.Get (& ch, sizeof (ch)) == sizeof (ch)) {
         if (! MemCm (ch.tag, CC("data"), 4, 'x'))
             {lenD = ch.siz;   if (f.Get (dat, lenD) == lenD)  got = true;}
         else f.Seek (EVEN_UP (ch.siz), CC("."));  // skip other Chunks
      }
      f.Shut ();
      if (! got) {
DBG("Sound::LoadDat  no WAV data chunk in `s", fn);
         return 0;
      }
//TRC("lenD=`d", lenD);
   // got it, so convert it to real samples now
      nch = _smp [i].chans;   nby = _smp [i].bytes;   flo = _smp [i].flopt;
      _smp [i].pos = pos;
      for (p = dat, s = 0;  s < _smp [i].len;  s++) {
         if (flo)  {sr = (nby <= 4) ? *((float *)p) : *((double *)p);
                    p +=  nby;}
         else {
            smp = 0;
            switch (nby) {          // single byte is weird; 2,3 similar; 4ez
               case 1:  smp = ((sbyt4)(*p++) - 128) << 24;          break;
               case 3:  smp |= *p++ <<  8;
               case 2:  smp |= *p++ << 16;   smp |= (*p++ << 24);   break;
               case 4:  smp = *((sbyt4 *)p);   p += 4;              break;
            }
            sr = (real)smp / MAXS4;
         }
         if (fabs (sr) > _max)  _max = fabs (sr);
         Sy._smp [pos++] = sr;
         p += (nby*(nch-1));        // skip smps in any unused chans
      }

   // check if this file does 2 _smp[] entries (stereo r side)
      if ( (i+1 < _nSmp) && (! StrCm (_smp [i+1].fn, _smp [i].fn)) &&
                                     (_smp [i+1].lr == 1) ) {
//TRC("got 2nd smp i=`d/`d pos=`d", i+1, _nSmp, pos);
         _smp [++i].pos = pos;
         for (p = & dat [nby], s = 0;  s < _smp [i].len;  s++) {
            if (flo)  {sr = (nby <= 4) ? *((float *)p) : *((double *)p);
                       p +=  nby;}
            else {
               smp = 0;
               switch (nby) {
                  case 1:  smp = ((sbyt4)(*p++) - 128) << 24;          break;
                  case 3:  smp |= *p++ <<  8;
                  case 2:  smp |= *p++ << 16;   smp |= (*p++ << 24);   break;
                  case 4:  smp = *((sbyt4 *)p);   p += 4;              break;
               }
               sr = (real)smp / MAXS4;
            }
            if (fabs (sr) > _max)  _max = fabs (sr);
            Sy._smp [pos++] = sr;
            p += (nby*(nch-1));        // skip smps in any unused chans
         }
      }
   // ok, scale them samples so ALL sounds have a max range of +-1.0
      for (;  p1 < pos;  p1++)  Sy._smp [p1] *= (1.0 / _max);
   }
TRC("} Sound::LoadDat  new pos=`d", pos);
   return pos;
}


bool Sound::LoadFmt (char *wfn, ubyte ky, ubyte vl)
// load a .WAV file header's fmt chunk into Sample
// _L, _R suffix means they're mono but should be paired up for stereo
// if there's only one sample (or one _L, one _R),
// it's 0-127 for both key and vel (max) range
{ TStr  fn;
  File  f;
  ubyt4 ln, lenD;
  ubyte bt, ch;
  real  fr;
  struct {char tag [4];  ubyt4 siz;} chk;
  char          id [4], got [3];
  WAVEFORMATEXTENSIBLE  wf;
  struct {
     ubyt4 manuf;  ubyt4 prod;  ubyt4 per;  ubyt4 note;  ubyt4 frac;
     ubyt4 sfmt;   ubyt4 sofs;  ubyt4 num;  ubyt4 dat;   ubyt4 cue;
     ubyt4 loop;   ubyt4 bgn;   ubyt4 end;  ubyt4 frc;   ubyt4 times;
  } smpl;
  Sample *s = & TSmp [_nSmp];
   StrCp (fn, _pa);   StrAp (fn, wfn);   StrCp (s->fn, wfn);
   if (! f.Open (fn, "r"))          // _pa always ends w '/'
      {DBG ("Sound::LoadFmt  can't read `s", fn);           return false;}
   if ( (f.Get (& chk, sizeof (chk)) != sizeof (chk)) ||
         MemCm (chk.tag, CC("RIFF"), 4, 'x') )
      {DBG ("Sound::LoadFmt  bad WAV file `s", fn);         return false;}
   if ( (f.Get (id, sizeof (id)) != sizeof (id)) ||
         MemCm (id,     CC("WAVE"), 4, 'x') )
      {DBG ("Sound::LoadFmt  bad WAV file 2 `s", fn);       return false;}
   MemSet (got, 0, sizeof (got));
   while (f.Get (& chk, sizeof (chk)) == sizeof (chk)) {
      if      (! MemCm (chk.tag, CC("fmt "), 4, 'x')) {
         got [0] = 'y';
         if (chk.siz > sizeof(wf))
            {DBG ("Sound::LoadFmt  bad WAV fmt `s", fn);    return false;}
         if (f.Get (& wf, chk.siz) != chk.siz)
            {DBG ("Sound::LoadFmt  eof in fmt `s", fn);     return false;}
      }
      else if (! MemCm (chk.tag, CC("data"), 4, 'x')) {
         got [1] = 'y';
         lenD = chk.siz;
         f.Seek (EVEN_UP (chk.siz), CC("."));     // skip fer now
      }
      else if (! MemCm (chk.tag, CC("smpl"), 4, 'x')) {
         got [2] = 'y';
         if (chk.siz > sizeof(smpl)) {
            if (f.Get (& smpl, sizeof(smpl)) != sizeof(smpl))
               {DBG ("Sound::LoadFmt  eof1 in smpl `s", fn); return false;}
            f.Seek (EVEN_UP(chk.siz - sizeof(smpl)), CC("."));
         }
         else
            if (f.Get (& smpl, chk.siz) != chk.siz)
               {DBG ("Sound::LoadFmt  eof2 in smpl `s", fn); return false;}
      }
      else
         f.Seek (EVEN_UP (chk.siz), CC("."));      // skip unused Chunks
   }
   f.Shut ();
   if (! got [0])  {DBG ("Sound::LoadFmt  no WAV fmt", fn);   return false;}
   if (! got [1])  {DBG ("Sound::LoadFmt  no WAV data",fn);   return false;}
   if (! got [2])  {MemSet (& smpl, 0, sizeof (smpl));
                    smpl.note = 60;   smpl.bgn = smpl.end = lenD;}
   fr = (real)   wf.Format.nSamplesPerSec;    // ^ no smpl chunk - no loopin
   ch = (ubyte)  wf.Format.nChannels;         // falls into loop check below
   bt = (ubyte) (wf.Format.nBlockAlign / ch);
   ln = lenD / (ch * bt);
   if ((smpl.bgn >= ln) || (smpl.end > ln) || (smpl.end <= smpl.bgn)) {
//DBG("loop off for `s (len=`d lpBgn=`d lpEnd=`d)", fn,ln,smpl.bgn,smpl.end);
      smpl.bgn = ln;
   }
   if ((_nSmp + ch) > BITS (TSmp))
      {DBG ("Sound::LoadFmt  too many samples `s", fn);   return false;}

   s->bytes = bt;   s->chans = ch;   s->pos = 0;   s->len = ln;
   s->frq   = fr;
   if      (_xFrq || _xRls)              s->lpBgn = ln;
   else if ((s->lpBgn = smpl.bgn) < ln)  s->len = ln = smpl.end;

   if      (wf.Format.wFormatTag == WAVE_FORMAT_PCM)        s->flopt = false;
/* else if (wf.Format.wFormatTag == WAVE_FORMAT_IEEE_FLOAT) s->flopt = true;
** else if (wf.Format.wFormatTag == WAVE_FORMAT_EXTENSIBLE) {
**    if      (wf.SubFormat == KSDATAFORMAT_SUBTYPE_PCM)    s->flopt = false;
**    else if (wf.SubFormat == KSDATAFORMAT_SUBTYPE_IEEE_FLOAT)
**                                                          s->flopt = true;
**    else  {DBG ("Sound::LoadFmt  unknown format", fn);   return false;}
** }
*/
   else     {DBG ("Sound::LoadFmt  unknown format", fn);   return false;}

   s->key = (ubyte)smpl.note;
   s->cnt = (ubyte)((ubyt4)smpl.frac / ((ubyt4)0x80000000/50));
   s->mxKey = ky;   s->mxVel = vl;
   _siz += ln;   if (lenD > _mxDat)  _mxDat = lenD;
   _nSmp++;
   if (ch == 1)                     // mono
      s->lr = StrSt (wfn, CC("_L.WAV")) ? 0 :
             (StrSt (wfn, CC("_R.WAV")) ? 1 : 2);
   else {                           // stereo
      MemCp (s+1, s, sizeof (TSmp[0]));
      s->lr = 0;   s++;   s->lr = 1;   _siz += ln;   _nSmp++;
   }
   return true;
}


bool Sound::SndDir (char *snd, char *dss, char *dds)
// split out sampset,drumset and make _pa (actual sound dir in filesys)
// set _xFrq,_xRls too
// lookin up defaults if needed.  _pa doesn't end w / YET !!  that's later
{ TStr  dr, ss, ds, mn, dfd [1024];
  char *p;
  ubyt4 i, j, ig;
  FDir  d;
  Path  pa;
  File  f;
TRC("SndDir bgn _nm=`s dss=`s dds=`s", snd, dss, dds);
   StrCp (_nm, snd);   App.Path (_pa, 'd');
                       StrAp (_pa, CC("/device/syn/"));
   ig = StrLn (_pa);                // don't care bout that part o path

// drum - pretty rough
   if      (! MemCm (_nm, CC("Drum/"), 5)) {
      StrCp (dr, & _nm [5]);   *ss = *ds = *mn = '\0';
      if ((p = StrCh (dr, ':'))) {
         *p++ = '\0';   StrCp (ds, p);
         if ((p = StrCh (ds, '/')))
            {*p++ = '\0';   *mn = '_';   StrCp (& mn [1], p);}
      }
      if ((p = StrCh (dr, '|')))  {*p++ = '\0';   StrCp (ss, p);}
      if (*ss == '\0')  StrCp (ss, dss);
      if (*ds == '\0')  StrCp (ds, dds);
      StrFmt (& _pa [StrLn (_pa)], "`s/Drum/`s/`s`s", ss, ds, dr, mn);
   }

// melodic etc  etc|sampset/name
   else if (! MemCm (_nm, CC("etc|"), 4)) {
      StrCp (ss, & _nm [4]);
      if (! (p = StrCh (ss, '/'))) {
DBG("Sound::SndDir  no / in `s", _nm);
         return false;
      }
      *p++ = '\0';
      StrFmt (& _pa [StrLn (_pa)], "`s/etc/`s", ss, p);
   }

// melodic gm  gmDir/gmSnd[|sset][/more]
   else {
      StrCp (dr, _nm);
   // split off /more if got
      if (! (p = StrCh (dr, '/'))) {     // 1st / BETTER be there
DBG("no / in `s", _nm);
         return false;
      }
      if ((p = StrCh (++p, '/')))
         {*p++ = '\0';   *mn = '_';   StrCp (& mn [1], p);}
      else               *mn = '\0';
   // split off |sset if got
      if ((p = StrCh (dr, '|')))  {*p++ = '\0';   StrCp (ss, p);}
      else                                        StrCp (ss, dss);
   // ok, got path now
      StrFmt (& _pa [StrLn (_pa)], "`s/`s`s", ss, dr, mn);
   }

   _xFrq = _xRls = false;           // no pitching/no release on noteUp
   i = StrLn (_pa);                 // special sound suffix - drum,hold,clip
   if (i > 5) {
      if ((! MemCm (_nm, CC("Drum/"), 5)) ||
          (! MemCm (& _pa [i-5], CC("_drum"), 5))) _xFrq = true;
      if  (! MemCm (& _pa [i-5], CC("_hold"), 5))  _xRls = true;
      if  (! MemCm (& _pa [i-5], CC("_clip"), 5))  _xFrq = _xRls = true;
   }
// if default sampset, look for it with any _morename
   if ( (! d.Got (_pa)) && (! StrCm (ss, dss)) ) {
TRC(" resolving path=`s for GM", _pa);
      StrCp (dr, _pa);   Fn2Path (dr);   // dr has par dir
      StrCp (_pa, & _pa [StrLn (dr)+1]); // _pa has just fn prefix
      i = pa.DLst (dr, dfd, (ubyt2)BITS (dfd));
      for (j = 0;  j < i;  j++)  if (! MemCm (dfd [j], _pa, StrLn (_pa)))
         {StrFmt (_pa, "`s/`s", dr, dfd [j]);   break;}
      if (j >= i) {
DBG("SndDir can't resolve GM snd `s in dir `s", _pa, dr);
         return false;
      }
   }
TRC("SndDir end _pa=`s xFrq=`b xRls=`b", & _pa [ig], _xFrq, _xRls);
   return true;
}


Sound::Sound (char *snd, ubyte dKey, char *dss, char *dds)
// list dir of .WAVs into w[] and load each wav hdr  (load samples later)
// snd w _hold suffix=> no release ramp on ntUp, _drum suffix=> un-pitched
// _clip suffix for both ^                    (or in a drum dir ^ )
{ Path  pa;
  ubyt4 nw,  i, ln;
  TStr  ts;
  ubyte ky, vl, k1;
  ubyt2 ns = 0;
  char *p;
TRC("{ Sound::Sound `s dKey=`d dss=`s dds=`s", snd, dKey, dss, dds);
   _nSmp = 0;   MemSet (TSmp, 0, sizeof (TSmp));
   _siz = _mxDat = 0;   _max = 0.0;

   if (! SndDir (snd, dss, dds)) {       // set _nm,_pa n _xFrq,_xRls
TRC("} Sound::Sound - no sampset for snd");
DBG("Sound::SndDir couldn't turn `s into a dir :(", snd);
      _smp = new Sample [1];
      return;
   }
// go thru each .WAV of the snd dir
// _k<note>_ in fn gives max key range
// _v<velo>_ in fn gives optional max velocity range
   nw = pa.FLst (_pa, WavFn, (ubyt2)BITS (WavFn));
   StrAp (_pa, CC("/"));
   for (i = 0;  i < nw;  i++) {     // pull overrd smpKey,maxVel from WAV fns
      ky = vl = 0xFF;
      StrCp (ts, WavFn [i]);   ln = StrLn (ts);
      for (p = ts;  *p;  p++)  if ( (StrLn (p) >= 4) &&
                                    (! MemCm (p, CC("_k"), 2)) &&
                                    (p [2] >= '0') && (p [2] <= '9') )
         {if ((k1 = MKey          (& p [2])) > 0)  ky = k1;
          break;}
      if (dKey < 128)  ky = dKey;
      for (p = ts;  *p;  p++)  if ( (StrLn (p) >= 3) &&
                                    (! MemCm (p, CC("_v"), 2)) &&
                                    (p [2] >= '0') && (p [2] <= '9') )
         {if ((k1 = (ubyte)Str2Int (& p [2])) > 0)  vl = k1;
          break;}
      if (! LoadFmt (ts, ky, vl)) { // dang, shut this down to 0 samples
TRC("} Sound::Sound - LoadFmt failed");
         _smp = new Sample [1];   _nSmp = 0;   _siz = _mxDat = 0;
         return;
      }
   }
// make key,vel ranges after sorting
   Sort (TSmp, _nSmp, sizeof (TSmp[0]), TSmCmp);
   for (i = 0;  i < _nSmp;  i++) {  // make key,vel ranges
      if (dKey < 128) {             // drums are ez (just velo rng)
         TSmp [i].mnKey = TSmp [i].mxKey = dKey;
         if      (i == 0)
               TSmp [i].mnVel = 0;
         else if (TSmp [i-1].mxVel == TSmp [i].mxVel)
               TSmp [i].mnVel = TSmp [i-1].mnVel;
         else  TSmp [i].mnVel = TSmp [i-1].mxVel+1;
      }
      else {                        // melodic is finicky
         if (i && (TSmp [i-1].mxKey == TSmp [i].mxKey)) {
                                    // same key as last time?
            if (TSmp [i-1].mxVel == TSmp [i].mxVel)     // same vel too?
                  TSmp [i].mnVel = TSmp [i-1].mnVel;    // yep
            else  TSmp [i].mnVel = TSmp [i-1].mxVel+1;  // nope
            TSmp [i].mnKey = TSmp [i-1].mnKey;
         }
         else {                     // start new key
            TSmp [i].mnVel = 0;     // vel rng ez
            TSmp [i].mnKey = i ? TSmp [i-1].mxKey+1 : 0;
         }
      }
   }

// alloc em n make m permanent.  then we're all setup for LoadDat later.
   _smp = new Sample [_nSmp];   MemCp (_smp, TSmp, _nSmp*sizeof (Sample));
TRC("} Sound::Sound - nSmp=`d", _nSmp);
}


Sound::~Sound ()  {delete [] _smp;}


//______________________________________________________________________________
// filter,fx classes fer syn

#define CLIP(_val,_min,_max) \
{(_val) = ((_val)<(_min))?(_min):(((_val)>(_max))?(_max):(_val));}

void LPF::Res (real r)                   // resonance (0-960 cB => 0-96 dB)
{  if ((fabs (res - pRes) <= 0.01))  return;
  real x = r / 10.;                 // x is q_dB
   if      (x <  0.)  x =  0.;
   else if (x > 96.)  x = 96.;
   x -= 3.01;
   res  = pow (10., x / 20.);
   gain = 1. / sqrt (res);
   pRes = r;
   pCut = -1.;                      // force a cut re-Calc, too
}

void LPF::Cut (real c)                   // cutoff frequency in absolute cents
// limit cut range n convert from cents to hz  (called once per buffer)
// rebuild coefficients if necessary           (1500-13500 ct => 20-20K hz)
{  cut = Ct2Hz (c);                         // (8000-13500 usable range tho)
   if      (cut > 0.45 * Sn._frq)  cut = 0.45 * Sn._frq;
   else if (cut < 5.)              cut = 5.;

// can we skip the grindin'...?
   if ((fabs (cut - pCut) <= 0.01))  return;

  real omega = 2.*M_PI * (cut / Sn._frq);
  real sinO  = sin (omega);
  real cosO  = cos (omega);
  real alpha = sinO / (2. * res);
  real a0Inv = 1. / (1. + alpha);
  real a1New = -2. * cosO   * a0Inv;
  real a2New = (1. - alpha) * a0Inv;
  real b1New = (1. - cosO)  * a0Inv * gain;
  real b2New = b1New * 0.5;
//TStr t1,t2,t3,t4,t5,t6,t7,t8,t9,ts;
//DBG("cut=`s omega=`s sinO=`s cosO=`s alpha=`s "
//"a0Inv=`s a1=`s a2=`s b1=`s b2=`s",
//R2Str(cut,t1),R2Str(omega,t2),R2Str(sinO,t3),
//R2Str(cosO,t4),R2Str(alpha,t5),R2Str(a0Inv,ts),
//R2Str(a1,t6),R2Str(a2,t7),R2Str(b1,t8),R2Str(b2,t9));
   if (init)  {init = false;   inc = 0;   a1 = a1New;   a2 = a2New;
                                          b1 = b1New;   b2 = b2New;}
   else {
      inc = Sn._nFr;
      a1Inc = (a1New-a1) / Sn._nFr;   a2Inc = (a2New-a2) / Sn._nFr;
      b2Inc = (b2New-b2) / Sn._nFr;   b1Inc = (b1New-b1) / Sn._nFr;
   }
   pCut = cut;
}

void LPF::Init ()
{  init = true;   pCut = pRes = -1.;   hist1 = hist2 = 0.;   Res (0.);  }

real LPF::Cvt (real smp)                 // actually DO the filterin'
{  if (fabs (hist1) < 1e-20)  hist1 = 0.;     // (called once per sample)
  real centerNode = smp - a1*hist1 - a2*hist2;
  real x =                b1*hist1 + b2*(centerNode + hist2);
   hist2 = hist1;   hist1 = centerNode;
   if (inc)  {inc--;   a1 += a1Inc;   a2 += a2Inc;
                       b1 += b1Inc;   b2 += b2Inc;}
//TStr t1,t2,t3,t4,t5,t6,t7,t8,t9;
//DBG("smp=`s x=`s centerNode=`s hist1=`s hist2=`s "
//"a1=`s a2=`s b1=`s b2=`s",
//R2Str(smp,t1),R2Str(x,t2),R2Str(centerNode,t3),
//R2Str(hist1,t4),R2Str(hist2,t5),R2Str(a1,t6),R2Str(a2,t7),
//R2Str(b1,t8),R2Str(b2,t9));
   return x;
}


//______________________________________________________________________________
// These assume 44.1KHz sample rate so adjust em
const ubyt4 LEN_COMB [NUM_COMB] =
                               {1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617};
const ubyt4 LEN_ALLP [NUM_ALLP] = {556, 441, 341, 225};

void Reverb::Update ()                 // reinit given new params
{  wet1 = wet * (width / 2 + 0.5f);   wet2 = wet * ((1 - width) / 2);
   for (sbyt4 i = 0;  i < NUM_COMB;  i++)
      {combL [i]->setfeedback (room);   combL [i]->setdamp (damp);
       combR [i]->setfeedback (room);   combR [i]->setdamp (damp);}
}

void Reverb::Mix (real *in, real *mixL, real *mixR)
{ sbyt4 i;
  real  outL, outR, input;
   for (ubyt4 k = 0;  k < Sn._nFr;  k++) {
      outL = outR = 0;
      input = (2 * in [k] + DC_OFFSET) * REVERB_GAIN;   // gain is fixed
      for (i = 0;  i < NUM_COMB;  i++)  {combL [i]->mix (input, & outL);
                                         combR [i]->mix (input, & outR);}
      for (i = 0;  i < NUM_ALLP;  i++)  {allpL [i]->mix (       & outL);
                                         allpR [i]->mix (       & outR);}
      outL -= DC_OFFSET;
      outR -= DC_OFFSET;
      mixL [k] += outL * wet1 + outR * wet2;
      mixR [k] += outR * wet1 + outL * wet2;
   }
}

Reverb::Reverb ()
{ ubyt4 i;
  real  lenSc = Sn._frq / 44100.0;
   for (i = 0;  i < NUM_COMB;  i++) {
      combL [i] = new Comb    ((sbyt4)( LEN_COMB [i]             * lenSc));
      combR [i] = new Comb    ((sbyt4)((LEN_COMB [i]+STEREO_SPR) * lenSc));
   }
   for (i = 0;  i < NUM_ALLP;  i++) {
      allpL [i] = new Allpass ((sbyt4)( LEN_ALLP [i]             * lenSc));
      allpR [i] = new Allpass ((sbyt4)((LEN_ALLP [i]+STEREO_SPR) * lenSc));
   }
   Set ();
}

Reverb::~Reverb ()
{ ubyt4 i;
   for (i = 0;  i < NUM_COMB;  i++)  {delete combL [i];   delete combR [i];}
   for (i = 0;  i < NUM_ALLP;  i++)  {delete allpL [i];   delete allpR [i];}
}


//______________________________________________________________________________
void Voice::ReFrq ()
// set frq - inc based on note frq versus root sample frq (drums not pitched)
{ real t;
   t = _smp->frq / Sn._frq;          // plus pbnd
   if (! _snd->_xFrq)
      t *= ( Ct2Hz ( _key * 100. +
                     (_chn->pbnr * 100. *
                      ((real)(_chn->pbnd - MID14) / (real)MID14) // -1..1
                     ) ) /
             Ct2Hz (_smp->key*100. - (sbyte)(_smp->cnt)) );
   _phInc = REAL2PHASE (t);
}


void Voice::ReFlt ()                       // set flt res,cut
{  _flt.Res (960. * _chn->res / 127.);
   if (_chn->vCut == 127) {         // vCut 127 means use chan's cut
//DBG("ReFlt vCut=127 so use cut=`d frq=`d",
//_chn->cut,   (int)(_chn->cut / 127. * 9500. + 4000.));
      _flt.Cut (_chn->cut / 127. * 9500. + 4000.);
   }
   else {                           // else use velo min'd at vCut
     real cMin = _chn->vCut / 127. * 9500.;
     real cRng = 9500. - cMin;
//DBG("ReFlt vCut=`d vel=`d cMin=`d cRng=`d frq=`d",
//_chn->vCut, _vel, (int)cMin, (int)cRng,
//(int)(             _vel * cRng / 127. + cMin + 4000.));
      _flt.Cut (_vel * cRng / 127. + cMin + 4000.);
   }
}


void Voice::ReAmp ()                       // set amp - based on vol cc n velo
{  _amp  = (_chn->vol / 127.) * (_vel / 127.);  }


void Voice::RePan ()                       // set pan - pan cc
{  _panL = (_smp->lr == 1) ? 0. : Pan (_chn->pan, true);
   _panR = (_smp->lr == 0) ? 0. : Pan (_chn->pan, false);
}


void Voice::Redo (char re)       // reset voice params given diff in chan,etc
{  if      (re == 'n')  ReFrq ();
   else if (re == 'f')  ReFlt ();
   else if (re == 'a')  ReAmp ();
   else if (re == 'p')  RePan ();
   else         /*'*'*/{ReFrq ();   ReFlt ();   ReAmp ();   RePan ();}
//Dump ("Redo");
}


void Voice::Bgn (Channel *chn, ubyte k, ubyte v, ubyt4 n, Sound *s, Sample *sm)
// called by synth's NtOn per matching sample of chan's sound
{  _chn = chn;   _key = k;   _vel = v;   _ntID = n;   _snd = s;   _smp = sm;
   _rels = _relsLen = 0;   _puts = 0;
   _loopin = (_smp->lpBgn < _smp->len) ? true : false;   _looped = false;
   _phase = (Phase)0;   _ampInc = 0.;
   _flt.Init ();
   Redo ('*');
   _on = 'd';
}


void Voice::Release ()                 // if chan hold, leave on till hold off
{                                      // else beGIN release
if (App.trc) Dump (CC("Release"));
   if (! _puts)  {
TRC("VC KILL");
                  End ();   return;}     // never even got heard - KILL!
   if (_rels)               return;      // already releasin' - skip out

   if (_chn && (_chn->hold >= 64))  {_on = 's';   return;}   // sustainin'

// samp has no loop and sez NO release - quick end
   if ((! _loopin) && _snd->_xRls)  {End ();      return;}

TRC("VC RELS BGN");
// start release a goin'
   _rels = 1;
// release for 1/64 of a beat means bump amp 128 time in time=1/16*qnote
// smp/sec => smp/min => smp/16thOfQNote => split by relsLen
   _relsLen = (ubyt4)(Sn._frq * 60. / 16. / 120.);
   if (! _relsLen)  _relsLen = 1;   // so say 44.1KHz,tmpo=200 => 6
}                                                       // 120 => 1378


void Voice::End ()                         // off it
{
if (App.trc) Dump(CC("VC RELS END"));
   _on = '\0';   _chn = nullptr;   _snd = nullptr;
}


ubyt4 Voice::Interpolate ()
// stretch l,r sample (per note frq vs. root frq) into l,r dsp bufs
// hi byte of phase fraction tells how to balance our 7 samples at a time
{ ubyt4 d,             sBgn, sEnd, s = 0;
  real *o, *in, *co,   sb0, sb1, sb2,   se0, se1, se2;
  Phase ph;
   in = & Sy._smp [_smp->pos];   o = _buf;

// bump to next halfsample since centered on 4th sample point
   d = 0;   ph = _phase + (Phase)0x80000000;

// set sBgn, sEnd per our range of 7 samples
// set sb0-2,se0-2 to the "outside samples" of the 7 in our set
   if (! _looped)  sb0 = sb1 = sb2 = in [sBgn = 0];
   else {                           // sBgn is 0 or lpBgn
      sBgn = _smp->lpBgn;
      sb0 = in [_smp->len-1];   sb1 = in [_smp->len-2];
                                sb2 = in [_smp->len-3];
   }
   sEnd = (_smp->len-1) - 3;
   if (! _loopin)  se0 = se1 = se2 = in [_smp->len-1];
   else                             // sEnd is end-3
      {se0 = in [_smp->lpBgn];    se1 = in [_smp->lpBgn+1];
                                  se2 = in [_smp->lpBgn+2];}
//TStr s1,s2,s3,s4,s5,s6;
//DBG("      Interp lr=`d d=`d s=`d sBgn=`d sEnd=`d ph=`d `d "
//"sb0=`s sb1=`s sb2=`s   se0=`s se1=`s se2=`s",
//lr, d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF),
//R2Str(sb0,s1), R2Str(sb1,s2), R2Str(sb2,s3),
//R2Str(se0,s4), R2Str(se1,s5), R2Str(se2,s6));
   for (;;) {
      s = PHASE_INDEX (ph);
      while (s == sBgn && d < Sn._nFr) {    // 1st sample point
//DBG("         a: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*sb2     + co[1]*sb1     + co[2]*sb0     +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
         ph += _phInc;   s = PHASE_INDEX (ph);     // bump phase
      }
      sBgn++;

      while (s == sBgn && d < Sn._nFr) {    // 2nd to 1st sample point
//DBG("         b: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*sb1     + co[1]*sb0     + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
         ph += _phInc;   s = PHASE_INDEX (ph);
      }
      sBgn++;

      while (s == sBgn && d < Sn._nFr) {     // 3rd to 1st sample point
//DBG("         c: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*sb0     + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
         ph += _phInc;   s = PHASE_INDEX (ph);
      }
      sBgn -= 2;

      while (s <= sEnd && d < Sn._nFr) {       // general case
//DBG("         d: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
         ph += _phInc;   s = PHASE_INDEX (ph);
      }
      if (d >= Sn._nFr)  break;   // break out if buffer filled

      sEnd++;
      while (s <= sEnd && d < Sn._nFr) {       // 3rd to last point
//DBG("         e: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*se0;
         ph += _phInc;   s = PHASE_INDEX (ph);
      }

      sEnd++;
      while (s <= sEnd && d < Sn._nFr) {       // 2nd to last point
//DBG("         f: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*se0     + co[6]*se1;
         ph += _phInc;   s = PHASE_INDEX (ph);
      }

      sEnd++;
      while (s <= sEnd && d < Sn._nFr) {       // last point
//DBG("         g: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*se0     + co[5]*se1     + co[6]*se2;
         ph += _phInc;   s = PHASE_INDEX (ph);
      }
      if (! _loopin)  break;        // done !

      if (s > sEnd) {
//DBG("         h: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         ph -= LONG2PHASE (_smp->len - _smp->lpBgn);
         if (! _looped) {
            _looped = true;          sb0 = in [_smp->len-1];
            sBgn    = _smp->lpBgn;   sb1 = in [_smp->len-2];
                                     sb2 = in [_smp->len-3];
         }
//DBG("         i: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
      }
      if (d >= Sn._nFr)  break;
      sEnd -= 3;
   }
//DBG("         j: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));

// sub 1/2 sample from ph since we offset it and save back in _phase
   _phase = ph - (Phase)0x80000000;
   return d;
}


void Voice::Mix (real *mixL, real *mixR, real *rvrb)    // da GUTS :)
{ ubyt4 i, len;
   if (! On ())                       return;
   if (_snd == nullptr)    {End ();   return;}
   _puts++;
   len = Interpolate ();
//TStr ts, t2, t4, t5;
//DBG("pre L=`s R=`s  panL=`s panR=`s",
//R2Str(mixL [0],ts), R2Str(mixR [0],t2), R2Str(_panL,t4), R2Str(_panR,t5));
   for (i = 0;  i < len;  i++)  {
     real s = _flt.Cvt (_buf [i]);            // filter it
      s *= _amp;                              // amp it
      mixL [i] += (s * _panL);                // pan,mix it
      mixR [i] += (s * _panR);
      rvrb [i] += (s * _chn->rvrb/127.);
      if (_rels) {
         if (_rels >= _relsLen)  {End ();   return;}
         ReAmp ();     // ^ kill yerself else bump
         _rels++;
      }
   }
//DBG("post L=`s R=`s", R2Str (mixL [0],ts), R2Str (mixR [0],t2));
   if (len < Sn._nFr)  End ();    // or if (nonloop) sample runs out
//for (i = 0; i < len; i++)
//DBG("`d mixL=`s R=`s", i, R2Str (mixL [i], ts), R2Str (mixR [i], t2));
}


void Voice::Dump (char *pre)
{ TStr t, s1, s2;
   DBG("`s on=`c key=`d=`s vel=`d ntID=`d puts=`d loopin=`b looped=`b",
       pre, _on?_on:' ', _key, MKey2Str (t,_key), _vel, _ntID,
       _puts, _loopin, _looped);
   DBG("   phase=`u `u phInc=`u `u panL=`s panR=`s rels=`d relsLen=`d",
       (ubyt4)(_phase>>32), (ubyt4)(_phase & 0xFFFFFFFF),
       (ubyt4)(_phInc>>32), (ubyt4)(_phInc & 0xFFFFFFFF),
       R2Str(_panL,s1), R2Str(_panR,s2), _rels, _relsLen);
   if (_chn) {_chn->Dump ();   if (_snd)  _snd->Dump ();
                               if (_smp)  _smp->Dump (_chn->DrCh ());}
}


Voice::Voice ()
{  _buf = new real [Sn._nFr];
   _on = _key = _vel = 0;
   _chn = nullptr;   _snd = nullptr;   _smp = nullptr;
}


Voice::~Voice ()   {delete [] _buf;}


//______________________________________________________________________________
// sound bank management
void Syn::DumpSnd ()
{ ubyte s;
  ubyt2 i;
  TStr  ds;
  Sample *sm;                          // first melo, then drum sounds...
   for (s = 0;  s < _nSnd;  s++) {
      DBG("Snd=`d/`d:", s, _nSnd);             _snd [s]->Dump ();
      for (i = 0, sm = _snd [s]->_smp;  i < _snd [s]->_nSmp;  i++, sm++)
         {DBG("   Smp=`d:", i);   sm->Dump (false);}
   }
   for (s = 0; s < 128; s++)  if (_drm [s]) {
      DBG("Drm=`d/`s:", s, MDrm2Str (ds,s));   _drm [s]->Dump ();
      for (i = 0, sm = _drm [s]->_smp;  i < _drm [s]->_nSmp;  i++, sm++)
         {DBG("   Smp=`d:", i);   sm->Dump (true);}
   }
}

void Syn::WipeSnd ()
{ ubyt2 s;
TRC("   Syn::WipeSnd");
   for (s = 0;  s <   128;  s++)  AllCh ((ubyte)s, 'e');   // 9 kills drums too
   for (s = 0;  s < _nSnd;  s++)     {delete _snd [s];   _snd [s] = nullptr;}
   _nSnd = 0;
   for (s = 0;  s <   128;  s++)  if (_drm [s])
                                     {delete _drm [s];   _drm [s] = nullptr;}
   delete [] _smp;   _smp = nullptr;   _nSmp = 0;
   _maxLvl = 1.0;   _maxVc = 0;
}

void Syn::LoadSnd ()
{ TStr  fn, ts;
  File  f;
  char  lst [256*sizeof (TStr)], *pc;
  bool  melo = true;
  ubyt4 p, len, ld;
  ubyte s;
  ubyt8 sz;
  TStr  sSetM, sSetD, dSet;         // default sampsets n drumset
TRC("Syn::LoadSnd");
   WipeSnd ();

// get default melo n drum sampsets n drumset
   App.Path (fn, 'd');   StrAp (fn, CC("/device/syn/sound.txt"));
   len = f.Load (fn, lst, sizeof (TStr)*3);
   lst [len] = '\0';
   *sSetM = *sSetD = *dSet = '\0';
   if (! MemCm (lst, CC("#SS "), 4)) {
      StrCp (lst, & lst [4]);
      if ((pc = StrCh (lst, ' '))) {
         *pc++ = '\0';       StrCp (sSetM, lst);   StrCp (lst, pc);
         if ((pc = StrCh (lst, ' '))) {
            *pc++ = '\0';    StrCp (sSetD, lst);   StrCp (lst, pc);
            if ((pc = StrCh (lst, '\n'))) {
               *pc = '\0';   StrCp (dSet,  lst);
TRC("ssetM=`s ssetD=`s dset=`s", sSetM, sSetD, dSet);
            }
            else DBG ("no default drum set");
         }
         else DBG ("no default drum sample set");
      }
      else DBG ("no default melodic sample set");
   }
   else DBG ("no #SS rec");

// load SoundBank.txt with melodic, then drum sounds (dirs of .WAV samples)
   App.Path (fn, 'd');   StrAp (fn, CC("/device/syn/SoundBank.txt"));
   len = f.Load (fn, lst, sizeof (lst));   f.Kill (fn);
TRC("soundbank.txt loaded n del'd");
   if (len >= sizeof (lst)) {
DBG("Syn::LoadSnd  SoundBank.txt too big :(", fn);
      return;
   }
   lst [len] = '\0';
   if (! len)
      DBG("Syn::LoadSnd: can't load device/syn/SoundBank.txt :(");
   for (p = 0;  p < len;) {
      p = NextLn (ts, lst, len, p);      // parse buf into seq of strs
TRC("p=`d/`d: `s", p, len, ts);
      if (! StrCm (ts, CC("Drum")))  {melo = false;   continue;}
      if (melo) {                   // drum marks when melodic sounds end
         if (_nSnd >= 128) {
DBG("Syn::LoadSnd  tooo many sounds");
            return;
         }
TRC("pgm=`d snd=`s", _nSnd, ts);
         _snd [_nSnd++] = new Sound (ts, 128, sSetM, CC(""));
      }
      else {                        // loading drum sounds now
         ts [4] = '\0';
         s = MDrm (ts);
         if (s == 128) {
DBG("Syn::LoadSnd - bad drum note=`s", ts);
            return;
         }
TRC("drm=`d snd=`s", s, & ts [5]);
         _drm [s] = new Sound (& ts [5], s, sSetD, dSet);
      }
   }

// load in all .WAV files' samples n convert em to real
   sz = ld = 0;                     // get ALL samples' len, max WAV DAT len
   for (s = 0;  s < _nSnd;  s++)
      {if (_snd [s]->_mxDat > ld)  ld = _snd [s]->_mxDat;
       sz +=                            _snd [s]->_siz;}
   for (s = 0;  s <   128;  s++)  if  (_drm [s])
      {if (_drm [s]->_mxDat > ld)  ld = _drm [s]->_mxDat;
       sz +=                            _drm [s]->_siz;}
   if ((sz * sizeof (real)) >= 4294967295) {
DBG("Syn::LoadSnd  tooo many samples (`d) :(",  sz);
      for (s = 0; s < _nSnd; s++)  DBG("   Snd `s siz=`d mxDat=`d",
                            _snd [s]->_nm, _snd [s]->_siz, _snd [s]->_mxDat);
      for (s = 0; s < 128; s++) if (_drm [s])
                                   DBG("   Drm `s siz=`d mxDat=`d",
                            _drm [s]->_nm, _drm [s]->_siz, _drm [s]->_mxDat);
      WipeSnd ();
      return;
   }
TRC("alloc dat n smp");
   _smp = new real [sz];   _nSmp = (ubyt4)sz;   len = 0;
  ubyte *dat = new ubyte [ld];
   if ((dat == nullptr) || (_smp == nullptr)) {
DBG("Syn::LoadSnd  Outa memory - `d samples => `d bytes :(", _nSmp, _nSmp*8);
      for (s = 0; s < _nSnd; s++)  DBG("   Snd `s siz=`d mxDat=`d",
                            _snd [s]->_nm, _snd [s]->_siz, _snd [s]->_mxDat);
      for (s = 0; s < 128; s++) if (_drm [s])
                                   DBG("   Drm `s siz=`d mxDat=`d",
                            _drm [s]->_nm, _drm [s]->_siz, _drm [s]->_mxDat);
      delete [] dat;   WipeSnd ();
      return;
   }
TRC("load snd n drm");
   for (s = 0;  s < _nSnd;  s++)     len = _snd [s]->LoadDat (dat, len);
   for (s = 0;  s <   128;  s++)  if (_drm [s])
                                     len = _drm [s]->LoadDat (dat, len);
TRC("free dat");
   delete [] dat;                   // just need it fer loadin
TRC("Syn::LoadSnd end");
}


//______________________________________________________________________________
// stuff used by notes...
void Syn::NOff (ubyte ch, ubyte key, ubyte vel)
{ TStr ts;
TRC(" NOff ch=`d key=`d=`s vel=`d",
ch, key, ((ch % 16) == 9) ? MDrm2Str (ts, key) : MKey2Str (ts, key), vel);
   if (_chn [ch].Drum ())  ch = 0x80 | key;
   for (ubyt2 i = 0;  i < _nVc;  i++)
      if (_vc [i].Down () && (_vc [i]._chn->id == ch) &&
                             (_vc [i]._key == key))
         {
TRC("  vc rel=`d/`d", i, _nVc);
          _vc [i].Release ();}
}


void Syn::NtOn (ubyte ch, ubyte key, ubyte vel)
{ Channel *c;
  Sound   *s;
  ubyt2    sm, i, j;
  real     prio, best = 999999.;
  bool     shr = false;
  TStr     ts;
   if (vel == 0)  return NOff (ch, key, 0);

TRC(" NtOn ch=`d key=`d=`s vel=`d",
ch, key, ((ch % 16) == 9) ? MDrm2Str (ts, key) : MKey2Str (ts, key), vel);
   if ((key >= 128) || (vel >= 128)) {
DBG("Syn::NtOn  bad key,vel ch=`d key=`s vel=`d", ch, MKey2Str (ts, key), vel);
      return;
   }
   c = & _chn [ch];
   if (c->Drum ()) {                // 9 => specific drum channel for note
      if ((s = _drm [key]) == nullptr) {
DBG("Syn::NtOn  drum sound not loaded ch=`d key=`s vel=`d",
ch, MDrm2Str (ts, key), vel);
         return;
      }                             // fix channel, get sound too
      c = & _chn [ch = 0x80 | key];
   }
   else {                           // melodic is easy
      if (c->snd >= _nSnd) {
DBG("Syn::NtOn  melo sound not loaded ch=`d key=`s vel=`d snd=`d of `d",
ch, MKey2Str (ts, key), vel, c->snd, _nSnd);
         return;
      }
      s = _snd [c->snd];
   }

//TODO kill off any voices exclusive to me
// release on same chan n note;  completely end if no puts yet (we're behind)
   for (i = 0;  i < _nVc;  i++)
      if (_vc [i].On () && (_vc [i]._chn->id == ch) && (_vc [i]._key == key))
         {
TRC("  vc rel=`d/`d  cuz excl", i, _nVc);
          _vc [i].Release ();}

// kick new voices for any matchin samples
   for (sm = 0;  sm < s->_nSmp;  sm++)
      if ( (key >= s->_smp [sm].mnKey) && (key <= s->_smp [sm].mxKey) &&
           (vel >= s->_smp [sm].mnVel) && (vel <= s->_smp [sm].mxVel) ) {
         for (i = 0;  i < _nVc;  i++)  if (! _vc [i].On ())  break;
         if (i >= _nVc) {
            if (_nVc < _xVc) {      // new one
               i = _nVc++;
               if (i > _maxVc) {_maxVc = i;
DBG("Syn: maxVoice=>`d", i+1);
               }
            }
            else                    // kill somebody
               for (j = 0;  j < _nVc;  j++) {
                  prio = 10000.;
                  if      (_vc [j]._chn->Drum ())  prio += 4000.;
                  else if (_vc [j].Rels ())        prio -= 2000.;
                  if      (_vc [j].Sust ())        prio -= 1000.;
                  prio -= (_ntID - _vc [j]._ntID);
                  if (prio < best)  {i = j;   best = prio;}
               }                    // ^ THAT guy gets stamped on toppa :/
         }
         else shr = true;           // try to keep max as shrunk as we can

         _vc [i].Bgn (c, key, vel, ++_ntID, s, & s->_smp [sm]);
if (App.trc) _vc [i].Dump (StrFmt (ts, "  vc add=`d/`d", i, _nVc));
      }
   if (shr)  {for (i = _nVc;  i && (! _vc [i-1].On ());  i--)  _nVc--;
TRC("  vc shr=`d", _nVc);
             }
}


//______________________________________________________________________________
// stuff used by CCs
void Syn::UnHold (ubyte ch)            // release all sust'd vcs on chn
{
TRC(" UnHold ch=`d", ch);
   for (ubyt2 i = 0;  i < _nVc;  i++)
      if (_vc [i].Sust () && _vc [i]._chn && (_vc [i]._chn->id == ch))
         _vc [i].Release ();
}

void Syn::AllCh (ubyte ch, char todo)  // mod all vcs on channel
{ ubyte dr = _chn [ch].Drum () ? 0x80 : 0;
TRC(" AllCh ch=`d `c/`s", ch, todo,
(todo=='i')?"init": ( (todo=='e')?"endNow": ((todo=='r')?"release":"redo") ) );
   if (todo == 'i') {               // reset all CCs is kinda different
      _chn [ch].Rset ();
      if (dr)  for (ch = 0;  ch < 128;  ch++)  _chn [0x80 | ch].Rset ();
      _maxLvl = 1.0;
      return;
   }                                // else we're dealing with just on vcs
   for (ubyt2 i = 0;  i < _nVc;  i++)
      if (_vc [i].On () && ((_vc [i]._chn->id == ch) ||
                            (_vc [i]._chn->id &  dr)))
         switch (todo) {
            case 'r':  _vc [i].Release ();   break;     // allnotesoff(rels)
            case 'e':  _vc [i].End     ();   break;     // allsoundoff NOW
            default:   _vc [i].Redo (todo);  break;     // recalc vc live
         }
}


//______________________________________________________________________________
// =MY= main api...
void Syn::Put (ubyte ch, ubyt2 c, ubyte v, ubyte v2)
// setup a chan's voices with CC else start/stop a voice with note
// only drum notes,CCs of ANOFF,ASOFF,ACOFF should be on ch 9
// rest should have hi bit set in chn, drum note in LS7bits
{ char re = '\0';
TStr t1;
TRC("Syn::Put ch=`d c=`d=`s v=`d v2=`d", ch, c, MCtl2Str(t1,c,'r'), v, v2);
   if (ch & 0x80) {                 // check drum cc
TRC(" ch=drum `s", MDrm2Str(t1,ch & 0x7F));
      if (! _drm [ch & 0x7F]) {
DBG ("Syn::Put  ch=`d=`s but no drum sound c=`d=$04x v=`d=$02x v2=`d=$`02x",
ch & 0x7F, MDrm2Str(t1,ch & 0x7F), c, c, v, v, v2, v2);
         return;
      }
   }
   if (! (c & 0xFF80)) {            // do note  on/off
      if (v & 0x80)  NtOn (ch, (ubyte)c, v & 0x7F);
      else           NOff (ch, (ubyte)c, v);
//TODO NPrs later
      return;
   }
                                       // else do CC
   if ((v >= 128) && (c != MC_PROG))   // let prog thru
      {DBG("Syn::Put bad valu  ch=`d cc=`d valu=`d", ch, c, v);  return;}
   switch (c) {
      case MC_PROG:
         if (_chn [ch].Drum () || _chn [ch].DrCh () || (v > _nSnd))
               DBG("Syn::Put  Prog on drum chan or too high "
                   "ch=`d prog=`d nProg=`d",  ch, v, _nSnd);
         else  _chn [ch].snd = v;
         return;
      case MC_CC|M_ANOFF: re = 'r';   break;   // release em
      case MC_CC|M_ASOFF: re = 'e';   break;   // end em
      case MC_CC|M_ACOFF: re = 'i';   break;   // reset all CCs

   // ...better not be any chan 9s after this
      case MC_CC|M_HOLD:  if ((_chn [ch].hold = v) < 64)  UnHold (ch);
                          return;
   // above ones don't do voice recalc;  below ones do
      case MC_PBND:       _chn [ch].pbnd = v << 7 | v2;   re = 'n';   break;
      case MC_CC|16:      _chn [ch].pbnr = v;             re = 'n';   break;
      case MC_CC|17:      _chn [ch].vCut = v;             re = 'f';   break;
      case MC_CC|18:      _chn [ch].cut  = v;             re = 'f';   break;
      case MC_CC|19:      _chn [ch].res  = v;             re = 'f';   break;
      case MC_CC|M_VOL:   _chn [ch].vol  = v;             re = 'a';   break;
      case MC_CC|M_PAN:   _chn [ch].pan  = v;             re = 'p';   break;
   /* reverb fixed till i find algos that don't suck
   ** case MC_CC|24:      _chn [ch].rvrb = v;             re = 'p';   break;
   **
   ** // not channel specific fx params
   ** case MC_CC|30:      _fxP.rRoom  = v;                re = 'x';   break;
   ** case MC_CC|31:      _fxP.rDamp  = v;                re = 'x';   break;
   ** case MC_CC|32:      _fxP.rWidth = v;                re = 'x';   break;
   ** case MC_CC|33:      _fxP.rLevel = v;                re = 'x';   break;
   */
      default:            return;
   }
   AllCh (ch, re);
}

void Syn::DumpChn ()  {for (ubyt2 c = 0;  c < 256;  c++)  _chn [c].Dump ();}

void Syn::DumpVc ()
{ TStr ts;
   for (int i = 0;  i < _nVc;  i++)
      _vc [i].Dump (StrFmt (ts, "  vc=`d/`d", i, _nVc));
}

void Syn::Dump ()  {DumpSnd ();   DumpVc ();   DumpChn ();}


//______________________________________________________________________________
sbyt2 Syn::r2i (real r, real dth)
{ TStr ts;
   if (fabs (r) > _maxLvl) {           // keep track of max level EVER
      _maxLvl = fabs (r);
DBG("Syn: maxLevel=>`s", R2Str (_maxLvl, ts));
   }
   r = r * 32766.0 / _maxLvl + dth;         // scale to sample width n dither
   if (r >= 0.)  return (sbyt2)(r+0.5);     // round to sbyt2 -32767..32767
                 return (sbyt2)(r-0.5);
}


void Syn::run ()
// live rendering: send interleaved stereo sbyt2 samples to soundcard
{ ubyt4 sz  = Sn._nFr * sizeof (real);
  ubyte per = 0;
  ubyt2 i;
  sbyt2 (*out)[2];
   while (_run) {
      out = & _out [per*Sn._nFr];   per = per ? 0 : 1;     // double bufferin
      MemSet (_mixL, 0, sz);   MemSet (_mixR, 0, sz);   MemSet (_rvrb, 0, sz);
      for (i = 0;  i < _nVc;  i++)  _vc [i].Mix (_mixL, _mixR, _rvrb);
      _rvP.Mix (_rvrb, _mixL, _mixR);
      for (i = 0;  i < Sn._nFr;  i++) {
         out [i][0] = r2i (_mixL [i], Dither [0][_dth]);
         out [i][1] = r2i (_mixR [i], Dither [1][_dth]);
         if (++_dth >= MAX_DITHER)  _dth = 0;
      }
      Sn.Put ((sbyt2 *)out);           // this'll block us on 2nd call and on
   }
}


Syn::Syn ()
{
DBG("Syn::Syn");
   InitLookup ();
   MemSet (_snd, 0, sizeof (_snd));   _nSnd = 0;
   MemSet (_drm, 0, sizeof (_drm));
   for (ubyt2 c = 0;  c < 256;  c++)  _chn [c].Init ((ubyte)c);
   _vc = new Voice [_xVc = 256];   _nVc = 0;   _maxVc  = 0;
   _ntID = _dth = 0;                           _maxLvl = 1.0;
   _mixL = new real [Sn._nFr];   _mixR = new real [Sn._nFr];
                                 _rvrb = new real [Sn._nFr];
   _out = new sbyt2 [2*Sn._nFr][2];    // 2 periods of nFr frames of interleaved
   _run = true;   start ();            // stereo sbyt2 (to double buffer)
DBG("} Syn::Syn");
}


Syn::~Syn ()
{  _run = false;   wait ();
   WipeSnd ();
   delete [] _vc;
   delete [] _mixL;   delete [] _mixR;   delete [] _rvrb;
   delete [] _out;
}

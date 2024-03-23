// syn.cpp - easy(ish) softsynth based on plain jane .WAVs

#include "syn.h"

Syn Sy;                                // welp, that's everything...

static char *R2Str (real f, char *s)   // fer dbg - no exp, 6 decimals
{ bool  neg;
  ubyt4 i, ln;
  TStr  t;
   neg = (f < 0.) ? true : false;   if (neg) f = -f;
   i = (ubyt4)f;   f -= (real)i;
   StrFmt (t, "`06d", (ubyt4)(f*1000000.));
   while ((ln = StrLn (t)))  {if (t [--ln] == '0') t [ln] = '\0';   else break;}
   return StrFmt (s, "`s`d`s`s", neg?"-":"", i, *t?".":"", t);
}


// lookup stufffff _____________________________________________________________
#define MAX_INTERP  (256)
#define MAX_DITHER  (48000)

static real     Interp [MAX_INTERP][7];     // interpolation coefficients
static void InitInterp ()                   // max_interp is 256 cuz hi byte of
{ real v, iShf;                             // phase frac picks which of the 256
   for (ubyte i = 0; i < 7; i++)
      for (ubyt2 j = 0; j < MAX_INTERP; j++) {
         iShf = (real)i - (7. / 2.) + (real)j / (real)MAX_INTERP;
         if (fabs (iShf) <= 0.000001)  v = 1.;
         else {v = sin (iShf * M_PI) / (M_PI * iShf);
               v *= 0.5 * (1. + cos (2.*M_PI * iShf / 7.));}
         Interp [MAX_INTERP-1 - j][i] = v;
      }
}

static real     Dither [2][MAX_DITHER];     // per l/r channel
static void InitDither (void)               // rand real btw -.999 and +.999
// add a smidge of randomness to limit truncation distortion goin real=>int
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
static real Cnv_ct2hz [1200];          // cents to hz
static real     Ct2Hz (real ct)
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

static real Cnv_pan   [127];           // 128 to pan
static real     Pan (ubyte c, ubyte lr)
{  if (c ==  0)  c =   1;              // so -64 => -63
   if (c > 127)  c = 127;              // limit at -63 .. 63
   c--;   if (lr == 0)  c = 126 - c;
   return Cnv_pan [c-1];
}

static void InitLookup ()
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
// sounds and their samples
#define MAX_SAMP (88*20*2)             // max #stereo WAVs per sound
                                       // all piano keys - 20 velo grps
const  real   MAXS4 = 2147483648.;
static TStr   WavFn [MAX_SAMP*2];      // hold sounds' wavs while loading
static Sample TSmp  [MAX_SAMP];        // sample is mostly just a data struct
                                       // (little code to it)
void Sample::Dump (bool dr)
{ TStr s, s1, s2;
   TRC("      key=`s-`s vel=`d-`d fn=`s",
       dr ? MDrm2Str(s1,mnKey) : MKey2Str(s1,mnKey),
       dr ? MDrm2Str(s2,mxKey) : MKey2Str(s2,mxKey), mnVel, mxVel, fn);
   TRC("      lr=`d lpBgn=`d len=`d pos=`d frq=`d key=`s cnt=`d",
       lr, lpBgn, len, pos, frq, dr ? MDrm2Str(s,key) : MKey2Str(s,key), cnt);
}


static int TSmCmp (void *p1, void *p2)
// find first numeric char and get oct,note from there to sort by
{ Sample *s1 = (Sample *)p1, *s2 = (Sample *)p2;
  int i;
   if ((i = s1->mxKey - s2->mxKey))  return i;
   if ((i = s1->mxVel - s2->mxVel))  return i;
   return   s1->lr    - s2->lr;
}


void Sound::Dump ()
{  TRC("   `s nSmp=`d siz=`d xFrq=`b xRls=`b",
           _nm,  _nSmp,  _siz,  _xFrq,  _xRls);  // " pa=`s", _pa);
}


ubyt4 Sound::LoadDat (ubyt4 pos)
// actually load each .WAV we found into smp buffer n offset our poss by len
{ TStr  fn, s1;
  Wav   w;
  sbyt4 si, lenD, s, p1 = pos;
  ubyt2 i;
  ubyte nby, flo, *p;
  real  sr, max = 0.;
TRC("Sound::LoadDat pos=`d", pos);
   for (i = 0;  i < _nSmp;  i++) {
      StrFmt (fn, "`s/device/syn/`s/`s",  App.Path (s1, 'd'), _pa, _smp [i].fn);
      w.Load (fn);                     // already know it's there n ok
      _smp [i].pos   = pos;            // already got len,lr,key n vel ranges
      _smp [i].lpBgn = (w._loop && (! _xRls)) ? w._lBgn : w._len;
      _smp [i].key   = w._key;
      _smp [i].cnt   = w._cnt;
      _smp [i].frq   = w._frq;
      nby = w._byts;   flo = w._real;
TRC("   got smp `d/`d pos=`d", i, _nSmp, pos);
      for (p = & SC(ubyte *,w._mem) [(_smp [i].lr == 1) ? nby : 0],
           s = 0;  s < _smp [i].len;  s++) {
         if (flo)  {sr = (nby <= 4) ? *((float *)p) : *((double *)p);
                    p +=  nby;}
         else {
            si = 0;
            switch (nby) {             // single byte is weird; 2,3 similar; 4ez
               case 1:  si = ((sbyt4)(*p++) - 128) << 24;          break;
               case 3:  si |= *p++ <<  8;
               case 2:  si |= *p++ << 16;   si |= (*p++ << 24);   break;
               case 4:  si = *((sbyt4 *)p);   p += 4;              break;
            }
            sr = (real)si / MAXS4;
         }
         if (fabs (sr) > max)  max = fabs (sr);
         Sy._smp [pos++] = sr;
      }
   }
// scale all samples into +/- 1.0
   for (;  p1 < pos;  p1++)  Sy._smp [p1] *= (1.0 / max);
TRC("Sound::LoadDat max=`s  new pos=`d",  R2Str(max,s1), pos);
   return pos;
}


bool Sound::LoadFmt (char *wfn, ubyte ky, ubyte vl)
// load .WAV fmt chunk in prep.  get len,lr;  set mxKey,mxVel
// bump sound's _nSmp and _siz per len,lr (stereo is special)
//    _L, _R suffix means they're mono but should be paired up for stereo
{ TStr  fn, s1;
  char *e;
  Wav   w;
  Sample *s = & TSmp [_nSmp];
   StrCp (s->fn, wfn);
   StrFmt (fn, "`s/device/syn/`s/`s",  App.Path (s1, 'd'), _pa, s->fn);
   if ((e = w.Load (fn)) != nullptr)
      {DBG("Sound::LoadFmt `s: `s", fn, e);                   return false;}
   if ((_nSmp + (w._mono?1:2)) > BITS (TSmp))
      {DBG("Sound::LoadFmt  too many samples upon `s", fn);   return false;}
   s->mxKey = ky;   s->mxVel = vl;
   s->len   = (w._loop && (! _xRls)) ? w._lEnd+1 : w._len;
   _nSmp++;   _siz += s->len;
   if (w._mono)
      s->lr = StrSt (wfn, CC("_L.WAV")) ? 0 :
             (StrSt (wfn, CC("_R.WAV")) ? 1 : 2);
   else {
      s->lr = 0;   MemCp (s+1, s, sizeof (TSmp[0]));   s++;
      s->lr = 1;   _nSmp++;   _siz += s->len;
   }
   return true;
}


Sound::Sound (char *snd, ubyte dKey)
// list .WAVs of snd dir into WavFn[].  load each hdr  (load samples later)
{ Path  pa;
  ubyt4 nw,  i;
  TStr  s, s2, bnk;
  ubyte ky, vl, k1;
  ubyt2 ns = 0;
  char *p;
TRC("Sound::Sound _nSnd=`d snd=`s dKey=`s",
Sy._nSnd, snd, (dKey==128)?"(melo)":MDrm2Str(s2,dKey));
   _siz  = 0;
   _xFrq = _xRls = false;              // default to pitching n looping
   _nSmp = 0;   _smp = nullptr;   MemSet (TSmp, 0, sizeof (TSmp));

// set _nm,_pa n _xFrq,_xRls
   StrCp (_nm, snd);                   // ex: Piano_Clavinet_ssAria
   StrCp (s,   snd);
  ColSep c (s, 8, '_');
   StrCp (bnk, c.Col [c.Len-1]);       // last _ split thingy is bank
   StrCp (s, snd);   s [StrLn (s) - StrLn (bnk) - 1] = '\0';
   StrFmt (_pa, "`s/`s", bnk, s);      // ex: ssAria/Piano_Clavinet
   i = StrLn (_pa);                    // special sound suffix - drum,hold,clip
   if (i > 5) {
   // snd w ~hold suffix=> no release ramp on ntUp, ~drum suffix=> un-pitched
   // ~clip suffix for both ^                    (or in a drum dir ^ )
      if ((! MemCm (_nm, CC("Drum/"), 5)) ||
          (! MemCm (& _pa [i-5], CC("~drum"), 5))) _xFrq = true;
      if  (! MemCm (& _pa [i-5], CC("~hold"), 5))  _xRls = true;
      if  (! MemCm (& _pa [i-5], CC("~clip"), 5))  _xFrq = _xRls = true;
   }
TRC("   _pa=`s xFrq=`b xRls=`b", _pa, _xFrq, _xRls);

// go thru each .WAV of the snd dir
// _k<note>_ in fn gives max key range
// _v<velo>_ in fn gives optional max velocity range
   nw = pa.FLst (StrFmt (s, "`s/device/syn/`s", App.Path (s2, 'd'), _pa),
                 WavFn, (ubyt2)BITS (WavFn));
   for (i = 0;  i < nw;  i++) {     // pull overrd smpKey,maxVel from WAV fns
      ky = vl = 0xFF;
      StrCp (s, WavFn [i]);
      for (p = s;  *p;  p++)
         if ( (StrLn (p) >= 5) && (! MemCm (p, CC("_k"), 2)) &&
                                  (p [2] >= '0') && (p [2] <= '9') )
            {if ((k1 =           MKey (& p [2])) > 0)  ky = k1;   break;}
      if (dKey < 128)  ky = dKey;
      for (p = s;  *p;  p++)
         if ( (StrLn (p) >= 6) && (! MemCm (p, CC("_v"), 2)) &&
                                  (p [2] >= '0') && (p [2] <= '9') )
            {if ((k1 = (ubyte)Str2Int (& p [2])) > 0)  vl = k1;   break;}

      if (! LoadFmt (s, ky, vl)) {     // rats!  shut this down to 0 samples
DBG("Sound::Sound - LoadFmt failed w snd=`s wav=`s", _nm, s);
         _smp = new Sample [1];   _nSmp = _siz = 0;
         return;
      }
   }
// sort then make key,vel ranges
   Sort (TSmp, _nSmp, sizeof (TSmp [0]), TSmCmp);
   for (i = 0;  i < _nSmp;  i++) {  // make key,vel ranges
      if (dKey < 128) {             // drums are ez (just velo rng)
         TSmp [i].mnKey = TSmp [i].mxKey = dKey;
         if      (i == 0)
              TSmp [i].mnVel = 0;
         else if (TSmp [i-1].mxVel == TSmp [i].mxVel)
              TSmp [i].mnVel = TSmp [i-1].mnVel;
         else TSmp [i].mnVel = TSmp [i-1].mxVel+1;
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
   if (_nSmp == 0) {
DBG("Sound::Sound end - Snd=`s nSmp=0 :(", _nm, s);
      _smp = new Sample [1];   _nSmp = _siz = 0;
      return;
   }
   _smp = new Sample [_nSmp];   MemCp (_smp, TSmp, _nSmp*sizeof (Sample));
TRC("   nSmp=`d", _nSmp);
}


Sound::~Sound ()  {delete [] _smp;}
//______________________________________________________________________________
// low pass filter for each voice
void LPF::Cut (real c)                 // cutoff frequency in absolute cents
// limit cut range n convert from cents to hz  (called once per buffer)
// rebuild coefficients if necessary           (1500-13500 ct => 20-20K hz)
{  cut = Ct2Hz (c);                    // (8000-13500 usable range tho)
   if      (cut > 0.45 * Sy._frq)  cut = 0.45 * Sy._frq;
   else if (cut < 5.)              cut = 5.;
   if ((fabs (cut - pCut) <= 0.01))  return;     // can we skip the grindin'...?

  real omega = 2.*M_PI * (cut / Sy._frq);
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
      inc = Sy._nFr;
      a1Inc = (a1New-a1) / inc;   a2Inc = (a2New-a2) / inc;
      b2Inc = (b2New-b2) / inc;   b1Inc = (b1New-b1) / inc;
   }
   pCut = cut;
}

void LPF::Res (real r)                 // resonance (0-960 cB => 0-96 dB)
{  if ((fabs (res - pRes) <= 0.01))  return;

  real x = r / 10.;                    // x is q_dB
   if      (x <  0.)  x =  0.;
   else if (x > 96.)  x = 96.;
   x -= 3.01;
   res  = pow (10., x / 20.);
   gain = 1. / sqrt (res);
   pRes = r;
   pCut = -1.;                         // force a cut re-Calc, too
}

void LPF::Init ()
{  init = true;   pCut = pRes = -1.;   hist1 = hist2 = 0.;   Res (0.);  }

real LPF::Mix (real smp)               // actually DO the filterin'
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
void Voice::Init ()  {_on = '\0';   _ch = 0xFF;}


// sample interpolation position+fraction
// index - hi ubyt4 - array pos within sample
// fract - lo ubyt4's hi ubyte - array pos in Interp[]
#define MAXU4           (4294967296.)  // max ubyt4 as real
#define LONG2PHASE(a)   (((Phase)(a))<<32)
#define REAL2PHASE(a)  ((((Phase)(a))<<32) | (Phase)(((a)-((ubyt4)(a)))*MAXU4))
#define PHASE_INDEX(x)  ((ubyt4)((x) >> 32))
#define PHASE_FRACT(x)  ((((ubyt4)(x)) >> 24) & 0x00FF)


void Voice::ReFrq ()
// set frq - inc based on card vs. smp root vs. note frq  (drum notes unpitched)
{ real t;
  TStr s;
  Channel *c = & Sy._chn [_ch];
//DBG("Voice::ReFrq smp=`d dev=`d",  _smp->frq, Sy._frq);
   t = (real)_smp->frq / (real)Sy._frq;
   if (! _snd->_xFrq) {
/*TStr x,y,z,a,b;
real nc,nh,sc,sh;
DBG("   key=`d pbnd=`d vs smpKey=`d smpCnt=`d tnow=`s",
_key, _chn->pbnd-MID14, _smp->key, (sbyte)_smp->cnt, R2Str(t,x));
   nc = _key * 100. +
        (c->pbnr * 100. * ((real)(c->pbnd - MID14) / (real)MID14));
   sc = _smp->key*100. - (sbyte)(_smp->cnt);
   nh = Ct2Hz (nc);
   sh = Ct2Hz (sc);
//DBG("   nt_ct=`s sm_ct=`s nt_hz=`s sm_hz=`s factor=`s",
//R2Str(nc,x), R2Str(sc,y), R2Str(nh,z), R2Str(sh,a),
//R2Str(nh/sh,b));
*/
      t *= ( Ct2Hz ( _key * 100. +
                     (c->pbnr * 100. *
                      ((real)(c->pbnd - MID14) / (real)MID14) // -1..1
                     ) ) /
             Ct2Hz (_smp->key*100. - (sbyte)(_smp->cnt)) );
   }
//DBG("   phInc=`s", R2Str(t,s));
   _phInc = REAL2PHASE (t);
}


void Voice::ReFlt ()                   // set flt res,cut
{ Channel *c = & Sy._chn [_ch];
   _flt.Res (960. * c->res / 127.);
   if (c->vCut == 127) {               // vCut 127 means use chan's cut
TRC("ReFlt vCut=127 so use cut=`d frq=`d",
c->cut,   (int)(c->cut / 127. * 9500. + 4000.));
      _flt.Cut (c->cut / 127. * 9500. + 4000.);
   }
   else {                              // else use velo min'd at vCut
     real cMin = c->vCut / 127. * 9500.;
     real cRng = 9500. - cMin;
TRC("ReFlt vel=`d vCut=`d cMin=`d cRng=`d frq=`d",
_vel, c->vCut, (int)cMin, (int)cRng,
(int)(          _vel * cRng / 127. + cMin + 4000.));
      _flt.Cut (_vel * cRng / 127. + cMin + 4000.);
   }
}


void Voice::ReAmp ()                   // set amp - based on vol cc n velo
{ Channel *c = & Sy._chn [_ch];
   _amp  = Sy._vol * (c->vol / 127.) * (_vel / 127.);
}


void Voice::RePan ()                   // set pan - pan cc
{ Channel *c = & Sy._chn [_ch];
   _panL = (_smp->lr == 1) ? 0. : Pan (c->pan, 0);
   _panR = (_smp->lr == 0) ? 0. : Pan (c->pan, 1);
}


void Voice::Redo (char re)             // reset voice params
{  if      (re == 'n')  ReFrq ();
   else if (re == 'f')  ReFlt ();
   else if (re == 'a')  ReAmp ();
   else if (re == 'p')  RePan ();
   else         /*'*'*/{ReFrq ();   ReFlt ();   ReAmp ();   RePan ();}
//Dump ("Redo");
}


void Voice::Bgn (ubyte c, ubyte k, ubyte v, ubyt4 n, Sound *s, Sample *sm)
// called by synth's NtOn per matching sample of chan's sound
{  _ch = c;   _key = k;   _vel = v;   _vcNo = n;   _snd = s;   _smp = sm;
   _phase = (Phase)0;   _looped = _rels = false;   _nPer = 0;
   Redo ('*');
// _flt.Init ();
   _on = 'd';
}


void Voice::Release ()
// if chan hold, leave on till hold off;  else begin release
{  if (! _nPer)  {End ();      return;}     // never even got heard - KILL!
   if (_rels)                  return;      // already releasin' - skip out
   if (Sy._chn [_ch].hold >= 64)
                 {_on = 's';   return;}     // sustainin'
// samp has no loop and sez NO release - straight ta End
   if ((_smp->lpBgn >= _smp->len) && _snd->_xRls)
                 {End ();      return;}
   _rels = true;                            // start release a goin'
}


void Voice::End ()  {_on = '\0';   _ch = 0xFF;   _snd = nullptr;}


ubyt4 Voice::Interpolate (real *ip)
// stretch sample (per note frq vs. root frq) into _intp dsp buffer
// hi byte of phase fraction tells how to balance our 7 samples at a time
{ ubyt4 d,             sBgn, sEnd, s, nfr = Sy._nFr;
  real *o, *in, *co,   sb0, sb1, sb2,
                       se0, se1, se2;
  Phase ph;
  bool  loopin = (_smp->lpBgn < _smp->len) ? true : false;

   in = & Sy._smp [_smp->pos];   o = ip;

// bump to next halfsample since centered on 4th sample point
   s = d = 0;   ph = _phase + (Phase)0x080000000;

// set sBgn, sEnd per our range of 7 samples
// set sb0-2,se0-2 to the "outside samples" of the 7 in our set
   if (! _looped)  sb0 = sb1 = sb2 = in [sBgn = 0];
   else {                              // sBgn is 0 or lpBgn
      sBgn = _smp->lpBgn;
      sb0 = in [_smp->len-1];   sb1 = in [_smp->len-2];
                                sb2 = in [_smp->len-3];
   }
   sEnd = (_smp->len-1) - 3;
   if (! loopin)   se0 = se1 = se2 = in [_smp->len-1];
   else {                              // sEnd is end-3
      se0 = in [_smp->lpBgn];    se1 = in [_smp->lpBgn+1];
                                 se2 = in [_smp->lpBgn+2];
   }
//TStr s1,s2,s3,s4,s5,s6;
//DBG("      Interpolate sBgn=`d sEnd=`d ph=`d,`u "
//"sb0=`s sb1=`s sb2=`s   se0=`s se1=`s se2=`s",
//sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF),
//R2Str(sb0,s1), R2Str(sb1,s2), R2Str(sb2,s3),
//R2Str(se0,s4), R2Str(se1,s5), R2Str(se2,s6));
   for (;;) {
      s = PHASE_INDEX (ph);
      while ((s == sBgn) && (d < nfr)) {    // 1st sample point
//DBG("         a: d=`d s=`d sBgn=`d sEnd=`d ph=`d,`u",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*sb2     + co[1]*sb1     + co[2]*sb0     +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
         ph += _phInc;   s = PHASE_INDEX (ph);     // bump phase
      }
      sBgn++;

      while ((s == sBgn) && (d < nfr)) {    // 2nd to 1st sample point
//DBG("         b: d=`d s=`d sBgn=`d sEnd=`d ph=`d,`u",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*sb1     + co[1]*sb0     + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
         ph += _phInc;   s = PHASE_INDEX (ph);
      }
      sBgn++;

      while ((s == sBgn) && (d < nfr)) {    // 3rd to 1st sample point
//DBG("         c: d=`d s=`d sBgn=`d sEnd=`d ph=`d,`u",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*sb0     + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
         ph += _phInc;   s = PHASE_INDEX (ph);
      }
      sBgn -= 2;

      while ((s <= sEnd) && (d < nfr)) {    // general case
//DBG("         d: d=`d s=`d sBgn=`d sEnd=`d ph=`d,`u",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
         ph += _phInc;   s = PHASE_INDEX (ph);
      }
      if (d >= nfr)  break;            // break out if buffer filled

      sEnd++;
      while ((s <= sEnd) && (d < nfr)) {    // 3rd to last point
//DBG("         e: d=`d s=`d sBgn=`d sEnd=`d ph=`d,`u",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*in[s+2] + co[6]*se0;
         ph += _phInc;   s = PHASE_INDEX (ph);
      }

      sEnd++;
      while ((s <= sEnd) && (d < nfr)) {    // 2nd to last point
//DBG("         f: d=`d s=`d sBgn=`d sEnd=`d ph=`d,`u",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*in[s+1] + co[5]*se0     + co[6]*se1;
         ph += _phInc;   s = PHASE_INDEX (ph);
      }

      sEnd++;
      while ((s <= sEnd) && (d < nfr)) {    // last point
//DBG("         g: d=`d s=`d sBgn=`d sEnd=`d ph=`d,`u",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                   co[3]*in[s]   +
                   co[4]*se0     + co[5]*se1     + co[6]*se2;
         ph += _phInc;   s = PHASE_INDEX (ph);
      }
      if (! loopin)  break;                 // done !

      if (s > sEnd) {
//DBG("         h: d=`d s=`d sBgn=`d sEnd=`d ph=`d,`u",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
         ph -= LONG2PHASE (_smp->len - _smp->lpBgn);
         if (! _looped) {
            _looped = true;          sb0 = in [_smp->len-1];
            sBgn    = _smp->lpBgn;   sb1 = in [_smp->len-2];
                                     sb2 = in [_smp->len-3];
         }
//DBG("         i: d=`d s=`d sBgn=`d sEnd=`d ph=`d,`u",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
      }
      if (d >= nfr)  break;
      sEnd -= 3;
   }
//DBG("      sBgn=`d sEnd=`d ph=`d,`u d=`d s=`d",
//sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF), d, s);

// sub 1/2 sample from ph since we offset it and save back in _phase
   _phase = ph - (Phase)0x080000000;
   return d;
}


void Voice::Mix ()                     // da GUTS :)
{ ubyt4 i, len;
  real  s, *mL = Sy._mixL, *mR = Sy._mixR, *ip = Sy._intp;
  Channel *c = & Sy._chn [_ch];
   if (! On ())                       return;
   if (_snd == nullptr)    {End ();   return;}
   _nPer++;
   len = Interpolate (ip);             // stretch/shrink sample into _intp
   for (i = 0;  i < len;  i++)  {
//    s  = _flt.Mix (ip [i]);          // filter it
      s  = ip [i];
      s *= _amp;                       // amp it
      mL [i] += (s * _panL);           // pan,mix it
      mR [i] += (s * _panR);
//    rv [i] += (s * c->rvrb/127.);    // mix reverb buf
      if (_rels) {
         if (_vel-- == 0)  {End ();   return;}
         ReAmp ();
      }
   }
   if (len < Sy._nFr)  End ();        // dooone if nonloop sample ran out
//TStr ts, t2;
//DBG("   smp0 L=`s R=`s", R2Str (mL [0],ts), R2Str (mR [0],t2));
//for (i = 0; i < len; i++)  DBG("`d L=`s R=`s",
//i, R2Str (mL [i], ts), R2Str (mR [i], t2));
}


void Voice::Dump ()
{ TStr t, s1, s2, s3;
TRC("on=`c ch=`d key=`s vel=`d looped=`b rels=`b vcNo=`d nPer=`d",
_on?_on:' ', _ch, (_ch==9)?MDrm2Str (t,_key):MKey2Str (t,_key),
_vel, _looped, _rels, _vcNo, _nPer);
TRC(" phase=`u.`u phInc=`u.`u amp=`s panL=`s panR=`s",
(ubyt4)(_phase>>32), (ubyt4)(_phase & 0xFFFFFFFF),
(ubyt4)(_phInc>>32), (ubyt4)(_phInc & 0xFFFFFFFF),
R2Str(_amp,s3), R2Str(_panL,s1), R2Str(_panR,s2));
   if (_snd)  _snd->Dump ();
   if (_smp)  _smp->Dump (_ch == 9);
}
//______________________________________________________________________________
// syn sound bank management
void Syn::WipeSnd ()
{ ubyte t;
   _lok.Grab ();
TRC("   Syn::WipeSnd");
   AllCh (9, 'e');   for (t = 0;  t <= _maxChn;  t++)  AllCh (t, 'e');
//TODO off all the drums too
   for (t = 0;  t < _nSnd;  t++)     {delete _snd [t];   _snd [t] = nullptr;}
   _nSnd = 0;
   for (t = 0;  t <   128;  t++)  if (_drm [t])
                                     {delete _drm [t];   _drm [t] = nullptr;}
   delete [] _smp;   _smp = nullptr;   _nSmp = 0;
   _maxLvl = 1.0;   _maxVc = 0;   _maxChn = 0;
   _lok.Toss ();
}

void Syn::LoadSnd (TStr *snd, ubyte maxch)
{ ubyte s;
  TStr  st;
  ubyt4 sz;
   if (! _run)  return;
TRC("Syn::LoadSnd maxch=`d", maxch);
   WipeSnd ();
   _maxChn = maxch;
   for (s = 0;  s < 128;  s++)  if (snd [s][0]) {
TRC("snd=`d `s", _nSnd,             snd [s]);
      _snd [_nSnd++] = new Sound (  snd [s]);
   }
   for (s = 0;  s < 128;  s++)  if (snd [s+128][0]) {
TRC("drm=`s `s", MDrm2Str (st, s),  snd [s+128]);
      _drm [s]       = new Sound (  snd [s+128], s);
   }

// alloc a big buffer for EVERY sounds' wavs' samples as real
   sz = 0;                             // get ALL samples' len
   for (s = 0;  s < _nSnd;  s++)                 sz += _snd [s]->_siz;
   for (s = 0;  s <   128;  s++)  if (_drm [s])  sz += _drm [s]->_siz;
TRC("alloc _smp[`d]", sz);
   _smp = new real [sz];   _nSmp = sz;   sz = 0;
   if (_smp == nullptr) {
TRC("Syn::LoadSnd  Outa memory - `d samples => `d bytes :(", _nSmp, _nSmp*8);
      for (s = 0; s < _nSnd; s++)
TRC("   Snd `s siz=`d", _snd [s]->_nm, _snd [s]->_siz);
      for (s = 0; s < 128; s++) if (_drm [s])
TRC("   Drm `s siz=`d", _drm [s]->_nm, _drm [s]->_siz);
      WipeSnd ();
      return;
   }
// load n convert -all- sounds' .WAV files' samples to real
TRC("loadin snd n drm");
   for (s = 0;  s < _nSnd;  s++)                 sz = _snd [s]->LoadDat (sz);
   for (s = 0;  s <   128;  s++)  if (_drm [s])  sz = _drm [s]->LoadDat (sz);
TRC("Syn::LoadSnd ok !");
Dump ();
}
//______________________________________________________________________________
// syn note funcs...
void Syn::NOff (ubyte ch, ubyte key, ubyte vel)
{  for (ubyt2 v = 0;  v < _nVc;  v++)
      if (_vc [v].Down () && (_vc [v]._ch == ch) && (_vc [v]._key == key))
          _vc [v].Release ();
}


void Syn::NtOn (ubyte ch, ubyte key, ubyte vel)
{ Channel *c;
  Sound   *s;
  ubyt2 sm, i, j;
  TStr  ts;
   if (vel == 0)  return NOff (ch, key, 0);

   if ((key >= 128) || (vel >= 128)) {
DBG("Syn::NtOn  bad key,vel ch=`d key=`s vel=`d", ch+1, MKey2Str(ts,key), vel);
      return;
   }
   c = & _chn [ch];
   if (ch == 9) {
      if ((s = _drm [key]) == nullptr) {
DBG("Syn::NtOn  no drum sound ch=`d key=`s", ch+1, MDrm2Str(ts,key));
         return;
      }
   }
   else {
      if (c->snd >= _nSnd) {
DBG("Syn::NtOn  no melo sound ch=`d key=`s snd=`d of `d",
ch+1, MKey2Str(ts,key), c->snd, _nSnd);
         return;
      }
      s = _snd [c->snd];
   }

   NOff (ch, key, vel);                // kill any voices similar to meee

// kick new voices for any matchin samples
// find our _vc spot - possibly findin a spot to replace if full
  bool shr  = false;
  real best = 999999., prio;
   for (sm = 0;  sm < s->_nSmp;  sm++)
      if ( (key >= s->_smp [sm].mnKey) && (key <= s->_smp [sm].mxKey) &&
           (vel >= s->_smp [sm].mnVel) && (vel <= s->_smp [sm].mxVel) ) {
         for (i = 0;  i < _nVc;  i++)  if (! _vc [i].On ())  break;
         if (i < _nVc)  shr = true;    // got a free spot to replace
         else {                        // need a new spot
            if (_nVc < 256) {          // room for new one
               i = _nVc++;
               if (i > _maxVc) {_maxVc = i;
DBG("Syn maxVoice=`d", i+1);
               }
            }
            else                       // no room - find who ta kill
               for (j = 0;  j < _nVc;  j++) {
                  prio = 10000.;
                  if      (_vc [j]._ch == 9)  prio += 4000.;
                  else if (_vc [j].Rels ())   prio -= 2000.;
                  if      (_vc [j].Sust ())   prio -= 1000.;
                  prio -= (_vcNo - _vc [j]._vcNo);    // older voice
                  if (prio < best)  {i = j;   best = prio;}
               }                       // ^ THAT guy gets stamped on toppa :/
         }                             // i has a nice spot for us
         _vc [i].Bgn (ch, key, vel, ++_vcNo, s, & s->_smp [sm]);
//DBG("Syn::NtOn  _vc[`d].Bgn", i);
//_vc [i].Dump (StrFmt (ts, "  vc add=`d/`d", i, _nVc));
      }
   if (shr)  {for (i = _nVc;  i && (! _vc [i-1].On ());  i--)  _nVc--;
//if (_nVc<j) TRC("  vc shrink `d=>`d", j, _nVc);
             }
}
//______________________________________________________________________________
// syn CC funcs...
void Syn::UnHold (ubyte ch)            // release all sust'd vcs on chn
{
//DBG(" UnHold ch=`d", ch+1);
   for (ubyt2 i = 0;  i < _nVc;  i++)
      if (_vc [i].Sust () && (_vc [i]._ch == ch))  _vc [i].Release ();
}

void Syn::AllCh (ubyte ch, char todo)  // mod all vcs on channel
{
TRC(" AllCh ch=`d `s", ch+1,
(todo=='n')?"frq"     : (
(todo=='f')?"flt"     : (
(todo=='a')?"amp"     : (
(todo=='p')?"pan"     : (
(todo=='*')?"frq/flt/amp/pan" : (
(todo=='i')?"init"    : (
(todo=='e')?"endNow"  : (
(todo=='r')?"release" : "redo") )))))));
   if (todo == 'i') {               // reset all CCs is kinda different
      _chn [ch].Init ();   _maxLvl = 1.0;   return;
   }                                // else we're dealing with just on vcs
   for (ubyt2 i = 0;  i < _nVc;  i++)
      if (_vc [i].On () && (_vc [i]._ch == ch))
         switch (todo) {
            case 'r':  _vc [i].Release ();   break;     // allnotesoff(rels)
            case 'e':  _vc [i].End ();       break;     // allsoundoff NOW
            default:   _vc [i].Redo (todo);  break;     // recalc vc live
         }                             // todo: n f a p = frq flt amp pan
}
//______________________________________________________________________________
// syn maaain api...
void Syn::Put (ubyte ch, ubyt2 c, ubyte v, ubyte v2)
// setup a chan's voices with CC else start/stop a voice with note
// only drum notes,CCs of ANOFF,ASOFF,ACOFF should be on ch 9
// rest should have hi bit set in chn, drum note in LS7bits
{ char re = '\0';
  TStr s;
   if (! _run)  return;
   _lok.Grab ();

// do note  on/off  (NPrs some day?)
   if (! (c & 0xFF80)) {
TRC("Syn::Put ch=`d `s`c`d",
ch+1, (ch == 9) ? MDrm2Str(s,c) : MKey2Str(s,c),
(v & 0x080) ? ((v2 & 0x080) ? '~' : '_') : '^', v & 0x07F);
      if (v & 0x80)  NtOn (ch, c, v & 0x7F);
      else           NOff (ch, c, v);
      _lok.Toss ();   return;
   }

// else do CC - check valu but always let prog thru
TRC("Syn::Put ch=`d `s v=`d v2=`d", ch+1, MCtl2Str(s,c,'r'), v, v2);
   if ((v >= 128) && (c != MC_PROG)) {
DBG("Syn::Put bad valu  ch=`d cc=`d valu=`d", ch+1, c, v);
       _lok.Toss ();   return;}
   switch (c) {
      case MC_PROG:
         if ((ch == 9) || (v > _nSnd))
DBG("Syn::Put M_PROG on drum chan or too high");
         else  _chn [ch].snd = v;
         _lok.Toss ();   return;
      case MC_CC|M_ANOFF: re = 'r';   break;   // release em
      case MC_CC|M_ASOFF: re = 'e';   break;   // end em
      case MC_CC|M_ACOFF: re = 'i';   break;   // reset all CCs

   // ...better not be any chan 9s after this
      case MC_CC|M_HOLD:  if ((_chn [ch].hold = v) < 64)  UnHold (ch);
                          _lok.Toss ();   return;
   // above ones don't do voice recalc;  below ones do
      case MC_PBND:       _chn [ch].pbnd = v << 7 | v2;   re = 'n';   break;
      case MC_CC|16:      _chn [ch].pbnr = v;             re = 'n';   break;
      case MC_CC|17:      _chn [ch].vCut = v;             re = 'f';   break;
      case MC_CC|18:      _chn [ch].cut  = v;             re = 'f';   break;
      case MC_CC|19:      _chn [ch].res  = v;             re = 'f';   break;
      case MC_CC|M_VOL:   _chn [ch].vol  = v;             re = 'a';   break;
      case MC_CC|M_PAN:   _chn [ch].pan  = v;             re = 'p';   break;
/* reverb fixed till i find algos that don't suck
**    case MC_CC|24:      _chn [ch].rvrb = v;             re = 'p';   break;
**
** // not channel specific fx params
**    case MC_CC|30:      _fxP.rRoom  = v;                re = 'x';   break;
**    case MC_CC|31:      _fxP.rDamp  = v;                re = 'x';   break;
**    case MC_CC|32:      _fxP.rWidth = v;                re = 'x';   break;
**    case MC_CC|33:      _fxP.rLevel = v;                re = 'x';   break;
*/
      default:            _lok.Toss ();   return;
   }
   AllCh (ch, re);
   _lok.Toss ();
}


void Syn::Dump ()  // sounds, samples, channels, voices
{ ubyte s;
  ubyt2 i, ns;
  TStr  ts;
  Sample *sm;                          // first melo, then drum sounds...
   for (s = 0;  s < _nSnd;  s++) {
      TRC("Snd=`d/`d:", s, _nSnd);       _snd [s]->Dump ();
      for (i = 0, ns = _snd [s]->_nSmp,
                  sm = _snd [s]->_smp;  i < ns;  i++, sm++)
         {TRC("   Smp=`d/`d:", i, ns);   sm->Dump (false);}
   }
   for (s = 0; s < 128; s++)  if (_drm [s]) {
      TRC("Drm=`s:", MDrm2Str (ts,s));   _drm [s]->Dump ();
      for (i = 0, ns = _drm [s]->_nSmp,
                  sm = _drm [s]->_smp;  i < ns;  i++, sm++)
         {TRC("   Smp=`d/`d:", i, ns);   sm->Dump (true);}
   }
   for (ubyte c = 0;  c <= _maxChn;  c++)  _chn [c].Dump ();
   for (ubyt2 v = 0;  v <  _nVc;     v++)  _vc  [v].Dump ();
}
//______________________________________________________________________________
sbyt2 Syn::r2i (real r, real dth)
{ TStr  s;
  sbyt2 i;
   if (fabs (r) > _maxLvl) {_maxLvl = fabs (r);
DBG("Syn maxLevel=`s", R2Str (_maxLvl,s));
   }
   r = r * 32766.0 / _maxLvl + dth;    // scale to sbyt2 n dither
   i = (sbyt2)((r >= 0.) ? (r+0.5) : (r-0.5));   // round to sbyt2 -32767..32767
   return i;
}


void Syn::run ()
// live rendering: send interleaved stereo sbyt2 samples to soundcard
{ ubyt4 sz  = _nFr * sizeof (real);
  ubyte per = 0;
  ubyt2   i;
  sbyt2 (*o)[2];
DBGTH("Syn");   DBG("run bgn");
   while (_run) {
      o = & _out [per*_nFr];   per = per ? 0 : 1;     // double bufferin
      MemSet (_mixL, 0, sz);   MemSet (_mixR, 0, sz);   //MemSet (_rvrb, 0, sz);
      _lok.Grab ();
      for (i = 0;  i < _nVc;  i++)  _vc [i].Mix ();
//    _rvP.Mix (_rvrb, _mixL, _mixR);
      for (i = 0;  i < _nFr;  i++) {   // sound card wants interleaved ints
         o [i][0] = r2i (_mixL [i], Dither [0][_dth]);
         o [i][1] = r2i (_mixR [i], Dither [1][_dth]);
         if (++_dth >= MAX_DITHER)  _dth = 0;
      }
      _lok.Toss ();
      _sn->Put ((sbyt2 *)o);           // this'll block us on 2nd call and on
   }
DBG("run end");
}


void Syn::Init ()
// open sound output device
// init samples, sounds, voices, buffers, and kick our soundcard writin thread
{  _run = false;                       // default to "no workie"
  TStr fn;
  File f;                              // MidiCfg picked sound descrip
  ulong ln;
   App.Path (fn, 'd');   StrAp (fn, CC("/device/syn.txt"));
   ln = f.Load (fn, _snDsc, sizeof (_snDsc)-1);
   _snDsc [ln] = '\0';
   while (ln && (_snDsc [ln-1] == '\n'))  _snDsc [--ln] = '\0';

   Snd.Load ();   StrCp (_snDev, Snd.Get (_snDsc));   // resolve to device
DBG("Syn::Init - sound output='`s' device='`s'", _snDsc, _snDev);
   if (*_snDev == '\0')
{DBG("   no sound device :(");       return;}
   if (*_snDev == '?')
{DBG("   sound device is off :(");   return;}
   _sn = new SndO (_snDev);
   if (_sn->Dead ())  return;          // with _run false :(

   _nFr = _sn->_nFr;                   // everbody needz theedz
   _frq = _sn->_frq;
DBG("   nFr=`d frq=`d", _nFr, _frq);

   InitLookup ();
   _vol = 0.07;                        // alsa always does volume=100% :)
   _smp = nullptr;
   MemSet (_snd, 0, sizeof (_snd));   _nSnd = 0;
   MemSet (_drm, 0, sizeof (_drm));
   for (ubyte c = 0;  c < 128;  c++)  _chn [c].Init ();
   for (ubyt2 v = 0;  v < 256;  v++)  _vc  [v].Init ();
   _maxChn = 0;
   _maxVc  = _nVc = _vcNo = _dth = 0;
   _maxLvl = 1.0;
   _intp = new real [_nFr];   _mixL = new real [_nFr];
                              _mixR = new real [_nFr];
   _out = new sbyt2 [2*_nFr][2];       // 2 periods of nFr frames of interleaved

   _run = true;   start ();            // stereo sbyt2 (to double buffer)
}


void Syn::Quit ()
{
TRC("Syn::Quit bgn");
   if (_run)  {_run = false;   wait ();   WipeSnd ();}
   if (_out)   delete [] _out;
   if (_intp)  delete [] _intp;   if (_mixL)  delete [] _mixL;
                                  if (_mixR)  delete [] _mixR;
   if (_sn)    delete _sn;
TRC("Syn::Quit end");
}

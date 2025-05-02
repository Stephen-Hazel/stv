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

static real Str2R (char *s)
{ QString st = s;
  bool ok;
  real r = st.toDouble (& ok);
   return ok ? r : 0.0;
}
                                       // turn pesky midi values into nicer junk
static bool Mb  (ubyte c)  {return (c >= 64) ? true : false;}
static real Mu  (ubyte c)  {return (real)c/127.;}
static real Mu2 (ubyt2 c)  {return (real)c/16383.;}
static real Ms  (ubyte c)  {if (c == 0) c = 1;   return (  64.-(real)c)/  63.;}
static real Ms2 (ubyt2 c)  {if (c == 0) c = 1;   return (8192.-(real)c)/8191.;}


// lookup stufffff _____________________________________________________________
const ubyt4 MAX_DITHER = 48000;

static real     Interp [256][2];       // interpolation coefficients
static void InitInterp ()              // 256 cuz hi byte of phase frac=> index
{  for (ubyt2 i = 0;  i < BITS (Interp);  i++) {
     real x = (real)i / (real)BITS (Interp);
      Interp [i][0] = 1.0 - x;   Interp [i][1] = x;
   }
}

static real     Dither [2][MAX_DITHER];     // per l/r channel
static void InitDither (void)               // rand real btw -.999 and +.999
// add a smidge of randomness to limit truncation distortion goin real=>sbyt4
{ real  d, dp;                         // tricky - read up on ole google
  sbyt4 c, i;
   for (c = 0;  c < 2;  c++) {
      for (dp = 0., i = 0;  i < MAX_DITHER-1;  i++) {
         d = rand () / (real)RAND_MAX - 0.5;
         Dither [c][i] = d - dp;   dp = d;
      }
      Dither [c][MAX_DITHER-1] = 0 - dp;
   }
}
                                       // conversion tables...
static real Cnv_ct2hz [1200];          // cents to hz
static real     Ct2Hz (real ct)
{  if (ct <     0.) return (real)    1.;
   if (ct <   900.) return (real)    6.875 * Cnv_ct2hz [(sbyt4)(ct + 300)];
   if (ct <  2100.) return (real)   13.75 * Cnv_ct2hz [(sbyt4)(ct -  900)];
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

static real Cnv_pan [127];             // 0..126 to pan (sin(0..PI/2)
static real     Pan (ubyte c, ubyte lr)     //TODO check this :/
{  if (c ==  0)  c =   1;              // so -64 => -63
   if (c > 127)  c = 127;              // limit at -63 .. 63
   c--;   if (lr == 0)  c = 126 - c;   // now 0..126
   return Cnv_pan [c];
}

static real Cnv_sin [4096];            // 1/4 sin table lookup (other 3/4 calcd)
static real     Sin (real in, ubyte q = 0)  // q is quad of the full sin curve
{ ubyt2 i;
   if (in <= 0.) i = 0;   else if (in >= 1.) i = 4095;
   else          i = (ubyt2) round (in*4095.);
  real o = Cnv_sin [(q & 0x01) ? (4095-i) : i];  // sin or cos (q1,2 default)
   if (q >= 2)  return -o;             // neg swing of sin/cos (q3,4)
   return o;
}

static void InitLookup ()
{ sbyt4 i;
  real  x;
   for (i = 0;  i < BITS (Cnv_ct2hz);  i++)
                              Cnv_ct2hz [i] = (real) pow (2., (real) i / 1200.);
   x = M_PI/2. /   (BITS (Cnv_pan) - 1.);
   for (i = 0;  i < BITS (Cnv_pan);  i++)  Cnv_pan [i] = (real) sin (i * x);
   x = M_PI/2. /   (BITS (Cnv_sin) - 1.);
   for (i = 0;  i < BITS (Cnv_sin);  i++)  Cnv_sin [i] = (real) sin (i * x);
   InitInterp ();   InitDither ();
//TStr ts; for (i = 0; i < BITS (Cnv_sin); i++) DBG("sin `d=`s",
//                                                  i, R2Str (Cnv_sin[i],ts));
}
//______________________________________________________________________________
void Channel::Init ()                  // channels are easy as pie at least
{  hold = snd = fRes = vCut = glide = glRate = pNt = nNt = 0;
   pBnd = MID14;   pBnR = 2;   vol = 100;   fCut = 127;   pan = glFrom = 64;
   nTm = 0;   *env = '\0';
}

void Channel::Dump ()
{
TRX("   snd=`d hold=`d pBnd=`d pBnr=`d fCut=`d fRes=`d vCut=`d vol=`d pan=`d\n"
    "   glide=`d glRate=`d glFrom=`d nTm=`d nNt=`d pNt=`d\n"
    "   env=`s",
        snd, hold, (int)MID14-pBnd, pBnR, fCut, fRes, vCut, vol, pan,
        glide, glRate, glFrom, nTm, nNt, pNt, env);
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
   TRX("      key=`s-`s vel=`d-`d fn=`s",
       dr ? MDrm2Str(s1,mnKey) : MKey2Str(s1,mnKey),
       dr ? MDrm2Str(s2,mxKey) : MKey2Str(s2,mxKey), mnVel, mxVel, fn);
   TRX("      lr=`d lpBgn=`d len=`d pos=`d frq=`d key=`s cnt=`d",
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
{  TRX("   `s nSmp=`d siz=`d xFrq=`b xRls=`b",
           _nm,  _nSmp,  _siz,  _xFrq,  _xRls);  // " pa=`s", _pa);
}


ubyt4 Sound::LoadDat (ubyt4 pos)
// actually load each .WAV we found into smp buffer n offset our poss by len
{ TStr  fn, s1;
  Wav   w;
  sbyt4 si, lenD, s, p1 = pos;
  ubyt2 i;
  ubyte nby, flo, *p, a, b;
  real  sr, max = 0.;
TRX("Sound::LoadDat pos=`d", pos);
   for (i = 0;  i < _nSmp;  i++) {
      StrFmt (fn, "`s/device/syn/`s/`s",  App.Path (s1, 'd'), _pa, _smp [i].fn);
      w.Load (fn);                     // already know it's there n ok
      _smp [i].pos   = pos;            // already got len,lr,key n vel ranges
      _smp [i].lpBgn = (w._loop && (! _xRls)) ? w._lBgn : w._len;
      _smp [i].key   = w._key;
      _smp [i].cnt   = w._cnt;
      _smp [i].frq   = w._frq;
      nby = w._byts;   flo = w._real;
TRX("   got smp `d/`d pos=`d", i, _nSmp, pos);
      for (p = & SC(ubyte *,w._mem) [((! w._mono) && (_smp [i].lr == 1))
                                     ? nby : 0],
           s = 0;  s < _smp [i].len;  s++) {
         if (flo)  {sr = (nby <= 4) ? *((float *)p) : *((double *)p);
                    p +=  nby;}
         else {
            si = 0;
            switch (nby) {             // single byte is weird; 2,3 similar; 4ez
               case 1:  si  = ((sbyt4)(*p++) - 128) << 24;   break;
               case 3:  si |= ((ubyt4)(*p++) <<  8);
               case 2:  si |= ((ubyt4)(a = *p++) << 16);
                        si |= ((ubyt4)(b = *p++) << 24);     break;
               case 4:  si  = *((sbyt4 *)p);   p += 4;       break;
            }
            sr = (real)si / MAXS4;
//DBG("p=`d a=`u b=`u int=`d r=`s", pos, a, b, si, R2Str(sr,s1));
         }
         if (fabs (sr) > max)  max = fabs (sr);
         Sy._smp [pos++] = sr;
         if (! w._mono)  p += nby;
      }
   }
// scale all samples into +/- 1.0
TStr s2;
   for (;  p1 < pos;  p1++) {
//DBG(" scale p=`d `s=>`s", p1, R2Str(Sy._smp [p1],s1),
//                              R2Str(Sy._smp [p1] * (1.0 / max),s2));
      Sy._smp [p1] *= (1.0 / max);
   }
TRX("Sound::LoadDat max=`s  new pos=`d",  R2Str(max,s1), pos);
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
TRX("Sound::Sound _nSnd=`d snd=`s dKey=`s",
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
TRX("   _pa=`s xFrq=`b xRls=`b", _pa, _xFrq, _xRls);

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
TRX("   nSmp=`d", _nSmp);
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
void Glide::Init (ubyte ch, ubyte key)
// glide (portamento) - tricky yet cool - scoot note freq from prev note
// get note we're comin from and it's offset to this key - ofs
// get #buffers to spread this ofs over - inc (per buffer)
{ Channel *c = & Sy._chn [ch];
  ubyte    pn = c->pNt;
   ofs = inc = 0.0;                    // no can do?
   if ((! Mb (c->glide)) || (! pn) || (pn == key) || (ch == 9))  return;

// ok, we're doin it !
   if (c->glFrom != 64)  pn = (ubyte)((ubyt2)key + c->glFrom - 64);
//TStr s1,s2;
//DBG("ch=`d glide `s => `s rate=`d",
//ch, MKey2Str(s1,pn), MKey2Str(s2,key), c->glRate);
   ofs = ((real)pn - key) * 100.0;
//DBG("   ofs=`s inc=`s", R2Str(ofs,s1),R2Str(inc,s2));

// OLD/calc:  samp/sec / samp/buf => buf/sec so s4 buffers use 4 secs
//real s4 = 4.0 * (real)Sy._frq / Sy._nFr;
// inc = -ofs / (s4 * (c->glRate+1) / 128.0);    // not enough quick vals tho

// SHOISH - 4 periods per rate step startin at 20 - max of about 0.75 secs
   inc = -ofs / (20.0 + 4.0 * c->glRate);
}

char Glide::Mix ()
{  if (ofs == 0.)  return '\0';        // not doin or done?
   ofs += inc;                         // ma new dude
   if (inc > 0.)  {if (ofs >= 0.)  {ofs = 0.;  return 'e';}}    // e to dbg :/
   else            if (ofs <= 0.)  {ofs = 0.;  return 'e';}
   return 'y';
}
//______________________________________________________________________________
void Env::Init (ubyte iid)
// init my destination n stages' stuff.
// songtime_dur/192 beat / (tmpo beat/min * min/60 sec)
//    dur beat / (tmpo beat/60 sec)
//    dur beat * (60 sec/tmpo beat)
//    dur * 60 / tmpo sec                 sec
//    frq sample/sec / nfr sample/buf
//    frq sample/sec * buf/nfr sample     buf/sec => bufs
//    frq/nfr buf/sec
{ ubyte i;
  real  np, l2, c;
   id  = iid;
   dst =            Sy._env [id].dst;
   stg = & Sy._stg [Sy._env [id].stg];
   s = 0;   p = 0;   lvl = stg [0].lvl;     // initial stg pos, per, n level
   for (i = 0;  stg [i].dur;  i++) {        // lvl,dur,crv => mul,add  per stg
      stg [i].nper = np =                   // songtime dur => # period buffers
         (real)stg [i].dur / (M_WHOLE/4.) / (Sy._in.tmpo / 60.) *
                                                    (real)Sy._frq/(real)Sy._nFr;
      if ((c = stg [i].crv) < 0.) continue; // curve ratio: sin has no init

      if (c < 0.000000001)  c = 0.000000001;     // -180 dB
      stg [i].mul = exp (-log ((1.0 + c) / c) / np);

   // offset add factor a little to REACH target, not just approach
      stg [i].add = (stg [i+1].lvl + stg [i].dir*c) * (1. - stg [i].mul);
   }
}

real Env::Mix ()
{  if (End () && stg [s].add == 0.)  return lvl;
  sbyte d  = stg [s  ].dir;            // git ma next dude
  real  l2 = stg [s+1].lvl;
   if (stg [s].crv >= 0.)
      lvl = lvl * stg [s].mul + stg [s].add;     // exp crv
   else {                                        // sin curve
     ubyte q = (d > 0) ? 0 : 1;
      if ((lvl < 0.) || (l2 < 0.))  q += 2;
      lvl = Sin (++p/stg [s].nper, q);
   }                                   // same ole stage?
   if (d > 0)  {if (lvl < l2)  return lvl;}
   else         if (lvl > l2)  return lvl;

   s++;   lvl = l2;   p = 0;                // bump ta next stage !
   if (End ()) {
      if (stg [s].add == 0.)  return lvl;   // not loopin so dooone
      lvl = stg [s = 0].lvl;                // loop back ta stg 0
   }
   return lvl;
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
{ sbyt4 k;
  real  t;
  ubyte mn = MKey (CC("0a")),
        mx = MKey (CC("8c"));
   t = (real)_smp->frq / (real)Sy._frq;
   if (! _snd->_xFrq) {
      k = _key;                        // 2100 - 10800  (0 - 8700)
      if (k < mn)  k = mn;             // limit to keyboard
      if (k > mx)  k = mx;                            // pBnR cents iz dumbb
      t *= ( Ct2Hz ( k         * 100. +
                     _gl.ofs +
//                   _eVal [0] * 100. + _eVal [1] +
                     _c->pBnR  * 100. * Ms2 (_c->pBnd) ) /
             Ct2Hz (_smp->key*100. - (sbyte)(_smp->cnt)) );
   }
   _phInc = REAL2PHASE (t);
// TStr s; DBG("   phInc=`s", R2Str(t,s));
}


void Voice::ReFlt ()                   // set filter cutoff,resonance
{ real cut = Mu (_c->fCut) * _eVal [2],
       res = Mu (_c->fRes) * _eVal [3];
   if ((_ch != 9) && Mb (_c->vCut))  cut *= Mu (_vel);
TStr s1, s2;
TRX("ReFlt cut=`s ecut=`s vCut=`b",
R2Str(cut,s1), R2Str(_eVal [2], s2), Mb (_c->vCut));
   _flt.Cut (9500.0 * cut + 4000.);  // 4000 - 13500 = 0 - 9500
   _flt.Res ( 960.0 * res);          //                0 - 960
}


void Voice::ReAmp ()                   // set amp - based on vol cc n velo
{  _amp  = Sy._vol * Mu (_c->vol) * Mu (_vel) * _eVal [4];  }


void Voice::RePan ()                   // set pan - pan cc
{  _panL = (_smp->lr == 1) ? 0. : Pan (_c->pan * _eVal [5], 0);
   _panR = (_smp->lr == 0) ? 0. : Pan (_c->pan * _eVal [5], 1);
}


void Voice::Re (char re)               // recalc voice params
{  if      (re == 'n')  ReFrq ();      // oscillator (note)
   else if (re == 'f')  ReFlt ();      // filter
   else if (re == 'a')  ReAmp ();
   else if (re == 'p')  RePan ();
   else         /*'*'*/{ReFrq ();   ReFlt ();   ReAmp ();   RePan ();}
//Dump ("Re");
}


void Voice::Bgn (ubyte c, ubyte k, ubyte v, ubyt4 n, Sound *s, Sample *sm,
                 char *es)
// called by synth's NtOn per matching sample of chan's sound (usually 2x: L,R)
{  _c = & Sy._chn [_ch = c];   _key = k;   _vel = v;   _vcNo = n;
   _snd = s;   _smp = sm;   _phase = (Phase)0;   _looped = false;   _nPer = 0;
   _flt.Init ();   _gl.Init (c, k);
   StrCp (_eStr, (es != nullptr) ? es : _c->env);
  ColSep ss (_eStr, 17);               // envelope init from it's str
  ubyte  i, j;
   for (i = 0;  i < 6;  i++)  _eVal [i] = 1.;
   for (i = _env.Ln = 0;  (i < 16) && ss.Col [i][0];  i++)
      for (j = 0;  j < Sy._env.Ln;  j++)
         if (! StrCm (ss.Col [i], Sy._env [j].nm))
            {_env.Ins ();   _env [i].Init (j);}
   if ((i == 0) || StrCm (ss.Col [i-1], CC("rel"))) {
      if (i >= 16) i--;   else _env.Ins ();
      _env [i].Init (0);               // env bank 0 is default release
   }
   Re ('*');
   _on = 'd';
}


void Voice::Rels ()                    // noteoff or hold=off
// never even got heard?  or samp has no loop and sez NO release?  KILL!
// if chan hold, hold till hold off
// else set state r which includes release in env[] to force amp to 0
{  if ((! _nPer)  ||  ((_smp->lpBgn >= _smp->len) && _snd->_xRls))
      {End ();      return;}
   if (_on == 'r')  return;            // already aaam
   _on = Mb (_c->hold) ? 'h' : 'r';
}


void Voice::End ()  {_on = '\0';   _ch = 0xFF;   _snd = nullptr;}


ubyt4 Voice::Osc ()
// just linear!  ez, fast, good enough.
// stretch sample (per note frq vs. root frq) into _intp dsp buffer (o here)
// hi byte of phase fraction tells how to balance our 2 samples at a time
{ ubyt4 s,  sEnd,   d,   nfr = Sy._nFr;
  real *in, se,    *o, *co;
  Phase ph;
  bool  loopin = (_smp->lpBgn < _smp->len) ? true : false;

   in = & Sy._smp [_smp->pos];   o = Sy._intp;
   ph = _phase;                  d = 0;

// sEnd,se to 2nd interp pos,value
   sEnd = (_smp->len-1) - 1;           // after this pos, 2nd sample is special
   if (! loopin)   se = in [_smp->len-1];              // 2nd sample for interp
   else            se = in [_smp->lpBgn];

   for (;;) {
      s = PHASE_INDEX (ph);
      while ((s <= sEnd) && (d < nfr)) {
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s] + co[1]*in[s+1];
         ph += _phInc;   s = PHASE_INDEX (ph);   // bump phase
      }
      if (d >= nfr)  break;                      // break out if buffer filled

      sEnd++;

      while ((s <= sEnd) && (d < nfr)) {         // interp last point
         co = Interp [PHASE_FRACT (ph)];
         o [d++] = co[0]*in[s] + co[1]*se;
         ph += _phInc;   s = PHASE_INDEX (ph);
      }
      if (! loopin)  break;                      // done !

      if (s > sEnd) {                            // back to loop start
         ph -= LONG2PHASE (_smp->len - _smp->lpBgn);
         if (! _looped)  _looped = true;
      }
      if (d >= nfr)  break;

      sEnd--;                                    // back to 2nd to last sample
   }

   _phase = ph;                        // store it for next time
   return d;
}


void Voice::Mix ()                     // da GUTS :)
{ ubyt4 i, ne, len;
  real  s, *mL = Sy._mixL, *mR = Sy._mixR;
  char  re [6], d;
   if (! _on)                         return;
   if (_snd == nullptr)    {End ();   return;}

   _nPer++;                            // track how many bufs we did
   len = Osc ();                       // stretch/shrink sample into _intp

// run all the _env[]  only do last (rel) upon note up
   for (i = 0;  i < 6;  i++)  _eVal [i] = 1.;
   ne = _env.Ln;   if (_on!='r') ne--;      // last env only upon release
   for (i = 0;  i < ne;  i++)
      {d = _env [i].dst;   re [d] = 1;   _eVal [d] *= _env [i].Mix ();}
   if (re [0] || re [1])  Re ('n');   if (re [4]) Re ('a');
   if (re [2] || re [3])  Re ('f');   if (re [5]) Re ('p');
Dump('q');

// mix in the buffer
   for (i = 0;  i < len;  i++)  {
      s  = _flt.Mix (Sy._intp [i]);    // filter it
      s *= _amp;                       // amp it
      mL [i] += (s * _panL);           // pan it
      mR [i] += (s * _panR);
   }
   if (_gl.Mix ())  ReFrq ();

// done w release?  or nonloop sample ran out?  END MEEE
   if (_env [_env.Ln-1].End () || (len < Sy._nFr))  End ();
}


void Voice::Dump (char q)
{ TStr t, s1, s2, s3;
   if (! _on)  {TRX("   (off)");   return;}
TRX("   on=`c ch=`d key=`s vel=`d looped=`b vcNo=`d nPer=`d",
_on, _ch+1, (_ch==9)?MDrm2Str (t,_key):MKey2Str (t,_key),
_vel, _looped, _vcNo, _nPer);
   for (ubyte i = 0;  i < _env.Ln;  i++)
TRX("   env `d=`s", i, Sy._env [_env [i].id].nm);
// Sy._chn [_ch].Dump ();
   if (q)  return;
TRX("   phase=`u.`u phInc=`u.`u amp=`s panL=`s panR=`s",
(ubyt4)(_phase>>32), (ubyt4)(_phase & 0xFFFFFFFF),
(ubyt4)(_phInc>>32), (ubyt4)(_phInc & 0xFFFFFFFF),
R2Str(_amp,s3), R2Str(_panL,s1), R2Str(_panR,s2));
   _c->Dump ();
   if (_snd)  _snd->Dump ();
   if (_smp)  _smp->Dump (_ch == 9);
}
//______________________________________________________________________________
// syn sound bank management
void Syn::WipeSnd ()
{ ubyte t;
   if (_run)  _lok.Grab ();
TRX("   Syn::WipeSnd");
   AllVc (9, 'e');   for (t = 0;  t <= _maxChn;  t++)  AllVc (t, 'e');
//TODO off all the drums too
   for (t = 0;  t < _nSnd;  t++)     {delete _snd [t];   _snd [t] = nullptr;}
   _nSnd = 0;
   for (t = 0;  t <   128;  t++)  if (_drm [t])
                                     {delete _drm [t];   _drm [t] = nullptr;}
   delete [] _smp;   _smp = nullptr;   _nSmp = 0;
   _maxLvl = 1.0;   _maxVc = 0;   _maxChn = 0;
   if (_run)  _lok.Toss ();
}

void Syn::LoadSnd (TStr *snd, ubyte maxch)
{ ubyte s;
  TStr  st;
  ubyt4 sz;
TRX("Syn::LoadSnd maxch=`d", maxch);
   WipeSnd ();
   if (_run)  _lok.Grab ();
   _maxChn = maxch;
   for (s = 0;  s < 128;  s++)  if (snd [s][0]) {
TRX("snd=`d `s", _nSnd,             snd [s]);
      _snd [_nSnd++] = new Sound (  snd [s]);
   }
   for (s = 0;  s < 128;  s++)  if (snd [s+128][0]) {
TRX("drm=`s `s", MDrm2Str (st, s),  snd [s+128]);
      _drm [s]       = new Sound (  snd [s+128], s);
   }

// alloc a big buffer for EVERY sounds' wavs' samples as real
   sz = 0;                             // get ALL samples' len
   for (s = 0;  s < _nSnd;  s++)                 sz += _snd [s]->_siz;
   for (s = 0;  s <   128;  s++)  if (_drm [s])  sz += _drm [s]->_siz;
TRX("alloc _smp[`d]", sz);
   _smp = new real [sz];   _nSmp = sz;   sz = 0;
   if (_smp == nullptr) {
TRX("Syn::LoadSnd  Outa memory - `d samples => `d bytes :(", _nSmp, _nSmp*8);
      for (s = 0; s < _nSnd; s++)
TRX("   Snd `s siz=`d", _snd [s]->_nm, _snd [s]->_siz);
      for (s = 0; s < 128; s++) if (_drm [s])
TRX("   Drm `s siz=`d", _drm [s]->_nm, _drm [s]->_siz);
      WipeSnd ();
      if (_run)  _lok.Toss ();
      return;
   }
// load n convert -all- sounds' .WAV files' samples to real
TRX("loadin snd n drm");
   for (s = 0;  s < _nSnd;  s++)                 sz = _snd [s]->LoadDat (sz);
   for (s = 0;  s <   128;  s++)  if (_drm [s])  sz = _drm [s]->LoadDat (sz);
   if (_run)  _lok.Toss ();
TRX("Syn::LoadSnd end");
//Dump('s');
}
//______________________________________________________________________________
// syn note funcs...
void Syn::NOff (ubyte ch, ubyte key, ubyte vel)
{  for (ubyt2 v = 0;  v < _nVc;  v++)
      if ((_vc [v]._on == 'd') && (_vc [v]._ch == ch) && (_vc [v]._key == key))
         _vc [v].Rels ();
}


void Syn::NtOn (ubyte ch, ubyte key, ubyte vel, char *es)
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
         for (i = 0;  i < _nVc;  i++)  if (! _vc [i]._on)  break;
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
                  if      (_vc [j]._ch == 9)    prio += 4000.;
                  else if (_vc [j]._on == 'r')  prio -= 2000.;
                  else if (_vc [j]._on == 'h')  prio -= 1000.;
                  prio -= (_vcNo - _vc [j]._vcNo);    // older voice
                  if (prio < best)  {i = j;   best = prio;}
               }                       // ^ THAT guy gets stamped on toppa :/
         }                             // i has a nice spot for us

      // FINALLY !!
         _vc [i].Bgn (ch, key, vel, ++_vcNo, s, & s->_smp [sm], es);

//TRX("start voice=`d", i);
      }
   if (shr)  {for (j = i = _nVc;  i && (! _vc [i-1]._on);  i--)  _nVc--;
//if (_nVc<j) TRX("  vc shrink `d=>`d", j, _nVc);
             }
//for (i = 0;  i < _nVc;  i++) {DBG("`d", i);  _vc [i].Dump ('q');}
}
//______________________________________________________________________________
// syn CC funcs...
void Syn::UnHold (ubyte ch)            // release all held vcs on chn
{  for (ubyt2 i = 0;  i < _nVc;  i++)
      if ((_vc [i]._on == 'h') && (_vc [i]._ch == ch))  _vc [i].Rels ();
}

void Syn::AllVc (ubyte ch, char todo)  // mod all vcs on channel
{
TRX(" AllVc ch=`d `s", ch+1,
(todo=='n')?"frq"     : (
(todo=='f')?"flt"     : (
(todo=='a')?"amp"     : (
(todo=='p')?"pan"     : (
(todo=='*')?"frq/flt/amp/pan" : (
(todo=='i')?"init"    : (
(todo=='e')?"endNow"  : (
(todo=='r')?"release" : "redo") )))))));
// reset all CCs is kinda different
   if (todo == 'i')  {_chn [ch].Init ();   _maxLvl = 1.0;   return;}
// else we're dealing with just on vcs
   for (ubyt2 i = 0;  i < _nVc;  i++)  if (_vc [i]._on && (_vc [i]._ch == ch))
      switch (todo) {
         case 'e':  _vc [i].End  ();     break;  // allsoundoff NOW
         case 'r':  _vc [i].Rels ();     break;  // allnotesoff(rels)
         default:   _vc [i].Re (todo);   break;  // recalc vc live
      }                                // todo: n f a p = frq flt amp pan
}
//______________________________________________________________________________
// syn maaain api...

void Syn::Put (ubyte ch, ubyt2 c, ubyte v, ubyte v2, char *es)
// setup a chan's voices with CC else start/stop a voice with note
// only drum notes,CCs of ANOFF,ASOFF,ACOFF should be on ch 9
// rest should have hi bit set in chn, drum note in LS7bits
{ char re = '\0';
  TStr s;
  Channel *chn = & _chn [ch];
   if (_run)  _lok.Grab ();

// do note on/off
   if (! (c & 0xFF80)) {
DBG("Syn::Put ch=`d `s`c`d",
ch+1, (ch == 9) ? MDrm2Str(s,c) : MKey2Str(s,c),
(v & 0x080) ? ((v2 & 0x080) ? '~' : '_') : '^', v & 0x07F);
      if (v & 0x80) {
         if (ch != 9) {                // track pNt of ch for glide
            if ((Sy._in.time < chn->nTm) ||      // musta restarted
                (Sy._in.time > chn->nTm+24)) {   // 32nd nt
               chn->pNt = chn->nNt;    // click!
               chn->nNt = c;           // save these for next time
               chn->nTm = Sy._in.time;
            }
            else                       // use lowest note of chord for pNt
               if (c < chn->nNt)  chn->nNt = c;
         }
         NtOn (ch, c, v & 0x7F, es);
      }
      else
         NOff (ch, c, v);
      if (_run)  _lok.Toss ();
      return;
   }

// else do CC - check valu but always let prog thru
TRX("Syn::Put ch=`d `s v=`d v2=`d", ch+1, MCtl2Str(s,c,'r'), v, v2);
   if ((v >= 128) && (c != MC_PROG)) {
DBG("Syn::Put bad valu  ch=`d cc=`d valu=`d", ch+1, c, v);
      if (_run)  _lok.Toss ();
      return;
   }
   switch (c) {
      case MC_PROG:       if ((v < _nSnd) && (ch != 9))  chn->snd = v;  else
DBG("Syn::Put PROGCH past bank or on drums");
                                      break;
      case MC_CC|M_ANOFF: re = 'r';   break;     // release em
      case MC_CC|M_ASOFF: re = 'e';   break;     // end em
      case MC_CC|M_ACOFF: re = 'i';   break;     // reset all CCs

   // ...better not be any chan 9s after this
      case MC_CC|M_HOLD:  if (! Mb (chn->hold = v))  UnHold (ch);   break;
      case MC_PBND:       chn->pBnd = v << 7 | v2;   re = 'n';      break;
      case MC_RP|0:       chn->pBnR = v;   re = 'n';   break;
      case MC_CC|16:      chn->fCut = v;   re = 'f';   break;
      case MC_CC|17:      chn->fRes = v;   re = 'f';   break;
      case MC_CC|18:      chn->vCut = v;   re = 'f';   break;
      case MC_CC|M_VOL:   chn->vol  = v;   re = 'a';   break;
      case MC_CC|M_PAN:   chn->pan  = v;   re = 'p';   break;
      case MC_CC|65:      chn->glide  = v;             break;
      case MC_CC|5:       chn->glRate = v;             break;
      case MC_CC|84:      chn->glFrom = v;             break;
      case MC_CC|19:      StrCp (chn->env, es);        break;
   }
   if (re)  AllVc (ch, re);
   if (_run)  _lok.Toss ();
}


void Syn::Dump (char x)                // sounds, samples, channels, voices
{ ubyte s;
  ubyt2 i, ns;
  TStr  ts;
  Sample *sm;                          // first melo, then drum sounds...
   if (x == 's') {
      for (s = 0;  s < _nSnd;  s++) {
         TRX("Snd=`d/`d:", s, _nSnd);       _snd [s]->Dump ();
         for (i = 0, ns = _snd [s]->_nSmp,
                     sm = _snd [s]->_smp;  i < ns;  i++, sm++)
            {TRX("   Smp=`d/`d:", i, ns);   sm->Dump (false);}
      }
      for (s = 0; s < 128; s++)  if (_drm [s]) {
         TRX("Drm=`s:", MDrm2Str (ts,s));   _drm [s]->Dump ();
         for (i = 0, ns = _drm [s]->_nSmp,
                     sm = _drm [s]->_smp;  i < ns;  i++, sm++)
            {TRX("   Smp=`d/`d:", i, ns);   sm->Dump (true);}
      }
   }
   for (ubyt2 v = 0;  v < _nVc;  v++)  {TRX("vc=`d",v);   _vc [v].Dump ();}
}
//______________________________________________________________________________
sbyt2 Syn::r2i (real r, real dth)
{  if (fabs (r) > _maxLvl) {
      _maxLvl = fabs (r);
TStr s; DBG("Syn maxLevel=`s", R2Str (_maxLvl,s));
   }
   r = r * 32766.0 / _maxLvl + dth;         // scale to sample width n dither
   if (r >= 0.)  return (sbyt2)(r+0.5);
                 return (sbyt2)(r-0.5);     // round to sbyt2 -32767..32767
}


void Syn::PutWav (sbyt2 *out, ubyt4 len)
// rendering direct to .wav 16bit 44.1KHz
{ ubyt4 ofs, sz = _nFr * sizeof (real);
  ubyt2 i;
  static ubyt4 _cur = 0;
   for (ofs = 0;  len--;  _cur++) {
      if (_cur == _nFr) {
TRX("mixLR wipe");
         MemSet (_mixL, 0, sz);   MemSet (_mixR, 0, sz);
         for (i = 0;  i < _nVc;  i++) {
TRX(" vc[`d].Mix",i);   _vc [i].Dump ();
            _vc [i].Mix ();
         }
         _cur = 0;
      }
      out [ofs++] = r2i (_mixL [_cur], Dither [0][_dth]);
      out [ofs++] = r2i (_mixR [_cur], Dither [1][_dth]);
TRX("   `d `d `d", ofs-2, out [ofs-2], out [ofs-1]);
      if (++_dth >= MAX_DITHER)  _dth = 0;
   }
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
      MemSet (_mixL, 0, sz);   MemSet (_mixR, 0, sz);
      _lok.Grab ();
      for (i = 0;  i < _nVc;  i++)  _vc [i].Mix ();
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


void Syn::LoadEnv ()
// load _stg,_env from .../device/syn/env.txt  (our envelope bank)
{ TStr  fn, s;
  ubyt4 en, st, d, i;
  char *ch, *p, *ds = CC("WHQEST612");   // whole, half, ... 64th, 128th, 256th
  StrArr t (CC("env"), 128, 128*sizeof(TStr));
   App.Path (fn, 'd');   StrAp (fn, CC("/device/syn/env.txt"));   t.Load (fn);
   en = st = 0;   _env.Ln = _stg.Ln = 0;
   for (i = 0;  i < t.num;  i++) {
      StrCp (s, t.str [i]);   if ((*s == '#') || (*s == '-'))  continue;

     ColSep ss (s, 4);
      if (*s != ' ') {
         if (_env.Full ())
{DBG("Syn::Load env.txt - env is full");   continue;}
         _env.Ins ();
         if (ss.Col [0][0] == '\0')
DBG("LoadEnv empty env name on # `d", en+1);
         StrCp (_env [en].nm,  ss.Col [0]);
         ch = & ss.Col [1][0];
         if      (! StrCm (ch, CC("oscNote")))  d = 0;
         else if (! StrCm (ch, CC("oscCent")))  d = 1;
         else if (! StrCm (ch, CC("fltCut")))   d = 2;
         else if (! StrCm (ch, CC("fltRes")))   d = 3;
         else if (! StrCm (ch, CC("amp" )))     d = 4;
         else if (! StrCm (ch, CC("pan" )))     d = 5;
         else
DBG("LoadEnv env=`s has bad dest=`s", ss.Col [0], ss.Col [1]);
         _env [en].dst = (ubyte)d;
         _env [en].stg = st;
//DBG("envcfg `d `s dst=`d stg=`d", en, ss.Col[0], d, st);
         en++;
      }
      else {
         if (_stg.Full ())
{DBG("Syn::Load env.txt - stg is full");   continue;}
         _stg.Ins ();
         d = 0;   _stg [st].add = 0.;  // default to end stage
         if (! StrCm (ss.Col [1], CC("loop")))
                  _stg [st].add = 1.;
         else
            for (ch = ss.Col [1];  *ch;  ch++) {
            // add up our dur into a songtime dur
               if (p = StrCh (ds, CHUP(*ch)))  d += (M_WHOLE >> (p-ds));
               if (*ch == '.')  d = d * 3 / 2;  // dotted
               if (*ch == '3')  d = d * 2 / 3;  // triplet
            }
         _stg [st].lvl = Str2R (ss.Col [0]);
         _stg [st].dur = d;
         if (! StrCm (ss.Col [2], CC("sin")))
               _stg [st].crv = -1.;
         else  _stg [st].crv = ss.Col [2][0] ? Str2R (ss.Col [2]) : 0.001;
//TStr s1;
//DBG("envstg `d lvl=`s dur=`d", st, R2Str (_stg [st].lvl,s1), d);
         st++;
      }
   }                                   // init dir
   for (i = 0;  i < st;  i++)  if (_stg [i].dur)
      _stg [i].dir = (_stg [i].lvl <= _stg [i+1].lvl) ? 1 : -1;
DBG("LoadEnv ne=`d ns=`d", _env.Ln, _stg.Ln);
}


void Syn::Init (char wav)
// open sound output device
// init samples, sounds, voices, buffers, and kick our soundcard writin thread
{  _run = false;                       // default to "no workie"
  TStr fn;
  File f;                              // MidiCfg picked sound descrip
  ulong ln;
   _trx = true;                        // just here no cfg file
TRX("Syn::Init bgn");
   if (_wav = wav) {                   // goin to .wav file - pretty easy
      *_snDsc = *_snDev = '\0';   _sn = nullptr;
      _nFr = 64;   _frq = 44100;   _vol = 1.0;
   }
   else {                              // goin live to sound card
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
      if (_sn->Dead ())  return;       // with _run false :(

      _nFr = _sn->_nFr;                // everbody needz theedz
      _frq = _sn->_frq;
DBG("   nFr=`d frq=`d", _nFr, _frq);
      _vol = 0.07;                     // alsa always does volume=100% :)
   }
   InitLookup ();   LoadEnv ();
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

   if (! _wav)  {_run = true;   start ();}
TRX("Syn::Init end");
}                                      // stereo sbyt2 (to double buffer)


void Syn::Quit ()
{
TRX("Syn::Quit bgn");
   if (_run)  {_run = false;   wait ();}
   WipeSnd ();
   if (_out)   delete [] _out;
   if (_intp)  delete [] _intp;   if (_mixL)  delete [] _mixL;
                                  if (_mixR)  delete [] _mixR;
   if (_sn)    delete _sn;
TRX("Syn::Quit end");
}

// syn.h - easy(ish) software synthesizer
//         .WAV files for instrument sounds

#ifndef SYN_H
#define SYN_H

#include "os.h"
#include "midi.h"
#include "snd.h"
#include "math.h"                      // no gettin round reals with synths

#define EVEN(n)     ((n)&0xFFFFFFFE)
#define EVEN_UP(n)  EVEN((n)+1)
#define MID14       ((ubyt2)(64 << 7))      // mid point of a midi 14 bit int
#define CLIP(_val,_min,_max) \
{(_val) = ((_val)<(_min))?(_min):(((_val)>(_max))?(_max):(_val));}


char *R2Str (real f, char *s);

// conversion tables
extern void InitLookup (void);
extern real Ct2Hz      (real cents);             // cents to hz
extern real Pan        (ubyte c, bool lft);      // 128 to pan, true=L/false=R
#define                MAX_INTERP  (256)
extern real Interp    [MAX_INTERP][7];
#define                MAX_DITHER  (48000)
extern real Dither [2][MAX_DITHER];    // per l/r channel


// sample interpolation position+fraction  ...just a uhuge w hilong=int,lo=frac
typedef ubyt8  Phase;
#define MAXU4  (4294967296.)           // max ubyt4 as real
#define LONG2PHASE(a)   (((Phase)(a))<<32)
#define REAL2PHASE(a)  ((((Phase)(a))<<32) | (Phase)(((a)-((ubyt4)(a)))*MAXU4))

// hi ubyt4=>index;  lo ubyt4/MAXULONG=>fract
#define PHASE_INDEX(x)  ((ubyt4)((x) >> 32))               // just int part

// phase to array pos in Interp[]
#define PHASE_FRACT(x)  ((((ubyt4)(x)) >> 24) & 0x00FF)    // MSB of frac part


struct ModCurv {
   ubyte relsVol [128];
   ubyt2 relsLen;

   ubyte rotoPan [252], rotoVol [252];      // Leslie rotating speaker fx
   ubyt2 rotoLen;

   void Init ()
   { ubyt2 i;
     ubyte p, v, d;

   // release curv
      relsLen = 128;
      for (i = 0; i < 128; i++)  relsVol [i] = 127-i;

   // roto (lame attempt at a Leslie rotating speaker fx)
      rotoLen = 252;
      p = 64;  v = 127;
      for (i =   0;  i <=  63;  i++) { // go left
         rotoPan [i] = p;   rotoVol [i] = v;
         p--;   if (! ( i      & 0x01)) v--;
      }
      p += 2;
      d = 0;
      for (i =  64;  i <= 189;  i++) { // round back to right
         rotoPan [i] = p;   rotoVol [i] = v;
         if (p == 64)  d = 1;
         p++;   if (! ((i- 64) & 0x01))  {if (d) v++;   else v--;}
      }
      p -= 2;
      for (i = 190;  i <= 251;  i++) { // right back to aaaalmost center
         rotoPan [i] = p;   rotoVol [i] = v;
         p--;   if (! ((i-190) & 0x01)) v++;
      }
//for (i = 0; i < rotoLen; i++)
//DBG("i=`d p=`d v=`d", i, rotoPan [i], rotoVol [i]);
//    [0..-63..0..63..1(loop)=>pan] => 64..1..64..127..65(loop)
//     64..1    64
//     2..64    63
//     65..127  63
//     126..65  62
//    [127..96..64..96..126(loop)=>vol] for WHOLE channel's
//     127..96  32
//     97..64   33
//     65..97
//     96..127
   }
};


// audio out specs
struct AuODef {ubyt2 bufLn;   ubyte bits;   bool flt;   real frq;
               real *smp;     ubyt4 nSmp, *tmpo;   ModCurv crv;};
extern AuODef  AuO;

#include "synSnd.h"                    // .WAV loading, etc
#include "synFx.h"                     // sample filtering, effects, etc


//------------------------------------------------------------------------------
struct Channel {
   ubyte id;                           // dev(0-7)*16+chn and 0x80|drum
   ubyte snd;
   ubyt2 pbnd, pbnr;
   ubyte hold, vol, pan,               // expr, bal later?
         vCut, cut, res, rPan, roto, chor, rvrb;
   ubyt2 rPos, rPoz;

   bool Drum ()  {return (((id & 0x80) == 0) && ((id % 16) == 9))
                                       ? true : false;}
   bool DrCh ()  {return   (id & 0x80) ? true : false;}

   void Rset ()
   {  pbnd = MID14;   pbnr = 2;   hold = 0;   vol = 127;   pan = 64;
      vCut = 0;   cut = 127;   rPan = roto = res = chor = 0;   rvrb = 6;
      rPos = rPoz = 0;
   }

   void Init (ubyte i)
   {  id = i;   snd = Drum () ? 128 : 0;   Rset ();  }

   void Dump ()
   { TStr ts;
DBG("   channel=`s: snd=`d pbnd=`d pbnr=`d hold=`d vol=`d pan=`d "
"vCut=`d cut=`d res=`d chor=`d rvrb=`d",
(id & 0x80) ? MDrm2Str (ts, id & 0x7F) : Int2Str (id, ts),
snd, pbnd, pbnr, hold, vol, pan, vCut, cut, res, chor, rvrb);
   }
};


//------------------------------------------------------------------------------
class Voice {
public:
   char     _on;                       // \0=free, d=down, s=sust
   ubyte    _key, _vel;
   ubyt4    _ntID;
   Channel *_chn;
   Sound   *_snd;
   Sample  *_smp;
   bool     _loopin, _looped;          // got loop?  hit it's end for 1st time?
   Phase    _phase,                    // pos w/in our samples
            _phInc;                    // amt we scoot per output sample
   real     _amp,  _ampInc,            // amp level
            _panL, _panR;              // pan pos
   ubyt4    _rels, _relsPoz, _relsPos;
   ubyt4    _puts;                     // #auo bufs since sample booted
   LPF      _flt;
   real    *_buf;                      // interpolated buf of output to mix

   ubyt2    _rpPoz;                    // voice fxy tricky stuffs
   bool     _rpDir;
   ubyte    _rpVal;

   Voice ()
   {  _buf = new real [AuO.bufLn];
      _on = _key = _vel = 0;
      _chn = nullptr;   _snd = nullptr;   _smp = nullptr;
   }

  ~Voice ()   {delete [] _buf;}

// status checkers
   bool On   ()   {return  _on              ? true : false;}  // on or sust
   bool Down ()   {return (_on == 'd')      ? true : false;}
   bool Sust ()   {return (_on == 's')      ? true : false;}
   bool Rels ()   {return (_chn == nullptr) ? true : false;}

   void Dump (char *pre)
   { TStr t, s1, s2;
      DBG("`s on=`c key=`d=`s vel=`d ntID=`d puts=`d loopin=`b looped=`b",
          pre, _on?_on:' ', _key, MKey2Str (t,_key), _vel, _ntID,
          _puts, _loopin, _looped);
      DBG("   phase=`u `u phInc=`u `u panL=`s panR=`s rels=`d relsPos=`d/`d",
          (ubyt4)(_phase>>32), (ubyt4)(_phase & 0xFFFFFFFF),
          (ubyt4)(_phInc>>32), (ubyt4)(_phInc & 0xFFFFFFFF),
          R2Str(_panL,s1), R2Str(_panR,s2), _rels, _relsPos, _relsPoz);
      if (_chn) {_chn->Dump ();   if (_snd)  _snd->Dump ();
                                  if (_smp)  _smp->Dump (_chn->DrCh ());}
   }

   void ReFrq ()
   // set frq - inc based on note frq versus root sample frq (drums not pitched)
   { real t;
      t = _smp->frq / AuO.frq;         // plus pbnd
      if (! _snd->_xFrq)
         t *= ( Ct2Hz ( _key * 100. +
                        (_chn->pbnr * 100. *
                         ((real)(_chn->pbnd - MID14) / (real)MID14) // -1..1
                        ) ) /
                Ct2Hz (_smp->key*100. - (sbyte)(_smp->cnt)) );
      _phInc = REAL2PHASE (t);
   }

   void ReFlt ()                       // set flt res,cut
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

   void ReAmp ()                       // set amp - based on vol cc n velo
   {  _amp  = (_chn->vol / 127.) * (_vel / 127.) *
              (AuO.crv.relsVol [_rels] / 127.);
   }

   void RePan ()                       // set pan - pan cc
   {  _panL = (_smp->lr == 1) ? 0. : Pan (_chn->pan, true);
      _panR = (_smp->lr == 0) ? 0. : Pan (_chn->pan, false);
   }

   void Redo (char re)       // reset voice params given diff in chan,etc
   {  if      (re == 'n')  ReFrq ();
      else if (re == 'f')  ReFlt ();
      else if (re == 'a')  ReAmp ();
      else if (re == 'p')  RePan ();
      else         /*'*'*/{ReFrq ();   ReFlt ();   ReAmp ();   RePan ();}
//Dump ("Redo");
   }

   void Bgn (Channel *chn, ubyte k, ubyte v, ubyt4 n, Sound *s, Sample *sm)
   // called by synth's NtOn per matching sample of chan's sound
   {  _chn = chn;   _key = k;   _vel = v;   _ntID = n;   _snd = s;   _smp = sm;
      _rels = _relsPos = _relsPoz = 0;   _puts = 0;
      _loopin = (_smp->lpBgn < _smp->len) ? true : false;   _looped = false;
      _phase = (Phase)0;   _ampInc = 0.;
      _flt.Init ();
      _rpPoz = 0;   _rpDir = false;   _rpVal = 64;
      Redo ('*');
      _on = 'd';
   }

   void Release ()                     // if chan hold, leave on till hold off
   {                                   // else beGIN release
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
      _relsPoz = (ubyt4)(AuO.frq * 60. / 16. / (real)(*AuO.tmpo)) /
                         AuO.crv.relsLen;
      if (! _relsPoz)  _relsPoz = 1;   // so say 44.1KHz,tmpo=200 => 6
      _relsPos = _relsPoz;             //                    120=>1378
   }

   void End ()                         // off it
   {
if (App.trc) Dump(CC("VC RELS END"));
      _on = '\0';   _chn = nullptr;   _snd = nullptr;
   }


   ubyt4 Interpolate ()
   // stretch l,r sample (per note frq vs. root frq) into l,r dsp bufs
   // hi byte of phase fraction tells how to balance our 7 samples at a time
   { ubyt4 d,             sBgn, sEnd, s = 0;
     real *o, *in, *co,   sb0, sb1, sb2,   se0, se1, se2;
     Phase ph;
      in = & AuO.smp [_smp->pos];   o = _buf;

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
         while (s == sBgn && d < AuO.bufLn) {    // 1st sample point
//DBG("         a: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
            co = Interp [PHASE_FRACT (ph)];
            o [d++] = co[0]*sb2     + co[1]*sb1     + co[2]*sb0     +
                      co[3]*in[s]   +
                      co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
            ph += _phInc;   s = PHASE_INDEX (ph);     // bump phase
         }
         sBgn++;

         while (s == sBgn && d < AuO.bufLn) {    // 2nd to 1st sample point
//DBG("         b: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
            co = Interp [PHASE_FRACT (ph)];
            o [d++] = co[0]*sb1     + co[1]*sb0     + co[2]*in[s-1] +
                      co[3]*in[s]   +
                      co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
            ph += _phInc;   s = PHASE_INDEX (ph);
         }
         sBgn++;

         while (s == sBgn && d < AuO.bufLn) {    // 3rd to 1st sample point
//DBG("         c: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
            co = Interp [PHASE_FRACT (ph)];
            o [d++] = co[0]*sb0     + co[1]*in[s-2] + co[2]*in[s-1] +
                      co[3]*in[s]   +
                      co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
            ph += _phInc;   s = PHASE_INDEX (ph);
         }
         sBgn -= 2;

         while (s <= sEnd && d < AuO.bufLn) {    // general case
//DBG("         d: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
            co = Interp [PHASE_FRACT (ph)];
            o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                      co[3]*in[s]   +
                      co[4]*in[s+1] + co[5]*in[s+2] + co[6]*in[s+3];
            ph += _phInc;   s = PHASE_INDEX (ph);
         }
         if (d >= AuO.bufLn)  break;   // break out if buffer filled

         sEnd++;
         while (s <= sEnd && d < AuO.bufLn) {    // 3rd to last point
//DBG("         e: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
            co = Interp [PHASE_FRACT (ph)];
            o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                      co[3]*in[s]   +
                      co[4]*in[s+1] + co[5]*in[s+2] + co[6]*se0;
            ph += _phInc;   s = PHASE_INDEX (ph);
         }

         sEnd++;
         while (s <= sEnd && d < AuO.bufLn) {    // 2nd to last point
//DBG("         f: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));
            co = Interp [PHASE_FRACT (ph)];
            o [d++] = co[0]*in[s-3] + co[1]*in[s-2] + co[2]*in[s-1] +
                      co[3]*in[s]   +
                      co[4]*in[s+1] + co[5]*se0     + co[6]*se1;
            ph += _phInc;   s = PHASE_INDEX (ph);
         }

         sEnd++;
         while (s <= sEnd && d < AuO.bufLn) {    // last point
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
         if (d >= AuO.bufLn)  break;
         sEnd -= 3;
      }
//DBG("         j: d=`d s=`d sBgn=`d sEnd=`d ph=`d `d",
//d, s, sBgn, sEnd, (ubyt4)(ph>>32), (ubyt4)(ph & 0xFFFFFFFF));

   // sub 1/2 sample from ph since we offset it and save back in _phase
      _phase = ph - (Phase)0x80000000;
      return d;
   }


   void Mix (real *mixL, real *mixR, real *chor, real *rvrb)    // da GUTS :)
   { ubyt4 i, len;
     ubyt2 rPoz, rPos;
      if (! On ())                       return;
      if (_snd == nullptr)    {End ();   return;}
      _puts++;
      len = Interpolate ();
//TStr ts, t2, t4, t5;
//DBG("pre L=`s R=`s  panL=`s panR=`s",
//R2Str(mixL [0],ts), R2Str(mixR [0],t2), R2Str(_panL,t4), R2Str(_panR,t5));
      rPoz = _chn->rPoz;   rPos = _chn->rPos;
      for (i = 0;  i < len;  i++)  {
        real s = _flt.Cvt (_buf [i]);            // filter it
         s *= _amp;                              // amp it
         mixL [i] += (s * _panL);                // pan,mix it
         mixR [i] += (s * _panR);
         if (_chn->chor)  chor [i] += (s * _chn->chor/127.);    // fx send
                          rvrb [i] += (s * _chn->rvrb/127.);
         if (_rels)  if (! _relsPos--) {
            if (_rels >= AuO.crv.relsLen)  {End ();   return;}
            _relsPos = _relsPoz;   ReAmp ();     // ^ kill yerself else bump
            _rels++;
         }
         if (_chn->roto)  if (! rPoz--) {
            rPoz = 3 * (128 - _chn->roto);       // 1..127=> 3*(127..1)
            if (++rPos >= AuO.crv.rotoLen)  rPos = 0;      // wrap
           ubyte p = AuO.crv.rotoPan [rPos];     // override ch's pan,vol
            _panL = (_smp->lr == 1) ? 0. : Pan (p, true);
            _panR = (_smp->lr == 0) ? 0. : Pan (p, false);
           ubyte v = AuO.crv.rotoVol [rPos];
            _amp  = (v / 127.) * (_vel / 127.);
         }
         if (_chn->rPan)  if (! _rpPoz--) {
            _rpPoz = 3 * (128 - _chn->rPan);
            if (_rpDir) { if (_rpVal != 127) _rpVal++;
                          else              {_rpVal--;   _rpDir = ! _rpDir;} }
            else        { if (_rpVal != 1)   _rpVal--;
                          else              {_rpVal++;   _rpDir = ! _rpDir;} }
            _panL = (_smp->lr == 1) ? 0. : Pan (_rpVal, true);
            _panR = (_smp->lr == 0) ? 0. : Pan (_rpVal, false);
            if (Rand () < (RAND_MAX/8))  _rpDir = ! _rpDir;
         }
      }
//DBG("post L=`s R=`s", R2Str (mixL [0],ts), R2Str (mixR [0],t2));
      if (len < AuO.bufLn)  End ();    // or if (nonloop) sample runs out
//for (i = 0; i < len; i++)
//DBG("`d mixL=`s R=`s", i, R2Str (mixL [i], ts), R2Str (mixR [i], t2));
   }
};


//------------------------------------------------------------------------------
typedef struct FxPDef {
   Chorus *chP;
   Reverb *rvP;
   ubyte   cNr,   cLevel, cSpeed, cDepth, cType,
           rRoom, rDamp,  rWidth, rLevel;

   void Updt ()
   {  chP->SetNr    (cNr*99 / 127);
      chP->SetLevel (cLevel / 127. * 10.);
      chP->SetSpeed (cSpeed / 127. * 4.71 + 0.29);
      chP->SetDepth (cDepth / 127. * 90300.);
      chP->SetType  ((cType >= 64) ? 1 : 0);  // tri : sin
      chP->Update ();
      rvP->SetRoom    (rRoom  / 127.);
      rvP->SetDamp    (rDamp  / 127.);
      rvP->SetWidth   (rWidth / 127.);
      rvP->SetLevel   (rLevel / 127.);
      rvP->Update ();
   }

   void Init (Chorus *ch, Reverb *rv)
   {  chP = ch;   rvP = rv;       // dflt range
      cNr    =   4;               // 3.   0-99    chorus params
      cLevel =  25;               // 2.   0.-10.
      cSpeed =   1;               // 0.3  0.29-5. Hz (.01 of .29 + [0 .. 4.71])
      cDepth =   1;               // 8.   0.-90300. Ms
      cType  =   0;               // 0    0=sin, 1=tri
/* #define initialroom   (.5)          // not usin freeverb's defaults
** #define initialdamp   (.2)
** #define initialwidth  (1.)
** #define initialwet    (1.)
*/
      rRoom  =  64;               // 0.2  0.-1.   reverb params:
      rDamp  =  25;               // 0.   0.-1.
      rWidth = 127;               // 0.5  0.-1.
      rLevel = 127;               // 0.9  0.-1.
/*    rRoom  =  25;               // 0.2  0.-1.   reverb params:
      rDamp  =   0;               // 0.   0.-1.
      rWidth =  64;               // 0.5  0.-1.
      rLevel = 114;               // 0.9  0.-1.
*/
      Updt ();
   }

   void Dump ()
   {  DBG("FxP: cNr=`d cLevel=`d cSpeed=`d cDepth=`d cType=`d "
               "rRoom=`d rDamp=`d rWidth=`d rLevel=`d",
          cNr, cLevel, cSpeed, cDepth, cType, rRoom, rDamp, rWidth, rLevel);
   }
} FxP;


//------------------------------------------------------------------------------
class Syn: public QObject {
   Q_OBJECT

public:
   Sound   *_snd [128];   ubyte _nSnd;      // melodic sounds (pitched)
   Sound   *_drm [128];                     // percussive ones (UNpitched)
   Channel  _chn [256];                     // midi chans: "canvases" for voices
   Voice   *_vc;          ubyt2 _nVc, _xVc; // each playin a note's samples
   real    *_chor, *_rvrb,                  // fx bufs
           *_mixL, *_mixR,                  // output buf of audio to GOooo
            _maxLvl;
   ubyt4    _ntID,  _dth,       _maxVc;     // note# n dither pos we're on
   FxP      _fxP;                           // params for fx (sorry bout namin)
   Chorus  *_chP;                           // fx processors
   Reverb  *_rvP;
   ubyt4    _cur;                           // hack fer writin .WAV :/
   bool     _quit;                          // we wanna shut down?

   Syn (ubyt4 *tmpo, ubyt2 bufLn = 64, ubyte bits = 16, bool flt = false,
        real ofrq = 44100., ubyt2 nvc = 256)
   // note - only ONE of me - no multiple instances...
   { ubyt2 c;
     TStr  s1;
      _quit = true;
DBG("{ Syn::Syn bufLn=`d bits=`d flt=`b ofrq=`s nvc=`d",
bufLn, bits, flt, R2Str (ofrq, s1), nvc);
      AuO.bufLn = bufLn;      AuO.bits = bits;      // ...globalize em :(
      AuO.flt   = flt;        AuO.frq  = ofrq;
      AuO.smp   = nullptr;    AuO.nSmp = 0;      AuO.tmpo = tmpo;
      AuO.crv.Init ();
      InitLookup ();
      MemSet (_snd, 0, sizeof (_snd));   _nSnd = 0;
      MemSet (_drm, 0, sizeof (_drm));
      for (c = 0;  c < 256;  c++)  _chn [c].Init ((ubyte)c);
      _vc = new Voice   [_xVc = nvc];    _nVc = 0;
      _ntID = _dth = 0;
      _mixL = new real [AuO.bufLn];   _mixR = new real [AuO.bufLn];
      _chor = new real [AuO.bufLn];   _rvrb = new real [AuO.bufLn];
      _chP  = new Chorus ();          _rvP  = new Reverb ();
      _fxP.Init (_chP, _rvP);
      _quit = false;
      _maxLvl = 1.0;   _maxVc = 0;   _cur = 0;
      start ();
DBG("} Syn::Syn");
   }

  ~Syn ()
   {  wait ();
      WipeSound ();                         delete    _rvP;    delete    _chP;
      delete [] _mixL;   delete [] _mixR;   delete [] _rvrb;   delete [] _chor;
      delete [] _vc;
   }


// -----------------------------------------------------------------------------
// sound bank management
   void DumpSnd ()
   { ubyte s;
     ubyt2 i;
     TStr  ds;
     Sample *sm;
   // first the sounds...
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

   void WipeSound ()
   { ubyt2 s;
      for (s = 0;  s <   128;  s++)  AllCh ((ubyte)s, 'e');     // 9s kill drums
      for (s = 0;  s < _nSnd;  s++)     {delete _snd [s];   _snd [s] = nullptr;}
      _nSnd = 0;
      for (s = 0;  s <   128;  s++)  if (_drm [s])
                                        {delete _drm [s];   _drm [s] = nullptr;}
      delete [] AuO.smp;   AuO.smp = nullptr;   AuO.nSmp = 0;
      _maxLvl = 1.0;   _maxVc = 0;   _cur = 0;
   }

   void LoadSound ()
   { TStr  fn, ts;
     File  f;
     char  lst [256*sizeof (TStr)], *pc;
     bool  melo = true;
     ubyt4 p, len, ld;
     ubyte s;
     ubyt8 sz;
     TStr  sSetM, sSetD, dSet;         // default sampsets n drumset
      { TStr s;   App.CfgGet (CC("tracesyn"), s);  // special-ish kinda UpdTrc()
         App.trc = (*s == 'y') ? true : false;
      }
TRC("{ Syn::LoadSound");
      WipeSound ();

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
               if ((pc = StrCh (lst, '\r'))) {
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
DBG("Syn::LoadSound  SoundBank.txt too big :(", fn);
         return;
      }
      lst [len] = '\0';
      if (! len)
         DBG("Syn::LoadSound: can't load device/syn/SoundBank.txt :(");
      for (p = 0;  p < len;) {
         p = NextLn (ts, lst, len, p);      // parse buf into seq of strs
TRC("p=`d/`d: `s", p, len, ts);
         if (! StrCm (ts, CC("Drum")))  {melo = false;   continue;}
         if (melo) {                   // drum marks when melodic sounds end
            if (_nSnd >= 128) {
DBG("Syn::LoadSound  tooo many sounds");
TRC("} Syn::LoadSound");
               return;
            }
TRC("pgm=`d snd=`s", _nSnd, ts);
            _snd [_nSnd++] = new Sound (ts, 128, sSetM, CC(""));
         }
         else {                        // loading drum sounds now
            ts [4] = '\0';
            s = MDrm (ts);
            if (s == 128) {
DBG("Syn::LoadSound - bad drum note=`s", ts);
TRC("} Syn::LoadSound");
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
DBG("Syn::LoadSound  tooo many samples (`d) :(",  sz);
         for (s = 0; s < _nSnd; s++)  DBG("   Snd `s siz=`d mxDat=`d",
                               _snd [s]->_nm, _snd [s]->_siz, _snd [s]->_mxDat);
         for (s = 0; s < 128; s++) if (_drm [s])
                                      DBG("   Drm `s siz=`d mxDat=`d",
                               _drm [s]->_nm, _drm [s]->_siz, _drm [s]->_mxDat);
         WipeSound ();
TRC("} Syn::LoadSound");
         return;
      }
TRC("alloc dat n smp");
      AuO.smp   = new real [sz];
      AuO.nSmp  = (ubyt4)sz;   len = 0;
     ubyte *dat = new ubyte [ld];
      if ((dat == nullptr) || (AuO.smp == nullptr)) {
DBG("Syn::LoadSound  Outa memory - `d samples => `d bytes :(",
AuO.nSmp, AuO.nSmp*8);
         for (s = 0; s < _nSnd; s++)  DBG("   Snd `s siz=`d mxDat=`d",
                               _snd [s]->_nm, _snd [s]->_siz, _snd [s]->_mxDat);
         for (s = 0; s < 128; s++) if (_drm [s])
                                      DBG("   Drm `s siz=`d mxDat=`d",
                               _drm [s]->_nm, _drm [s]->_siz, _drm [s]->_mxDat);
         delete [] dat;   WipeSound ();
TRC("} Syn::LoadSound");
         return;
      }
TRC("load snd n drm");
      for (s = 0;  s < _nSnd;  s++)     len = _snd [s]->LoadDat (dat, len);
      for (s = 0;  s <   128;  s++)  if (_drm [s])
                                        len = _drm [s]->LoadDat (dat, len);
TRC("free dat");
      delete [] dat;                   // just need it fer loadin
TRC("} Syn::LoadSound at end");
   }


// -----------------------------------------------------------------------------
// stuff used by notes...
   void NOff (ubyte ch, ubyte key, ubyte vel)
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


   void NtOn (ubyte ch, ubyte key, ubyte vel)
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


// -----------------------------------------------------------------------------
// stuff used by CCs
   void UnHold (ubyte ch)              // release all sust'd vcs on chn
   {
TRC(" UnHold ch=`d", ch);
      for (ubyt2 i = 0;  i < _nVc;  i++)
         if (_vc [i].Sust () && _vc [i]._chn && (_vc [i]._chn->id == ch))
            _vc [i].Release ();
   }

   void AllCh (ubyte ch, char todo) // mod all vcs on channel
   { ubyte dr = _chn [ch].Drum () ? 0x80 : 0;
TRC(" AllCh ch=`d `c/`s", ch, todo,
(todo=='i')?"init": ( (todo=='e')?"endNow": ((todo=='r')?"release":"redo") ) );
      if (todo == 'i') {               // reset all CCs is kinda different
         _chn [ch].Rset ();
         if (dr)  for (ch = 0;  ch < 128;  ch++)  _chn [0x80 | ch].Rset ();
         _fxP.Init (_chP, _rvP);
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


// -----------------------------------------------------------------------------
// =MY= main api...
   void Put (char *cmd, ubyte ch, ubyt2 c, ubyte v, ubyte v2)
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
      if ((v >= 128) && (c != MC_PROG))       // let prog thru
         {DBG("Syn::Put bad valu  ch=`d cc=`d valu=`d", ch, c, v);  return;}
      switch (c) {
      case MC_PROG:
         if (_chn [ch].Drum () || _chn [ch].DrCh () || (v > _nSnd))
               DBG("Syn::Put  Prog on drum chan or too high "
                   "ch=`d prog=`d nProg=`d",  ch, v, _nSnd);
         else  _chn [ch].snd = v;
         return;
      case MC_CC|M_ANOFF: re = 'r';   break;       // release em
      case MC_CC|M_ASOFF: re = 'e';   break;       // end em
      case MC_CC|M_ACOFF: re = 'i';   break;       // reset all CCs

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
      case MC_CC|21:      _chn [ch].rPan = v;             re = 'p';   break;
      case MC_CC|22:      _chn [ch].roto = v;             re = '*';   break;
/* reverb fixed n chorus off for now till i find algos that don't suck
**    case MC_CC|23:      _chn [ch].chor = v;             re = 'p';   break;
**    case MC_CC|24:      _chn [ch].rvrb = v;             re = 'p';   break;
**
** // not channel specific fx params
**    case MC_CC|25:      _fxP.cNr    = v;                re = 'x';   break;
**    case MC_CC|26:      _fxP.cLevel = v;                re = 'x';   break;
**    case MC_CC|27:      _fxP.cSpeed = v;                re = 'x';   break;
**    case MC_CC|28:      _fxP.cDepth = v;                re = 'x';   break;
**    case MC_CC|29:      _fxP.cType  = v;                re = 'x';   break;
**    case MC_CC|30:      _fxP.rRoom  = v;                re = 'x';   break;
**    case MC_CC|31:      _fxP.rDamp  = v;                re = 'x';   break;
**    case MC_CC|32:      _fxP.rWidth = v;                re = 'x';   break;
**    case MC_CC|33:      _fxP.rLevel = v;                re = 'x';   break;
*/
      default:            return;
      }
      if (re == 'x') _fxP.Updt ();   else AllCh (ch, re);
   }

   void DumpChn ()  {for (ubyt2 c = 0;  c < 256;  c++)  _chn [c].Dump ();}

   void DumpVc ()
   { TStr ts;
      for (int i = 0;  i < _nVc;  i++)
         _vc [i].Dump (StrFmt (ts, "  vc=`d/`d", i, _nVc));
   }

   void Dump ()  {DumpSnd ();   DumpVc ();   DumpChn ();}//   _fxP.Dump ();}


// -----------------------------------------------------------------------------
   sbyt2 r2i (real r, real dth)
   { TStr ts;
      if (fabs (r) > _maxLvl) {        // keep track of max level EVER
         _maxLvl = fabs (r);
DBG("Syn: maxLevel=>`s", R2Str (_maxLvl, ts));
      }
      r = r * 32766.0 / _maxLvl + dth;      // scale to sample width n dither
      if (r >= 0.)  return (sbyt2)(r+0.5);  // round to sbyt2 -32767..32767
                    return (sbyt2)(r-0.5);
   }

   float r2f (real r, real dth)
   { TStr ts;
      if (fabs (r) > _maxLvl) {        // keep track of max level EVER
         _maxLvl = fabs (r);
DBG("Syn: maxLevel=>`s", R2Str (_maxLvl, ts));
      }
      r = r / _maxLvl + dth;
      return (float)r;
   }

   void run ()  override
// void PutAuO (void *out)
   // live rendering...  send 16 bit output
   { ubyt4 o, len = AuO.bufLn,  sz = len * sizeof (real);
     ubyt2 i;
     sbyt2 *o16 = (sbyt2 *)out;
     float *ofl = (float *)out;
      MemSet (_mixL, 0, sz);   MemSet (_mixR, 0, sz);
      MemSet (_chor, 0, sz);   MemSet (_rvrb, 0, sz);
      for (i = 0;  i < _nVc;  i++)  _vc [i].Mix (_mixL, _mixR, _chor, _rvrb);
      _chP->Mix (_chor, _mixL, _mixR);
      _rvP->Mix (_rvrb, _mixL, _mixR);
   // upd ch rPoz,rPos for next buf
      for (i = 0;  i < 128;  i++)  if (_chn [i].roto)
         for (o = 0;  o < AuO.bufLn;  o++) {
            if (_chn [i].rPoz == 0) {       // 1..127=>3*(127..1)
               _chn [i].rPoz = 3 * (128 - _chn [i].roto);
               if (++_chn [i].rPos >= AuO.crv.rotoLen)  _chn [i].rPos = 0;
            }
            else _chn [i].rPoz--;
         }
      for (o = 0;  o < len;  o++) {
         if (AuO.flt) {
            *ofl++ = r2f (_mixL [o], Dither [0][_dth]);
            *ofl++ = r2f (_mixR [o], Dither [1][_dth]);
         }
         else {
            if (AuO.bits > 16)  *o16++ = 0;
                                *o16++ = r2i (_mixL [o], Dither [0][_dth]);
            if (AuO.bits > 16)  *o16++ = 0;
                                *o16++ = r2i (_mixR [o], Dither [1][_dth]);
         }
         if (++_dth >= MAX_DITHER)  _dth = 0;
      }
   }
/*
   void PutWav (ubyt4 len, sbyt2 *out)
   // rendering direct to .WAV (16bit 44.1KHz)
   { ubyt4 o, ofs, sz = AuO.bufLn * sizeof (real);
     ubyt2 i;
      if (_quit)  return;
      for (ofs = 0;  len--;  _cur++) {
         if (_cur == AuO.bufLn) {
            MemSet (_mixL, 0, sz);   MemSet (_mixR, 0, sz);
            MemSet (_rvrb, 0, sz);   MemSet (_chor, 0, sz);
            for (i = 0; i < _nVc; i++) _vc [i].Mix (_mixL, _mixR, _chor, _rvrb);
            _chP->Mix (_chor, _mixL, _mixR);
            _rvP->Mix (_rvrb, _mixL, _mixR);
            for (i = 0;  i < 128;  i++)  if (_chn [i].roto)
               for (o = 0; o < AuO.bufLn; o++) {
                  if (_chn [i].rPoz == 0) {
                     _chn [i].rPoz = 3 * (128 - _chn [i].roto);
                     if (++_chn [i].rPos >= AuO.crv.rotoLen)  _chn [i].rPos = 0;
                  }
                  else _chn [i].rPoz--;
               }
            _cur = 0;
         }
         out [ofs++] = r2i (_mixL [_cur], Dither [0][_dth]);
         out [ofs++] = r2i (_mixR [_cur], Dither [1][_dth]);
         if (++_dth >= MAX_DITHER)  _dth = 0;
      }
   }
*/
};

#endif

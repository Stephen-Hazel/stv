// syn.h - easy(ish) software synthesizer
//         .WAV files for instrument sounds

#ifndef SYN_H
#define SYN_H

#include "os.h"
#include "midi.h"
#include "snd.h"
#include "wav.h"
#include "math.h"                      // no gettin round reals with synths

#define MID14  ((ubyt2)(64 << 7))      // mid point of a midi 14 bit int

char *R2Str (real f, char *s);

// conversion tables
extern void InitLookup (void);
extern real Ct2Hz      (real cents);             // cents to hz
extern real Pan        (ubyte c, ubyte lr);      // 128 to pan, 0=L / 1=R
#define                MAX_INTERP  (256)
extern real Interp    [MAX_INTERP][7];
#define                MAX_DITHER  (48000)
extern real Dither [2][MAX_DITHER];    // per l/r channel

extern SndO Sn;                        // sound output device picked by MidiCfg

// sample interpolation position+fraction  ...just a uhuge w hilong=int,lo=frac
typedef ubyt8  Phase;
#define MAXU4  (4294967296.)           // max ubyt4 as real
#define LONG2PHASE(a)   (((Phase)(a))<<32)
#define REAL2PHASE(a)  ((((Phase)(a))<<32) | (Phase)(((a)-((ubyt4)(a)))*MAXU4))

// hi ubyt4=>index;  lo ubyt4/MAXULONG=>fract
#define PHASE_INDEX(x)  ((ubyt4)((x) >> 32))               // just int part

// phase to array pos in Interp[]
#define PHASE_FRACT(x)  ((((ubyt4)(x)) >> 24) & 0x00FF)    // MSB of frac part


//______________________________________________________________________________
// sound (.WAV) part of syn
#define MAX_SAMP (88*20*2)             // max #stereo WAVs per sound
                                       // all piano keys - 20 velo grps
class Sample {                         // stereo .WAVs get split to 2 samples
public:
   TStr  fn;                 // wav fn (_k,_v prefix n _l/_r suffix)
   ubyt4 pos, lpBgn, len;    // where it's at in Syn._smp[] in samples
   real  frq;                // sampled frq matching key.cnt
   ubyte key, cnt,           // root note matchin frq for repitching
         bytes, flopt,       // file format - 24 bit=>3, etc;  0=int,1=flo pt
         chans, lr,          // ;  0=left, 1=right, 2=mono(played on both sides)
         mnVel, mxVel,       // midi key,vel ranges for pickin smp
         mnKey, mxKey;
   void Dump (bool dr);
};

class Sound {                          // a dir of stereo or mono .WAV files
public:
   TStr  _nm,                //                     Piano_AcousticGrand_Bank
         _pa;                // .../device/syn/Bank/Piano_AcousticGrand
   ubyt4 _siz;               // #samples of all WAVs
   real  _max;               // max sample range of all WAVs
   bool  _xFrq, _xRls;       // unpitched;  no release on ntUp (can't be looped)
   Sample *_smp;             // alloc'd
   ubyt2  _nSmp;

   Sound (char *snd, ubyte dKey = 128);
  ~Sound ();
   void  Dump ();
   bool  LoadFmt (char *wfn, ubyte ky, ubyte vl);
   ubyt4 LoadDat (ubyt4 pos);
};


//______________________________________________________________________________
// sample filtering, effects, etc fer syn

#define CLIP(_val,_min,_max) \
{(_val) = ((_val)<(_min))?(_min):(((_val)>(_max))?(_max):(_val));}

class LPF {                            // low pass filter
   bool  init;
   ubyt4 inc;
   real  cut,  res,                    // cutoff frequency, resonance params
        pCut, pRes, gain,              // prev cut;  gain derived from res
         a1,    a2,    b1,    b2,
         a1Inc, a2Inc, b1Inc, b2Inc,
         hist1, hist2;                 // buffer past calcs

public:
   void Res (real r);
   void Cut (real c);
   void Init ();
   real Cvt (real smp);
};


#define DC_OFFSET         (1e-8)       // initial value for all effects' bufs
#define ALLPASS_FEEDBACK  (0.5)        // allpass has fixed feedback

class Allpass {                        // allpass and comb are simpler filters
public:                                // used by reverb to do it's thing
   real *buf;
   sbyt4 len, pos;

   Allpass (sbyt4 l)
   {  buf = new real [len = l];   pos = 0;
      while (l)  buf [--l] = DC_OFFSET;
   }

  ~Allpass ()  {delete [] buf;}

   inline void mix (real *in)      // allpass's feedback is fixed
   { real bufout, out;
      bufout = buf [pos];   out = bufout - *in;
      buf [pos] = *in + (bufout * ALLPASS_FEEDBACK);
      if (++pos >= len)  pos = 0;
      *in = out;
   }
};

class Comb {
public:
   real *buf, feedback, damp1, damp2, store;
   sbyt4 len, pos;

   Comb (sbyt4 l)
   {  buf = new real [len = l];   pos = 0;   store = 0;
      while (l)  buf [--l] = DC_OFFSET;
   }

  ~Comb ()  {delete [] buf;}

   void setfeedback (real val)  {feedback = val;}
   void setdamp     (real val)  {damp1 = val;   damp2 = 1 - val;}

   inline void mix (real input, real *out)
   { real tmp = buf [pos];
      store = (tmp * damp2) + (store * damp1);
      buf [pos] = input + (store * feedback);
      if (++pos >= len)  pos = 0;
      *out += tmp;
   }
};

#define NUM_COMB     (8)               // # comb    filters used by reverb
#define NUM_ALLP     (4)               // # allpass filters used by reverb
#define REVERB_GAIN  (0.015)           // reverb has fixed gain
#define STEREO_SPR   (23)              // stereo spread

#define SCALE_WET    (3.)              // turning params into values to use
#define SCALE_DAMP   (1.)
#define SCALE_ROOM   (0.28)
#define OFFSET_ROOM  (0.7)

// These assume 44.1KHz sample rate so adjust em
extern const ubyt4 LEN_COMB [NUM_COMB];
extern const ubyt4 LEN_ALLP [NUM_ALLP];

class Reverb {
public:
   Comb    *combL [NUM_COMB], *combR [NUM_COMB];
   Allpass *allpL [NUM_ALLP], *allpR [NUM_ALLP];
   real     room, damp, wet, width,  wet1, wet2;

   void SetRoom  (real val)  {room  = (val * SCALE_ROOM) + OFFSET_ROOM;}
   void SetDamp  (real val)  {damp  =  val * SCALE_DAMP;}
   void SetWidth (real val)  {width =  val;}
   void SetLevel (real val)  {CLIP (val, 0., 1.);   wet = val * SCALE_WET;}
   void Update ();

   void Set (ubyte r = 64, ubyte d = 25, ubyte w = 127, ubyte l = 127)
   {  SetRoom (r / 127.);   SetDamp  (d / 127.);   SetWidth (w / 127.);
                            SetLevel (l / 127.);   Update ();
   }

   void Mix (real *in, real *mixL, real *mixR);

   Reverb ();
  ~Reverb ();
};


//______________________________________________________________________________
struct Channel {
   ubyte id;                           // midi channel #
   ubyte snd;
   ubyt2 pbnd, pbnr;
   ubyte hold, vol, pan, vCut, cut, res, rvrb;

   bool Drum ()  {return (id == 9)   ? true : false;}
   bool DrCh ()  {return (id & 0x80) ? true : false;}

   void Rset ()
   {  pbnd = MID14;   pbnr = 2;   hold = 0;   vol = 127;   pan = 64;
      vCut = 0;   cut = 127;   res = 0;   rvrb = 6;
   }

   void Init (ubyte i)
   {  id = i;   snd = Drum () ? 128 : 0;   Rset ();  }

   void Dump ()
   { TStr ts;
DBG("   channel=`s: snd=`d pbnd=`d pbnr=`d hold=`d vol=`d pan=`d "
"vCut=`d cut=`d res=`d rvrb=`d",
(id & 0x80) ? MDrm2Str (ts, id & 0x7F) : Int2Str (id, ts),
snd, pbnd, pbnr, hold, vol, pan, vCut, cut, res, rvrb);
   }
};


//______________________________________________________________________________
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
   ubyt4    _rels, _relsLen;           // how release goes upon NtUp
   ubyt4    _puts;                     // # periods since sample booted
   LPF      _flt;                      // lowpass filter for everyone
   real    *_buf;                      // interpolated buf of output to mix

   Voice ();
  ~Voice ();

// status checkers
   bool  On   ()   {return  _on              ? true : false;}   // down or sust
   bool  Down ()   {return (_on == 'd')      ? true : false;}
   bool  Sust ()   {return (_on == 's')      ? true : false;}
   bool  Rels ()   {return (_chn == nullptr) ? true : false;}

   void  Dump  (char *pre);
   void  ReFrq ();
   void  ReFlt ();
   void  ReAmp ();
   void  RePan ();
   void  Redo  (char re);
   void  Bgn   (Channel *chn, ubyte k, ubyte v, ubyt4 n, Sound *s, Sample *sm);
   void  Release ();
   void  End   ();
   ubyt4 Interpolate ();
   void  Mix (real *mixL, real *mixR, real *rvrb);
};


//______________________________________________________________________________
class Syn: public QThread {
   Q_OBJECT

public:
   real    *_smp;         ubyt4 _nSmp;      // buf o samples from inst wavs
   Sound   *_snd [128];   ubyte _nSnd;      // melodic sounds (pitched)
   Sound   *_drm [128];                     // percussive   (UNpitched)
   Reverb   _rvP;                           // reverb fx processor
   Channel  _chn [256];                     // midi chans: "canvases" for voices
   Voice   *_vc;                            // voice per (mono) sample in use
   ubyt2   _nVc, _xVc, _maxVc;              // #used, #max, max we ever used
   real    *_rvrb,                          // fx buf
           *_mixL, *_mixR,                  // output buf of audio to GOooo
            _maxLvl;                        // max level we ever did
   ubyt4    _ntID, _dth;                    // note# n dither pos we're on
   bool     _run;                           // spin our sample writin thread?
   sbyt2  (*_out)[2];                       // sound device sample buffer

   Syn ();
  ~Syn ();
   void WipeSnd ();
   void LoadSnd ();

   void NOff (ubyte ch, ubyte key, ubyte vel);
   void NtOn (ubyte ch, ubyte key, ubyte vel);

   void UnHold (ubyte ch);
   void AllCh  (ubyte ch, char todo);

   void Put (ubyte ch = 0, ubyt2 c = 0, ubyte v = 0, ubyte v2 = 0);
   void DumpSnd ();
   void DumpChn ();
   void DumpVc  ();
   void Dump    ();

   sbyt2 r2i (real r, real dth);
   void  run ()  override;
};
extern Syn Sy;                         // our top

#endif

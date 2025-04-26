// syn.h - easy(ish) software synthesizer
//         .WAV files for instrument sounds

#ifndef SYN_H
#define SYN_H

#include "os.h"
#include "midi.h"
#include "snd.h"
#include "wav.h"
#include "math.h"                      // no gettin around reals with synths

#define TRX(...)  if(Sy._trx)DBG(__VA_ARGS__)    // syn local TRC

typedef ubyt8 Phase;                   // hi 4 bytes is int pos
                                       // lo 4 bytes is fract
const ubyt2 MID14 = 64 << 7;           // mid point of a midi 14 bit int

struct Channel {
   ubyte hold, snd;
   ubyt2 pBnd, pBnR;
   ubyte fCut, fRes, vCut, vol, pan,
         glide, glRate, glFrom,        // prev note, portam on/off, rate, fr key
         nNt, pNt;                     // nt,tm shift reg to set pNt right
   ubyt4 nTm;
   TStr  env;
   void Init ();
   void Dump ();
};

struct Sample {                        // stereo .WAVs get split to L,R samples
public:
   TStr  fn;                           // _k,_v prefix n _l/_r suffix
   ubyt4 pos, lpBgn, len,  frq;        // lpBgn==len means no loop
   ubyte mnKey, mxKey, mnVel, mxVel,   // ranges for midi key,vel to pick sample
         lr, key, cnt;                 // 0=left 1=right 2=mono, pitch center
   void Dump (bool dr);
};

class Sound {                          // a dir of.WAV files - stereo or mono
public:
   TStr  _nm,                //                     Piano_AcousticGrand_Bank
         _pa;                // .../device/syn/Bank/Piano_AcousticGrand
   bool  _xFrq, _xRls;       // unpitched;  no release on ntUp (can't be looped)
   ubyt4 _siz;               // #samples of all WAVs
   Sample *_smp;             // alloc'd
   ubyt2  _nSmp;

   Sound (char *snd, ubyte dKey = 128);
  ~Sound ();
   bool  LoadFmt (char *wfn, ubyte ky, ubyte vl);
   ubyt4 LoadDat (ubyt4 pos);
   void  Dump ();
};
//______________________________________________________________________________
class LPF {                            // low pass filter (per voice)
   bool  init;
   ubyt4 inc;
   real  cut,  res,                    // cutoff frequency, resonance params
        pCut, pRes, gain,              // prev cut;  gain derived from res
         a1,    a2,    b1,    b2,
         a1Inc, a2Inc, b1Inc, b2Inc,
         hist1, hist2;                 // buffer past calcs

public:
   void Init ();
   void Cut (real c);
   void Res (real r);
   real Mix (real smp);
};
//______________________________________________________________________________
struct Glide {                         // cool modulation on freq
   real ofs, inc;
   void Init (ubyte cid, ubyte key);
   char Mix ();
};
//______________________________________________________________________________
struct EnvStg {ubyt4 dur;   real lvl, crv,   mul, add;};
                   // dur  songtime dur;  0 for last stage (loop stored in add)
                   // lvl  starting level (ending at next stg's .lvl)
                   // crv  (curve) .0001 mostly exponential .. 100 mostly linear
                   // mul, add  calc'd in Init
struct EnvCfg {WStr nm;  ubyte dst;  ubyt2 stg;};

class Env {
public:
   ubyte   dst;
   EnvStg *stg;
   ubyte   s;                          // which stg # we're on
   sbyte   dir;                        // dir from initial to target lvl(-/+1)
   real    lvl;                        // output level
   void Init (ubyte id);
   real Mix ();
   bool End ()  {return stg [s].dur == 0;}
   void SetStg (sbyte st2)  {s = st2;}
};
//______________________________________________________________________________
class Voice {
public:                                // Core stuph:
   char     _on;                       // \0=free, d=down, h=hold(NOff but sust)
                                       //          r=release(amp curve to 0)
   ubyte    _ch;                       // channel num n ptr
   Channel *_c;
   ubyte    _key, _vel;                // key and velocity of note on
   ubyt4    _vcNo, _nPer;              // voice # I am, periods since Bgn

   Sound   *_snd;                      // Oscillator:
   Sample  *_smp;                      // which wav of instrument wav set
   bool     _looped;                   // hit end of rep'ing loop for 1st time?
   Phase    _phase,                    // pos w/in our sample
            _phInc;                    // amt we scoot pos per output sample

   LPF      _flt;                      // Filter: lowpass one for eeevery voice

   real     _amp, _panL, _panR;        // Amp n Pan

   TStr        _eStr;                  // Modulation - envelopes string
   Arr<Env,16> _env;
   real        _eVal [6];              // oNt oCnt fCut fRes amp pan
   Glide       _gl;                    // doin glide? (portamento) pitch offset

   void  Init ();
   void  Bgn  (ubyte ch, ubyte k, ubyte v, ubyt4 n, Sound *s, Sample *sm,
               char *es);
   void  Rels ();
   void  End  ();
   void  ReFrq (), ReFlt (), ReAmp (), RePan (), Re (char todo);
   void  Dump  (char q = '\0');

   ubyt4 Osc ();                       // tough part (sample=>interpolation buf)
   void  Mix ();                       // guts - cook up next sound card buf
};
//______________________________________________________________________________
struct SInfo  {ubyt4 time;   ubyt2 tmpo;   ubyte bt, sb;}; // song info fer syn


class Syn: public QThread {
   Q_OBJECT

   void run ()  override;

public:
   char    _wav;                       // to .wav?  else live to sound card
   SndO   *_sn;                        // sound device we're writin ta
   TStr    _snDsc, _snDev;
   ubyt4   _frq, _nFr;                 // sound params everybody needz
   real    _vol;                       // global volume (not midi controlled)
   real   *_smp;         ubyt4 _nSmp;  // buf o samples for all sounds' wavs
   Sound  *_snd [128];   ubyte _nSnd;  // melodic sounds (pitched)
   Sound  *_drm [128];                 // percussive   (UNpitched)
   Channel _chn [128];                 // midi chans: "canvases" for voices
   Arr<EnvStg,128> _stg;               // envelope bank with arr of stages per
   Arr<EnvCfg,64>  _env;
   Voice   _vc  [256];   ubyt2 _nVc;   // voice per (mono) sample in use
   ubyte   _maxChn;                    // max channel sequencer uses
   ubyt2   _maxVc;                     // #used, max we ever used
   real    _maxLvl;                    // max level we ever did
   ubyt4   _dth, _vcNo;                // dither pos, count of voices so far
   real   *_intp, *_mixL, *_mixR;      // bufs: interpolation, mix left n right
   sbyt2 (*_out)[2];                   // sound device's sample double buffer

   bool    _run;                       // spinnin our sample writin thread?
   ThLock  _lok;                       // lock - so i don't step on my thread
   bool    _trx;                       // syn specific TRX like TRC
   SInfo   _in;

   void  Init (char wav = '\0'),  Quit ();
   bool  Dead ()  {return ! _run;}
   void  PutWav (sbyt2 *out, ubyt4 len);

   void  LoadEnv ();
   void  WipeSnd ();
   void  LoadSnd (TStr *snd, ubyte maxch);

   void  NOff (ubyte ch, ubyte key, ubyte vel);
   void  NtOn (ubyte ch, ubyte key, ubyte vel, char *es);

   void  UnHold (ubyte ch);
   void  AllVc  (ubyte ch, char todo);

   sbyt2 r2i (real r, real dth);
   void  Put (ubyte ch = 0, ubyt2 c = 0, ubyte v = 0, ubyte v2 = 0,
              char *es = nullptr);
   void  Tell (SInfo *in)  {MemCp (& _in, in, sizeof (_in));}

   void  Dump (char x = '\0');
};
extern Syn Sy;                         // that's me

#endif

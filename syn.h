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

struct Channel {                       // not much to a channel :)
   ubyte snd,  hold, vol, pan, res, rvrb;
   ubyt2       pbnd, pbnr;
   void Init ();
   void Dump ();
};

struct Sample {                         // stereo .WAVs get split to 2 samples
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
   void  Dump ();
   bool  LoadFmt (char *wfn, ubyte ky, ubyte vl);
   ubyt4 LoadDat (ubyt4 pos);
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
class Voice {
public:
   char     _on;                       // \0=free, d=down, s=sust(NOff but hold)
   ubyte    _key, _vel;
   ubyte    _ch;
   Sound   *_snd;
   Sample  *_smp;
   bool     _looped,                   // hit loop's end for 1st time?
            _rel;                      // in note release?
   Phase    _phase,                    // pos w/in our sample
            _phInc;                    // amt we scoot per output sample
   real     _amp, _panL, _panR;
   ubyt4    _vcNo, _nPer;              // voice # I am, pers since Bgn
   LPF      _flt;                      // lowpass filter for eeevery voice
   real     _rMul, _rInc;              // release ampenv stage (only)

   void  Init ();
   bool  On   ()   {return  _on         ? true : false;}   // down or sust
   bool  Down ()   {return (_on == 'd') ? true : false;}
   bool  Sust ()   {return (_on == 's') ? true : false;}
   bool  Rels ()   {return _rel;}

   void  Bgn     (ubyte ch, ubyte k, ubyte v, ubyt4 n, Sound *s, Sample *sm);
   void  Release ();
   void  End     ();
   void  ReFrq (), ReFlt (), ReAmp (), RePan (), Redo (char re);
   void  Dump  (char q = '\0');

   ubyt4 Interpolate (real *ib);       // tough part (sample=>interpolation buf)
   void  Mix ();                       // da guts
};
//______________________________________________________________________________
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
   Voice   _vc  [256];   ubyt2 _nVc;   // voice per (mono) sample in use
   ubyte   _maxChn;                    // max channel sequencer uses
   ubyt2   _maxVc;                     // #used, max we ever used
   real    _maxLvl;                    // max level we ever did
   ubyt4   _dth, _vcNo;                // dither pos, count of voices so far
   real   *_intp, *_mixL, *_mixR;      // bufs: interpolation, mix left n right
   sbyt2 (*_out)[2];                   // sound device's sample double buffer

   bool    _run;                       // spinnin our sample writin thread?
   ThLock  _lok;                       // lock - so i don't step on my thread

   void  Init (char wav = '\0'),  Quit ();
   bool  Dead ()  {return ! _run;}
   void  PutWav (sbyt2 *out, ubyt4 len);

   void  WipeSnd ();
   void  LoadSnd (TStr *snd, ubyte maxch);

   void  NOff (ubyte ch, ubyte key, ubyte vel);
   void  NtOn (ubyte ch, ubyte key, ubyte vel);

   void  UnHold (ubyte ch);
   void  AllVc  (ubyte ch, char todo);

   sbyt2 r2i (real r, real dth);
   void  Put (ubyte ch = 0, ubyt2 c = 0, ubyte v = 0, ubyte v2 = 0);

   void  Dump (char x = '\0');
   bool  _trx;
};
extern Syn Sy;                         // that's me

#endif

// wav.h - load/save a .wav file.  pretty painful sigh.

#ifndef WAV_H
#define WAV_H

#include "os.h"

#define EVEN(n)     ((n)&0xFFFFFFFE)
#define EVEN_UP(n)  EVEN((n)+1)
                                       // does linux have a .h for this junk ??
const sbyt2 WAVE_FORMAT_PCM        = 0x0001;
const sbyt2 WAVE_FORMAT_IEEE_FLOAT = 0x0003;
const sbyt2 WAVE_FORMAT_EXTENSIBLE = 0xFFFE;

struct WAVEFORMATEX {
   sbyt2 wFormatTag;
   sbyt2 nChannels;
   sbyt4 nSamplesPerSec;
   sbyt4 nAvgBytesPerSec;
   sbyt2 nBlockAlign;
   sbyt2 wBitsPerSample;
   sbyt2 cbSize;
};

struct GUID {                          // so dumbb
   ubyt4 Data1;
   ubyt2 Data2;
   ubyt2 Data3;
   ubyte Data4 [8];
};

extern GUID KSDATAFORMAT_SUBTYPE_PCM;
extern GUID KSDATAFORMAT_SUBTYPE_IEEE_FLOAT;

struct WAVEFORMATEXTENSIBLE {
   WAVEFORMATEX Format;
   union {
      sbyt2 wValidBitsPerSample;
      sbyt2 wSamplesPerBlock;
      sbyt2 wReserved;
   } Samples;
   sbyt4    dwChannelMask;
   GUID     SubFormat;                 // ms is duuumb
};

typedef char ID [4];

struct CHUNK {
   ID    tag;
   ubyt4 siz;
};

#define SamplerID  'smpl'
                                       // only key,cnt,bgn,end used !
struct WAVESMPL {                      // i'm keepin chunkID,chunkSize outa it
   ubyt4 manuf, prod,                  // junk - 0
         per,                          // dumb - just another nSamplePerSec
         key, cnt,                     // sampled key, cent offset (but weird)
                                       //           ^      ^ aaactually used
         sfmt, sofs,                   // smpte junk - 0
         num,                          // num loops i guess? - always 1 fer me
         dat,                          // dunno? - 0
// this is actually Loops[] but always 1 fer me
         cue, loop,                    // loopid (junk), looping or not?
         bgn, end,                     // loopBgn, loopEnd - aaactually used
         frc, times;                   // fraction??  playcount?? - 0
};
/*
struct SampleLoop {                    // ...nawww gonna just use WAVESMPL
   sbyt4 dwIdentifier, dwType, dwStart, dwEnd, dwFraction, dwPlayCount;
};

struct SamplerChunk {                  // (see above)
   ID    chunkID;
   sbyt4 chunkSize;
   sbyt4 dwManufacturer,  dwProduct,
         dwSamplePeriod,
         dwMIDIUnityNote, dwMIDIPitchFraction,
         dwSMPTEFormat,   dwSMPTEOffset,
         cSampleLoops,    cbSamplerData;
   SampleLoop Loops [];
};
*/

// Load populates
//    _name         path/fn
//    _frq          samples/sec from 'fmt ' chunk
//    _mem, _len    samples ('data' chunk) _len is in samples !
//    _bits, _byts  bits/sample bytes/sample
//
// then update _frq,   _key, _cnt,
//      _bgn, _end, _loop, _lBgn, _lEnd
// ta Save which will ALWAYS write a 'smpl' chunk
//    BUTT  _mono,_real,_bits,_byts,_len can't be updated !!

class Wav {
public:
   Wav ();
  ~Wav ();
   void  Wipe ();
   char *Load (char *fn);
   void  Save (char *fn);
   TStr  _name;                        // path/filename of what we loaded
   ubyt4 _frq;                         // frequency
   void *_mem;                         // wav data chunk - of whatever kinda
   ubyt4 _len;                         // in SAMPLES - bytes/_byts/2*!_mono
   ubyte _bits,                        // bits/sample -         8,16,24,32,etc
         _byts;                        // bytes/sample - used.  1,2,3,4,8,etc
   bool  _mono, _real, _loop;          // mono/stereo, float/int, got loop
   ubyte _key, _cnt;                   // sampled key n cent tuning
   ubyt4 _bgn, _end, _lBgn, _lEnd;     // bgn/end, loop bgn/end
private:
   MemFile              _mf;           // don't be messin w deez
   WAVEFORMATEXTENSIBLE _fmt;
   ubyt4                _fmtSz;
   WAVESMPL             _smp;
};

#endif

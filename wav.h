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
// this is a TRICKY struct !!  be careful.
// only key, cnt, num, bgn, end are used !
// key,cnt are pitch center key (midi note) and a weirdly calc'd cents.
// num of 0 means NO loop - undefined bgn,end.  (lpBgn=lpEnd=len)
// loop on calc'd from num!=0 and bgn,end being < sample length
// .per should not be used.  use FMT.nSamplePerSec.  but i set it.
// loop is always 0 for me - loop forward (pingpong=1,backward=2 are dumb)
// remaining set to 0
struct WAVESMPL {                      // i'm keepin chunkID,chunkSize outa it
   ubyt4 manuf, prod,                  // junk - 0
         per,                          // junk - set from fmt.nSaamplePerSec
         key, cnt,                     // sampled midi key, cent offset - USED
         sfmt, sofs,                   // smpte junk - 0
         num,                          // num loops - always 1 fer me
         dat,                          // junk - 0
// this is actually Loops[] struct but always 1 of em fer me
         cue, loop,                    // junk - 0, looptype always 0=forward
         bgn, end,                     // loopBgn, loopEnd - USED
         frc, times;                   // fraction? playcount? junk - 0
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
//    _name                path/fn
//    _frq                 samples/sec from 'fmt ' chunk
//    _mem, _len           samples ('data' chunk) _len is in samples !
//    _bits, _byts         bits/sample bytes/sample
//    _mono, _real, _loop  mono/stereo, real/integer, looping/not
//    _lBgn, _lEnd         loop bgn/end IF _loop else set to _len
//    _key, _cnt           midi key, cents of pitch center
//
// then update:
//    _frq,   _bgn,_end,   _loop,_lBgn,_lEnd,   _key,_cnt,
// BUTT DO NOT update:
//    _mem,_len,_bits,_byts,_mono,_real can't be updated !!
// ta Save which will ALWAYS write a 'smpl' chunk

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

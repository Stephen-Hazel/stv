// wav.h - load/save a .wav file

#include "os.h"

#define EVEN(n)     ((n)&0xFFFFFFFE)
#define EVEN_UP(n)  EVEN((n)+1)

typedef struct GUID {
   ubyt4 Data1;
   ubyt2 Data2;
   ubyt2 Data3;
   ubyte Data4 [8];
} GUID;

typedef struct {
   sbyt2 wFormatTag;
   sbyt2 nChannels;
   sbyt4 nSamplesPerSec;
   sbyt4 nAvgBytesPerSec;
   sbyt2 nBlockAlign;
   sbyt2 wBitsPerSample;
   sbyt2 cbSize;
} WAVEFORMATEX;

typedef struct {
   WAVEFORMATEX Format;
   union {
      sbyt2 wValidBitsPerSample;
      sbyt2 wSamplesPerBlock;
      sbyt2 wReserved;
   } Samples;
   sbyt4   dwChannelMask;
   GUID    SubFormat;
} WAVEFORMATEXTENSIBLE;

typedef struct {
   ubyt4 manuf;  ubyt4 prod;  ubyt4 per;  ubyt4 key;  ubyt4 cnt;
   ubyt4 sfmt;   ubyt4 sofs;  ubyt4 num;  ubyt4 dat;  ubyt4 cue;
   ubyt4 loop;   ubyt4 bgn;   ubyt4 end;  ubyt4 frc;  ubyt4 times;
} WAVESMPL;
/*
#define SamplerID  'smpl'

typedef struct {
   sbyt4 dwIdentifier;
   sbyt4 dwType;
   sbyt4 dwStart;
   sbyt4 dwEnd;
   sbyt4 dwFraction;
   sbyt4 dwPlayCount;
} SampleLoop;

typedef struct {
   ID    chunkID;
   sbyt4 chunkSize;
   sbyt4 dwManufacturer;
   sbyt4 dwProduct;
   sbyt4 dwSamplePeriod;
   sbyt4 dwMIDIUnityNote;
   sbyt4 dwMIDIPitchFraction;
   sbyt4 dwSMPTEFormat;
   sbyt4 dwSMPTEOffset;
   sbyt4 cSampleLoops;
   sbyt4 cbSamplerData;
   struct SampleLoop Loops [];
} SamplerChunk;
*/
class Wav {
public:
   Wav ();
  ~Wav ();
   void  Wipe ();
   char *Load (char *fn);
   void  Save (char *fn);
   TStr    _name;
   MemFile _mf;
   void   *_mem;                       // wav sample data - ptr sbyte/sbyt2[1/2]
   bool    _mono, _loop;
   ubyte   _byts;                      // 1,2,3,4
   ubyte   _bits, _key, _cnt;          // 8,16,24,32;  sampled key n cent tuning
   ubyt4   _len,  _frq,                // num samples, frequency,
           _bgn, _end, _lBgn, _lEnd;   // bgn/end loop point bgn/end
   WAVEFORMATEXTENSIBLE _fmt;
   ubyt4                _fmtSz;
   WAVESMPL            *_smp;
};

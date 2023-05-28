// wav.h - load/save a .wav file

#include "os.h"

#define EVEN(n)     ((n)&0xFFFFFFFE)
#define EVEN_UP(n)  EVEN((n)+1)

const sbyt2 WAVE_FORMAT_PCM        = 0x0001;
const sbyt2 WAVE_FORMAT_IEEE_FLOAT = 0x0003;
const sbyt2 WAVE_FORMAT_EXTENSIBLE = 0xFFFE;

struct GUID {                          // so dumbb
   ubyt4 Data1;
   ubyt2 Data2;
   ubyt2 Data3;
   ubyte Data4 [8];
};

extern GUID KSDATAFORMAT_SUBTYPE_PCM;
extern GUID KSDATAFORMAT_SUBTYPE_IEEE_FLOAT;

struct WAVEFORMATEX {
   sbyt2 wFormatTag;
   sbyt2 nChannels;
   sbyt4 nSamplesPerSec;
   sbyt4 nAvgBytesPerSec;
   sbyt2 nBlockAlign;
   sbyt2 wBitsPerSample;
   sbyt2 cbSize;
};

struct WAVEFORMATEXTENSIBLE {
   WAVEFORMATEX Format;
   union {
      sbyt2 wValidBitsPerSample;
      sbyt2 wSamplesPerBlock;
      sbyt2 wReserved;
   } Samples;
   sbyt4    dwChannelMask;
   GUID     SubFormat;
};

typedef char ID [4];

struct CHUNK {
   ID    tag;
   ubyt4 siz;
};

#define SamplerID  'smpl'

struct WAVESMPL {
   ubyt4 manuf;  ubyt4 prod;  ubyt4 per;  ubyt4 key;  ubyt4 cnt;
   ubyt4 sfmt;   ubyt4 sofs;  ubyt4 num;  ubyt4 dat;  ubyt4 cue;
   ubyt4 loop;   ubyt4 bgn;   ubyt4 end;  ubyt4 frc;  ubyt4 times;
};

struct SampleLoop {                    // ...nawww gonna just use WAVESMPL
   sbyt4 dwIdentifier;
   sbyt4 dwType;
   sbyt4 dwStart;
   sbyt4 dwEnd;
   sbyt4 dwFraction;
   sbyt4 dwPlayCount;
};

struct SamplerChunk {                  // (see above)
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
   SampleLoop Loops [];
};

class Wav {
public:
   Wav ();
  ~Wav ();
   void  Wipe ();
   char *Load (char *fn);
   void  Save (char *fn);
   TStr  _name;
   void *_mem;                       // wav sample data - of whatever kinda
   ubyt4 _len;                       // len of it in bytes
   bool  _mono, _real, _loop;
   ubyt4 _frq;                       // frequency,
   ubyte _byts;                      // 1,2,3,4,8
   ubyte _bits, _key, _cnt;          // 8,16,24,32;  sampled key n cent tuning
   ubyt4 _bgn, _end, _lBgn, _lEnd;   // bgn/end loop point bgn/end
private:
   MemFile              _mf;           // don't be messin w deez
   WAVEFORMATEXTENSIBLE _fmt;
   ubyt4                _fmtSz;
   WAVESMPL             _smp;
};

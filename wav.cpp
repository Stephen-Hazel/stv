// wav.cpp - mostly for my sample editor waver.  actually mostly Syn these days.

#include "wav.h"
#include "midi.h"                      // need MKey,MKey2Str

GUID KSDATAFORMAT_SUBTYPE_PCM = {
   0x00000001,0x0000,0x0010,
   {0x80,0x00,0x00,0xaa,0x00,0x38,0x9b,0x71}
};
GUID KSDATAFORMAT_SUBTYPE_IEEE_FLOAT = {
   0x00000003,0x0000,0x0010,
   {0x80,0x00,0x00,0xaa,0x00,0x38,0x9b,0x71}
};

void Wav::Wipe ()  // wipe all data
{  _mf.Shut ();
   _name [0] = '\0';   _mem = nullptr;   _len = 0;
   MemSet (& _fmt, 0, sizeof (_fmt));
   MemSet (& _smp, 0, sizeof (_smp));
}


char *Wav::Load (char *fn)
{ ubyte *p, *pb, *pe;
  ubyt4  l;
  CHUNK  chnk;
  ID     id;
  TStr   ts;
  bool   got [3];
  static BStr out;
TRC("Wav::Load `s", fn);
   Wipe ();
   *out = '\0';
   StrCp (_name, fn);   MemSet (got, 0, sizeof (got));
   if ((pb = (ubyte *) _mf.Open (fn)) == NULL)
                                return StrCp (out, CC("ERROR can't read"));
   p = pb;
   if ((l = _mf.Len ()) < 12)   return StrCp (out, CC("ERROR not a WAV file"));
   MemCp (& chnk, p,   8);
   MemCp (id,     p+8, 4);   p += 12;  l -= 12;
   if (MemCm (chnk.tag, CC("RIFF"), 4, 'x'))
                                return StrCp (out, CC("ERROR no RIFF chunk"));
   if (MemCm (id,        CC("WAVE"), 4, 'x'))
                                return StrCp (out, CC("ERROR no WAVE id"));
   for (pe = p + l;  p < pe;) {
//DBG(" pofs=`08x=`d/`08x=`d", (int)(p-pb), (int)(p-pb), l, l);
      if ((p + 8) > pe)  break;        // hit end?  bail

      MemCp (& chnk, p, 8);   p += 8;
//DBG(" ckSize=`08x=`d", chnk.siz, chnk.siz);
      if (chnk.siz & 0x80000000)  break;    // rogue neg ckSize?  bail
      if (p + chnk.siz > pe)      break;

      if      (MemCm (chnk.tag, CC("fmt "), 4, 'x') == 0) {
//DBG("  got fmt");
         got [0] = true;
         MemCp (& _fmt, p, _fmtSz = chnk.siz);
      }
      else if (MemCm (chnk.tag, CC("data"), 4, 'x') == 0) {
//DBG("  got data   bytes=`d", chnk.siz);
         got [1] = true;
         _mem = p;   _len = chnk.siz;  // initially #bytes but #samples later
      }
      else if (MemCm (chnk.tag, CC("smpl"), 4, 'x') == 0) {
//DBG("  got smpl");
         got [2] = true;               // optional
         MemCp (& _smp, p, sizeof (WAVESMPL));
      }
      p += EVEN_UP (chnk.siz);
   }
   if (! got [0])  return StrCp (out, CC("ERROR no fmt chunk"));
   if (! got [1])  return StrCp (out, CC("ERROR no data chunk"));
   _real = false;
   if      (_fmt.Format.wFormatTag == WAVE_FORMAT_PCM)         _real = false;
   else if (_fmt.Format.wFormatTag == WAVE_FORMAT_IEEE_FLOAT)  _real = true;
   else if (_fmt.Format.wFormatTag == WAVE_FORMAT_EXTENSIBLE) {
      if      (! MemCm (RC(char *,& _fmt.SubFormat),
                        RC(char *,& KSDATAFORMAT_SUBTYPE_PCM),
                        sizeof (GUID), 'x'))                   _real = false;
      else if (! MemCm (RC(char *,& _fmt.SubFormat),
                        RC(char *,& KSDATAFORMAT_SUBTYPE_IEEE_FLOAT),
                        sizeof (GUID), 'x'))                   _real = true;
      else         return StrCp (out, CC("ERROR format unknown"));
   }
   else            return StrCp (out, CC("ERROR format unknown"));
   if (_fmt.Format.nChannels > 2)
                   return StrCp (out, CC("ERROR beyond 2 channels"));
// whew, the damn thing is ok
   _frq  =  _fmt.Format.nSamplesPerSec;
   _mono = (_fmt.Format.nChannels == 1) ? true : false;
   _bits =  _fmt.Format.wBitsPerSample;
   _byts =  _fmt.Format.nBlockAlign / (_mono ? 1 : 2);
   _len /=  _byts*(_mono?1:2);
   _bgn = 0;   _end = _len - 1;
TRC(" _frq=`d _mono=`b _bits=`d _byts=`d _len=`d _bgn=`d _end=`d",
_frq,_mono,_bits,_byts,_len,_bgn,_end);
   if (got [2]) {
      _key = (ubyte)_smp.key;
      _cnt = (ubyte)((ubyt4)_smp.cnt/((ubyt4)0x80000000/50));
TStr x;
TRC(" _key=`s _cnt=`d (_smp.cnt=`d)", MKey2Str(x,_key), _cnt, _smp.cnt);
   // smpl is ILLDEFINED, check stuph !
      if ((_smp.bgn >= _len) || (_smp.end > _len) || (_smp.end <= _smp.bgn)) {
TRC(" _len=`d _smp.lpBgn=`d _smp.lpEnd=`d  ...weird so",
_len,_smp.bgn,_smp.end);
         _smp.bgn = _len;
TRC(" _smp.bgn = _len now  (so cant loop)");
      }
      _loop = true;   _lBgn = _smp.bgn;
                      _lEnd = _smp.end - 1;
TRC(" _loop=true _lBgn=`d _lEnd=`d from _smp.bgn/end-1", _lBgn,_lEnd);
      if (_lBgn > _lEnd)  {
        ubyt4 t = _lEnd;
         _lEnd = _lBgn;   _lBgn = t;
TRC(" swapped _lBgn n _lEnd");
      }
      if (_lBgn > _end) {
         _lBgn = _end;
TRC(" limit _lBgn to _end");
      }
      if (_lEnd > _end) {
         _lEnd = _end;
TRC(" limit _lEnd to _end");
      }
      if ((_smp.num == 0) || ((_lBgn == _end) && (_lEnd == _end))) {
         _loop = false;   _lBgn = _lEnd = _len;
TRC(" loop=false!");
      }
   }
   else {
TRC(" no smpl so  _loop = false _lBgn=_lEnd=_len=`d _key=4c _cnt=0", _len);
      _loop = false;   _lBgn = _lEnd = _len;
      _key = MKey (CC("4c"));   _cnt = 0;
TRC(" _smp 0d cept per,key,_lBgn=_lEnd=_len=`d", _len);
      MemSet (& _smp, 0, sizeof (_smp));
      _smp.per = 1000000000 / _frq;   _smp.key = _key;
      _smp.bgn = _smp.end = _len;
   }
TRC(" fn=`s frq=`d bits=`d mono=`b len=`d lBgn=`d lEnd=`d key=`s cnt=`d",
FnName(fn,_name),_frq,_bits,_mono,_len,_lBgn,_lEnd,MKey2Str(ts,_key),_cnt);
   return nullptr;
}


// then update _frq, _bgn, _end,   _key, _cnt,   _loop, _lBgn, _lEnd
//    BUTT  _mono,_real,_bits,_byts,_len can't be updated !!


void Wav::Save (char *fni)
// always makes a smpl chunk and hasta be mono/stereo
{ File  f;
  TStr  fn;
  ubyt4 ln1, ln2, ln3, ln4;
DBG("Wav::Save '`s'", fni);
   if (_fmt.Format.nChannels > 2)
      {DBG("Wav::Save can't do beyond stereo");   return;}
TStr x;
DBG(" _len=`d _mono=`b _real=`b _loop=`b _frq=`d _byts=`d _bits=`d "
"_key=`s _cnt=`d _bgn=`d _end=`d _lBgn=`d _lEnd=`d",
_len,_mono,_real,_loop,_frq,_byts,_bits,MKey2Str(x,_key), _cnt,
_bgn,_end,_lBgn,_lEnd);
   StrCp (fn, fni);
   _fmt.Format.nSamplesPerSec = _frq;  // ONLY _fmt change is frq !

// rebuild _smp
   _smp.per = 1000000000 / _frq;
   _smp.key = _key ? _key : MKey (CC("4c"));
   _smp.cnt = (sbyt4)(_cnt * ((ubyt4)0x80000000/50));
   _smp.num = 1;

   if (! _loop)  {_smp.num = 0;   _lBgn = _lEnd = _end-_bgn+1;}
   else           _smp.num = 1;
   _smp.bgn = (_lBgn     - _bgn);
   _smp.end = (_lEnd + 1 - _bgn);
DBG(" smp manuf=`d prod=`d per=`d key=`d cnt=`d "
    "sfmt=`d sofs=`d num=`d dat=`d cue=`d "
    "loop=`d bgn=`d end=`d frc=`d times=`d",
_smp.manuf,_smp.prod,_smp.per,_smp.key,_smp.cnt,
_smp.sfmt,_smp.sofs,_smp.num,_smp.dat,_smp.cue,
_smp.loop,_smp.bgn,_smp.end,_smp.frc,_smp.times);

// calc lengths n dump into a file
   ln4 = sizeof (WAVESMPL);
   ln3 = _end - _bgn + 1;  if (ln3 == 0) {
DBG("got _end-_bgn+1=0 :(");
      return;
   }
   if (! _mono) ln3 <<= 1;   ln3 *= _byts;
   ln2 = _fmtSz;
   ln1 = 4 + 8 + ln2 + 8 + ln3;   ln1 += (8 + ln4);   // smpl too
   if (! f.Open (fn, "w")) {
DBG("can't write '`s'", fn);
      return;
   }
   f.Put (CC("RIFF"));  f.Put (& ln1, 4);  f.Put (CC("WAVE"));
   f.Put (CC("fmt "));  f.Put (& ln2, 4);  f.Put (& _fmt, ln2);
   f.Put (CC("data"));  f.Put (& ln3, 4);
   f.Put (& ((ubyte *)_mem) [_bgn*(_mono?1:2)*_byts], ln3);
   f.Put (CC("smpl"));  f.Put (& ln4, 4);  f.Put (& _smp, ln4);
   f.Shut ();
}


Wav::Wav (): _mem (nullptr), _len (0)  {_name [0] = '\0';}

Wav::~Wav ()  {Wipe ();}

// wav.cpp

#include "wav.h"
#include "midi.h"

void Wav::Wipe ()  // wipe all data
{  _mf.Shut ();
   _name [0] = '\0';   _mem = nullptr;   _len = 0;
   MemSet (& _fmt, 0, sizeof (_fmt));    _smp = nullptr;
}


char *Wav::Load (char *fn)
{ ubyte *p, *pb, *pe;
  ubyt4  l;
  struct {char ckID [4];  ubyt4 ckSize;} chnk;
  char   id [4];
  TStr   ts;
  bool   got [3];
  static BStr out;
   Wipe ();
//DBG("Load `s", fn);
   StrCp (_name, fn);   MemSet (got, 0, sizeof (got));
   if ((pb = (ubyte *) _mf.Open (fn)) == NULL)
                                return StrCp (out, "ERROR can't read");
   p = pb;
   if (_mf.LenH ())             return StrCp (out, "ERROR file TOO big");
   if ((l = _mf.Len ()) < 12)   return StrCp (out, "ERROR not a WAV file");
   MemCp (& chnk, p, 8);
   MemCp (id, p+8, 4);   p += 12;  l -= 12;
   if (MemCm (chnk.ckID, "RIFF", 4, 'x'))
                                return StrCp (out, "ERROR no RIFF");
   if (MemCm (id,        "WAVE", 4, 'x'))
                                return StrCp (out, "ERROR no WAVE");
   for (pe = p + l;  p < pe;) {
//DBG("pofs=`08x=`d/`08x=`d", (int)(p-pb), (int)(p-pb), l, l);
      if ((p + 8) > pe)  break;                  // hit end?  bail
      MemCp (& chnk, p, 8);  p += 8;
//DBG("ckSize=`08x=`d", chnk.ckSize, chnk.ckSize);
      if (chnk.ckSize & 0x80000000)  break;      // rogue neg ckSize?  bail
      if (p + chnk.ckSize > pe)      break;
      if      (MemCm (chnk.ckID, "fmt ", 4, 'x') == 0) {
//DBG("fmt");
         got [0] = true;
         MemCp (& _fmt, p, _fmtSz = chnk.ckSize);
      }
      else if (MemCm (chnk.ckID, "data", 4, 'x') == 0) {
//DBG("data");
         got [1] = true;
         _mem = p;   _len = chnk.ckSize;
      }
      else if (MemCm (chnk.ckID, "smpl", 4, 'x') == 0) {
//DBG("smpl");
         got [2] = true;
         _smp = (WAVESMPL *) p;
      }
      p += EVEN_UP (chnk.ckSize);
   }
   if (! got [0])  return StrCp (out, "ERROR no fmt chunk");
   if (! got [1])  return StrCp (out, "ERROR no data chunk");
   _frq  = _fmt.Format.nSamplesPerSec;
   _bits = (char)_fmt.Format.wBitsPerSample;
   _mono = (_fmt.Format.nChannels == 1) ? true : false;
   if (_fmt.Format.nChannels > 2)
      return StrCp (out, "ERROR beyond 2 channels");
   if (_bits          > 32)
      return StrCp (out, "ERROR beyond 32 bit samples");
   if      (_bits > 24)  _byts = 4;
   else if (_bits > 16)  _byts = 3;
   else if (_bits >  8)  _byts = 2;
   else                  _byts = 1;
   _len /= (_byts*(_mono?1:2));
   _bgn = 0;   _end = _len - 1;
   if (got [2]) {
      _loop = true;   _lBgn = _smp->bgn;
                      _lEnd = _smp->end - 1;
      _key = (ubyte)_smp->key;
      _cnt = (ubyte)((ubyt4)_smp->cnt/((ubyt4)0x80000000/50));
      if (_lBgn > _lEnd)  { ubyt4 t = _lEnd;   _lEnd = _lBgn;   _lBgn = t;}
      if (_lBgn > _end)  _lBgn = _end;
      if (_lEnd > _end)  _lEnd = _end;
      if ((_lBgn == _end) && (_lEnd == _end))  _loop = false;
   }
   else {
      _loop = false;   _lBgn = 0;   _lEnd = _end;
      _key = 0;   _cnt = 0;
   }

   StrFmt (out, "`>10d `>3d `>4d `>10d",
           _frq, _bits, _fmt.Format.nChannels, _len);
   if (_loop)  StrFmt (& out [StrLn (out)],  " `>10d `>10d ",  _lBgn, _lEnd);
   else        StrCp  (& out [StrLn (out)],  "                       ");
   if (_key)
      StrFmt (& out [StrLn (out)], "`<4s `>4d", MKey2Str (ts, _key), _cnt);
//DBG("   out='`s'", out);
   return out;
}


// then update _frq, _bgn, _end,   _key, _cnt,   _loop, _lBgn, _lEnd
// (_mono,_bits,_byts,_len can't be updated)


void Wav::Save (char *fni)
{ File  f;
  TStr  fn;
  ubyt4 ln1, ln2, ln3, ln4;
  WAVESMPL smp;
//DBG("save '`s'", fni);
   StrCp (fn, fni);

// rebuild fmt's _frq only
   if (_fmt.Format.nChannels > 2)  Die ("can't save beyond stereo");
   _fmt.Format.nSamplesPerSec = _frq;

// rebuild smp from existing _smp if avail
   if (_smp)  MemCp  (& smp, _smp, sizeof (smp));
   else       MemSet (& smp, 0,    sizeof (smp));
   smp.per = 1000000000 / _frq;
   smp.key = _key ? _key : M_NT(M_C,4);
   smp.cnt = (sbyt4)(_cnt * ((ubyt4)0x80000000/50));
   smp.num = 1;

   if (! _loop)  _lBgn = _lEnd = _end-_bgn+1;
   smp.bgn = (_lBgn     - _bgn);
   smp.end = (_lEnd + 1 - _bgn);

// calc lengths n dump into a file
   ln4 = sizeof (WAVESMPL);
   ln3 = _end - _bgn + 1;  if (ln3 == 0) {
DBG("got _end-_bgn+1=0 :(");
      return;
   }
   if (! _mono) ln3 <<= 1;   ln3 *= _byts;
   ln2 = _fmtSz;
   ln1 = 4 + 8 + ln2 + 8 + ln3;   ln1 += (8 + ln4); /* smpl too */
   if (! f.Open (fn, "w")) {
DBG("can't write '`s'", fn);
      return;
   }
   f.Put ("RIFF");  f.Put (& ln1, 4);  f.Put ("WAVE");
   f.Put ("fmt ");  f.Put (& ln2, 4);  f.Put (& _fmt, ln2);
   f.Put ("data");  f.Put (& ln3, 4);
   f.Put (& ((ubyte *)_mem) [_bgn*(_mono?1:2)*_byts], ln3);
   f.Put ("smpl");  f.Put (& ln4, 4);  f.Put (& smp, ln4);
   f.Shut ();
}


Wav::Wav (): _mem (nullptr), _len (0)  {_name [0] = '\0';}

Wav::~Wav ()  {Wipe ();}

// synSnd.h - sound (.WAV) part of syn

#ifndef SYNSND_H
#define SYNSND_H

typedef struct {             // stereo wavs get split to 2 samples
   TStr  fn;                 // wav fn (_k,_v prefix n _l/_r suffix)
   ubyt4 pos, lpBgn, len;    // where it's at in AudO.Smp[] in samples
   real  frq;                // sampled frq matching key.cnt
   ubyte key, cnt,           // root note matchin frq for repitching
         bytes, flopt,       // file format - 24 bit=>3, etc;  0=int,1=flo pt
         chans, lr,          // ;  0=left, 1=right, 2=mono(played on both sides)
         mnVel, mxVel,       // midi key,vel ranges for pickin smp
         mnKey, mxKey;

   void Dump (bool dr)
   { TStr s1, s2;
      DBG("   smp=`s", fn);
      DBG("       mnKey=`d/`s mxKey=`d/`s mnVel=`d mxVel=`d",
          mnKey, dr ? MDrm2Str(s1,mnKey) : MKey2Str(s1,mnKey),
          mxKey, dr ? MDrm2Str(s2,mxKey) : MKey2Str(s2,mxKey), mnVel, mxVel);
      DBG("       len=`d lpBgn=`d lr=`d pos=`d",
          len, lpBgn, lr, pos);
      DBG("       frq=`s key=`d/`s cnt=`d bytes=`d chans=`d",
          R2Str (frq, s1), key, dr ? MDrm2Str(s2,key) : MKey2Str(s2,key),
          cnt, bytes, chans);
   }
} Sample;                              // ...stereo .WAV makes 2 of these

#define MAX_SAMP (88*20*2)             // max #stereo WAVs per sound
                                       // all piano keys - 20 velo grps
extern TStr   WavFn [MAX_SAMP*2];      // hold it while we load it :/
extern Sample TSmp  [MAX_SAMP];

inline int TSmCmp (void *p1, void *p2)
// find first numeric char and get oct,note from there to sort by
{ Sample *s1 = (Sample *)p1, *s2 = (Sample *)p2;
  int i;
   if ((i = s1->mxKey - s2->mxKey))  return i;
   if ((i = s1->mxVel - s2->mxVel))  return i;
   return  s1->lr    - s2->lr;
}

const real MAXS4 = 2147483648.;

#include "wav.h"

class Sound {                          // a dir of stereo or mono .WAV files
public:
   TStr  _nm,                // Piano\AcousticGrand_acouPno|sampset
         _pa;                // c:...\syn\sampset\Piano\AcousticGrand_acouPno\ .
   ubyt4 _siz,  _mxDat;      // #samples of all WAVs, max DAT buf in bytes needd
   real  _max;               // max sample range of all WAVs
   bool  _xFrq, _xRls;       // unpitched;  no release on ntUp (can't be looped)
   Sample *_smp;             // alloc'd
   ubyt2  _nSmp;


   ubyt4 LoadDat (ubyte *dat, ubyt4 pos)
   // actually load each .WAV we found into smp buffer n offset our poss by len
   { TStr  fn;
     File  f;
     sbyt4 smp;
     ubyt4 lenD, s, p1 = pos;
     ubyt2 i;
     ubyte nch, nby, flo, *p;
     real  sr;
     bool  got;
     struct {char tag [4];  ubyt4 siz;} ch;
     char          id [4];
TRC("{ Sound::LoadDat  pos=`d", pos);
      for (i = 0;  i < _nSmp;  i++) {
         _max = 0.0;
         got = false;
         StrCp (fn, _pa);   StrAp (fn, _smp [i].fn);  // already know it opens
//TRC("i=`d/`d fn=.../`s'", i, _nSmp, _smp [i].fn);
         f.Open (fn, "r");   f.Get (& ch, sizeof (ch));
                             f.Get (id,   sizeof (id));
         while (f.Get (& ch, sizeof (ch)) == sizeof (ch)) {
            if (! MemCm (ch.tag, CC("data"), 4, 'x'))
                {lenD = ch.siz;   if (f.Get (dat, lenD) == lenD)  got = true;}
            else f.Seek (EVEN_UP (ch.siz), CC("."));  // skip other Chunks
         }
         f.Shut ();
         if (! got) {
DBG("Sound::LoadDat  no WAV data chunk in `s", fn);
            return 0;
         }
//TRC("lenD=`d", lenD);
      // got it, so convert it to real samples now
         nch = _smp [i].chans;   nby = _smp [i].bytes;   flo = _smp [i].flopt;
         _smp [i].pos = pos;
         for (p = dat, s = 0;  s < _smp [i].len;  s++) {
            if (flo)  {sr = (nby <= 4) ? *((float *)p) : *((double *)p);
                       p +=  nby;}
            else {
               smp = 0;
               switch (nby) {          // single byte is weird; 2,3 similar; 4ez
                  case 1:  smp = ((sbyt4)(*p++) - 128) << 24;          break;
                  case 3:  smp |= *p++ <<  8;
                  case 2:  smp |= *p++ << 16;   smp |= (*p++ << 24);   break;
                  case 4:  smp = *((sbyt4 *)p);   p += 4;              break;
               }
               sr = (real)smp / MAXS4;
            }
            if (fabs (sr) > _max)  _max = fabs (sr);
            AuO.smp [pos++] = sr;
            p += (nby*(nch-1));        // skip smps in any unused chans
         }

      // check if this file does 2 _smp[] entries (stereo r side)
         if ( (i+1 < _nSmp) && (! StrCm (_smp [i+1].fn, _smp [i].fn)) &&
                                        (_smp [i+1].lr == 1) ) {
//TRC("got 2nd smp i=`d/`d pos=`d", i+1, _nSmp, pos);
            _smp [++i].pos = pos;
            for (p = & dat [nby], s = 0;  s < _smp [i].len;  s++) {
               if (flo)  {sr = (nby <= 4) ? *((float *)p) : *((double *)p);
                          p +=  nby;}
               else {
                  smp = 0;
                  switch (nby) {
                     case 1:  smp = ((sbyt4)(*p++) - 128) << 24;          break;
                     case 3:  smp |= *p++ <<  8;
                     case 2:  smp |= *p++ << 16;   smp |= (*p++ << 24);   break;
                     case 4:  smp = *((sbyt4 *)p);   p += 4;              break;
                  }
                  sr = (real)smp / MAXS4;
               }
               if (fabs (sr) > _max)  _max = fabs (sr);
               AuO.smp [pos++] = sr;
               p += (nby*(nch-1));        // skip smps in any unused chans
            }
         }
      // ok, scale them samples so ALL sounds have a max range of +-1.0
         for (;  p1 < pos;  p1++)  AuO.smp [p1] *= (1.0 / _max);
      }
TRC("} Sound::LoadDat  new pos=`d", pos);
      return pos;
   }

//______________________________________________________________________________

   bool LoadFmt (char *wfn, ubyte ky, ubyte vl)
   // load a .WAV file header's fmt chunk into Sample
   // _L, _R suffix means they're mono but should be paired up for stereo
   // if there's only one sample (or one _L, one _R),
   // it's 0-127 for both key and vel (max) range
   { TStr  fn;
     File  f;
     ubyt4 ln, lenD;
     ubyte bt, ch;
     real  fr;
     struct {char tag [4];  ubyt4 siz;} chk;
     char          id [4], got [3];
     WAVEFORMATEXTENSIBLE  wf;
     struct {
        ubyt4 manuf;  ubyt4 prod;  ubyt4 per;  ubyt4 note;  ubyt4 frac;
        ubyt4 sfmt;   ubyt4 sofs;  ubyt4 num;  ubyt4 dat;   ubyt4 cue;
        ubyt4 loop;   ubyt4 bgn;   ubyt4 end;  ubyt4 frc;   ubyt4 times;
     } smpl;
     Sample *s = & TSmp [_nSmp];
      StrCp (fn, _pa);   StrAp (fn, wfn);   StrCp (s->fn, wfn);
      if (! f.Open (fn, "r"))          // _pa always ends w '\'
         {DBG ("Sound::LoadFmt  can't read `s", fn);           return false;}
      if ( (f.Get (& chk, sizeof (chk)) != sizeof (chk)) ||
            MemCm (chk.tag, CC("RIFF"), 4, 'x') )
         {DBG ("Sound::LoadFmt  bad WAV file `s", fn);         return false;}
      if ( (f.Get (id, sizeof (id)) != sizeof (id)) ||
            MemCm (id,     CC("WAVE"), 4, 'x') )
         {DBG ("Sound::LoadFmt  bad WAV file 2 `s", fn);       return false;}
      MemSet (got, 0, sizeof (got));
      while (f.Get (& chk, sizeof (chk)) == sizeof (chk)) {
         if      (! MemCm (chk.tag, CC("fmt "), 4, 'x')) {
            got [0] = 'y';
            if (chk.siz > sizeof(wf))
               {DBG ("Sound::LoadFmt  bad WAV fmt `s", fn);    return false;}
            if (f.Get (& wf, chk.siz) != chk.siz)
               {DBG ("Sound::LoadFmt  eof in fmt `s", fn);     return false;}
         }
         else if (! MemCm (chk.tag, CC("data"), 4, 'x')) {
            got [1] = 'y';
            lenD = chk.siz;
            f.Seek (EVEN_UP (chk.siz), CC("."));     // skip fer now
         }
         else if (! MemCm (chk.tag, CC("smpl"), 4, 'x')) {
            got [2] = 'y';
            if (chk.siz > sizeof(smpl)) {
               if (f.Get (& smpl, sizeof(smpl)) != sizeof(smpl))
                  {DBG ("Sound::LoadFmt  eof1 in smpl `s", fn); return false;}
               f.Seek (EVEN_UP(chk.siz - sizeof(smpl)), CC("."));
            }
            else
               if (f.Get (& smpl, chk.siz) != chk.siz)
                  {DBG ("Sound::LoadFmt  eof2 in smpl `s", fn); return false;}
         }
         else
            f.Seek (EVEN_UP (chk.siz), CC("."));      // skip unused Chunks
      }
      f.Shut ();
      if (! got [0])  {DBG ("Sound::LoadFmt  no WAV fmt", fn);   return false;}
      if (! got [1])  {DBG ("Sound::LoadFmt  no WAV data",fn);   return false;}
      if (! got [2])  {MemSet (& smpl, 0, sizeof (smpl));
                       smpl.note = 60;   smpl.bgn = smpl.end = lenD;}
      fr = (real)   wf.Format.nSamplesPerSec;    // ^ no smpl chunk - no loopin
      ch = (ubyte)  wf.Format.nChannels;         // falls into loop check below
      bt = (ubyte) (wf.Format.nBlockAlign / ch);
      ln = lenD / (ch * bt);
      if ((smpl.bgn >= ln) || (smpl.end > ln) || (smpl.end <= smpl.bgn)) {
//DBG("loop off for `s (len=`d lpBgn=`d lpEnd=`d)", fn,ln,smpl.bgn,smpl.end);
         smpl.bgn = ln;
      }
      if ((_nSmp + ch) > BITS (TSmp))
         {DBG ("Sound::LoadFmt  too many samples `s", fn);   return false;}

      s->bytes = bt;   s->chans = ch;   s->pos = 0;   s->len = ln;
      s->frq   = fr;
      if      (_xFrq || _xRls)              s->lpBgn = ln;
      else if ((s->lpBgn = smpl.bgn) < ln)  s->len = ln = smpl.end;

      if      (wf.Format.wFormatTag == WAVE_FORMAT_PCM)        s->flopt = false;
/*    else if (wf.Format.wFormatTag == WAVE_FORMAT_IEEE_FLOAT) s->flopt = true;
**    else if (wf.Format.wFormatTag == WAVE_FORMAT_EXTENSIBLE) {
**       if      (wf.SubFormat == KSDATAFORMAT_SUBTYPE_PCM)    s->flopt = false;
**       else if (wf.SubFormat == KSDATAFORMAT_SUBTYPE_IEEE_FLOAT)
**                                                             s->flopt = true;
**       else  {DBG ("Sound::LoadFmt  unknown format", fn);   return false;}
**    }
*/
      else     {DBG ("Sound::LoadFmt  unknown format", fn);   return false;}

      s->key = (ubyte)smpl.note;
      s->cnt = (ubyte)((ubyt4)smpl.frac / ((ubyt4)0x80000000/50));
      s->mxKey = ky;   s->mxVel = vl;
      _siz += ln;   if (lenD > _mxDat)  _mxDat = lenD;
      _nSmp++;
      if (ch == 1)                     // mono
         s->lr = StrSt (wfn, CC("_L.WAV")) ? 0 :
                (StrSt (wfn, CC("_R.WAV")) ? 1 : 2);
      else {                           // stereo
         MemCp (s+1, s, sizeof (TSmp[0]));
         s->lr = 0;   s++;   s->lr = 1;   _siz += ln;   _nSmp++;
      }
      return true;
   }


   bool SndDir (char *snd, char *dss, char *dds)
   // split out sampset,drumset and make _pa (actual sound dir in filesys)
   // set _xFrq,_xRls too
   // lookin up defaults if needed.  _pa doesn't end w / YET !!  that's later
   { TStr  dr, ss, ds, mn, dfd [1024];
     char *p;
     ubyt4 i, j, ig;
     FDir  d;
     Path  pa;
     File  f;
TRC("SndDir bgn _nm=`s dss=`s dds=`s", snd, dss, dds);
      StrCp (_nm, snd);   App.Path (_pa, 'd');
                          StrAp (_pa, CC("/device/syn/"));
      ig = StrLn (_pa);                // don't care bout that part o path

   // drum - pretty rough
      if      (! MemCm (_nm, CC("Drum/"), 5)) {
         StrCp (dr, & _nm [5]);   *ss = *ds = *mn = '\0';
         if ((p = StrCh (dr, ':'))) {
            *p++ = '\0';   StrCp (ds, p);
            if ((p = StrCh (ds, '/')))
               {*p++ = '\0';   *mn = '_';   StrCp (& mn [1], p);}
         }
         if ((p = StrCh (dr, '|')))  {*p++ = '\0';   StrCp (ss, p);}
         if (*ss == '\0')  StrCp (ss, dss);
         if (*ds == '\0')  StrCp (ds, dds);
         StrFmt (& _pa [StrLn (_pa)], "`s/Drum/`s/`s`s", ss, ds, dr, mn);
      }

   // melodic etc  etc|sampset\name
      else if (! MemCm (_nm, CC("etc|"), 4)) {
         StrCp (ss, & _nm [4]);
         if (! (p = StrCh (ss, '/'))) {
DBG("Sound::SndDir  no / in `s", _nm);
            return false;
         }
         *p++ = '\0';
         StrFmt (& _pa [StrLn (_pa)], "`s/etc/`s", ss, p);
      }

   // melodic gm  gmDir/gmSnd[|sset][/more]
      else {
         StrCp (dr, _nm);
      // split off /more if got
         if (! (p = StrCh (dr, '/'))) {     // 1st \ BETTER be there
DBG("no / in `s", _nm);
            return false;
         }
         if ((p = StrCh (++p, '/')))
            {*p++ = '\0';   *mn = '_';   StrCp (& mn [1], p);}
         else               *mn = '\0';
      // split off |sset if got
         if ((p = StrCh (dr, '|')))  {*p++ = '\0';   StrCp (ss, p);}
         else                                        StrCp (ss, dss);
      // ok, got path now
         StrFmt (& _pa [StrLn (_pa)], "`s/`s`s", ss, dr, mn);
      }

      _xFrq = _xRls = false;           // no pitching/no release on noteUp
      i = StrLn (_pa);                 // special sound suffix - drum,hold,clip
      if (i > 5) {
         if ((! MemCm (_nm, CC("Drum/"), 5)) ||
             (! MemCm (& _pa [i-5], CC("_drum"), 5))) _xFrq = true;
         if  (! MemCm (& _pa [i-5], CC("_hold"), 5))  _xRls = true;
         if  (! MemCm (& _pa [i-5], CC("_clip"), 5))  _xFrq = _xRls = true;
      }
   // if default sampset, look for it with any _morename
      if ( (! d.Got (_pa)) && (! StrCm (ss, dss)) ) {
TRC(" resolving path=`s for GM", _pa);
         StrCp (dr, _pa);   Fn2Path (dr);   // dr has par dir
         StrCp (_pa, & _pa [StrLn (dr)+1]); // _pa has just fn prefix
         i = pa.DLst (dr, dfd, (ubyt2)BITS (dfd));
         for (j = 0;  j < i;  j++)  if (! MemCm (dfd [j], _pa, StrLn (_pa)))
            {StrFmt (_pa, "`s/`s", dr, dfd [j]);   break;}
         if (j >= i) {
DBG("SndDir can't resolve GM snd `s in dir `s", _pa, dr);
            return false;
         }
      }
TRC("SndDir end _pa=`s xFrq=`b xRls=`b", & _pa [ig], _xFrq, _xRls);
      return true;
   }


   Sound (char *snd, ubyte dKey = 128, char *dss = CC(""), char *dds = CC(""))
   // list dir of .WAVs into w[] and load each wav hdr  (load samples later)
   // snd w _hold suffix=> no release ramp on ntUp, _drum suffix=> un-pitched
   // _clip suffix for both ^                    (or in a drum dir ^ )
   { Path  pa;
     ubyt4 nw,  i, ln;
     TStr  ts;
     ubyte ky, vl, k1;
     ubyt2 ns = 0;
     char *p;
TRC("{ Sound::Sound `s dKey=`d dss=`s dds=`s", snd, dKey, dss, dds);
      _nSmp = 0;   MemSet (TSmp, 0, sizeof (TSmp));
      _siz = _mxDat = 0;   _max = 0.0;

      if (! SndDir (snd, dss, dds)) {       // set _nm,_pa n _xFrq,_xRls
TRC("} Sound::Sound - no sampset for snd");
DBG("Sound::SndDir couldn't turn `s into a dir :(", snd);
         _smp = new Sample [1];
         return;
      }
   // go thru each .WAV of the snd dir
   // _k<note>_ in fn gives max key range
   // _v<velo>_ in fn gives optional max velocity range
      nw = pa.FLst (_pa, WavFn, (ubyt2)BITS (WavFn));
      StrAp (_pa, CC("/"));
      for (i = 0;  i < nw;  i++) {     // pull overrd smpKey,maxVel from WAV fns
         ky = vl = 0xFF;
         StrCp (ts, WavFn [i]);   ln = StrLn (ts);
         for (p = ts;  *p;  p++)  if ( (StrLn (p) >= 4) &&
                                       (! MemCm (p, CC("_k"), 2)) &&
                                       (p [2] >= '0') && (p [2] <= '9') )
            {if ((k1 = MKey          (& p [2])) > 0)  ky = k1;
             break;}
         if (dKey < 128)  ky = dKey;
         for (p = ts;  *p;  p++)  if ( (StrLn (p) >= 3) &&
                                       (! MemCm (p, CC("_v"), 2)) &&
                                       (p [2] >= '0') && (p [2] <= '9') )
            {if ((k1 = (ubyte)Str2Int (& p [2])) > 0)  vl = k1;
             break;}
         if (! LoadFmt (ts, ky, vl)) { // dang, shut this down to 0 samples
TRC("} Sound::Sound - LoadFmt failed");
            _smp = new Sample [1];   _nSmp = 0;   _siz = _mxDat = 0;
            return;
         }
      }
   // make key,vel ranges after sorting
      Sort (TSmp, _nSmp, sizeof (TSmp[0]), TSmCmp);
      for (i = 0;  i < _nSmp;  i++) {  // make key,vel ranges
         if (dKey < 128) {             // drums are ez (just velo rng)
            TSmp [i].mnKey = TSmp [i].mxKey = dKey;
            if      (i == 0)
                  TSmp [i].mnVel = 0;
            else if (TSmp [i-1].mxVel == TSmp [i].mxVel)
                  TSmp [i].mnVel = TSmp [i-1].mnVel;
            else  TSmp [i].mnVel = TSmp [i-1].mxVel+1;
         }
         else {                        // melodic is finicky
            if (i && (TSmp [i-1].mxKey == TSmp [i].mxKey)) {
                                       // same key as last time?
               if (TSmp [i-1].mxVel == TSmp [i].mxVel)     // same vel too?
                     TSmp [i].mnVel = TSmp [i-1].mnVel;    // yep
               else  TSmp [i].mnVel = TSmp [i-1].mxVel+1;  // nope
               TSmp [i].mnKey = TSmp [i-1].mnKey;
            }
            else {                     // start new key
               TSmp [i].mnVel = 0;     // vel rng ez
               TSmp [i].mnKey = i ? TSmp [i-1].mxKey+1 : 0;
            }
         }
      }

   // alloc em n make m permanent.  then we're all setup for LoadDat later.
      _smp = new Sample [_nSmp];   MemCp (_smp, TSmp, _nSmp*sizeof (Sample));
TRC("} Sound::Sound - nSmp=`d", _nSmp);
   }


  ~Sound ()  {delete [] _smp;}


   void Dump ()
   {  DBG("   snd=`s nSmp=`d xFrq=`b xRls=`b siz=`d mxDat=`d",
          _nm, _nSmp, _xFrq, _xRls, _siz, _mxDat);
//    DBG("      dir=`s", _pa);
   }
};

#endif

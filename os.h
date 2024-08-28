//      os.h - os stuph like machine specific types, os base, filesys, etc
//             straight linux - no Qt
#ifndef OS_H
#define OS_H

#include <stdio.h>                     // just gimme em all shoish
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <pthread.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/sendfile.h>


#define MAX_PATH     (240)                  // 4k is just too big :(
#define BITS(a)      (sizeof(a)/sizeof(a[0]))
#define RECOFS(s,f)  ((ubyte *)(& s.f) - (ubyte *)(& s))
#define SC(t,x)      (static_cast<t>(x))    // modern c++ iz duuumb
#define RC(t,x)      (reinterpret_cast<t>(x))
#define CC(s)        (const_cast<char *>(s))


// jist mah types_______________________________________________________________
typedef char           sbyte;          // our usual ints by signed/un, #bytes
typedef unsigned char  ubyte;          // come on c++ - why bits ??
typedef   int16_t      sbyt2;
typedef u_int16_t      ubyt2;
typedef   int32_t      sbyt4;
typedef u_int32_t      ubyt4;
typedef   int64_t      sbyt8;
typedef u_int64_t      ubyt8;
typedef double          real;          // float is worthless
ubyt4 const FIX1  = 10000;             // fixed int holding 99999.9999
ubyt4 const MAXUL = 0xFFFFFFFF;
                                       // char shall be utf-8 !!  always !!
typedef char WStr [32];                // ascii word str - 31 chars max
typedef char TStr [MAX_PATH+1];        // utf8 temp str
typedef char BStr [10*1024];           // bigger than one line, a page-ISH
ubyt4 const MAXTSTR = sizeof(TStr)-1;
ubyt4 const MAXWSTR = sizeof(WStr)-1;

typedef void  (*pfunc)();
typedef char *(*FDoTextFunc)(char *buf, ubyt2 len, ubyt4 pos, void *ptr);
typedef bool  (*FDoDirFunc )(void *ptr, char dfx, char *fn);
                                       // process filenames in a dir
                                       // return true to STOP;  false=keep goin
inline ubyt4 ABSL (sbyt4 i)  {if (i < 0)  return SC(ubyt4,-i);
                              return SC(ubyt4, i);}
inline bool  CHNUM(char  c)  {return ((c>='0') && (c<='9')) ? true : false;}
inline char  CHUP (char  c)  {if ((c >= 'a') && (c <= 'z'))  return c-'a'+'A';
                              return c;}
inline char  CHDN (char  c)  {if ((c >= 'A') && (c <= 'Z'))  return c-'A'+'a';
                              return c;}

// mem stuff
void  MemSet  (void *dst, ubyte c,   ubyt4 len);
void  MemCp   (void *dst, void *src, ubyt4 len);
void *MemCh   (void *mem, ubyte c,   ubyt4 len);
char *MemSt   (void *big, char *sm,  ubyt4 len, char x = '\0');
sbyt4 MemCm   (char *s1,  char *s2,  ubyt4 len, char x = '\0');

// str stuff
ubyt4 StrLn   (char *str);
char *StrCp   (char *dst, char *src, char ul = '\0');     // up/lo case?
char *StrAp   (char *dst, char *src, ubyt4 ofs = 0);
char *StrCh   (char *str, char c);
char *StrSt   (char *big, char *sm,    char x = '\0');    // exact? or caseless
sbyt4 StrCm   (char *s1,  char *sbyt2, char x = '\0');
int   StrCm2  (void *p1, void *p2);
ubyt2 ZZLn    (char *zz);                   // zz num strs in list (not StrLn)
void  ZZCp    (char *dzz, char *szz);
void  ZZAp    (char *dzz, char *szz);       // zz str list append

// str formattin stuff
char *Int2Str (sbyt4 Int, char *buf12, char base = 'd');   // 'x' for hex
sbyt4 Str2Int (char *Str, char **p = nullptr);
char *StrFmt  (char *s, char const *fmt, ...);   // my sprintf replacement

void  DBGTH   (char const *s);
void  DBG     (char const *fmt, ...);
void  DbgX    (char *s, char zz = '\0');
#define TRC(...)  if(App.trc)DBG(__VA_ARGS__)    // DBG but only if tracing on

// str array/file-ish stuff
ubyt4 LinePos (char *s, ubyt4 ln);     // pos of start of line ln in \n sep'd s
ubyt4 NextLn  (char *str, char *buf, ubyt4 len, ubyt4 p);
char *Chomp   (char *str, char end = 'b');  // default to killin 1st, else end
char *ReplCh  (char *str, char fr, char to);
ubyt4 PosInZZ (char *t, char *s,                   char x = '\0');
ubyt4 PosInWZ (char *t, char *s, ubyt2 w,          char x = '\0');
ubyt4 PosInWH (char *t, char *s, ubyt2 w, ubyt4 h, char x = '\0');

// fn stuff
sbyt4 PathCmp (char *f1, char *f2);    // can't just StrCm fns cuzu dir depths
char *Fn2Name (char *fn);              // strip .ext leaving path/name
char *Fn2Path (char *fn);              // strip name.ext leaving path
char *FnExt   (char *ext, char *fn);   // return just ext
char *FnName  (char *nm,  char *fn);   // return just name.ext
char *FnFix   (char *fn,  char to = '_');   // remove special chars

void  RandInit ();                     // seed w current millisec
ubyt4 Rand (ubyt4 n);                  // return pseudo random num - 0..n-1

void  Sort (void *ptr, size_t num, size_t siz, int (*cmp)(void *, void *) );

// array of record stuff - be careful with len during RecIns,RecDel !!
inline void  RecClr (void *p, ubyt4 siz, ubyt4 r = 0, ubyt4 nr = 1)
{  MemSet ((ubyte *)p + r*siz, 0, nr*siz);  }

inline void  RecCp  (void *p, ubyt4 siz, ubyt4 d, ubyt4 s, ubyt4 nr = 1)
{  MemCp ((ubyte *)p + d*siz, (ubyte *)p + s*siz, nr*siz);  }

inline ubyt4 RecMv  (void *p, ubyt4 len, ubyt4 siz, ubyt4 r, char to = 'd')
{ BStr  tmp;
  ubyt4 t;
   if ((to == 'u') || (to == 't'))
        {if (r == 0    )  return r;
         t = (to == 'u') ? r-1 : 0;}
   else {if (r == len-1)  return r;
         t = (to == 'd') ? r+1 : len-1;}
   MemCp (& tmp, (ubyte *)p + r*siz, siz);   RecCp (p, siz, r, t);
   MemCp ((ubyte *)p + t*siz, & tmp, siz);
   return t;
}

inline void  RecIns (void *p, ubyt4 len, ubyt4 siz, ubyt4 r = 0, ubyt4 nr = 1)
// len should be NEW LENGTH (not old len)
{  if (r < len)  RecCp  (p, siz, r+nr, r, len-r-nr);  }

inline void  RecDel (void *p, ubyt4 len, ubyt4 siz, ubyt4 r = 0, ubyt4 nr = 1)
// len should be OLD LENGTH (not new len)
{  if (r < len)  RecCp  (p, siz, r, r+nr, len-r-nr);  }


//______________________________________________________________________________
template<typename RO, ubyt4 MX = 256 > class Arr {
// tame those arrays of structs (Row Array, Raggy)
public:
   static const ubyt4 Mx = MX;
   ubyt4              Ln;

   Arr ()  {         _row = new RO [MX];   _siz = sizeof (RO);   Ln = 0;}
  ~Arr ()  {delete[] _row;}

   void Dump (char *nm)  {DBG("`s `08x[`d/`d] siz=`d", nm, _row, Ln, Mx, _siz);}
   RO& el0 ()   {return _row [0];}

   RO& operator[] (int i)
   {  if ((ubyt4)i > Ln) {             // allow &arr[Ln] even tho not there yet
        TStr err;
         StrFmt (err, "Arr[`d] wanted but len=`d  (Mx=`d,siz=`d)",
                 i, Ln, Mx, _siz);
      }
      return _row [i];
   }

   ubyte *Ptr ()  {return (ubyte *)_row;}
   ubyt4  Siz ()  {return          _siz;}

   void Clr (ubyt4 r, ubyt4 nr = 1)
   {  MemSet ((ubyte *)& _row [r], 0,  nr * _siz);  }

   void Cp  (ubyt4 d, ubyt4 s, ubyt4 nr = 1)
   {  MemCp  ((ubyte *)& _row [d], (ubyte *)& _row [s], nr * _siz);  }

   bool Full (ubyt4 nr = 1)  {return (Ln + nr > Mx) ? true : false;}

   ubyt4 Ins (ubyt4 r = MAXUL, ubyt4 nr = 1)
   {  if (Full (nr)) {
        TStr err, db;
         StrFmt (err, "FULL in Arr.Ins(r=`d,nr=`d)  Ln=`d Mx=`d siz=`d)",
                 r, nr,  Ln, Mx, _siz);
         return (r == MAXUL) ? Ln : r;
      }
      if (r == MAXUL)  r = Ln;
      if (r < Ln)  Cp (r + nr, r, Ln - r);
      Clr (r, nr);
      Ln += nr;
      return r;
   }

   void Del (ubyt4 r, ubyt4 nr = 1)
   {  if (r < Ln)  Cp (r, r + nr, Ln - r - nr);
      Ln -= nr;
   }

   void MvUp (ubyt4 r)
   { RO t;
      if (r == 0)  return;
      MemCp (& t,   & _row [r], _siz);    Del (r);   Ins (r-1);
      MemCp (& _row [r-1], & t, _siz);
   }

   void MvBgn (ubyt4 r)
   { RO t;
      if (r == 0)  return;
      MemCp (& t, & _row [r], _siz);      Del (r);   Ins (0);
      MemCp (& _row [0], & t, _siz);
   }

   void MvDn (ubyt4 r)
   { RO t;
      if (r >= Ln-1)  return;
      MemCp (& t, &   _row [r], _siz);    Del (r);   Ins (r+1);
      MemCp (& _row [r+1], & t, _siz);
   }

   void MvEnd (ubyt4 r)
   { RO t;
      if (r >= Ln-1)  return;
      MemCp (& t,    & _row [r], _siz);   Del (r);   Ins (Ln);
      MemCp (& _row [Ln-1], & t, _siz);
   }
private:
   ubyt4 _siz;
   RO   *_row;
};


//______________________________________________________________________________
// parses a record with columns separated by (usually) spaces - handy for files
// NOTE - your Rec string is used as a buffer and will be WRECKED upon return !!
class ColSep {
public:
   char *Col [90];
   ubyte Len;
   ColSep (char *Rec, ubyte Pre, char Sp = ' ')
   { ubyt2 p, c;
      if (Pre >= BITS (Col))  Pre = BITS (Col) - 1;  // just to be sorta safe
   // skip any leading spaces
      for (p = 0;  Rec [p] && (Rec [p] == Sp);  p++)  ;
   // parse out the "pre" space sep'd columns
      for (c = 0;  Pre;  Pre--) {
      // point at it
         Col [c++] = & Rec [p];
      // find end of it
         while (Rec [p] && (Rec [p] != Sp))  p++;
      // if needed, \0 terminate it and skip any trailing Sp
         if (Rec [p]) {
            Rec [p] = '\0';
            for (p++;  Rec [p] && (Rec [p] == Sp);  p++)  ;
         }
      }
   // leftover goes into next Col, rest of Cols point at the last \0
      Col [c++] = & Rec [p];
      while (c < BITS (Col))  Col [c++] = & Rec [StrLn (Rec)];
      for (Len = 0;  Len < 90;  Len++)  if (Col [Len][0] == '\0')  break;
   }
};


//______________________________________________________________________________
extern char *Now   (char *s);          // current time as yyyymmdd.hhmmss.Day
extern char *NowMS (char *s);          // current time in msec for debuggin


class FDir {
public:
   FDir ()  {_d = nullptr;}

   bool Got (char *dir)
   // Open won't distinguish btw missing n empty so that's what WE do
   {  if (StrLn (dir) >= sizeof (TStr)) {
//DBG("FDir::Got dir TOO LONG len=`d dir=`s", StrLn (dir), dir);
         return false;
      }
      if ((_d = opendir (dir)) == nullptr)  return false;
      Shut ();                              return true;
   }

   char Open (char *fn, char *dir, char all = '\0')
   { char df;                          // all of y means list .git, .old dirs
//DBG("FDir::Open dir=`s", dir);
      if (! Got (dir))  return *fn = '\0';
      _dir = dir;   _all = all;
      _d = opendir (_dir);             // Got() made sure it won't be null
      df = Next (fn);
      if (! df)  {Shut ();             // sigh - make it ez on user
//DBG("FDir::Open just 1 so auto Shut");
      }
      return df;
   }

   char Next (char *fn)
   { TStr s;
     int  rc;
      if (_d == nullptr)     {               return *fn = '\0';
DBG("FDir::Next was null:(");}
      if ((_e = readdir (_d)) == nullptr){   return *fn = '\0';
DBG("FDir::Next readdir came back null");}
      StrCp (s, _e->d_name);
      if ((StrLn (_dir) + 1 + StrLn (s)) >= sizeof (TStr)) {
//DBG("FDir::Next filename TOO LONG len=`d dir=`s fn=`s",
//1+StrLn(_dir)+StrLn(s), _dir, s);
         return Next (fn);
      }
      StrFmt (fn, "`s/`s", _dir, s);
      if ( (PosInZZ (s, CC(".\0"     "..\0")) == 0) &&
          ((PosInZZ (s, CC(".git\0"  ".old\0"  ".flatpak-builder\0"
                                           )) == 0) || _all) ) {
        struct stat s;
         if ((rc = stat (fn, & s)))
DBG("FDir::Next stat(`s) died rc=`d", fn, rc);
         else {
            if (S_ISDIR (s.st_mode)) {
               if ((rc = lstat (fn, & s)))
DBG("FDir::Next lstat(`s) died rc=`d", fn, rc);
               else if (! S_ISLNK (s.st_mode)) { // don't follow links!
//DBG("FDir::Next  d=`s", fn);                   // call em files cuz whatev
                  return 'd';
               }
            }
         }
//DBG("FDir::Next  f=`s", fn);
         return 'f';
      }
//DBG("FDir::Next skip '`s'", fn);
      return Next (fn);
   }

   void Shut ()
   {  if (_d) {closedir (_d);   _d = nullptr;}  }

private:
   char   *_dir, _all;
   DIR    *_d;
   dirent *_e;
};


class Path {
public:
   bool Empty (char *dir)
   // exists AND empty - kinda tricky :(
   { FDir d;
     TStr fn;
      if (! d.Got (dir))  return false;     // can't be empty if it ain't there
      if (d.Open (fn, dir))  {d.Shut ();   return false;}
      return true;
   }

// ez-er than Open/Next/Shut
   ubyt2 FLst (char *dir, TStr *lst, ubyt2 max)
   // get (just) files (nonrecursively) in dir matching pat
   { ubyt2 len = 0;
     FDir  d;
     char  df;
     TStr  fn;
      if ((df = d.Open (fn, dir))) {
         do {
            if (df == 'f') {
               if (len >= max)  {DBG ("Path::FLst  len>max  `s", dir);   break;}
               else              StrCp (lst [len++], & fn [StrLn (dir)+1]);
            }
         } while ((df = d.Next (fn)));
         d.Shut ();
      }
      return len;
   }

   ubyt2 DLst (char *dir, TStr *lst, ubyt2 max)
   // get subdirs of dir
   { ubyt2 len = 0;
     FDir  d;
     char  df;
     TStr  fn;
      if ((df = d.Open (fn, dir))) {
         do {
            if (df == 'd') {
               if (len >= max)  {DBG ("Path::DLst  len>max  `s", dir);   break;}
               else              StrCp (lst [len++], & fn [StrLn (dir)+1]);
            }
         } while ((df = d.Next (fn)));
         d.Shut ();
      }
      return len;
   }

// path level "dos" ops
   bool Make (char *dir, ubyt2 perm = 0755);
   bool Kill (char *dir);
   bool Copy (char *from, char *to);
};


class File {
public:
// file level "dos" ops
   bool Copy (char *from, char *to);   // copy from to to
   bool ReNm (char *from, char *to);   // rename from to to
   bool Kill (char *fn);               // kill (delete) fn

   ubyt4 Size (char *fn)
   { struct stat s;
      if (! stat (fn, & s))  return SC(ubyt4,s.st_size);
      return 0;
   }

   char *TmStr (char *ts, char *fn)
   { struct stat s;
      *ts = '\0';
      if (! stat (fn, & s))
         strftime (ts, 20, "%Y%m%d.%H%M%S.%a", localtime (& s.st_mtime));
      return ts;
   }

   ubyt4 HrsOld (char *fn)
   { struct stat s;
      if (stat (fn, & s))  return 0;
      return SC(ubyt4,(time (nullptr) - s.st_mtime) / (60*60));
   }

// read/write stuff
   bool Open (char *name, char const *mode, ubyt2 perm = 0644)
   {  StrCp (_fn, name);
      if      (*mode == 'r') {
         if (0 <= (_f = open (_fn, O_RDONLY)))  return true;
      }
      else if (*mode == 'w') {
        File tf;
         if ((mode [1] == 'b') && tf.Size (_fn)) {    // back it up?
           TStr dir, ext, fn, s;       // get dir n ext
            StrCp (dir, _fn);   Fn2Path (dir);   StrAp (dir, CC("/.old"));
            FnExt (ext, _fn);
            StrFmt  (fn, "`s/`s.`s", dir, Now (s), ext);
            tf.Copy (name, fn);        // copy to back up
         }
        TStr dir;
        Path d;
         StrCp (dir, _fn);   Fn2Path (dir);   if (*dir)  d.Make (dir);
         if (0 <= (_f = open (_fn, O_WRONLY | O_CREAT | O_TRUNC, perm)))
            return true;
      }
DBG("File::Open('`s','`s') failed\n`s", _fn, mode, strerror (errno));
      _f = -1;
      return false;
   }

   void Shut (void)
   {  if (_f >= 0)  close (_f);
      _f = -1;
   }

   bool IsOpen ()  {return (_f >= 0) ? true : false;}
   int Hnd ()      {return  _f;}

   ubyt4 Get (void *buf, ubyt4 len)
   { sbyt4 ln = read  (_f, buf, len);   return (ln<0)?0:ln;}

   ubyt4 Put (void *buf, ubyt4 len)
   { sbyt4 ln = write (_f, buf, len);   return (ln<0)?0:ln;}

   ubyt4 Put (char *buf)  {return Put (buf, StrLn (buf));}

   sbyt4 Seek (sbyt4 amt, char dir = '>')
   { int mode = (dir == '.') ? SEEK_CUR :
               ((dir == '>') ? SEEK_SET : SEEK_END);
      return lseek (_f, amt, mode);
   }

// open/read|write/close funcs
   ubyt4 Load (char *name, void *buf, ubyt4 len, char zt = '\0')
   { ubyt4 l = 0;
      *((char *)buf) = '\0';
      if (Open (name, "r"))  {
         l = Get (buf, len);
         if ((zt != '\0') && (zt != 'z'))  DBG("File::Load BUG w zt");
         if (zt) {
            if (l >= len)  l = len-1;
            ((char *)buf) [l] = '\0';
         }
         Shut ();
      }
      return l;
   }

   ubyt4 Save (char *name, void *buf, ubyt4 len)
   { ubyt4 l = 0;
      if (Open (name, "w"))  {l = Put (buf, len);   Shut ();}   return l;
   }

   void  DoDir  (char *dir,  void *ptr, FDoDirFunc  func, char *skip = nullptr);
   char *DoText (char *name, void *ptr, FDoTextFunc func, ubyt4 maxlen = 500);
private:
   int  _f;
   TStr _fn;
};


//______________________________________________________________________________
class MemFile {
public:
   MemFile ()  {_mem = nullptr;   _len = 0;}

   void *Open (char *fn)
   { File f;
      _mem = nullptr;   _len = 0;
      if (! f.Open (fn, "r"))  return nullptr;
      _len = f.Size (fn);
      _mem = mmap (NULL, _len, PROT_READ, MAP_PRIVATE, f.Hnd (), 0);
      if (_mem == MAP_FAILED) {
DBG("MemFile::Open mmap `s died", fn);
         f.Shut ();   _mem = nullptr;   _len = 0;
         return nullptr;
      }
      f.Shut ();
      return _mem;
   }

   void *Mem ()  {return _mem;}
   ubyt4 Len ()  {return _len;}

   void Shut ()
   {  if (_mem == nullptr)  return;
      if ((munmap (_mem, _len)))
      _mem = nullptr;   _len = 0;
   }
private:
   void *_mem;
   ubyt4 _len;
};


//______________________________________________________________________________
inline int StrArrCmp (void *p1, void *p2)
{ char *s1 = *((char **)p1), *s2 = *((char **)p2);   return StrCm (s1, s2);  }

class StrArr {                         // big str arr that's quick to app,sort
public:
   TStr   nm, x, y, z;
   char **str;   ubyt4 maxs, num;
   char  *buf;   ubyt4 maxb, siz;
   char  *skip, *quit;

   void Wipe ()
   {  if (str) delete [] str;   if (buf) delete [] buf;
      str = NULL;   buf = NULL;
      nm [0] = '\0';   num = maxs = siz = maxb = 0;
   }

   void Init (char *inm, ubyt4 imaxs, ubyt4 imaxb)
   {  Wipe ();
      StrCp (nm, inm);
      maxs = imaxs;   maxb = imaxb;   num = siz = 0;
      str = new char * [maxs];   buf = new char [maxb];
   }

   void Clr ()  {num = siz = 0;}

   void Init (char *inm, ubyt4 imaxs = 1024)
   {  Init (inm, imaxs, imaxs*MAX_PATH);  }

   StrArr ()    {str = NULL;   buf = NULL;   Wipe ();}
   StrArr (char *inm, ubyt4 imaxs)               {str = NULL;   buf = NULL;
                                                  Init (inm, imaxs);}
   StrArr (char *inm, ubyt4 imaxs, ubyt4 imaxb)  {str = NULL;   buf = NULL;
                                                  Init (inm, imaxs, imaxb);}
  ~StrArr ()  {Wipe ();}

   char *Name ()    {return nm;}
   ubyt4 MaxRow ()  {return maxs;}
   ubyt4 NRow ()    {return num;}

   char *Get (ubyt4 r)
   {  if (r >= num) {
         DBG ("StrArr::Get  arr=`s row=`d past end=`d", nm, r, num);
         return nullptr;
      }
      return str [r];
   }

   void Dump ()
   {  DBG(nm);   for (ubyt4 r = 0; r < num; r++)  DBG("`d: `s", r, str[r]);  }

   ubyt4 Got (char *s)
   {  for (ubyt4 r = 0; r < num; r++)  if (! StrCm (Get (r), s))  return 1+r;
      return 0;
   }

   bool Full ()  {return (bool)(num >= maxs);}

   bool Add (char *s, char *rc = nullptr)
   { ubyt4 ln = StrLn (s) + 1;
      if (rc)  *rc = '\0';
      if (((siz + ln) > maxb) || ((num +  1) > maxs)) {
         DBG("StrArr::Add FULL !!");
         return false;
      }
      str [num++] = & buf [siz];
      MemCp (& buf [siz], s, ln);   siz += ln;
      return true;
   }

   bool Ins (char *s, ubyt4 r = 0)     // backwards, but whatev
   {  if (! Add (s))  return false;
     char *t = str [NRow ()-1];
      MemCp (& str [r+1], & str [r], sizeof (char *) * (NRow ()-r-1));
      str [r] = t;
      return true;
   }

   static char *DoRec (char *ibuf, ubyt2 len, ubyt4 pos, void *ptr)
   { StrArr *t = (StrArr *)ptr;
      (void)pos;   (void)len;          // modern DUMB standards
      if (t->quit && (! MemCm (ibuf, t->quit, StrLn (t->quit))))
         return CC("quit");
      if ((t->skip == NULL) || MemCm (ibuf, t->skip, StrLn (t->skip)))
         if (! t->Add (ibuf))  return CC("memorygone");
      return NULL;
   }

   char *Load (char *fn, char *iskip = NULL, char *iquit = NULL)
   { File f;   skip = iskip;   quit = iquit;
      return f.DoText (fn, this, DoRec);
   }

   void Sort (int (*cmp)(void *, void *) = StrArrCmp)
   {  ::Sort (str, NRow (), sizeof (str[0]),     cmp);  }

   void GetDir (char *dir, char fd = 'd', ubyt4 max = 1024, char ext = '\0')
   // put dirs or files of a source dir into me
   { TStr fn, t;
     FDir d;
     char df;
      Init (dir, max);
      if (! (df = d.Open (fn, dir)))  return;
      do    if (fd == df) {
         StrCp (t, & fn [StrLn (dir)+1]);
         if (ext)  Fn2Name (t);
         if (! Add (t))  break;
      }
      while ((df = d.Next (fn)));
      d.Shut ();
      Sort ();
   }

   char *SetZZ (char *zi)              // z better be & BStr
   { char *p, *z = zi;
     ulong l, i, t = 0;
      for (i = 0;  i < NRow ();  i++) {
         l = 1 + StrLn (p = Get (i));
         if ((t += l) >= sizeof (BStr))  DBG("StrArr.SetZZ died :(");
         MemCp (z, p, l);   z += l;
      }
      *z = '\0';
      return zi;
   }
};


//______________________________________________________________________________
// load a text file into an array of Strs...
#ifndef STABLE_MAXROW
#define STABLE_MAXROW  8192
#endif

class STable {                         // ...give StrArr 2 dimensions
private:
   StrArr _sa;                         // of maxRow*nCol entries
   ubyte  _nCol;
   ubyt4  _nRow, _maxRow;
   char  *_skip;  // param for DoRec
public:
   void Wipe ()
   {  _nCol = 0;   _nRow = _maxRow = 0;   _sa.Wipe ();}

   STable ()  {Wipe ();}

   void Init (char *nm, ubyte nc = 1, ubyt4 maxRow = STABLE_MAXROW)
   {  Wipe ();
      _nCol = nc;   _maxRow = maxRow;   _sa.Init (nm, _nCol*_maxRow);
   }

   void Cp (STable *src)
   {  Init (src->Name (), src->NCol (), src->MaxRow ());
      for (ubyt4 r = 0;  r < src->NRow ();  r++)
         for (ubyte c = 0;  c < src->NCol ();  c++)  Add (src->Get (r, c));
   }

   void Clr ()  {_sa.Clr ();   _nRow = 0;}

   bool  Initd  ()  {return _nCol ? true : false;}
   char *Name   ()  {return _sa.Name ();}
   ubyte NCol   ()  {return _nCol;}
   ubyt4 NRow   ()  {return _nRow;}
   ubyt4 MaxRow ()  {return _maxRow;};

   char *Get (ubyt4 r, ubyte c)
   {  if (r >= _maxRow)
          DBG("STable::Get `s row `d >= max `d", Name (), r, _maxRow);
      if (c >= _nCol)
          DBG("STable::Get `s col `d >= max `d", Name (), c, _nCol);
      return _sa.Get (r*_nCol + c);
   }

   sbyt4 GetI (ubyt4 r, ubyte c)  {return Str2Int (Get (r, c));}

   ubyt4 Got (char *s, ubyte c = 0)
   {  for (ubyt4 r = 0; r < _nRow; r++)
         if (! StrCm (Get (r, c), s))  return 1+r;
      return 0;
   }

   void Add (char *s)
   {  if ( ((_sa.NRow () % _nCol) == 0) &&
           ((_sa.NRow () / _nCol) >= _nRow) )  _nRow++;
      _sa.Add (s);
   }

   void Ins (ubyt4 r, char **s)
   {  if ((_nRow + 1) > _maxRow) return;    // can't
      r *= _nCol;
     ubyte c = _nCol;
      while (c--)  _sa.Ins (*s++, r++);
      _nRow++;
   }

   void Upd (ubyt4 r, ubyte c, char *s)
   // argh - this is pretttttty ugly, hopefully it werks fer ya...
   {  if (StrLn (Get (r, c)) >= StrLn (s))  StrCp (_sa.str [r*_nCol+c], s);
      else  {_sa.Add (s);   _sa.str [r*_nCol+c] = _sa.str [--_sa.num];}
   }

   static char *DoRec (char *buf, ubyt2 len, ubyt4 pos, void *ptr)
   { STable *t = (STable *)ptr;
     ColSep  ss (buf, t->_nCol-1);
     ubyte   c = t->_nCol;
      if (t->_skip && (MemCm (ss.Col [0], t->_skip, StrLn (t->_skip)) == 0))
         return NULL;
      for (ubyte c = 0; c < t->_nCol; c++)  t->Add (ss.Col [c]);
      return NULL;
   }

   void Load (char *fn, char *skip = NULL,
                        ubyte nc = 1, ubyt4 maxRow = STABLE_MAXROW)
   { TStr nm;
     File f;
      FnName (nm, fn);   Fn2Name (nm);   Init (nm, nc, maxRow);
      _skip = skip;   f.DoText (fn, this, DoRec);
   }

   void FPut (File *f)
   { char rc [8000];
      for (ubyt4 r = 0;  r < NRow ();  r++) {
         *rc = '\0';
         for (ubyte c = 0;  c < NCol ();  c++)  {if (c)  StrAp (rc, CC(" "));
                                                 StrAp (rc, Get (r, c));}
         StrAp (rc, CC("\n"));
         f->Put (rc);
      }
   }

   void Dump ()
   { ubyt4 r;
     ubyte c;
     BStr  d;
      DBG("table=`s", Name ());
      for (r = 0; r < _nRow; r++) {
         StrFmt (d, " `04d: ", r);
         for (c = 0; c < _nCol; c++)  StrFmt (& d [StrLn (d)], "`s`s",
                                              c ? "," : "", Get (r, c));
         DBG(d);
      }
   }
};


class ThLock {                         // thread lock - less typin'
public:
   ThLock ()     {pthread_mutex_init    (& _mutex, nullptr);}
  ~ThLock ()     {pthread_mutex_destroy (& _mutex);}
   void Grab ()  {pthread_mutex_lock    (& _mutex);}
   void Toss ()  {pthread_mutex_unlock  (& _mutex);}
private:
   pthread_mutex_t _mutex;
};


//______________________________________________________________________________
struct AppBase {
public:
   void Init ()
   { TStr s;   CfgGet (CC("trc"), s);
      if (*s)  trc = (*s == 'y') ? true : false;
      else {                           // uh oh !  kick initme !
         CfgPut (CC("trc"), CC("n"));  // skip this?  initme infinite loop :)
         Run (CC("initme"));
         trc = true;
      }
   }

   char *Path (char *s, char typ = 'a')
   // [a]pp, [c]fg, [h]ome, else read [c]/s.cfg  (usually d.cfg)
   // d.cfg will usually give /home/sh/pianocheetah or wherever initme picks
   { char *p;
     TStr  t;
      if (typ == 'a')  return StrCp (s, CC("/app/bin"));
      if (typ == 'c')  return StrCp (s, CC("/var/config"));
      if (typ == 'h') {
         if (! (p = getenv ("HOME")))
            {DBG("getenv HOME failed");   *s = '\0';   return s;}
         StrCp (s, p);   return s;
      }
      t [0] = typ;   t [1] = '\0';
      return CfgGet (t, s);
   }

   char *CfgGet (char *fn, char *s, ubyt4 max = 0)
   { TStr  p, q;
     File  f;
     ubyt4 l;
      StrFmt (p, "`s/`s.cfg", Path (q, 'c'), fn);
      l = f.Load (p, s, max ? max : MAX_PATH);
      if ((l == 0) && StrCm (fn, CC("trc")))     // might be not be initme'd !
         DBG("CfgGet(`s) got nothin :(  CfgPath=`s", fn, q);
      if (max == 0) {
         s [l] = '\0';
         if (l && (s [l-1] == '\n'))  s [--l] = '\0';      // no \n at end !!
      }
      return s;
   }

   void  CfgPut (char *fn, char *s, ubyt4 len = 0)
   { TStr p, q;
     File f;
      StrFmt (p, "`s/`s.cfg", Path (q, 'c'), fn);
      f.Save (p, s, len ? len : StrLn (s));
   }

   void TrcPut (bool tf)
   {  trc = tf;   CfgPut (CC("trc"), CC(tf?"y":"n"));  }
/*
   void Run (char *cmd, ubyte narg)
   { pid_t p;
      p = fork ();
      if (p <  0) {DBG("fork error for `s", cmd);   return;}
      if (p == 0) {
        BStr a;
         Path (a);
        ColSep cs (cmd, narg);
         cs.Col [narg] = nullptr;
         execv (a, cs.Col);
      }
      return;
   }

*/
   void Spinoff (char *cmd)
   // spin it off in another session totally in parallel
   { BStr a, t;
     int  rc;
      StrFmt (a, "setsid `s/`s </dev/null >/dev/null 2>/dev/null &",
              Path (t), cmd);
      if ((rc = system (a)))  DBG("Spinoff `s died rc=`d", a, rc);
   }

   void Run (char *cmd)
   // run n wait for cmd
   { int rc;
      if ((rc = system (cmd)))  DBG("`s died rc=`d", cmd, rc);
   }

   void Open (char *fn)
   { BStr c;   Run (StrFmt (c, "xdg-open `p &", fn));  }

   bool trc;
};

extern AppBase App;


// duuumb - don't use me :/
inline void Die (char const *s)  {DBG("DIED cuz `s", s);   exit (99);}

inline ubyt4 WGet (char *buf, ubyt4 siz, char *url)
{ ubyt4 i = 0;
  TStr  fn, c;
  int   rc;
  File  f;
   do StrFmt (fn, "/tmp/wget.`d", ++i);   while (f.Size (fn));  // find tmp fn
   if ((rc = system (StrFmt (c, "wget -q -O `p `p", fn, url)))) // wget it
         {DBG("`s died rc=`d", c, rc);   i = 0;}
   else  {i = f.Load (fn, buf, siz);
DBG("WGet `p size=`d got=`d", url, siz, i);
          f.Kill (fn);}            // load n kill
   buf [i] = '\0';                     // term string
   return i;
}

inline void Zip (char *dir, char xc = 'x')
{ TStr cmd, pdir;
  int  rc;
   StrCp (pdir, dir);   Fn2Path (pdir);
   if (xc == 'x') {                    // x tract .tar.gz to a dir (n kill it)
     File f;
      if ((rc = system (StrFmt (cmd, "cd `p && tar xzf `p.tar.gz", pdir, dir))))
DBG("`s died rc=`d", cmd, rc);
      StrFmt (cmd, "rm `p.tar.gz", dir);
   }
   else                                // c reate .tar.gz of a dir
      StrFmt (cmd, "cd `p && tar czf `p.tar.gz `p", pdir, dir, dir);
   if ((rc = system (cmd)))
DBG("`s died rc=`d", cmd, rc);
}


#endif

//      os.h - os stuph like machine specific types, os base, filesys, etc
#ifndef OS_H
#define OS_H

#include <stdio.h>                     // just gimme em all shoish
#include <string.h>
#include <stdarg.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/sendfile.h>

#define MAX_PATH     (240)
#define BITS(a)      (sizeof(a)/sizeof(a[0]))
#define RECOFS(s,f)  ((ubyte *)(& s.f) - (ubyte *)(& s))
#define SC(t,x)      (static_cast<t>(x))
#define CC(s)        (const_cast<char *>(s))

// jist mah types_______________________________________________________________
typedef char            sbyte;         // our usual ints by signed/un, #bytes
typedef unsigned char   ubyte;
typedef short           sword;
typedef unsigned short  uword;
typedef long            slong;
typedef unsigned long   ulong;
typedef double          real;          // float is worthless
ulong const FIX1  = 10000;             // fixed int holding 99999.9999
ulong const MAXUL = 0xFFFFFFFF;
                                       // char shall be utf-8
typedef char     WStr [32];            // ascii word str - 31 chars max
typedef char     TStr [MAX_PATH];      // utf8 temp str
typedef char     PStr [10*1024];       // bigger than one line, a page-ISH
ulong const MAXTSTR = sizeof(TStr)-1;
ulong const MAXWSTR = sizeof(WStr)-1;

typedef void  (*pfunc)();
typedef char *(*FDoTextFunc)(char *buf, uword len, ulong pos, void *ptr);
typedef bool  (*FDoDirFunc )(void *ptr, char dfx, char *fn);

inline ulong ABSL (slong i)  {if (i < 0)  return SC(ulong,-i);
                              return SC(ulong, i);}
inline bool  CHNUM(char  c)  {return ((c>='0') && (c<='9')) ? true : false;}
inline char  CHUP (char  c)  {if ((c >= 'a') && (c <= 'z'))  return c-'a'+'A';
                              return c;}
inline char  CHDN (char  c)  {if ((c >= 'A') && (c <= 'Z'))  return c-'A'+'a';
                              return c;}

// mem stuff
void  MemSet  (void *dst, ubyte c, ulong len);
void  MemCp   (void *dst, void *src, ulong len);
void *MemCh   (void *mem, ubyte c, ulong len);
char *MemSt   (void *big, char *sm, ulong len, char x = '\0');
slong MemCm   (char *s1,  char *s2, ulong len, char x = '\0');

// str stuff
ulong StrLn   (char *str);
char *StrCp   (char *dst, char *src);
char *StrAp   (char *dst, char *src, ulong ofs = 0);
char *StrCh   (char *str, char c);
char *StrSt   (char *big, char *sm, char x = '\0');
slong StrCm   (char *s1,  char *s2, char x = '\0');
int   StrCm2  (void *p1, void *p2);

// str formattin stuff
char *Int2Str (slong Int, char *buf12, char base = 'd');   // 'x' for hex
slong Str2Int (char *Str, char **p = nullptr);
char *StrFmt  (char *s, char const *fmt, ...);    // my sprintf replacement
void  DBG     (char const *fmt, ...);

// str array/file-ish stuff
ulong LinePos (char *s, ulong ln);     // pos of start of line ln in \n sep'd s
ulong NextLn  (char *str, char *buf, ulong len, ulong p);
char *Chomp   (char *str, char end = 'b');  // default to killin 1st, else end
char *ReplCh  (char *str, char fr, char to);
ulong PosInZZ (char *t, char *s,                   char x = '\0');
ulong PosInWZ (char *t, char *s, uword w,          char x = '\0');
ulong PosInWH (char *t, char *s, uword w, ulong h, char x = '\0');

// fn stuff
slong PathCmp (char *f1, char *f2);    // can't just StrCm fns cuzu dir depths
char *Fn2Name (char *fn);              // strip .ext leaving path/name
char *Fn2Path (char *fn);              // strip name.ext leaving path
char *FnExt   (char *ext, char *fn);   // return just ext
char *FnName  (char *nm,  char *fn);   // return just name.ext
char *FnFix   (char *fn);              // remove special chars

void  Sort (void *ptr, size_t num, size_t siz, int (*cmp)(void *, void *) );


//______________________________________________________________________________
class FDir {
public:
   FDir ()  {_d = nullptr;}

   char Open (char *fn, char *dir)
   {  if (StrLn (dir) >= sizeof (TStr)) {
DBG("FDir.Open dir TOO LONG len=`d dir=`s", StrLn (dir), dir);
         return *fn = '\0';
      }
      _dir = dir;
      if ((_d = opendir (_dir)) == nullptr)  return *fn = '\0';
      return Next (fn);
   }

   char Next (char *fn)
   { TStr s;
      if (_d == nullptr)                   return *fn = '\0';
      if ((_e = readdir (_d)) == nullptr)  return *fn = '\0';
      StrCp (s, _e->d_name);
      if ((StrLn (_dir) + 1 + StrLn (s)) >= sizeof (TStr)) {
DBG("FDir.Next filename TOO LONG len=`d dir=`s fn=`s",
1+StrLn(_dir)+StrLn(s), _dir, s);
         return Next (fn);
      }
      StrFmt (fn, "`s/`s", _dir, s);
      if (StrCm (s, CC(".")) && StrCm (s, CC("..")) && StrCm (s, CC(".git"))) {
         if (_e->d_type == DT_REG)  return 'f';
         if (_e->d_type == DT_DIR)  return 'd';
// IGNORE FIFO,SOCK,CHR,BLK,LNK !!
      }
      return Next (fn);
   }

   void Shut ()
   {  if (_d) {closedir (_d);   _d = nullptr;}  }

   uword FLst (char *dir, TStr *lst, uword max)
   // get (just) files (nonrecursively) in dir matching pat
   { uword len = 0;
     char  df;
     TStr  fn;
      if ((df = Open (fn, dir))) {
         do {
            if (df == 'f') {
               if (len >= max)  {DBG ("FDir::FLst  len>max  `s", dir);   break;}
               else              StrCp (lst [len++], & fn [StrLn (dir)+1]);
            }
         } while ((df = Next (fn)));
         Shut ();
      }
      return len;
   }

   uword DLst (char *dir, TStr *lst, uword max)
   // get subdirs of dir
   { uword len = 0;
     char  df;
     TStr  fn;
      if ((df = Open (fn, dir))) {
         do {
            if (df == 'd') {
               if (len >= max)  {DBG ("FDir::DLst  len>max  `s", dir);   break;}
               else              StrCp (lst [len++], & fn [StrLn (dir)+1]);
            }
         } while ((df = Next (fn)));
         Shut ();
      }
      return len;
   }
   char   *_dir;
   DIR    *_d;
   dirent *_e;
};


//______________________________________________________________________________
inline char *Now (char *s)
{ time_t t = time (nullptr);
   strftime (s, 20, "%Y%m%d.%H%M%S.%a", localtime (& t));
   return s;
}


class File {
public:
// path level "dos" ops
   bool PathGot (char *fn)
   { FDir d;
     TStr tfn;
      if (d.Open (tfn, fn))  {d.Shut ();  return false;}
      return true;
   }
   bool PathEmpty (char *fn)        // see if any files or dirs inside it
   { FDir d;
     char df;
     TStr tfn;
      if (! PathGot (fn))  return false;    // can't be empty if not there
      df = d.Open (tfn, fn);   d.Shut ();
      return df ? false : true;
   }
   bool PathMake  (char *fn);
   bool PathKill  (char *fn);
   bool PathCopy  (char *from, char *to);

// file level "dos" ops
   bool Kill (char *fn);               // kill (delete) fn
   bool ReNm (char *from, char *to);   // rename from to to
   bool Copy (char *from, char *to);   // copy from to to

   ulong Size (char *fn)
   { struct stat s;
      if (! stat (fn, & s))  return SC(ulong,s.st_size);
      return 0;
   }

   char *TmStr (char *ts, char *fn)
   { struct stat s;
      *ts = '\0';
      if (! stat (fn, & s))
         strftime (ts, 20, "%Y%m%d.%H%M%S.%a", localtime (& s.st_mtime));
      return ts;
   }

   ulong HrsOld (char *fn)
   { struct stat s;
      if (stat (fn, & s))  return 0;
      return SC(ulong,(time (nullptr) - s.st_mtime) / (60*60));
   }

// read/write stuff
   bool  Open (char *name, char const *mode)
   {  StrCp (_fn, name);   *_bkFn = '\0';
      if      (*mode == 'r') {
         if ((_f = fopen (_fn, mode)) != nullptr)  return true;
      }
      else if (*mode == 'w') {
        File tf;
         if ((mode [1] == 'b') && tf.Size (_fn)) {    // back it up?
           TStr dir, ext, fn, s;       // get dir n ext
            StrCp (dir, _fn);   Fn2Path (dir);   FnExt (ext, _fn);
            StrFmt  (fn, "`s/`s.`s", dir, Now (s), ext);
            tf.Copy (name, fn);        // copy to back up
            StrCp (_bkFn, fn);
         }
         if ((_f = fopen (_fn, "w")) != nullptr)  return true;
         else                           DBG("File::Open('`s','w') failed", _fn);
      }
      else if (*mode == 'a')
         if ((_f = fopen (_fn, mode)) != nullptr)  return true;
      _f = nullptr;
      return false;
   }

   void  Shut (void)
   {  fclose (_f);   _f = nullptr;   if (! *_bkFn)  return;
//   TStr c;
//    App.Path (c);   StrAp (c, "\\_DelSame");   BOOT (c, _bkFn);
   }

   bool  IsOpen ()  {return ((_f == nullptr) ? false : true);}

   ulong Get (void *buf, ulong len)  {return fread  (buf, 1, len, _f);}
   ulong Put (void *buf, ulong len)  {return fwrite (buf, 1, len, _f);}
   ulong Put (char *buf)  {return Put (buf, SC(ulong,StrLn (buf)));}

   ulong Seek (slong amt, char *src)
   {  if (src && amt)  return 0;
      return 0;
/*
     DWORD mode;
      mode =  (*src == '.') ? FILE_CURRENT :
             ((*src == '>') ? FILE_END : FILE_BEGIN);
      return ::SetFilePointer (_hand, amt, NULL, mode);
*/
   }

// open/read|write/close funcs
   ulong Load (char *name, void *buf, ulong len)
   { ulong l = 0;
      if (Open (name, "r"))  {l = Get (buf, len);   Shut ();}
      return l;
   }

   ulong Save (char *name, void *buf, ulong len)
   { ulong l = 0;
     TStr  dir;
      StrCp (dir, name);   Fn2Path (dir);   PathMake (dir);
      if (Open (name, "w"))  {l = Put (buf, len);   Shut ();}
      return l;
   }

   void  DoDir  (char *dir,  void *ptr, FDoDirFunc  func, char *skip = nullptr);
   char *DoText (char *name, void *ptr, FDoTextFunc func, ulong maxlen = 500);
private:
   FILE *_f;
   TStr  _bkFn, _fn;
};


//______________________________________________________________________________
inline int StrArrCmp (void *p1, void *p2)
{ char *s1 = *((char **)p1), *s2 = *((char **)p2);   return StrCm (s1, s2);  }

class StrArr {                         // big str arr that's quick to app,sort
public:
   TStr   nm, x, y, z;
   char **str;   ulong maxs, num;
   char  *buf;   ulong maxb, siz;
   char  *skip, *quit;

   void Wipe ()
   {  if (str) delete [] str;   if (buf) delete [] buf;
      str = NULL;   buf = NULL;
      nm [0] = '\0';   num = maxs = siz = maxb = 0;
   }

   void Init (char *inm, ulong imaxs, ulong imaxb)
   {  Wipe ();
      StrCp (nm, inm);
      maxs = imaxs;   maxb = imaxb;   num = siz = 0;
      str = new char * [maxs];   buf = new char [maxb];
   }

   void Clr ()  {num = siz = 0;}

   void Init (char *inm, ulong imaxs = 1024)
   {  Init (inm, imaxs, imaxs*MAX_PATH);  }

   StrArr ()    {str = NULL;   buf = NULL;   Wipe ();}
   StrArr (char *inm, ulong imaxs)               {str = NULL;   buf = NULL;
                                                  Init (inm, imaxs);}
   StrArr (char *inm, ulong imaxs, ulong imaxb)  {str = NULL;   buf = NULL;
                                                  Init (inm, imaxs, imaxb);}
  ~StrArr ()  {Wipe ();}

   char *Name ()    {return nm;}
   ulong MaxRow ()  {return maxs;}
   ulong NRow ()    {return num;}

   char *Get (ulong r)
   {  if (r >= num) {
         DBG ("StrArr::Get  arr=`s row=`d past end=`d", nm, r, num);
         return nullptr;
      }
      return str [r];
   }

   void Dump ()
   {  DBG(nm);   for (ulong r = 0; r < num; r++)  DBG("`d: `s", r, str[r]);  }

   ulong Got (char *s)
   {  for (ulong r = 0; r < num; r++)  if (! StrCm (Get (r), s))  return 1+r;
      return 0;
   }

   bool Full ()  {return (bool)(num >= maxs);}

   void Add (char *s, char *rc = nullptr)
   { ulong ln = StrLn (s) + 1;
      if (rc)  *rc = '\0';
      if ((siz + ln) > maxb)  {if (rc) *rc = 'b';
                               else    DBG ("StrArr::Add  outa buf");
                               return;}
      if ((num +  1) > maxs)  {if (rc) *rc = 's';
                               else    DBG ("StrArr::Add  outa str");
                               return;}
      str [num++] = & buf [siz];
      MemCp (& buf [siz], s, ln);   siz += ln;
   }

   void Ins (char *s, ulong r = 0)     // kinda backwards, but this is how i go
   {  Add (s);
     char *t = str [NRow ()-1];
      MemCp (& str [r+1], & str [r], sizeof (char *) * (NRow ()-r-1));
      str [r] = t;
   }

   static char *DoRec (char *ibuf, uword len, ulong pos, void *ptr)
   { StrArr *t = (StrArr *)ptr;
      (void)pos;   (void)len;          // modern DUMB standards
      if (t->quit && (! MemCm (ibuf, t->quit, StrLn (t->quit))))
         return CC("quit");
      if ((t->skip == NULL) || MemCm (ibuf, t->skip, StrLn (t->skip)))
         t->Add (ibuf);
      return NULL;
   }

   char *Load (char *fn, char *iskip = NULL, char *iquit = NULL)
   { File f;   skip = iskip;   quit = iquit;
      return f.DoText (fn, this, DoRec);
   }

   void Sort (int (*cmp)(void *, void *) = StrArrCmp)
   {  ::Sort (str, NRow (), sizeof (str[0]),     cmp);  }

   void GetDir (char *dir, char fd = 'd', ulong max = 1024, char ext = '\0')
   // put dirs or files of a source dir into me
   { TStr fn, t;
     FDir d;
     char df;
      Init (dir, max);
      if (! (df = d.Open (fn, dir)))  return;
      do    if (fd == df) {
         StrCp (t, & fn [StrLn (dir)+1]);
         if (ext)  Fn2Name (t);
         Add (t);
      }
      while ((df = d.Next (fn)));
      d.Shut ();
      Sort ();
   }
};


#endif

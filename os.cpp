// os.cpp
// 06/04/98 new;  03/04/04 rewrite to toss exceptions;  12/01/20 win=>linux

#include "os.h"
#include <math.h>                      // for NowMS :/

AppBase App;

void MemSet (void *dst, ubyte c, ubyt4 len)
{ ubyte *p = SC(ubyte *,dst);
   while (len--)  *p++ = c;
}

void MemCp (void *dst, void *src, ubyt4 len)
{ ubyte *d = SC(ubyte *,dst), *s = SC(ubyte *,src);
   if ((! s) || (! d))  return;
   if ((d > s) && (d < s+len)) {
        d = SC(ubyte *,dst) + len - 1;
        s = SC(ubyte *,src) + len - 1;
        while (len--)  *d-- = *s--;
   }
   else while (len--)  *d++ = *s++;
}

void *MemCh (void *mem, ubyte c, ubyt4 len)
{ ubyte *p = SC(ubyte *,mem);
   while (len--)  if (*p++ == c)  return SC(void *,--p);
   return nullptr;
}

char *MemSt (void *big, char *sm, ubyt4 len, char x)
{ char *cp;
  ubyt4 ln = StrLn (sm);
//DBG("MemSt big=`08x sm=`s len=`d, x=`c", big, sm, len, x);
   if (! ln)  return SC(char *,big);
   for (cp = SC(char *,big);  len >= ln;  cp++, len--)
      if (! MemCm (cp, sm, ln, x))  return cp;
   return nullptr;
}

sbyt4 MemCm (char *s1, char *s2, ubyt4 len, char x)
{ char  c1, c2;
  sbyt4 rc;
   while (len--) {
      c1 = *s1++;   c2 = *s2++;
      if (! x)  {c1 = CHDN (c1);   c2 = CHDN (c2);}
      if ((rc = c1 - c2))  return rc;
   }
   return 0;
}


//______________________________________________________________________________
ubyt4 StrLn (char *s)                  // guard against NULLs too :/
{ ubyt4 len = 0;   if (s) while (*s++) len++;   return len;}

char *StrCp (char *dst, char *src, char ul)      // slow n safe...
{ ubyt4 len = StrLn (src);   MemCp (dst, src, len+1);
   if (ul)  for (char *p = dst;  len--;  p++)
               *p = ((ul == 'u') ? CHUP (*p) : CHDN (*p));
   return dst;
}

char *StrAp (char *dst, char *src, ubyt4 ofs)
{ ubyt4 ln = StrLn (dst);
   if (ofs > ln)  ofs = ln;
   StrCp (& dst [ln-ofs], src);  return dst;
}

char *StrCh (char *str, char c)
{  return SC(char *,MemCh (str, SC(ubyte,c), StrLn (str)));  }

char *StrSt (char *big, char *sm, char x)
{ char *cp = big, *s1, *s2;
   if (cp == nullptr)  {DBG ("StrSt arg 1 (big str) is NULL");
                        return nullptr;}
   if (sm == nullptr)  {DBG ("StrSt arg 2 (little str) is NULL");
                        return nullptr;}
   if (*sm == '\0')  return big;
   while (*cp) {
      s1 = cp;
      s2 = sm;
      if (x)  while (*s1 && *s2 && !(     *s1 -     *s2 ))  {s1++; s2++;}
      else    while (*s1 && *s2 && !(CHDN(*s1)-CHDN(*s2)))  {s1++; s2++;}
      if (! *s2)  return cp;
      cp++;
   }
   return nullptr;
}

sbyt4 StrCm (char *s1, char *s2, char x)
{ char  c1, c2;
  sbyt4 rc;
   for (;;) {
      c1 = *s1++;   c2 = *s2++;
      if (! x)  {c1 = CHDN (c1);  c2 = CHDN (c2);}
      if ((rc = c1 - c2))    return rc;
      if ((! c1) && (! c2))  break;
   }
   return 0;
}

int StrCm2 (void *p1, void *p2)
{  return StrCm (SC(char *,p1),SC(char *,p2));  }

ubyt2 ZZLn (char *s)
{ ulong r = 0;
   while (*s)  {r++;   s += (StrLn (s)+1);}
   return r;
}

void ZZAp (char *dzz, char *szz)
{ char *p, *dp;
  ubyt4 ln, sln;                                 // get len of szz
   for (p = szz, sln = 1;  (ln = StrLn (p));  p += (ln+1))  sln += (ln+1);
   for (p = dzz;  *p;  p += (StrLn (p)+1))  ;    // get dzz end ta tack onta
   MemCp (p, szz, sln);
}

void ZZCp (char *d, char *s)   {*d = '\0';   ZZAp (d, s);}


//______________________________________________________________________________
// int convert string ops
char *Int2Str (sbyt4 i, char *buf12, char x)
{ char *so, dg;
  bool  ng = false;
  ubyt2 bs = (x == 'x') ? 16 : 10;
  ubyt4 j;
   so = & buf12 [11];   *so = '\0';
   if (i == 0) *(--so) = '0';
   else {
      ng = false;
      if ((x != 'x') && (i < 0)) {ng = true;   j = SC(ubyt4,-i);}
      else                                     j = SC(ubyt4, i);
      while (j) {
         dg = SC(char,j % bs);   dg += ((dg > 9) ? ('A'-10) : '0');
         *(--so) = dg;   j /= bs;
      }
      if (ng)  *(--so) = '-';
   }
   return so;
}

char *Unt2Str (ubyt4 i, char *buf12)
{ char *so;
   so = & buf12 [11];   *so = '\0';
   if (i == 0)  *(--so) = '0';
   else         while (i)  {*(--so) = (i % 10) + '0';   i /= 10;}
   return so;
}

sbyt4 Str2Int (char *s, char **p)
{ sbyt4 i = 0;
  bool  ng = false;
   while ((*s == ' ') || (*s == '\t'))  s++;
   if    (*s == '-')  {s++;  ng = true;}
   while ((*s >= '0') && (*s <= '9'))  i = i * 10 + (*(s++) - '0');
   if (p)  *p = SC(char *,s);
   return ng ? -i : i;
}

char *StrFmtX (char *out, char const *fmti, va_list va)  // sprintf replacement
// `   substitute
// <>0 justification/prefix 0s
// ``  allow ` without substitue
// s   string
// p   path escaped by ' for shell :/
// c   char
// b   bool
// u   unsigned int decimal
// d     signed int decimal
// x   hex int
{ char *fmt, ju, fc,   *s, buf [12],  c, *pc;
  ubyt4      ln,        sLn, padLn,   u;
  sbyt4 i;
  bool  b;
  BStr  bs;
   *out = '\0';   if ((fmt = CC(fmti)) == nullptr)  return out;
   while (*fmt) {
      if (*fmt != '`')                 // un-substitued char (HAS to be ascii)
         {ln = StrLn (out);   out [ln] = *fmt++;   out [ln+1] = '\0';
          continue;}

   // init to stringize dat arg
      ++fmt;   ju = '\0';   s = buf;   ln = padLn = 0;

   // get ju,ln before our format char (fc)
      if (StrCh (CC("<>0"), *fmt)) {
         ju = *fmt++;   ln = SC(ubyt4,Str2Int (fmt, & fmt));
         if (! ln)  {DBG("StrFmtX: bad args (len)");   return out;}
      }

   // set fc n va_arg in whatev we gots.  we end w s pointin to it's str
      switch (fc = *fmt++) {
         case '`':
            s = CC("`");               // allow `` => `
            break;
         case 's': case 'p':
            s =          va_arg (va, char *);
            break;
         case 'u':
            u = SC(ubyt4,va_arg (va, unsigned int));
            s = Unt2Str (u, buf);
            break;
         case 'd':  case 'x':
            i = SC(sbyt4,va_arg (va, int));
            s = Int2Str (i, buf, fc);
            if ((fc == 'x') && (! ju)) {    // square up the hex ln
               ln = StrLn (s);
               if ((ln > 1) && (ln % 2))  {ju = '0';   ln++;}
            }
            break;
         case 'c':
            c = SC(char,va_arg (va, int));   buf [0] = c;   buf [1] = '\0';
            break;
         case 'b':
            b =         va_arg (va, int) ? true : false;
            StrCp (buf, CC(b ? "T" : "F"));
            break;
         default:
            DBG("StrFmtX: bad args (fmt=`s fc=`d)", fmti, fc);   return out;
      }

   // do ju,ln make us put stuff in front of s?
      if ( ju && ((sLn = StrLn (s)) < ln) ) {
         padLn = ln - sLn;
         if (ju != '<') {              // < means put stuff aaafter s (later)
            if (ju != '0')  ju = ' ';
            ln = StrLn (out);   MemSet (& out [ln], SC(ubyte,ju), padLn);
                                          out [ln+padLn] = '\0';
            padLn = 0;
         }
      }

   // in goes s
      if      (s == nullptr)  StrAp (out, CC("NULL"));
      else if (fc != 'p')     StrAp (out, s);
      else {                           // fuckin linux
         ln = StrLn (out);   StrCp (bs, s);   s = bs;
         while (*s) {
            if ((pc = StrCh (s, '\'')) != nullptr) {
               if (pc != s) {
                  *pc = '\0';          // now string ends where ' was
                  StrAp (out, CC("'"));   StrAp (out, s);
                  StrAp (out, CC("'"));
               }
               do StrAp (out, CC("\\'"));   while (*(++pc) == '\'');
                                        s = pc;
            }
            else {
               StrAp (out, CC("'"));   StrAp (out, s);
               StrAp (out, CC("'"));   *s = '\0';     // DONE !!
            }
         }
      }

   // did ju,ln make us put stuff after s?
      if (padLn) {
         ln = StrLn (out);   MemSet (& out [ln], ' ', padLn);
                                       out [ln+padLn] = '\0';
      }
   }
   return out;
}


char *StrFmt (char *so, char const *fmt, ...)     // sprintf replacement
{ va_list va;
   va_start (va, fmt);   StrFmtX (so, fmt, va);   va_end (va);   return so;
}

#include "pthread.h"

void DbgX (char *s, char zz)
{ char *p;
  FILE *f;
  TStr  fn, buf;
   if (! (p = getenv ("HOME")))  return;
   StrCp (fn, p);   StrAp (fn, CC("/dbg.txt"));
   f = fopen (fn, "a");
// f = stderr;
   if (zz) {
     ubyt2 r = 0, ln = ZZLn (s);
      fprintf (f, "%s %s-%08X nZZ=%d\n",
                  NowMS (buf), App.app, SC(int,pthread_self ()), ln);
      while (ln--)  {fprintf (f, "%d: %s\n", r++, s);   s += (StrLn (s)+1);}
   }
   else
      fprintf (f, "%s %s-%08X %s\n",
                  NowMS (buf), App.app, SC(int,pthread_self ()), s);
   fclose (f);
}


inline void DBG (char const *fmt, ...)      // printf-y debugging
{ va_list va;
  BStr    out;
   va_start (va, fmt);   StrFmtX (out, fmt, va);   va_end (va);   DbgX (out);
}


//______________________________________________________________________________
ubyt4 LinePos (char *s, ubyt4 ln)
{ ubyt4 p = 0;
  char *n;
   while (ln--) {
      if ((n = StrCh (& s [p], '\n')) == nullptr)  return 0;
      p += SC(ubyt4,n - & s [p] + 1);
   }
   return p;
}

ubyt4 NextLn (char *str, char *buf, ubyt4 len, ubyt4 p)
{ char *ch;
  ubyt4 ln;
   *str = '\0';
   if ((ch = StrCh (& buf [p], '\n'))) {
      MemCp (str, & buf [p], ln = SC(ubyt4,ch - & buf [p]));
      p += (1 + ln);
      str [ln] = '\0';
   }
   else {StrCp (str, & buf [p]);   p = len;}
   return p;
}

char *Chomp (char *str, char end)
{ char *ch;
   if (end == 'b') {
      if ((ch = StrCh (str, '\r')))  *ch = '\0';
      if ((ch = StrCh (str, '\n')))  *ch = '\0';
   }
   else {
     ubyt4 ln = StrLn (str);
      if (ln && (str [ln-1] == '\n'))  str [--ln] = '\0';
      if (ln && (str [ln-1] == '\r'))  str [--ln] = '\0';
   }
   return str;
}

char *ReplCh (char *s, char f, char t)
{ char *in = s;
   while (*s)  {if (*s == f) *s = t;   s++;}
   return in;
}

ubyt4 PosInZZ (char *t, char *s, char x)
// \0 sep'd, \0\0 term'd list
{ ubyt4 p = 1;
   while (*s) {
      if (StrCm (s, t, x) == 0)  return p;
      p++;   s += (StrLn (s) + 1);
   }
   return 0;
}

ubyt4 PosInWZ (char *t, char *s, ubyt2 w, char x)
// w sep'd, \0 term'd list
{ ubyt4 p = 1;
   while (*s) {
      if (StrCm (s, t, x) == 0)  return p;
      p++;   s += w;
   }
   return 0;
}

ubyt4 PosInWH (char *t, char *s, ubyt2 w, ubyt4 h, char x)
// w sep'd, h term'd list
{ ubyt4 p = 1;
   while (h--) {
      if (StrCm (s, t, x) == 0)  return p;
      p++;   s += w;
   }
   return 0;
}


//______________________________________________________________________________
// filename string ops
sbyt4 PathCmp (char *f1, char *f2)
// sort by just dir, in case different depths, then fn
{ TStr  b1, b2;
  sbyt4 cmp;
   StrCp (b1, f1);   Fn2Path (b1);
   StrCp (b2, f2);   Fn2Path (b2);
   return (cmp = StrCm (b1, b2)) ? cmp : StrCm (f1, f2);
}

char *Fn2Name (char *fn)
// kill .ext:  strip tail end of fn till you hit a .
{ ubyt4 p;
   if ((p = StrLn (fn)) == 0)        return fn;
   do --p;  while (p && (fn [p] != '.') && (fn [p] != '/'));
   if ((p == 0) || (fn [p] == '/'))  return fn;
   fn [p] = '\0';
   return fn;
}

char *Fn2Path (char *fn)
// kill name.ext:  strip tail end of fn till you hit a / n lose it
{ ubyt4 p;
   if ((p = StrLn (fn)) == 0)  return fn;
   do --p;  while (p && (fn [p] != '/'));
   fn [p] = '\0';
   return fn;
}

char *FnExt (char *ext, char *fn)
// return just the .ext (without path/name)
{ TStr t;
   StrCp (t, fn);
  ubyt4 ln = StrLn (Fn2Name (t));
   StrCp (ext, (ln < StrLn (fn)) ? (& fn [ln+1]) : CC(""));
   return ext;
}

char *FnName (char *nm, char *fn)
// return just name.ext (without path)
{ TStr t;
   StrCp (t, fn);
  ubyt4 ln = StrLn (Fn2Path (t));
   StrCp (nm,  (ln < StrLn (fn)) ? (ln ? & fn [ln+1] : fn) : CC(""));
   return nm;
}

char *FnFix (char *fn)
{ char *p;
   for (p = fn; *p; p++)
      if ((*p <= ' ') || (*p >  '~') ||
          (*p == '*') || (*p == '?') || (*p == '/') || (*p == '\\') ||
          (*p == ':') || (*p == '"') || (*p == '>') || (*p == '<' ) ||
          (*p == '|'))
         *p = '_';
   for (; *fn && (fn [StrLn (fn)-1] == '_');)  StrAp (fn, CC(""), 1);
   return fn;
}


//______________________________________________________________________________
static ubyt4 RandSeed;                 // copied from ms crt src ;)

void  RandInit ()
{ timespec ts;
   clock_gettime (CLOCK_REALTIME, & ts);   RandSeed = ts.tv_nsec;
}

ubyt2 Rand ()                          // return pseudo-random num 0-32767
{  return (ubyt2)(((RandSeed = RandSeed * 214013L + 2531011L)>>16) & RANDMAX);
}

ubyt2 Rnd (ubyt2 n)                    // return 0..n-1
{ ubyt2 r = Rand ();   return (ubyt2)(((r?(r-1):0) * n) / RANDMAX);  }


//______________________________________________________________________________
#define CUTOFF  (8)
#define STKSIZ  (8*sizeof(void*) - 2)

void swap (char *a, char *b, size_t w)
{ char tmp;
   if (a != b)  while (w--)  {tmp = *a;   *a++ = *b;   *b++ = tmp;}
}

void shortsort (char *lo, char *hi, size_t w, int (*comp)(void *, void *) )
{ char *p, *max;
   while (hi > lo) {
      max = lo;
      for (p = lo+w; p <= hi; p += w)  {if (comp (p, max) > 0)  max = p;}
      swap (max, hi, w);
      hi -= w;
   }
}

void Sort (void *base, size_t num, size_t width, int (*comp)(void *, void *) )
{ char  *lo, *hi;
  char  *mid;
  char  *loGuy, *hiGuy;
  size_t size;
  char  *loStk [STKSIZ], *hiStk [STKSIZ];
  int    stkPtr;
   if (num < 2 || width == 0)  return;
   stkPtr = 0;
   lo = (char *) base;
   hi = (char *) base + width * (num-1);
recurse:
   size = (hi - lo) / width + 1;

   if (size <= CUTOFF)  shortsort (lo, hi, width, comp);
   else {
      mid = lo + (size / 2) * width;
      if (comp (lo, mid) > 0)  swap (lo,  mid, width);
      if (comp (lo, hi ) > 0)  swap (lo,  hi,  width);
      if (comp (mid, hi) > 0)  swap (mid, hi,  width);
      loGuy = lo;
      hiGuy = hi;
      for (;;) {
         if (mid > loGuy)
            {do loGuy += width;  while (loGuy < mid && comp (loGuy, mid) <= 0);}
         if (mid <= loGuy)
            {do loGuy += width;  while (loGuy <= hi && comp (loGuy, mid) <= 0);}
         do hiGuy -= width;  while (hiGuy > mid && comp(hiGuy, mid) > 0);
         if (hiGuy < loGuy)  break;
         swap (loGuy, hiGuy, width);
         if (mid == hiGuy)  mid = loGuy;
      }
      hiGuy += width;
      if (mid < hiGuy)
         {do hiGuy -= width;  while (hiGuy > mid && comp (hiGuy, mid) == 0);}
      if (mid >= hiGuy)
         {do hiGuy -= width;  while (hiGuy > lo  && comp (hiGuy, mid) == 0);}
      if (hiGuy - lo >= hi - loGuy) {
         if (lo < hiGuy) {
            loStk [stkPtr  ] = lo;
            hiStk [stkPtr++] = hiGuy;
         }
         if (loGuy < hi) {lo = loGuy;   goto recurse;}
      }
      else {
         if (loGuy < hi) {
            loStk [stkPtr  ] = loGuy;
            hiStk [stkPtr++] = hi;
         }
         if (lo < hiGuy) {hi = hiGuy;   goto recurse;}
      }
   }
   --stkPtr;
   if (stkPtr >= 0) {
      lo = loStk [stkPtr];
      hi = hiStk [stkPtr];
      goto recurse;
   }
}


//______________________________________________________________________________
char *Now (char *s)
{ time_t t = time (nullptr);
   strftime (s, 20, "%Y%m%d.%H%M%S.%a", localtime (& t));
   return s;
}

char *NowMS (char *s)                  // current time in msec for debuggin
{ int  msec;
  TStr buf;
  struct timeval tv;
  struct tm     *tm;
   gettimeofday (& tv, NULL);
   msec = lrint (tv.tv_usec / 1000.0);      // Round to nearest msec
   if (msec >= 1000)  {msec -= 1000;   tv.tv_sec++;}
   tm = localtime (& tv.tv_sec);
   strftime (buf, sizeof (buf), "%d.%a.%H:%M:%S", tm);
   return StrFmt (s, "`s.`03d", buf, msec);
}


bool File::Copy (char *from, char *to)
{ int ff, ft;
  BStr tod;
  Path d;
TRC("File::Copy from='`s' to='`s'", from, to);
   StrCp (tod, to);   Fn2Path (tod);   d.Make (tod);
   if (! (ff = open (from, O_RDONLY, 0)))
      {DBG ("Copy from=`s open failed", from);             return false;}
   if (! (ft = open (to,   O_WRONLY | O_CREAT, 0644)))
      {DBG ("Copy to=`s open failed", to);   close (ff);   return false;}
  struct stat st;
   fstat (ff, & st);
   sendfile64 (ft, ff, 0, st.st_size);   close (ff);   close (ft);
   return true;
}

bool File::ReNm (char *from, char *to)
{ TStr tod;
  Path d;
TRC("File::ReNm from='`s' to='`s'", from, to);
   StrCp (tod, to);   Fn2Path (tod);   d.Make (tod);
   if (! rename (from, to))  return true;
DBG("rename(fr=`s to=`s) error:`s\n", from, to, strerror (errno));
   return false;
}

bool File::Kill (char *fn)
{
TRC("File::Kill `s", fn);
   if (! remove (fn))  return true;
DBG("File::Kill remove(`s) error:`s\n", fn, strerror (errno));
   return false;
}


bool Path::Make (char *dir, ubyt2 perm)
// make dir (and everything up to it)
{ TStr path;
  FDir d;
  bool got;
  ubyt2 p;
TRC("Path::Make `s", dir);
   StrCp (path, dir);
// see if the dir is there.  if not, trim it down and see if THAT's there, etc
   do {
      got = d.Got (path);
      if (! got) {
         Fn2Path (path);
         if (*path == '\0') {
DBG("Path::Make couldn't get path root for `s", dir);
            return false;
         }
      }
   } while (! got);
// ok, we got SOMEthing, so make any further dirs needed
   while (StrLn (path) < StrLn (dir)) {
   // tack on next dir increment
      StrAp (path, CC("/"));
      for (p = SC(ubyt2,StrLn (path));  dir [p] && (dir [p] != '/');  p++)
         path [p] = dir [p];
      path [p] = '\0';
TRC(" mkdir `s", path);
      if (mkdir (path, SC(int,perm))) {   // finally MAKE it
DBG("Path::Make mkdir died: `s", strerror (errno));
         return false;
      }
   }
   return true;
}


bool Path::Kill (char *dir)
// kill dir (and EVERything in it)
{ TStr fn;
  File f;
  FDir d;
  char df;
TRC("Path::Kill `s", dir);
   if ( (*dir == '\0') || (! StrCm (dir, CC("/"))) ) {
DBG("Path::Kill  NOT gonna kill your whole hard drive...");
      return false;
   }
// recursively kill files first cuz can't kill dirs till they're ALL gone
   if ((df = d.Open (fn, dir))) {
      do {
//DBG(" bye `s,`c", fn,df);
         if (! ((df == 'd') ? Kill (fn) : f.Kill (fn)) ) {
DBG("Path::Kill `s died early :(", dir);
            d.Shut ();
            return false;
         }
         df = d.Next (fn);
//DBG(" Next=`s,`c", fn,df);
      } while (df);
      d.Shut ();
   }
// NOW we can kill the dir
   rmdir (dir);
TRC(" rmdir `s", dir);
   return true;
}


bool Path::Copy (char *from, char *to)
// copy dir in FROM (and everything in it) to TO
{ TStr src, dst;
  File f;
  FDir d;
  char df;
TRC("Path::Copy `s `s", from, to);
   StrCp (src, from);
   if (! d.Got (from)) {
DBG("Path::Copy  from dir not there");
      return false;
   }
   Make (to);                          // make dst path in case it ain't there
// do every non . or .. dir and every file
   if ((df = d.Open (src, from))) {
      do {
         StrFmt (dst, "`s`s`s", to, to [StrLn (to)-1] == '/' ? "" : "/",
                                 & src [StrLn (from)+1]);
         if (! ((df == 'd') ? Copy (src, dst) : f.Copy (src, dst)) ) {
            d.Shut ();
DBG("Path::Copy  :(");
            return false;
         }
      } while ((df = d.Next (src)));
      d.Shut ();
   }
   return true;
}


void File::DoDir (char *dir, void *ptr, FDoDirFunc func, char *skip)
// find any files and pass em to callback func
{ FDir d;
  char df;
  TStr fn;
  bool naw = false;
//DBG("File::DoDir `s", dir);
   if      (func (ptr, 'd', dir))  naw = true;
   else if ((df = d.Open (fn, dir))) {
      do {
         if (df == 'f')  {if      (skip && (! StrCm (fn, skip)))  naw = true;
                          else if (func (ptr, 'f', fn))           naw = true;}
      }  while ((! naw) && (df = d.Next (fn)));
      d.Shut ();
   }
   if (! naw) {
      df  = d.Open (fn, dir);
      do {
         if (df == 'd')  DoDir (fn, ptr, func, skip);
      }  while ((df = d.Next (fn)));
      d.Shut ();
   }
   func (ptr, 'x', dir);
}


char *File::DoText (char *name, void *ptr, FDoTextFunc func, ubyt4 maxlen)
// load and parse a text file given a parsin func - pretty OS specific cuzu \n
{ char *msg = nullptr;
  ubyt4 line = 0;
  ubyt2 len, p = 0, l, ls;
  bool  gotLn;
  BStr  buf;
  static BStr err;
   if (! Open (name, "r"))
      return  StrFmt (err, "File::DoText  Couldn't read file '`s'", name);
   while ((len = p + (ubyt2) Get (& buf [p], sizeof (buf) - p)) &&
          (msg == nullptr)) {
      p = 0;
      do {
         gotLn = false;
         for (l = 0;  p+l < len;  l++)  if (buf [p+l] == '\n')  break;
         if (p+l < len) {
            if (l > maxlen)   return CC("Not a text file (reclen >max)");

            gotLn = true;
            ls = l;              if (buf [p+l-1] == '\r')  ls--;
            buf [p+ls] = '\0';   msg = func (& buf [p], ls, line++, ptr);
            p += (l+1);
         }
      } while (gotLn && (msg == nullptr));
      if      (p == 0)   {msg = CC("Not a text file (reclen>4k)");   break;}
      else if (p < len)  {MemCp (buf, & buf [p], len-p);   p = len-p;}
      else  p = 0;
   }
   Shut ();
   return msg;
}

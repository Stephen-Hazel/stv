// os.cpp
// 06/04/98 new;  03/04/04 rewrite to toss exceptions;  12/01/20 win=>linux

#include "os.h"
#include <math.h>


void MemSet (void *dst, ubyte c, ulong len)
{ ubyte *p = SC(ubyte *,dst);
   while (len--)  *p++ = c;
}

void MemCp (void *dst, void *src, ulong len)
{ ubyte *d = SC(ubyte *,dst), *s = SC(ubyte *,src);
   if ((! s) || (! d))  return;
   if ((d > s) && (d < s+len)) {
        d = SC(ubyte *,dst) + len - 1;
        s = SC(ubyte *,src) + len - 1;
        while (len--)  *d-- = *s--;
   }
   else while (len--)  *d++ = *s++;
}

void *MemCh (void *mem, ubyte c, ulong len)
{ ubyte *p = SC(ubyte *,mem);
   while (len--)  if (*p++ == c)  return SC(void *,--p);
   return nullptr;
}

char *MemSt (void *big, char *sm, ulong len, char x)
{ char *cp;
  ulong ln = StrLn (sm);
//DBG("MemSt big=`08x sm=`s len=`d, x=`c", big, sm, len, x);
   if (! ln)  return SC(char *,big);
   for (cp = SC(char *,big);  len >= ln;  cp++, len--)
      if (! MemCm (cp, sm, ln, x))  return cp;
   return nullptr;
}

slong MemCm (char *s1, char *s2, ulong len, char x)
{ char  c1, c2;
  slong rc;
   while (len--) {
      c1 = *s1++;   c2 = *s2++;
      if (! x)  {c1 = CHDN (c1);   c2 = CHDN (c2);}
      if ((rc = c1 - c2))  return rc;
   }
   return 0;
}


//______________________________________________________________________________
ulong StrLn (char *s)                  // guard against NULLs too :/
{ ulong len = 0;   if (s) while (*s++) len++;   return len;}

char *StrCp (char *dst, char *src)     // slow n safe...
{ ulong len = StrLn (src);   MemCp (dst, src, len+1);   return dst;}

char *StrAp (char *dst, char *src, ulong ofs)
{ ulong ln = StrLn (dst);
   if (ofs > ln)  ofs = ln;
   StrCp (& dst [ln-ofs], src);  return dst;
}

char *StrCh (char *str, char c)
{  return SC(char *,MemCh (str, SC(ubyte,c), StrLn (str)));  }

char *StrSt (char *big, char *sm, char x)
{ char *cp = big, *s1, *s2;
   if (cp == nullptr)  DBG ("StrSt arg 1 (big str) is NULL");
   if (sm == nullptr)  DBG ("StrSt arg 2 (little str) is NULL");
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

slong StrCm (char *s1, char *s2, char x)
{ char  c1, c2;
  slong rc;
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


//______________________________________________________________________________
// int convert string ops
char *Int2Str (slong i, char *buf12, char x)
{ char *so, dg;
  bool  ng = false;
  uword bs = (x == 'x') ? 16 : 10;
  ulong j;
   so = & buf12 [11];   *so = '\0';
   if (i == 0) *(--so) = '0';
   else {
      ng = false;
      if ((x != 'x') && (i < 0)) {ng = true;   j = SC(ulong,-i);}
      else                                     j = SC(ulong, i);
      while (j) {
         dg = SC(char,j % bs);   dg += ((dg > 9) ? ('A'-10) : '0');
         *(--so) = dg;   j /= bs;
      }
      if (ng)  *(--so) = '-';
   }
   return so;
}

char *Unt2Str (ulong i, char *buf12)
{ char *so;
   so = & buf12 [11];   *so = '\0';
   if (i == 0)  *(--so) = '0';
   else         while (i)  {*(--so) = (i % 10) + '0';   i /= 10;}
   return so;
}

slong Str2Int (char *s, char **p)
{ slong i = 0;
  bool  ng = false;
   while ((*s == ' ') || (*s == '\t'))  s++;
   if    (*s == '-')  {s++;  ng = true;}
   while ((*s >= '0') && (*s <= '9'))  i = i * 10 + (*(s++) - '0');
   if (p)  *p = SC(char *,s);
   return ng ? -i : i;
}

char *StrFmtX (char *so, char const *fmti, va_list va)  // sprintf replacement
{ char *s, c, fc, ju, buf [12], *fmt = CC(fmti);
  ulong ln, ln2, ln3, u;
  slong i;
  bool  b;
   *so = '\0';
   if (fmt == nullptr)  return so;
   while (*fmt) {
      if (*fmt != '`')                 // fmt HAS to be ascii
           {ln = StrLn (so);   so [ln] = *fmt++;   so [ln+1] = '\0';}
      else {                           // init to stringize dat arg
         ++fmt;   ju = '\0';   s = buf;   ln = ln3 = 0;
         if (StrCh (CC("<>0"), *fmt)) {
            ju = *fmt++;   ln = SC(ulong,Str2Int (fmt, & fmt));
            if (! ln)  {DBG("StrFmtX: bad args (len)");   return so;}
         }
         switch (fc = *fmt++) {        // ^gots ta pad
            case '`':
               s = CC("`");            // allow `` => `
               break;
            case 's':
               s =          va_arg (va, char *);
               break;
            case 'u':
               u = SC(ulong,va_arg (va, unsigned int));
               s = Unt2Str (u, buf);
               break;
            case 'd':  case 'x':
               i = SC(slong,va_arg (va, int));   s = Int2Str (i, buf, fc);
               if ((fc == 'x') && (! ju)) {      // square up the hex ln
                  ln = StrLn (s);
                  if ((ln > 1) && (ln % 2))  {ju = '0';   ln++;}
               }
               break;
            case 'c':
               c = SC(char,va_arg (va, int));   buf [0] = c;   buf [1] = '\0';
               break;
            case 'b':
               b =         va_arg (va, int) ? true : false;
               StrCp (buf, CC(b ? "t" : "f"));
               break;
            default:
               DBG("StrFmtX: bad args (fmt=`s fc=`d)", fmti, fc);   return so;
         }
         if ( ju && ((ln2 = StrLn (s)) < ln) ) {
            ln3 = ln - ln2;
            if (ju != '<')
                   {if (ju != '0')  ju = ' ';
                    ln = StrLn (so);   MemSet (& so [ln], SC(ubyte,ju), ln3);
                    so [ln+ln3] = '\0';
                    ln3 = 0;}
         }
         StrAp (so, s ? s : CC("NULL"));
         if (ln3)  {ln = StrLn (so);   MemSet (& so [ln], ' ', ln3);
                    so [ln+ln3] = '\0';}
      }
   }
   return so;
}

char *StrFmt (char *so, char const *fmt, ...)     // sprintf replacement
{ va_list va;
   va_start (va, fmt);   StrFmtX (so, fmt, va);   va_end (va);   return so;
}


void DbgX (char *s)
{ TStr buf;
  int  msec;
  struct tm *tm;
  struct timeval tv;
   gettimeofday (& tv, NULL);
   msec = lrint (tv.tv_usec / 1000.0);      // Round to nearest millisec
   if (msec >= 1000)  {msec -= 1000;   tv.tv_sec++;}
   tm = localtime (& tv.tv_sec);
   strftime (buf, sizeof (buf), "%a.%H:%M:%S", tm);
   fprintf (stderr, "%s.%03d %s\n", buf, msec, s);

// fprintf (stderr, "%s\n", s);  fflush (stderr);
/*
  char  s2 [16000];
  ulong ln, thr, i;
  uword ind;
  ubyte x;
   ln = StrLn (s);
   StrFmt (s2, "`d ", thr = ::GetCurrentThreadId ());
   for (x = 0;  x < BITS (DbgThr);  x++)  if ((DbgThr [x] == 0) ||
                                              (DbgThr [x] == thr))  break;
   if (x >= BITS (DbgThr))  Die ("too many threads to debug");
   DbgThr [x] = thr;   ind = DbgInd [x];
   if (*s == '}')  ind--;
   for (i = 0;  i < ind;  i++)  StrAp (s2, "  ");
   if ((StrLn (s2) + ln) < sizeof (s2))  StrAp (s2, s);
   else {                              // gotta chop
      MemCp (& s2 [StrLn (s2)], s, sizeof (s2)-1-StrLn (s2));
      s2 [sizeof (s2)-1] = '\0';
   }
#ifndef DBG_F
   ::OutputDebugString (StrCvt (su, s2, ' ', BITS (su)));
#else
   MemCp (QDbg [NDbg], s2, sizeof (TStr));  // faster to a memory q
   QDbg [NDbg++][sizeof (TStr)-1] = '\0';
   if (NDbg == BITS (QDbg))  {NDbg = 0;   WDbg = true;}
   if (*s == '\0') {
      if (WDbg)  for (i = NDbg;  i < BITS (QDbg);  i++)
         ::OutputDebugString (StrCvt (su, QDbg [i]));
      for            (i = 0;     i < NDbg;         i++)
         ::OutputDebugString (StrCvt (su, QDbg [i]));
      NDbg = 0;   WDbg = false;
   }
#endif
   if (*s == '{')  ind++;
   DbgInd [x] = ind;
*/
}


inline void DBG (char const *fmt, ...)        // printf-y debugging
{ va_list va;
  PStr    out;
   va_start (va, fmt);   StrFmtX (out, fmt, va);   va_end (va);   DbgX (out);
}


//______________________________________________________________________________
ulong LinePos (char *s, ulong ln)
{ ulong p = 0;
  char *n;
   while (ln--) {
      if ((n = StrCh (& s [p], '\n')) == nullptr)  return 0;
      p += SC(ulong,n - & s [p] + 1);
   }
   return p;
}

ulong NextLn (char *str, char *buf, ulong len, ulong p)
{ char *ch;
  ulong ln;
   *str = '\0';
   if ((ch = StrCh (& buf [p], '\n'))) {
      MemCp (str, & buf [p], ln = SC(ulong,ch - & buf [p]));
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
     ulong ln = StrLn (str);
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

ulong PosInZZ (char *t, char *s, char x)
// \0 sep'd, \0\0 term'd list
{ ulong p = 1;
   while (*s) {
      if (StrCm (s, t, x) == 0)  return p;
      p++;   s += (StrLn (s) + 1);
   }
   return 0;
}

ulong PosInWZ (char *t, char *s, uword w, char x)
// w sep'd, \0 term'd list
{ ulong p = 1;
   while (*s) {
      if (StrCm (s, t, x) == 0)  return p;
      p++;   s += w;
   }
   return 0;
}

ulong PosInWH (char *t, char *s, uword w, ulong h, char x)
// w sep'd, h term'd list
{ ulong p = 1;
   while (h--) {
      if (StrCm (s, t, x) == 0)  return p;
      p++;   s += w;
   }
   return 0;
}


//______________________________________________________________________________
// filename string ops
slong PathCmp (char *f1, char *f2)
// sort by just dir, in case different depths, then fn
{ TStr  b1, b2;
  slong cmp;
   StrCp (b1, f1);   Fn2Path (b1);
   StrCp (b2, f2);   Fn2Path (b2);
   return (cmp = StrCm (b1, b2)) ? cmp : StrCm (f1, f2);
}

char *Fn2Name (char *fn)
// kill .ext:  strip tail end of fn till you hit a .
{ ulong p;
   if ((p = StrLn (fn)) == 0)        return fn;
   do --p;  while (p && (fn [p] != '.') && (fn [p] != '/'));
   if ((p == 0) || (fn [p] == '/'))  return fn;
   fn [p] = '\0';
   return fn;
}

char *Fn2Path (char *fn)
// kill name.ext:  strip tail end of fn till you hit a / n lose it
{ ulong p;
   if ((p = StrLn (fn)) == 0)  return fn;
   do --p;  while (p && (fn [p] != '/'));
   fn [p] = '\0';
   return fn;
}

char *FnExt (char *ext, char *fn)
// return just the .ext (without path/name)
{ TStr t;
   StrCp (t, fn);
  ulong ln = StrLn (Fn2Name (t));
   StrCp (ext, (ln < StrLn (fn)) ? (& fn [ln+1]) : CC(""));
   return ext;
}

char *FnName (char *nm, char *fn)
// return just name.ext (without path)
{ TStr t;
   StrCp (t, fn);
  ulong ln = StrLn (Fn2Path (t));
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
bool File::Kill (char *fn)
{
DBG("File::Kill `s", fn);
   if (remove (fn)) {
DBG("remove() error:`s\n", strerror (errno));
      return false;
   }
   return true;
}


bool File::ReNm (char *from, char *to)
{
DBG("File::ReNm from='`s' to='`s'", from, to);
   if (rename (from, to)) {
DBG("rename() error:`s\n", strerror (errno));
      return false;
   }
   return true;
}


bool File::Copy (char *from, char *to)
{
DBG("File::Copy from='`s' to='`s'", from, to);
  int ff = open (from, O_RDONLY, 0);
  int ft = open (to,   O_WRONLY | O_CREAT, 0644);
  struct stat st;
   fstat (ff, & st);
   sendfile64 (ft, ff, 0, st.st_size);   close (ff);   close (ft);
   return true;
}


bool File::PathMake (char *fn)
// make dir in fn (and everything up to it)
{ TStr path;
  bool got;
  uword p;
DBG("File::PathMake `s", fn);
   StrCp (path, fn);
// see if the dir is there.  if not, trim it down and see if THAT's there, etc
   do {
      got = PathGot (path);
      if (! got) {  // trim trailing stuff off till you get a \ or a :
         Fn2Path (path);
         if (*path == '\0') {
DBG("File::PathMake couldn't get path root for `s", fn);
            return false;
         }
      }
   } while (! got);
// ok, we got SOMEthing, so make any further dirs needed
   for (;;) {
   // tack on next dir increment
      p = SC(uword,StrLn (path));
      path [p] = fn [p];  path [p+1] = '\0';     // cuzu when you've got C:
      if (StrLn (path) >= StrLn (fn))  break;    // YAY :)

      for (p = SC(uword,StrLn (path));  fn [p] && (fn [p] != '/');  p++)
         path [p] = fn [p];
      path [p] = '\0';
   // MAKE it
      if (! mkdir (path, 0755)) {
DBG("File::PathMake - mkdir died: `s", path);
         return false;
      }
DBG("File::PathMake - mkdir `s", path);
   }
   return true;
}


bool File::PathKill (char *dir)
// kill dir (and EVERything in it)
{ TStr fn;
  FDir d;
  char df;
DBG("File::PathKill `s", dir);
   if ( (*dir == '\0') || (! StrCm (dir, CC("/"))) ) {
DBG("File::PathKill  NOT gonna kill your whole hard drive...");
      return false;
   }
// recursively kill files first cuz can't kill dirs till they're ALL gone
   if ((df = d.Open (fn, dir))) {
      do
         if (! ((df == 'd') ? PathKill (fn) : Kill (fn)) ) {
DBG("File::PathKill `s died early :(", dir);
            d.Shut ();
            return false;
         }
      while ((df = d.Next (fn)));
      d.Shut ();
   }
// NOW we can kill the dir
   rmdir (dir);
DBG (" rmdir(`s)", dir);
   return true;
}


bool File::PathCopy (char *from, char *to)
// copy dir in FROM (and everything in it) to TO
{ TStr src, dst;
  FDir d;
  char df;
DBG("File::PathCopy `s `s", from, to);
   StrCp (src, from);
   if (! (df = d.Open (src, from))) {
DBG("File::PathCopy  from dir not therez");
      return false;
   }
   PathMake (to);                      // make dst path in case it ain't there
// do every non . or .. dir and every file
   do {
      StrFmt (dst, "`s`s`s", to, to [StrLn (to)-1] == '/' ? "" : "/",
                              & src [StrLn (from)+1]);
      if (! ((df == 'd') ? PathCopy (src, dst) : Copy (src, dst)) ) {
         d.Shut ();
DBG("File::PathCopy  :(");
         return false;
      }
   } while ((df = d.Next (src)));
   d.Shut ();
   return true;
}


void File::DoDir (char *dir, void *ptr, FDoDirFunc func, char *skip)
// find any files and pass em to callback func
{ FDir d;
  char df;
  TStr fn;
  bool naw = false;
//DBG("DoDir `s", dir);
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


char *File::DoText (char *name, void *ptr, FDoTextFunc func, ulong maxlen)
// load and parse a text file given a parsin func - pretty OS specific cuzu \n
{ char *msg = nullptr, c;
  ulong line = 0;
  uword len, p = 0, l, ls;
  bool  gotLn;
  PStr  buf;
  static PStr err;
   if (! Open (name, "r"))
      return  StrFmt (err, "File::DoText  Couldn't read file '`s'", name);
   while ((len = p + (uword) Get (& buf [p], sizeof (buf) - p)) &&
          (msg == nullptr)) {
      p = 0;
      do {
         gotLn = false;
         for (l = 0;  p+l < len;  l++)  if ((c = buf [p+l]) == '\n')  break;
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

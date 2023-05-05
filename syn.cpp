// syn.cpp - easy(ish) softsynth based on plain jane .WAVs

#include "syn.h"

AuODef AuO;

TStr   WavFn [MAX_SAMP*2];
Sample TSmp  [MAX_SAMP];

char *R2Str (real f, char *s)
{ bool neg;
  int  i, ln;
  TStr t;
   neg = (f < 0.) ? true : false;   if (neg) f = -f;
   i = (int)f;   f -= (real)i;
   StrFmt (t, "`09d", (int)(f*1000000000.));
   while ((ln = StrLn (t)))  {if (t [--ln] == '0')  t [ln] = '\0';  else break;}
   return StrFmt (s, "`s`d`s`s", neg?"-":"", i, *t?".":"", t);
}

//------------------------------------------------------------------------------
real     Interp [MAX_INTERP][7];       // interpolation coefficients
void InitInterp ()
{ real v, iShf;
   for (ubyte i = 0; i < 7; i++)
      for (ubyt2 j = 0; j < MAX_INTERP; j++) {
         iShf = (real)i - (7. / 2.) + (real)j / (real)MAX_INTERP;
         if (fabs (iShf) <= 0.000001)  v = 1.;
         else {v = sin (iShf * M_PI) / (M_PI * iShf);
               v *= 0.5 * (1. + cos (2.*M_PI * iShf / 7.));}
         Interp [MAX_INTERP-1 - j][i] = v;
      }
}

real     Dither [2][MAX_DITHER];       // per l/r channel
void InitDither (void)                 // rand real btw -.999 and +.999
{ real  d, dp;
  sbyt4 c, i;
   for (c = 0; c < 2; c++) {
      dp = 0;
      for (i = 0; i < MAX_DITHER-1; i++) {
         d             = rand () / (real)RAND_MAX - 0.5f;
         Dither [c][i] = d - dp;
         dp            = d;
      }
      Dither [c][MAX_DITHER-1] = 0 - dp;
   }
}
                                       // conversion tables
real Cnv_ct2hz [1200];                 // cents  2 hz
real Cnv_pan   [127];                  // 128    2 pan

real Ct2Hz (real ct)
{  if (ct <     0.) return (real)    1.;
   if (ct <   900.) return (real)  6.875 * Cnv_ct2hz [(sbyt4)(ct +   300)];
   if (ct <  2100.) return (real)  13.75 * Cnv_ct2hz [(sbyt4)(ct -   900)];
   if (ct <  3300.) return (real)   27.5 * Cnv_ct2hz [(sbyt4)(ct -  2100)];
   if (ct <  4500.) return (real)   55.  * Cnv_ct2hz [(sbyt4)(ct -  3300)];
   if (ct <  5700.) return (real)  110.  * Cnv_ct2hz [(sbyt4)(ct -  4500)];
   if (ct <  6900.) return (real)  220.  * Cnv_ct2hz [(sbyt4)(ct -  5700)];
   if (ct <  8100.) return (real)  440.  * Cnv_ct2hz [(sbyt4)(ct -  6900)];
   if (ct <  9300.) return (real)  880.  * Cnv_ct2hz [(sbyt4)(ct -  8100)];
   if (ct < 10500.) return (real) 1760.  * Cnv_ct2hz [(sbyt4)(ct -  9300)];
   if (ct < 11700.) return (real) 3520.  * Cnv_ct2hz [(sbyt4)(ct - 10500)];
   if (ct < 12900.) return (real) 7040.  * Cnv_ct2hz [(sbyt4)(ct - 11700)];
   if (ct < 14100.) return (real)14080.  * Cnv_ct2hz [(sbyt4)(ct - 12900)];
   return                  (real)    1.;
}

real Pan (ubyte c, bool lft)
{  if (c ==  0)  c =   1;              // so -64 => -63
   if (c > 127)  c = 127;              // limit at -63 .. 63
   c--;   if (lft)  c = 126 - c;
   return Cnv_pan [c-1];
}


void InitLookup (void)
{ sbyt4 i;
  real  x;
   for (i = 0;  i < BITS (Cnv_ct2hz);  i++)
      Cnv_ct2hz [i] = (real) pow (2., (real) i / 1200.);
   x = M_PI/2. / (BITS (Cnv_pan) - 1.);
   for (i = 0;  i < BITS (Cnv_pan);  i++)  Cnv_pan [i] = (real) sin (i * x);
//TStr ts;
//for (i = 0; i < BITS (Cnv_pan); i++)
//DBG("pan `d `s", i, R2Str (Cnv_pan[i],ts));
   InitInterp ();   InitDither ();
}

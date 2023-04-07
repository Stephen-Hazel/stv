// synFx.h - filter,fx classes fer syn

class LPF {                            // low pass filter
   bool  init;
   ulong inc;
   real  cut,  res,                    // cutoff frequency, resonance params
        pCut, pRes, gain,              // prev cut;  gain derived from res
         a1,    a2,    b1,    b2,
         a1Inc, a2Inc, b1Inc, b2Inc,
         hist1, hist2;                 // buffer past calcs

public:
   void Res (real r)                   // resonance (0-960 cB => 0-96 dB)
   {  if ((fabs (res - pRes) <= 0.01))  return;
     real x = r / 10.;                 // x is q_dB
      if      (x <  0.)  x =  0.;
      else if (x > 96.)  x = 96.;
      x -= 3.01;
      res  = pow (10., x / 20.);
      gain = 1. / sqrt (res);
      pRes = r;
      pCut = -1.;                      // force a cut re-Calc, too
   }

   void Cut (real c)                   // cutoff frequency in absolute cents
   // limit cut range n convert from cents to hz  (called once per buffer)
   // rebuild coefficients if necessary           (1500-13500 ct => 20-20K hz)
   {  cut = Ct2Hz (c);                         // (8000-13500 usable range tho)
      if      (cut > 0.45 * AuO.frq)  cut = 0.45 * AuO.frq;
      else if (cut < 5.)              cut = 5.;

   // can we skip the grindin'...?
      if ((fabs (cut - pCut) <= 0.01))  return;

     real omega = 2.*PI * (cut / AuO.frq);
     real sinO  = sin (omega);
     real cosO  = cos (omega);
     real alpha = sinO / (2. * res);
     real a0Inv = 1. / (1. + alpha);
     real a1New = -2. * cosO   * a0Inv;
     real a2New = (1. - alpha) * a0Inv;
     real b1New = (1. - cosO)  * a0Inv * gain;
     real b2New = b1New * 0.5;
//TStr t1,t2,t3,t4,t5,t6,t7,t8,t9,ts;
//DBG("cut=`s omega=`s sinO=`s cosO=`s alpha=`s "
//"a0Inv=`s a1=`s a2=`s b1=`s b2=`s",
//R2Str(cut,t1),R2Str(omega,t2),R2Str(sinO,t3),
//R2Str(cosO,t4),R2Str(alpha,t5),R2Str(a0Inv,ts),
//R2Str(a1,t6),R2Str(a2,t7),R2Str(b1,t8),R2Str(b2,t9));
      if (init)  {init = false;   inc = 0;   a1 = a1New;   a2 = a2New;
                                             b1 = b1New;   b2 = b2New;}
      else {
         inc = AuO.bufLn;
         a1Inc = (a1New-a1) / AuO.bufLn;   a2Inc = (a2New-a2) / AuO.bufLn;
         b2Inc = (b2New-b2) / AuO.bufLn;   b1Inc = (b1New-b1) / AuO.bufLn;
      }
      pCut = cut;
   }

   void Init ()
   {  init = true;   pCut = pRes = -1.;   hist1 = hist2 = 0.;   Res (0.);  }

   real Cvt (real smp)                 // actually DO the filterin'
   {  if (fabs (hist1) < 1e-20)  hist1 = 0.;     // (called once per sample)
     real centerNode = smp - a1*hist1 - a2*hist2;
     real x =                b1*hist1 + b2*(centerNode + hist2);
      hist2 = hist1;   hist1 = centerNode;
      if (inc)  {inc--;   a1 += a1Inc;   a2 += a2Inc;
                          b1 += b1Inc;   b2 += b2Inc;}
//TStr t1,t2,t3,t4,t5,t6,t7,t8,t9;
//DBG("smp=`s x=`s centerNode=`s hist1=`s hist2=`s "
//"a1=`s a2=`s b1=`s b2=`s",
//R2Str(smp,t1),R2Str(x,t2),R2Str(centerNode,t3),
//R2Str(hist1,t4),R2Str(hist2,t5),R2Str(a1,t6),R2Str(a2,t7),
//R2Str(b1,t8),R2Str(b2,t9));
      return x;
   }
};


//------------------------------------------------------------------------------
#define MAX_BLOCK  (99)                // max # blocks for chorus
#define MIN_SPEED  (0.29)              // speed range (in Hz)
#define MAX_SPEED  (5.)

#define MAX_SAMPLES_LN2            (12)
#define MAX_SAMPLES                (1 << (MAX_SAMPLES_LN2-1))
#define MAX_SAMPLES_ANDMASK        (MAX_SAMPLES-1)
#define INTERP_SUBSAMPLES_LN2      (8)  // ...interpolation
#define INTERP_SUBSAMPLES          (1 << (INTERP_SUBSAMPLES_LN2-1))
#define INTERP_SUBSAMPLES_ANDMASK  (INTERP_SUBSAMPLES-1)
#define INTERP_SAMPLES             (5)

class Chorus {
public:
   slong  blocks;                      // number of blocks used 0-MAX_BLOCK(=99)
   real   level, speed, depth;         // speed in Hz; depth in msec
   slong  type;                        // 0=sin 1=tri
   slong  modPerSamples;               // modulation period samples
   slong  counter;
   real  *chorusbuf;
   slong *lookup;
   slong  phase [MAX_BLOCK];
   real   sinc_table [INTERP_SAMPLES][INTERP_SUBSAMPLES];

   void sine     (slong *buf, slong len, slong depth)
   { slong i;
     real  val;
      for (i = 0;  i < len;  i++) {
         val = sin ((real) i / (real)len * 2.*PI);
         buf [i]  = (slong)((1. + val) * (real) depth / 2. *
                                         (real) INTERP_SUBSAMPLES);
         buf [i] -= 3* MAX_SAMPLES *            INTERP_SUBSAMPLES;
      }
   }

   void triangle (slong *buf, slong len, slong depth)
   { slong i = 0, ii = len-1;
     real  val, val2;
      while (i <= ii) {
         val  = i * 2. / len * (real)depth *     (real)INTERP_SUBSAMPLES;
         val2 = (slong)(val + 0.5) - 3 * MAX_SAMPLES * INTERP_SUBSAMPLES;
         buf [i++]  = (slong) val2;
         buf [ii--] = (slong) val2;
      }
   }

   void Update ()
   // reset stuff for new params
   { slong i, modDepthSamps;
//TStr s1,s2,s3;
//DBG("chorus Update blocks=`d level=`s speed=`s depthh=`s type=`d",
//blocks, R2Str(level,s1), R2Str(speed,s2), R2Str(depth,s3), type);
      modPerSamples = (slong)(AuO.frq / speed);
      modDepthSamps = (slong)(depth / 1000. * AuO.frq);
      if (modDepthSamps > MAX_SAMPLES)  modDepthSamps = MAX_SAMPLES;
      if (type == 1)  triangle (lookup, modPerSamples, modDepthSamps);
      else            sine     (lookup, modPerSamples, modDepthSamps);
      for (i = 0;  i < blocks;  i++)  phase [i] =
                          (slong)((real)modPerSamples * (real)i / (real)blocks);
      counter = 0;                     // (re)start circular buffer
   }

   void SetNr    (slong val)
   {  if      (val < 0)          val = 0;
      else if (val > MAX_BLOCK)  val = MAX_BLOCK;        blocks = val;}

   void SetLevel (real val)
   {  if      (val <  0.)  val =  0.;
      else if (val > 10.)  val = 10.;                     level = val;}

   void SetSpeed (real val)
   {  if      (val < MIN_SPEED)  val = MIN_SPEED;
      else if (val > MAX_SPEED)  val = MAX_SPEED;         speed = val;}

   void SetDepth (real val)   {if (val < 0.)  val = 0.;   depth = val;}
   void SetType (slong val)                               {type = val;}

   Chorus ()
   { slong i, ii;
     real  iShf;
      MemSet (this, 0, sizeof (Chorus));
      for (i = 0; i < INTERP_SAMPLES; i++)
         for (ii = 0; ii < INTERP_SUBSAMPLES; ii++) {
            iShf = ((real) i - ((real) INTERP_SAMPLES) / 2. +
                         (real) ii / (real) INTERP_SUBSAMPLES);
            if (fabs (iShf) < 0.000001)  sinc_table [i][ii] = (real)1;
            else {
               sinc_table [i][ii]  = (real)sin (iShf * PI) / (iShf * PI);
               sinc_table [i][ii] *= (real)0.5 * (1. +
                         cos (2.*PI * iShf / (real)INTERP_SAMPLES));
            }
         }
      lookup = new slong [(ulong)(AuO.frq / MIN_SPEED)];
      chorusbuf  = new real  [MAX_SAMPLES];
   // init
      for (slong i = 0; i < MAX_SAMPLES; i++)  chorusbuf [i] = 0.;
   }

  ~Chorus ()  {delete [] chorusbuf;   delete [] lookup;}

   void Mix (real *in, real *mixL, real *mixR)
   { ulong smp;
     slong i, ii, pos_samples, pos_subsamples;
     real  d_in, d_out;
      for (smp = 0;  smp < AuO.bufLn;  smp++) {
         d_in = in [smp];   d_out = 0.;
         chorusbuf [counter] = d_in;
         for (i = 0;  i < blocks;  i++) {
            pos_subsamples  = (INTERP_SUBSAMPLES*counter - lookup [phase [i]]);
            pos_samples     = pos_subsamples / INTERP_SUBSAMPLES;
            pos_subsamples &= INTERP_SUBSAMPLES_ANDMASK;

            for (ii = 0; ii < INTERP_SAMPLES; ii++) {
               d_out += chorusbuf [pos_samples & MAX_SAMPLES_ANDMASK] *
                                                sinc_table [ii][pos_subsamples];
               pos_samples--;
            }
            phase [i]++;   phase [i] %= modPerSamples;
         }
         d_out *= level;   mixL [smp] += d_out;   mixR [smp] += d_out;
         counter++;   counter %= MAX_SAMPLES;
      }
   }
};


//------------------------------------------------------------------------------
#define DC_OFFSET         (1e-8)       // initial value for all effects' bufs
#define ALLPASS_FEEDBACK  (0.5)        // allpass has fixed feedback


class Allpass {                        // allpass and comb are simpler filters
public:                                // used by reverb to do it's thing
   real *buf;
   slong len, pos;

   Allpass (slong l)
   {  buf = new real [len = l];   pos = 0;
      while (l)  buf [--l] = DC_OFFSET;
   }

  ~Allpass ()  {delete [] buf;}

   inline void mix (real *in)      // allpass's feedback is fixed
   { real bufout, out;
      bufout = buf [pos];   out = bufout - *in;
      buf [pos] = *in + (bufout * ALLPASS_FEEDBACK);
      if (++pos >= len)  pos = 0;
      *in = out;
   }
};


class Comb {
public:
   real *buf, feedback, damp1, damp2, store;
   slong len, pos;

   Comb (slong l)
   {  buf = new real [len = l];   pos = 0;   store = 0;
      while (l)  buf [--l] = DC_OFFSET;
   }

  ~Comb ()  {delete [] buf;}

   void setfeedback (real val)  {feedback = val;}
   void setdamp     (real val)  {damp1 = val;   damp2 = 1 - val;}

   inline void mix (real input, real *out)
   { real tmp = buf [pos];
      store = (tmp * damp2) + (store * damp1);
      buf [pos] = input + (store * feedback);
      if (++pos >= len)  pos = 0;
      *out += tmp;
   }
};


#define NUM_ALLP     (4)               // # allpass filters used by reverb
#define NUM_COMB     (8)               // # comb    filters used by reverb
#define REVERB_GAIN  (0.015)           // reverb has fixed gain
#define STEREO_SPR   (23)              // stereo spread

#define SCALE_WET    (3.)              // turning params into values to use
#define SCALE_DAMP   (1.)
#define SCALE_ROOM   (0.28)
#define OFFSET_ROOM  (0.7)

/* #define initialroom   (.5)          // not usin freeverb's defaults
** #define initialdamp   (.2)
** #define initialwidth  (1.)
** #define initialwet    (1.)
*/

// These assume 44.1KHz sample rate so adjust em
const ulong LEN_COMB [NUM_COMB] =
                               {1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617};
const ulong LEN_ALLP [NUM_ALLP] = {556, 441, 341, 225};

class Reverb {
public:
   Comb    *combL [NUM_COMB], *combR [NUM_COMB];
   Allpass *allpL [NUM_ALLP], *allpR [NUM_ALLP];
   real     room, damp, wet, width,  wet1, wet2;

   void Update ()                      // reinit given new params
   {  wet1 = wet * (width / 2 + 0.5f);   wet2 = wet * ((1 - width) / 2);
      for (slong i = 0;  i < NUM_COMB;  i++)
         {combL [i]->setfeedback (room);   combL [i]->setdamp (damp);
          combR [i]->setfeedback (room);   combR [i]->setdamp (damp);}
   }

   void SetRoom  (real val)  {room  = (val * SCALE_ROOM) + OFFSET_ROOM;}
   void SetDamp  (real val)  {damp  =  val * SCALE_DAMP;}
   void SetLevel (real val)  {CLIP (val, 0., 1.);   wet = val * SCALE_WET;}
   void SetWidth (real val)  {width =  val;}

   Reverb ()
   { ulong i;
     real  lenSc = AuO.frq / 44100.0;
      for (i = 0;  i < NUM_COMB;  i++) {
         combL [i] = new Comb    ((slong)( LEN_COMB [i]             * lenSc));
         combR [i] = new Comb    ((slong)((LEN_COMB [i]+STEREO_SPR) * lenSc));
      }
      for (i = 0;  i < NUM_ALLP;  i++) {
         allpL [i] = new Allpass ((slong)( LEN_ALLP [i]             * lenSc));
         allpR [i] = new Allpass ((slong)((LEN_ALLP [i]+STEREO_SPR) * lenSc));
      }
   }

  ~Reverb ()
   { ulong i;
      for (i = 0;  i < NUM_COMB;  i++)  {delete combL [i];   delete combR [i];}
      for (i = 0;  i < NUM_ALLP;  i++)  {delete allpL [i];   delete allpR [i];}
   }

   void Mix (real *in, real *mixL, real *mixR)
   { slong i;
     real  outL, outR, input;
      for (ulong k = 0;  k < AuO.bufLn;  k++) {
         outL = outR = 0;
         input = (2 * in [k] + DC_OFFSET) * REVERB_GAIN;   // gain is fixed
         for (i = 0;  i < NUM_COMB;  i++)  {combL [i]->mix (input, & outL);
                                            combR [i]->mix (input, & outR);}
         for (i = 0;  i < NUM_ALLP;  i++)  {allpL [i]->mix (       & outL);
                                            allpR [i]->mix (       & outR);}
         outL -= DC_OFFSET;
         outR -= DC_OFFSET;
         mixL [k] += outL * wet1 + outR * wet2;
         mixR [k] += outR * wet1 + outL * wet2;
      }
   }
};

i don't care about full/fancy envelopes (yet)
but i DO care about sample release!  so for meee...

voice init:
   get rate,ratio from user pick/default   (rate > 0, ration >= 0.000000001)
      rate  of .2 * 44100 for .2 sec
      ratio of .0001 mostly exp, 100 mostly linear
   coef = exp (-log ((1.0 + ratio) / ratio) / rate;
   _rel = 1.0
each voice mix:
   s *= (_amp * _rel);
   if (in release) {
      _rel *= coef;
      if (_rel <= 0.0)  kill voice
   }


// env.h
//
//    attack  starts at 0        goes to 1
//    decay   starts at 1        goes to susLevel
//    release starts at susLevel goes to 0

class Env {
public:
   EnvADSR (void)
   {  reset ();   setSus (1.0);
      setAtt (0, 0.3);   setDec (0, 0.0001);   setRel (0, 0.0001);
   }                                   // usually same ratio for decay,release
  ~EnvADSR (void)  {}

   enum envStage {env_idle = 0,  env_att,  env_dec,  env_sus,  env_rel};
   int     stage;
   real susLevel,   aRate, aRatio, aCoef, aBase,
                    dRate, dRatio, dCoef, dBase,
                    rRate, rRatio, rCoef, rBase,   output;
// setup-y_____________________________

   inline void reset ()  {stage = env_idle;   output = 0.0;}

   inline void setSus (real level)
   {  susLevel = level;                // decay depends on me so redo it's base
      dBase = (susLevel - dRatio) * (1.0 - dCoef);
   }

   inline real Coef (real rate, real ratio)
   {  if (ratio < 0.000000001)  ratio = 0.000000001;  // -180 dB
      if (rate <= 0)  return 0;
      return exp (-log ((1.0 + ratio) / ratio) / rate;
   }

   inline void setAtt (real rate, real ratio)
   // rate is time actually 0.1 * 44100 (4410.0) would be 0.1 seconds
   // ratio is curve shape .0001 mostly exponential .. 100 nearly linear
   {  aCoef = Coef (aRate = rate, aRatio = ratio);
      aBase = (1.0      + aRatio) * (1.0 - aCoef);
   }

   inline void setDec (real rate, real ratio)
   {  dCoef = Coef (dRate = rate, dRatio = ratio);
      dBase = (susLevel - dRatio) * (1.0 - dCoef);
   }

   inline void setRel (real rate, real ratio)
   {  rCoef = Coef (rRate = rate, rRatio = ratio);
      rBase = (0.0      - rRatio  * (1.0 - rCoef);
   }

// realtime-y__________________________

   inline void gate (int on);
   {  if      (gate)               stage = env_att;
      else if (stage != env_idle)  stage = env_rel;
   }

   inline real process ()
   {  switch (stage) {
         case env_idle:
            break;
         case env_att:
            output = aBase + output * aCoef;
            if (output >= 1.0)      {output = 1.0;        stage = env_dec;}
            break;
         case env_dec:
            output = dBase + output * dCoef;
            if (output <= susLevel) {output = susLevel;   stage = env_sus;}
            break;
         case env_sus:
            break;
         case env_rel:
            output = rBase + output * rCoef;
            if (output <= 0.0)      {output = 0.0;        stage = env_idle;}
      }
      return output;
   }

   inline real getOutput ()  {return output;}
   inline int  getStage  ()  {return stage;}
};

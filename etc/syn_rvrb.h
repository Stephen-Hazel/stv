// syn_rvrb.h

// this reverb ain't that great...  but, until i can get it better sigh...
#define DC_OFFSET         (1e-8)       // initial value for all effects' bufs
#define ALLPASS_FEEDBACK  (0.5)        // allpass has fixed feedback

#define CLIP(_val,_min,_max) \
{(_val) = ((_val)<(_min))?(_min):(((_val)>(_max))?(_max):(_val));}

class Allpass {                        // allpass and comb are simpler filters
public:                                // used by reverb to do it's thing
   real *buf;
   sbyt4 len, pos;

   Allpass (sbyt4 l)
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
   sbyt4 len, pos;

   Comb (sbyt4 l)
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

#define NUM_COMB     (8)               // # comb    filters used by reverb
#define NUM_ALLP     (4)               // # allpass filters used by reverb
#define REVERB_GAIN  (0.015)           // reverb has fixed gain
#define STEREO_SPR   (23)              // stereo spread

#define SCALE_WET    (3.)              // turning params into values to use
#define SCALE_DAMP   (1.)
#define SCALE_ROOM   (0.28)
#define OFFSET_ROOM  (0.7)

// These assume 44.1KHz sample rate so adjust em
extern const ubyt4 LEN_COMB [NUM_COMB];
extern const ubyt4 LEN_ALLP [NUM_ALLP];

class Reverb {
public:
   Comb    *combL [NUM_COMB], *combR [NUM_COMB];
   Allpass *allpL [NUM_ALLP], *allpR [NUM_ALLP];
   real     room, damp, wet, width,  wet1, wet2;

   void SetRoom  (real val)  {room  = (val * SCALE_ROOM) + OFFSET_ROOM;}
   void SetDamp  (real val)  {damp  =  val * SCALE_DAMP;}
   void SetWidth (real val)  {width =  val;}
   void SetLevel (real val)  {CLIP (val, 0., 1.);   wet = val * SCALE_WET;}
   void Update ();

   void Set (ubyte r = 64, ubyte d = 25, ubyte w = 127, ubyte l = 127)
   {  SetRoom (r / 127.);   SetDamp  (d / 127.);   SetWidth (w / 127.);
                            SetLevel (l / 127.);   Update ();
   }

   void Mix (real *in, real *mixL, real *mixR);

   Reverb ();
  ~Reverb ();
};

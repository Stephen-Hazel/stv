// snd.h - sound - out only - deal with alsa pcm

#ifndef SND_H
#define SND_H

#include "os.h"
#define  ALSA_PCM_NEW_HW_PARAMS_API
#include <alsa/asoundlib.h>


class SndLst {                         // query the hardwarez we gots
public:
   struct {TStr dev, desc;} lst [64];
   ubyte                    len;
   void Load (), Dump ();
   char *Get (char *desc);
};
extern SndLst Snd;


class SndO {
public:
   SndO (char *dev, ubyt4 nfr = 64, ubyt4 frq = 44100);
  ~SndO ();

   ubyt4 _nFr, _frq;                   // device may change these 2
   bool  Dead ()  {return (_hnd == nullptr) ? true : false;}
   char *Dev  ()  {return _dev;}
   void  Dump (snd_pcm_hw_params_t *hw);
   void  Put  (sbyt2 *buf);

private:
   TStr       _dev;
   snd_pcm_t *_hnd;
};

#endif  // SND_H

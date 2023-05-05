// snd.h - sound - out only - deal with alsa pcm

#ifndef SND_H
#define SND_H

#include "os.h"
#include <QThread>
#define  ALSA_PCM_NEW_HW_PARAMS_API
#include <alsa/asoundlib.h>


struct SndLst {                      // query the hardwarez we gots
public:
   struct {TStr dev, desc;} lst [64];
   ubyte                    len;
   void Load (), Dump ();
   char *Get (char *desc);
};
extern SndLst Snd;


class Syn;

class SndO: QThread {
   Q_OBJECT

public:
   SndO (Syn *syn, ubyt4 nfr = 64, ubyt4 frq = 44100);
  ~SndO ();

   bool  Dead ()  {return (_hnd == nullptr) ? true : false;}
   char *Desc ()  {return _desc;}
   char *Dev  ()  {return _dev;}
   ubyt4 NFr  ()  {return _nFr;}       // audio device may change these 2
   ubyt4 Frq  ()  {return _frq;}       // but unlikely :)
   void  run  ()  override;
   void  Dump (snd_pcm_hw_params_t *hw);

private:
   TStr  _desc, _dev;
   snd_pcm_t   *_hnd;
   ubyt4 _nFr,  _frq;
   sbyt2      (*_buf)[2];
   bool  _run;
   Syn  *_syn;
};

#endif  // SND_H

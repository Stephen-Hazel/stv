// snd.h - sound - out only - deal with alsa pcm

#ifndef SND_H
#define SND_H

#include "os.h"
#include <QThread>

#define  ALSA_PCM_NEW_HW_PARAMS_API
#include <alsa/asoundlib.h>

struct SndOLst {                     // query the hardwarez we gots
public:
   struct {TStr dev, desc;} lst [64];
   ubyte                    len;

   void Load ()
   { TStr name;
     int  c, d, e;                     // card, device, error
     snd_pcm_stream_t stream = SND_PCM_STREAM_PLAYBACK;
     snd_ctl_t *h;                     // handle
     snd_ctl_card_info_t *info;
     snd_pcm_info_t      *pcmi;
      len = 0;   snd_ctl_card_info_alloca (& info);
      c = -1;         snd_pcm_info_alloca (& pcmi);
      if ((snd_card_next (& c) < 0) || (c < 0)) {
         StrCp (lst [0].desc, CC("(no audio outs)"));
                lst [0].dev [0] = '\0';
         return;
      }
      snd_pcm_stream_name (stream);
      while (c >= 0) {
         StrFmt (name, "hw:`d", c);
         if ((e = snd_ctl_open (& h, name, 0)) < 0)
{DBG("snd_ctl_open died - `s", snd_strerror (e));   exit (1);}
         if ((e = snd_ctl_card_info (h, info)) < 0)
{DBG("snd_ctl_card_info died - `s", snd_strerror (e));   exit (1);}
         d = -1;
         for (;;) {
            if ((e = snd_ctl_pcm_next_device (h, & d)) < 0)
{DBG("snd_ctl_pcm_next_device died - `s", snd_strerror (e));   exit (1);}
            if (d < 0)  break;

            snd_pcm_info_set_device    (pcmi, d);
            snd_pcm_info_set_subdevice (pcmi, 0);     // there's never any more
            snd_pcm_info_set_stream    (pcmi, stream);
            snd_ctl_pcm_info        (h, pcmi);
            StrFmt (lst [len].dev,  "hw:`d,`d", c, d);
            StrFmt (lst [len].desc, "`s|`s",
               snd_ctl_card_info_get_name (info), snd_pcm_info_get_name (pcmi));
            len++;
         }
         snd_ctl_close (h);
         if (snd_card_next (& c) < 0)
{DBG("snd_card_next died");   exit (1);}
      }
   }

   void Dump ()
   {  for (ubyte i = 0;  i < len;  i++)  DBG ("`s => `s", lst [i].dev,
                                                          lst [i].desc);
   }

   char *Get (char *desc)
   {  for (ubyte i = 0;  i < len;  i++)
         if (! StrCm (desc, lst [i].desc))  return lst [i].dev;
DBG("Snd.Get(`s) got nothin !", desc);
      return nullptr;
   }
};
extern SndOLst Snd;


/* duuumb
**void **hints, **n;
**char  *name, *descr, *descr1, *io;
** if (snd_device_name_hint (-1, "pcm", & hints) < 0)  return;
** n = hints;
** while (*n != NULL) {
**    name  = snd_device_name_get_hint (*n, "NAME");
**    descr = snd_device_name_get_hint (*n, "DESC");
**    io    = snd_device_name_get_hint (*n, "IOID");
**    if ((io == nullptr) || (! StrCm (io, CC("Output")))) {
**DBG("`s [`s] `s", name, descr, io);
**    }
**    if (name  != nullptr)  free (name);
**    if (descr != nullptr)  free (descr);
**    if (io    != nullptr)  free (io);
**    n++;
** }
** snd_device_name_free_hint (hints);
*/


class SndO: public QThread {
   Q_OBJECT

   void run () override;

public:
   SndO (char *name);
  ~SndO ();

   bool  Dead ()  {return (_hnd == nullptr) ? true : false;}
   char *Desc ()  {return _desc;}
   char *Dev  ()  {return _dev;}

signals:
   void SndBuf ();

private:
   snd_pcm_t *_hnd;
   TStr  _desc, _dev;
   sbyt2 _buf [128];
   ubyte _bAdd, _bRmv;
   bool  _bErr, _run;
//   ubyt4 _frq, _len;
//   ubyte _bits;
//   bool  _float;
};

#endif  // SND_H

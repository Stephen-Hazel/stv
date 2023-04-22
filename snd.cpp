// snd.h - sound - out only - deal with alsa pcm

#include "snd.h"

SndOLst Snd;

void SndO::run ()                      // poll loop runnin in sep thread
{ ubyte buf [256];
  ubyt2 re;
  int   i, ln, err, npf;
  struct pollfd *pf;
TRC("run bgn `s", _desc);
   snd_pcm_writei (_hnd, nullptr, 0);      // trigger reading
   npf = snd_pcm_poll_descriptors_count (_hnd);
   pf  = (struct pollfd *)alloca (npf * sizeof (struct pollfd));
   snd_pcm_poll_descriptors (_hnd, pf, npf);
   while (_run) {
      err = poll (pf, npf, 500);       // timeout at 1/2 sec (500 millisec)
      if (err < 0)
         {DBG ("poll failed: `s", strerror (errno));   break;}

      err = snd_pcm_poll_descriptors_revents (_hnd, pf, npf, & re);
      if (err < 0)
         {DBG("s_r_poll_d_revents failed: `s", snd_strerror (err));   break;}
      if (re & (POLLERR | POLLHUP))
         {DBG("s_r_poll_d_revents ERR/HUP");   break;}

      err = snd_pcm_writei (_hnd, buf, sizeof (buf));
      if (err == -EAGAIN)  continue;   // just ain't nothin therez

      if (err < 0)
         {DBG ("s_r_read failed: `s", snd_strerror (err));   break;}

      if (ln == 0)  continue;
      if (ln >  4)  continue;
      emit SndBuf ();
   }
   _run = false;
TRC("run end `s", _desc);
}

/*
static void AudORun (void *d)
{ AudO  *dev = (AudO *)d;
  sbyt2 *buf;
  int    buffer_size, offset, n;
   buffer_size = d->buffer_size;
   buf = new short [buffer_size];
   if (buf == nullptr)
{DBG("AudO thread out of memory");   exit (1);}
   if ((e = snd_pcm_prepare (AudO)) < 0)
{DBG("pcm_prepare died - `s", snd_strerror (e));   exit (1);}

   while (d->cont) {
      d->write_s16 (buffer_size, buf, 0, 2, buf, 1, 2);
      offset = 0;
      while (offset < buffer_size) {
         n = snd_pcm_writei (AudO, (void *)(buf + 2*offset),
                                            buffer_size - offset);
         if (n >= 0)  offset += n;
         else {
            switch (n) {
               case -EAGAIN:
                  snd_pcm_wait (AudO, 1);
                  break;
               case -ESTRPIPE:
                  if (snd_pcm_resume (AudO) != 0)
{DBG("pcm_resume died");   exit (1);}
               case -EPIPE:  case -EBADFD:
                  if (snd_pcm_prepare (AudO) != 0)
{DBG("pcm_prepare died");   exit (1);}
                  break;
               default:
{DBG("pcm error `s", snd_strerror (e));   exit (1);}
            }
         }
      }
   }
   delete [] buf;
}
*/


static ubyt2 PerSize = 4096, Periods = 1;
static sbyt2 Buf [4096], Buf2 [4096];

SndO::SndO (char *desc)
// timer needed to stamp MidiEv.time
{ int e;
   _hnd = nullptr;   _bAdd = _bRmv = 0;   _bErr = false;
   StrCp (_desc, desc);
TRC("SndO `s", _desc);
   if (StrCp (_dev, Snd.Get (_desc)) == nullptr)
      {DBG("SndO no device for `s", _desc);   return;}
   if (*_dev == '?')
      {DBG("SndO device `s isn't on", _desc);   return;}
TRC("   dev=`s", _dev);

   if ((e = snd_pcm_open (& _hnd, _dev, SND_PCM_STREAM_PLAYBACK,
                                        SND_PCM_NONBLOCK))) {
      if (e == -EBUSY)
DBG("snd_pcm_open - another app has it - `s", _dev, snd_strerror (e));
      else
DBG("snd_pcm_open died - `s", _dev, snd_strerror (e));
      _hnd = nullptr;   return;
   }

// SHOISH !!!
  snd_pcm_hw_params_t *hw;
   snd_pcm_hw_params_alloca (& hw);
   snd_pcm_hw_params_any (_hnd, hw);
   if ((e = snd_pcm_hw_params_set_access (_hnd, hw,
                                          SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
DBG("pcm_access interleaved died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_hw_params_set_format (_hnd, hw, SND_PCM_FORMAT_S16)) < 0) {
DBG("pcm_format s16 died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_hw_params_set_channels (_hnd, hw, 2)) < 0) {
DBG("pcm_channels stereo died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }
  unsigned int frq = 44100;
   if ((e = snd_pcm_hw_params_set_rate_near (_hnd, hw, & frq, 0)) < 0) {
DBG("pcm_rate 44100 died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }
   if (frq != 44100)
DBG("pcm_rate only `d not 44100 :(", frq);
  snd_pcm_uframes_t nfr = PerSize;
  int               dir = 0;
   if ((e = snd_pcm_hw_params_set_period_size_near (_hnd, hw, & nfr, & dir))
                                                                          < 0) {
DBG("pcm_period died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }
   if (nfr != PerSize) {
DBG("pcm_period only `d not `d :(", nfr, PerSize);
      PerSize = nfr;
   }
  ubyt4 per = Periods;
   if ((e = snd_pcm_hw_params_set_periods_near (_hnd, hw, & per, & dir)) < 0) {
DBG("pcm_periods died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }
   if (per != Periods) {
DBG("pcm_periods only `d not `d :(", per, Periods);
      Periods = per;
   }
   if ((e = snd_pcm_hw_params (_hnd, hw)) < 0) {
DBG("pcm_hw died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }

  snd_pcm_sw_params_t *sw;
   snd_pcm_sw_params_alloca (& sw);
   snd_pcm_sw_params_current (_hnd, sw);
   if ((e = snd_pcm_sw_params_set_start_threshold (_hnd, sw, PerSize)) < 0) {
DBG("pcm_start_thresh died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_sw_params_set_avail_min (_hnd, sw, PerSize)) < 0) {
DBG("pcm_avail_min `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_sw_params (_hnd, sw)) < 0) {
DBG("pcm_sw died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_nonblock (_hnd, 0)) < 0) {
DBG("pcm_nonblock died - `s", snd_strerror (e));
      _hnd = nullptr;   return;
   }

// connect (this, & SndO::SndBuf,   this, & SndO::EvIns);
   connect (this, & SndO::finished, this, & QObject::deleteLater);
   _run = true;
   start ();
}


SndO::~SndO (void)
{ int err;
TRC("~SndO `s", *_desc ? _desc : "?");
   if (Dead ())  {TRC("...was dead");   return;}
   if (_run)  {_run = false;   wait ();}
   if ((err = ::snd_pcm_close (_hnd)))
      DBG("snd_pcm_close died - `s", ::snd_strerror (err));
   _hnd = nullptr;
}

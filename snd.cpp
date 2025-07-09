// snd.h - sound - out only - deal with alsa pcm

#include "snd.h"

SndLst Snd;

void SndLst::Load ()                   // whiiich device/descrips do we gots?
{ TStr name;
  int  c, d, e;                        // card, device, error
  snd_pcm_stream_t stream = SND_PCM_STREAM_PLAYBACK;
  snd_ctl_t *h;                        // handle
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
         snd_pcm_info_set_subdevice (pcmi, 0);   // there's never any more
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

void SndLst::Dump ()
{  for (ubyte i = 0;  i < len;  i++)  DBG ("`s => `s", lst [i].dev,
                                                       lst [i].desc);
}

char *SndLst::Get (char *desc)
{  for (ubyte i = 0;  i < len;  i++)
      if (! StrCm (desc, lst [i].desc))  return lst [i].dev;
DBG("SndLst.Get(`s) got nothin !", desc);
   return CC("");
}


/*______________________________________________________________________________
any:
   PCM handle name  = 'hw:2,0'
   PCM state        = OPEN
   access type      = (null)
   format           = '(null)' ((null))
   subformat        = 'STD' (Standard)
   channels         = 0
   rate             = 0 bps
   period time      = 0 us
   period size      = 159518096 frames
   buffer time      = 0 us
   buffer size      = 0 frames
   periods per buffer = 0 frames
 * exact rate       = 0/32658 bps
 * significant bits = -22
   is batch         = 1
   is block transfer = 1
   is double        = 0
   is half duplex   = 0
   is joint duplex  = 0
   can overrange    = 0
 * can mmap         = 1
 * can pause        = 1
 * can resume       = 0
 * can sync start   = 0

after example hw init:
   PCM state        = PREPARED
   access type      = MMAP_INTERLEAVED
   format           = 'S16_LE' (Signed 16 bit Little Endian)
   channels         = 2
   rate             = 44100 bps
   period time      = 1451 us
   period size      = 64 frames
   buffer time      = 2902 us
   buffer size      = 128 frames
   periods per buffer = 2 frames
   exact rate       = 44100/1 bps
   significant bits = 16

now:
   PCM handle name  = 'hw:3,0'
   PCM state        = PREPARED
   access type      = RW_INTERLEAVED
   format           = 'S16_LE' (Signed 16 bit Little Endian)
   subformat        = 'STD' (Standard)
   channels         = 2
   rate             = 44100 bps
   period time      = 1451 us
   period size      = 64 frames
   buffer time      = 2902 us
   buffer size      = 128 frames
   periods per buffer = 2 frames
   exact rate       = 44100/1 bps
   significant bits = 16
   is batch         = 1
   is block transfer = 1
   is double        = 0
   is half duplex   = 0
   is joint duplex  = 0
   can overrange    = 0
   can mmap         = 1
   can pause        = 1
   can resume       = 0
   can sync start   = 0
______________________________________________________________________________*/


void SndO::Dump (snd_pcm_hw_params_t *hw)
{ unsigned int val, val2;
  int          dir;
  snd_pcm_uframes_t fr;
   DBG("PCM handle name = '`s'",
      snd_pcm_name (_hnd));

   DBG("PCM state = `s",
      snd_pcm_state_name (snd_pcm_state (_hnd)));

   snd_pcm_hw_params_get_access (hw, (snd_pcm_access_t *) & val);
   DBG("access type = `s",
      snd_pcm_access_name ((snd_pcm_access_t)val));

  snd_pcm_format_t fmt;
   snd_pcm_hw_params_get_format (hw, & fmt);
   DBG("format = '`s' (`s)",
      snd_pcm_format_name        (fmt),
      snd_pcm_format_description (fmt));

   snd_pcm_hw_params_get_subformat (hw, (snd_pcm_subformat_t *)&val);
   DBG("subformat = '`s' (`s)",
      snd_pcm_subformat_name        ((snd_pcm_subformat_t)val),
      snd_pcm_subformat_description ((snd_pcm_subformat_t)val));

   snd_pcm_hw_params_get_channels (hw, &val);
   DBG("channels = `d", val);

   snd_pcm_hw_params_get_rate (hw, &val, &dir);
   DBG("rate = `d bps", val);

   snd_pcm_hw_params_get_period_time (hw, &val, &dir);
   DBG("period time = `d us", val);

   snd_pcm_hw_params_get_period_size (hw, & fr, &dir);
   DBG("period size = `d frames", (int)fr);

   snd_pcm_hw_params_get_buffer_time (hw, &val, &dir);
   DBG("buffer time = `d us", val);

   snd_pcm_hw_params_get_buffer_size (hw, (snd_pcm_uframes_t *) &val);
   DBG("buffer size = `d frames", val);

   snd_pcm_hw_params_get_periods (hw, &val, &dir);
   DBG("periods per buffer = `d frames", val);

   snd_pcm_hw_params_get_rate_numden (hw, &val, &val2);
   DBG("exact rate = `d/`d bps", val, val2);

   val = snd_pcm_hw_params_get_sbits (hw);
   DBG("significant bits = `d", val);

   val = snd_pcm_hw_params_is_batch (hw);
   DBG("is batch = `d", val);

   val = snd_pcm_hw_params_is_block_transfer (hw);
   DBG("is block transfer = `d", val);

   val = snd_pcm_hw_params_is_double (hw);
   DBG("is double = `d", val);

   val = snd_pcm_hw_params_is_half_duplex (hw);
   DBG("is half duplex = `d", val);

   val = snd_pcm_hw_params_is_joint_duplex (hw);
   DBG("is joint duplex = `d", val);

   val = snd_pcm_hw_params_can_overrange (hw);
   DBG("can overrange = `d", val);

   val = snd_pcm_hw_params_can_mmap_sample_resolution (hw);
   DBG("can mmap = `d", val);

   val = snd_pcm_hw_params_can_pause (hw);
   DBG("can pause = `d", val);

   val = snd_pcm_hw_params_can_resume (hw);
   DBG("can resume = `d", val);

   val = snd_pcm_hw_params_can_sync_start (hw);
   DBG("can sync start = `d", val);
}


SndO::SndO (char *dev, ubyt4 inFr, ubyt4 ifrq)
// device to write,  frames in 1 period,  and frequency
// open up our alsa pcm device (audio out)
// always 2 periods of nFr frames - interleaved stereo s16
: _nFr (inFr), _frq (ifrq)             // what we ask fer.  what we get may diff
{ int   e;  // error
  sbyt4 dir  = 0;
  ubyt4 nPer = 2, nFr = inFr, frq = ifrq;
   StrCp (_dev, dev);
   _hnd = nullptr;
   if ((e = snd_pcm_open (& _hnd, _dev, SND_PCM_STREAM_PLAYBACK,
                                        SND_PCM_NONBLOCK))) {
      if (e == -EBUSY)
DBG("snd_pcm_open `s - another app has it - `s", _dev, snd_strerror (e));
      else
DBG("snd_pcm_open `s died - `s",                 _dev, snd_strerror (e));
      _hnd = nullptr;   return;
   }

// SHOISH !!!
  snd_pcm_hw_params_t *hw;
   snd_pcm_hw_params_alloca (& hw);
   snd_pcm_hw_params_any (_hnd, hw);
   if ((e = snd_pcm_hw_params_set_access (_hnd, hw,
                                          SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
DBG("pcm_access rw_interleaved died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_hw_params_set_format (_hnd, hw, SND_PCM_FORMAT_S16)) < 0) {
DBG("pcm_format s16 died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_hw_params_set_channels (_hnd, hw, 2)) < 0) {
DBG("pcm_channels stereo died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }

   if ((e = snd_pcm_hw_params_set_rate_near (_hnd, hw, & frq, 0)) < 0) {
DBG("pcm_rate died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
   if (_frq != frq)  DBG("pcm_rate wanted `d got `d :/", _frq, frq);

   if ((e = snd_pcm_hw_params_set_period_size_near (_hnd, hw,
                                   (snd_pcm_uframes_t *)(& nFr), & dir)) < 0) {
DBG("pcm_period died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
   if (_nFr != nFr)  DBG("pcm_period wanted `d got `d :/", _nFr, nFr);

   if ((e = snd_pcm_hw_params_set_periods_near (_hnd, hw, & nPer, & dir)) < 0) {
DBG("pcm_periods died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
   if (nPer != 2) {
DBG("pcm_periods is `d not 2 :(", nPer);
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_hw_params (_hnd, hw)) < 0) {
DBG("pcm_hw died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
Dump (hw);

  snd_pcm_sw_params_t *sw;
   snd_pcm_sw_params_alloca (& sw);
   snd_pcm_sw_params_current (_hnd, sw);
   if ((e = snd_pcm_sw_params_set_start_threshold (_hnd, sw, _nFr)) < 0) {
DBG("pcm_start_thresh died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_sw_params_set_avail_min (_hnd, sw, _nFr)) < 0) {
DBG("pcm_avail_min died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
   if ((e = snd_pcm_sw_params (_hnd, sw)) < 0) {
DBG("pcm_sw died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }

   if ((e = snd_pcm_nonblock (_hnd, 0)) < 0) {
DBG("pcm_nonblock died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }

   if ((e = snd_pcm_prepare (_hnd)) < 0) {
DBG("pcm_prepare died - `s", snd_strerror (e));
      snd_pcm_close (_hnd);   _hnd = nullptr;   return;
   }
}


void SndO::Put (sbyt2 *buf)
{ ubyt4 p;
  int   e;
   for (p = 0;  p < _nFr;) {
      e = snd_pcm_writei (_hnd, (void *)(& buf [p*2]), _nFr - p);
      if (e >= 0)  p += e;
      else                          // oops - fixup stuff
         switch (e) {
            case -EAGAIN:
               if ((e = snd_pcm_wait    (_hnd, 1)) < 0)
{DBG("pcm_wait died - `s",    snd_strerror (e));   return;}
               break;
            case -ESTRPIPE:
               if ((e = snd_pcm_resume  (_hnd)) < 0)    // n fall thru
{DBG("pcm_resume died - `s",  snd_strerror (e));   return;}
            case -EPIPE:  case -EBADFD:
               if ((e = snd_pcm_prepare (_hnd)) < 0)
{DBG("pcm_prepare died - `s", snd_strerror (e));   return;}
               break;
            default:
{DBG("pcr_writei died - `s",  snd_strerror (e));   return;}
         }
   }
}


SndO::~SndO (void)
{ int e;
TRC("~SndO `s", *_dev ? _dev : "?");
   if (Dead ())  {DBG("...was dead");   return;}
   if ((e = snd_pcm_close (_hnd)) < 0)
DBG("snd_pcm_close died - `s", snd_strerror (e));
   _hnd = nullptr;
}

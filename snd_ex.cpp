// dad.h
#include <QApplication>

class Kid;

class Dad: public QApplication {
   Q_OBJECT

public:
   Dad (int & argc, char **argv);
signals:
   void MsgKid ();
private:
   Kid *_kid;
};


// kid.h
class Kid: public QProcess {
   Q_OBJECT

public:
   explicit Kid (QObject *parent = nullptr);
private slots:
   void onDadMsg ();
};


// dad.cpp
#include "dad.h"
#include "kid.h"

Dad::Dad (int & argc, char **argv)
: QApplication (argc, argv)
{  _kid = new Kid (this);
   _kid->start ();
   _kid->waitForStarted ();
   emit HeyKid ("howdy");
   emit HeyKid ("bye");
   _kid->close ();
}


// kid.cpp
#include "dad.h"
#include "kid.h"

Kid::Kid (QObject *parent)
: QProcess (parent)
{  if (parent) {
     auto pop = qobject_cast<Dad *>(parent);
      if (me)  connect (pop,  SIGNAL (MsgKid (char *m)),
                        this, SLOT (onDadMsg (char *m)));
   }
}

void Kid::onDadMsg (char *m)
{  DBG("dad sez `s", m);  }






















//test/pcm.c
/*
 *  This small demo sends a simple sinusoidal wave to your speakers.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sched.h>
#include <errno.h>
#include <getopt.h>
#include "../include/asoundlib.h"
#include <sys/time.h>
#include <math.h>

static snd_pcm_format_t format = SND_PCM_FORMAT_S16;  // sample format
static ubyt4  rate        = 44100;     // stream rate
static ubyt4  channels    = 1;         // count of channels
static real   freq        = 440;       // sin wave frequency in Hz
static snd_pcm_sframes_t PerSize;

static void generate_sine (
   const snd_pcm_channel_area_t *areas, snd_pcm_uframes_t offset, sbyt4 count,
                                                                  real *_phase)
{  static real max_phase = 2. * M_PI;
   real   phase = *_phase;
   real   step  = max_phase * freq / (real)rate;
   ubyte *samples [channels];
   sbyt4  steps [channels];
   ubyt4  chn;
   sbyt4  format_bits = snd_pcm_format_width (format);
   ubyt4  maxval      = (1 << (format_bits - 1)) - 1;
   sbyt4  bps         = format_bits / 8;  /* bytes per sample */
   sbyt4  phys_bps    = snd_pcm_format_physical_width (format) / 8;
   sbyt4  big_endian  = snd_pcm_format_big_endian (format) == 1;
   sbyt4  to_unsigned = snd_pcm_format_unsigned (format) == 1;
   sbyt4  is_float    = (format == SND_PCM_FORMAT_FLOAT_LE ||
                         format == SND_PCM_FORMAT_FLOAT_BE);
/* verify and prepare the contents of areas */
   for (chn = 0; chn < channels; chn++) {
      if ((areas [chn].first % 8) != 0) {
         printf("areas[%u].first == %u, aborting...\n", chn, areas [chn].first);
         exit (99);
      }
      samples [chn] = (((ubyte *)areas [chn].addr) +
                                (areas [chn].first / 8));
      if ((areas [chn].step % 16) != 0) {
         printf("areas[%u].step == %u, aborting...\n", chn, areas[chn].step);
         exit (99);
      }
      steps [chn] = areas [chn].step / 8;
      samples [chn] += offset * steps [chn];
   }
// fill the channel areas
   while (count-- > 0) {
     union {
        real f;
        sbyt4 i;
     } fval;
     sbyt4 res, i;
      if (is_float) {
         fval.f = sin (phase);
         res = fval.i;
      }
      else
         res = sin (phase) * maxval;
      if (to_unsigned)
         res ^= 1U << (format_bits - 1);
      for (chn = 0; chn < channels; chn++) {
      // Generate data in native endian format
         if (big_endian) {
            for (i = 0; i < bps; i++)
               *(samples [chn] + phys_bps - 1 - i) = (res >> i * 8) & 0xff;
         }
         else {
            for (i = 0; i < bps; i++)
               *(samples [chn] + i) = (res >>  i * 8) & 0xff;
         }
         samples [chn] += steps [chn];
      }
      phase += step;
      if (phase >= max_phase)  phase -= max_phase;
   }
   *_phase = phase;
}


static sbyt4 xrun_recovery (snd_pcm_t *hnd, sbyt4 err)
// Underrun and suspend recovery
{
DBG("xrun_recovery");
   if (err == -EPIPE) {                // under-run
      err = snd_pcm_prepare (hnd);
      if (err < 0)
DBG("xrun pcm_prepare died - `s", snd_strerror (err));
      return 0;                        // yay :)
   }
   else if (err == -ESTRPIPE) {
      while ((err = snd_pcm_resume (hnd)) == -EAGAIN)
         sleep (1);                    // wait until suspend flag released
      if (err < 0) {
         err = snd_pcm_prepare (hnd);
         if (err < 0)
DBG("xrun pcm_prepare2 died - `s", snd_strerror (err));
      }
      return 0;                        // yay :)
   }
   return err;
}


// asynchronous notification + direct write
struct async_private_data {
   sbyt2                  *samples;
   snd_pcm_channel_area_t *areas;
   real                    phase;
};

static void async_direct_callback (snd_async_handler_t *ahnd)
{ snd_pcm_t                 *hnd = snd_async_handler_get_pcm (ahnd);
  struct async_private_data *data =
                         snd_async_handler_get_callback_private (ahnd);
  const snd_pcm_channel_area_t *my_areas;
  snd_pcm_uframes_t offset, frames, size;
  snd_pcm_sframes_t avail, commitres;
  snd_pcm_state_t   state;
  sbyt4             first = 0, err;
   while (1) {
      state = snd_pcm_state (hnd);
      if (state == SND_PCM_STATE_XRUN) {
         err = xrun_recovery (hnd, -EPIPE);
         if (err < 0) {
DBG("XRUN died - `s", snd_strerror (err));
            exit (99);
         }
         first = 1;
      }
      else if (state == SND_PCM_STATE_SUSPENDED) {
         err = xrun_recovery (hnd, -ESTRPIPE);
         if (err < 0) {
DBG("SUSPEND died - `s", snd_strerror (err));
            exit (99);
         }
      }
      avail = snd_pcm_avail_update (hnd);
      if (avail < 0) {
         err = xrun_recovery (hnd, avail);
         if (err < 0) {
DBG("XRUN2 died - `s", snd_strerror (err));
            exit (99);
         }
         first = 1;
         continue;
      }
      if (avail < PerSize) {
         if (first) {
            first = 0;
            err = snd_pcm_start (hnd);
            if (err < 0) {
DBG("pcm_start died - `s", snd_strerror (err));
               exit (99);
            }
         }
         else
            break;
         continue;
      }
      size = PerSize;
      while (size > 0) {
         frames = size;
         err = snd_pcm_mmap_begin (hnd, &my_areas, &offset, &frames);
         if (err < 0) {
            if ((err = xrun_recovery (hnd, err)) < 0) {
DBG("pcm_mmap_begin died - `s", snd_strerror (err));
               exit (99);
            }
            first = 1;
         }
         generate_sine (my_areas, offset, frames, &data->phase);
         commitres = snd_pcm_mmap_commit (hnd, offset, frames);
         if (commitres < 0 || (snd_pcm_uframes_t)commitres != frames) {
            if ((err = xrun_recovery(hnd,
                                    commitres >= 0 ? -EPIPE : commitres)) < 0) {
DBG("pcm_mmap_commit died - `s", snd_strerror (err));
               exit (99);
            }
            first = 1;
         }
         size -= frames;
      }
   }
}


static sbyt4 async_direct_loop (
   snd_pcm_t *hnd, sbyt2 *samples, snd_pcm_channel_area_t *areas)
{ struct async_private_data     data;
  snd_async_handler_t          *ahnd;
  const snd_pcm_channel_area_t *my_areas;
  snd_pcm_uframes_t             offset, frames, size;
  snd_pcm_sframes_t             commitres;
  sbyt4                         err, count;
   data.samples = NULL;
/* we do not require the global sample area for direct write */
   data.areas   = NULL;
/* we do not require the global areas for direct write */
   data.phase   = 0;
   err = snd_async_add_pcm_handler (& ahnd, hnd, async_direct_callback, & data);
   if (err < 0) {
      printf ("Unable to register async handler\n");
      exit (99);
   }
   for (count = 0;  count < 2;  count++) {
      size = PerSize;
      while (size > 0) {
         frames = size;
         err = snd_pcm_mmap_begin (hnd, &my_areas, &offset, &frames);
         if (err < 0) {
            if ((err = xrun_recovery (hnd, err)) < 0) {
DBG("snd_mmap_begin died - `s", snd_strerror (err));
               exit (99);
            }
         }
         generate_sine (my_areas, offset, frames, &data.phase);
         commitres = snd_pcm_mmap_commit (hnd, offset, frames);
         if (commitres < 0 || (snd_pcm_uframes_t)commitres != frames) {
            if ((err = xrun_recovery (
                          hnd, commitres >= 0 ? -EPIPE : commitres)) < 0) {
DBG("snd_mmap_commit died - `s", snd_strerror (err));
               exit (99);
            }
         }
         size -= frames;
      }
   }
   err = snd_pcm_start (hnd);
   if (err < 0) {
DBG("pcm_start died - `s", snd_strerror (err));
      exit (99);
   }
   while (1)  sleep (1);               // signal handler does it all so sleep
}


// direct write only

static sbyt4 direct_loop (snd_pcm_t *hnd, sbyt2 *samples,
                          snd_pcm_channel_area_t *areas)
{ real              phase = 0;
  const snd_pcm_channel_area_t *my_areas;
  snd_pcm_uframes_t offset, frames, size;
  snd_pcm_sframes_t avail, commitres;
  snd_pcm_state_t   state;
  sbyt4             err, first = 1;
   while (1) {
      state = snd_pcm_state (hnd);
      if (state == SND_PCM_STATE_XRUN) {
         err = xrun_recovery (hnd, -EPIPE);
         if (err < 0) {
            printf ("XRUN recovery failed: %s\n", snd_strerror (err));
            return err;
         }
         first = 1;
      }
      else if (state == SND_PCM_STATE_SUSPENDED) {
         err = xrun_recovery (hnd, -ESTRPIPE);
         if (err < 0) {
            printf ("SUSPEND recovery failed: %s\n", snd_strerror (err));
            return err;
         }
      }
      avail = snd_pcm_avail_update (hnd);
      if (avail < 0) {
         err = xrun_recovery (hnd, avail);
         if (err < 0) {
            printf ("avail update failed: %s\n", snd_strerror (err));
            return err;
         }
         first = 1;
         continue;
      }
      if (avail < PerSize) {
         if (first) {
            first = 0;
            err = snd_pcm_start (hnd);
            if (err < 0) {
               printf ("Start error: %s\n", snd_strerror (err));
               exit (99);
            }
         }
         else {
            err = snd_pcm_wait (hnd, -1);
            if (err < 0) {
               if ((err = xrun_recovery (hnd, err)) < 0) {
                  printf ("snd_pcm_wait error: %s\n", snd_strerror (err));
                  exit (99);
               }
               first = 1;
            }
         }
         continue;
      }
      size = PerSize;
      while (size > 0) {
         frames = size;
         err = snd_pcm_mmap_begin (hnd, &my_areas, &offset, &frames);
         if (err < 0) {
            if ((err = xrun_recovery (hnd, err)) < 0) {
               printf ("MMAP begin avail error: %s\n", snd_strerror (err));
               exit (99);
            }
            first = 1;
         }
         generate_sine (my_areas, offset, frames, &phase);
         commitres = snd_pcm_mmap_commit (hnd, offset, frames);
         if (commitres < 0 || (snd_pcm_uframes_t)commitres != frames) {
            if ((err = xrun_recovery (hnd,
                                    commitres >= 0 ? -EPIPE : commitres)) < 0) {
               printf ("MMAP commit error: %s\n", snd_strerror (err));
               exit (99);
            }
            first = 1;
         }
         size -= frames;
      }
   }
}

// direct write only using mmap_write functions

static sbyt4 direct_write_loop (snd_pcm_t *hnd, sbyt2 *samples,
                                snd_pcm_channel_area_t *areas)
{ real  phase = 0;
  sbyt2 *ptr;
  sbyt4 cptr, err;
   while (1) {
      generate_sine (areas, 0, PerSize, & phase);
      ptr = samples;   cptr = PerSize;
      while (cptr > 0) {
         err = snd_pcm_mmap_writei (hnd, ptr, cptr);
         if (err == -EAGAIN)  continue;
         if (err < 0) {
            if (xrun_recovery (hnd, err) < 0) {
DBG("pcm_mmap_write died - `s", snd_strerror (err));
               exit (99);
            }
            break;                     // skip one period
         }
         ptr += err * channels;   cptr -= err;
      }
   }
}


struct transfer_method {
   const char      *name;
   snd_pcm_access_t access;
   sbyt4 (*transfer_loop)(
      snd_pcm_t *handle, sbyt2 *samples, snd_pcm_channel_area_t *areas);
};
static struct transfer_method transfer_methods [] = {
   {"async_direct",         SND_PCM_ACCESS_MMAP_INTERLEAVED, async_direct_loop},
   {"direct_interleaved",   SND_PCM_ACCESS_MMAP_INTERLEAVED,    direct_loop},
   {"direct_noninterleaved",SND_PCM_ACCESS_MMAP_NONINTERLEAVED, direct_loop},
   {"direct_write",         SND_PCM_ACCESS_MMAP_INTERLEAVED, direct_write_loop},
};

// sound via pipewire - out only

#include "snd.h"

#include <pipewire/pipewire.h>
#include <spa/param/audio/format-utils.h>
#include <cstring>
#include <algorithm>


// Stream events file-scope so pointer stays valid for stream's life
static pw_stream_events setPWStreamEvents ()
{ pw_stream_events e {};
   e.version = PW_VERSION_STREAM_EVENTS;
   e.process = SndO::mix;
   return e;
}

static const pw_stream_events kPWStrEv = setPWStreamEvents ();


void SndO::open (static char *name, pfunc pmix, ubyt4 frq)
// init the pw thread to pull audio from syn
{  _mix = pmix;   _frq = frq;

   pw_init (nullptr, nullptr);         // init n wipe
   _pwLoop   = nullptr;
   _pwStream = nullptr;
   _run.store (false, std::memory_order_relaxed);

   _pwLoop = pw_main_loop_new (nullptr);
   if (! _pwLoop) {
DBG("pw_main_loop_new DIED !!");   return;}

  struct pw_properties *props = pw_properties_new (
      PW_KEY_MEDIA_TYPE,     "Audio",
      PW_KEY_MEDIA_CATEGORY, "Playback",
      PW_KEY_MEDIA_ROLE,     "Music",
      nullptr
   );
   _pwStream = pw_stream_new_simple (
      pw_main_loop_get_loop (_pwLoop), name, props, &kPWStrEv, this);
   if (! _pwStream) {
DBG("pw_stream_new_simple DIED");   return;}

  ubyte buf [1024];                    // Build the SPA audio format pod
  spa_pod_builder b = SPA_POD_BUILDER_INIT (buf, sizeof (buf));
  spa_audio_info_raw info {};
   info.format   = SPA_AUDIO_FORMAT_F32;    // i guess pw prefers this ??
   info.channels = 2;                       // always stereo fo mee
   info.rate     = frq;
  const spa_pod *parm [1];
   parm [0] = spa_format_audio_raw_build (& b, SPA_PARAM_EnumFormat, & info);
   pw_stream_connect (
      _pwStream, PW_DIRECTION_OUTPUT, PW_ID_ANY,
      static_cast<pw_stream_flags> (PW_STREAM_FLAG_AUTOCONNECT |
                                    PW_STREAM_FLAG_MAP_BUFFERS |
                                    PW_STREAM_FLAG_RT_PROCESS),
      parm, 1);
   _thread = std::thread ([this] { pw_main_loop_run (_pwLoop); });
   _run.store (true, std::memory_order_release);
}                                      // run thread for pipewire event loop


void SndO::shut ()
{  _run.store (false, std::memory_order_release);
   if (_pwLoop)  pw_main_loop_quit (_pwLoop);
   if (_thread.joinable ())  _thread.join ();
   if (_pwStream) {pw_stream_destroy    (_pwStream);   _pwStream = nullptr;}
   if (_pwLoop)   {pw_main_loop_destroy (_pwLoop);     _pwLoop   = nullptr;}
}


void SndO::mix (void *arg)
// pipewire real-time process callback
{ auto      *me    = static_cast<SndO *>(arg);
  pw_buffer *pwBuf = pw_stream_dequeue_buffer (me->_pwStream);
   if (! pwBuf)  return;

  spa_buffer *spaBuf = pwBuf->buffer;
  auto       *dst    = static_cast<real4 *> (spaBuf->datas [0].data);
   if (! dst)  {pw_stream_queue_buffer (me->_pwStream, pwBuf);   return;}

// ok!  got our pwBuf > spaBuf > dst
  const ubyt4 szFr = sizeof (real4) * 2;    // size of frame (real4 stereo)
  ubyt4       nFr  = spaBuf->datas [0].maxsize / szFr;
   if (pwBuf->requested > 0)
      nFr = std::min (nFr, static_cast<ubyt4> (pwBuf->requested));

// if paused, just send a buf o zeroz
   if (! me->_run.load (std::memory_order_acquire))
         MemSet (dst, 0, nFr * szFr);
   else  me->_mix (dst, nFr);
   spaBuf->datas [0].chunk->offset = 0;
   spaBuf->datas [0].chunk->stride = static_cast<sbyt4> (szFr);
   spaBuf->datas [0].chunk->size   = nFr * szFr;
   pw_stream_queue_buffer (me->_pwStream, pwBuf);
}

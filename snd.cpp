// sound via pipewire - out only

#include "snd.h"
#include <spa/param/audio/format-utils.h>


// Stream events file-scope so pointer stays valid for stream's life
static pw_stream_events setPWStreamEvents ()
{ pw_stream_events e {};
   e.version = PW_VERSION_STREAM_EVENTS;
   e.process = SndO::Mix;
   return e;
}

static const pw_stream_events kPWStrEv = setPWStreamEvents ();


void SndO::Mix (void *arg)
// pipewire real-time process callback
{ SndO      *me    = static_cast<SndO *>(arg);
  pw_buffer *pwBuf = pw_stream_dequeue_buffer (me->_pwStream);
   if (! pwBuf)  return;

  spa_data *plug = pwBuf->buffer->datas;
  auto     *buf  = static_cast<real4 *> (plug [0].data);
   if (! buf)  {pw_stream_queue_buffer (me->_pwStream, pwBuf);   return;}

// ok!  got our pwBuf > pl(ugin) data > buf
  const ubyt4 szFr = sizeof (real4) * 2;    // size of frame (real4 stereo)
        ubyt4  nFr = pwBuf->requested;
   if (nFr > 0) {
     const ubyt4 maxFr = plug [0].maxsize / szFr;
      nFr = me->_mix (buf, nFr, maxFr);

      plug [0].chunk->offset = 0;
      plug [0].chunk->stride = szFr;
      plug [0].chunk->size   = szFr * nFr;
   }
   pw_stream_queue_buffer (me->_pwStream, pwBuf);
}


void SndO::run ()
{  DBGTH("SndO::run");
// init - jump thru pipewire's hoops...
   pw_init (nullptr, nullptr);         // init n wipe
   _pwLoop = pw_main_loop_new (nullptr);
   if (! _pwLoop)
{DBG("pw_main_loop_new DIED !!");   return;}

  TStr l;
   StrFmt (l, "`d/`d", _nFr, _frq);
  pw_properties *props = pw_properties_new (
      PW_KEY_MEDIA_TYPE,     "Audio",
      PW_KEY_MEDIA_CATEGORY, "Playback",
      PW_KEY_MEDIA_ROLE,     "Production",
      PW_KEY_NODE_LATENCY,   l,
      nullptr
   );
   _pwStream = pw_stream_new_simple (
      pw_main_loop_get_loop (_pwLoop), _name, props, &kPWStrEv, this);
   if (! _pwStream)
{DBG("pw_stream_new_simple DIED");   return;}

  ubyte buf [1024];                    // Build the SPA audio format pod
  spa_pod_builder b = SPA_POD_BUILDER_INIT (buf, sizeof (buf));
  spa_audio_info_raw info {};
   info.format   = SPA_AUDIO_FORMAT_F32;    // i guess pw prefers this ??
   info.channels = 2;                       // always stereo fo mee
   info.rate     = _frq;
  const spa_pod *parm [1];
   parm [0] = spa_format_audio_raw_build (& b, SPA_PARAM_EnumFormat, & info);
   pw_stream_connect (
      _pwStream, PW_DIRECTION_OUTPUT, PW_ID_ANY,
      static_cast<pw_stream_flags> (PW_STREAM_FLAG_AUTOCONNECT |
                                    PW_STREAM_FLAG_MAP_BUFFERS |
                                    PW_STREAM_FLAG_RT_PROCESS),
      parm, 1);
TRC("pw_main_loop_run bgn");
   pw_main_loop_run (_pwLoop);         // <==all the good stuff happens herez
TRC("pw_main_loop_run end");

// quit - more pw hoops
   if (_pwStream) {pw_stream_destroy    (_pwStream);   _pwStream = nullptr;}
   if (_pwLoop)   {pw_main_loop_destroy (_pwLoop);     _pwLoop   = nullptr;}
}


void SndO::Init (const char *name, mixfunc pmix, ubyt4 frq, ubyt4 nFr)
// init the pw thread to pull audio from syn
{  _mix = pmix;   _frq = frq;   _nFr = nFr;   _name = name;
   _pwLoop   = nullptr;
   _pwStream = nullptr;
   start ();                           // run thread for pipewire event loop
}


void SndO::Quit ()
{  if (_pwLoop)  pw_main_loop_quit (_pwLoop);
   wait ();
}

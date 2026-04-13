// snd.h - sound via pipewire - out only
// does pipewire require all these c++ standard libraries things?  hmmm...

#pragma once
#include <atomic>
#include <functional>
#include <thread>

struct pw_main_loop;
struct pw_stream;

class SndO {
public:
   bool open (const char *name, pfunc mix, ubyt4 frq = 48000, ubyte4 nFr = 64);
   bool shut ();

private:
   pw_main_loop     *_pwLoop;          // pipewire handles
   pw_stream        *_pwStream;
   std::thread       _thread;          // thread and run state
   std::atomic<bool> _run {false};
   pfunc _mix;                         // mix func (syn)
   ubyt4 _frq, _nFr;                   // samples/sec from open
};

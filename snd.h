// snd.h - sound via pipewire - out only
// does pipewire require all these c++ standard libraries things?  hmmm...

#pragma once
#include "os.h"
#include <atomic>
#include <functional>
#include <thread>

struct pw_main_loop;
struct pw_stream;

typedef real4 smp;
typedef ubyt4 (*mixfunc)(smp *buf, ubyt4 nFr, ubyt4 maxFr);

class SndO {
public:
   bool Open (const char *name, mixfunc mix, ubyt4 frq = 48000, ubyt4 nFr = 64);
   void Shut ();
   static void Mix (void *data);       // pipewire main callback

private:
   pw_main_loop     *_pwLoop;          // pipewire handles
   pw_stream        *_pwStream;
   std::thread       _thread;          // thread and run state
   std::atomic<bool> _run {false};
   mixfunc           _mix;             // what mix calls to get audio buf
   ubyt4             _frq, _nFr;       // samples/sec, #frames from open
};

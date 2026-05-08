// snd.h - sound via pipewire - out only
// does pipewire require all these c++ standard libraries things?  hmmm...

#pragma once
#include "os.h"
#include <QThread>
#include <pipewire/pipewire.h>

typedef real4 smp;
typedef ubyt4 (*mixfunc)(smp *buf, ubyt4 nFr, ubyt4 maxFr);

class SndO: public QThread {
   Q_OBJECT
public:
   void Init (const char *name, mixfunc mix, ubyt4 frq = 48000, ubyt4 nFr = 64);
   void Quit ();
   static void Mix (void *data);       // pipewire main callback

protected:
   void run ()  override;

private:
   pw_main_loop *_pwLoop;              // pipewire handles
   pw_stream    *_pwStream;
   const char   *_name;
   mixfunc       _mix;                 // what mix calls to get audio buf
   ubyt4         _frq, _nFr;           // samples/sec, #frames from open
};

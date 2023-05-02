// timer.h - a millisec timer (1/1000 sec)

#ifndef TIMER_H
#define TIMER_H

#include "os.h"
#include <QThread>                        // need QThread,etc
#include "poll.h"
#include "sys/timerfd.h"

inline ubyt4 QTm ()
{ timespec ts;
   clock_gettime (CLOCK_REALTIME, & ts);
   return ts.tv_sec*1000 + ts.tv_nsec / 1000000;
}


class DurTimer {                       // handy lil debuggin' guy
public:
   DurTimer ()            {_end = 0;   _off = false;   Bgn ();}
   void  Bgn ()           {_bgn = QTm ();}
   void  End ()           {_end = QTm ();}
   ubyt4 Dur ()           {return _end - _bgn;}
   ubyt4 DurNow ()        {End ();   return Dur ();}
   void  SetOff (bool tf) {_off = tf;}
   bool  Off ()           {return _off;}
private:
   bool  _off;                         // off means "not timin at the moment"
   ubyt4 _bgn, _end;
};


//------------------------------------------------------------------------------
#define M_BEAT   ((ubyt4)192)          // my prefered resolution (ticks/qnote)
#define M_WHOLE  (M_BEAT*4)            // whole note


class Timer: public QThread {
   Q_OBJECT

public:
   ubyt4 _tempo;

private:
   bool  _run, _pause;                 // pausing?  (not bumping msec,time)
   ubyt4 _msec,           _msecSig,    // msec accumulator, msec to sig on or 0
         _time, _timeErr, _timeSig,    // songtime, fractional accumulator,
                             _pSig;    // time to sig on or 0;  prv for dbg'n
   void run () override
   { struct pollfd     pfd;
     struct itimerspec ts;
     uint64_t          ln, x;
TRC("Timer bgn");
      if (-1 == (pfd.fd = timerfd_create (CLOCK_MONOTONIC, 0)))
         {DBG("timerfd_create failed `s",   strerror (errno));   _run = false;}
      pfd.events = POLLIN;
      MemSet (& ts, 0, sizeof (ts));   // millisec interval
      ts.it_value.tv_nsec = ts.it_interval.tv_nsec = 1000000;
      if (-1 == timerfd_settime (pfd.fd, 0, & ts, nullptr))
         {DBG ("timerfd_settime failed `s", strerror (errno));   _run = false;}
      while (_run) {
         if (poll (& pfd, 1, 100) < 0)
            {DBG ("poll failed: `s",        strerror (errno));   _run = false;}
         if (pfd.revents & POLLIN)
            if ((ln = read (pfd.fd, & x, sizeof (x))) < sizeof (x))
               {DBG("timer read failed");                        _run = false;}
         _msec++;                      // TICK !!!
         if    (_msecSig && (_msec >= _msecSig))      // msecs always bump
               {_msecSig = 0;                       emit TimerMsEv ();}
         if (! _pause) {                              // time only bumps unpozd
            _timeErr += (_tempo * 2);
            while (_timeErr >= 625)  {_time++;  _timeErr -= 625;}
            if (_timeSig && (_time >= _timeSig))
               {_pSig = _timeSig;   _timeSig = 0;   emit TimerEv ();}
         }
      }
      if (pfd.fd != -1)  close (pfd.fd);
      _run = false;
TRC("Timer end");
   }

public:
   Timer ()
   {  _run = true;   _pause = false;   _tempo = 120;
      _msec = _msecSig = _time = _timeSig = _timeErr = _pSig = 0;
      connect (this, & Timer::finished, this, & QObject::deleteLater);
      start ();
   }

  ~Timer ()  {if (_run)  {_run = false;   wait ();}}

   bool  Pause ()  {return _pause;}
   ubyt2 Tempo ()  {return (ubyt2)_tempo;}
   ubyt4 Get ()    {return _time;}
   ubyt4 MS ()     {return _msec;}
   ubyt4 Sig ()    {return _timeSig;}

   void SetPause (bool pause, ubyt4 msSig = 0)
   {  if (pause) {
         if (msSig)  _msecSig = _msec + msSig;
         _timeErr = 0;                 // for when time restarts
      }
      _pause = pause;
   }

   void SetTempo (ubyt2 tempo)  {
      _tempo = tempo;
TRC("Timer.SetTempo `d", tempo);
   }

   void Set (ubyt4 time)
   {  _time = time;   _timeErr = 0;   if (! time) _msec = 0;  }

   void SetSig (ubyt4 time)  {_timeSig = time;}

signals:
   void TimerEv ();                    // hit a time
   void TimerMsEv ();                  // ms delay expired
};


#endif  // TIMER_H

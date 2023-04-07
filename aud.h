// aud.h - deal with alsa pcm...  only out

#ifndef AUD_H
#define AUD_H

#include "os.h"

class AudO {
public:
   AudO ()  {}

   void Wipe ()
   {
   }


   void Pause (bool p)
   {
   }


   void Open ()
   {
   }


   void *GetBuf ()
   {  return nullptr;
   }


   void  PutBuf ()
   {
   }


   void Shut ()
   {
   }

public:
   ubyt4 _frq, _len;
   ubyte _bits;
   bool  _float;

private:
};


#endif  // AUD_H

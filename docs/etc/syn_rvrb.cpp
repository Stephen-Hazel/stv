// syn_rvrb.cpp

// These assume 44.1KHz sample rate so adjust em
const ubyt4 LEN_COMB [NUM_COMB] =
                               {1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617};
const ubyt4 LEN_ALLP [NUM_ALLP] = {556, 441, 341, 225};

void Reverb::Update ()                 // reinit given new params
{  wet1 = wet * (width / 2 + 0.5f);   wet2 = wet * ((1 - width) / 2);
   for (sbyt4 i = 0;  i < NUM_COMB;  i++)
      {combL [i]->setfeedback (room);   combL [i]->setdamp (damp);
       combR [i]->setfeedback (room);   combR [i]->setdamp (damp);}
}

void Reverb::Mix (real *in, real *mixL, real *mixR)
{ sbyt4 i;
  real  outL, outR, input;
   for (ubyt4 k = 0;  k < Sy->_nFr;  k++) {
      outL = outR = 0;
      input = (2 * in [k] + DC_OFFSET) * REVERB_GAIN;   // gain is fixed
      for (i = 0;  i < NUM_COMB;  i++)  {combL [i]->mix (input, & outL);
                                         combR [i]->mix (input, & outR);}
      for (i = 0;  i < NUM_ALLP;  i++)  {allpL [i]->mix (       & outL);
                                         allpR [i]->mix (       & outR);}
      outL -= DC_OFFSET;
      outR -= DC_OFFSET;
      mixL [k] += outL * wet1 + outR * wet2;
      mixR [k] += outR * wet1 + outL * wet2;
   }
}

Reverb::Reverb ()
{ ubyt4 i;
  real  lenSc = Sy->_frq / 44100.0;
   for (i = 0;  i < NUM_COMB;  i++) {
      combL [i] = new Comb    ((sbyt4)( LEN_COMB [i]             * lenSc));
      combR [i] = new Comb    ((sbyt4)((LEN_COMB [i]+STEREO_SPR) * lenSc));
   }
   for (i = 0;  i < NUM_ALLP;  i++) {
      allpL [i] = new Allpass ((sbyt4)( LEN_ALLP [i]             * lenSc));
      allpR [i] = new Allpass ((sbyt4)((LEN_ALLP [i]+STEREO_SPR) * lenSc));
   }
   Set ();
}

Reverb::~Reverb ()
{ ubyt4 i;
   for (i = 0;  i < NUM_COMB;  i++)  {delete combL [i];   delete combR [i];}
   for (i = 0;  i < NUM_ALLP;  i++)  {delete allpL [i];   delete allpR [i];}
}

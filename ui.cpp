// ui.cpp

#include "ui.h"
#include <QClipboard>
#include <QMimeData>


// clipboard (ascii only)
void ClipPut (QApplication *a, char *s)
{ QClipboard *c = a->clipboard ();
   c->clear ();   c->setText (s);
}

ulong ClipGetLen (QApplication *a)
{  return a->clipboard ()->text ().length ();  }

char *ClipGet (QApplication *a, char *s, ulong siz)
{  *s = '\0';
   if ((ClipGetLen (a) + 1) <= siz)
      StrCp (s, CC(UnQS (a->clipboard ()->text ())));
   return s;
}

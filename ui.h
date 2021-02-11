//      ui.h - ui stuph like window controls, etc
#ifndef UI_H
#define UI_H

#include "os.h"

#include <QApplication>
#include <QMainWindow>
#include <QFileDialog>
#include <QScreen>
#include <QSettings>
#include <QTimer>
#include <QString>
#include <QMessageBox>
#include <QWindow>

extern QApplication *APP;
extern QMainWindow  *WIN;

inline char *UnQS (QString s)  {return CC(s.toLocal8Bit ().constData ());}

inline void QSleep (int ms)
{ QEventLoop loop;
  QTimer t;
   t.connect(& t, & QTimer::timeout, & loop, & QEventLoop::quit);
   t.start (ms);
   loop.exec ();
}


// clipboard (ascii only) ______________________________________________________
void  ClipPut    (QApplication *a, char *s);
ulong ClipGetLen (QApplication *a);
char *ClipGet    (QApplication *a, char *s, ulong siz);


//______________________________________________________________________________
//inline void RUN  (char *cmd, char *dat)     // edit, blah.txt
//{}

//inline void BOOT (char *exe, char *arg)    // pa\\blah.exe, arg1 arg2 etc
//{}

//inline int RunWait (char *cmdLn, bool show = false, char *curdir = NULL)
//{}


//______________________________________________________________________________
inline void WinLoad (QMainWindow *w, char const *gr, char const *fn)
{ QSettings s (gr, fn);
   if (s.contains ("size"))  w->resize (s.value ("size").toSize ());
   w->move (0, 0);   w->show ();

   if (s.contains ("scr")) {
     QString scr = s.value ("scr").toString ();
     QList<QScreen *> scrLs = QGuiApplication::screens ();
      for (int i = 0;  i < scrLs.size ();  i++)  if (scrLs [i]->name () == scr)
         {w->windowHandle ()->setScreen (scrLs [i]);   break;}
   }
   if (s.contains ("pos"))  w->move (s.value ("pos").toPoint ());
}

inline void WinSave (QMainWindow *w, char const *gr, char const *fn)
{ QSettings s (gr, fn);
   s.setValue ("size", w->size ());
   s.setValue ("pos",  w->pos ());
   s.setValue ("scr",  w->windowHandle ()->screen ()->name ());
}


//______________________________________________________________________________
inline void Hey (char const *msg)
{ QMessageBox m;
   m.setText (msg);   m.exec ();
}

inline bool YNo (char const *msg, char const *inf = nullptr)
{ QMessageBox m;
   m.setText (msg);
   if (*inf)  m.setInformativeText (inf);
   m.setStandardButtons (QMessageBox::Yes | QMessageBox::No);
   m.setDefaultButton   (QMessageBox::Yes);
   return (m.exec () == QMessageBox::Yes) ? true : false;
}


inline bool AskR (QMainWindow *w, char *name, char const *titl)
{ QString dir = QDir::homePath ();
   if (*name)  dir = Fn2Path (name);
   QString fn = QFileDialog::getOpenFileName (w, titl, dir, "All Files (*)");
   if (fn.isEmpty ())  return false;
   StrCp (name, CC(UnQS (fn)));
   return true;
}

inline bool AskW (QMainWindow *w, char *name, char const *titl)
{ QString dir = QDir::homePath ();
   if (*name)  dir = Fn2Path (name);
   QString fn = QFileDialog::getSaveFileName (w, titl, dir, "All Files (*)");
   if (fn.isEmpty ())  return false;
   StrCp (name, CC(UnQS (fn)));
   return true;
}

inline bool AskDir (QMainWindow *w, char *name, char const *titl)
{ QString dir = QDir::homePath ();
   if (*name)  dir = name;
   dir = QFileDialog::getExistingDirectory (
      w, titl, dir, QFileDialog::ShowDirsOnly |
                    QFileDialog::DontResolveSymlinks);
   if (dir.isEmpty ())  return false;
   StrCp (name, CC(UnQS (dir)));
   return true;
}


#endif

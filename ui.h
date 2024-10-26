//      ui.h - ui stuph like window controls, etc - all da Qt
#ifndef UI_H
#define UI_H

#include "os.h"

#include <QApplication>
#include <QMainWindow>
#include <QDebug>
#include <QFileDialog>
#include <QScreen>
#include <QSettings>
#include <QTimer>
#include <QString>
#include <QMessageBox>
#include <QWindow>
#include <QDialog>
#include <QSplitter>
#include <QVBoxLayout>
#include <QToolBar>
#include <QToolButton>
#include <QLabel>
#include <QPushButton>
#include <QLineEdit>
#include <QTextEdit>
#include <QSpinBox>
#include <QColor>
#include <QPalette>
#include <QLCDNumber>
#include <QTableWidget>
#include <QHeaderView>
#include <QComboBox>
#include <QCheckBox>
#include <QSlider>
#include <QStyledItemDelegate>
#include <QGraphicsView>
#include <QEvent>
#include <QWheelEvent>
#include <QKeyEvent>
#include <QPaintEvent>
#include <QResizeEvent>


inline char *UnQS (QString s)  {return CC(s.toLocal8Bit ().constData ());}
inline char *HxQS (QString s)  {return UnQS (s.toUtf8 ().toHex (':'));}

inline void Zzz (int ms)               // shouldn't use this inna gui...
{ QEventLoop loop;
  QTimer     t;
   t.connect (& t, & QTimer::timeout, & loop, & QEventLoop::quit);
   t.start (ms);   loop.exec ();
}


//______________________________________________________________________________
struct QtEr {                          // my layer on toppa Qt
public:
   QApplication *A ()  {return _a;}
   QMainWindow  *W ()  {return _w;}

   ubyt2 FontH ()
   { QFontMetrics fm (_a->font ());
      return fm.capHeight () + 1 + fm.descent ();
   }

   ubyte NArg ();                      // argv [0] AIN'T an arg !  [0]=1st arg
   char  *Arg (ubyte pos);             // returns "" if beyond argc

   ubyt4 ClipLen ();                   // clipboard (ascii only)
   char *ClipGet (char *s, ubyt4 siz);
   void  ClipPut (char *s);

   void  Hey (char const *msg);        // usual dialogs
   bool  YNo (char const *msg);
   bool  AskR   (char *name, char const *titl);
   bool  AskW   (char *name, char const *titl);
   bool  AskDir (char *name, char const *titl);
                                       // fer main
   void  Init (QApplication *ia, QMainWindow *iw, const char *ttl,
               char fixw = '\0');
   void  WinLoad (QSplitter *spl = nullptr);
   void  WinSave (QSplitter *spl = nullptr);
   void  DlgLoad (QDialog *d, QString nm, QSplitter *spl = nullptr);
   void  DlgSave (QDialog *d, QString nm, QSplitter *spl = nullptr);
   void  DlgMv   (QDialog *d, QPointF p, char const *anch);
   void  SetTtl  (char *t);
   void  FullSc  (bool b)
   {  if (b) _w->setWindowState (_w->windowState () |   Qt::WindowFullScreen );
      else   _w->setWindowState (_w->windowState () & (~Qt::WindowFullScreen));
   }
   int   Loop ()  {if (_q)  return 0;
                   int r = _a->exec ();   return r;}
   void  Quit ()  {_q = true;   _a->exit (0);}

private:
   QApplication *_a;
   QMainWindow  *_w;
   char          _fixw;
   bool          _q;
   TStr          _ttl;
};
extern QtEr Gui;


//______________________________________________________________________________
// in main:   app.installEventFilter (new EvDump);
/*
class EvDump: public QObject {
   Q_OBJECT
protected:
   bool eventFilter (QObject *o, QEvent *e) override
   { TStr buf;
      if ((e->type () != QEvent::HoverMove    ) &&
          (e->type () != QEvent::MouseMove    ) &&
          (e->type () != QEvent::ToolTip      ) &&
          (e->type () != QEvent::PolishRequest) &&
          (e->type () != QEvent::LayoutRequest) &&
//          (e->type () != QEvent::ChildAdded   ) &&
//          (e->type () != QEvent::ChildRemoved ) &&
          (e->type () != QEvent::ChildPolished) &&
//          (e->type () != QEvent::Hide         ) &&
//          (e->type () != QEvent::Expose       ) &&
//          (e->type () != QEvent::Enter        ) &&
//          (e->type () != QEvent::Leave        ) &&
          (e->type () != QEvent::Destroy      ) &&
          (e->type () != QEvent::UpdateLater  ))
         qDebug () << NowMS (buf) << e->type () << o;
      return QObject::eventFilter (o, e);
   }
};


#include <QMetaObject>
#include <QMetaMethod>

void DumpSig ()
{ QTableWidget ob;
  QMetaObject mo = ob.staticMetaObject;
   do {
      for (int i = 0;  i < mo.methodCount ();  i++) {
        QMetaMethod m = mo.method (i);
         if (m.methodType () == QMetaMethod::Signal)
            DBG("`s", UnQS (m.methodSignature ()));
      }
      DBG("-----^");
   } while ((mo = mo->superClass ()));
}
*/


//______________________________________________________________________________
class CtlTBar {
public:
   CtlTBar (QMainWindow *w, const char *tipIcoKey, const char *nm = "");
   CtlTBar (QDialog *d,     const char *tipIcoKey);
   QAction *Act (ubyte p)
   {  if (p >= _na)  {DBG("BAD toolbar button=`d :(", p);   return nullptr;}
      return _ac [p];
   }
   QToolButton *Btn (ubyte p)
   { QAction *a = Act (p);
      return a ? dynamic_cast<QToolButton *>(_w->widgetForAction (a)) : nullptr;
   }
private:
   QToolBar *_w;
   ubyte     _na;
   QAction  *_ac [32];
};


class CtlLabl {
public:
   CtlLabl (QLabel *w)  {_w = w;}
   void Set (char *s)   {_w->setText (s);}
private:
   QLabel *_w;
};


class CtlBttn {
public:
   CtlBttn (QPushButton *w)  {_w = w;}
   char *Get    (char *s)   {return UnQS (_w->text ());}
   void  Set    (char *s)   {_w->setText    (s);}
   void  Enable (bool  s)   {_w->setEnabled (s);}
private:
   QPushButton *_w;
};


class CtlChek {
public:
   CtlChek (QCheckBox *c)  {_c = c;}

   bool Get ()
   {  return (_c->checkState () == Qt::Checked) ? true : false;  }

   void Set (bool tf)
   {  _c->setCheckState (tf ? Qt::Checked : Qt::Unchecked);  }

private:
   QCheckBox *_c;
};


class CtlLine {
public:
   CtlLine (QLineEdit *w)  {_w = w;}
   char *Get ()            {return UnQS (_w->text ());}
   void  Set (char *s)     {_w->setText (s);}
private:
   QLineEdit *_w;
};


class CtlLcd {
public:
   CtlLcd (QLCDNumber *w)  {_w = w;   _w->setSmallDecimalPoint (true);}
   void SetDg (ubyte d)    {_w->setDigitCount (d);}
   void Set (char *n)      {_w->display (n);}
private:
   QLCDNumber *_w;
};


class CtlSpin {
public:
   CtlSpin (QSpinBox *w, sbyt4 min = 0, sbyt4 max = 0)
   {  _w = w;   if (min) _w->setMinimum (min);
                if (max) _w->setMaximum (max);  }
   sbyt4 Get ()     {return _w->value ();}
   void  Set (sbyt4 i)  {_w->setValue (i);}
private:
   QSpinBox *_w;
};


class CtlText {
public:
   CtlText (QTextEdit *w)    {_w = w;
                              _w->document ()->setDocumentMargin (0.0);}
   QColor Fg ()  {return Gui.A ()->palette ().color (QPalette::Text);}
   QColor Hi ()  {return Gui.A ()->palette ().color (QPalette::Highlight);}
   void   SetFg (QColor fg)  {_w->setTextColor (fg);}
   void   Clr ()             {_w->clear ();}
   char  *Get ()             {return UnQS (_w->toPlainText ());}
   void   Add (char *txt)    {_w->insertPlainText (txt);}
   QTextEdit *_w;
};


class CtlList {
public:
   CtlList (QComboBox *w, char *ls = nullptr)  {_w = w;   SetLs (ls);}

   void ClrLs ()      {_w->clear ();}

   void InsLs (char *s, ubyt2 p = 65535)
   {  if (p == 65535)  _w->addItem (s);  else _w->insertItem (p, s);  }

   void SetLs (char *ls = nullptr)
   {  if (ls == nullptr)  return;
     char *s;
      ClrLs ();  for (s = ls;  *s;  s = & s [StrLn (s)+1])  InsLs (s);
   }

   ubyt2 Get    ()         {return _w->currentIndex ();}
   void  Set    (ubyt2 p)      {_w->setCurrentIndex (p);}
   char *GetS   (char *s)  {return StrCp (s, UnQS (_w->currentText ()));}
   void  SetS   (char *s)                      {_w->setCurrentText (s);}
   void  Enable (bool s)   {_w->setEnabled (s);}
   void  SetVis (ubyt2 n)  {_w->setMaxVisibleItems (n);}
private:
   QComboBox *_w;
};


class CtlSldr {
public:
   CtlSldr (QSlider *w, sbyt4 min = 0, sbyt4 max = 0)
   {  _w = w;   if (min) _w->setMinimum (min);
                if (max) _w->setMaximum (max);  }
   sbyt4 Get ()     {return _w->value ();}
   void  Set (sbyt4 i)  {_w->setValue (i);}
private:
   QSlider *_w;
};


//______________________________________________________________________________
// tables need these dumb tricky "QStyledItemDelegate"s so they
// can drop combos :/

// callback to set type of edit ('e'dit, 'l'ist, '\0') and zz droplist if l
typedef char (*ppop)(char *ls, ubyt2 r, ubyte c);

class SID_EvF: public QObject {        // needed by SIDlg to autodrop lists sigh
   Q_OBJECT
protected:
   bool eventFilter (QObject *ob, QEvent *ev) override
   { QComboBox *cb = qobject_cast<QComboBox *>(ob);
      if (cb && (ev->type() == QEvent::HoverEnter))
         {cb->showPopup ();   return true;}
      return false;
   }
};


//______________________________________________________________________________
// when a table cell with an editor gets clicked on:
//    createEditor, updateEditorGeometry, setEditorData, and tons of paints
// when a CtlList/combo gets PICKED:
//    cbChanged
//       emit commitData
//          setModelData calls mod->setData which calls setEditorData
//       emit closeEditor
//          does it remove editor i hope?
// but when I update a table cell...  AGAIN...
//    createEditor, updateEditorGeometry, setEditorData, and tons of paints, AND
//          setModelData calls mod->SetData which calls setEditorData
//______________________________________________________________________________
class SIDlg: public QStyledItemDelegate {
   Q_OBJECT
private:
   char    *_ed;                       // of CtlTabl
   ppop     _pop;                      // of app
   QObject *_ef;                       // dumb thing ta auto drop CComboBox

public:
   SIDlg (QObject *par, char *ed, ppop pop)
   : QStyledItemDelegate (par)
   {  _ed = ed;   _pop = pop;  _ef = new SID_EvF;}
  ~SIDlg ()  {}


   void     paint         (QPainter *p, const QStyleOptionViewItem &opt,
                                        const QModelIndex &ind)  const override
   {  if (! opt.icon.isNull ())
           {p->save ();   opt.icon.paint (p, opt.rect);
            p->restore ();}
      else  QStyledItemDelegate::paint (p, opt, ind);
   }


   void     updateEditorGeometry (
                           QWidget *ed, const QStyleOptionViewItem &opt,
                                        const QModelIndex &ind)  const override
   { QComboBox *cb = qobject_cast<QComboBox *>(ed);
//DBG("SIDlg::updateEditorGeometry bgn");
      if (cb)  cb->setGeometry (opt.rect);
      else  QStyledItemDelegate::updateEditorGeometry (ed, opt, ind);
//DBG("SIDlg::updateEditorGeometry end");
   }


   QWidget *createEditor  (QWidget *tb, const QStyleOptionViewItem &opt,
                                        const QModelIndex &ind)  const override
   // table cell w an editor got clicked
   { BStr bs;
     char ed, *s;
//DBG("SIDlg::createEditor row=`d col=`d _ed=`c",
      if (_pop == nullptr)  DBG("BUG! edit cell and no pop func!!");
//ind.row (), ind.column (), _ed [ind.column ()]);
      if (_ed [ind.column ()] == '\0')      // this col never edits
                           return nullptr;  // (prob never happens)
      ed = _pop (bs, ind.row (), ind.column ());
      if (ed == '\0')      return nullptr;  // read only fer now at least
      if (ed == 'l') {                      // l = droplist
         if (*bs == '\0')  return nullptr;  // no list - read only fer now
        QComboBox *cb = new QComboBox (tb);
         cb->setMaxVisibleItems (16);
         connect (cb, QOverload<int>::of(&QComboBox::activated),
                  this,      & SIDlg::cbChanged);
//       connect (cb, QOverload<int>::of(&QComboBox::currentIndexChanged),
//                this,      & SIDlg::cbChanged);
         cb->setSizeAdjustPolicy (QComboBox::AdjustToContents);
//cb->view ()->window ()->setWidth ();
         for (s = bs;  *s;  s = & s [StrLn (s)+1])
            cb->addItem (StrCm (s, CC("-")) ? s : "");
         cb->installEventFilter (_ef);
//DBG("SIDlg::createEditor ^ end");
         return cb;
      }                                     // e = string edit
      return    QStyledItemDelegate::createEditor (tb, opt, ind);
   }


   void     setEditorData (QWidget *ed, const QModelIndex &ind)  const override
   { QComboBox *cb = qobject_cast<QComboBox *>(ed);
      if (cb) {
//DBG("SIDlg::setEditorData ^ bgn");
        TStr s, t;
         StrCp (s, UnQS (ind.data (Qt::EditRole).toString ()));
        int i = cb->findText (s);
         if (i < 0) {                     // no exact match, look fer edit bein
            i = 0;                        // prefix of a list str;  else pos 0
            for (int j = 0;  j < cb->count ();  j++) {
               StrCp (t, UnQS (cb->itemText (j)));
               if (! MemCm (t, s, StrLn (t)))  {i = j;   break;}
            }
         }
         cb->setCurrentIndex (i);
//       cb->move (opt.rect.x (), opt.rect.y ());
//       cb->showPopup ();          // and doesn't work so tryin eventFilter
//DBG("SIDlg::setEditorData ^ end");
      }
      else  QStyledItemDelegate::setEditorData (ed, ind);
   }


   void     setModelData  (QWidget *ed, QAbstractItemModel *mod,
                                        const QModelIndex &ind)  const override
   { QComboBox *cb = qobject_cast<QComboBox *>(ed);
//DBG("SIDlg::setModelData `s", cb?"^":"_");
      if (cb) {
//DBG("   r=`d c=`d s=`s",
//ind.row (), ind.column (), UnQS (cb->currentText ()));
         mod->setData (ind, cb->currentText (), Qt::EditRole);
//DBG("   back from mod->setData");
      }
      else  QStyledItemDelegate::setModelData (ed, mod, ind);
//DBG("SIDlg::setModelData end");
   }


public slots:
   void cbChanged ()                   // got a pick !
   { QComboBox *cb = qobject_cast<QComboBox *>(sender ());
//DBG("SIDlg::cbChanged commitData");
      emit commitData  (cb);
//DBG("SIDlg::cbChanged closeEditor");
      emit closeEditor (cb);
//DBG("SIDlg::cbChanged end");
   }
};


//______________________________________________________________________________
class CtlTabl {                        // ez outsidea droppin a combo :/
public:
   CtlTabl ()  {_t = nullptr;   _nr = _tr = _ih = _tc = 0;}
  ~CtlTabl ()  {}

// hdr is zz string of labels
//  >| prefix means right or center just
//  * prefix means icon
//  _ prefix means edit (string or droplist edit)
   void  Init    (QTableWidget *t, const char *hdr, ppop pop = nullptr,
                  char wrap = '\0');
   void  SetColWrapOK (ubyte c);
   void  SetRowH (ubyt2 h);
   ubyt2 ColW    (ubyte c);
   void  SetColW (ubyte c, ubyt2 w);
   ubyt2 NRow    ();
   ubyte NCol    ();
   ubyt2 CurRow  ();
   ubyte CurCol  ();
   char *Get     (ubyt2 r, ubyte c);
   void  Set     (ubyt2 r, ubyte c, char *s);
   void  HopTo   (ubyt2 r, ubyte c);
   void  SetColor (ubyt2 r, QColor c);

   void  Open    ();
   void  Put     (char **rp);
   void  Shut    (bool rehop = false);

private:
   QTableWidget *_t;
   char  _ju [40];
   char  _ed [40];
   ubyt2 _nr, _tr, _ih;
   ubyte      _tc;
   bool  _wr;
};


//______________________________________________________________________________
// color
#define GRAY(n)  (QColor(n,n,n))

const QColor CBLACK = GRAY(0);              // RGB fer them oole DOS colors
const QColor CDGRAY = GRAY(128);
const QColor CLGRAY = GRAY(192);
const QColor CWHITE = GRAY(255);

const QColor CRED   = QColor(255,000,000);  // ...bright colors
const QColor CGRN   = QColor(000,255,000);
const QColor CBLU   = QColor(000,000,255);
const QColor CYEL   = QColor(255,255,000);
const QColor CCYAN  = QColor(000,255,255);
const QColor CMAG   = QColor(255,000,255);

const QColor CDRED  = QColor(064,000,000);  // ...dark colors
const QColor CDGRN  = QColor(000,064,000);
const QColor CDBLU  = QColor(000,000,128);
const QColor CBRN   = QColor(128,128,000);
const QColor CDCYAN = QColor(000,064,064);
const QColor CDMAG  = QColor(128,000,128);

inline real HSL_hue (real v1, real v2, real vh)
{  if (vh < 0.0)  vh += 1.0;
   if (vh > 1.0)  vh -= 1.0;
   if ((6 * vh) < 1.0)  return v1 + (v2 - v1) * 6.0 * vh;
   if ((2 * vh) < 1.0)  return v2;
   if ((3 * vh) < 2.0)  return v1 + (v2 - v1) * ((2.0 / 3.0 - vh) * 6.0);
   return v1;
}

inline QColor HSL (int hue, int sat, int lum)
// hue 0-359 sat 0-100, lum 0-100
{ real rhue, rsat, rlum,  v1, v2,  r, g, b;
   rhue = (real)hue / 360.0;   rsat = (real)sat / 100.0;
                               rlum = (real)lum / 100.0;
   if (sat == 0)  r = g = b = rlum;
   else {
      if (rlum < 0.5)  v2 = rlum * (1.0 + rsat);
      else             v2 = (rlum + rsat) - (rlum * rsat);
      v1 = 2.0 * rlum - v2;
      r = HSL_hue (v1, v2, rhue + 1.0/3.0);
      g = HSL_hue (v1, v2, rhue);
      b = HSL_hue (v1, v2, rhue - 1.0/3.0);
   }
   r *= 255.0;   if ((r-(real)((int)r)) >= 0.495)  r += 1.0;
   g *= 255.0;   if ((g-(real)((int)g)) >= 0.495)  g += 1.0;
   b *= 255.0;   if ((b-(real)((int)b)) >= 0.495)  b += 1.0;
   return QColor ((int)r, (int)g, (int)b);
}

class ColRng {
public:
   void Init (QColor *tab, ubyte max = 127, ubyte lum = 178, ubyte sat = 255)
   // colors that gradually step from 0-max =>
   //                                       green..cyan..blue..purple..red..yell
   // green..yellow = 2/6..5.9/6, wrap to 0/6..1/6 (throw out 1.2/6 to 1.9/6)
   { ubyt2 i;
     ubyte t;
      for (i = 0; i <= (ubyt2)max; i++) {
         t = (ubyte)(((2*255/6) + i * (5*255/6) / max) % 255);
         tab [i] = HSL2RGB (t, sat, lum);
      }
   }

   QColor HSL2RGB (int hue, int sat, int lum)
   // hue = 0..255 = red..yellow..green..cyan..blue..purple
   // sat = 0..255 = grey(achromatic)..chromatic
   // lum = 0..255 = black..white
   { sbyt2 v, m, split6, fract, vsf, mid1, mid2;
     ubyte r, g, b;
      v = (lum < 128) ? (lum * (256 + sat)) >> 8
                      : (((lum + sat) << 8) - lum * sat) >> 8;
      if (v <= 0)  r = g = b = 0;
      else {
         m      = lum + lum - v;
         hue   *= 6;
         split6 = hue >> 8;
         fract  = hue - (split6 << 8);
         vsf    = v * fract * (v - m) / v >> 8;
         mid1   = m + vsf;
         mid2   = v - vsf;
         switch (split6) {
         case 0:  r = (ubyte)v;      g = (ubyte)mid1;   b = (ubyte)m;     break;
         case 1:  r = (ubyte)mid2;   g = (ubyte)v;      b = (ubyte)m;     break;
         case 2:  r = (ubyte)m;      g = (ubyte)v;      b = (ubyte)mid1;  break;
         case 3:  r = (ubyte)m;      g = (ubyte)mid2;   b = (ubyte)v;     break;
         case 4:  r = (ubyte)mid1;   g = (ubyte)m;      b = (ubyte)v;     break;
         case 5:  r = (ubyte)v;      g = (ubyte)m;      b = (ubyte)mid2;  break;
         }
      }
      return QColor (r,g,b);
   }
};

// window.palette ().window ().color ().lightnessF () - check bg for dark mode

class Canvas: public QPainter {
public:
   Canvas (QPaintDevice *dev): QPainter (dev)  {}
   Canvas ()  {}
  ~Canvas ()  {}

   QPainter *Painter ()  {return this;}

   void Dump () {
DBG("antiAlias=`b textAA=`b smoothPMapTrans=`b losslessImg=`b", // f t f f
testRenderHint(QPainter::Antialiasing),
testRenderHint(QPainter::TextAntialiasing),
testRenderHint(QPainter::SmoothPixmapTransform),
testRenderHint(QPainter::LosslessImageRendering));
   }
   void SetAA (bool b)  {setRenderHint (QPainter::Antialiasing, b);}
   void SetTA (bool b)  {setRenderHint (QPainter::TextAntialiasing, b);}
   void SetSm (bool b)  {setRenderHint (QPainter::SmoothPixmapTransform, b);}
   void SetLL (bool b)  {setRenderHint (QPainter::LosslessImageRendering, b);}

   void SetFont (char const *ff = nullptr, int pt = 12)
   {  _f = (ff == nullptr) ? Gui.A ()->font () : QFont (ff, pt);
     QFontMetrics fm (_f);
      _fasc = fm.ascent ();   _fdsc = fm.descent ();   _fcap = fm.capHeight ();
   }

   ubyt2 FontH ()  {return _fcap + 1 + _fdsc;}
/*
DBG("fh=`d fa=`d fd=`d avgCharW=`d capH=`d dpi=`d leading=`d "
"lineSpc=`d lineW=`d maxW=`d minLftBear=`d minRitBear=`d overlinePos=`d "
"strikePos=`d underlinePos=`d xH=`d lftBeari=`d ritBeari=`d horizAdv=`d",
fm.height(), _fasc, _fdsc,
fm.averageCharWidth(), _fcap, (int)fm.fontDpi(), fm.leading(),
fm.lineSpacing(), fm.lineWidth(), fm.maxWidth(), fm.minLeftBearing(),
fm.minRightBearing(), fm.overlinePos(), fm.strikeOutPos(),
fm.underlinePos(), fm.xHeight(), fm.leftBearing('i'), fm.rightBearing('i'),
fm.horizontalAdvance("0123456789"));
     QRect r = fm.boundingRect("0123456789");
     QRect s = fm.tightBoundingRect("0123456789");
DBG("bound `d `d `d `d", r.x(), r.y(), r.width(), r.height());
DBG("tight `d `d `d `d", s.x(), s.y(), s.width(), s.height());

QFontMetrics    values for NotoSans 12
H=22        always ascent+descent
ascent=17   height from baseline to top
descent=5   height below baseline for lowercase p's tail
capH=11         H from baseline of typical caps char like H, I (not nece O, A)
underlinePos=2  H from baseline to underscore (but DESCENT, not ascent)
strikePos=5     H from baseline to "strikeoutline" (?)
xH=9            H from baseline of lowercase x
overlinePos=18  H from baseline to "overline" (?)
leading=0   blank line space between 2 rows of text
lineSpc=22  dist between baselines in 2 rows of text (=H+leading)

avgCharW=9  W of, say 0
maxW=45     W of, say W
lineW=1     W of _ or strikeout
minLftBear=-10 usu neg.  w from horizAdv into next char's space
minRitBear=-10           w from origin into prev char's space
lftBeari=1
ritBeari=1
horizAdv=91
bound 0 -17 90 22
tight 0 -12 90 12
dpi=96
Noto Sans 14:
fh=26 fa=20 fd=6 avgCharW=11 capH=14 dpi=96 leading=0 lineSpc=26 lineW=1
maxW=54 minLftBear=-12 minRitBear=-12 overlinePos=21 strikePos=6
underlinePos=2 xH=10 lftBeari=1 ritBeari=1 horizAdv=109
*/
   void bgn (QPaintDevice *dev)  {begin (dev);   setFont (_f);}

   void SetMode (char opaq)
   {  setBackgroundMode ((opaq == 'o') ? Qt::OpaqueMode
                                       : Qt::TransparentMode);  }
   void SetFg (QColor c)  { QPen   p (c);   setPen   (p);  }
   void SetBg (QColor c)  { QBrush b (c);   setBrush (b);  }

   void Text (ubyt2 x, ubyt2 y, char *s)
   {  SetMode ('t');   drawText (x, y+_fcap+1, s);  }

   void TextV (ubyt2 x, ubyt2 y, char *s)   // vertical
   {  SetMode ('t');
      save ();   translate (x, y);   rotate (90);   drawText (0, 0, s);
      restore ();
   }
/*
drawText x,y point is at the BASELINE, and i dunno if 1 below or exactly ??
=i= want x,y to be top,left of text rect.
for regular horiz text,
I don't want dead space above the font (above capsH).
so using only capsH, not full ascent.  (and +1 i guess ???)
and for vert text, I don't want desc (p,q) space
and x,y is AT baseline, so a plain translate x,y and rotate 90
SEEM TO LINE UP :/

s = CC("HIOAW  0123456789_hioawpqg.,`~_");
drawText (400, 500+_fcap+1, s);
save();   translate (400, 500);   rotate (90);   drawText (0, 0, s);
restore ();
save ();
SetFg (CRED);
drawLine (400,500, 400+300,500);  // horz orig
drawLine (400,500, 400,    500+300);  // vert orig
restore ();

save ();   translate (500, 500);   rotate (0);     drawText (0, 0, s);
SetFg (CRED);
drawLine (0,0-_fa, 300,0-_fa);  // total ascent
drawLine (0,0-14 , 300,0-14);   // cap ascent
drawLine (0,0,     300,0);      // baseline
drawLine (0,0+_fd, 300,0+_fd);  // total descent
restore ();

save ();   translate (500, 500);   rotate (180);   drawText (0, 0, s);
SetFg (CRED);
drawLine (0,0-_fa, 300,0-_fa);  // total ascent
drawLine (0,0-14 , 300,0-14);   // cap ascent
drawLine (0,0,     300,0);      // baseline
drawLine (0,0+_fd, 300,0+_fd);  // total descent
restore ();

save ();   translate (100, 500);   rotate (90);    drawText (0, 0, s);
SetFg (CRED);
drawLine (0,0-_fa, 300,0-_fa);  // total ascent
drawLine (0,0-14 , 300,0-14);   // cap ascent
drawLine (0,0,     300,0);      // baseline
drawLine (0,0+_fd, 300,0+_fd);  // total descent
restore ();

save ();   translate (100, 500);   rotate (270);   drawText (0, 0, s);
SetFg (CRED);
drawLine (0,0-_fa, 300,0-_fa);  // total ascent
drawLine (0,0-14 , 300,0-14);   // cap ascent
drawLine (0,0,     300,0);      // baseline
drawLine (0,0+_fd, 300,0+_fd);  // total descent
restore ();
*/

   void TextC  (ubyt2 x, ubyt2 y, char *s, QColor c)
   {  SetFg (c);   Text  (x, y, s);  }

   void TextVC (ubyt2 x, ubyt2 y, char *s, QColor c)
   {  SetFg (c);   TextV (x, y, s);  }

   void Line (ubyt2 x1, ubyt2 y1, ubyt2 x2, ubyt2 y2)
   {  drawLine (x1, y1, x2, y2);  }

   void Rect (ubyt2 x, ubyt2 y, ubyt2 w, ubyt2 h, QColor c)
   {  SetMode ('t');   SetFg (c);   drawRect (x, y, w, h);  }

   void RectF (ubyt2 x, ubyt2 y, ubyt2 w, ubyt2 h, QColor c)
   {  fillRect (x, y, w, h, c);  }

   void Blt (QPixmap pm, ubyt2 x1, ubyt2 y1, ubyt2 w1, ubyt2 h1,
                         ubyt2 x2, ubyt2 y2, ubyt2 w2, ubyt2 h2)
   {  drawPixmap (x1, y1, w1, h1, pm, x2, y2, w2, h2);  }

   void Blt (QPixmap pm, ubyt2 x1, ubyt2 y1,
                         ubyt2 x2, ubyt2 y2, ubyt2 w, ubyt2 h)
   {  Blt (pm, x1, y1, w, h, x2, y2, w, h);  }

   void Blt (QPixmap pm, ubyt2 x, ubyt2 y)
   {  Blt (pm, x, y, 0, 0, pm.width (), pm.height ());  }

private:
   QFont _f;
   ubyt2 _fasc, _fdsc, _fcap;
};
/* NEd
  QColor fc (f), bc (b);
  QPen   penf (fc), penb (bc);
  QBrush            brs  (bc);
  QPainter p (Scr.pm);
   p.setFont (Gui.A ()->font ());
   p.setPen (penb);   p.setBrush (brs);
   p.drawRect (bgn*Scr.wLn/80, y*Scr.hCh, (end-bgn+1)*Scr.wLn/80, Scr.hCh);
   p.setPen (penf);
   p.drawText (bgn*Scr.wLn/80, y*Scr.hCh+Scr.hBL, s);
*/


/* RGB(0x01,0xB0,0xF0),             // good ltish blue
** RGB(0xFF,0x00,0x84),             // ok pink
** RGB(0x52,0x71,0xE0),             // ok purplish blue
**
** RGB(0x7B,0xF5,0x3D),             // x green
** RGB(0xFF,0xA3,0x00),             // x orange,brownish
** RGB(0xFF,0xFF,0x00),             // x yellow
** RGB(0x6B,0xBA,0x70),             // x ok green grey
** RGB(0xFC,0xB4,0xD9),             // x faint pink
** RGB(0xB3,0xBE,0xE6),             // x faint purple
** RGB(0xFF,0xAA,0x95),             // x another salmon
** RGB(0xBB,0x02,0x14),             // x another good red
** RGB(0xC1,0x34,0x80),             // x faintish pink
** RGB(0xDA,0x6B,0x9E),             // x ?beige
**
** RGB(0x33,0x90,0x64),      RGB(0x12,0x80,0x73),
** RGB(0x0F,0x66,0x79),      RGB(0x32,0x63,0x43),
** RGB(0xEF,0x59,0x7A),      RGB(0x50,0x88,0xC1),
** RGB(0x3D,0x99,0xAD),      RGB(0x96,0xD3,0x77),
** RGB(0xFF,0x6D,0x06),      RGB(0xF7,0xA7,0x00),
** RGB(0x90,0x62,0x0C),      RGB(0x3D,0x36,0x7D),
** RGB(0x5D,0x23,0x6E),      RGB(0xC3,0x4E,0x02),
** RGB(0xC6,0x06,0x4F),      RGB(0xF9,0x39,0x81),
** RGB(0x81,0x73,0xB1),      RGB(0x8D,0xC1,0xE8),
** RGB(0x66,0x2B,0x2D),      RGB(0xEA,0xB2,0x39),
** RGB(0x19,0x3D,0x55),      RGB(0x2B,0x87,0x7E),
** RGB(0x00,0x53,0x81),      RGB(0x0D,0x81,0x3D),
** RGB(0x81,0xBB,0x99),      RGB(0xD8,0x78,0x7E),
** RGB(0xFF,0xD5,0x7A),      RGB(0xD3,0x1D,0x8C),
** RGB(0xCB,0xCB,0x9C),      RGB(0x10,0xC8,0xCD),
** RGB(0xFA,0xF9,0x3C),      RGB(0x40,0x96,0xEE),
** RGB(0x99,0x66,0x00),      RGB(0x73,0x88,0x0A),
** RGB(0x33,0x00,0xCC),      RGB(0xC7,0x98,0x10),
** RGB(0x3F,0x4C,0x6B),      RGB(0x00,0x63,0xDC),
** RGB(0x99,0xDD,0xFC),      RGB(0x9C,0xCE,0x2E)
**
** RGB(0xFF,0xCC,0xCC), // red - light tints   255,204,204 hsv=0,240,216
** RGB(0xFF,0xC8,0xB0), // orange              255,200,176    12,240,176
** RGB(0xFF,0xFF,0xCC), // yellow              255,255,204    40,240,216
** RGB(0xCC,0xFF,0xCC), // green               204,255,204    80,"  ,"
** RGB(0xCC,0xFF,0xFF), // cyan                204,255,255   120,"  ,"
** RGB(0xCC,0xCC,0xFF), // blue                204,204,255   160
** RGB(0xFF,0xCC,0xFF)  // purple              255,204,255   200
**
** RGB(0xFF,0x67,0x66), // red - nice set
** RGB(0xFF,0xB6,0x67), // orange
** RGB(0xFF,0xFC,0x65), // yellow
** RGB(0x6E,0xFF,0x66), // green
** RGB(0x66,0xFF,0xFD), // cyan
** RGB(0x66,0x92,0xFF), // blue
** RGB(0xD4,0x65,0xFF)  // purple
**
** RGB(0xE2,0x1C,0x48), // red  - from boomwhackers.com
** RGB(0xF9,0x9D,0x1C), // orange
** RGB(0xFF,0xF3,0x2b), // yellow
** RGB(0xBC,0xD8,0x5F), // green   (lime)
** RGB(0x00,0x9C,0x95), // turquoise
** RGB(0x5E,0x50,0xA1), // purple  (dark)
** RGB(0xCF,0x3E,0x96)  // pink    (dark)
**
** RGB(0x9A,0x2F,0x34),  // red
** RGB(0x35,0x63,0x78),  // blu
** RGB(0x5C,0x81,0x57),  // grn
** RGB(0xDA,0xCA,0x8F),  // yel
** RGB(0xC7,0x4A,0x6C),  // pnk
** RGB(0x88,0x69,0x7B),  // prp
** RGB(0x8A,0x70,0x47),  // brn
** RGB(0xE4,0xA1,0x45),  // org
*/



#endif

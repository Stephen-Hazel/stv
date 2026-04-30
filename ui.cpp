// ui.cpp - ui stuph like window controls, etc - all da Qt

#include "ui.h"
#include <QClipboard>
#include <QMimeData>

QtEr Gui;

ubyte QtEr::NArg ()  {return _a->arguments ().count () - 1;}

char *QtEr::Arg (ubyte pos)
{  if (pos < NArg ())  return CC(UnQS (_a->arguments ().at (pos+1)));
   return CC("");
}
//______________________________________________________________________________
struct kmap {int in;   char str [6];   key k;};

static kmap KMap [] = {
   {Qt::Key_Return,    "ret", RET_KEY},  {Qt::Key_Tab,      "tab", TAB_KEY},
   {Qt::Key_Insert,    "ins", INS_KEY},  {Qt::Key_Delete,   "del", DEL_KEY},
   {Qt::Key_Backspace, "bsp", BSP_KEY},  {Qt::Key_Escape,   "esc", ESC_KEY},
   {Qt::Key_Up,        "up",  UP_KEY},   {Qt::Key_Down,     "dn",  DN_KEY},
   {Qt::Key_Left,      "lft", LFT_KEY},  {Qt::Key_Right,    "rit", RIT_KEY},
   {Qt::Key_PageUp,    "pup", PUP_KEY},  {Qt::Key_PageDown, "pdn", PDN_KEY},
   {Qt::Key_Home,      "hom", HOM_KEY},  {Qt::Key_End,      "end", END_KEY},
   {Qt::Key_Clear,     "clr", CLR_KEY},
   {Qt::Key_Print,     "prt", PRT_KEY},  {Qt::Key_Pause,    "brk", BRK_KEY},

   {Qt::Key_F1, "f01", F01_KEY}, {Qt::Key_F2, "f02", F02_KEY},
   {Qt::Key_F3, "f03", F03_KEY}, {Qt::Key_F4, "f04", F04_KEY},
   {Qt::Key_F5, "f05", F05_KEY}, {Qt::Key_F6, "f06", F06_KEY},
   {Qt::Key_F7, "f07", F07_KEY}, {Qt::Key_F8, "f08", F08_KEY},
   {Qt::Key_F9, "f09", F09_KEY}, {Qt::Key_F10,"f10", F10_KEY},
   {Qt::Key_F11,"f11", F11_KEY}, {Qt::Key_F12,"f12", F12_KEY}
};

key   KeyMap::Map (Qt::KeyboardModifiers mo, int ky)
{ char  c;
  ulong i;
  key   k = 0;
//DBG("mo=`d ky=`d", mo, ky);
   if ((ky >= ' ') && (ky <= '~')) {   // is it ascii?
      c = SC(char,ky);   if (! mo.testFlag (Qt::ShiftModifier))  c = CHDN (c);
      k = SC(key,c);                   // toss out shift bit (EXCEpt for spc :/)
      if ((c == ' ') && mo.testFlag (Qt::ShiftModifier))  k |= SHF;
   }
   else {
      for (i = 0;  i < BITS (KMap);  i++)  if (ky == KMap [i].in)
         {k = KMap [i].k;   break;}
      if (i == BITS (KMap))  return 0;      // not decodin everything so give up

      if (mo.testFlag (Qt::ShiftModifier))    k |= SHF;
   }
   if (mo.testFlag    (Qt::ControlModifier))  k |= CTL;
   if (mo.testFlag    (Qt::AltModifier))      k |= ALT;
   if (mo.testFlag    (Qt::MetaModifier))     k |= MET;
   return k;
}

char *KeyMap::Str (key k)
{ static char buf [2];
   if (k & 0x80)  return KMap [k & 0x7F].str;
   buf [0] = k & 0x7F;   buf [1] = '\0';   return buf;
}

/*
QKeySequence KeyMap::UnStr (char *k)
{ Qt::Key o;
  TStr i;
  int  j;
   o = (Qt::Key)0;
   StrCp (i, k);
   while ( (StrLn (i) > 4) && (i [3] == '|') ) {
      if (MemCm (i, CC("SHF|"), 4) == 0)  o = o | Qt::SHIFT;
      if (MemCm (i, CC("CTL|"), 4) == 0)  o = o | Qt::CTRL;
      if (MemCm (i, CC("ALT|"), 4) == 0)  o = Qt::ALT;
      if (MemCm (i, CC("MET|"), 4) == 0)  o = Qt::META;
      else {
DBG("KeyMap::UnStr error with shift str='`s'", k);
         return o;
      }
      StrCp (i, & i [4]);
   }
   if (StrLn (i) > 1)
      for (j = 0;  j < BITS (KMap);  j++)  if (StrCm (i, KMap [j].str) == 0)
         {o |= KMap [j].in;   break;}
   else   o |= (*i);
   return QKeySequence (o);
}
*/
//______________________________________________________________________________
ubyt4 QtEr::ClipLen ()
{  return _a->clipboard ()->text ().length ();  }

char *QtEr::ClipGet (char *s, ubyt4 siz)
{  *s = '\0';
   if ((ClipLen () + 1) <= siz) {
//DBG("clipget ok `d <= `d", ClipLen () + 1, siz);
      StrCp (s, CC(UnQS (_a->clipboard ()->text ())));
   }
//DBG("ClipGet returns s=`s", s);
   return s;
}

void QtEr::ClipPut (char *s)
{ QClipboard *c = _a->clipboard ();
   c->clear ();   c->setText (s);
}


//______________________________________________________________________________
void QtEr::Hey (char const *msg)
{ QMessageBox m;
   m.setWindowIcon ((Dark () && _d) ? _icoD : _ico);
DBG("hey `s", msg);   m.setText (msg);   m.exec ();
}

bool QtEr::YNo (char const *msg)
{ QMessageBox m;
DBG("YNo `s", msg);
   m.setWindowIcon ((Dark () && _d) ? _icoD : _ico);
   m.setText (msg);
   m.setStandardButtons (QMessageBox::Yes | QMessageBox::No);
   m.setDefaultButton   (QMessageBox::Yes);
   return (m.exec () == QMessageBox::Yes) ? true : false;
}

bool QtEr::AskR (char *name, char const *titl)
{ QString dir = QDir::homePath ();
   if (*name)  dir = Fn2Path (name);
  QString fn = QFileDialog::getOpenFileName (_w, titl, dir, "All Files (*)");
   if (fn.isEmpty ())  return false;
   StrCp (name, CC(UnQS (fn)));
   return true;
}

bool QtEr::AskW (char *name, char const *titl)
{ QString dir = QDir::homePath ();
   if (*name)  dir = Fn2Path (name);
  QString fn = QFileDialog::getSaveFileName (_w, titl, dir, "All Files (*)");
   if (fn.isEmpty ())  return false;
   StrCp (name, CC(UnQS (fn)));
   return true;
}

bool QtEr::AskDir (char *name, char const *titl)
{ QString dir = QDir::homePath ();
   if (*name)  dir = name;
DBG("AskDir given=`s", UnQS (dir));
   dir = QFileDialog::getExistingDirectory (
      _w, titl, dir, QFileDialog::ShowDirsOnly |
                     QFileDialog::DontResolveSymlinks |
                     QFileDialog::DontUseNativeDialog);
DBG("AskDir got=`s", UnQS (dir));
   if (dir.isEmpty ())  return false;
   StrCp (name, CC(UnQS (dir)));
   return true;
}


//______________________________________________________________________________
void QtEr::SetTtl (char *t)  {_w->setWindowTitle (t);}

/* how i found raspi needed it's global font
void DbgF (char *s)
{ { QFont f = Gui.A ()->font ();
DBG("`s app font=`s `d", s, UnQS (f.family ()), f.pointSize ());
  }
  { QFont f = Gui.W ()->font ();
DBG("`s win font=`s `d", s, UnQS (f.family ()), f.pointSize ());
  }
}
*/

void QtEr::ReIco ()
{  _w->setWindowIcon ((Dark () && _d) ? _icoD : _ico);  }

void QtEr::WinLoad (QSplitter *spl)
{ QSettings s ("win", _ttl);
   if (_fixw) {
      if (s.contains ("font"))
           {QFont f (s.value ("font").toString (), s.value ("fontpt").toInt ());
                                         _a->setFont (f);   _w->setFont (f);}
      else {QFont f ("monospace", 14);   _a->setFont (f);   _w->setFont (f);}
//QFont f = Gui.A ()->font ();         // raspi needs _w too sigh
//DBG("WinLoad font=`s `d", UnQS (f.family ()), f.pointSize ());
   }
   if (s.contains ("size"))  _w->resize (s.value ("size").toSize ());
// else                      _w->resize (400, 400);
   _w->move (0, 0);   _w->show ();

   if (s.contains ("scr")) {
     QString scr = s.value ("scr").toString ();
     QList<QScreen *> scrLs = QGuiApplication::screens ();
      for (int i = 0;  i < scrLs.size ();  i++)
         if (scrLs [i]->name () == scr)
            {_w->windowHandle ()->setScreen (scrLs [i]);   break;}
   }
   if (s.contains ("pos"))  _w->move (s.value ("pos").toPoint ());
   if (s.contains ("spl") && spl) {
     QString    t = s.value ("spl").toString ();
     QList<int> p = spl->sizes ();
     ColSep     cs (UnQS (t), p.count ());
      for (ubyte i = 0;  i < p.count ();  i++)  p [i] = Str2Int (cs.Col [i]);
      spl->setSizes (p);
   }
   SetTtl (_ttl);
   ReIco ();
}

void QtEr::WinSave (QSplitter *spl)
{ QSettings s ("win", _ttl);
   if (_fixw) {
      s.setValue ("font",   _w->font ().family ());
      s.setValue ("fontpt", _w->font ().pointSize ());
   }
   if (spl) {
     QList<int> p = spl->sizes ();
     ubyte i;
     TStr  t;
      for (i = 0, *t = '\0';  i < p.count ();  i++)
         StrFmt (& t [StrLn (t)], "`s`d", i ? " ":"", p [i]);
      s.setValue ("spl", QString (t));
   }
   s.setValue ("size", _w->size ());
   s.setValue ("pos",  _w->pos ());
   s.setValue ("scr",  _w->windowHandle ()->screen ()->name ());
}

void QtEr::Init (QApplication *a, QMainWindow *w, const char *ttl, char d2,
                 char fixw)
{  _a = a;   _w = w;   StrCp (_ttl, ttl);   _fixw = fixw;  _d = d2;
   _q = false;
   _ico  =      QIcon (":/app");
   _icoD = _d ? QIcon (":/app_d") : QIcon ();
}

ubyt2 QtEr::FontH ()
{ QFontMetrics fm (_a->font ());
   return fm.capHeight () + 1 + fm.descent ();
}


bool QtEr::Dark ()
{  return _w->palette ().color (QPalette::Window).lightness () < 128;  }


char *ColRGB (char *s, QColor c, char x)
{  if (x != 'd')
      return StrFmt (s, "#`02x`02x`02x", c.red (), c.green (), c.blue ());
   return    StrFmt (s, "`d,`d,`d",      c.red (), c.green (), c.blue ());
}


QColor Color (const char *c)
{ QPalette p = QApplication::palette ();
   if      (! StrCm (c, "fg"))      return p.color (QPalette::Text);
//"else if (! StrCm (c, "fgWin"))   return p.color (QPalette::WindowText);
//"else if (! StrCm (c, "fgTip"))   return p.color (QPalette::ToolTipText);
//"else if (! StrCm (c, "fgBtn"))   return p.color (QPalette::ButtonText);
   else if (! StrCm (c, "bg"))      return p.color (QPalette::Window);
   else if (! StrCm (c, "fgSel"))   return p.color (QPalette::HighlightedText);
   else if (! StrCm (c, "bgSel"))   return p.color (QPalette::Highlight);
//"else if (! StrCm (c, "accent"))  return p.color (QPalette::Accent);
//"else if (! StrCm (c, "fgBrite")) return p.color (QPalette::BrightText);
//"else if (! StrCm (c, "base"))    return p.color (QPalette::Base);
//"else if (! StrCm (c, "lite"))    return p.color (QPalette::Light);
   else if (! StrCm (c, "fgPlace")) return p.color (QPalette::PlaceholderText);
   else if (! StrCm (c, "bgTip"))   return p.color (QPalette::ToolTipBase);
// else if (! StrCm (c, "base2"))   return p.color (QPalette::AlternateBase);
   else if (! StrCm (c, "bgBtn"))   return p.color (QPalette::Button);
   else if (! StrCm (c, "link"))    return p.color (QPalette::Link);
   else if (! StrCm (c, "linkVis")) return p.color (QPalette::LinkVisited);
   else if (! StrCm (c, "medLt"))   return p.color (QPalette::Midlight);
   else if (! StrCm (c, "medDk"))   return p.color (QPalette::Mid);
   else if (! StrCm (c, "dark"))    return p.color (QPalette::Dark);
   else if (! StrCm (c, "shadow"))  return p.color (QPalette::Shadow);

DBG("Color='`s' is a bug", c);
   return p.color (QPalette::Text);
}
/*
light mode:
* fg       35  38  41  1 *fg = fgWin = fgTip = fgBtn
x fgWin    35  38  41  1
x fgTip    35  38  41  1
x fgBtn    35  38  41  1
> bg      239 240 241
* fgSel   255 255 255  2 *fgSel = fgBrite = base = light
x fgBrite 255 255 255  2
x base    255 255 255  2
x light   255 255 255  2
* bgSel    61 174 233  3 *bgSel = accent
x accent   61 174 233  3
> fgPlace 112 125 138
* bgTip   247 247 247  4
x base2   247 247 247  4 base2 = *bgTip
> bgBtn   252 252 252
> link     41 128 185
> linkVis 155  89 182
> midLt   249 250 250
> midDk   189 193 198
> dark    113 117 122
> shadow   59  61  63

dark mode:
fgWin   252 252 252
bg       32  35  38
fgSel   252 252 252
bgSel    61 174 233
accent   61 174 233
fg      252 252 252
fgBrite 255 255 255
fgPlace 161 169 177
base     20  22  24
base2    29  31  34
fgTip   252 252 252
bgTip    41  44  48
fgBtn   252 252 252
bgBtn    41  44  48
link     29 153 243
linkVis 155  89 182
light    64  70  76
midLt    51  56  60
midDk    28  30  33
dark     16  17  18
Shadow=  11  12  13
*/

void QtEr::DlgLoad (QDialog *d, QString nm, QSplitter *spl)
{ QSettings s ("win", nm);
   if (s.contains ("size"))  d->resize (s.value ("size").toSize ());
   d->move (0, 0);
   if (s.contains ("pos"))  d->move (s.value ("pos").toPoint ());
   if (s.contains ("spl") && spl) {
     QString    t = s.value ("spl").toString ();
     QList<int> p = spl->sizes ();
     ColSep     cs (UnQS (t), p.count ());
      for (ubyte i = 0;  i < p.count ();  i++)  p [i] = Str2Int (cs.Col [i]);
      spl->setSizes (p);
   }
}

void QtEr::DlgSave (QDialog *d, QString nm, QSplitter *spl)
{ QSettings s ("win", nm);
   s.setValue ("size", d->size ());
   s.setValue ("pos",  d->pos ());
   if (spl) {
     QList<int> p = spl->sizes ();
     ubyte i;
     TStr  t;
      for (i = 0, *t = '\0';  i < p.count ();  i++)
         StrFmt (& t [StrLn (t)], "`s`d", i ? " ":"", p [i]);
      s.setValue ("spl", QString (t));
   }
}

void QtEr::DlgMv (QDialog *d, QPointF p, char const *anch)
// move relative to mouse  (caps in anch give a little extra room)
{ int w, h; //, mw, mh;
//   mw = 1;  // HORZRES
//   mh = 1;  // VERTRES
   w = d->width ();   h = d->height ();
   switch (anch [0]) {              // tweak y
      case 't': break;
      case 'T': p.setY ( p.y () + 15);   break;
      case 'b': p.setY ((p.y () > h     ) ? (p.y () - h)      : 0);  break;
      case 'B': p.setY ((p.y () > (h+15)) ? (p.y () - (h+15)) : 0);  break;
      default:  p.setY ((p.y () > h/2   ) ? (p.y () - h/2)    : 0);  break;
   }
   switch (anch [1]) {              // tweak x
      case 'l': break;
      case 'L': p.setX ( p.x () + 15);   break;
      case 'r': p.setX ((p.x () > w     ) ? (p.x () - w)      : 0);  break;
      case 'R': p.setX ((p.x () > (w+15)) ? (p.x () - (w+15)) : 0);  break;
      default:  p.setX ((p.x () > w/2   ) ? (p.x () - w/2)    : 0);  break;
   }
//   if (p.x () + w > mw) p.setX (mw - w);    // limit to on screen
//   if (p.y () + h > mh) p.setY (mh - h);
   d->move (SC(int,p.x ()), SC(int,p.y ()));
}


//______________________________________________________________________________
void CtlTBar::Init (QMainWindow *w, const char *nm)
{ QToolBar *tb = w->addToolBar (nm);
   _w = tb;   StrCp (_nm, (char *)nm);
   _w->setIconSize (QSize (32, 32));
   _w->setMovable (false);
   _nb = 0;
}


void CtlTBar::Init (QDialog *d, const char *nm)
// for dialogs, pass in main layout instead of mainwindow
{ QToolBar *tb = new QToolBar (nm);
   _w = tb;   StrCp (_nm, (char *)nm);
   d->layout ()->setMenuBar (_w);
   _w->setIconSize (QSize (32, 32));
   _w->setMovable (false);
   _nb = 0;
}


void CtlTBar::Sep (ubyte b)
{  if (b >= BITS (_b))  {DBG("tooo many buttons in Sep !");   return;}

   if (b >= _nb)  _nb = b+1;
  CtlBtn *bt = & _b [b];
   bt->ni = bt->ic = 0;

   _w->addSeparator ();
}

//bool icok (QIcon i)
//{  if (i.isNull ())                     {DBG("null");  return false;}
//   if (i.availableSizes ().isEmpty ())  {DBG("empty"); return false;}
//   return true;
//}

void CtlTBar::Btn (ubyte b, char *tip, const char *ico, const char *key)
{ TStr ts, is;
  bool big;
   if (b >= BITS (_b))  {DBG("tooo many buttons in Init !");   return;}

   if (b >= _nb)  _nb = b+1;
  CtlBtn *bt = & _b [b];
   bt->ni = bt->ic = 0;   bt->i  [0] = QIcon ();
                          bt->di [0] = QIcon ();   bt->d [0] = false;
   if (*key != '\0')  StrFmt (ts, "`s  [`s]", tip, key);
   else               StrCp  (ts, tip);
   if (ico [0] == '*') {               // text instead of icon
      big = (ico [1] == '^');
      bt->ac = new QAction (& ico [big ? 2 : 1], _w);
      if (big) {
        QFont f = bt->ac->font ();
         f.setPointSize (20);          // beeg !
         bt->ac->setFont (f);
      }
   }
   else {
      bt->i [0] = QIcon (StrFmt (is, ":/tbar/`s/`d", _nm, b));
      bt->ac = new QAction (bt->i [0], "", _w);
      bt->ni = 1;
      if (*ico == 'd')  {bt->d  [0] = true;
                         bt->di [0] = QIcon (StrAp (is, "_d"));
//DBG("doin _d is=`s dk=`b d=`b icok=`b",
//is, Gui.Dark (), bt->d [0],
//(Gui.Dark () && bt->d [0]) ? icok (bt->di [0]) : icok (bt->i [0]));
                         }
      bt->ac->setIcon ((Gui.Dark () && bt->d [0]) ? bt->di [0] : bt->i [0]);
   }
   bt->ac->setToolTip (ts);
   if (*key != '\0')  bt->ac->setShortcut (QKeySequence (key));
   _w->addAction (bt->ac);
}


void CtlTBar::Btn (ubyte b, const char *tip, const char *ico, const char *key)
{  Btn (b, CC(tip), ico, key);  }


void CtlTBar::Ico (ubyte b, ubyte i, const char *ico)
{ TStr is;
   if (b >= _nb)           {DBG("button past #buttons in Ico !");   return;}
  CtlBtn *bt = & _b [b];
   if (i >= BITS (_b->i))  {DBG("tooo many icons in Ico !");        return;}

   if (i >= bt->ni)  bt->ni = i+1;
   bt->i [i] = QIcon (StrFmt (is, ":/tbar/`s/`d_`d", _nm, b, i));
   if (*ico == 'd')  {bt->d  [i] = true;
                      bt->di [i] = QIcon (StrAp (is, CC("_d")));}
   else              {bt->d  [i] = false;
                      bt->di [i] = QIcon ();}
}


void CtlTBar::Set (ubyte b, ubyte i)
{  if (b >= _nb)     {DBG("Set button `d past #buttons !", b);   return;}
  CtlBtn *bt = & _b [b];
   if (bt->ni == 0)  return;           // skip seperator

   if (i >= bt->ni)  {DBG("Set button `d icon `d past #icons !", b, i);
                      return;}
   bt->ic = i;
//DBG("tbar Set nm=`s b=`d i=`d dk=`b bt->d=`b x=`08x",
//_nm, b, i, Gui.Dark (), bt->d [i],
                    (Gui.Dark () && bt->d [i]) ? bt->di [i] : bt->i [i]);
   bt->ac->setIcon ((Gui.Dark () && bt->d [i]) ? bt->di [i] : bt->i [i]);
}


void CtlTBar::ReDo ()
{ TStr s, t;
   for (ubyte b = 0;  b < _nb;  b++)  Set (b, _b [b].ic);
//DBG("tbar bg=`s", ColRGB (t, Color ("bg"), 'd'));
   _w->setStyleSheet (StrFmt (s, "QToolBar{background-color:`s;}",
                                 ColRGB (t, Color ("bg"))));
}


//______________________________________________________________________________
// this ain't workin :(
class WrapOK: public QStyledItemDelegate {
public:
   using QStyledItemDelegate::QStyledItemDelegate;
protected:
   void initStyleOption (QStyleOptionViewItem *option,
                         const QModelIndex & index) const override
   {  QStyledItemDelegate::initStyleOption (option, index);
      option->textElideMode = Qt::ElideNone;
//      TextWrapAnywhere is what i waaant
   }
};

void CtlTabl::Init (QTableWidget *t, const char *hdr, ppop pop,
                    const char *mode, const char *what, char wrap)
// hdr is zz string of labels
//  >| prefix means right or center just
//  +  prefix means icon
//  *  prefix means icon w dark
//  _  prefix means edit (string or droplist edit)
{ ubyte c;
  char *h;
  char  ed = '\0';      // _ means editing, ^ means QComboBox so delegate too
  QStringList sl;
   _t = t;
   for (c = 0, h = CC(hdr);  *h;  c++, h = & h [StrLn (h)+1]) {
      _ju [c] = _ed [c] = '\0';
      if ((*h == '+') || (*h == '*') ||
          (*h == '>') || (*h == '|'))  _ju [c] = *h++;
      if  (*h == '_')  {_ed [c] = *h++;   ed = '_';}
      sl << QString::fromStdString (h);
   }
   _t->horizontalHeader ()->setSectionResizeMode (  //QHeaderView::Interactive);
      QHeaderView::ResizeMode::ResizeToContents);
   _t->setColumnCount (c);   _t->setHorizontalHeaderLabels (sl);
   _t->verticalHeader ()->hide ();
   _t->setAlternatingRowColors (false);
   if (ed)  _t->setItemDelegate (new SIDlg (_t, _ed, pop));
   _t->setEditTriggers (ed ? QAbstractItemView::AllEditTriggers
                           : QAbstractItemView::NoEditTriggers);
   SetSelect (mode, what);
   _t->setWordWrap (_wr = wrap ? true : false);
   if (_wr)  _t->resizeRowsToContents ();
   _t->setSizePolicy (QSizePolicy::Expanding, QSizePolicy::Expanding);
   _t->setVerticalScrollBarPolicy   (Qt::ScrollBarAsNeeded);
   _t->setHorizontalScrollBarPolicy (Qt::ScrollBarAlwaysOff);
}

/* _t->horizontalHeader ()->setSectionResizeMode (2, QHeaderView::Fixed)
** _t->horizontalHeader ()->setStretchLastSection (true);
** _t->horizontalHeader ()->setHighlightSections (false);
** _t->horizontalHeader ()->savestate ();   n restorestate ()
** _t->setSelectionBehavior (QAbstractItemView::SelectItems/Rows);
** _t->setSelectionMode     (QAbstractItemView::ExtendedSelection);
** _t->setGridShow (false);
*/

void  CtlTabl::SetColWrapOK (ubyte c)
{  _t->setItemDelegateForColumn (c, new WrapOK);  }

void  CtlTabl::SetRowH (ubyt2 h)
{  _t->verticalHeader ()->setDefaultSectionSize (h);
//TODO this ok?
   _t->setIconSize (QSize (h, h));
   _ih = h;
}

ubyt2 CtlTabl::ColW    (ubyte c)  {return _t->columnWidth (c);}
void  CtlTabl::SetColW (ubyte c, ubyt2 w)
{  _t->horizontalHeader ()->setSectionResizeMode (c, QHeaderView::Fixed);
   _t->setColumnWidth (c, w);
}

ubyt2 CtlTabl::NRow ()    {return _t->rowCount ();}
ubyte CtlTabl::NCol ()    {return _t->columnCount ();}
sbyt2 CtlTabl::CurRow ()  {return _t->currentRow ();}
ubyte CtlTabl::CurCol ()  {return _t->currentColumn ();}
char *CtlTabl::Get (ubyt2 r, ubyte c)  {return UnQS (_t->item (r, c)->text ());}

void  CtlTabl::Set (ubyt2 r, ubyte c, char *s, char *tip)
{ QTableWidgetItem *it = _t->item (r, c);
  TStr ico;
//DBG("CtlTabl::Set r=`d c=`d s=`s it=`x", r, c, s, it);
   _t->blockSignals (true);
   if ((_ju [c] != '+') && (_ju [c] != '*'))  it->setText (s);
   else {
      if      (*s == '*')
         it->setText (++s);            // override icon :/
      else if (*s) {
         if ((_ju [c] == '*') && Gui.Dark ())
            it->setIcon (QIcon (StrFmt (ico, ":/tabl/`s_d", s)));
         else
            it->setIcon (QIcon (StrFmt (ico, ":/tabl/`s",   s)));
         it->setSizeHint (QSize (_ih, _ih));
      }
// it->setData (Qt::DecorationRole, QPixmap (StrFmt (ico, ":/tabl/`s", s))
//   .scaled (_ih, _ih, Qt::KeepAspectRatio, Qt::SmoothTransformation));
      else  it->setIcon (QIcon ());
   }
   if (tip != nullptr)  it->setToolTip (tip);
   _t->blockSignals (false);
}

void CtlTabl::HopTo (ubyt2 r, ubyte c)  {_t->scrollToItem   (_t->item (r, c));
                                         _t->setCurrentItem (_t->item (r, c));
                                         _t->selectRow (r);}

void CtlTabl::SetColor (ubyt2 r, ubyte c, QColor co)
{  _t->item (r, c)->setBackground (QBrush (co));
// _t->item (r, c)->setForeground (QBrush (CBLACK));
}

void CtlTabl::SetRowCol (ubyt2 r, QColor c)
{ ubyte i = NCol ();   while (i) SetColor (r, --i, c);  }

void CtlTabl::SetSelect (const char *mode, const char *what)
{ QAbstractItemView::SelectionMode     m;
  QAbstractItemView::SelectionBehavior b;
   if      (! StrCm (CC(mode), CC("none")))
      m = QAbstractItemView::NoSelection;
   else if (! StrCm (CC(mode), CC("single")))
      m = QAbstractItemView::SingleSelection;
   else if (! StrCm (CC(mode), CC("multi")))
      m = QAbstractItemView::MultiSelection;
   else if (! StrCm (CC(mode), CC("extended")))
      m = QAbstractItemView::ExtendedSelection;
   else if (! StrCm (CC(mode), CC("contig")))
      m = QAbstractItemView::ContiguousSelection;
   else
{DBG("CtlTable::SetSelect bad arg1");   return;}
   _t->setSelectionMode (m);
   if      (! StrCm (CC(what), CC("item")))
      b = QAbstractItemView::SelectItems;
   else if (! StrCm (CC(what), CC("row")))
      b = QAbstractItemView::SelectRows;
   else if (! StrCm (CC(what), CC("col")))
      b = QAbstractItemView::SelectColumns;
   else
{DBG("CtlTable::SetSelect bad arg2");   return;}
   _t->setSelectionBehavior (b);
}


void CtlTabl::Open ()
{
//DBG("CtlTabl::Open bgn");
   _tr = CurRow ();   _tc = CurCol ();
   _t->hide ();   _t->blockSignals (true);
   _t->clearContents ();   _t->setRowCount (0);
   _nr = 0;
//DBG("CtlTabl::Open end");
}

void CtlTabl::Put (char **rp, char *tip)
{ ubyte c;
  TStr  ico;
  QTableWidgetItem *it;
   _t->setRowCount (_nr+1);
   for (c = 0;  *rp;  c++, rp++) {
      if (c >= NCol ())  DBG("HEY!  CtlTabl::Put went beyond NCol !!");
      if (! (it = _t->item (_nr, c)))
               _t->setItem (_nr, c, it = new QTableWidgetItem);
      if (_ed [c])  it->setFlags (it->flags () |  Qt::ItemIsEditable);
      else          it->setFlags (it->flags () & ~Qt::ItemIsEditable);
      if ((_ju [c] == '+') || (_ju [c] == '*')) {
         if (**rp) {
            if      (**rp == '*')
               it->setText (++(*rp));
            else if ((_ju [c] == '*') && Gui.Dark ())
               it->setIcon (QIcon (StrFmt (ico, ":/tabl/`s_d", *rp)));
            else
               it->setIcon (QIcon (StrFmt (ico, ":/tabl/`s", *rp)));
         }
         else  it->setIcon (QIcon ());
      }
      else {
         if      (_ju [c] == '>')  it->setTextAlignment (Qt::AlignRight);
         else if (_ju [c] == '|')  it->setTextAlignment (Qt::AlignCenter);
//       else                      it->setTextAlignment (Qt::AlignLeft|
//                                           ((Qt::Alignment)Qt::TextWordWrap));
         it->setText (*rp);
//DBG("CtlTabl::Put r=`d c=`d d=`s", _nr, c, *rp);
      }
      if (tip != nullptr)  it->setToolTip (tip);
   }
   _nr++;
}

void CtlTabl::Shut (bool rehop)
{
//DBG("CtlTabl::Shut show");
   _t->show ();
   _t->resizeColumnsToContents ();
   if (_wr)  _t->resizeRowsToContents ();
//DBG("CtlTabl::Shut HopTo");
   if (rehop)  HopTo (_tr, _tc);       // in 6 on CLOSE, ed sets value again:(
//DBG("CtlTabl::Shut blockSignals(false)");
   _t->blockSignals (false);
//DBG("CtlTabl::Shut end");
}

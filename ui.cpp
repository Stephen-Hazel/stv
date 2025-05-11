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
typedef struct {int in;   char str [6];   key k;} kmap;

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
DBG("clipget ok `d <= `d", ClipLen () + 1, siz);
      StrCp (s, CC(UnQS (_a->clipboard ()->text ())));
   }
DBG("ClipGet returns s=`s", s);
   return s;
}

void QtEr::ClipPut (char *s)
{ QClipboard *c = _a->clipboard ();
   c->clear ();   c->setText (s);
}


//______________________________________________________________________________
void QtEr::Hey (char const *msg)
{ QMessageBox m;
   m.setWindowIcon (QIcon (":/app.ico"));
DBG("hey `s", msg);   m.setText (msg);   m.exec ();
}

bool QtEr::YNo (char const *msg)
{ QMessageBox m;
DBG("YNo `s", msg);
   m.setWindowIcon (QIcon (":/app.ico"));
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
   _w->setWindowIcon (QIcon (":/app.ico"));
   SetTtl (_ttl);
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


void QtEr::Init (QApplication *a, QMainWindow *w, const char *ttl, char fixw)
{  _a = a;   _w = w;   StrCp (_ttl, CC(ttl));   _fixw = fixw;  _q = false;  }


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
CtlTBar::CtlTBar (QMainWindow *w, const char *tip, const char *nm)
{ ubyte p;
  char *t;
  TStr  s, ts, tp, is, ks;
  QToolBar *tb;
   if (! StrCm (CC(nm), CC("")))
        {tb = w->addToolBar ("toolbar");   tb->setMovable (false);}
   else {tb = w->addToolBar (nm);          tb->setMovable (true);}
   _w = tb;
   for (p = 0, t = CC(tip);  *t;  p++, t = & t [StrLn (t)+1]) {
      if (*t == '|')  tb->addSeparator ();
      else {
         StrCp (s, t);
        ColSep cs (s, 2, '`');         // tip, icon/txtStr, keyStr
         StrCp (tp, cs.Col [0]);   StrCp (is, cs.Col [1]);
                                   StrCp (ks, cs.Col [2]);
         if (*ks)  StrAp (tp, StrFmt (ts, " (`s)", ks));
         if (*is == '*') {
            _ac [p] = new QAction (& is [1], w);
            _ac [p]->setToolTip (tp);
         }
         else {
           QIcon ico = (*is == ':') ? QIcon (is)
                                    : QIcon::fromTheme (is, QIcon (is));
            _ac [p] = new QAction (ico, tp, w);
         }
         if (*ks)  _ac [p]->setShortcut (QKeySequence (ks));
         tb->addAction (_ac [p]);
      }
   }
   _na = p;
}

CtlTBar::CtlTBar (QDialog *d, const char *tip)
// for dialogs, pass in main layout instead of mainwindow
{ ubyte p;
  char *t;
  TStr  s, ts, tp, is, ks;
  QToolBar *tb = new QToolBar ();      // yep, setMenuBar -IZ- how ya do it :/
   _w = tb;
   d->layout ()->setMenuBar (tb);   tb->setMovable (false);
   for (p = 0, t = CC(tip);  *t;  p++, t = & t [StrLn (t)+1]) {
      if (*t == '|')  tb->addSeparator ();
      else {
         StrCp (s, t);
        ColSep cs (s, 2, '`');         // tip, icon/txtStr, keyStr
         StrCp (tp, cs.Col [0]);   StrCp (is, cs.Col [1]);
                                   StrCp (ks, cs.Col [2]);
         if (*ks)  StrAp (tp, StrFmt (ts, " (`s)", ks));
         if (*is == '*') {
            _ac [p] = new QAction (& is [1], tb);
            _ac [p]->setToolTip (tp);
         }
         else {
           QIcon ico = (*is == ':') ? QIcon (is)
                                    : QIcon::fromTheme (is, QIcon (is));
            _ac [p] = new QAction (ico, tp, tb);
         }
         if (*ks)  _ac [p]->setShortcut (QKeySequence (ks));
         tb->addAction (_ac [p]);
      }
   }
   _na = p;
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

void CtlTabl::Init (QTableWidget *t, const char *hdr, ppop pop, char wrap)
// hdr is zz string of labels
//  >| prefix means right or center just
//  *  prefix means icon
//  _  prefix means edit (string or droplist edit)
{ ubyte c;
  char *h;
  char  ed = '\0';      // _ means editing, ^ means QComboBox so delegate too
  QStringList sl;
   _t = t;
   for (c = 0, h = CC(hdr);  *h;  c++, h = & h [StrLn (h)+1]) {
      _ju [c] = _ed [c] = '\0';
      if ((*h == '*') || (*h == '>') || (*h== '|'))  _ju [c] = *h++;
      if  (*h == '_') {_ed [c] = *h++;   ed = '_';}
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
   _t->setSelectionBehavior (QAbstractItemView::SelectRows);
   _t->setWordWrap (_wr = wrap ? true : false);
   if (_wr)  _t->resizeRowsToContents ();
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
ubyt2 CtlTabl::CurRow ()  {return _t->currentRow ();}
ubyte CtlTabl::CurCol ()  {return _t->currentColumn ();}
char *CtlTabl::Get (ubyt2 r, ubyte c)  {return UnQS (_t->item (r, c)->text ());}

void  CtlTabl::Set (ubyt2 r, ubyte c, char *s, char *tip)
{ QTableWidgetItem *it = _t->item (r, c);
  TStr ico;
//DBG("CtlTabl::Set r=`d c=`d s=`s it=`x", r, c, s, it);
   _t->blockSignals (true);
   if (_ju [c] != '*')      it->setText (s);
   else {
      if      (*s == '*')   it->setText (++s);
      else if (*s)         {it->setIcon (QIcon (StrFmt (ico, ":/tico/`s", s)));
                            it->setSizeHint (QSize (_ih, _ih));}
// it->setData (Qt::DecorationRole, QPixmap (StrFmt (ico, ":/tico/`s", s))
//   .scaled (_ih, _ih, Qt::KeepAspectRatio, Qt::SmoothTransformation));
      else                  it->setIcon (QIcon ());
   }
   if (tip != nullptr)  it->setToolTip (tip);
   _t->blockSignals (false);
}

void CtlTabl::HopTo (ubyt2 r, ubyte c)  {_t->scrollToItem   (_t->item (r, c));
                                         _t->setCurrentItem (_t->item (r, c));}
void CtlTabl::SetColor (ubyt2 r, QColor c)
{  for (ubyte i = NCol ();  i;  i--) {
//    _t->item (r, i-1)->setForeground (QBrush (CBLACK));
      _t->item (r, i-1)->setBackground (QBrush (c));
   }
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
      if (_ju [c] == '*') {
         if (**rp) {
            if      (**rp == '*')  it->setText (++(*rp));
            else if (**rp)         it->setIcon (QIcon (StrFmt (ico,
                                                           ":/tico/`s", *rp)));
            else                   it->setIcon (QIcon ());
         }
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

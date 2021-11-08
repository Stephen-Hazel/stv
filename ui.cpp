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
ubyt4 QtEr::ClipLen ()
{  return _a->clipboard ()->text ().length ();  }

char *QtEr::ClipGet (char *s, ubyt4 siz)
{  *s = '\0';
   if ((ClipLen () + 1) <= siz)
      StrCp (s, CC(UnQS (_a->clipboard ()->text ())));
   return s;
}

void QtEr::ClipPut (char *s)
{ QClipboard *c = _a->clipboard ();
   c->clear ();   c->setText (s);
}


//______________________________________________________________________________
void QtEr::Hey (char const *msg)
{ QMessageBox m;
DBG("hey `s", msg);   m.setText (msg);   m.exec ();
}

bool QtEr::YNo (char const *msg, char const *inf)
{ QMessageBox m;
DBG("YNo `s", msg);
   m.setText (msg);
   if (*inf)  m.setInformativeText (inf);
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
   dir = QFileDialog::getExistingDirectory (
      _w, titl, dir, QFileDialog::ShowDirsOnly |
                     QFileDialog::DontResolveSymlinks);
   if (dir.isEmpty ())  return false;
   StrCp (name, CC(UnQS (dir)));
   return true;
}


//______________________________________________________________________________
void QtEr::SetTtl (char *t)  {_w->setWindowTitle (t);}

void QtEr::WinLoad (QSplitter *spl)
{ QSettings s (App.grp, App.app);
   if (_fixw) {
      if (s.contains ("font"))
           {QFont f (s.value ("font").toString (), s.value ("fontpt").toInt ());
                                         _a->setFont (f);}
      else {QFont f ("monospace", 14);   _a->setFont (f);}
//QFont f = Gui.A ()->font ();
//DBG("font=`s `d", UnQS (f.family ()), f.pointSize ());      
   }
   if (s.contains ("size"))  _w->resize (s.value ("size").toSize ());
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
   SetTtl (App.ttl);
}

void QtEr::WinSave (QSplitter *spl)
{ QSettings s (App.grp, App.app);
   if (_fixw) {
      s.setValue ("font",   _a->font ().family ());
      s.setValue ("fontpt", _a->font ().pointSize ());
   }
   if (spl) {
     QList<int> p = spl->sizes ();
     ubyte i;
     TStr  t;
      for (i = 0, *t = '\0';  i < p.count ();  i++)
         StrFmt (& t [StrLn (t)], "`s`d", i ? " ":"", p [i]);
      s.setValue ("spl", t);
   }
   s.setValue ("size", _w->size ());
   s.setValue ("pos",  _w->pos ());
   s.setValue ("scr",  _w->windowHandle ()->screen ()->name ());
}


void QtEr::Init (QApplication *a, QMainWindow *w, char fixw)
{  _a = a;   _w = w;   _fixw = fixw;  }

int  QtEr::Loop ()  { int r = _a->exec ();   return r;  }


void QtEr::DlgLoad (QDialog *d, QString nm, QSplitter *spl)
{ QSettings s (App.grp, nm);
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
{ QSettings s (App.grp, nm);
   s.setValue ("size", d->size ());
   s.setValue ("pos",  d->pos ());
   if (spl) {
     QList<int> p = spl->sizes ();
     ubyte i;
     TStr  t;
      for (i = 0, *t = '\0';  i < p.count ();  i++)
         StrFmt (& t [StrLn (t)], "`s`d", i ? " ":"", p [i]);
      s.setValue ("spl", t);
   }
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
   for (p = 0, t = CC(tip);  *t;  p++, t = & t [StrLn (t)+1]) {
      if (*t == '|')  tb->addSeparator ();
      else {
         StrCp (s, t);
        ColSep cs (s, 2, '`');
         StrCp (tp, cs.Col [0]);   StrCp (is, cs.Col [1]);
                                   StrCp (ks, cs.Col [2]);
         if (*ks)  StrAp (tp, StrFmt (ts, " (`s)", ks));
        QIcon ico = (*is == ':') ? QIcon (is)
                                 : QIcon::fromTheme (is, QIcon (is));
         _ac [p] = new QAction (ico, tp, w);
         if (*ks)  _ac [p]->setShortcut (QKeySequence (ks));
         tb->addAction (_ac [p]);
      }
   }
}

CtlTBar::CtlTBar (QDialog *d, const char *tip)
// for dialogs, pass in main layout instead of mainwindow
{ ubyte p;
  char *t;
  TStr  s, ts, tp, is, ks;
  QToolBar *tb = new QToolBar ();      // yep, setMenuBar -IZ- how ya do it :/
   d->layout ()->setMenuBar (tb);   tb->setMovable (false);
   for (p = 0, t = CC(tip);  *t;  p++, t = & t [StrLn (t)+1]) {
      if (*t == '|')  tb->addSeparator ();
      else {
         StrCp (s, t);
        ColSep cs (s, 2, '`');
         StrCp (tp, cs.Col [0]);   StrCp (is, cs.Col [1]);
                                   StrCp (ks, cs.Col [2]);
         if (*ks)  StrAp (tp, StrFmt (ts, " (`s)", ks));
        QIcon ico = (*is == ':') ? QIcon (is)
                                 : QIcon::fromTheme (is, QIcon (is));
         _ac [p] = new QAction (ico, tp, tb);
         if (*ks)  _ac [p]->setShortcut (QKeySequence (ks));
         tb->addAction (_ac [p]);
      }
   }
}


//______________________________________________________________________________
SIDlg::SIDlg (QObject *par, char *ed, ppop pop)
: QStyledItemDelegate (par)
{  _ed = ed;   _pop = pop;  }

void SIDlg::paint (QPainter *p, const QStyleOptionViewItem &opt,
                                const QModelIndex &ind)  const
{  if (! opt.icon.isNull ()) 
        {p->save ();   opt.icon.paint (p, opt.rect);
         p->restore ();}
   else  QStyledItemDelegate::paint (p, opt, ind);
}

void SIDlg::cbChanged (int i)
{ QComboBox *cb = qobject_cast<QComboBox *>(sender ());
   (void)i;   emit commitData (cb);   emit closeEditor (cb);
}

QWidget *SIDlg::createEditor (QWidget *par, const QStyleOptionViewItem &opt,
                              const QModelIndex &ind)  const
{ BStr bs;
  char *s;
   if (_ed [ind.column ()] == '^') {
      _pop (bs, ind.row (), ind.column ());
      if (*bs == 0)  return nullptr;
      
     QComboBox *cb = new QComboBox (par);
      for (s = bs;  *s;  s = & s [StrLn (s)+1])   
         cb->addItem (StrCm (s, CC("-")) ? s : "");
      return cb;
   }
   return    QStyledItemDelegate::createEditor (par, opt, ind);
}

void SIDlg::setEditorData (QWidget *ed, const QModelIndex &ind)  const
{ QComboBox *cb = qobject_cast<QComboBox *>(ed);
   if (cb) {
     int i = cb->findText (ind.data (Qt::EditRole).toString ());
      if (i < 0)  i = 0;
      cb->setCurrentIndex (i);   cb->showPopup ();
      connect (cb, & QComboBox::currentIndexChanged,
               this,   & SIDlg::cbChanged);
   }
   else  QStyledItemDelegate::setEditorData (ed, ind);
}

void SIDlg::setModelData (QWidget *ed, QAbstractItemModel *mod,
                          const QModelIndex &ind)  const
{ QComboBox *cb = qobject_cast<QComboBox *>(ed);
   if (cb)  mod->setData (ind, cb->currentText (), Qt::EditRole);
   else  QStyledItemDelegate::setModelData (ed, mod, ind);
}


void CtlTabl::Init (QTableWidget *t, const char *hdr, ppop pop)
// hdr is zz string of labels
//  *  prefix means icon
//  >| prefix means right or center just
//  _^ prefix means string or combo edit
{ ubyte c;
  char *h;
  char  ed = '\0';      // _ means editing, ^ means QComboBox so delegate too
  QStringList sl;
   _t = t;
   for (c = 0, h = CC(hdr);  *h;  c++, h = & h [StrLn (h)+1]) {
      _ju [c] = _ed [c] = '\0';
      if ((*h == '*') || (*h == '>') || (*h== '|'))  _ju [c] = *h++;
      if  (*h == '_') {_ed [c] =      *h++;   if (ed != '^') ed = '_';}
      if  (*h == '^')  _ed [c] = ed = *h++;
      sl << QString::fromStdString (h);
   }
   _t->horizontalHeader ()->setSectionResizeMode (
                                                 QHeaderView::ResizeToContents);
   _t->setColumnCount (c);   _t->setHorizontalHeaderLabels (sl);
   _t->verticalHeader ()->hide ();
   _t->setAlternatingRowColors (false);
   if (ed == '^')  _t->setItemDelegate (new SIDlg (_t, _ed, pop));
   _t->setEditTriggers (ed ? QAbstractItemView::AllEditTriggers
                           : QAbstractItemView::NoEditTriggers);
   _t->setSelectionBehavior (QAbstractItemView::SelectRows);
}

/* _t->horizontalHeader ()->setSectionResizeMode (2, QHeaderView::Fixed)
** _t->horizontalHeader ()->setStretchLastSection (true);
** _t->horizontalHeader ()->setHighlightSections (false);
** _t->setSelectionBehavior (QAbstractItemView::SelectItems/Rows);
** _t->setSelectionMode     (QAbstractItemView::ExtendedSelection);
** _t->setGridShow (false);
*/

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

void  CtlTabl::Set (ubyt2 r, ubyte c, char *s)
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
   _t->blockSignals (false);
}

void CtlTabl::HopTo (ubyt2 r, ubyte c)  {_t->scrollToItem   (_t->item (r, c));
                                         _t->setCurrentItem (_t->item (r, c));}
void CtlTabl::SetColor (ubyt2 r, QColor c)
{  for (ubyte i = NCol ();  i;  i--) {
//    _t->item (r, i-1)->setForeground (QBrush (CBLACK));
      _t->item (r, i-1)->setForeground (QBrush (c));
   }
}

void CtlTabl::Open ()
{  _tr = CurRow ();   _tc = CurCol ();
   _t->hide ();   _t->blockSignals (true);
   _t->clearContents ();   _t->setRowCount (0);
   _nr = 0;
}

void CtlTabl::Put (char **rp)
{ ubyte c;
  TStr  ico;
  QTableWidgetItem *it;
   _t->setRowCount (_nr+1);
   for (c = 0;  *rp;  c++, rp++) {
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
         if (_ju [c] == '>')  it->setTextAlignment (Qt::AlignRight);
         if (_ju [c] == '|')  it->setTextAlignment (Qt::AlignCenter);
         it->setText (*rp);
      }
   }
   _nr++;
}

void CtlTabl::Shut ()
{  _t->show ();   _t->blockSignals (false);  HopTo (_tr, _tc);  }

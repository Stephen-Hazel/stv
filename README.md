
# stv -- Steve common cpp classes n funcs n stuff

environment:  Qt c++ in a flatpak

CMakeList.txt s using me will have 
   ../../stv/os.cpp
   ../../stv/ui.cpp
   (etc)

--------------------------------------------------------------------------------

## maybe some day I'll doc all these dang things...  maybe not...


## to do:
```
os.cpp  File::Open(paths/fn,r)
   make it case INsensitive on read
   that's the dumbest thing about linux sigh
   if open fails  (prob cuz case),
      lowercase the paths/fn and StrCm existing paths/fn to find it to open,r
   (technically this is a fix to stv library, not shazware itself)
```

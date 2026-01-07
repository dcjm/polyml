/*
    Title:      X-Windows/Motif Interface.

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#if (defined(WITH_XWINDOWS))
// X-Windows is required.

/* xwindows.c */

/* Removed indirection from get_C_* functions SPF 31/10/93 */
/* Added Handle type 2/11/93 */
/* Fixed "GetString can only be used once" bug 17/11/93 */

/* Dealing with gcc warning messages SPF 6/1/94 */
/* Retrofit to old Sun cc SPF 7/1/94 */
/* 25/1/94 SPF Fixed bug in EmptyVisual (core-dump when v==NULL) */

/* Comment added 4/11/93 SPF

Global Invariants:

(1) Get functions promise not to allocate on the Poly/ML heap

(2) The Poly/ML heap contains pointers into the C heap!
    As these are only valid for one session, the run-time
    system records which Poly/ML objects have been created
    in the current session. Only these objects contain
    valid C pointers, and so may be dereferenced.
    
    The "bad" Poly/ML objects are:

Flags      Object                      Bad Field                    Access Function
-----      ------                      ---------                    ---------------
M      X_GC_Object             GC               *gc                 GetGC                       
       X_Font_Object           Font             *font               GetFont
       ditto                   XFontStruct     **fs                 GetFS
       X_Cursor_Object         Cursor           *cursor             GetCursor
BM     X_Window_Object         Drawable         *drawable           GetDrawable, GetPixmap
       X_Pixmap_Object         Pixmap           *pixmap             GetDrawable, GetPixmap
       X_Colormap_Object       Colormap         *cmap               GetColormap
       X_Visual_Object         Visual          **visual             GetVisual  (* FISHY *)
B      X_Display_Object        Display          *display            (?) GetDisplay (?)
       ditto                   XtAppContext      app_context        NONE(?)
M      X_Widget_Object         Widget           *widget             GetWidget, GetNWidget
B      X_Trans_Object          XtTranslations    table              GetTrans
B      X_Acc_Object            XtAccelerators    acc                GetAcc
       
   WARNING: the above list of unsafe fields was created by SPF
            and may be incomplete.

   The function CheckExists should be called on these objects
   before it is safe to use any of the above fields. That's
   because the object may have been created in a previous ML
   session, so the pointers that it contains may no longer be
   valid. Using the appropriate access function listed above
   guarantees that CheckExists is called.
   
   Exception: the fields can safely be tested against C's zero
   (None, Null) even if CheckExists hasn't been called. Note that
   this is only database-safe because this value is used for
   uninitialised fields, so it doesn't confuse the garbage-collector.
   
   For all the above fields EXCEPT display, app_context, table, acc
   the run-time system creates an indirection object in the Poly heap.
   These fields don't need an indirection object because the object
   which contains them is itself a BYTE object.
   
   This indirection is a byte-object. The indirection is necessary
   because the garbage collector would object to finding a C pointer
   in a standard ML labelled record. The alternative would be to store
   the C pointer as an ML integer, but then we would have to convert
   back to a C pointer befor we could dereference it.
   
   For similar reasons, eventMask is also stored as a boxed PolyWord.

  abstype Colormap = Colormap with end; (* X_Colormap_Object *)
  abstype Cursor   = Cursor   with end; (* X_Cursor_Object   *)
  abstype Drawable = Drawable with end; (* X_Window_Object, XPixmap_Object *)
  abstype Font     = Font     with end; (* X_Font_Object     *)
  abstype GC       = GC       with end; (* X_GC_Object       *)
  abstype Visual   = Visual   with end; (* X_Visual_Object   *)
  abstype Display  = Display  with end; (* X_Display_Object  *)

  abstype Widget         = Widget         of int with end;
  abstype XtAccelerators = XtAccelerators of int with end;
  abstype XtTranslations = XtTranslations of int with end;
*/



/* MLXPoint, MLXRectangle, MLXArc, MLPair, MLTriple added 31/10/93 SPF */

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

/* what goes wrong? ... gid, fd, private15 inaccessible */
/* THIS NEEDS TO BE FIXED!!!! */
#define XLIB_ILLEGAL_ACCESS 1       /* We need access to some opaque structures */

/* use prototypes, but make sure we get Booleans, not ints */
#define NeedWidePrototypes 0

#include <X11/Xlib.h>

#include <X11/keysym.h> /* IsCursorKey, IsFunctionKey, et cetera */
#include <X11/Xproto.h> /* needed for protocol names such as X_CreateWindow */
#include <X11/Xatom.h>  /* XA_ATOM, et cetera */

#include <Xm/Xm.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Command.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>

/* Motif 1.2 */
#include <Xm/VendorS.h>
/* for XmIsDesktopObject */
#include <Xm/DesktopP.h>
/* for XmIsExtObject */
#include <Xm/ExtObjectP.h>
/* for XmIsShellExt */
#include <Xm/ShellEP.h>
/* for XmIsVendorShellExt */
#include <Xm/VendorSEP.h>
#if(0)
/* for XmIsWorldObject */
/* This is not supported in FreeBSD or Solaris 8. */
#include <Xm/WorldP.h>
#endif

#include "globals.h"
#include "sys.h"
#include "xwindows.h"

#include "run_time.h"
#include "arb.h"
#include "mpoly.h"
#include "gc.h"
#include "xcall_numbers.h"
#include "diagnostics.h"
#include "processes.h"
#include "save_vec.h"
#include "polystring.h"
#include "scanaddrs.h"
#include "memmgr.h"
#include "machine_dep.h"
#include "processes.h"
#include "rts_module.h"
#include "rtsentry.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyXWindowsGeneral(POLYUNSIGNED threadId, POLYUNSIGNED params);
}
/* The following are only forward so we can declare attributes */
static void RaiseXWindows(TaskData *taskData, const char *s) __attribute__((noreturn));


#define ButtonClickMask (((unsigned)1 << 29))

#define XMASK(m) ((m) &~ButtonClickMask)

#undef SIZEOF

#define debug1(fmt,p1) { /*EMPTY*/ }
#undef  debug1
#define debug1(fmt,p1) {if (debugOptions & DEBUG_X) printf(fmt,p1);}
#define debug3(fmt,p1,p2,p3) {if (debugOptions & DEBUG_X) printf(fmt,p1,p2,p3);}

#define debugCreate(type,value)  debug1("%lx " #type " created\n",(unsigned long)(value))
#define debugReclaim(type,value) debug1("%lx " #type " reclaimed\n",(unsigned long)(value))
#define debugReclaimRef(type,value) debug1("%lx " #type " reference reclaimed\n",(unsigned long)(value))
#define debugRefer(type,value) debug1("%lx " #type " referenced\n",(unsigned long)(value))
#define debugCreateCallback(MLValue,CValue,CListCell)  debug3("%p Widget callback reference created (%p,%p)\n",CValue,CListCell,MLValue)
#define debugReclaimCallback(MLValue,CValue,CListCell) debug3("%p Widget callback reference removed (%p,%p)\n",CValue,CListCell,MLValue)


/* forward declarations */

static Atom WM_DELETE_WINDOW(Display *d); /* was int SPF 6/1/94 */

#define DEREFDISPLAYHANDLE(h) ((X_Display_Object *)DEREFHANDLE(h))
#define DEREFWINDOWHANDLE(h)  ((X_Window_Object *)DEREFHANDLE(h))
#define DEREFXOBJECTHANDLE(h) ((X_Object *)DEREFHANDLE(h))

#define SAVE(x) taskData->saveVec.push(x)

#define Make_int(x) Make_arbitrary_precision(taskData, x)
#define Make_string(s) SAVE(C_string_to_Poly(taskData, s))
#define Make_bool(b) Make_arbitrary_precision(taskData, (b) != 0)

#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))

#define min(a,b) (a < b ? a : b)
#define max(a,b) (a > b ? a : b)

#define ISNIL(p)  (ML_Cons_Cell::IsNull(p))
#define NONNIL(p) (!ISNIL(p))

typedef Handle EventHandle;


/********************************************************************************/
/* Objects are created MUTABLE and are FINISHED when all their fields have been */
/* filled in (assuming they are immutable objects). This is so that we can      */
/* consider the possibility of storing immutable objects in read-only memory    */
/* segments (not currently implemented).    SPF 7/12/93                         */
/********************************************************************************/
static Handle FINISHED(TaskData *taskData, Handle P)
{
    PolyObject *pt = DEREFHANDLE(P);
    assert(taskData->saveVec.isValidHandle(P));
    assert(pt->IsMutable());
    POLYUNSIGNED lengthW = pt->LengthWord();
    pt->SetLengthWord(lengthW & ~_OBJ_MUTABLE_BIT);
    return P;
}


static void RaiseXWindows(TaskData *taskData, const char *s)
{
  if (mainThreadPhase == MTP_USER_CODE)
    {
      raise_exception_string(taskData, EXC_XWindows,s);
    }
  else
    {
     /* Crash added 7/7/94 SPF */
     Crash("Tried to raise exception (XWindows \"%s\") during garbage collection\n",s);
    }
  /*NOTREACHED*/
}


/* bugfixed 6/12/94 SPF */
#define RaiseXWindows2(varmessage,constmessage) \
{ \
   const char message[] = constmessage; \
   int n1 = strlen(varmessage); \
   int n2 = strlen(message); \
   char *mess = (char *)alloca(n1 + n2 + 1); \
   strcat(strncpy(mess,varmessage,n1),message); \
   RaiseXWindows(taskData, mess); \
   /*NOTREACHED*/ \
}

static void RaiseRange(TaskData *taskData)
{
  raise_exception0(taskData, EXC_size);
}

typedef unsigned char uchar;

static uchar get_C_uchar(TaskData *taskData, PolyWord a)
{
    unsigned u = get_C_ushort(taskData, a);
    
    if (u >= 256) RaiseRange(taskData);
    
    return u;
}


/******************************************************************************/
/*                                                                            */
/*      String                                                                */
/*                                                                            */
/******************************************************************************/

//#define String PolyStringObject

//#define GetString(s) _GetString((PolyWord *)(s))
/* can only be called TABLESIZE times per X opcode */
static PolyStringObject *GetString(PolyWord s)
{
#define TABLESIZE 5
  static PolyStringObject string[TABLESIZE];
  static int index = 0;
  
  if (! s.IsTagged()) return (PolyStringObject *) s.AsObjPtr();

  index = (index + 1) % TABLESIZE;
  string[index].length = 1;
  string[index].chars[0] = UNTAGGED(s);

  return &string[index];
#undef TABLESIZE
}


/******************************************************************************/
/*                                                                            */
/*      XObjects (Type definitions)                                           */
/*                                                                            */
/******************************************************************************/

/* We keep a list of all objects created by calls to X.         */
/* When an object is created we add an entry to the list and    */
/* return the entry. If the entry becomes inaccessible          */
/* by the garbage collector then we free the object.            */
/* The list is created by malloc so that it is not in the heap. */


// Types of objects.  These are tagged when they are stored
// in objects because some objects are not byte objects.
typedef enum
{
  X_GC       = 111,
  X_Font     = 222,
  X_Cursor   = 333,
  X_Window   = 444,
  X_Pixmap   = 555,
  X_Colormap = 666,
  X_Visual   = 777,
  X_Display  = 888,
  X_Widget   = 999,
  X_Trans    = 1111,
  X_Acc      = 2222
} X_types;

class X_Object: public PolyObject
{
public:
    X_Object(): type(TAGGED(1)) {} // Just to keep gcc happy
    PolyWord type;
};

class X_Trans_Object: public X_Object              /* BYTE object */
{
public:
  XtTranslations table;      /* C value */
};

class X_Acc_Object: public X_Object               /* BYTE object */
{
public:
  XtAccelerators acc;        /* C value */
};

class X_Display_Object: public X_Object             /* BYTE object */
{
public:
  Display     *display;     /* C value */
  unsigned     screen;      /* C value */
  XtAppContext app_context; /* C value */
} ;  

class X_Font_Object: public X_Object
{
public:
  Font             *font;  /* Token for C value */
  XFontStruct     **fs;    /* Token for C value */
  X_Display_Object *ds;    /* Token */
} ;

class X_Cursor_Object: public X_Object
{
public:
  Cursor           *cursor; /* Token for C value */
  X_Display_Object *ds;     /* Token */
} ;

class X_Pixmap_Object: public X_Object
{
public:
  Pixmap           *pixmap;  /* Token for C value */
  X_Display_Object *ds;      /* Token */
} ;

class X_Colormap_Object: public X_Object
{
public:
  Colormap         *cmap;  /* Token for C value */
  X_Display_Object *ds;    /* Token */
} ;

class X_Widget_Object: public X_Object                    /* MUTABLE */
{
public:
  Widget       *widget;       /* Token for C value */
  PolyWord     callbackList; /* mutable */
  PolyWord     state;        /* mutable */
  X_Display_Object *ds;           /* Token */
} ;

class X_Visual_Object: public X_Object
{
public:
  Visual          **visual;  /* Token for C value */
  X_Display_Object *ds;      /* Token */
} ;

class X_GC_Object: public X_Object                   /* MUTABLE */
{
public:
  GC               *gc;          /* Token for C value */
  X_Font_Object    *font_object; /* mutable; may be 0 */
  X_Pixmap_Object  *tile;        /* mutable; may be 0 */
  X_Pixmap_Object  *stipple;     /* mutable; may be 0 */
  X_Pixmap_Object  *clipMask;    /* mutable; may be 0 */
  X_Display_Object *ds;          /* Token */
} ;

class X_Window_Struct: public X_Object                 /* MUTABLE */
{
public:
  Drawable          *drawable;         /* Token for C value */
  PolyWord          handler;          /* mutable? */
  PolyWord          state;            /* mutable? */
  PolyObject        *eventMask;        /* Token for C value; token itself is mutable */
  X_Colormap_Object *colormap_object;  /* mutable; may be 0 */
  X_Cursor_Object   *cursor_object;    /* mutable; may be 0 */
  X_Pixmap_Object   *backgroundPixmap; /* mutable; may be 0 */
  X_Pixmap_Object   *borderPixmap;     /* mutable; may be 0 */
  X_Window_Struct   *parent;           /* may be 0 */
  X_Display_Object  *ds;               /* Token */
};

typedef X_Window_Struct X_Window_Object;


/******************************************************************************/
/*                                                                            */
/*      Forward declarations                                                  */
/*                                                                            */
/******************************************************************************/
static Font           GetFont(TaskData *taskData, X_Object *P);
static Cursor         GetCursor(TaskData *taskData,X_Object *P);
static Colormap       GetColormap(TaskData *taskData,X_Object *P);
static Visual        *GetVisual(TaskData *taskData,X_Object *P);
static XtTranslations GetTrans(TaskData *taskData,X_Object *P);
static XtAccelerators GetAcc(TaskData *taskData,X_Object *P);
static Pixmap         GetPixmap(TaskData *, X_Object *P);
static Widget         GetNWidget(TaskData *, X_Object *P);
static Window         GetWindow(TaskData *, X_Object *P);
static Display       *GetDisplay(TaskData *, X_Object *P);

static void DestroyWindow(X_Object *W);
static void DestroySubwindows(X_Object *W);

static X_GC_Object     *GCObject(X_Object *P);
static X_Pixmap_Object *PixmapObject(X_Object *P);
static X_Widget_Object *WidgetObject(TaskData *, X_Object *P);
static X_Window_Object *WindowObject(X_Object *P);

/******************************************************************************/
/*                                                                            */
/*      C lists (Type definitions)                                            */
/*                                                                            */
/******************************************************************************/

typedef struct X_List_struct X_List;

struct X_List_struct
{
  X_List   *next;           /* pointer into C heap */
  X_Object *object;         /* pointer into Poly heap; weak */
};

typedef struct timeval TimeVal;

/* In C heap */
typedef struct T_List_struct T_List;
struct T_List_struct
{
  T_List          *next;           /* pointer into C heap */
  TimeVal          timeout;       /* here */
  X_Window_Object *window_object; /* pointer into Poly heap, or 0; weak */
  X_Widget_Object *widget_object; /* pointer into Poly heap, or 0; strong */
  PolyObject       *alpha;         /* pointer into Poly heap; strong */
  PolyObject       *handler;       /* pointer into Poly heap; strong */
  int              expired;       /* here */
};
/* NB precisely one of window_object and widget_object should be non-zero */

/* In C heap */
typedef struct C_List_struct C_List;
struct C_List_struct
{
  PolyObject      *function; /* pointer into Poly heap; strong */
  X_Widget_Object *widget_object;   /* pointer into Poly heap; strong */
  C_List          *next;     /* pointer into C heap */
};

/* lists of X objects currently in Poly heap i.e. those created in this session */
#define XLISTSIZE 1001 /* must be coprime to 4 ('cos pointers are PolyWord-aligned) */
static X_List *XList[XLISTSIZE] = {0};

static T_List *TList       = 0; /* C pending messages list, ordered by arrival time */
static C_List *CList       = 0; /* Acts as root for objects "owned" by C callbacks */
static PolyWord FList = TAGGED(0); /* ML Callback list - acts as a Root for the Heap */
static PolyWord GList = TAGGED(0); /* ML Event list - acts as a Root for the Heap */

static Bool callbacks_enabled = False;



/******************************************************************************/
/*                                                                            */
/*      High-speed XList routines                                             */
/*                                                                            */
/******************************************************************************/

/* maps an (X_Object *) to an (unsigned); this mapping from must give the same   */
/* (unsigned) for each (X_Object) for an entire Poly/ML session, even though its */
/* address may change at every garbage collection.                               */
/* The way we achieve this is by returning the address of the corresponding C    */
/* object. Note that since the ML object doesn't necessarily correspond to a real*/
/* C object, this value may be neither valid nor sensible (but it WILL be a      */
/* constant).                                                                    */
/* Unfortunately, we can't do this for GCs or VISUALS, since the actual C object */
/* contains the id we want, and we can't access the id if we haven't got the     */
/* object. For these, we return a constant instead.                              */
static unsigned long hashId(X_Object *P)
{

#define HASH_GC     0
#define HASH_VISUAL 1

  switch(UNTAGGED(P->type))
  {
    case X_GC:       return HASH_GC;
    case X_Font:     return (unsigned long)(*(((X_Font_Object*)P)->font));
    case X_Cursor:   return (unsigned long)(*(((X_Cursor_Object*)P)->cursor));
    case X_Window:   return (unsigned long)(*(((X_Window_Struct*)P)->drawable));
    case X_Pixmap:   return (unsigned long)(*(((X_Pixmap_Object*)P)->pixmap));
    case X_Colormap: return (unsigned long)(*(((X_Colormap_Object*)P)->cmap));
    case X_Visual:   return HASH_VISUAL;
    case X_Display:  return (unsigned long)(((X_Display_Object*)P)->display);
    case X_Widget:   return (unsigned long)(*(((X_Widget_Object*)P)->widget));
    case X_Trans:    return (unsigned long)(((X_Trans_Object*)P)->table);
    case X_Acc:      return (unsigned long)(((X_Acc_Object*)P)->acc);
    default:         Crash ("Bad X_Object type (%d) in hashId",UNTAGGED(P->type));
  }
  /*NOTREACHED*/
}

static void initXList(void)
{
  int i;
  
  for (i = 0; i < XLISTSIZE; i++)
    {
      XList[i] = NULL;
    }
}

static X_List **hashXList(X_Object *P)
{
  unsigned long id = hashId(P);
  unsigned n  = (id % XLISTSIZE); /* a poor hash function, but good enough for now */
  return &(XList[n]);
}

static X_List *findXList(unsigned long id)
{
  unsigned n  = (id % XLISTSIZE); /* a poor hash function, but good enough for now */
  return XList[n];
}

/******************************************************************************/
/*                                                                            */
/*      C lists (Polymorphic functions)                                       */
/*                                                                            */
/******************************************************************************/
// Creates a list from a vector of items.

static Handle CreateList4(TaskData *taskData, unsigned n, void *p, unsigned objSize, Handle (*f)(TaskData *, void *))
{

    Handle saved = taskData->saveVec.mark();
    Handle list  = SAVE(ListNull);
    // Process the vector in reverse order.  That way we can make the
    // cells as immutable objects rather than having to create them as
    // mutable and then lock them.
    while (n)
    {
        n--;
        byte *objP = (byte*)p + objSize*n;
        Handle value = (* f)(taskData, objP);
        Handle next  = alloc_and_save(taskData, SIZEOF(ML_Cons_Cell));
        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
        
        /* reset save vector to stop it overflowing */    
        taskData->saveVec.reset(saved);
        list = SAVE(DEREFHANDLE(next));
    }
    
    return list;
}

static Handle CreateList4I(TaskData *taskData, unsigned n, void *p, unsigned objSize,
                           Handle (*f)(TaskData *, void *, unsigned i))
{

    Handle saved = taskData->saveVec.mark();
    Handle list  = SAVE(ListNull);
    while (n)
    {
        n--;
        byte *objP = (byte*)p + objSize*n;
        Handle value = (* f)(taskData, objP, n);
        Handle next  = alloc_and_save(taskData, SIZEOF(ML_Cons_Cell));
        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
        
        /* reset save vector to stop it overflowing */    
        taskData->saveVec.reset(saved);
        list = SAVE(DEREFHANDLE(next));
    }
    
    return list;
}

static Handle CreateList5(TaskData *taskData, POLYUNSIGNED n, void *p, POLYUNSIGNED objSize,
                          Handle (*f)(TaskData *, void *, Handle), Handle a1)
{
    Handle saved = taskData->saveVec.mark();
    Handle list  = SAVE(ListNull);
    // Process the vector in reverse order.  That way we can make the
    // cells as immutable objects rather than having to create them as
    // mutable and then lock them.
    while (n)
    {
        n--;
        byte *objP = (byte*)p + objSize*n;
        Handle value = (* f)(taskData, objP, a1);
        Handle next  = alloc_and_save(taskData, SIZEOF(ML_Cons_Cell));
        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
        
        /* reset save vector to stop it overflowing */    
        taskData->saveVec.reset(saved);
        list = SAVE(DEREFHANDLE(next));
    }
    
    return list;
}

static void GetList4(TaskData *taskData, PolyWord list, void *v, unsigned bytes,
                     void (*get)(TaskData *, PolyWord, void*, unsigned))
{
    unsigned i = 0;
    byte *s = (byte*)v;
    
    for(PolyWord p = list; NONNIL(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
    {
        (* get)(taskData, ((ML_Cons_Cell*)p.AsObjPtr())->h, s, i);
        s += bytes;
        i++;
    }
}

/* ListLength no longer requires indirection via handle SPF 4/11/93 */
static unsigned ListLength(PolyWord list)
{
    unsigned  n = 0;
    
    for(PolyWord p = list; NONNIL(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t) n++;
    
    return n;
}

/******************************************************************************/
/*                                                                            */
/*      TList Purge Functions (SPF 29/11/93)                                  */
/*                                                                            */
/******************************************************************************/
static void PurgePendingWidgetMessages(X_Widget_Object *P)
{
  T_List **T = &TList;
  
  while(*T)
  {
    T_List *t = *T;
    
    if (t->widget_object == P)  /* clear out pending messages for this widget */
    {
      *T = t->next;
      free(t);
    }
    else T = &t->next;
  }
}  

static void PurgePendingWindowMessages(X_Window_Object *P)
{
  T_List **T = &TList;
  
  while(*T)
  {
    T_List *t = *T;
    
    if (t->window_object == P)  /* clear out pending messages for this window */
    {
      *T = t->next;
      free(t);
    }
    else T = &t->next;
  }
}  

/******************************************************************************/
/*                                                                            */
/*      CList Purge Functions (SPF 29/2/96)                                   */
/*                                                                            */
/******************************************************************************/
static void PurgeCCallbacks(X_Widget_Object *P, Widget w)
{
  C_List **C = &CList;
  
  while(*C)
  {
    C_List *c = *C;
    
    if (c->widget_object == P)  /* clear out callback info for this widget */
    {
      debugReclaimCallback(P,w,c);
      *C = c->next;
      free(c);
    }
    else C = &c->next;
  }
}  

/******************************************************************************/
/*                                                                            */
/*      XObjects (Polymorphic functions 1)                                    */
/*                                                                            */
/******************************************************************************/
static int ResourceExists(X_Object *P)
{
  X_List *L;

  for(L = *hashXList(P); L; L = L->next)
    {
      if (L->object == P) return 1;
    }

  return 0;
}

/* SafeResourceExists is like ResourceExists but doesn't assume that
   we actually have a valid X object, so it doesn't use hashing.
   SPF 6/4/95 */ 
static int SafeResourceExists(X_Object *P)
{
  unsigned n;
  
  for (n = 0; n < XLISTSIZE; n++)
  {
    X_List *L;
  
    for(L = XList[n]; L; L = L->next)
      {
    if (L->object == P) return 1;
      }
  }
  return 0;
}

static void DestroyXObject(X_Object *P)
{
    TaskData *taskData = processes->GetTaskDataForThread();
    
    X_List **X = hashXList(P);
    
    switch(UNTAGGED(P->type))
    {
    case X_GC:
        {
            X_GC_Object *G = GCObject(P);
            
            GC       gc = *G->gc;
            Display *d  =  G->ds->display;
            
            if (gc == DefaultGC(d,G->ds->screen))
            {
                debugReclaimRef(GC,gc->gid);
            }
            else
            {
                debugReclaim(GC,gc->gid);
                XFreeGC(d,gc); /* SAFE(?) */
            }
            break;
        }
        
    case X_Font:
        {
            Font f = GetFont(taskData, P);
            
            if (f == None)
            {
                debugReclaimRef(Font,f);
            }
            else
            {
                debugReclaim(Font,f);
                
#if NEVER
                XUnloadFont(GetDisplay(taskData, P),f);
#endif
            }
            break;
        }
        
    case X_Cursor:
        {
            Cursor cursor = GetCursor(taskData, P);
            
            if (cursor == None)
            {
                debugReclaimRef(Cursor,cursor);
            }
            else
            {
                debugReclaim(Cursor,cursor);
                
#if NEVER
                XFreeCursor(GetDisplay(taskData, P),cursor);
#endif
            }
            
            break;
        }
        
    case X_Window:
        {
            /* added 29/11/93 SPF */
            PurgePendingWindowMessages(WindowObject(P));
            
            if (((X_Window_Object *)P)->parent != 0) /* this clients window */
            {
                debugReclaim(Window,GetWindow(taskData, P));
                DestroyWindow(P);
            }
            else /* None, ParentRelative, and other clients windows */
            {
                debugReclaimRef(Window,GetWindow(taskData, P));
            }
            break;
        }
        
    case X_Pixmap:
        {
            Pixmap pixmap = GetPixmap(taskData, P);
            
            if (pixmap == None)
            {
                debugReclaimRef(Pixmap,pixmap);
            }
            else
            {
                debugReclaim(Pixmap,pixmap);
                
#if NEVER
                XFreePixmap(GetDisplay(taskData, P),pixmap);
#endif
            }
            
            break;
        }
        
    case X_Colormap:
        {
            Colormap cmap = GetColormap(taskData, P);
            
            if (cmap == None)
            {
                debugReclaimRef(Colormap,cmap);
            }
            else
            {
                debugReclaim(Colormap,cmap);
#if NEVER
                XFreeColormap(GetDisplay(taskData, P),cmap);
#endif
            }
            break;
        }
        
    case X_Visual:
        {
            Visual *visual = GetVisual(taskData, P);
            
            debugReclaimRef(Visual,visual->visualid);
            break;
        }
        
    case X_Widget:
        {
            Widget widget = GetNWidget(taskData, P);
            
            PurgePendingWidgetMessages(WidgetObject(taskData, P));
            debugReclaimRef(Widget,widget);
            break;
        }
        
    case X_Trans:
        {
            XtTranslations table = GetTrans(taskData, P);
            
            debugReclaimRef(Trans,table);
            break;
        }
        
    case X_Acc:
        {
            XtAccelerators acc = GetAcc(taskData, (X_Object *)P);
            
            debugReclaimRef(Acc,acc);
            break;
        }
        
    default: Crash ("Unknown X_Object type %d",UNTAGGED(P->type));
  }
  
  while(*X)
  {
      X_List *L = *X;
      
      if (L->object == P)
      {
          *X = L->next;
          free(L);
          return;
      }
      else X = &L->next;
  }
  printf("DestroyXObject: destroy failed\n");
}

#define CheckExists(P,resource) \
{\
  if (! ResourceExists(P)) RaiseXWindows(taskData, (char*) "Non-existent " #resource); \
}

static X_Font_Object *FontObject(X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Font);

  return (X_Font_Object *)P;
}

static X_Object *FindResource
(
  Handle   dsHandle,   /* Handle to (X_Display_Object *) */
  X_types  type,
  unsigned long id,
  unsigned long hashid
)
{
  X_List *L;
  X_Display_Object *d = (type == X_Widget) ? NULL : DEREFDISPLAYHANDLE(dsHandle);

  for(L = findXList(hashid); L; L = L->next)
  {
    X_Object *P = L->object;
    
    if (UNTAGGED(P->type) == type)
    {
      switch(type)
      {
        case X_GC:       if (((X_GC_Object*)P)->ds == d && (*((X_GC_Object*)P)->gc)->gid          == id) return P; break;
        case X_Font:     if (((X_Font_Object*)P)->ds == d && (*((X_Font_Object*)P)->font)             == id) return P; break;
        case X_Cursor:   if (((X_Cursor_Object*)P)->ds == d && (*((X_Cursor_Object*)P)->cursor)           == id) return P; break;
        case X_Window:   if (((X_Window_Object*)P)->ds == d && (*((X_Window_Object*)P)->drawable)         == id) return P; break;
        case X_Pixmap:   if (((X_Pixmap_Object*)P)->ds == d && (*((X_Pixmap_Object*)P)->pixmap)           == id) return P; break;
        case X_Colormap: if (((X_Colormap_Object*)P)->ds == d && (*((X_Colormap_Object*)P)->cmap)             == id) return P; break;
        case X_Visual:   if (((X_Visual_Object*)P)->ds == d && (*((X_Visual_Object*)P)->visual)->visualid == id) return P; break;
        
        case X_Widget:   if (*(((X_Widget_Object*)P)->widget) == (Widget) id) return P; break;
        
        case X_Display:  break;
        case X_Trans:    break;
        case X_Acc:      break;

        default: Crash ("Bad X_Object type (%d) in FindResource", type);
      }
    }
  }

  return 0;
}

// Why are there these casts to unsigned here????
#define FindWindow(d,id)   ((X_Window_Object *)   FindResource(d,X_Window,(unsigned long)id,(unsigned long)id))
#define FindPixmap(d,id)   ((X_Pixmap_Object *)   FindResource(d,X_Pixmap,(unsigned long)id,(unsigned long)id))
#define FindCursor(d,id)   ((X_Cursor_Object *)   FindResource(d,X_Cursor,(unsigned long)id,(unsigned long)id))
#define FindFont(d,id)     ((X_Font_Object *)     FindResource(d,X_Font,(unsigned long)id,(unsigned long)id))
#define FindColormap(d,id) ((X_Colormap_Object *) FindResource(d,X_Colormap,(unsigned long)id,(unsigned long)id))
#define FindWidget(id)     ((X_Widget_Object *)   FindResource((Handle)NULL,X_Widget,(unsigned long)id,(unsigned long)id))

/* can't use id for hashing in the following, so use arbitrary values instead */
#define FindGC(d,id)       ((X_GC_Object *)       FindResource(d,X_GC,(unsigned long)id,HASH_GC))
#define FindVisual(d,id)   ((X_Visual_Object *)   FindResource(d,X_Visual,(unsigned long)id,HASH_VISUAL))

static Handle AddXObject(Handle objectHandle)
{
  X_List **X = hashXList(DEREFXOBJECTHANDLE(objectHandle));
  X_List  *L = (X_List *) malloc(sizeof(X_List));

  L->next   = *X;
  L->object = (X_Object *)DEREFHANDLE(objectHandle);

  *X = L;

  return objectHandle;
}

/******************************************************************************/
/*                                                                            */
/*      MLXPoint - implements ML XPoint datatype                              */
/*                                                                            */
/******************************************************************************/
typedef struct /* depends on XPoint datatype + ML compiler hash function */
{
    PolyWord    x; /* ML int */
    PolyWord    y; /* ML int */
} MLXPoint;

inline MLXPoint * Point(PolyWord p) { return (MLXPoint *) p.AsObjPtr(); }

/* shouldn't these be long values? */
inline short GetPointX(TaskData *taskData, PolyWord p) { return get_C_short(taskData, Point(p)->x); }
inline short GetPointY(TaskData *taskData, PolyWord p) { return get_C_short(taskData, Point(p)->y); }

inline short GetOffsetX(TaskData *taskData, PolyWord p) { return get_C_ushort(taskData, Point(p)->x); }
inline short GetOffsetY(TaskData *taskData, PolyWord p) { return get_C_ushort(taskData, Point(p)->y); }

static Handle CreatePoint(TaskData *taskData, int x, int y)
{
  Handle pointHandle = alloc_and_save(taskData, SIZEOF(MLXPoint), F_MUTABLE_BIT);
  
/* Still allocating, so must use explicit DEREF for each element */
#define point ((MLXPoint *)DEREFHANDLE(pointHandle))
  point->x = DEREFWORD(Make_int(x));
  point->y = DEREFWORD(Make_int(y));
#undef point

  return FINISHED(taskData, pointHandle);
}

static void GetPoints(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    XPoint *A = (XPoint *)v;
    A->x = GetPointX(taskData, p);
    A->y = GetPointY(taskData, p);
}

/******************************************************************************/
/*                                                                            */
/*      MLXRectangle - implements ML XRectangle datatype                      */
/*                                                                            */
/******************************************************************************/

typedef struct /* depends on XRectangle datatype + ML compiler hash function */
{
    PolyWord    top;     /* ML int */
    PolyWord    left;    /* ML int */
    PolyWord    right;   /* ML int */
    PolyWord    bottom;  /* ML int */
} MLXRectangle;

inline MLXRectangle *Rect(PolyWord R) { return (MLXRectangle *) R.AsObjPtr(); }

inline short GetRectTop(TaskData *taskData, PolyWord R)    { return get_C_short(taskData, Rect(R)->top); }
inline short GetRectLeft(TaskData *taskData, PolyWord R)   { return get_C_short(taskData, Rect(R)->left); }
inline short GetRectRight(TaskData *taskData, PolyWord R)  { return get_C_short(taskData, Rect(R)->right); }
inline short GetRectBottom(TaskData *taskData, PolyWord R) { return get_C_short(taskData, Rect(R)->bottom); }

#define GetRectX(taskData, R)  GetRectLeft(taskData, R)
#define GetRectY(taskData, R)  GetRectTop(taskData, R)

/* functions added 29/10/93 SPF */
static unsigned GetRectW(TaskData *taskData, PolyWord R)
{
  long result = GetRectRight(taskData, R) - GetRectLeft(taskData, R);

  if (result < 0) RaiseRange(taskData);
  return (unsigned)result;
}

static unsigned GetRectH(TaskData *taskData, PolyWord R)
{
  long result = GetRectBottom(taskData, R) - GetRectTop(taskData, R);

  if (result < 0) RaiseRange(taskData);
  return (unsigned)result;
}

/* static MLXRectangle **CreateRect(top,left,bottom,right) */
static Handle CreateRect(TaskData *taskData, int top, int left, int bottom, int right)
{
  Handle rectHandle = alloc_and_save(taskData, SIZEOF(MLXRectangle), F_MUTABLE_BIT);

/* Still allocating, so must use explicit DEREF for each element */
#define rect ((MLXRectangle *)DEREFHANDLE(rectHandle))
  rect->top    = DEREFWORD(Make_int(top));
  rect->left   = DEREFWORD(Make_int(left));
  rect->right  = DEREFWORD(Make_int(right));
  rect->bottom = DEREFWORD(Make_int(bottom));
#undef rect

  return FINISHED(taskData, rectHandle);
}

#define CreateArea(w,h) CreateRect(taskData, 0,0,(int)h,(int)w)

static void GetRects(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    XRectangle *A = (XRectangle *)v;
    A->x      = GetRectX(taskData, p);
    A->y      = GetRectY(taskData, p);
    A->width  = GetRectW(taskData, p);
    A->height = GetRectH(taskData, p);
}

static void CheckZeroRect(TaskData *taskData, PolyWord R)
{
  unsigned x = GetRectX(taskData, R);
  unsigned y = GetRectY(taskData, R);
  unsigned w = GetRectW(taskData, R);
  unsigned h = GetRectH(taskData, R);

  if (x != 0 || y != 0 || 
/*     w <= 0 || h <= 0 ||   w,h now unsigned SPF 29/10/93 */
       w == 0 || h == 0 || 
       w > 65535 || h > 65535) RaiseRange(taskData); 
}


/******************************************************************************/
/*                                                                            */
/*      MLXArc - implements ML XArc datatype                                  */
/*                                                                            */
/******************************************************************************/

/* MLXArc added 31/10/93 SPF; depends on ML XArc datatype */
typedef struct
{
    PolyWord    r;      /* MMLXRectangle* */
    PolyWord    a1;   /* ML int */
    PolyWord    a2;   /* ML int */
} MLXArc;

inline MLXArc *Arc(PolyWord A) { return (MLXArc *) A.AsObjPtr(); }

inline PolyWord GetArcR(PolyWord A)  { return Arc(A)->r; }
inline short GetArcA1(TaskData *taskData, PolyWord A) { return get_C_short(taskData, Arc(A)->a1); }
inline short GetArcA2(TaskData *taskData, PolyWord A) { return get_C_short(taskData, Arc(A)->a2); }

static void GetArcs(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    XArc *A = (XArc *)v;
    A->x      = GetRectX(taskData, GetArcR(p));
    A->y      = GetRectY(taskData, GetArcR(p));
    A->width  = GetRectW(taskData, GetArcR(p));
    A->height = GetRectH(taskData, GetArcR(p));
    A->angle1 = GetArcA1(taskData, p);
    A->angle2 = GetArcA2(taskData, p);
}


/******************************************************************************/
/*                                                                            */
/*      Colormap                                                              */
/*                                                                            */
/******************************************************************************/

static X_Colormap_Object *ColormapObject(X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Colormap);

  return (X_Colormap_Object *)P;
}

static Colormap GetColormap(TaskData *taskData, X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Colormap);

  /* val NoColormap = xcall (23,0) : Colormap; */
  /* special case for NoColormap - correct(?) */
  if ( *(((X_Colormap_Object *)P)->cmap) == None) return None;

  CheckExists(P,colormap);

  return *(((X_Colormap_Object *)P)->cmap);
}

static Handle EmptyColormap
(
  TaskData *taskData,
  Handle   dsHandle /* Handle to (X_Display_Object *) */,
  Colormap id
)
{
  X_Colormap_Object *E = FindColormap(dsHandle,id);

  if (E)
  {
    return SAVE(E);
  }
  else
  {
    Handle objectHandle = alloc_and_save(taskData, SIZEOF(X_Colormap_Object), F_MUTABLE_BIT);
    Handle cmapHandle   = alloc_and_save(taskData, 1, F_MUTABLE_BIT | F_BYTE_OBJ);
    
    /* Must do all allocations before we do the first dereference */
    X_Colormap_Object *object = (X_Colormap_Object *)DEREFHANDLE(objectHandle);
    Colormap          *cmap   = (Colormap *)DEREFHANDLE(cmapHandle);
    
    *cmap = id; FINISHED(taskData, cmapHandle);

    object->type = TAGGED(X_Colormap);
    object->cmap = cmap;
    object->ds   = DEREFDISPLAYHANDLE(dsHandle);

    debugRefer(Colormap,id);

    return AddXObject(FINISHED(taskData, objectHandle));
  }
}


/******************************************************************************/
/*                                                                            */
/*      Visual                                                                */
/*                                                                            */
/******************************************************************************/
static Visual *GetVisual(TaskData *taskData, X_Object *P)
{
  static Visual EMPTYVISUAL = { 0 };

  assert(UNTAGGED(P->type) == X_Visual);

  /* val NoVisual = xcall (24,0) : Visual; */
  /* special case for NoVisual */
  if (*(((X_Visual_Object *)P)->visual) == None) return &EMPTYVISUAL; /* FISHY (?) */

  CheckExists(P,visual);

  return *(((X_Visual_Object *)P)->visual);
}

static Handle EmptyVisual
(
  TaskData *taskData,
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  Visual *v
)
{
  if (v != None)
    {
      X_Visual_Object *E = FindVisual(dsHandle,v->visualid);

      if (E) return SAVE(E);
    }
  
  /* else */
  {
    Handle objectHandle = alloc_and_save(taskData, SIZEOF(X_Visual_Object), F_MUTABLE_BIT);
    Handle visualHandle = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
    
    /* Must do all allocations before we do the first dereference */
    X_Visual_Object *object = (X_Visual_Object *)DEREFHANDLE(objectHandle);
    Visual         **visual = (Visual **)DEREFHANDLE(visualHandle);
    
    *visual = v; FINISHED(taskData, visualHandle);
  
    object->type   = TAGGED(X_Visual);
    object->visual = visual;
    object->ds     = DEREFDISPLAYHANDLE(dsHandle);
  
    debugRefer(Visual,(v == None) ? None : v->visualid);
      
    return AddXObject(FINISHED(taskData, objectHandle));
  }
}

/******************************************************************************/
/*                                                                            */
/*      GC                                                                    */
/*                                                                            */
/******************************************************************************/
static X_GC_Object *GCObject(X_Object *P)
{
  assert(UNTAGGED(P->type) == X_GC);

  return (X_GC_Object *)P;
}

static GC GetGC(TaskData *taskData, X_Object *P)
{
  assert(UNTAGGED(P->type) == X_GC);

  CheckExists(P,gc);

  return *(((X_GC_Object *)P)->gc);
}


static Handle GetDefaultGC(TaskData *taskData, Handle dsHandle /* Handle to (X_Display_Object *) */)
{
  GC defaultGC = 
    DefaultGC(DEREFDISPLAYHANDLE(dsHandle)->display,
              DEREFDISPLAYHANDLE(dsHandle)->screen);

  X_GC_Object *G = FindGC(dsHandle,defaultGC->gid);

  if (G)
  {
    return SAVE(G);
  }
  else
  {
    Handle objectHandle = alloc_and_save(taskData, SIZEOF(X_GC_Object), F_MUTABLE_BIT);
    Handle GCHandle     = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
    
    /* Must do all allocations before we do the first dereference */
    X_GC_Object *object  = (X_GC_Object *)DEREFHANDLE(objectHandle);
    GC          *gc      = (GC *)DEREFHANDLE(GCHandle);

    *gc = defaultGC; FINISHED(taskData, GCHandle);

    debugRefer(GC,defaultGC->gid);

    object->type = TAGGED(X_GC);
    object->gc   = gc;
    object->ds   = DEREFDISPLAYHANDLE(dsHandle);
/*
    object->font_object = 0;
    object->tile        = 0;
    object->stipple     = 0;
    object->clipMask    = 0;
*/    

    return AddXObject(objectHandle); /* must stay MUTABLE */
  }
}

static void ChangeGC(TaskData *taskData, X_GC_Object *G, unsigned n, PolyWord P)
{
  XGCValues v;
  
  unsigned mask = 1 << n;
  
  switch(mask)
  {
    case GCFunction:          v.function           = get_C_ushort(taskData, P); break;
    case GCPlaneMask:         v.plane_mask         = get_C_ulong (taskData, P); break;
    case GCForeground:        v.foreground         = get_C_ulong (taskData, P); break;
    case GCBackground:        v.background         = get_C_ulong (taskData, P); break;
    case GCLineWidth:         v.line_width         = get_C_short (taskData, P); break;
    case GCLineStyle:         v.line_style         = get_C_ushort(taskData, P); break;
    case GCCapStyle:          v.cap_style          = get_C_ushort(taskData, P); break;
    case GCJoinStyle:         v.join_style         = get_C_ushort(taskData, P); break;
    case GCFillStyle:         v.fill_style         = get_C_ushort(taskData, P); break;
    case GCFillRule:          v.fill_rule          = get_C_ushort(taskData, P); break;
    case GCTileStipXOrigin:   v.ts_x_origin        = get_C_short (taskData, P); break;
    case GCTileStipYOrigin:   v.ts_y_origin        = get_C_short (taskData, P); break;
    case GCSubwindowMode:     v.subwindow_mode     = get_C_ushort(taskData, P); break;
    case GCGraphicsExposures: v.graphics_exposures = get_C_ushort(taskData, P); break;
    case GCClipXOrigin:       v.clip_x_origin      = get_C_short (taskData, P); break;
    case GCClipYOrigin:       v.clip_y_origin      = get_C_short (taskData, P); break;
    case GCDashOffset:        v.dash_offset        = get_C_ushort(taskData, P); break;
    case GCDashList:          v.dashes             = get_C_uchar (taskData, P); break;
    case GCArcMode:           v.arc_mode           = get_C_ushort(taskData, P); break;

    case GCFont:     v.font = GetFont(taskData, (X_Object *)P.AsObjPtr());
                     G->font_object = FontObject((X_Object *)P.AsObjPtr());
                     break;
                     
    case GCTile:     v.tile = GetPixmap(taskData, (X_Object *)P.AsObjPtr());
                     G->tile = PixmapObject((X_Object *)P.AsObjPtr());
                     break;
                     
    case GCStipple:  v.stipple = GetPixmap(taskData, (X_Object *)P.AsObjPtr());
                     G->stipple = PixmapObject((X_Object *)P.AsObjPtr());
                     break;
                     
    case GCClipMask: v.clip_mask = GetPixmap(taskData, (X_Object *)P.AsObjPtr());
                     G->clipMask = PixmapObject((X_Object *)P.AsObjPtr());
                     break;

    default: Crash ("Bad gc mask %u",mask);
  }
  
  XChangeGC(GetDisplay(taskData, (X_Object *)G),GetGC(taskData, (X_Object *)G),mask,&v);
}

static Handle CreateGC
(
  TaskData *taskData,
  Handle   dsHandle /* Handle to (X_Display_Object *) */,
  Drawable w
)
{
  Handle objectHandle = alloc_and_save(taskData, SIZEOF(X_GC_Object), F_MUTABLE_BIT);
  Handle GCHandle     = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
  
  /* Must do all allocations before we do the first dereference */
  X_GC_Object *object  = (X_GC_Object *)DEREFHANDLE(objectHandle);
  GC          *gc      = (GC *)DEREFHANDLE(GCHandle);

  *gc = XCreateGC(DEREFDISPLAYHANDLE(dsHandle)->display,w,0,0);
  FINISHED(taskData, GCHandle);

  debugCreate(GC,(*gc)->gid);

  object->type = TAGGED(X_GC);
  object->gc   = gc;
  object->ds   = DEREFDISPLAYHANDLE(dsHandle);
/*
  object->font_object = 0;
  object->tile        = 0;
  object->stipple     = 0;
  object->clipMask    = 0;
*/    
  return AddXObject(objectHandle); /* must remain MUTABLE */
}


/******************************************************************************/
/*                                                                            */
/*      Window                                                                */
/*                                                                            */
/******************************************************************************/
static X_Window_Object *WindowObject(X_Object *P)

{
  assert(UNTAGGED(P->type) == X_Window);

  return (X_Window_Object *)P;
}

static Window GetWindow(TaskData *taskData, X_Object *P)
{
  if (UNTAGGED(P->type) == X_Pixmap)
  {
    if (*((X_Pixmap_Object*)P)->pixmap == None) return None;
  
    RaiseXWindows(taskData, "Not a window");
  }

  assert(UNTAGGED(P->type) == X_Window);

  CheckExists(P,window);

  return *(((X_Window_Object*)P)->drawable);
}

static Handle EmptyWindow
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window w
)
{
  X_Window_Object *W = FindWindow(dsHandle,w);

  if (W)
  {
    return SAVE(W);
  }
  else
  {
  
    Handle objectHandle    = alloc_and_save(taskData, SIZEOF(X_Window_Object), F_MUTABLE_BIT);
    Handle eventMaskHandle = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
    Handle drawableHandle  = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
  
    /* Must do all allocations before we do the first dereference */
    X_Window_Object *object    = DEREFWINDOWHANDLE(objectHandle);
    Drawable        *drawable  = (Drawable *)DEREFHANDLE(drawableHandle);
    PolyObject      *eventMask = DEREFHANDLE(eventMaskHandle);
 
    *drawable  = w; FINISHED(taskData, drawableHandle);
    
#ifdef nodef
    /* DCJM: This gets in the way of trying to handle ButtonPress events -
       get rid of it. */
    /* so that Motif windows get ButtonClick XEvent structures */
    eventMask->Set(0, PolyWord::FromUnsigned(ButtonClickMask)); /* eventMask must remain MUTABLE */
#else
    eventMask->Set(0, PolyWord::FromUnsigned(0));
#endif

    object->type      = TAGGED(X_Window);
    object->drawable  = drawable;
    object->handler   = TAGGED(0);
    object->state     = TAGGED(0);
    object->eventMask = eventMask;
/*
    object->colormap_object  = 0;
    object->cursor_object    = 0;
    object->backgroundPixmap = 0;
    object->borderPixmap     = 0;
    object->parent           = 0;
*/    
    object->ds        = DEREFDISPLAYHANDLE(dsHandle);

    debugRefer(Window,w);

    return AddXObject(objectHandle); /* must remain MUTABLE */
  }
}


/******************************************************************************/
/*                                                                            */
/*      Pixmap                                                                */
/*                                                                            */
/******************************************************************************/

static X_Pixmap_Object *PixmapObject(X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Pixmap);
  
  return (X_Pixmap_Object *)P;
}

static Pixmap GetPixmap(TaskData *taskData, X_Object *P)
{
  if (UNTAGGED(P->type) == X_Window)
  {

    if (! ResourceExists(P)) 
      {
        debug1("Non-existent window %lx\n",(long)P);
      }

    if (*(((X_Window_Object*)P)->drawable) == None) return None;
  
    RaiseXWindows(taskData, "Not a pixmap");
  }

  assert(UNTAGGED(P->type) == X_Pixmap);

  /* val NoDrawable     = xcall (20,0) : Drawable; */
  /* val ParentRelative = xcall (20,1) : Drawable; */

  /* special case for NoDrawable */
  if (*((X_Pixmap_Object*)P)->pixmap == 0) return None;

  /* special case for ParentRelative */
  if (*((X_Pixmap_Object*)P)->pixmap == 1) return None;

  CheckExists(P,pixmap);

  return *(((X_Pixmap_Object*)P)->pixmap);
}

static Handle EmptyPixmap
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Pixmap id
)
{
  X_Pixmap_Object *E = FindPixmap(dsHandle,id);

  if (E)
  {
    return SAVE(E);
  }
  else
  {
    Handle objectHandle = alloc_and_save(taskData, SIZEOF(X_Pixmap_Object), F_MUTABLE_BIT);
    Handle pixmapHandle = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
  
    /* Must do all allocations before we do the first dereference */
    X_Pixmap_Object *object  = (X_Pixmap_Object *)DEREFHANDLE(objectHandle);
    Pixmap          *pixmap  = (Pixmap *)DEREFHANDLE(pixmapHandle);
    
    *pixmap = id; FINISHED(taskData, pixmapHandle);

    object->type   = TAGGED(X_Pixmap);
    object->pixmap = pixmap;
    object->ds     = DEREFDISPLAYHANDLE(dsHandle);

    debugCreate(Pixmap,id);

    return AddXObject(FINISHED(taskData, objectHandle));
  }
}


/******************************************************************************/
/*                                                                            */
/*      Drawable                                                              */
/*                                                                            */
/******************************************************************************/

static Drawable GetDrawable(TaskData *taskData, X_Object *P)
{
  CheckExists(P,drawable);

  switch(UNTAGGED(P->type))
  {
    case X_Window: return *(((X_Window_Object*)P)->drawable);
    case X_Pixmap: return *(((X_Pixmap_Object*)P)->pixmap);
    default: Crash ("Bad X_Object type (%d) in GetDrawable",UNTAGGED(P->type));
  }
  
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      DS / Display                                                          */
/*                                                                            */
/******************************************************************************/
static Handle GetDS(TaskData *taskData, X_Object *P)
{
  X_Display_Object *ds;
  
  CheckExists(P,resource);

  switch(UNTAGGED(P->type))
  {
    case X_GC:       ds = ((X_GC_Object*)P)->ds; break;
    case X_Font:     ds = ((X_Font_Object*)P)->ds; break;
    case X_Cursor:   ds = ((X_Cursor_Object*)P)->ds; break;
    case X_Window:   ds = ((X_Window_Object*)P)->ds; break;
    case X_Pixmap:   ds = ((X_Pixmap_Object*)P)->ds; break;
    case X_Colormap: ds = ((X_Colormap_Object*)P)->ds; break;
    case X_Visual:   ds = ((X_Visual_Object*)P)->ds; break;
    case X_Widget:   ds = ((X_Widget_Object*)P)->ds; break;
    case X_Display:  ds = (X_Display_Object*)P; break; /* i.e. P cast to the right type */
    
    default: Crash ("Bad X_Object type (%d) in GetDS",UNTAGGED(P->type));
  }

  assert((PolyWord)ds != TAGGED(0));

  return SAVE(ds);
}


static Display *GetDisplay(TaskData *taskData, X_Object *P)
{
  CheckExists(P,resource);

  switch(UNTAGGED(P->type))
  {
    case X_GC:       return ((X_GC_Object*)P)->ds->display;
    case X_Font:     return ((X_Font_Object*)P)->ds->display;
    case X_Cursor:   return ((X_Cursor_Object*)P)->ds->display;
    case X_Window:   return ((X_Window_Object*)P)->ds->display;
    case X_Pixmap:   return ((X_Pixmap_Object*)P)->ds->display;
    case X_Colormap: return ((X_Colormap_Object*)P)->ds->display;
    case X_Visual:   return ((X_Visual_Object*)P)->ds->display;
    case X_Widget:   return ((X_Widget_Object*)P)->ds->display;
    case X_Display:  return ((X_Display_Object*)P)->display;
    
    default:         Crash ("Bad X_Object type (%d) in GetDisplay",UNTAGGED(P->type));
  }

  /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      FS / Font                                                             */
/*                                                                            */
/******************************************************************************/
static Font GetFont(TaskData *taskData, X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Font);
  
  /* val NoFont = xcall (22,0) : Font; */
  /* special case for NoFont - valid(?) */
  if (*(((X_Font_Object *)P)->font) == None) return None;

  CheckExists(P,font);
  
  return *(((X_Font_Object *)P)->font);
}


static Handle EmptyFont
(
  TaskData     *taskData,
  Handle       dsHandle, /* Handle to (X_Display_Object *) */
  Font         id,
  XFontStruct *fs
)
{
  X_Font_Object *E = FindFont(dsHandle,id);

  if (E && (fs == NULL || *(E->fs) == fs))
  {
    return SAVE(E);
  }
  else
  {
  
    Handle objectHandle  = alloc_and_save(taskData, SIZEOF(X_Font_Object), F_MUTABLE_BIT);
    Handle fontHandle    = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
    Handle FSHandle      = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
  
    /* Must do all allocations before we do the first dereference */
    X_Font_Object *object = (X_Font_Object *)DEREFHANDLE(objectHandle);
    Font          *font   = (Font *)DEREFHANDLE(fontHandle);
    XFontStruct  **xfstr  = (XFontStruct **)DEREFHANDLE(FSHandle);

    *font = id; FINISHED(taskData, fontHandle);
    *xfstr = fs; FINISHED(taskData, FSHandle);

    object->type = TAGGED(X_Font);
    object->font = font;
    object->fs   = xfstr;
    object->ds   = DEREFDISPLAYHANDLE(dsHandle);

    debugCreate(Font,id);

    return AddXObject(FINISHED(taskData, objectHandle));
  }
}



/******************************************************************************/
/*                                                                            */
/*      Cursor                                                                */
/*                                                                            */
/******************************************************************************/
static X_Cursor_Object *CursorObject(X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Cursor);

  return (X_Cursor_Object *)P;
}

static Cursor GetCursor(TaskData *taskData, X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Cursor);
  
  /* val NoCursor = xcall (21,0) : Cursor; */
  /* special case for NoCursor */
  if (*(((X_Cursor_Object *)P)->cursor) == None) return None;

  CheckExists(P,cursor);

  return *(((X_Cursor_Object *)P)->cursor);
}


static Handle EmptyCursor
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Cursor id
)
{
  X_Cursor_Object *E = FindCursor(dsHandle,id);

  if (E)
  {
    return SAVE(E);
  }
  else
  {
  
    Handle objectHandle  = alloc_and_save(taskData, SIZEOF(X_Cursor_Object), F_MUTABLE_BIT);
    Handle cursorHandle  = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
  
    /* Must do all allocations before we do the first dereference */
    X_Cursor_Object *object = (X_Cursor_Object *)DEREFHANDLE(objectHandle);
    Cursor          *cursor = (Cursor *)DEREFHANDLE(cursorHandle);
    
    *cursor = id; FINISHED(taskData, cursorHandle);

    object->type   = TAGGED(X_Cursor);
    object->cursor = cursor;
    object->ds     = DEREFDISPLAYHANDLE(dsHandle);

    debugRefer(Cursor,id);

    return AddXObject(FINISHED(taskData, objectHandle));
  }
}

static Handle CreateFontCursor
(
  TaskData *taskData,
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  unsigned shape
)
{
  return EmptyCursor(taskData, dsHandle,XCreateFontCursor(DEREFDISPLAYHANDLE(dsHandle)->display,shape));
}

static Handle CreateGlyphCursor
(
  TaskData *taskData,
  Handle    dsHandle, /* Handle to (X_Display_Object *) */
  Font      sf,
  Font      mf,
  unsigned  sc,
  unsigned  mc,
  XColor   *foreground,
  XColor   *background
)
{
  return EmptyCursor(taskData, dsHandle,XCreateGlyphCursor(DEREFDISPLAYHANDLE(dsHandle)->display,sf,mf,sc,mc,foreground,background));
}

static Handle CreatePixmapCursor
(
  TaskData *taskData,
  Handle    dsHandle, /* Handle to (X_Display_Object *) */
  Pixmap    source,
  Pixmap    mask,
  XColor   *foreground,
  XColor   *background,
  unsigned  x,
  unsigned  y
)
{
  return EmptyCursor(taskData, dsHandle,XCreatePixmapCursor(DEREFDISPLAYHANDLE(dsHandle)->display,source,mask,foreground,background,x,y));
}

/******************************************************************************/
/*                                                                            */
/*      Widget                                                                */
/*                                                                            */
/******************************************************************************/
static Widget GetNWidget(TaskData *taskData, X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Widget);
  
  if (*(((X_Widget_Object *)P)->widget) == NULL) return NULL;
  
  CheckExists(P,widget);

  return *(((X_Widget_Object *)P)->widget);
}

static Widget GetWidget(TaskData *taskData, X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Widget);
  
  if (*(((X_Widget_Object *)P)->widget) == NULL)
    {
      RaiseXWindows(taskData, "Not a real widget");
    }

  CheckExists(P,widget);

  return *(((X_Widget_Object *)P)->widget);
}

/* added 6/11/94 SPF */
static Widget GetRealizedWidget(TaskData *taskData, char *where, X_Object *P)
{
  Widget w;
  
  assert(UNTAGGED(P->type) == X_Widget);

  w = *(((X_Widget_Object *)P)->widget);

  if (w == NULL)
  {
     RaiseXWindows2(where,": not a real widget");
  }

  CheckExists(P,widget);

  if (XtIsRealized(w) == False)
  {
     RaiseXWindows2(where,": widget is not realized");
  }

  return w;
}

/* P is a pointer to an X_Widget_Object */
static X_Widget_Object *WidgetObjectToken(X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Widget);
  return (X_Widget_Object *)P;
}

/* P is a pointer to an X_Widget_Object, which is bound to a C widget */
static X_Widget_Object *WidgetObject(TaskData *taskData, X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Widget);
  
  CheckExists(P,widget);

  return (X_Widget_Object *)P;
}


static Handle EmptyWidget
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Widget id
)
{
  X_Widget_Object *E = FindWidget(id);

  if (E)
  {
    return SAVE(E);
  }
  else
  {
  
    Handle objectHandle = alloc_and_save(taskData, SIZEOF(X_Widget_Object), F_MUTABLE_BIT);
    Handle widgetHandle = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
  
    /* Must do all allocations before we do the first dereference */
    X_Widget_Object *object = (X_Widget_Object *)DEREFHANDLE(objectHandle);
    Widget          *widget = (Widget *)DEREFHANDLE(widgetHandle);

    *widget = id; FINISHED(taskData, widgetHandle);

    object->type         = TAGGED(X_Widget);
    object->widget       = widget;
    object->callbackList = ListNull;
    object->state        = TAGGED(0);
    object->ds           = DEREFDISPLAYHANDLE(dsHandle);

    debugRefer(Widget,id);
    
    return AddXObject(objectHandle); /* Must stay MUTABLE */
  }
}

static Handle NewWidget
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Widget id
)
{
  X_Widget_Object *E = FindWidget(id);

  if (E) DestroyXObject((X_Object *)E);
  
  return EmptyWidget(taskData, dsHandle,id);
}


/******************************************************************************/
/*                                                                            */
/*      Text Widgets                                                          */
/*                                                                            */
/******************************************************************************/
static Widget GetTextWidget(TaskData *taskData, char *funcname, X_Object *P)
{
  Widget w = GetWidget(taskData, P);
  
  if (XmIsText(w)) return w;
  
  /* Text operations are also legal on TextField widgets */
  if (XmIsTextField(w)) return w;
  
  RaiseXWindows2(funcname,": not a Text or TextField widget");
  /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      TextField Widgets                                                     */
/*                                                                            */
/******************************************************************************/
static Widget GetTextFieldWidget(TaskData *taskData, char *funcname, X_Object *P)
{
  Widget w = GetWidget(taskData, P);
  
  if (XmIsTextField(w)) return w;
  
  RaiseXWindows2(funcname,": not a TextField widget");
  /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      List Widgets                                                          */
/*                                                                            */
/******************************************************************************/
static Widget GetListWidget(TaskData *taskData, char *funcname, X_Object *P)
{
  Widget w = GetWidget(taskData, P);
  
  if (XmIsList(w)) return w;
  
  RaiseXWindows2(funcname,": not a List widget");
  /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      Window                                                                */
/*                                                                            */
/******************************************************************************/
static void RemoveWindowEvents(Display *d, Window w)
{
  XEvent event;

  XSync(d,False);

  while(XCheckWindowEvent(d,w,~0,&event))
  {
    /* do nothing */
  }
}


static Handle AddWindow
(
  TaskData *taskData,
  Window W,
  Handle handlerHandle, /* Handle to (PolyWord *) (?)  */
  Handle stateHandle,   /* Handle to (PolyWord *) (?)  */      
  Handle parentHandle   /* Handle to (X_Window_Object *) */
)
{
  XWMHints hints;
  Atom deleteWindow; /* was int SPF 6/1/94 */
  Display *d = GetDisplay(taskData, DEREFXOBJECTHANDLE(parentHandle));

  Handle objectHandle    = alloc_and_save(taskData, SIZEOF(X_Window_Object), F_MUTABLE_BIT);
  Handle eventMaskHandle = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);
  Handle drawableHandle  = alloc_and_save(taskData, 1, F_MUTABLE_BIT|F_BYTE_OBJ);

  /* Must do all allocations before we do the first dereference */
  X_Window_Object *object    = DEREFWINDOWHANDLE(objectHandle);
  Drawable        *drawable  = (Drawable *)DEREFHANDLE(drawableHandle);
  PolyObject      *eventMask = DEREFHANDLE(eventMaskHandle);

  eventMask->Set(0, PolyWord::FromUnsigned(0)); /* eventMask must remain MUTABLE */
  *drawable  = W; FINISHED(taskData, drawableHandle);

  hints.flags = InputHint;
  hints.input = True;

  XSetWMHints(d,W,&hints);
  
  deleteWindow = WM_DELETE_WINDOW(d);
  
  if (deleteWindow != None) XSetWMProtocols(d,W,&deleteWindow,1);

  debugCreate(Window,W);

  object->type      = TAGGED(X_Window);
  object->drawable  = drawable;
  object->eventMask = eventMask;
  object->handler   = DEREFHANDLE(handlerHandle);
  object->state     = DEREFHANDLE(stateHandle);
  object->parent    = DEREFWINDOWHANDLE(parentHandle);
  object->ds        = DEREFWINDOWHANDLE(parentHandle)->ds; /* Tidy up (?) */
/*
  object->colormap_object  = 0;
  object->cursor_object    = 0;
  object->backgroundPixmap = 0;
  object->borderPixmap     = 0;
*/

  if (ISNIL(DEREFHANDLE(handlerHandle))) Crash ("No handler set");

  return AddXObject(objectHandle); /* object must remain MUTABLE */
}

static void DestroyWindow(X_Object *W /* Should be a Window Object! */)
{
    TaskData *taskData = processes->GetTaskDataForThread();
  Window   w = GetWindow(taskData, W);
  Display *d = GetDisplay(taskData, W);
  
  debugReclaim(Window,w);

  XUnmapWindow(d,w);

  DestroySubwindows(W);
  
  XDestroyWindow(d,w);

  RemoveWindowEvents(d,w);
}

static Handle CreateSimpleWindow
(
  TaskData *taskData,
  Handle   parent, /* Handle to (X_Window_Object *) */
  int      x,
  int      y,
  unsigned w,
  unsigned h,
  unsigned borderWidth,
  unsigned border,
  unsigned background,
  Handle   handler, /* Handle to (PolyWord *) (?) */
  Handle   state    /* Handle to (PolyWord *) (?) */
)
{
  Window W = XCreateSimpleWindow(GetDisplay(taskData, DEREFXOBJECTHANDLE(parent)),
                                   GetWindow(taskData, DEREFXOBJECTHANDLE(parent)),
                                   x,y,w,h,
                                   borderWidth,border,background);

  if (W == 0) RaiseXWindows(taskData, "XCreateSimpleWindow failed");

  return AddWindow(taskData,W,handler,state,parent);
}

static Handle CreateWindow
(
  TaskData *taskData,
  Handle   parent, /* Handle to (X_Window_Object *) */
  int      x,
  int      y,
  unsigned w,
  unsigned h,
  unsigned borderWidth,
  unsigned depth,
  unsigned clas,
  Visual   *visual,
  Handle   handler, /* Handle to (PolyWord *) (?) */
  Handle   state    /* Handle to (PolyWord *) (?) */
)
{
  Window W;

  W = XCreateWindow(GetDisplay(taskData, DEREFXOBJECTHANDLE(parent)),
                      GetWindow(taskData, DEREFXOBJECTHANDLE(parent)),
                      x,y,w,h,
                      borderWidth,depth,clas,visual,0,0);

  if (W == 0) RaiseXWindows(taskData, "XCreateWindow failed");

  return AddWindow(taskData,W,handler,state,parent);
}

static void DestroySubwindows(X_Object *W /* should be a Window object! */)
{
    TaskData *taskData = processes->GetTaskDataForThread();
  Window   root,parent,*children;
  unsigned n;
  int      s;

  Window   w = GetWindow(taskData, W);
  Display *d = GetDisplay(taskData, W);
  
  s = XQueryTree(d,w,&root,&parent,&children,&n);

  if (s == 0)
  {
    RaiseXWindows(taskData, "XDestroySubwindows failed");
    return;
  }
  
  XUnmapSubwindows(d,w);

  if (n)
  {
    Handle dsHandle = GetDS(taskData, W);

    while(n--)
    {
      X_Window_Object *child = FindWindow(dsHandle,children[n]);

      if (child) DestroyXObject((X_Object *)child);
    }

    XFree((char *)children);
  }

  XDestroySubwindows(d,w);
}


/******************************************************************************/
/*                                                                            */
/*      Translations / Accelerators                                           */
/*                                                                            */
/******************************************************************************/
static Handle EmptyTrans(TaskData *taskData, XtTranslations table)
{
  Handle objectHandle = alloc_and_save(taskData, SIZEOF(X_Trans_Object), F_MUTABLE_BIT|F_BYTE_OBJ);

  /* Must do all allocations before we do the first dereference */
  X_Trans_Object *object = (X_Trans_Object *)DEREFHANDLE(objectHandle);

  /* OK to store C values because this is a byte object */
  object->type  = TAGGED(X_Trans);
  object->table = table;
  
  debugRefer(Trans,table);

  return AddXObject(FINISHED(taskData, objectHandle));
}

static XtTranslations GetTrans(TaskData *taskData, X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Trans);
  
  CheckExists(P,trans);
  
  return ((X_Trans_Object *)P)->table;
}

static Handle EmptyAcc(TaskData *taskData, XtTranslations acc)
{

  Handle objectHandle = alloc_and_save(taskData, SIZEOF(X_Acc_Object), F_MUTABLE_BIT|F_BYTE_OBJ);

  /* Must do all allocations before we do the first dereference */
  X_Acc_Object *object = (X_Acc_Object *)DEREFHANDLE(objectHandle);

  /* OK to store C values because this is a byte object */
  object->type = TAGGED(X_Acc);
  object->acc  = acc;

  debugRefer(Acc,acc);

  return AddXObject(FINISHED(taskData, objectHandle));
}

static XtAccelerators GetAcc(TaskData *taskData, X_Object *P)
{
  assert(UNTAGGED(P->type) == X_Acc);
  
  CheckExists(P,acc);
  
  return ((X_Acc_Object *)P)->acc;
}

/******************************************************************************/
/*                                                                            */
/*      Utility functions                                                     */
/*                                                                            */
/******************************************************************************/

static XtGrabKind GetXtGrabKind(TaskData *taskData, PolyWord P)
{
  int i = get_C_long(taskData, P);
  
  /* This encoding must be the same as that used in Motif/ml_bind.ML */
  switch (i)
  {
    case 0:  return XtGrabNone;
    case 1:  return XtGrabNonexclusive;
    case 2:  return XtGrabExclusive;
    
    default: Crash ("Bad XtGrabKind index (%d) in GetXtGrabKind",i);
  }

  return XtGrabNone; /* to keep lint/gcc happy */
}

/******************************************************************************/
/*                                                                            */
/*      MLXStandardColormap - implements ML XStandardColormap datatype        */
/*                                                                            */
/******************************************************************************/

typedef struct
{
    X_Colormap_Object *Colormap;
    PolyWord          redMax;    /* ML int */
    PolyWord          redMult;   /* ML int */
    PolyWord          greenMax;  /* ML int */
    PolyWord          greenMult; /* ML int */
    PolyWord          blueMax;   /* ML int */
    PolyWord          blueMult;  /* ML int */
    PolyWord          basePixel; /* ML int */
    X_Visual_Object   *visual;
}  MLXStandardColormap;


static void GetStandardColormap(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    MLXStandardColormap *P = (MLXStandardColormap *)p.AsObjPtr();
    XStandardColormap *s = (XStandardColormap *)v;
    s->colormap   = GetColormap(taskData, (X_Object *)P->Colormap);

    s->red_max    = get_C_ulong(taskData, P->redMax);
    s->red_mult   = get_C_ulong(taskData, P->redMult);
    s->green_max  = get_C_ulong(taskData, P->greenMax);
    s->green_mult = get_C_ulong(taskData, P->greenMult);
    s->blue_max   = get_C_ulong(taskData, P->blueMax);
    s->blue_mult  = get_C_ulong(taskData, P->blueMult);
    s->base_pixel = get_C_ulong(taskData, P->basePixel);

    s->visualid   = GetVisual(taskData, (X_Object *)P->visual)->visualid; /* UNSAFE(?) */
    s->killid     = None;
}

static Handle CreateStandardColormap
(
  TaskData *taskData,
  void *v,
  Handle dsHandle /* Handle to (X_Display_Object *) */
)
{
    XStandardColormap *s = (XStandardColormap *)v;
  XVisualInfo T;
  XVisualInfo *info;
  int count;
  
  Handle tupleHandle = alloc_and_save(taskData, SIZEOF(MLXStandardColormap), F_MUTABLE_BIT);
  
  T.visualid = s->visualid;
  T.visual   = None;
  
  info = XGetVisualInfo(DEREFDISPLAYHANDLE(dsHandle)->display,VisualIDMask,&T,&count);
  
  if (info)
  {
    T.visual = info->visual;
    
    XFree((char *)info);
  }

/* Still allocating, so must use explicit DEREF for each element */
#define tuple /* hack */((MLXStandardColormap *)DEREFHANDLE(tupleHandle))
  tuple->Colormap  = (X_Colormap_Object *)DEREFHANDLE(EmptyColormap(taskData, dsHandle,s->colormap));
  tuple->redMax    = DEREFWORD(Make_arbitrary_precision(taskData, s->red_max));
  tuple->redMult   = DEREFWORD(Make_arbitrary_precision(taskData, s->red_mult));
  tuple->greenMax  = DEREFWORD(Make_arbitrary_precision(taskData, s->green_max));
  tuple->greenMult = DEREFWORD(Make_arbitrary_precision(taskData, s->green_mult));
  tuple->blueMax   = DEREFWORD(Make_arbitrary_precision(taskData, s->blue_max));
  tuple->blueMult  = DEREFWORD(Make_arbitrary_precision(taskData, s->blue_mult));
  tuple->basePixel = DEREFWORD(Make_arbitrary_precision(taskData, s->base_pixel));
  tuple->visual    = (X_Visual_Object *)DEREFHANDLE(EmptyVisual(taskData, dsHandle,T.visual));
#undef tuple
  
  return FINISHED(taskData, tupleHandle);
}


/******************************************************************************/
/*                                                                            */
/*      Polymorphic pairs                                                     */
/*                                                                            */
/******************************************************************************/

class MLPair: public PolyObject
{
public:
    PolyWord x0; /* first value  */
    PolyWord x1; /* second value */
};


/* Polymorphic pair creation */
static Handle CreatePair(TaskData *taskData, Handle p1, Handle p2)
{
  Handle pairHandle = alloc_and_save(taskData, SIZEOF(MLPair), F_MUTABLE_BIT);

/* Still allocating, so must use explicit DEREF for each element */
#define pair ((MLPair *)DEREFHANDLE(pairHandle))
  pair->x0 = DEREFWORD(p1);
  pair->x1 = DEREFWORD(p2);
#undef pair

  return FINISHED(taskData, pairHandle);
}


/******************************************************************************/
/*                                                                            */
/*      Polymorphic triples                                                   */
/*                                                                            */
/******************************************************************************/

class MLTriple: public PolyObject
{
public:
    PolyWord x0; /* first value  */
    PolyWord x1; /* second value */
    PolyWord x2; /* third value  */
};

inline PolyWord FST(PolyWord P)   { return ((MLTriple*)P.AsObjPtr())->x0; }
inline PolyWord SND(PolyWord P)   { return ((MLTriple*)P.AsObjPtr())->x1; }
inline PolyWord THIRD(PolyWord P) { return ((MLTriple*)P.AsObjPtr())->x2; }

static Handle CreateTriple(TaskData *taskData, Handle p1, Handle p2, Handle p3)
{
  Handle tripleHandle = alloc_and_save(taskData, SIZEOF(MLTriple), F_MUTABLE_BIT);

/* Still allocating, so must use explicit DEREF for each element */
#define triple ((MLTriple *)DEREFHANDLE(tripleHandle))
  triple->x0 = DEREFWORD(p1);
  triple->x1 = DEREFWORD(p2);
  triple->x2 = DEREFWORD(p3);
#undef triple

  return FINISHED(taskData, tripleHandle);
}


/******************************************************************************/
/*                                                                            */
/*      MLXImage - Implements ML XImage datatype                              */
/*                                                                            */
/******************************************************************************/
typedef struct
{
  PolyWord data;            /* ML (abstype containing) string */
  PolyWord size;            /* MLXRectangle * */
  PolyWord depth;           /* ML int */
  PolyWord format;          /* (short ML int) XYBitmap | XYPixmap | ZPixmap */
  PolyWord xoffset;         /* ML int */
  PolyWord bitmapPad;       /* ML int */
  PolyWord byteOrder;       /* (short ML int) LSBFirst | MSBFirst */
  PolyWord bitmapUnit;      /* ML int */
  PolyWord bitsPerPixel;    /* ML int */
  PolyWord bytesPerLine;    /* ML int */
  PolyWord visualRedMask;   /* ML int */
  PolyWord bitmapBitOrder;  /* (short ML int) LSBFirst | MSBFirst */
  PolyWord visualBlueMask;  /* ML int */
  PolyWord visualGreenMask; /* ML int */
} MLXImage;

#define MLImageFormat(n) (n+1)
#define MLImageOrder(n)  (n+1)
#define CImageFormat(n)  (n-1)
#define CImageOrder(n)   (n-1)

static unsigned ImageBytes(XImage *image)
{
    unsigned dsize = image->bytes_per_line * image->height;
    if (image->format == XYPixmap) dsize = dsize * image->depth;
    return dsize;
}

static XImage *GetXImage(TaskData *taskData, Display *d, PolyWord p)
/* can only be called once per X opcode */
{
    MLXImage *I = (MLXImage *)p.AsObjPtr();
  static XImage image = { 0 };
  
  PolyStringObject  *data           = GetString(I->data);
  unsigned width          = GetRectW(taskData, I->size);
  unsigned height         = GetRectH(taskData, I->size);
  unsigned depth          = get_C_ulong(taskData, I->depth);
  unsigned format         = get_C_ulong(taskData, I->format);
  int      xoffset        = get_C_short(taskData, I->xoffset);
  int      bitmapPad      = get_C_short(taskData, I->bitmapPad);
  int      bytesPerLine   = get_C_long (taskData, I->bytesPerLine);

  unsigned byteOrder      = get_C_ulong(taskData, I->byteOrder);
  unsigned bitmapUnit     = get_C_ulong(taskData, I->bitmapUnit);
  unsigned bitsPerPixel   = get_C_ulong(taskData, I->bitsPerPixel);
  unsigned bitmapBitOrder = get_C_ulong(taskData, I->bitmapBitOrder);

  format         = CImageFormat(format);
  byteOrder      = CImageOrder(byteOrder);
  bitmapBitOrder = CImageOrder(bitmapBitOrder);
  
  image.width            = width;
  image.height           = height;
  image.xoffset          = xoffset;
  image.format           = format;
  image.data             = data->chars;
  image.byte_order       = byteOrder;
  image.bitmap_unit      = bitmapUnit;
  image.bitmap_bit_order = bitmapBitOrder;
  image.bitmap_pad       = bitmapPad;
  image.depth            = depth;
  image.bytes_per_line   = bytesPerLine;
  image.bits_per_pixel   = bitsPerPixel;
  image.red_mask         = get_C_ulong(taskData, I->visualRedMask);
  image.green_mask       = get_C_ulong(taskData, I->visualGreenMask);
  image.blue_mask        = get_C_ulong(taskData, I->visualBlueMask);
  
  if (ImageBytes(&image) != data->length) RaiseXWindows(taskData, "Bad image string length");

  XInitImage(&image);
  
  return &image;
}

static Handle CreateImage(TaskData *taskData, XImage *image)
{
  Handle XHandle = alloc_and_save(taskData, SIZEOF(MLXImage), F_MUTABLE_BIT);
  
  int dsize = ImageBytes(image);

/* Still allocating, so must use explicit DEREF for each element */
#define  X ((MLXImage *)DEREFHANDLE(XHandle))
  X->data            = C_string_to_Poly(taskData, image->data,dsize);
  X->size            = DEREFWORD(CreateArea(image->width,image->height));
  X->depth           = DEREFWORD(Make_arbitrary_precision(taskData, image->depth));
  X->format          = DEREFWORD(Make_arbitrary_precision(taskData, MLImageFormat(image->format)));
  X->xoffset         = DEREFWORD(Make_int(image->xoffset));
  X->bitmapPad       = DEREFWORD(Make_int(image->bitmap_pad));
  X->byteOrder       = DEREFWORD(Make_arbitrary_precision(taskData, MLImageOrder(image->byte_order)));
  X->bitmapUnit      = DEREFWORD(Make_arbitrary_precision(taskData, image->bitmap_unit));
  X->bitsPerPixel    = DEREFWORD(Make_arbitrary_precision(taskData, image->bits_per_pixel));
  X->bytesPerLine    = DEREFWORD(Make_int(image->bytes_per_line));
  X->visualRedMask   = DEREFWORD(Make_arbitrary_precision(taskData, image->red_mask));
  X->bitmapBitOrder  = DEREFWORD(Make_arbitrary_precision(taskData, MLImageOrder(image->bitmap_bit_order)));
  X->visualBlueMask  = DEREFWORD(Make_arbitrary_precision(taskData, image->blue_mask));
  X->visualGreenMask = DEREFWORD(Make_arbitrary_precision(taskData, image->green_mask));
#undef X

  XDestroyImage(image);
  
  return FINISHED(taskData, XHandle);
}

static Handle GetImage
(
  TaskData *taskData,
  Display *d,
  Drawable drawable,
  int      x,
  int      y,
  unsigned w,
  unsigned h,
  unsigned /* long */ mask,
  int format
)
{
  XImage *image = XGetImage(d,drawable,x,y,w,h,mask,CImageFormat(format));
  
  if (image == 0) RaiseXWindows(taskData, "XGetImage failed");
  
  return CreateImage(taskData, image);
}

static Handle SubImage
(
  TaskData *taskData,
  XImage   *image,
  int      x,
  int      y,
  unsigned w,
  unsigned h
)
{
  XImage *subimage = XSubImage(image,x,y,w,h);
  
  if (subimage == 0) RaiseXWindows(taskData, "XSubImage failed");
  
  return CreateImage(taskData, subimage);
}


/******************************************************************************/
/*                                                                            */
/*      XImage                                                                */
/*                                                                            */
/******************************************************************************/
static void GetSubImage
(
  Display  *d,
  Drawable  drawable,
  int       sx,
  int       sy,
  unsigned  sw,
  unsigned  sh,
  unsigned /* long */ mask,
  int       format,
  XImage   *image,
  int       dx,
  int       dy
)
{
  XGetSubImage(d,drawable,sx,sy,sw,sh,mask,CImageFormat(format),image,dx,dy);
  
  /* XFree((char *)image); */
}

static void PutImage
(
  Display  *d,
  Drawable drawable,
  GC       gc,
  XImage  *image,
  int      sx,
  int      sy,
  int      dx,
  int      dy,
  unsigned dw,
  unsigned dh
)
{
  XPutImage(d,drawable,gc,image,sx,sy,dx,dy,dw,dh);
  
  /* XFree((char *)image); */
}

static Handle GetPixel(TaskData *taskData, XImage *image, int x, int y)
{
  unsigned pixel = XGetPixel(image,x,y);
  
  /* XFree((char *)image); */
  
  return Make_arbitrary_precision(taskData, pixel);
}

static void PutPixel(XImage *image, int x, int y, unsigned pixel)
{
  XPutPixel(image,x,y,pixel);
  
  /* XFree((char *)image); */
}

static void AddPixel(XImage *image, unsigned value)
{
  XAddPixel(image,value);
  
  /* XFree((char *)image); */
}


/******************************************************************************/
/*                                                                            */
/*      TimeVal                                                               */
/*                                                                            */
/******************************************************************************/
static int DoubleClickTime = 250; /* Double click time in milliseconds       */
static int MouseDrift      = 5;   /* Mouse movement allowed in button events */

static void NormaliseTime(TimeVal *t)
{
  while(t->tv_usec >= 1000000) { t->tv_usec -= 1000000; t->tv_sec++; }
  while(t->tv_usec < 0)        { t->tv_usec += 1000000; t->tv_sec--; }
}

static void TimeAdd(TimeVal *a, TimeVal *b, TimeVal *t)
{
  t->tv_sec  = a->tv_sec  + b->tv_sec;
  t->tv_usec = a->tv_usec + b->tv_usec;

  NormaliseTime(t);
}

static int TimeLt(TimeVal *a, TimeVal *b)
{
  return ((a->tv_sec <  b->tv_sec) ||
         ((a->tv_sec == b->tv_sec) && (a->tv_usec <  b->tv_usec)));
}

static int TimeLeq(TimeVal *a, TimeVal *b)
{
  return ((a->tv_sec <  b->tv_sec) ||
         ((a->tv_sec == b->tv_sec) && (a->tv_usec <=  b->tv_usec)));
}

/******************************************************************************/
/*                                                                            */
/*      (?)                                                                   */
/*                                                                            */
/******************************************************************************/
typedef struct
{
  XButtonEvent *button;    /* initial button press event   */
  int           up,down;   /* count of button transitions  */
} PredicateArgs;

static Bool SameClickEvent(Display *dpy, XEvent *ev, XPointer arg)
{
  PredicateArgs *A = (PredicateArgs *)arg;
  
  switch(ev->type)
  {
    case MotionNotify:
    {
      int dx = ev->xmotion.x - A->button->x;
      int dy = ev->xmotion.y - A->button->y;

      if (ev->xmotion.window != A->button->window) return False;

      if (abs(dx) > MouseDrift) return False;
      if (abs(dy) > MouseDrift) return False;

      return True;
    }

    case ButtonPress:
    case ButtonRelease:
    {
      int dx = ev->xbutton.x - A->button->x;
      int dy = ev->xbutton.y - A->button->y;

      if (ev->xbutton.window != A->button->window) return False;

      if (ev->xbutton.button != A->button->button) return False;

      if (abs(dx) > MouseDrift) return False;
      if (abs(dy) > MouseDrift) return False;

      if (ev->type == ButtonPress) A->down++; else A->up++;

      return True;
    }
  }

  return False;
}

static void WaitDoubleClickTime(Handle dsHandle, PredicateArgs *A)
{
  XEvent N;
  TimeVal start_time,end_time,dt;
  Display *d = DEREFDISPLAYHANDLE(dsHandle)->display;

  /* 
    AIX doesn't document support for NULL pointers in the select call,
     so we have to initialise empty fd_sets instead. SPF 30/10/95
  */
  fd_set read_fds, write_fds, except_fds;
  FD_ZERO(&read_fds);
  FD_ZERO(&write_fds);
  FD_ZERO(&except_fds);

  {
    int fd = d->fd;
    assert (0 <= fd && fd < FD_SETSIZE); 
    FD_SET(fd,&read_fds);
  }

  gettimeofday(&start_time, NULL);

  dt.tv_sec  = 0;
  dt.tv_usec = DoubleClickTime * 1000;

  TimeAdd(&start_time,&dt,&end_time);

  for (;;)
  {
    int extended = 0;

    while(XCheckIfEvent(d,&N,SameClickEvent,(char *) A))
    {
      if (DEREFDISPLAYHANDLE(dsHandle)->app_context) XtDispatchEvent(&N);
      
      extended = 1;
    }

    if (QLength(d)) break;  /* some other event to be processed next */

    if (extended)           /* button event extended, so extend time period */
    {
      dt.tv_sec  = 0;
      dt.tv_usec = DoubleClickTime * 1000;

      TimeAdd(&end_time,&dt,&end_time);
    }

    if (TimeLeq(&end_time,&start_time)) break; /* the time period has elapsed */

    select(FD_SETSIZE,&read_fds,&write_fds,&except_fds,&dt);

    gettimeofday(&start_time, NULL);
  }
}

static Handle GetKeyVector(TaskData *taskData, void *k, unsigned i)
{
    uchar *keys = (uchar*)k;
    unsigned index = i / 8;
    unsigned mask  = 1 << (i % 8);
    return Make_bool(keys[index] & mask);
}

static Handle QueryKeymap(TaskData *taskData, Display *d)
{
    char keys[32];
    XQueryKeymap(d, keys);
    return CreateList4I(taskData, 256,keys,0,GetKeyVector);
}

/******************************************************************************/
/*                                                                            */
/*      EventName                                                             */
/*                                                                            */
/******************************************************************************/
typedef struct
{
  const char *name;
  int   type;
} EventName;

static EventName EventNames[] =
{
  { "KeyPress",KeyPress },
  { "KeyRelease",KeyRelease },
  { "ButtonPress",ButtonPress },
  { "ButtonRelease",ButtonRelease },
  { "MotionNotify",MotionNotify },
  { "EnterNotify",EnterNotify },
  { "LeaveNotify",LeaveNotify },
  { "FocusIn",FocusIn },
  { "FocusOut",FocusOut },
  { "KeymapNotify",KeymapNotify },
  { "Expose",Expose },
  { "GraphicsExpose",GraphicsExpose },
  { "NoExpose",NoExpose },
  { "VisibilityNotify",VisibilityNotify },
  { "CreateNotify",CreateNotify },
  { "DestroyNotify",DestroyNotify },
  { "UnmapNotify",UnmapNotify },
  { "MapNotify",MapNotify },
  { "MapRequest",MapRequest },
  { "ReparentNotify",ReparentNotify },
  { "ConfigureNotify",ConfigureNotify },
  { "ConfigureRequest",ConfigureRequest },
  { "GravityNotify",GravityNotify },
  { "ResizeRequest",ResizeRequest },
  { "CirculateNotify",CirculateNotify },
  { "CirculateRequest",CirculateRequest },
  { "PropertyNotify",PropertyNotify },
  { "SelectionClear",SelectionClear },
  { "SelectionRequest",SelectionRequest },
  { "SelectionNotify",SelectionNotify },
  { "ColormapNotify",ColormapNotify },
  { "ClientMessage",ClientMessage },
  { "MappingNotify",MappingNotify },
};

#define NEVENTS (sizeof(EventNames)/sizeof(EventName))

static const char *DebugEventName(int type)
{
    for(unsigned i = 0; i < NEVENTS; i++)
    {
        if (EventNames[i].type == type) return EventNames[i].name;
    }
    
    return "** BAD EVENT **";
}

static int WM_PROTOCOLS(Display *d)
{
  static int protocols = None;
  
  if (protocols == None) protocols = XInternAtom(d,"WM_PROTOCOLS",True);
  
  return protocols;
}

static Atom WM_DELETE_WINDOW(Display *d)
{
  static Atom deleteWindow = None;
  
  if (deleteWindow == None) deleteWindow = XInternAtom(d,"WM_DELETE_WINDOW",True);
  
  return deleteWindow;
}

/******************************************************************************/
/*                                                                            */
/*      Structures used by CreateEvent function.                              */
/*                                                                            */
/* These typedefs should correspond with the tuples used by MakeXKeyEvent etc */
/*                                                                            */
/******************************************************************************/


typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
PolyWord        time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
PolyWord        modifiers;  /* ML modifier (int) */
PolyWord        keycode;    /* ML int */
} ML_KeyEvent_Data;

typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
PolyWord        time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
PolyWord        modifiers;  /* ML modifier (int) */
PolyWord        button;     /* ML int */
} ML_ButtonEvent_Data;

typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
PolyWord        time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
PolyWord        modifiers;  /* ML modifier (int) */
PolyWord        button;     /* ML int */
PolyWord        up;         /* ML int */
PolyWord        down;       /* ML int */
} ML_ButtonClick_Data;

typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
PolyWord        time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
PolyWord        modifiers;  /* ML modifier (int) */
PolyWord        isHint;     /* ML bool */
} ML_MotionEvent_Data;


typedef struct
{
X_Window_Object *root;
X_Window_Object *subwindow;
PolyWord        time;       /* ML int */
MLXPoint        *pointer;
MLXPoint        *rootPointer;
PolyWord        mode;        /* ?  */
PolyWord        detail;      /* ? */
PolyWord        focus;       /* ? */
PolyWord        modifiers;   /* ML modifier (int) */
} ML_CrossingEvent_Data;


typedef struct
{
    MLXRectangle *region;
    PolyWord     count;  /* ML int */
} ML_ExposeEvent_Data;

typedef struct
{
    X_Window_Object *window;
    MLXPoint        *position;
    MLXRectangle    *size;
    PolyWord       borderWidth;      /* ML int */
    X_Window_Object *above;
    PolyWord        overrideRedirect; /* ML bool */
} ML_ConfigureNotify_Data;

typedef struct
{
    X_Window_Object *window;
    MLXPoint        *position;
    MLXRectangle    *size;
    PolyWord        borderWidth;
    X_Window_Object *above;
    PolyWord        detail;      /* ? */
} ML_ConfigureRequest_Data;


typedef struct
{
    MLXRectangle *region;
    PolyWord     count;  /* ML int */
    PolyWord     code;   /* ML int */
} ML_GraphicsExposeEvent_Data;

typedef struct
{
    PolyWord mode;   /* ML int ? */
    PolyWord detail; /* ML int ? */
    } ML_FocusChangeEvent_Data;

typedef struct
{
    X_Window_Object *window;
    MLXPoint        *position;
    MLXRectangle    *size;
    PolyWord       borderWidth;      /* ML int */
    PolyWord       overrideRedirect; /* ML bool */
} ML_CreateEvent_Data;

typedef struct
{
    X_Window_Object *window;
    PolyWord        fromConfigure; /* ML bool */
} ML_UnmapEvent_Data;

typedef struct
{
    X_Window_Object *window;
    PolyWord        overrideRedirect; /* ML bool */
} ML_MapEvent_Data;

typedef struct
{
X_Window_Object *window;
X_Window_Object *parent;
MLXPoint        *position;
PolyWord        overrideRedirect; /* ML bool */
} ML_ReparentEvent_Data;

typedef struct
{
X_Window_Object *window;
MLXPoint        *position;
} ML_GravityEvent_Data;

typedef struct
{
X_Window_Object *window;
PolyWord        place;
} ML_CirculateEvent_Data;

typedef struct
{
X_Colormap_Object *colormap_object;
PolyWord          c_new;        /* ML bool */
PolyWord          installed;  /* ML bool */
} ML_ColormapEvent_Data;

typedef struct
{
PolyWord selection; /* ML int */
PolyWord time;      /* ML int */
} ML_SelectionClear_Data;

typedef struct
{
    X_Window_Object *requestor;
    PolyWord        selection; /* ML int */
    PolyWord        target;    /* ML int */
    PolyWord        property;  /* ML int */
    PolyWord        time;      /* ML int */
} ML_SelectionRequest_Data;


typedef struct
{
    PolyWord selection; /* ML int */
    PolyWord target;    /* ML int */
    PolyWord property;  /* ML int */
    PolyWord time;      /* ML int */
} ML_Selection_Data;


class ML_Event: public PolyObject
{
public:
    PolyWord        type;       /* ML (?) */
    PolyWord        sendEvent;  /* ML bool */
    PolyWord        window;     /* X_Window_Object* */
    PolyWord        data;       /* pointer to event-specific data, in ML_XXX_Data format */
    PolyWord        callbacks;  /* ML list of something */
    PolyWord        events;     /* ML list */
};


/******************************************************************************/
/*                                                                            */
/*      CreateEvent function                                                  */
/*                                                                            */
/******************************************************************************/

static Handle CreateEvent
(
  TaskData *taskData,
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  XEvent *ev,
  Handle  W         /* Handle to (X_Window_Object *) */
)
{
  Handle eventHandle = alloc_and_save(taskData, SIZEOF(ML_Event), F_MUTABLE_BIT);

  Display *d     = DEREFDISPLAYHANDLE(dsHandle)->display;
  int type       = ev->xany.type;
  int send_event = ev->xany.send_event;
  
  assert(d == ev->xany.display);
  
  if (debugOptions & DEBUG_X)
  {
    printf("CreateEvent called, type=%s,", DebugEventName(type));
    printf(" window=%lx\n", ev->xany.window);
  }

#define event ((ML_Event *)DEREFHANDLE(eventHandle))
  event->type      = DEREFWORD(Make_arbitrary_precision(taskData, type));
  event->sendEvent = DEREFWORD(Make_bool(send_event));
  event->window    = DEREFWINDOWHANDLE(W);

  switch(type)
  {
    case KeyPress:
    case KeyRelease:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_KeyEvent_Data), F_MUTABLE_BIT);

#define data ((ML_KeyEvent_Data *)DEREFHANDLE(dataHandle))
      data->root        = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xkey.root));
      data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xkey.subwindow));
      data->time        = DEREFWORD(Make_arbitrary_precision(taskData, ev->xkey.time));
      data->pointer     = (MLXPoint *)DEREFHANDLE(CreatePoint(taskData, ev->xkey.x,ev->xkey.y));
      data->rootPointer = (MLXPoint *)DEREFHANDLE(CreatePoint(taskData, ev->xkey.x_root,ev->xkey.y_root));
      data->modifiers   = DEREFWORD(Make_arbitrary_precision(taskData, ev->xkey.state));
      data->keycode     = DEREFWORD(Make_arbitrary_precision(taskData, ev->xkey.keycode));
#undef data

      event->data = DEREFHANDLE(FINISHED(taskData, dataHandle));
      
      break;
    }

      
    case ButtonPress:
    case ButtonRelease:
    {
      
      if (DEREFWINDOWHANDLE(W)->eventMask->Get(0).AsUnsigned() & ButtonClickMask)
      {
        Handle dataHandle;
        PredicateArgs A;

        A.button = &ev->xbutton;
        A.up     = (ev->type == ButtonRelease);
        A.down   = (ev->type == ButtonPress);
      
        WaitDoubleClickTime(dsHandle,&A);

        dataHandle = alloc_and_save(taskData, SIZEOF(ML_ButtonClick_Data), F_MUTABLE_BIT);
 
#define data ((ML_ButtonClick_Data *)DEREFHANDLE(dataHandle))
        data->root        = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xbutton.root));
        data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xbutton.subwindow));
        data->time        = DEREFWORD(Make_arbitrary_precision(taskData, ev->xbutton.time));
        data->pointer     = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xbutton.x,ev->xbutton.y));
        data->rootPointer = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xbutton.x_root,ev->xbutton.y_root));
        data->modifiers   = DEREFWORD(Make_arbitrary_precision(taskData, ev->xbutton.state));
        data->button      = DEREFWORD(Make_arbitrary_precision(taskData, ev->xbutton.button));
        data->up          = DEREFWORD(Make_arbitrary_precision(taskData, A.up));
        data->down        = DEREFWORD(Make_arbitrary_precision(taskData, A.down));
#undef data
  
        event->type = DEREFWORD(Make_arbitrary_precision(taskData, 42)); /* What's this for? */
        event->data = DEREFWORD(FINISHED(taskData, dataHandle));

      }
      else
      {
        Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_ButtonEvent_Data), F_MUTABLE_BIT);
  
#define data ((ML_ButtonEvent_Data *)DEREFHANDLE(dataHandle))
        data->root        = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xbutton.root));
        data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xbutton.subwindow));
        data->time        = DEREFWORD(Make_arbitrary_precision(taskData, ev->xbutton.time));
        data->pointer     = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xbutton.x,ev->xbutton.y));
        data->rootPointer = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xbutton.x_root,ev->xbutton.y_root));
        data->modifiers   = DEREFWORD(Make_arbitrary_precision(taskData, ev->xbutton.state));
        data->button      = DEREFWORD(Make_arbitrary_precision(taskData, ev->xbutton.button));
#undef data
  
        event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      }
      
      break;
    }


    case MotionNotify:
    {

      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_MotionEvent_Data), F_MUTABLE_BIT);

#define data ((ML_MotionEvent_Data *)DEREFHANDLE(dataHandle))
      data->root        = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xmotion.root));
      data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xmotion.subwindow));
      data->time        = DEREFWORD(Make_arbitrary_precision(taskData, ev->xmotion.time));
      data->pointer     = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xmotion.x,ev->xmotion.y));
      data->rootPointer = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xmotion.x_root,ev->xmotion.y_root));
      data->modifiers   = DEREFWORD(Make_arbitrary_precision(taskData, ev->xmotion.state));
      data->isHint      = DEREFWORD(Make_arbitrary_precision(taskData, ev->xmotion.is_hint));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
    
      
      break;
    }

    case EnterNotify:
    case LeaveNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_CrossingEvent_Data), F_MUTABLE_BIT);

#define data ((ML_CrossingEvent_Data *)DEREFHANDLE(dataHandle))
      data->root        = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xcrossing.root));
      data->subwindow   = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xcrossing.subwindow));
      data->time        = DEREFWORD(Make_arbitrary_precision(taskData, ev->xcrossing.time));
      data->pointer     = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xcrossing.x,ev->xcrossing.y));
      data->rootPointer = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xcrossing.x_root,ev->xcrossing.y_root));
      data->mode        = DEREFWORD(Make_arbitrary_precision(taskData, ev->xcrossing.mode));
      data->detail      = DEREFWORD(Make_arbitrary_precision(taskData, ev->xcrossing.detail));
      data->focus       = DEREFWORD(Make_bool(ev->xcrossing.focus));
      data->modifiers   = DEREFWORD(Make_arbitrary_precision(taskData, ev->xcrossing.state));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
    
      break;
    }

    case Expose:
    {
      int left   = ev->xexpose.x;
      int top    = ev->xexpose.y;
      int right  = left + ev->xexpose.width;
      int bottom = top  + ev->xexpose.height;

      Handle dataHandle;

      while(XCheckTypedWindowEvent(d,ev->xexpose.window,Expose,ev))
      {
        int L = ev->xexpose.x;
        int T = ev->xexpose.y;
        int R = L + ev->xexpose.width;
        int B = T + ev->xexpose.height;
        
        assert(ev->type == Expose);

        left   = min(left,L);
        top    = min(top,T);
        right  = max(right,R);
        bottom = max(bottom,B);
      }
      
      dataHandle = alloc_and_save(taskData, SIZEOF(ML_ExposeEvent_Data), F_MUTABLE_BIT);

#define data ((ML_ExposeEvent_Data *)DEREFHANDLE(dataHandle))
      data->region = (MLXRectangle *)DEREFHANDLE(CreateRect(taskData, top,left,bottom,right));
      data->count  = DEREFWORD(Make_arbitrary_precision(taskData, 0));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      break;
    }


    case GraphicsExpose:
    {
      int left   = ev->xgraphicsexpose.x;
      int top    = ev->xgraphicsexpose.y;
      int right  = left + ev->xgraphicsexpose.width;
      int bottom = top  + ev->xgraphicsexpose.height;

      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_GraphicsExposeEvent_Data), F_MUTABLE_BIT);

#define data ((ML_GraphicsExposeEvent_Data *)DEREFHANDLE(dataHandle))
      data->region = (MLXRectangle *)DEREFHANDLE(CreateRect(taskData, top,left,bottom,right));
      data->count  = DEREFWORD(Make_arbitrary_precision(taskData, ev->xgraphicsexpose.count));
      data->code   = DEREFWORD(Make_arbitrary_precision(taskData, ev->xgraphicsexpose.major_code));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      break;
    }

    case NoExpose:
    {
      event->data = DEREFWORD(Make_arbitrary_precision(taskData, ev->xnoexpose.major_code));
      
      break;
    }

    case ConfigureNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_ConfigureNotify_Data), F_MUTABLE_BIT);

#define data ((ML_ConfigureNotify_Data *)DEREFHANDLE(dataHandle))
      data->window           = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xconfigure.window));
      data->position         = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xconfigure.x,ev->xconfigure.y));
      data->size             = (MLXRectangle *)   DEREFHANDLE(CreateArea(ev->xconfigure.width,ev->xconfigure.height));
      data->borderWidth      = DEREFWORD(Make_int(ev->xconfigure.border_width));
      data->above            = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xconfigure.above));
      data->overrideRedirect = DEREFWORD(Make_bool(ev->xconfigure.override_redirect));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      break;
    }



    case FocusIn:
    case FocusOut:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_FocusChangeEvent_Data), F_MUTABLE_BIT);

#define data ((ML_FocusChangeEvent_Data *)DEREFHANDLE(dataHandle))
      data->mode   = DEREFWORD(Make_int(ev->xfocus.mode));
      data->detail = DEREFWORD(Make_int(ev->xfocus.detail));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));

      break;
    }

    case VisibilityNotify:
    {
      event->data = DEREFWORD(Make_int(ev->xvisibility.state));
      
      break;
    }


    case CreateNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_CreateEvent_Data), F_MUTABLE_BIT);

#define data ((ML_CreateEvent_Data *)DEREFHANDLE(dataHandle))
      data->window           = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xcreatewindow.window));
      data->position         = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xcreatewindow.x,ev->xcreatewindow.y));
      data->size             = (MLXRectangle *)   DEREFHANDLE(CreateArea(ev->xcreatewindow.width,ev->xcreatewindow.height));
      data->borderWidth      = DEREFWORD(Make_int(ev->xcreatewindow.border_width));
      data->overrideRedirect = DEREFWORD(Make_bool(ev->xcreatewindow.override_redirect));
#undef data

      event->data = DEREFHANDLE(FINISHED(taskData, dataHandle));
      
      break;
    }

    case DestroyNotify:
    {
      debugReclaim(Window,ev->xdestroywindow.window);
      event->data = DEREFWORD(EmptyWindow(taskData, dsHandle,ev->xdestroywindow.window));
      
      break;
    }
    
    case UnmapNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_UnmapEvent_Data), F_MUTABLE_BIT);

#define data ((ML_UnmapEvent_Data *)DEREFHANDLE(dataHandle))
      data->window        = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xunmap.window));
      data->fromConfigure = DEREFWORD(Make_bool(ev->xunmap.from_configure));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));

      break;
    }
    
    case MapNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_MapEvent_Data), F_MUTABLE_BIT);

#define data ((ML_MapEvent_Data *)DEREFHANDLE(dataHandle))
      data->window           = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xmap.window));
      data->overrideRedirect = DEREFWORD(Make_bool(ev->xmap.override_redirect));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      break;
    }
    
    case MapRequest:
    {
      event->data = DEREFWORD(EmptyWindow(taskData, dsHandle,ev->xmaprequest.window));
      
      break;
    }


    case ReparentNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_ReparentEvent_Data), F_MUTABLE_BIT);

#define data ((ML_ReparentEvent_Data *)DEREFHANDLE(dataHandle))
      data->window           = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xreparent.window));
      data->parent           = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xreparent.parent));
      data->position         = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xreparent.x,ev->xreparent.y));
      data->overrideRedirect = DEREFWORD(Make_bool(ev->xreparent.override_redirect));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      break;
    }


    case ConfigureRequest:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_ConfigureRequest_Data), F_MUTABLE_BIT);

#define data ((ML_ConfigureRequest_Data *)DEREFHANDLE(dataHandle))
      data->window      = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xconfigurerequest.window));
      data->position    = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xconfigurerequest.x,ev->xconfigurerequest.y));
      data->size        = (MLXRectangle *)   DEREFHANDLE(CreateArea(ev->xconfigurerequest.width,ev->xconfigurerequest.height));
      data->borderWidth = DEREFWORD(Make_int(ev->xconfigurerequest.border_width));
      data->above       = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xconfigurerequest.above));
      data->detail      = DEREFWORD(Make_int(ev->xconfigurerequest.detail));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));

      break;
    }

    case GravityNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_GravityEvent_Data), F_MUTABLE_BIT);

#define data ((ML_GravityEvent_Data *)DEREFHANDLE(dataHandle))
      data->window   = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xgravity.window));
      data->position = (MLXPoint *)       DEREFHANDLE(CreatePoint(taskData, ev->xgravity.x,ev->xgravity.y));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));

      break;
    }

    case ResizeRequest:
    {
      event->data = DEREFWORD(CreateArea(ev->xresizerequest.width,ev->xresizerequest.height));
      
      break;
    }


    case CirculateNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_CirculateEvent_Data), F_MUTABLE_BIT);

#define data ((ML_CirculateEvent_Data *)DEREFHANDLE(dataHandle))
      data->window = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xcirculate.window));
      data->place  = DEREFWORD(Make_int(ev->xcirculate.place));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      break;
    }

    case CirculateRequest:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_CirculateEvent_Data), F_MUTABLE_BIT);

#define data ((ML_CirculateEvent_Data *)DEREFHANDLE(dataHandle))
      data->window = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xcirculaterequest.window));
      data->place  = DEREFWORD(Make_int(ev->xcirculaterequest.place));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));

      break;
    }

    case ColormapNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_ColormapEvent_Data), F_MUTABLE_BIT);

#define data ((ML_ColormapEvent_Data *)DEREFHANDLE(dataHandle))
      data->colormap_object = (X_Colormap_Object *)DEREFHANDLE(EmptyColormap(taskData, dsHandle,ev->xcolormap.colormap));
      data->c_new             = DEREFWORD(Make_bool(ev->xcolormap.c_new));
      data->installed       = DEREFWORD(Make_bool(ev->xcolormap.state == ColormapInstalled));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      break;
    }
    
    case MappingNotify:
    {
      XRefreshKeyboardMapping((XMappingEvent *)ev); /* cast added SPF 6/1/94 */
      return 0; /* HACK !!!! */
    }

    case SelectionClear:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_SelectionClear_Data), F_MUTABLE_BIT);

#define data ((ML_SelectionClear_Data *)DEREFHANDLE(dataHandle))
      data->selection = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselectionclear.selection));
      data->time      = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselectionclear.time));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));

      break;
    }
    
    case SelectionNotify:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_Selection_Data), F_MUTABLE_BIT);

#define data ((ML_Selection_Data *)DEREFHANDLE(dataHandle))
      data->selection = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselection.selection));
      data->target    = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselection.target));
      data->property  = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselection.property));
      data->time      = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselection.time));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      break;
    }
    
    case SelectionRequest:
    {
      Handle dataHandle = alloc_and_save(taskData, SIZEOF(ML_SelectionRequest_Data), F_MUTABLE_BIT);

#define data ((ML_SelectionRequest_Data *)DEREFHANDLE(dataHandle))
      data->requestor = DEREFWINDOWHANDLE(EmptyWindow(taskData, dsHandle,ev->xselectionrequest.requestor));
      data->selection = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselectionrequest.selection));
      data->target    = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselectionrequest.target));
      data->property  = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselectionrequest.property));
      data->time      = DEREFWORD(Make_arbitrary_precision(taskData, ev->xselectionrequest.time));
#undef data

      event->data = DEREFWORD(FINISHED(taskData, dataHandle));
      
      break;
    }

    case ClientMessage:
    {
      unsigned protocols    = WM_PROTOCOLS(d);
      int deleteWindow = WM_DELETE_WINDOW(d);
      
      if (protocols                != None      && 
           deleteWindow             != None      && 
           ev->xclient.message_type == protocols && 
           ev->xclient.format       == 32        && 
           ev->xclient.data.l[0]    == deleteWindow)
      {
        event->type = DEREFWORD(Make_arbitrary_precision(taskData, 43)); /* (?) */
      
        break;
      }
      else return 0;
    }

    case PropertyNotify: return 0;
    
    case KeymapNotify: return 0;   /* Broken: the window field does not tell me the window requesting this event */

    default: Crash ("Bad event type %x",ev->type);
  }
  
  event->callbacks = FList; /* Safe, since FList is a Root */
  FList = TAGGED(0);

  event->events = GList; /* Safe, since GList is a Root */
  GList = TAGGED(0);

  return FINISHED(taskData, eventHandle);
#undef event
}

/******************************************************************************/
/*                                                                            */
/*      HERE                                                                  */
/*                                                                            */
/******************************************************************************/
static Handle LookupString(TaskData *taskData, Display *d, unsigned keycode, unsigned modifiers)
{
  XKeyEvent ev;
  int n;
  KeySym keysym; /* was int SPF 6/1/94 */
  char buffer[500];
  
  ev.display = d;
  ev.keycode = keycode;
  ev.state   = modifiers;
  
  n = XLookupString(&ev,buffer,sizeof(buffer)-1,&keysym,NULL);
  
  buffer[n] = '\0';
  
  return CreatePair(taskData, Make_string(buffer),Make_arbitrary_precision(taskData, keysym));
}

static Handle GetScreenSaver(TaskData *taskData, Display *d)
{
  int timeout,interval,blanking,exposures;
  Handle tuple;
  
  XGetScreenSaver(d,&timeout,&interval,&blanking,&exposures);
  
  tuple = alloc_and_save(taskData, 4, F_MUTABLE_BIT);

#define data DEREFHANDLE(tuple)
  data->Set(0, DEREFWORD(Make_int(timeout)));
  data->Set(1, DEREFWORD(Make_int(interval)));
  data->Set(2, DEREFWORD(Make_arbitrary_precision(taskData, blanking)));
  data->Set(3, DEREFWORD(Make_arbitrary_precision(taskData, exposures)));
#undef data
  
  return FINISHED(taskData, tuple);
}

static Handle TranslateCoordinates
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window src,
  Window dst,
  int    x,
  int    y
)
{
  Window child;
  int    dx,dy,s;
  
  s = XTranslateCoordinates(DEREFDISPLAYHANDLE(dsHandle)->display,src,dst,x,y,&dx,&dy,&child);
  
  if (s == 0) RaiseXWindows(taskData, "XTranslateCoordinates failed");
  
  return CreatePair(taskData, CreatePoint(taskData, dx,dy),EmptyWindow(taskData, dsHandle,child));
}
  

static Handle QueryBest
(
 TaskData *taskData,
 int    (*f)(Display*, Drawable, unsigned, unsigned, unsigned *, unsigned *),
 Display *d,
 Drawable drawable,
 unsigned width,
 unsigned height
 )
{
    unsigned W,H;
    
    int s = (* f)(d,drawable,width,height,&W,&H);
    
    if (s == 0) RaiseXWindows(taskData, "XQueryBest failed");
    
    return CreateArea(W,H);
}

static Handle QueryPointer
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window  w
)
{
  Window   root,child;
  int      rootX,rootY;
  int      winX,winY;
  unsigned mask;
  int      s;
  Handle tuple;
  
  s = XQueryPointer(DEREFDISPLAYHANDLE(dsHandle)->display,w,&root,&child,&rootX,&rootY,&winX,&winY,&mask);
  
  tuple = alloc_and_save(taskData, 6, F_MUTABLE_BIT);

#define data DEREFHANDLE(tuple)  
  data->Set(0, DEREFWORD(Make_arbitrary_precision(taskData, s)));
  data->Set(1, DEREFWORD(EmptyWindow(taskData, dsHandle,root)));
  data->Set(2, DEREFWORD(EmptyWindow(taskData, dsHandle,child)));
  data->Set(3, DEREFWORD(CreatePoint(taskData, rootX,rootY)));
  data->Set(4, DEREFWORD(CreatePoint(taskData, winX,winY)));
  data->Set(5, DEREFWORD(Make_arbitrary_precision(taskData, mask)));
#undef data
  
  return FINISHED(taskData, tuple);
}

static Handle ReadBitmap
(
  TaskData *taskData,
  Handle   dsHandle, /* handle to (X_Display_Object *) */
  Drawable w,
  PolyStringObject  *string
)
{
  unsigned width,height;
  char name[500];
  int s,xhot,yhot;
  Pixmap pixmap;
  Handle tuple;

  Poly_string_to_C(string,name,sizeof(name));

  s = XReadBitmapFile(DEREFDISPLAYHANDLE(dsHandle)->display,w,name,&width,&height,&pixmap,&xhot,&yhot);
  
  tuple = alloc_and_save(taskData, 4, F_MUTABLE_BIT);

#define data DEREFHANDLE(tuple)

  data->Set(0,DEREFWORD(Make_arbitrary_precision(taskData, s)));

  if (s == BitmapSuccess)
  {
    data->Set(1, DEREFWORD(EmptyPixmap(taskData, dsHandle,pixmap)));
    data->Set(2, DEREFWORD(CreateArea(width,height)));
    data->Set(3, DEREFWORD(CreatePoint(taskData, xhot,yhot)));
  }
  
  /******************** What if we don't succeed? Badly-formed tuple !!!! */

#undef data
  
  return FINISHED(taskData, tuple);
}

static Handle WriteBitmapFile
(
  TaskData *taskData,
  PolyStringObject  *string,
  Display *d,
  Pixmap   bitmap,
  unsigned w,
  unsigned h,
  int      x,
  int      y
)
{
  char name[500]; int s;

  Poly_string_to_C(string,name,sizeof(name));

  s = XWriteBitmapFile(d,name,bitmap,w,h,x,y);
  
  return Make_arbitrary_precision(taskData, s);
}

static Handle GetDefault(TaskData *taskData, Display *d, PolyStringObject *s1, PolyStringObject *s2)
{
  char program[500]; char option[500]; char *s;

  Poly_string_to_C(s1,program,sizeof(program));
  Poly_string_to_C(s2,option ,sizeof(option));

  s = XGetDefault(d,program,option);
  
  if (s == NULL) RaiseXWindows(taskData, "XGetDefault failed");
  
  return Make_string(s);
}


static void GetWindows(TaskData *taskData, PolyWord p, void *w, unsigned)
{
    *(Window *)w = GetWindow(taskData, (X_Object *)p.AsObjPtr());
}


static void GetSegments(TaskData *taskData, PolyWord pp, void *w, unsigned)
{
    XSegment *A = (XSegment *)w;
    PolyObject *p = pp.AsObjPtr();
    A->x1 = GetPointX(taskData, p->Get(0));
    A->y1 = GetPointY(taskData, p->Get(0));
    A->x2 = GetPointX(taskData, p->Get(1));
    A->y2 = GetPointY(taskData, p->Get(1));
}

static void GetChar2(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    XChar2b *A = (XChar2b *)v;
    unsigned short u = get_C_ushort(taskData, p);
    A->byte1 = u >> 8;
    A->byte2 = u &0xFF;
}

static void CopyString(TaskData *, PolyWord w, void *v, unsigned)
{
    char **p = (char**)v;
    PolyStringObject *s = GetString(w);
    POLYUNSIGNED    n = s->length+1;
    *p = (char*)malloc(n);
    Poly_string_to_C(s,*p,n);
}

static void GetText(TaskData *taskData, PolyWord p, void *w, unsigned)
{
    XTextItem *A = (XTextItem *)w;
    PolyObject *obj = p.AsObjPtr();
    CopyString(taskData, obj->Get(0), &A->chars, 0);
    
    A->nchars = strlen(A->chars);
    A->delta  = get_C_short(taskData, obj->Get(1));
    A->font   = GetFont(taskData, (X_Object *)obj->Get(2).AsObjPtr());
}

static void GetText16(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    XTextItem16 *A = (XTextItem16 *)v;
    PolyObject *obj = p.AsObjPtr();
    unsigned     N = ListLength(obj->Get(0));
    XChar2b *L = (XChar2b *) malloc(N * sizeof(XChar2b));
    
    GetList4(taskData,obj->Get(0),L,sizeof(XChar2b),GetChar2);
    
    A->chars  = L;
    A->nchars = N;
    A->delta  = get_C_short(taskData, obj->Get(1));
    A->font   = GetFont(taskData, (X_Object *)obj->Get(2).AsObjPtr());
}

typedef void (*GetFunc)(TaskData *taskData, PolyWord, void*, unsigned);

static void SetClipRectangles
(
  TaskData *taskData,
  Display *d,
  GC       gc,
  int      x,
  int      y,
  Handle   list,
  unsigned order
)
{
  if (ISNIL(DEREFWORD(list)))
  {
    XSetClipRectangles(d,gc,x,y,NULL,0,order);
  }
  else
  {
    unsigned        N = ListLength(DEREFWORD(list));
    XRectangle *L = (XRectangle *) alloca(N * sizeof(XRectangle));
    
    GetList4(taskData, DEREFWORD(list),L,sizeof(XRectangle),GetRects);
    
    XSetClipRectangles(d,gc,x,y,L,N,order);
  }
}

static void GetUChars(TaskData *taskData, PolyWord p, void *u, unsigned)
{
    *(uchar*)u = get_C_uchar(taskData, p);
}

static void SetDashes
(
 TaskData *taskData,
 Display *d,
 GC       gc,
 unsigned offset,
 Handle   list
 )
{
    if (NONNIL(DEREFWORD(list)))
    {
        unsigned   N = ListLength(DEREFWORD(list));
        char *D  = (char *) alloca(N);
        
        GetList4(taskData,DEREFWORD(list),D,sizeof(uchar),GetUChars);
        
        XSetDashes(d,gc,offset,D,N);
    }
}

static Handle CreateDrawable
(
  TaskData *taskData,
  void  *p,
  Handle   dsHandle /* Handle to (X_Display_Object *) */
)
{
    return EmptyWindow(taskData, dsHandle,*(Window*)p);
}

static Handle QueryTree
(
  TaskData *taskData,
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  Window   w
)
{
  Window root,parent,*children; 
  unsigned n;
  Handle data;

  int s = XQueryTree(DEREFDISPLAYHANDLE(dsHandle)->display,w,&root,&parent,&children,&n);

  if (s == 0) RaiseXWindows(taskData, "XQueryTree failed");

  data = CreateTriple(taskData, EmptyWindow(taskData, dsHandle,root),
                      EmptyWindow(taskData, dsHandle,parent),
                      CreateList5(taskData, n,children,sizeof(Window),CreateDrawable,dsHandle));

  if (n) XFree((char *)children);

  return data;
}

static void RestackWindows(TaskData *taskData, Handle list /* handle to list of X_Window_Objects (?) */)
{
  if (NONNIL(DEREFWORD(list)))
  {
    unsigned N = ListLength(DEREFWORD(list));
    Window  *W = (Window *) alloca(N * sizeof(Window));
    Display *d = GetDisplay(taskData, (X_Object *)DEREFLISTHANDLE(list)->h.AsObjPtr());
  
    GetList4(taskData, DEREFWORD(list),W,sizeof(Window),GetWindows);
  
    XRestackWindows(d,W,N);
  }
}

static Handle GetGeometry
(
  TaskData *taskData,
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  Drawable w
)
{
  int x,y;
  unsigned width,height,borderWidth,depth;
  Window root;
  Handle dataHandle;

  int s = XGetGeometry(DEREFDISPLAYHANDLE(dsHandle)->display,w,&root,&x,&y,&width,&height,&borderWidth,&depth);
  
  if (s == 0) RaiseXWindows(taskData, "XGetGeometry failed");
  
  dataHandle = alloc_and_save(taskData, 5, F_MUTABLE_BIT);

#define data DEREFHANDLE(dataHandle) 
  data->Set(0, DEREFWORD(EmptyWindow(taskData, dsHandle,root)));
  data->Set(1, DEREFWORD(CreatePoint(taskData, x,y)));
  data->Set(2, DEREFWORD(CreateArea(width,height)));
  data->Set(3, DEREFWORD(Make_arbitrary_precision(taskData, borderWidth)));
  data->Set(4, DEREFWORD(Make_arbitrary_precision(taskData, depth)));
#undef data

  return FINISHED(taskData, dataHandle);
}

static Handle GetWindowAttributes
(
  TaskData *taskData,
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  Drawable w
)
{
  XWindowAttributes wa; 
  Handle dataHandle;
  
  int s = XGetWindowAttributes(DEREFDISPLAYHANDLE(dsHandle)->display,w,&wa);
  
  if (s == 0) RaiseXWindows(taskData, "XGetWindowAttributes failed");
  
  dataHandle = alloc_and_save(taskData, 20, F_MUTABLE_BIT);

/* HACKY - should define struct? */
  DEREFHANDLE(dataHandle)->Set( 0, DEREFWORD(CreatePoint(taskData, wa.x,wa.y)));
  DEREFHANDLE(dataHandle)->Set( 1, DEREFWORD(CreateArea(wa.width,wa.height)));
  DEREFHANDLE(dataHandle)->Set( 2, DEREFWORD(Make_int(wa.border_width)));
  DEREFHANDLE(dataHandle)->Set( 3, DEREFWORD(Make_arbitrary_precision(taskData, wa.depth)));
  DEREFHANDLE(dataHandle)->Set( 4, DEREFWORD(EmptyVisual(taskData, dsHandle,wa.visual)));
  DEREFHANDLE(dataHandle)->Set( 5, DEREFWORD(EmptyWindow(taskData, dsHandle,wa.root)));
  DEREFHANDLE(dataHandle)->Set( 6, DEREFWORD(Make_arbitrary_precision(taskData, wa.c_class)));
  DEREFHANDLE(dataHandle)->Set( 7, DEREFWORD(Make_arbitrary_precision(taskData, wa.bit_gravity)));
  DEREFHANDLE(dataHandle)->Set( 8, DEREFWORD(Make_arbitrary_precision(taskData, wa.win_gravity)));
  DEREFHANDLE(dataHandle)->Set( 9, DEREFWORD(Make_arbitrary_precision(taskData, wa.backing_store)));
  DEREFHANDLE(dataHandle)->Set(10, DEREFWORD(Make_arbitrary_precision(taskData, wa.backing_planes)));
  DEREFHANDLE(dataHandle)->Set(11, DEREFWORD(Make_arbitrary_precision(taskData, wa.backing_pixel)));
  DEREFHANDLE(dataHandle)->Set(12, DEREFWORD(Make_bool(wa.save_under)));
  DEREFHANDLE(dataHandle)->Set(13, DEREFWORD(EmptyColormap(taskData, dsHandle,wa.colormap)));
  DEREFHANDLE(dataHandle)->Set(14, DEREFWORD(Make_bool(wa.map_installed)));
  DEREFHANDLE(dataHandle)->Set(15, DEREFWORD(Make_arbitrary_precision(taskData, wa.map_state)));
  DEREFHANDLE(dataHandle)->Set(16, DEREFWORD(Make_arbitrary_precision(taskData, wa.all_event_masks)));
  DEREFHANDLE(dataHandle)->Set(17, DEREFWORD(Make_arbitrary_precision(taskData, wa.your_event_mask)));
  DEREFHANDLE(dataHandle)->Set(18, DEREFWORD(Make_arbitrary_precision(taskData, wa.do_not_propagate_mask)));
  DEREFHANDLE(dataHandle)->Set(19, DEREFWORD(Make_bool(wa.override_redirect)));
  
  return FINISHED(taskData, dataHandle);
}

static void ChangeWindowAttributes
(
  TaskData *taskData,
  X_Window_Object *W,
  unsigned         n,
  PolyWord         P
)
{
  XSetWindowAttributes a;
  
  unsigned mask = 1 << n;
  
  switch(mask)
  {
    case CWBitGravity:       a.bit_gravity           = get_C_ulong(taskData, P); break;
    case CWWinGravity:       a.win_gravity           = get_C_ulong(taskData, P); break;
    case CWBackingStore:     a.backing_store         = get_C_ulong(taskData, P); break;
    case CWBackingPlanes:    a.backing_planes        = get_C_ulong(taskData, P); break;
    case CWBackingPixel:     a.backing_pixel         = get_C_ulong(taskData, P); break;
    case CWOverrideRedirect: a.override_redirect     = get_C_ulong(taskData, P); break;
    case CWSaveUnder:        a.save_under            = get_C_ulong(taskData, P); break;
    case CWEventMask:        a.event_mask            = get_C_ulong(taskData, P); break;
    case CWDontPropagate:    a.do_not_propagate_mask = get_C_ulong(taskData, P); break;

    case CWBackPixel:    a.background_pixel = get_C_ulong(taskData, P);
                         W->backgroundPixmap = 0;
                         break;
                         
    case CWBackPixmap:   a.background_pixmap = GetPixmap(taskData, (X_Object *)P.AsObjPtr());
                         W->backgroundPixmap = PixmapObject((X_Object *)P.AsObjPtr());
                         break;
                         
    case CWBorderPixel:  a.border_pixel = get_C_ulong(taskData, P);
                         W->borderPixmap = 0;
                         break;
                         
    case CWBorderPixmap: a.border_pixmap = GetPixmap(taskData, (X_Object *)P.AsObjPtr());
                         W->borderPixmap = PixmapObject((X_Object *)P.AsObjPtr());
                         break;
                         
    case CWColormap:     a.colormap = GetColormap(taskData, (X_Object *)P.AsObjPtr());
                         W->colormap_object = ColormapObject((X_Object *)P.AsObjPtr());
                         break;
                         
    case CWCursor:       a.cursor = GetCursor(taskData, (X_Object *)P.AsObjPtr());
                         W->cursor_object = CursorObject((X_Object *)P.AsObjPtr());
                         break;

    default: Crash ("Bad window mask %u",mask);
  }
  
  XChangeWindowAttributes(GetDisplay(taskData, (X_Object *)W),GetWindow(taskData, (X_Object *)W),mask,&a);
}


static void ConfigureWindow
(
  TaskData *taskData,
  Display *d,
  Window   w,
  PolyWord   tup /* (P,S,w,d,s,flags) */
)
{
    PolyObject *tuple = tup.AsObjPtr();
  XWindowChanges wc;
  
  unsigned mask = get_C_ulong(taskData, tuple->Get(5));
  
  CheckZeroRect(taskData, tuple->Get(1));
  
  wc.x            = GetPointX  (taskData,tuple->Get(0));
  wc.y            = GetPointY  (taskData,tuple->Get(0));
  wc.width        = GetRectW   (taskData,tuple->Get(1)); 
  wc.height       = GetRectH   (taskData,tuple->Get(1));
  wc.border_width = get_C_ulong(taskData, tuple->Get(2));
  wc.sibling      = GetWindow  (taskData,(X_Object *)tuple->Get(3).AsObjPtr());
  wc.stack_mode   = get_C_ulong(taskData, tuple->Get(4));
  
  XConfigureWindow(d,w,mask,&wc);
}



/* The order of these depends on the XColor datatype */

typedef struct
{
  PolyWord red;     /* ML bool */
  PolyWord blue;    /* ML bool */
  PolyWord doRed;   /* ML bool */
  PolyWord green;   /* ML int */
  PolyWord pixel;   /* ML int */
  PolyWord doBlue;  /* ML int */
  PolyWord doGreen; /* ML int */
} MLXColor;      /* in Poly heap */

static void ClearXColor(XColor *x)
{
  x->red = x->green = x->blue = x->pixel = x->flags = 0;
}

static Handle CreateXColor(TaskData *taskData, XColor *x)
{
  Handle XHandle = alloc_and_save(taskData, SIZEOF(MLXColor), F_MUTABLE_BIT);

#define X ((MLXColor *)DEREFHANDLE(XHandle))
  X->red     = DEREFWORD(Make_arbitrary_precision(taskData, x->red));
  X->green   = DEREFWORD(Make_arbitrary_precision(taskData, x->green));
  X->blue    = DEREFWORD(Make_arbitrary_precision(taskData, x->blue));
  X->pixel   = DEREFWORD(Make_arbitrary_precision(taskData, x->pixel));
  X->doRed   = DEREFWORD(Make_bool(x->flags &DoRed));
  X->doGreen = DEREFWORD(Make_bool(x->flags &DoGreen));
  X->doBlue  = DEREFWORD(Make_bool(x->flags &DoBlue));
#undef X

  return FINISHED(taskData, XHandle);
}

static Handle CreateXColorF(TaskData *taskData, void *p)
{
    return CreateXColor(taskData, (XColor*)p);
}

static XColor xcolor1 = { 0 };
static XColor xcolor2 = { 0 };

static void GetXColor(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    MLXColor *P = (MLXColor *)p.AsObjPtr();
    XColor *x = (XColor *)v;
    x->red   = get_C_ushort(taskData, P->red);
    x->green = get_C_ushort(taskData, P->green);
    x->blue  = get_C_ushort(taskData, P->blue);
    x->pixel = get_C_ulong (taskData, P->pixel);
    
    x->flags = (DoRed   * get_C_ulong(taskData, P->doRed))
        | (DoGreen * get_C_ulong(taskData, P->doGreen))
        | (DoBlue  * get_C_ulong(taskData, P->doBlue));
}

static XColor *GetXColor1(TaskData *taskData, PolyWord P)
{
    GetXColor(taskData, P, &xcolor1, 0);
    return &xcolor1;
}

static XColor *GetXColor2(TaskData *taskData, PolyWord P)
{
    GetXColor(taskData, P, &xcolor2, 0);
    return &xcolor2;
}

static Handle AllocColor(TaskData *taskData, Display *d, Colormap cmap, XColor *x)
{
  int s = XAllocColor(d,cmap,x);

  if (s == 0) RaiseXWindows(taskData, "XAllocColor failed");
  
  return CreateXColor(taskData, x);
}

static Handle CreateUnsigned(TaskData *taskData, void *q)
{
    unsigned *p = (unsigned *)q;
    return Make_arbitrary_precision(taskData, *p);
}

static Handle CreateUnsignedLong(TaskData *taskData, void *p)
{
    return Make_arbitrary_precision(taskData, *(unsigned long*)p);
}

static Handle AllocColorCells
(
  TaskData *taskData,
  Display *d,
  Colormap cmap,
  unsigned contig,
  unsigned nplanes,
  unsigned ncolors
)
{
  unsigned long *masks;  /* was unsigned SPF 6/1/94 */
  unsigned long *pixels; /* was unsigned SPF 6/1/94 */
  int s;
  
  if (ncolors < 1) RaiseRange(taskData);
  
  masks  = (unsigned long *) alloca(nplanes * sizeof(unsigned long));
  pixels = (unsigned long *) alloca(ncolors * sizeof(unsigned long));

  s = XAllocColorCells(d,cmap,contig,masks,nplanes,pixels,ncolors);

  if (s == 0) RaiseXWindows (taskData, "XAllocColorCells failed");
  
  return CreatePair(taskData, CreateList4(taskData,nplanes,masks ,sizeof(unsigned long),CreateUnsignedLong),
                    CreateList4(taskData,ncolors,pixels,sizeof(unsigned long),CreateUnsignedLong));
}

static Handle AllocColorPlanes
(
  TaskData *taskData,
  Display *d,
  Colormap cmap,
  unsigned contig,
  unsigned ncolors,
  unsigned nreds,
  unsigned ngreens,
  unsigned nblues
)
{
  unsigned long rmask;   /* was unsigned SPF 6/1/94 */ 
  unsigned long gmask;   /* was unsigned SPF 6/1/94 */
  unsigned long bmask;   /* was unsigned SPF 6/1/94 */
  unsigned long *pixels; /* was unsigned SPF 6/1/94 */
  Handle tuple;
  int s;
  
  if (ncolors < 1) RaiseRange(taskData);
  
  pixels = (unsigned long *) alloca(ncolors * sizeof(unsigned long));

  s = XAllocColorPlanes(d,cmap,contig,pixels,ncolors,nreds,ngreens,nblues,&rmask,&gmask,&bmask);

  if (s == 0) RaiseXWindows (taskData, "XAllocColorPlanes failed");
  
  tuple = alloc_and_save(taskData, 4, F_MUTABLE_BIT);

#define data DEREFHANDLE(tuple)
  data->Set(0, DEREFWORD(CreateList4(taskData,ncolors,pixels,sizeof(unsigned long),CreateUnsignedLong)));
  data->Set(1, DEREFWORD(Make_arbitrary_precision(taskData, rmask)));
  data->Set(2, DEREFWORD(Make_arbitrary_precision(taskData, gmask)));
  data->Set(3, DEREFWORD(Make_arbitrary_precision(taskData, bmask)));
#undef data

  return FINISHED(taskData, tuple);
}

static Handle AllocNamedColor(TaskData *taskData, Display *d, Colormap cmap, PolyStringObject *string)
{
  char   name[500];
  int    s;
  XColor hardware;
  XColor database;
  
  ClearXColor(&hardware);
  ClearXColor(&database);
  
  Poly_string_to_C(string,name,sizeof(name));

  s = XAllocNamedColor(d,cmap,name,&hardware,&database);

  if (s == 0) RaiseXWindows (taskData, "XAllocNamedColor failed");
  
  return CreatePair(taskData, CreateXColor(taskData, &hardware),CreateXColor(taskData, &database));
}

static Handle LookupColor(TaskData *taskData, Display *d, Colormap cmap, PolyStringObject *string)
{
  char   name[500];
  int    s;
  XColor hardware;
  XColor database;
  
  ClearXColor(&hardware);
  ClearXColor(&database);
  
  Poly_string_to_C(string,name,sizeof(name));

  s = XLookupColor(d,cmap,name,&database,&hardware);

  if (s == 0) RaiseXWindows (taskData, "XLookupColor failed");
  
  return CreatePair(taskData, CreateXColor(taskData, &database),CreateXColor(taskData, &hardware));
}

static Handle ParseColor(TaskData *taskData, Display *d, Colormap cmap, PolyStringObject *string)
{
  char   name[500];
  int    s;
  XColor x;
  
  ClearXColor(&x);
  
  Poly_string_to_C(string,name,sizeof(name));

  s = XParseColor(d,cmap,name,&x);

  if (s == 0) RaiseXWindows(taskData, "XParseColor failed");
  
  return CreateXColor(taskData, &x);
}

static Handle QueryColor(TaskData *taskData, Display *d, Colormap cmap, unsigned pixel)
{
  XColor x;
  
  ClearXColor(&x);

  x.pixel = pixel;
  
  XQueryColor(d,cmap,&x);

  return CreateXColor(taskData, &x);
}

static void GetXPixel(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    XColor *X = (XColor *)v;
    ClearXColor(X);
    X->pixel = get_C_ulong(taskData, p);
}

static Handle QueryColors(TaskData *taskData, Display *d, Colormap cmap, Handle list)
{
  unsigned N = ListLength(DEREFWORD(list));
  XColor  *P = (XColor *) alloca(N * sizeof(XColor));
  
  GetList4(taskData, DEREFWORD(list),P,sizeof(XColor),GetXPixel);
  
  XQueryColors(d,cmap,P,N);

  return CreateList4(taskData,N,P,sizeof(XColor),CreateXColorF);
}

static void StoreNamedColor
(
  Display *d,
  Colormap cmap,
  PolyStringObject  *string,
  unsigned pixel,
  unsigned doRed,
  unsigned doGreen,
  unsigned doBlue
)
{
  unsigned flags = (DoRed * doRed) | (DoGreen * doGreen) | (DoBlue * doBlue);

  char name[500];
  
  Poly_string_to_C(string,name,sizeof(name));

  XStoreNamedColor(d,cmap,name,pixel,flags);
}

static void StoreColors(TaskData *taskData, Display *d, Colormap cmap, Handle list)
{
  unsigned N = ListLength(DEREFWORD(list));
  XColor  *P = (XColor *) alloca(N * sizeof(XColor));
  
  GetList4(taskData, DEREFWORD(list),P,sizeof(XColor),GetXColor);
  
  XStoreColors(d,cmap,P,N);
}

static void GetUnsigned(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    unsigned *u = (unsigned *)v;
    *u = get_C_ulong(taskData, p);
}

static void GetUnsignedLong(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    unsigned long *u = (unsigned long *)v;
    *u = get_C_ulong(taskData, p);
}


static void FreeColors
(
  TaskData *taskData, 
  Display *d,
  Colormap cmap,
  Handle   list,
  unsigned planes
)
{
  unsigned  N = ListLength(DEREFWORD(list));
  unsigned long *P = (unsigned long *) alloca(N * sizeof(unsigned long));
  
  GetList4(taskData,DEREFWORD(list),P,sizeof(unsigned long),GetUnsignedLong);
  
  XFreeColors(d,cmap,P,N,planes);
}

static Handle CreateColormap
(
  TaskData *taskData,
  void *p,
  Handle   dsHandle /* handle to (X_Display_Object *) */
)
{
  return EmptyColormap(taskData, dsHandle,*(Colormap *)p);
}

static Handle ListInstalledColormaps
(
  TaskData *taskData,
  Handle   dsHandle, /* handle to (X_Display_Object *) */
  Drawable drawable
)
{
  int  count;
  Colormap *cmaps;
  Handle list;
  
  cmaps = XListInstalledColormaps(DEREFDISPLAYHANDLE(dsHandle)->display,drawable,&count);
  
  if (cmaps == 0) RaiseXWindows(taskData, "XListInstalledColormaps failed");
  
  list = CreateList5(taskData,count,cmaps,sizeof(Colormap),CreateColormap,dsHandle);
  
  XFree((char *)cmaps);
  
  return list;
}


static Handle GetTimeOfDay(TaskData *taskData)
{
  TimeVal now;
  
  gettimeofday(&now, NULL);
  
  return CreatePair(taskData, Make_arbitrary_precision(taskData, now.tv_sec),Make_arbitrary_precision(taskData, now.tv_usec));
}

static Handle GetState(TaskData *taskData, X_Window_Object *P)
{
  assert(UNTAGGED(P->type) == X_Window);

  CheckExists((X_Object *)P,window);

  if (ISNIL(P->handler)) Crash ("No handler set");

  return CreatePair(taskData, SAVE(P->handler),SAVE(P->state));
}

static void SetState(X_Window_Object *W, PolyWord handler, PolyWord state)
{
  if (! ResourceExists((X_Object *)W)) return;

  assert(W->type == TAGGED(X_Window));

  if (NONNIL(handler))
  {
    /* we are setting the handler and initial state    */
    /* so we need to remove all pending messages for   */
    /* this window since they will have the wrong type */
    
    PurgePendingWindowMessages(W);
  
    W->handler = handler;
    W->state = state;

  }
  else W->state = state;   /* just update state */
}

/* Check if the first timer event has already expired. */
static void CheckTimerQueue(void)
{
  if (TList)
  {
    TimeVal now;
    gettimeofday(&now, NULL);
    TList->expired = TimeLeq(&TList->timeout,&now);
  }
}

static void InsertTimeout
(
  TaskData *taskData,
  X_Window_Object *window_object,
  unsigned        ms,
  PolyWord        alpha,
  PolyWord        handler
)
{
  T_List **tail;
  T_List *newp;
  TimeVal now;
  
  assert(window_object->type == TAGGED(X_Window));
  CheckExists((X_Object *)window_object,window);
  
  if (ISNIL(window_object->handler)) Crash ("No handler set");
  
  if (window_object->handler != handler) RaiseXWindows(taskData, "Handler mismatch");
  
  { /* find insertion point in list */
    TimeVal dt;
    
    gettimeofday(&now, NULL);
    dt.tv_sec  = ms / 1000;
    dt.tv_usec = 1000 * (ms % 1000);
    
    newp = (T_List *) malloc(sizeof(T_List));
    TimeAdd(&now,&dt,&newp->timeout);
    
    /* We use TimeLt here, not TimeLeq, because we
       want to add new messages AFTER existing ones.
       SPF 21/3/97
    */
    for(tail = &TList; *tail; tail = &(*tail)->next)
    {
      if (TimeLt(&newp->timeout,&(*tail)->timeout)) break;
    }
  }
  
  newp->next          = *tail;
  newp->window_object = window_object;
  newp->widget_object = (X_Widget_Object *)0;
  newp->alpha         = alpha.AsObjPtr();
  newp->handler       = handler.AsObjPtr();
  newp->expired       = 0;

  *tail = newp;
}

/* called when a widget is destroyed by Xt/Motif */
static void DestroyWidgetCallback
(
  Widget    widget,
  XtPointer client_data,
  XtPointer call_data
)
{
  /* find the ML widget (if any) associated with the C widget */
  X_Widget_Object *widget_object = FindWidget(widget);
  
  if (widget_object != NULL)
    {
      /* Destroy the ML widget representations */
      DestroyXObject((X_Object *)widget_object);
      /* Assume we can't get a C callback from a destroyed widget */
      PurgeCCallbacks(widget_object,widget);
    }

  debugReclaim(Widget,widget);
}

#if 0
#define CheckRealized(Widget,Where)\
{ \
  if (XtIsRealized(Widget) == False) \
    RaiseXWindows(taskData, #Where ": widget is not realized"); \
}

static Window WindowOfWidget(TaskData *taskData, Widget widget)
{
  CheckRealized(widget,WindowOfWidget);
  return XtWindowOfObject(widget);
}
#endif

/* Now returns NULL (None) for unrealized widgets SPF 1/2/94 */
static Window WindowOfWidget(Widget widget)
{
  return XtIsRealized(widget) ? XtWindowOfObject(widget) : None;
}


static void InsertWidgetTimeout
(
 TaskData *taskData,
 X_Widget_Object *widget_object,
 unsigned         ms,
 PolyWord         alpha,
 PolyWord         handler
 )
{
    T_List **tail;
    T_List *newp;
    TimeVal now;
    
    assert(widget_object->type == TAGGED(X_Widget));
    CheckExists((X_Object *)widget_object,widget);
#if NEVER
    CheckRealized(GetWidget(taskData, (X_Object *)widget_object),InsertWidgetTimeout);
#endif
    
    /* check that handler occurs in widget's callback list */
    {
        PolyWord p = widget_object->callbackList;
        for(; NONNIL(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
        {
            MLPair *q = (MLPair *)((ML_Cons_Cell*)p.AsObjPtr())->h.AsObjPtr();
            if (SND(q) == handler) break;
        }
        if (ISNIL(p)) RaiseXWindows(taskData, "Handler mismatch");
    }
    
    
    {
        TimeVal dt;
        
        gettimeofday(&now, NULL);
        
        dt.tv_sec = ms / 1000;
        dt.tv_usec = 1000 * (ms % 1000);
        
        newp = (T_List *) malloc(sizeof(T_List));
        
        TimeAdd(&now,&dt,&newp->timeout);
        
        /* We use TimeNegative here, not TimeExpired, because we
        want to add new messages AFTER existing ones.
        SPF 21/3/97
        */
        for(tail = &TList; *tail; tail = &(*tail)->next)
        {
            if (TimeLt(&newp->timeout,&(*tail)->timeout)) break;
        }
    }
    
    newp->next          = *tail;
    newp->window_object = (X_Window_Object *)0;
    newp->widget_object = widget_object;
    newp->alpha         = alpha.AsObjPtr();
    newp->handler       = handler.AsObjPtr();
    newp->expired       = 0;
    
    *tail = newp;
}

static Handle NextEvent(TaskData *taskData, Handle dsHandle /* handle to (X_Display_Object *) */)
{
    for (;;)
    {
        /* Added here SPF 23/2/95 - check whether a timer event has expired */
        CheckTimerQueue();
        
        if (TList && TList->expired)
        {
            T_List *next = TList->next;
            
            EventHandle E = alloc_and_save(taskData, SIZEOF(ML_Event), F_MUTABLE_BIT);
            
#define event ((ML_Event *)DEREFHANDLE(E))
            event->type       = DEREFWORD(Make_arbitrary_precision(taskData, 99));
            event->sendEvent  = DEREFWORD(Make_bool(True));
            event->data       = TList->alpha;
            
            if (TList->window_object != 0)
            {
                assert(TList->widget_object == 0);
                
                event->window     = TList->window_object;
                event->callbacks  = ListNull;
                event->events     = ListNull;
                
                assert(TList->window_object->handler == TList->handler);
            }
            else /* it is a Widget message */
            {
                /* TList->widget_object etc. act like Roots */
                assert(TList->widget_object != 0);
                
                {
                    Window w        = WindowOfWidget(GetWidget(taskData, (X_Object *)TList->widget_object));
                    event->window   = DEREFWINDOWHANDLE(EmptyWindow(taskData, GetDS(taskData, (X_Object *)TList->widget_object),w));
                }
                
                { /* create callback list - allocates storage */
                    Handle tailHandle    = SAVE(ListNull);
                    Handle widgetHandle  = SAVE(TList->widget_object);
                    Handle handlerHandle = SAVE(TList->handler);
                    Handle pairHandle    = CreatePair(taskData, widgetHandle,handlerHandle);
                    
                    event->callbacks     = DEREFLISTHANDLE(CreatePair(taskData, pairHandle,tailHandle));
                    event->events        = ListNull;
                }
            }
#undef event
            
            free(TList);
            
            TList = next;
            
            return FINISHED(taskData, E);
        }
        else /* ! (TList && TList->expired) */ if (DEREFDISPLAYHANDLE(dsHandle)->app_context == 0)
                /* use XNextEvent to get next event */
        {
            Display *display = DEREFDISPLAYHANDLE(dsHandle)->display;
            int      pending = XPending(display);
            
            if (pending == 0)
            {
                WaitInputFD waiter(display->fd);
                processes->ThreadPauseForIO(taskData, &waiter);
            }
            else /* X Event arrived */
            {
                XEvent ev;
                X_Window_Object *W;
                
                XNextEvent(display,&ev);
                W = FindWindow(dsHandle,ev.xany.window);
                
                if (W && NONNIL(W->handler))
                {
                    EventHandle E = CreateEvent(taskData, dsHandle,&ev,SAVE(W));
                    if (E) return E;
                }
            }
        }
        else /* use XtAppNextEvent to get next event */
        {
            /* should use Xt to do time events as well */
            int pending = XtAppPending(DEREFDISPLAYHANDLE(dsHandle)->app_context);
            
            if (pending == 0)
            {
                WaitInputFD waiter(DEREFDISPLAYHANDLE(dsHandle)->display->fd);
                processes->ThreadPauseForIO(taskData, &waiter);
            }
            else
            {
                if ((pending & XtIMXEvent) == 0)   /* not an X Event, probably an Xt timer event */
                {
                    assert(FList == TAGGED(0));
                    
                    callbacks_enabled = True;
                    XtAppProcessEvent(DEREFDISPLAYHANDLE(dsHandle)->app_context,pending);
                    callbacks_enabled = False;
                    
                    if (FList != TAGGED(0))
                    {
                        EventHandle E = alloc_and_save(taskData, SIZEOF(ML_Event), F_MUTABLE_BIT);
                        
#define event ((ML_Event *)DEREFHANDLE(E))
                        event->type      = DEREFWORD(Make_arbitrary_precision(taskData, 100));
                        event->sendEvent = DEREFWORD(Make_bool(True));
                        event->window    = TAGGED(0);
                        event->data      = TAGGED(0);
                        event->callbacks = FList; /* FList != 0 */
                        event->events    = GList;
#undef event
                        FList = TAGGED(0);
                        GList = TAGGED(0);
                        return FINISHED(taskData, E);
                    }
                }
                else /* Xt Event arrived */
                {
                    XEvent ev;
                    int dispatched;
                    
                    assert(FList == TAGGED(0));  
                    
                    XtAppNextEvent(DEREFDISPLAYHANDLE(dsHandle)->app_context,&ev);
                    
                    callbacks_enabled = True;
                    dispatched = XtDispatchEvent(&ev);
                    callbacks_enabled = False;
                    
                    if (!dispatched)
                    {
                        X_Window_Object *W = FindWindow(dsHandle,ev.xany.window);
                        
                        assert(FList == TAGGED(0) && GList == TAGGED(0)); 
                        
                        if (W && NONNIL(W->handler))
                        {
                            EventHandle E = CreateEvent(taskData, dsHandle,&ev,SAVE(W));
                            if (E) return E;
                        }
                    }
                    else if (! FList.IsTagged() || ! GList.IsTagged())
                    {
                        EventHandle E = CreateEvent(taskData, dsHandle,&ev,EmptyWindow(taskData, dsHandle,ev.xany.window));
                        if (E) return E;
                    }
                }
            }
        }
    }
}

static Handle GetInputFocus(TaskData *taskData, Handle dsHandle /* handle to (X_Display_Object *) */)
{
  Window focus;
  int revertTo;
  
  XGetInputFocus(DEREFDISPLAYHANDLE(dsHandle)->display,&focus,&revertTo);
  
  return CreatePair(taskData, EmptyWindow(taskData, dsHandle,focus),Make_arbitrary_precision(taskData, revertTo));
}

static void SetSelectionOwner
(
  Handle   dsHandle, /* handle to (X_Display_Object *) */
  unsigned selection,
  Window   owner,
  unsigned time
)
{
  Window old = XGetSelectionOwner(DEREFDISPLAYHANDLE(dsHandle)->display,selection);
  
  if (old != owner)
  {
    /* SelectionClear is only sent by the server when the ownership of a */
    /* selection passes from one client to another.  We want every ML    */
    /* window to behave like a separate client, so when the ownership of */
    /* a selection passes from one ML window to another we have to send  */
    /* the SelectionClear ourselves.                                     */
  
    X_Window_Object *W = FindWindow(dsHandle,old);
    
    if (W && NONNIL(W->handler))   /* this clients window */
    {
      XEvent event; /* was XSelectionClearEvent SPF 6/1/94 */
      
      event.xselectionclear.type       = SelectionClear;
      event.xselectionclear.serial     = 0;
      event.xselectionclear.send_event = True;
      event.xselectionclear.display    = DEREFDISPLAYHANDLE(dsHandle)->display;
      event.xselectionclear.window     = old;
      event.xselectionclear.selection  = selection;
      event.xselectionclear.time       = time;
      
      XSendEvent(DEREFDISPLAYHANDLE(dsHandle)->display,old,True,0,&event);
    }
  }
  
  XSetSelectionOwner(DEREFDISPLAYHANDLE(dsHandle)->display,selection,owner,time);
}

static void SendSelectionNotify
(
  Display *d,
  unsigned selection,
  unsigned target,
  unsigned property,
  Window   requestor,
  unsigned time
)
{
  XEvent event; /* was XSelectionEvent SPF 6/1/94 */
  
  event.xselection.type       = SelectionNotify;
  event.xselection.serial     = 0;
  event.xselection.send_event = True;
  event.xselection.display    = d;
  event.xselection.requestor  = requestor;
  event.xselection.selection  = selection;
  event.xselection.target     = target;
  event.xselection.property   = property;
  event.xselection.time       = time;
  
  XSendEvent(d,requestor,True,0,&event);
}

static Handle InternAtom
(
  TaskData *taskData,
  Display *d,
  PolyStringObject  *string,
  Bool     only_if_exists
)
{
  char name[500];

  Poly_string_to_C(string,name,sizeof(name));

  return Make_arbitrary_precision(taskData, XInternAtom(d,name,only_if_exists));
}

static Handle GetAtomName(TaskData *taskData, Display *d, unsigned atom)
{
  Handle s;
  
  char *name = XGetAtomName(d,atom);
  
  if (name == NULL) RaiseXWindows(taskData, "XGetAtomName failed");
  
  s = Make_string(name);
  
  XFree((char *)name);
  
  return s;
}

/* The order of these depends on the XCharStruct datatype */
typedef struct
{
    PolyWord width;      /* ML int */
    PolyWord ascent;     /* ML int */
    PolyWord descent;    /* ML int */
    PolyWord lbearing;   /* ML int */
    PolyWord rbearing;   /* ML int */
    PolyWord attributes; /* ML int */
} MLXCharStruct;

static Handle CreateCharStruct(TaskData *taskData, void *v)
{
    XCharStruct *cs = (XCharStruct *)v;
  Handle dataHandle = alloc_and_save(taskData, SIZEOF(MLXCharStruct), F_MUTABLE_BIT);
  
#define data ((MLXCharStruct *)DEREFHANDLE(dataHandle))
  data->width      = DEREFWORD(Make_int(cs->width));
  data->ascent     = DEREFWORD(Make_int(cs->ascent));
  data->descent    = DEREFWORD(Make_int(cs->descent));
  data->lbearing   = DEREFWORD(Make_int(cs->lbearing));
  data->rbearing   = DEREFWORD(Make_int(cs->rbearing));
  data->attributes = DEREFWORD(Make_arbitrary_precision(taskData, cs->attributes));
#undef data
  
  return FINISHED(taskData, dataHandle);
}

/* The order of these depends on the XFontStruct datatype */
typedef struct
{
    X_Font_Object  *font_object;
    PolyWord       ascent;        /* ML int */
    PolyWord       descent;       /* ML int */
    PolyWord       maxChar;       /* ML int */
    PolyWord       minChar;       /* ML int */
    PolyWord       perChar;       /* ML XCharStruct list */
    PolyWord       maxByte1;      /* ML int */
    PolyWord       minByte1;      /* ML int */
    PolyWord       direction;     /* (short ML int) FontLeftToRight | FontRightToLeft */
    MLXCharStruct  *maxBounds;
    MLXCharStruct  *minBounds;
    PolyWord       defaultChar;   /* ML int */
    PolyWord       allCharsExist; /* ML bool */
} MLXFontStruct;

static Handle CreateFontStruct
(
  TaskData *taskData,
  void *v,
  Handle       dsHandle /* Handle to (X_Display_Object *) */
)
{
  XFontStruct *fs = (XFontStruct *)v;
  Handle dataHandle = alloc_and_save(taskData, SIZEOF(MLXFontStruct), F_MUTABLE_BIT);

  int n = fs->max_char_or_byte2 - fs->min_char_or_byte2 + 1;
  
  if (fs->per_char == 0) n = 0;

#define data ((MLXFontStruct *)DEREFHANDLE(dataHandle))
  data->font_object   = (X_Font_Object *)DEREFHANDLE(EmptyFont(taskData, dsHandle,fs->fid,fs));
  data->ascent        = DEREFWORD(Make_int(fs->ascent));
  data->descent       = DEREFWORD(Make_int(fs->descent));
  data->maxChar       = DEREFWORD(Make_arbitrary_precision(taskData, fs->max_char_or_byte2));
  data->minChar       = DEREFWORD(Make_arbitrary_precision(taskData, fs->min_char_or_byte2));
  data->perChar       = DEREFHANDLE(CreateList4(taskData,n,fs->per_char,sizeof(XCharStruct),CreateCharStruct));
  data->maxByte1      = DEREFWORD(Make_arbitrary_precision(taskData, fs->max_byte1));
  data->minByte1      = DEREFWORD(Make_arbitrary_precision(taskData, fs->min_byte1));
  data->direction     = DEREFWORD(Make_arbitrary_precision(taskData, (fs->direction == FontLeftToRight) ? 1 : 2));
  data->maxBounds     = (MLXCharStruct *)DEREFHANDLE(CreateCharStruct(taskData, &fs->max_bounds));
  data->minBounds     = (MLXCharStruct *)DEREFHANDLE(CreateCharStruct(taskData, &fs->min_bounds));
  data->defaultChar   = DEREFWORD(Make_arbitrary_precision(taskData, fs->default_char));
  data->allCharsExist = DEREFWORD(Make_bool(fs->all_chars_exist));
#undef data
  
  return FINISHED(taskData, dataHandle);
}

static XFontStruct *GetFS(TaskData *taskData, X_Font_Object *P)
{
  
  assert(UNTAGGED(P->type) == X_Font);

  if (*(P->fs) == NULL) RaiseXWindows(taskData, "Not a real XFontStruct");

  CheckExists((X_Object *)P,font);

  return *(P->fs);
}

static XFontStruct *GetFontStruct(TaskData *taskData,PolyWord p)
{
    MLXFontStruct *P = (MLXFontStruct *)p.AsObjPtr();
    return GetFS(taskData,P->font_object);
}

static Handle CreateString(TaskData *taskData, void *s)
{
    return Make_string(*(char **)s);
}

static Handle GetFontPath(TaskData *taskData, Display *d)
{
  Handle list;
  char **names;
  int count;

  names = XGetFontPath(d,&count);

  if (names == 0) RaiseXWindows(taskData, "XGetFontPath failed");

  list = CreateList4(taskData,count,names,sizeof(char *),CreateString);
  
  XFreeFontNames(names);
  
  return list;
}

static void FreeStrings(char **s, int n)
{
  while(n--) free(*s++);
  return;
}

static void SetFontPath(TaskData *taskData, Display *d, Handle list)
{
  if (NONNIL(DEREFWORD(list)))
  {
    unsigned   N = ListLength(DEREFWORD(list));
    char **D = (char **) alloca(N * sizeof(char *));
  
    GetList4(taskData, DEREFWORD(list),D,sizeof(char *),CopyString);
    
    XSetFontPath(d,D,N);
    
    FreeStrings(D,N);
  }
  return;
}

static Handle ListFonts(TaskData *taskData,Display *d, PolyStringObject *string, unsigned maxnames)
{
  char name[500];
  Handle list; 
  char **names;
  int count;

  Poly_string_to_C(string,name,sizeof(name));

  names = XListFonts(d,name,maxnames,&count);

  if (names == 0) RaiseXWindows(taskData, "XListFonts failed");

  list = CreateList4(taskData,count,names,sizeof(char *),CreateString);
  
  XFreeFontNames(names);
  
  return list;
}

static Handle ListFontsWithInfo
(
  TaskData *taskData,
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  PolyStringObject  *string,
  unsigned maxnames
)
{
  char name[500];
  char **names;
  int count;
  XFontStruct *info;
  Handle pair;

  Poly_string_to_C(string,name,sizeof(name));

  names = XListFontsWithInfo(DEREFDISPLAYHANDLE(dsHandle)->display,name,maxnames,&count,&info);

  if (names == 0) RaiseXWindows(taskData, "XListFontsWithInfo failed");

  pair = CreatePair(taskData, CreateList4(taskData,count,names,sizeof(char *),CreateString),
                    CreateList5(taskData,count,info,sizeof(XFontStruct),CreateFontStruct,dsHandle));
  
  XFree((char *)info);
  XFreeFontNames(names);
  
  return pair;
}

static Handle LoadFont
(
  TaskData *taskData,
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  PolyStringObject *string
)
{
  char name[500]; Font font;

  Poly_string_to_C(string,name,sizeof(name));

  font = XLoadFont(DEREFDISPLAYHANDLE(dsHandle)->display,name);

  if (font == 0) RaiseXWindows(taskData, "XLoadFont failed");

  return EmptyFont(taskData, dsHandle,font,(XFontStruct *)NULL);
}

static Handle LoadQueryFont
(
  TaskData *taskData,
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  PolyStringObject *string
)
{
  char name[500]; XFontStruct *fs;

  Poly_string_to_C(string,name,sizeof(name));

  fs = XLoadQueryFont(DEREFDISPLAYHANDLE(dsHandle)->display,name);

  if (fs == 0) RaiseXWindows(taskData, "XLoadQueryFont failed");
  
  return CreateFontStruct(taskData,fs,dsHandle);
}

static Handle QueryFont
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Font   font
)
{
  XFontStruct *fs;

  fs = XQueryFont(DEREFDISPLAYHANDLE(dsHandle)->display,font);

  if (fs == 0) RaiseXWindows(taskData, "XQueryFont failed");
  
  return CreateFontStruct(taskData,fs,dsHandle);
}

static Handle TextExtents(TaskData *taskData, XFontStruct *fs, PolyStringObject *s)
{
  Handle dataHandle = alloc_and_save(taskData, 4, F_MUTABLE_BIT);
  
  int direction,ascent,descent; XCharStruct overall;

  XTextExtents(fs,s->chars,s->length,&direction,&ascent,&descent,&overall);

#define data DEREFHANDLE(dataHandle)  
  data->Set(0, DEREFWORD(Make_arbitrary_precision(taskData, (direction == FontLeftToRight) ? 1 : 2)));
  data->Set(1, DEREFWORD(Make_int(ascent)));
  data->Set(2, DEREFWORD(Make_int(descent)));
  data->Set(3, DEREFWORD(CreateCharStruct(taskData, &overall)));
#undef data
  
  return FINISHED(taskData, dataHandle);
}

static Handle TextExtents16(TaskData *taskData, XFontStruct *fs, Handle list)
{
  Handle dataHandle = alloc_and_save(taskData, 4, F_MUTABLE_BIT);
  
  int direction,ascent,descent; XCharStruct overall;
  
  unsigned     N = ListLength(DEREFWORD(list));
  XChar2b *L = (XChar2b *) alloca(N * sizeof(XChar2b));

  GetList4(taskData,DEREFWORD(list),L,sizeof(XChar2b),GetChar2);

  XTextExtents16(fs,L,N,&direction,&ascent,&descent,&overall);
  
#define data DEREFHANDLE(dataHandle)  
  data->Set(0, DEREFWORD(Make_arbitrary_precision(taskData, (direction == FontLeftToRight) ? 1 : 2)));
  data->Set(1, DEREFWORD(Make_int(ascent)));
  data->Set(2, DEREFWORD(Make_int(descent)));
  data->Set(3, DEREFWORD(CreateCharStruct(taskData, &overall)));
#undef data
  
  return FINISHED(taskData, dataHandle);
}

static Handle TextWidth(TaskData *taskData, XFontStruct *fs, PolyStringObject *s)
{
  if (fs->per_char == 0) return Make_int(s->length * fs->max_bounds.width);
  
  return Make_int(XTextWidth(fs,s->chars,s->length));
}

static Handle TextWidth16(TaskData *taskData, XFontStruct *fs, Handle list)
{
  unsigned     N = ListLength(DEREFWORD(list));
  XChar2b *L = (XChar2b *) alloca(N * sizeof(XChar2b));

  GetList4(taskData, DEREFWORD(list),L,sizeof(XChar2b),GetChar2);

  return Make_int(XTextWidth16(fs,L,N));
}

static Handle GetTextProperty(TaskData *taskData, Display *d, Window w, unsigned property)
{
  XTextProperty T;
  Handle tuple;
  
  int s = XGetTextProperty(d,w,&T,property);
  
  if (s == 0) RaiseXWindows(taskData, "XGetTextProperty failed");
  
  tuple = alloc_and_save(taskData, 4, F_MUTABLE_BIT);

#define data DEREFHANDLE(tuple)
  data->Set(0, C_string_to_Poly(taskData, (char *)T.value,T.nitems * T.format / 8));
  data->Set(1, DEREFWORD(Make_arbitrary_precision(taskData, T.encoding)));
  data->Set(2, DEREFWORD(Make_int(T.format)));
  data->Set(3, DEREFWORD(Make_arbitrary_precision(taskData, T.nitems)));
#undef data
  
  return FINISHED(taskData, tuple);
}

static void GetXWMHints(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    PolyObject *P = p.AsObjPtr();
    XWMHints *H = (XWMHints *)v;
    H->input         = get_C_ulong(taskData, P->Get(0));
    H->initial_state = get_C_ulong(taskData, P->Get(1));
    H->icon_pixmap   = GetPixmap(taskData, (X_Object *)P->Get(2).AsObjPtr());
    H->icon_window   = GetWindow(taskData, (X_Object *)P->Get(3).AsObjPtr());
    H->icon_x        = GetPointX(taskData, P->Get(4));
    H->icon_y        = GetPointY(taskData, P->Get(4));
    H->icon_mask     = GetPixmap(taskData, (X_Object *)P->Get(5).AsObjPtr());
    H->flags         = get_C_ulong(taskData, P->Get(6));
    H->window_group  = 0;
}



typedef struct
{
    PolyWord    x0;
    PolyWord    x1;
    PolyWord    x2;
    PolyWord    x3;
    PolyWord    x4;
    PolyWord    x5;  /* pair of points */
    PolyWord    x6;
    PolyWord    x7;
    PolyWord    x8;
} MLXWMSizeHintsTuple;

static void GetXWMSizeHints(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    MLXWMSizeHintsTuple *P = (MLXWMSizeHintsTuple *)p.AsObjPtr();
    XSizeHints *H = (XSizeHints *)v;
    CheckZeroRect(taskData, P->x1);
    CheckZeroRect(taskData, P->x2);
    CheckZeroRect(taskData, P->x3);
    CheckZeroRect(taskData, P->x4);
    CheckZeroRect(taskData, P->x6);

    H->x            = GetPointX(taskData, P->x0);
    H->y            = GetPointY(taskData, P->x0);
    H->width        = GetRectW(taskData, P->x1);
    H->height       = GetRectH(taskData, P->x1);
    H->min_width    = GetRectW(taskData, P->x2);
    H->min_height   = GetRectH(taskData, P->x2);
    H->max_width    = GetRectW(taskData, P->x3);
    H->max_height   = GetRectH(taskData, P->x3);
    H->width_inc    = GetRectW(taskData, P->x4);
    H->height_inc   = GetRectH(taskData, P->x4);
    H->min_aspect.x = GetPointX(taskData, FST(P->x5));
    H->min_aspect.y = GetPointY(taskData, FST(P->x5));
    H->max_aspect.x = GetPointX(taskData, SND(P->x5));
    H->max_aspect.y = GetPointY(taskData, SND(P->x5));
    H->base_width   = GetRectW(taskData, P->x6);
    H->base_height  = GetRectH(taskData, P->x6);
    H->win_gravity  = get_C_ulong(taskData, P -> x7);
    H->flags        = get_C_ulong(taskData, P -> x8);
}

static void GetIconSize(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    MLTriple *P = (MLTriple *)p.AsObjPtr();
    XIconSize *s = (XIconSize *)v;
    CheckZeroRect(taskData, FST(P));
    CheckZeroRect(taskData, SND(P));
    CheckZeroRect(taskData, THIRD(P));
    
    s->min_width = GetRectW(taskData, FST(P));
    s->min_height = GetRectH(taskData, FST(P));
    s->max_width = GetRectW(taskData, SND(P));
    s->max_height = GetRectH(taskData, SND(P));
    s->width_inc = GetRectW(taskData, THIRD(P));
    s->height_inc = GetRectH(taskData, THIRD(P));
}

static void GetSigned(TaskData *taskData, PolyWord p, void *i, unsigned)
{
    *(int*)i = get_C_long(taskData, p);
}

static void GetPixmaps(TaskData *taskData, PolyWord pp, void *m, unsigned)
{
    X_Object *p = (X_Object *)pp.AsObjPtr();
    *(Pixmap *)m = GetPixmap(taskData, p);
}

static void GetColormaps(TaskData *taskData, PolyWord pp, void *v, unsigned)
{
    X_Object *p = (X_Object *)pp.AsObjPtr();
    *(Colormap *)v = GetColormap(taskData, p);
}

static void GetCursors(TaskData *taskData, PolyWord pp, void *c, unsigned)
{
    X_Object *p = (X_Object *)pp.AsObjPtr();
    *(Cursor *)c = GetCursor(taskData, p);
}

static void GetDrawables(TaskData *taskData, PolyWord pp, void *d, unsigned)
{
    X_Object *p = (X_Object *)pp.AsObjPtr();
    *(Drawable *)d = GetDrawable(taskData, p);
}

static void GetFonts(TaskData *taskData, PolyWord pp, void *f, unsigned)
{
    X_Object *p = (X_Object *)pp.AsObjPtr();
    *(Font *)f = GetFont(taskData, p);
}

static void GetVisualIds(TaskData *taskData, PolyWord pp, void *u, unsigned)
{
    X_Object *p = (X_Object *)pp.AsObjPtr();
    *(unsigned *)u = GetVisual(taskData, p)->visualid;
}

static void SetProperty
(
  TaskData *taskData,
  Display *d,
  Window   w,
  unsigned property,
  unsigned target,
  Handle   list,
  unsigned encoding
)
{
    unsigned format; 
    unsigned bytes;
    uchar *value;
    
    /* SPF 7/7/94 - XA_STRING pulled out as special case; this enables */
    /* gcc to understand the previously data-dependant control flow.   */
    if (encoding == XA_STRING) 
    {
        PolyStringObject *s = GetString (DEREFHANDLE(list));
        
        format = 8;
        bytes  = s->length;
        value  = (uchar *) s->chars;
    }
    
    else
    {
        unsigned length = ListLength(DEREFWORD(list));
        unsigned size;
        GetFunc get;
        
        switch(encoding)
        {
        case XA_ATOM:          size = sizeof(unsigned);          get = GetUnsigned;         format = 32; break;
        case XA_BITMAP:        size = sizeof(Pixmap);            get = GetPixmaps;          format = 32; break;
        case XA_COLORMAP:      size = sizeof(Colormap);          get = GetColormaps;        format = 32; break;
        case XA_CURSOR:        size = sizeof(Cursor);            get = GetCursors;          format = 32; break;
        case XA_DRAWABLE:      size = sizeof(Drawable);          get = GetDrawables;        format = 32; break;
        case XA_FONT:          size = sizeof(Font);              get = GetFonts;            format = 32; break;
        case XA_PIXMAP:        size = sizeof(Pixmap);            get = GetPixmaps;          format = 32; break;
        case XA_VISUALID:      size = sizeof(unsigned);          get = GetVisualIds;        format = 32; break;
        case XA_CARDINAL:      size = sizeof(unsigned);          get = GetUnsigned;         format = 32; break;
        case XA_INTEGER:       size = sizeof(int);               get = GetSigned;           format = 32; break;
        case XA_WINDOW:        size = sizeof(Window);            get = GetWindows;          format = 32; break;
        case XA_ARC:           size = sizeof(XArc);              get = GetArcs;             format = 16; break;
        case XA_POINT:         size = sizeof(XPoint);            get = GetPoints;           format = 16; break;
        case XA_RECTANGLE:     size = sizeof(XRectangle);        get = GetRects;            format = 16; break;
        case XA_RGB_COLOR_MAP: size = sizeof(XStandardColormap); get = GetStandardColormap; format = 32; break;
        case XA_WM_HINTS:      size = sizeof(XWMHints);          get = GetXWMHints;         format = 32; break;
        case XA_WM_SIZE_HINTS: size = sizeof(XSizeHints);        get = GetXWMSizeHints;     format = 32; break;
        case XA_WM_ICON_SIZE:  size = sizeof(XIconSize);         get = GetIconSize;         format = 32; break;
        default: Crash ("Bad property type %x",encoding); /*NOTREACHED*/
        }
        
        bytes  = length * size;
        value  = (uchar *) alloca(bytes);
        GetList4(taskData, DEREFWORD(list),value,(int)size,get);
    }
    
    {
        XTextProperty T;
        
        T.value    = value;
        T.encoding = target;
        T.format   = format;
        T.nitems   = (bytes * 8) / format;
        
        XSetTextProperty(d,w,&T,property);
    }
}

static Handle GetWMHints
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window w
)
{
  Handle tuple = alloc_and_save(taskData, 7, F_MUTABLE_BIT);
  
  XWMHints *H = XGetWMHints(DEREFDISPLAYHANDLE(dsHandle)->display,w);
  
  if (H)
  {
  
#define data DEREFHANDLE(tuple)  
    data->Set(0, DEREFWORD(Make_arbitrary_precision(taskData, H->input)));
    data->Set(1, DEREFWORD(Make_arbitrary_precision(taskData, H->initial_state)));
    data->Set(2, DEREFWORD(EmptyPixmap(taskData, dsHandle,H->icon_pixmap)));
    data->Set(3, DEREFWORD(EmptyWindow(taskData, dsHandle,H->icon_window)));
    data->Set(4, DEREFWORD(CreatePoint(taskData, H->icon_x,H->icon_y)));
    data->Set(5, DEREFWORD(EmptyPixmap(taskData, dsHandle,H->icon_mask)));
    data->Set(6, DEREFWORD(Make_arbitrary_precision(taskData, H->flags)));
#undef data

    XFree((char *)H);
  }
  
  /* else what (?) */
  
  return FINISHED(taskData, tuple);
}

static Handle GetWMSizeHints
(
  TaskData *taskData,
  Display *d,
  Window   w,
  unsigned property
)
{
  XSizeHints H;
  long supplied; /* was unsigned SPF 6/1/94 */
  
  Handle tuple = alloc_and_save(taskData, 9, F_MUTABLE_BIT);
  
  int s = XGetWMSizeHints(d,w,&H,&supplied,property);
  
  if (s)
  {
    Handle p1 = CreatePoint(taskData, H.min_aspect.x,H.min_aspect.y);
    Handle p2 = CreatePoint(taskData, H.max_aspect.x,H.max_aspect.y);

#define data DEREFHANDLE(tuple)   
    data->Set(0, DEREFWORD(CreatePoint(taskData, H.x,H.y)));
    data->Set(1, DEREFWORD(CreateArea(H.width,H.height)));
    data->Set(2, DEREFWORD(CreateArea(H.min_width,H.min_height)));
    data->Set(3, DEREFWORD(CreateArea(H.max_width,H.max_height)));
    data->Set(4, DEREFWORD(CreateArea(H.width_inc,H.height_inc)));
    data->Set(5, DEREFWORD(CreatePair(taskData, p1,p2)));
    data->Set(6, DEREFWORD(CreateArea(H.base_width,H.base_height)));
    data->Set(7, DEREFWORD(Make_arbitrary_precision(taskData, H.win_gravity)));
    data->Set(8, DEREFWORD(Make_arbitrary_precision(taskData, H.flags)));
#undef data
  }
  
  /* else (?) */
  
  return FINISHED(taskData, tuple);
}

#if 0
typedef struct
{
MLPair       *x0; /* pair of points */
MLXRectangle *x1;
PolyWord     x2; /* ML int */
} MLWMGeometryTriple;
#endif

static Handle WMGeometry
(
  TaskData *taskData,
  Handle        dsHandle, /* Handle to (X_Display_Object *) */
  PolyStringObject        *user,
  PolyStringObject        *def,
  unsigned      borderWidth,
  PolyWord      P
)
{
  XSizeHints H; int x,y,width,height,gravity,mask;
  
  char userGeometry[500],defaultGeometry[500];
  
  GetXWMSizeHints(taskData, P, &H, 0);
  
  Poly_string_to_C(user,userGeometry   ,sizeof(userGeometry));
  Poly_string_to_C(def ,defaultGeometry,sizeof(defaultGeometry));

  mask = XWMGeometry(DEREFDISPLAYHANDLE(dsHandle)->display,
                       DEREFDISPLAYHANDLE(dsHandle)->screen,
                       userGeometry,
                       defaultGeometry,
                       borderWidth,
                       &H,&x,&y,&width,&height,&gravity);
  
  return CreateTriple(taskData, CreatePoint(taskData, x,y),CreateArea(width,height),Make_arbitrary_precision(taskData, gravity));
}

static Handle CreateIconSize(TaskData *taskData, void *v)
{
    XIconSize *s = (XIconSize *)v;
    return CreateTriple(taskData, CreateArea(s->min_width,s->min_height),
                        CreateArea(s->max_width,s->max_height),
                        CreateArea(s->width_inc,s->height_inc));
}

static Handle GetIconSizes(TaskData *taskData, Display *d, Window w)
{
    XIconSize *sizes; 
    int count;
    
    int s = XGetIconSizes(d,w,&sizes,&count);
    
    if (s)
    {
        Handle list = CreateList4(taskData,count,sizes,sizeof(XIconSize),CreateIconSize);
        
        XFree((char *)sizes);
        
        return list;
    }
    
    return SAVE(ListNull);
}

static Handle GetTransientForHint
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window w
)
{
  Window p;
  
  int s = XGetTransientForHint(DEREFDISPLAYHANDLE(dsHandle)->display,w,&p);
  
  if (s == 0) RaiseXWindows(taskData, "XGetTransientForHint failed");
  
  return EmptyWindow(taskData, dsHandle,p);
}

static Handle GetWMColormapWindows
(
  TaskData *taskData,
  Handle dsHandle, /* Handle to (X_Display_Object *) */
  Window parent
)
{
  Window *windows;
  int count;
  
  int s = XGetWMColormapWindows(DEREFDISPLAYHANDLE(dsHandle)->display,parent,&windows,&count);
  
  if (s)
  {
    Handle list = CreateList5(taskData,count,windows,sizeof(Window),CreateDrawable,dsHandle);

    XFree((char *)windows);
    
    return list;
  }
  
  return SAVE(ListNull);
}


static Handle GetRGBColormaps
(
  TaskData *taskData,
  Handle   dsHandle, /* Handle to (X_Display_Object *) */
  Window   w,
  unsigned property
)
{
  XStandardColormap *maps;
  int count;
  
  int s = XGetRGBColormaps(DEREFDISPLAYHANDLE(dsHandle)->display,w,&maps,&count,property);
  
  if (s)
  {
    Handle list = CreateList5(taskData,count,maps,sizeof(XStandardColormap),CreateStandardColormap,dsHandle);

    XFree((char *)maps);
    
    return list;
  }
  
  return SAVE(ListNull);
}

static Handle GetID(TaskData *taskData, X_Object *P)
{
    switch(UNTAGGED(P->type))
    {
    case X_GC:       return Make_arbitrary_precision(taskData, GetGC(taskData, P)->gid);           /* GCID       */
    case X_Font:     return Make_arbitrary_precision(taskData, GetFont(taskData, P));              /* FontID     */
    case X_Cursor:   return Make_arbitrary_precision(taskData, GetCursor(taskData, P));            /* CursorId   */
    case X_Window:   return Make_arbitrary_precision(taskData, GetWindow(taskData, P));            /* DrawableID */
    case X_Pixmap:   return Make_arbitrary_precision(taskData, GetPixmap(taskData, P));            /* DrawableID */
    case X_Colormap: return Make_arbitrary_precision(taskData, GetColormap(taskData, P));          /* ColormapID */
    case X_Visual:   return Make_arbitrary_precision(taskData, GetVisual(taskData, P)->visualid);  /* VisualID   */
    case X_Widget:   return Make_arbitrary_precision(taskData, (unsigned long)GetNWidget(taskData, P)); /* Widget -- SAFE(?) */
    default:         Crash ("Bad X_Object type (%d) in GetID",UNTAGGED(P->type)) /*NOTREACHED*/;
    }
}

static Handle OpenDisplay(TaskData *taskData, PolyStringObject *string)
{
    char               name[500];
    Display           *display;
    Handle dsHandle /* Handle to (X_Display_Object *) */;
    
    Poly_string_to_C(string,name,sizeof(name));
    
    display = XOpenDisplay(name);
    
    if (display == 0) RaiseXWindows(taskData, "XOpenDisplay failed");
    
    /* I don't think this is needed.  DCJM 26/5/2000. */
    /* add_file_descr(display->fd); */
    
    dsHandle = alloc_and_save(taskData, SIZEOF(X_Display_Object), F_MUTABLE_BIT|F_BYTE_OBJ);
    
    debug1 ("%s display opened\n",DisplayString(display));
    
    debug1 ("%x display fd\n",display->fd);
    
#define ds DEREFDISPLAYHANDLE(dsHandle)
    /* Ok to store C values because this is a byte object */
    ds->type        = TAGGED(X_Display);
    ds->display     = display;
    ds->screen      = DefaultScreen(display);
    ds->app_context = 0;
#undef ds
    
    return AddXObject(FINISHED(taskData, dsHandle));
}

/* indirection removed SPF 11/11/93 */
static XmFontList GetXmFontList(PolyWord p /* NOT a handle */)
{
    if (NONNIL(p))
    {
        char       charset[500];
        XmFontList L;
        MLPair    *q = (MLPair *)(((ML_Cons_Cell*)p.AsObjPtr())->h.AsObjPtr());
        
        Poly_string_to_C(SND(q),charset,sizeof(charset));
        L = XmFontListCreate((XFontStruct *)FST(q).AsObjPtr(),charset); /* cast added SPF 6/1/94 */
        
        p = ((ML_Cons_Cell*)p.AsObjPtr())->t;
        
        while(NONNIL(p))
        {
            q = (MLPair *)(((ML_Cons_Cell*)p.AsObjPtr())->h.AsObjPtr());
            
            Poly_string_to_C(SND(q),charset,sizeof(charset));
            L = XmFontListAdd(L,(XFontStruct *)FST(q).AsObjPtr(),charset); /* cast added SPF 6/1/94 */
            
            p = ((ML_Cons_Cell*)p.AsObjPtr())->t;
        }
        
        return L;
    }
    
    return 0;
}

/*
      datatype CType = CAccelerators  of XtAccelerators
                     | CBool          of bool
                     | CColormap      of Colormap
                     | CCursor        of Cursor
                     | CDimension     of int
                     | CFontList      of (XFontStruct * string) list
                     | CInt           of int
                     | CIntTable      of int list
                     | CKeySym        of int
                     | CPixmap        of Drawable
                     | CPosition      of int
                     | CString        of string
                     | CStringTable   of string list
                     | CTrans         of XtTranslations
                     | CUnsignedChar  of int
                     | CUnsignedTable of int list
                     | CVisual        of Visual
                     | CWidget        of Widget
                     | CWidgetList    of Widget list
                     | CXmString      of XmString
                     | CXmStringTable of XmString list;
*/

#define CAccelerators  1
#define CBool          2
#define CColormap      3
#define CCursor        4
#define CDimension     5
#define CFontList      6
#define CInt           7
#define CIntTable      8
#define CKeySym        9
#define CPixmap        10
#define CPosition      11
#define CString        12
#define CStringTable   13
#define CTrans         14
#define CUnsignedChar  15
#define CUnsignedTable 16
#define CVisual        17
#define CWidget        18
#define CWidgetList    19
#define CXmString      20
#define CXmStringTable 21

typedef struct
{
    unsigned  tag;
    unsigned  N;
    char *name;
    union
    {
        XtAccelerators acc;
        Boolean        boolean;
        Colormap       cmap;
        Cursor         cursor;
        Dimension      dim;
        XmFontList     F;
        int            i;
        int           *I;
        KeySym         keysym;
        Pixmap         pixmap;
        Position       posn;
        char          *string;
        char         **S;
        XtTranslations trans;
        uchar          u;
        uchar         *U;
        Visual        *visual;
        Widget         widget;
        WidgetList     W;
        XmString       xmString;
        XmString      *X;
    } u;
} ArgType;


static void GetXmString(TaskData *taskData, PolyWord w, void *v, unsigned )
{
    XmString *p = (XmString *)v;
    char *s;
    CopyString(taskData, w, &s, 0);
    *p = XmStringCreateLtoR(s, (char *)XmSTRING_DEFAULT_CHARSET);
    free(s);
}

static void GetXmStrings(TaskData *taskData, PolyWord list, ArgType *T)
{
    T->N   = 0;
    T->u.X = 0;
    
    if (NONNIL(list))
    {
        T->N   = ListLength(list);
        T->u.X = (XmString *) malloc(T->N * sizeof(XmString));
        
        GetList4(taskData, list,T->u.X,sizeof(XmString),GetXmString);
    }
}

static void GetStrings(TaskData *taskData, PolyWord list, ArgType *T)
{
    T->N   = 0;
    T->u.S = 0;
    
    if (NONNIL(list))
    {
        T->N   = ListLength(list);
        T->u.S = (char **) malloc(T->N * sizeof(char *));
        
        GetList4(taskData, list,T->u.S,sizeof(char *),CopyString);
    }
}

static void FreeXmStrings(ArgType *T)
{
    for(unsigned i = 0; i < T->N; i++) XmStringFree (T->u.X[i]);
    
    free(T->u.X);
}

static void GetITable(TaskData *taskData, PolyWord list, ArgType *T)
{
    T->N   = 0;
    T->u.I = 0;
    
    if (NONNIL(list))
    {
        T->N   = ListLength(list);
        T->u.I = (int *) malloc(T->N * sizeof(int));
        
        GetList4(taskData, list,T->u.I,sizeof(int),GetUnsigned);
    }
}

static void GetUTable(TaskData *taskData, PolyWord list, ArgType *T)
{
    T->N   = 0;
    T->u.U = 0;
    
    if (NONNIL(list))
    {
        T->N   = ListLength(list);
        T->u.U = (uchar *)malloc(T->N * sizeof(uchar));
        
        GetList4(taskData, list,T->u.U,sizeof(uchar),GetUChars);
    }
}

/*
    case CIntTable:      GetITable   ((ML_Cons_Cell *)v,T); break;
    case CUnsignedTable: GetUTable   ((ML_Cons_Cell *)v,T); break;
    case CString:        CopyString  (v,&T->u.string); break;
    case CStringTable:   GetStrings  ((ML_Cons_Cell *)v,T); break;
    case CXmString:      GetXmString (v,&T->u.xmString); break;
    case CXmStringTable: GetXmStrings((ML_Cons_Cell *)v,T); break;
*/

static void FreeArgs(ArgType *T, unsigned N)
{
  while(N--)
  {
    free(T->name);
    
    switch(T->tag)
    {
      case CAccelerators:  break;
      case CBool:          break;
      case CColormap:      break;
      case CCursor:        break;
      case CDimension:     break;
      case CFontList:      XmFontListFree(T->u.F); break;
      case CInt:           break;
      case CIntTable:      break;
      case CKeySym:        break;
      case CPixmap:        break;
      case CPosition:      break;
      case CString:        XtFree(T->u.string); break;
      case CStringTable:   FreeStrings(T->u.S,T->N); free(T->u.S); break;
      case CTrans:         break;
      case CUnsignedChar:  break;
      case CUnsignedTable: break;
      case CVisual:        break;
      case CWidget:        break;
      case CWidgetList:    break;
      case CXmString:      XmStringFree (T->u.xmString); break;
      case CXmStringTable: FreeXmStrings(T); break;
      
      default: Crash ("Bad arg type %x",T->tag);
    }
    
    T++;
  }
}

/* 
type Arg sharing type Arg = exn;
val Exn: Arg -> Exn = Cast;
val Arg: Exn -> Arg = Cast;
datatype Exn = EXN of unit ref * string * unit;
*/

/* (string,(v,tag)) */
static void SetArgTypeP(TaskData *taskData, PolyWord fst, PolyWord snd, ArgType *T)
{
  PolyWord v = FST(snd);
  
  T->tag = UNTAGGED(SND(snd));
  T->N   = 0;
  T->u.i = 0;

  CopyString(taskData, fst, &T->name, 0);
  
  switch(T->tag)
  {
    case CAccelerators:  T->u.acc    = GetAcc       (taskData, (X_Object *)v.AsObjPtr()); break;
    case CBool:          T->u.boolean   = get_C_ulong  (taskData, v); break;
    case CColormap:      T->u.cmap   = GetColormap  (taskData, (X_Object *)v.AsObjPtr()); break;
    case CCursor:        T->u.cursor = GetCursor    (taskData, (X_Object *)v.AsObjPtr()); break;
    case CDimension:     T->u.dim    = get_C_ushort (taskData, v); break;
    case CFontList:      T->u.F      = GetXmFontList(v); break;
    case CInt:           T->u.i      = get_C_long   (taskData, v); break;
    case CKeySym:        T->u.keysym = get_C_ulong  (taskData, v); break;
    case CPixmap:        T->u.pixmap = GetPixmap    (taskData, (X_Object *)v.AsObjPtr()); break;
    case CPosition:      T->u.posn   = get_C_short  (taskData, v); break;
    case CTrans:         T->u.trans  = GetTrans     (taskData, (X_Object *)v.AsObjPtr()); break;
    case CUnsignedChar:  T->u.u      = get_C_uchar  (taskData, v); break;
    case CVisual:        T->u.visual = GetVisual    (taskData, (X_Object *)v.AsObjPtr()); break;
    case CWidget:        T->u.widget = GetNWidget   (taskData, (X_Object *)v.AsObjPtr()); break;

    /* The following types allocate memory, but only in the C heap */
    
    case CIntTable:      GetITable   (taskData, v,T); break;
    case CUnsignedTable: GetUTable   (taskData, v,T); break;
    case CString:        CopyString  (taskData, v, &T->u.string, 0); break;
    case CStringTable:   GetStrings  (taskData, v,T); break;
    case CXmString:      GetXmString (taskData, v, &T->u.xmString, 0); break;
    case CXmStringTable: GetXmStrings(taskData, v,T); break;
    
    default: Crash ("Bad arg type %x",T->tag);
  }
}

static void SetArgType(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    ArgType *T = (ArgType *)v;
    SetArgTypeP(taskData, FST(p), SND(p), T);
}

static void SetArgs(Arg *A, ArgType *T, unsigned N)
{
  while(N--)
  {
    A->name = T->name;
  
    switch(T->tag)
    {
      case CAccelerators:  A->value = (XtArgVal) T->u.acc; break;
      case CBool:          A->value = (XtArgVal) T->u.boolean; break;
      case CColormap:      A->value = (XtArgVal) T->u.cmap; break;
      case CCursor:        A->value = (XtArgVal) T->u.cursor; break;
      case CDimension:     A->value = (XtArgVal) T->u.dim; break;
      case CFontList:      A->value = (XtArgVal) T->u.F; break;
      case CInt:           A->value = (XtArgVal) T->u.i; break;
      case CIntTable:      A->value = (XtArgVal) T->u.I; break;
      case CKeySym:        A->value = (XtArgVal) T->u.keysym; break;
      case CPixmap:        A->value = (XtArgVal) T->u.pixmap; break;
      case CPosition:      A->value = (XtArgVal) T->u.posn; break;
      case CString:        A->value = (XtArgVal) T->u.string; break;
      case CStringTable:   A->value = (XtArgVal) T->u.S; break;
      case CTrans:         A->value = (XtArgVal) T->u.trans; break;
      case CUnsignedChar:  A->value = (XtArgVal) T->u.u; break;
      case CUnsignedTable: A->value = (XtArgVal) T->u.U; break;
      case CVisual:        A->value = (XtArgVal) T->u.visual; break;
      case CWidget:        A->value = (XtArgVal) T->u.widget; break;
      case CXmString:      A->value = (XtArgVal) T->u.xmString; break;
      case CXmStringTable: A->value = (XtArgVal) T->u.X; break;
      
      default: Crash ("Bad arg type %x",T->tag);
    }
    
    A++;
    T++;
  }
}

/* add current callback to (pending?) FList */
static void RunWidgetCallback(Widget w, XtPointer closure, XtPointer call_data)
{
    C_List *C = (C_List *)closure;
    
    if (callbacks_enabled)
    {
        // Only synchronous callbacks are handled.
        TaskData *taskData = processes->GetTaskDataForThread();
        Handle tailHandle     = SAVE(FList);
        Handle widgetHandle   = SAVE(C->widget_object);
        Handle functionHandle = SAVE(C->function);
        Handle pairHandle     = CreatePair(taskData, widgetHandle,functionHandle);
        
        FList = DEREFWORD(CreatePair(taskData, pairHandle,tailHandle));
    }
#if 0
    else printf("Ignoring event for widget %p\n",C->widget_object);
#endif
}

static void SetCallbacks(TaskData *taskData, X_Widget_Object *W, PolyWord list, PolyWord initial)
{
    char name[100];
    Widget w = GetWidget(taskData, (X_Object *)W);
    
    assert(w != NULL); /* SPF */
    assert(w != (Widget)1); /* SPF */
    
    for(PolyWord pp = W->callbackList; NONNIL(pp); pp = ((ML_Cons_Cell*)pp.AsObjPtr())->t)
    {
        MLPair *q = (MLPair *)((ML_Cons_Cell*)pp.AsObjPtr())->h.AsObjPtr();
        
        Poly_string_to_C(FST(q),name,sizeof(name));
        
        if (strcmp(name,"messageCallback")  != 0
            && strcmp(name,XtNdestroyCallback) != 0)
        {
            XtRemoveAllCallbacks(w,name);
        }
    }
    
#if 0
    /* We no longer need the old callback data for this widget,
    assuming we've replaced all the callbacks. But what if
    we've only replaced some of them? It's probably better
    to allow this space leak that to delete vital callback data.
    I'll have to think about this hard sometime. (Of course, the
    user isn't supposed to call XtSetCallbacks more than once, in which
    case the problem doesn't even arise.) SPF 29/2/96 */
    PurgeCCallbacks(W,w);
#endif
    
    for(PolyWord p = list; NONNIL(p); p = ((ML_Cons_Cell*)p.AsObjPtr())->t)
    {
        C_List *C = (C_List *)malloc(sizeof(C_List));
        MLPair *q = (MLPair *)((ML_Cons_Cell*)p.AsObjPtr())->h.AsObjPtr();
        
        C->function      = SND(q).AsObjPtr();
        C->widget_object = W;
        C->next          = CList;
        
        debugCreateCallback(W,w,C);
        
        CList = C;
        
        Poly_string_to_C(FST(q),name,sizeof(name));
        
        if (strcmp(name,"messageCallback")  != 0
            && strcmp(name,XtNdestroyCallback) != 0)
        {
            XtAddCallback(w,name,RunWidgetCallback,C);
        }
    }
    
    W->state = initial;
    W->callbackList = list;
}

static void RunWidgetEventhandler (Widget w, XtPointer p, XEvent *ev, Boolean *c)
{
    C_List *C = (C_List *)p;
    if ( callbacks_enabled )
    {
        TaskData *taskData = processes->GetTaskDataForThread();
        Handle tailHandle     = SAVE(GList);
        Handle widgetHandle   = SAVE(C->widget_object);
        Handle functionHandle = SAVE(C->function);
        Handle pairHandle     = CreatePair(taskData, widgetHandle,functionHandle);
        
        GList = (ML_Cons_Cell *)DEREFHANDLE(CreatePair(taskData, pairHandle,tailHandle));
    }
}

static void AddEventhandler (
   TaskData *taskData, X_Widget_Object *W, EventMask EventM, Boolean nonmask, Handle p)
{
  Widget w = GetWidget(taskData, (X_Object *)W) ;
  C_List *C = (C_List *) malloc ( sizeof(C_List) ) ;
  /* Add the function to the callback list, so that it
     will not be G.C'ed away. */
  C->function = DEREFHANDLE(p);
  C->widget_object   = W ;
  C->next     = CList ;
      
  CList = C ;

  XtAddEventHandler (w, EventM, nonmask, RunWidgetEventhandler, C);
}

static Handle AppInitialise
(
 TaskData *taskData,
 PolyWord s1,
 PolyWord s2,
 PolyWord s3,
 Handle fallbackHead,
 Handle argHead
 )
{
    char               displayName[500];
    char               appName[500];
    char               appClass[500];
    XtAppContext       app_context;
    Display           *display;
    Widget             shell;
    Handle dsHandle /* Handle to (X_Display_Object *) */;
    int                argc = 0; /* an "int" for Solaris, but should be "unsigned" for SunOS */
    unsigned           F = ListLength(DEREFWORD(fallbackHead)) + 1;
    unsigned           N = ListLength(DEREFWORD(argHead));
    char             **S = (char   **) alloca(F * sizeof(char *));
    Arg               *R = (Arg     *) alloca(N * sizeof(Arg));
    ArgType           *T = (ArgType *) alloca(N * sizeof(ArgType));
    
    Poly_string_to_C(s1,displayName ,sizeof(displayName));
    Poly_string_to_C(s2,appName     ,sizeof(appName));
    Poly_string_to_C(s3,appClass    ,sizeof(appClass));
    
    app_context = XtCreateApplicationContext();
    
    GetList4(taskData, DEREFWORD(fallbackHead),S,sizeof(char *),CopyString);
    S[F-1] = NULL;   /* list must be NULL terminated */
    XtAppSetFallbackResources(app_context,S);
    
    display = XtOpenDisplay(app_context,displayName,appName,appClass,NULL,0,&argc,0);
    if (display == 0) RaiseXWindows(taskData, "XtAppInitialise failed (can't open display)");
    
    /* I don't think this is needed.  DCJM 26/5/2000 */
    /* add_file_descr(display->fd); */
    
    debug1 ("%s display opened\n",DisplayString(display));
    debug1 ("%x display fd\n",display->fd);
    
    /* ok to store C values because this is a BYTE object */
    dsHandle = alloc_and_save(taskData, SIZEOF(X_Display_Object), F_MUTABLE_BIT|F_BYTE_OBJ);
    DEREFDISPLAYHANDLE(dsHandle)->type        = TAGGED(X_Display);
    DEREFDISPLAYHANDLE(dsHandle)->display     = display;
    DEREFDISPLAYHANDLE(dsHandle)->screen      = DefaultScreen(display);
    DEREFDISPLAYHANDLE(dsHandle)->app_context = app_context; 
    AddXObject(FINISHED(taskData, dsHandle));
    
    GetList4(taskData, DEREFWORD(argHead),T,sizeof(ArgType),SetArgType);
    SetArgs(R,T,N);
    shell = XtAppCreateShell(appName,appClass,applicationShellWidgetClass,display,R,N);
    FreeArgs(T,N);
    
    if (shell == 0) RaiseXWindows(taskData, "XtAppInitialise failed  (can't create application shell)");
    
    /* added 7/12/94 SPF */
    XtAddCallback(shell,XtNdestroyCallback,DestroyWidgetCallback,NULL);
    
    return NewWidget(taskData, dsHandle,shell);
}

static Handle CreatePopupShell
(
  TaskData *taskData,
  PolyStringObject  *s,
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  Widget  parent,
  Handle  list
)
{
  char name[100]; Widget shell;
  
  unsigned     N = ListLength(DEREFWORD(list));
  Arg     *A = (Arg     *) alloca(N * sizeof(Arg));
  ArgType *T = (ArgType *) alloca(N * sizeof(ArgType));

  GetList4(taskData, DEREFWORD(list),T,sizeof(ArgType),SetArgType);
  SetArgs(A,T,N);
  
  Poly_string_to_C(s,name,sizeof(name));
  
  shell = XtCreatePopupShell(name,applicationShellWidgetClass,parent,A,N);
  
  FreeArgs(T,N);

  if (shell == 0) RaiseXWindows(taskData, "XtCreatePopupShell failed");
  
  /* added 7/12/94 SPF */
  XtAddCallback(shell,XtNdestroyCallback,DestroyWidgetCallback,NULL);

  return NewWidget(taskData, dsHandle,shell);
}

static Handle CreateXm
(
  TaskData *taskData,
  Widget (*create)(Widget, String, ArgList, Cardinal),
  char   *failed,
  Handle  dsHandle, /* Handle to (X_Display_Object *) */
  Widget  parent,
  PolyStringObject *s,
  Handle  list      /* Handle to (ML_Cons_Cell *) */
)
{
  char name[100]; Widget w;
  
  
  unsigned     N = ListLength(DEREFWORD(list));
  Arg     *A = (Arg     *) alloca(N * sizeof(Arg));
  ArgType *T = (ArgType *) alloca(N * sizeof(ArgType));

  GetList4(taskData, DEREFWORD(list),T,sizeof(ArgType),SetArgType);
  SetArgs(A,T,N);

  Poly_string_to_C(s,name,sizeof(name));
  
  w = (* create)(parent,name,A,N);
  
  FreeArgs(T,N);

  if (w == 0) RaiseXWindows(taskData, failed);

  XtAddCallback(w,XtNdestroyCallback,DestroyWidgetCallback,NULL);

  return NewWidget(taskData, dsHandle,w);
}

static void SetValues(TaskData *taskData, Widget w, Handle list)
{
  unsigned     N = ListLength(DEREFWORD(list));
  Arg     *A = (Arg     *) alloca(N * sizeof(Arg));
  ArgType *T = (ArgType *) alloca(N * sizeof(ArgType));

  GetList4(taskData, DEREFWORD(list),T,sizeof(ArgType),SetArgType);
  SetArgs(A,T,N);
  
  XtSetValues(w,A,N);

  FreeArgs(T,N);
}

typedef struct
{
  const char *listName;
  char *intName;
} StringPair;

static StringPair listTypes[] =
{
  {"argv"                  ,(char *) "argc"},
  {"buttonAccelerators"    ,(char *) "buttonCount"},
  {"buttonAcceleratorText" ,(char *) "buttonCount"},
  {"buttonMnemonicCharSets",(char *) "buttonCount"},
  {"buttonMnemonics"       ,(char *) "buttonCount"},
  {"buttons"               ,(char *) "buttonCount"},
  {"buttonType"            ,(char *) "buttonCount"},
  {"children"              ,(char *) "numChildren"},
  {"dirListItems"          ,(char *) "dirListItemCount"},
  {"fileListItems"         ,(char *) "fileListItemCount"},
  {"historyItems"          ,(char *) "historyItemCount"},
  {"items"                 ,(char *) "itemCount"},
  {"listItems"             ,(char *) "listItemCount"},
  {"selectedItems"         ,(char *) "selectedItemCount"},
  {"selectionArray"        ,(char *) "selectionArrayCount"},
};

#define MAXListTYPES (sizeof(listTypes)/sizeof(listTypes[0]))

/* (string,(v,tag)) - ML (string*Ctype) */
static void GetArgType
(
  TaskData *taskData,
  PolyWord p,
  ArgType *T,
  int      i, /* not used; needed to keep function type right */
  Widget   w
)
{
    T->tag = UNTAGGED(SND(SND(p)));
    T->N   = 0;
    T->u.i = 0;
    
    CopyString(taskData, FST(p), &T->name, 0);
    
    if (T->tag == CIntTable      ||
        T->tag == CUnsignedTable ||
        T->tag == CWidgetList    ||
        T->tag == CStringTable   ||
        T->tag == CXmStringTable)      /* if it is a list type we need to get the length from another resource */
    {
        Arg arg;
        unsigned i;
        int result;
        
        for(i = 0; i < MAXListTYPES; i++)
        {
            if (strcmp(listTypes[i].listName,T->name) == 0) break;
        }
        
        if (i == MAXListTYPES) Crash ("Bad list resource name %s",T->name);
        
    arg.name = listTypes[i].intName;
        arg.value = (XtArgVal) &result;
        /* Bug fix here which only appeared in OpenMotif and LessTif.  We need
           to pass the address of an integer here to receive the result.
           DCJM 17/5/02. */
        
        XtGetValues(w, &arg, 1);
        
        T->N = result;
    }
}

static Handle CreateWidget(TaskData *taskData, void *p, Handle dsHandle /* Handle to (X_Display_Object *) */)
{
    return EmptyWidget(taskData, dsHandle, *(Widget*)p);
}

static Handle CreateXmString(TaskData *taskData, void *t)
{
    char  *s;
    Handle S;
    
    XmStringGetLtoR(*(XmString *)t,(char *) XmSTRING_DEFAULT_CHARSET,&s);
    
    S = Make_string(s);
    
    XtFree(s);
    
    return S;
}

static Handle CreateFontList
(
 TaskData *taskData,
 Handle     dsHandle, /* Handle to (X_Display_Object *) */
 XmFontList F
 )
{
    XmFontContext   C;
    XmStringCharSet charset;
    XFontStruct    *fs;
    
    Handle list  = 0;
    Handle tail  = 0;
    
    if (XmFontListInitFontContext(&C,F) == False) return SAVE(ListNull);
    // TODO: This previously reset the save vector each time to make sure it
    // didn't overflow.  I've removed that code but it needs to be put back.
    
    while (XmFontListGetNextFont(C,&charset,&fs))
    {
        Handle L = alloc_and_save(taskData, SIZEOF(ML_Cons_Cell), F_MUTABLE_BIT);
        
        if (list == 0) list = L; // This is the first.
        
        if (tail != 0)
        { 
            DEREFLISTHANDLE(tail)->t = DEREFWORD(L);
            FINISHED(taskData, tail);
        }

        tail = L;
        /* the new list element is joined on, but not filled in */
        DEREFLISTHANDLE(tail)->h = DEREFWORD(CreatePair(taskData, CreateFontStruct(taskData,fs,dsHandle),Make_string(charset))); 
        DEREFLISTHANDLE(tail)->t = ListNull;
    }
    
    XmFontListFreeFontContext(C);
    
    if (tail != 0) FINISHED(taskData, tail);
    
    return list;
}

static Handle CreateUChar(TaskData *taskData, void *p)
{
  return Make_arbitrary_precision(taskData, *(uchar *)p);
}

static Handle CreateArg(TaskData *taskData, void *v, Handle   dsHandle /* Handle to (X_Display_Object *) */)
{
    ArgType *T = (ArgType *)v;
    Handle value;
    
    switch(T->tag)
    {
    case CAccelerators:  value = EmptyAcc      (taskData, T->u.acc);       break;
    case CBool:          value = Make_bool     (T->u.boolean);      break;
    case CColormap:      value = EmptyColormap (taskData, dsHandle,T->u.cmap);   break;
    case CCursor:        value = EmptyCursor   (taskData, dsHandle,T->u.cursor); break;
    case CDimension:     value = Make_int      (T->u.dim);       break;
    case CFontList:      value = CreateFontList(taskData, dsHandle,T->u.F);      break;
    case CInt:           value = Make_int      (T->u.i);         break;
    case CKeySym:        value = Make_arbitrary_precision (taskData, T->u.keysym);    break;
    case CPixmap:        value = EmptyPixmap   (taskData, dsHandle,T->u.pixmap); break;
    case CPosition:      value = Make_int      (T->u.posn);      break;
    case CString:        value = Make_string   (T->u.string);    break;
    case CTrans:         value = EmptyTrans    (taskData, T->u.trans);     break;
    case CUnsignedChar:  value = Make_arbitrary_precision (taskData, T->u.u);         break;
    case CVisual:        value = EmptyVisual   (taskData, dsHandle,T->u.visual); break;
    case CWidget:        value = EmptyWidget   (taskData, dsHandle,T->u.widget); break;
        
    case CXmString:      value = CreateXmString(taskData, &T->u.xmString); break;
        
    case CIntTable:      value = CreateList4(taskData, T->N,T->u.I,sizeof(int),     CreateUnsigned);        break;
    case CUnsignedTable: value = CreateList4(taskData, T->N,T->u.U,sizeof(uchar),   CreateUChar);           break;
    case CStringTable:   value = CreateList4(taskData, T->N,T->u.S,sizeof(char *),  CreateString);          break;
    case CWidgetList:    value = CreateList5(taskData,T->N,T->u.W,sizeof(Widget),  CreateWidget,dsHandle); break;
    case CXmStringTable: value = CreateList4(taskData, T->N,T->u.X,sizeof(XmString),CreateXmString);        break;
        
    default: Crash ("Bad arg type %x",T->tag); /*NOTREACHED*/
    }
    
    return value;
}

static Handle GetValue
(
 TaskData *taskData,
 Handle  dsHandle, /* Handle to (X_Display_Object *) */
 Widget  w,
 PolyWord pair /* ML (string*Ctype) */
 )
{
    Arg       A;
    ArgType   T;
    XmString *X = (XmString *) 0x55555555;
    XmString *Y = (XmString *) 0xAAAAAAAA;
    
    GetArgType(taskData,pair,&T,0,w);
    
    A.name  = T.name;
    A.value = (XtArgVal) &T.u;
    T.u.X   = X;
    
    /* The value is set to X. If it is left set to X      */
    /* then this may be a value this widget doesn't have. */
    
    XtGetValues(w,&A,1);
    
    if (T.u.X == X)
    {
        T.u.X = Y;
        
        XtGetValues(w,&A,1);
        
        if (T.u.X == Y)
        {
            char buffer[500];
            
            sprintf(buffer,"XtGetValues (%s) failed",T.name);
            
            RaiseXWindows(taskData, buffer);
        }
    }
    
    return CreateArg(taskData, &T,dsHandle);
}

/* What is the real ML type of p? (string*Ctype*string*string*string*Ctype) */
static void GetResource
(
 TaskData *taskData,
 PolyWord      pp,
 XtResource *R,
 int         i,
 ArgType    *T,
 ArgType    *D,
 Widget      w
 )
{
    PolyObject *p = pp.AsObjPtr();
    GetArgType(taskData,pp,&T[i],0,w); /* HACK !!! */
    
    CopyString(taskData, p->Get(0), &R->resource_name, 0);
    CopyString(taskData, p->Get(2), &R->resource_class, 0);
    CopyString(taskData, p->Get(3), &R->resource_type, 0);
    
    R->resource_size   = 4;
    R->resource_offset = (byte*)(&T[i].u) - (byte*)(T);
    
    SetArgTypeP(taskData, p->Get(4), p->Get(5), &D[i]); /* This was a hack.  I hope I converted it correctly.  DCJM */
    
    R->default_type = D[i].name;
    
    if (UNTAGGED(p->Get(5).AsObjPtr()->Get(1)) == CString)
        R->default_addr = (XtPointer) D[i].u.string;
    else
        R->default_addr = (XtPointer) &D[i].u;
}

static Handle GetSubresources
(
 TaskData *taskData,
 Handle  dsHandle, /* Handle to (X_Display_Object *) */
 Widget  w,
 PolyStringObject *s1,
 PolyStringObject *s2,
 Handle  list
 )
{
    char name [100];
    char clas[100];
    
    unsigned        N = ListLength(DEREFWORD(list));
    ArgType    *T = (ArgType    *) alloca(N * sizeof(ArgType));
    ArgType    *D = (ArgType    *) alloca(N * sizeof(ArgType));
    XtResource *R = (XtResource *) alloca(N * sizeof(XtResource));
    
    {
        unsigned i = 0;
        
        for(PolyWord p = DEREFWORD(list); NONNIL(p); p = ((ML_Cons_Cell *)p.AsObjPtr())->t)
        {
            GetResource(taskData,((ML_Cons_Cell *)p.AsObjPtr())->h,&R[i],i,T,D,w);
            i++;
        }
    }
    
    Poly_string_to_C(s1,name ,sizeof(name));
    Poly_string_to_C(s2,clas,sizeof(clas));
    
    XtGetSubresources(w,T,name,clas,R,N,NULL,0);
    
    return CreateList5(taskData,N,T,sizeof(ArgType),CreateArg,dsHandle);
}

static Handle GetApplicationResources (TaskData *taskData,
                                       Handle  dsHandle, /* Handle to (X_Display_Object *) */
                                       Widget  w,
                                       Handle  list
                                       )
{
    unsigned        N = ListLength (DEREFLISTHANDLE(list)) ;
    ArgType    *T = (ArgType    *) alloca ( N * sizeof(ArgType) ) ;
    ArgType    *D = (ArgType    *) alloca ( N * sizeof(ArgType) ) ;
    XtResource *R = (XtResource *) alloca ( N * sizeof(XtResource) ) ;
    
    {
        unsigned i = 0;
        for(PolyWord p = DEREFWORD(list); NONNIL(p); p = ((ML_Cons_Cell *)p.AsObjPtr())->t)
        {
            GetResource(taskData,((ML_Cons_Cell *)p.AsObjPtr())->h,&R[i],i,T,D,w);
            i++;
        }
    }
    
    XtGetApplicationResources ( w,T,R,N,NULL,0 ) ;
    
    return CreateList5 (taskData, N,T,sizeof(ArgType),CreateArg,dsHandle ) ;
}

static void GetChild(TaskData *taskData, PolyWord p, void *v, unsigned)
{
    Widget *w = (Widget *)v;
    *w = GetWidget(taskData, (X_Object *)p.AsObjPtr());
    
    if (XtParent(*w) == NULL) RaiseXWindows(taskData, "not a child"); 
}

static void ManageChildren(TaskData *taskData, Handle list)
{
    unsigned    N = ListLength(DEREFWORD(list));
    Widget *W = (Widget *) alloca(N * sizeof(Widget));
    
    GetList4(taskData, DEREFWORD(list),W,sizeof(Widget),GetChild);
    
    XtManageChildren(W,N);
}

static void UnmanageChildren(TaskData *taskData, Handle list)
{
    unsigned    N = ListLength(DEREFWORD(list));
    Widget *W = (Widget *) alloca(N * sizeof(Widget));
    
    GetList4(taskData, DEREFWORD(list),W,sizeof(Widget),GetChild);
    
    XtUnmanageChildren(W,N);
}

static Handle ParseTranslationTable(TaskData *taskData, PolyStringObject *s)
{
    XtTranslations table;
    
    int   size   = s->length + 1;
    char *buffer = (char *)alloca(size);
    
    Poly_string_to_C(s,buffer,size);
    table = XtParseTranslationTable(buffer);
    
    return EmptyTrans(taskData, table);
}

static void CommandError(TaskData *taskData, Widget w, PolyWord s)
{
    XmString p;
    GetXmString(taskData, s, &p, 0);
    XmCommandError(w,p);
    XmStringFree (p);
}

static void FileSelectionDoSearch(TaskData *taskData, Widget w, PolyWord s)
{
    XmString p;
    GetXmString(taskData, s, &p, 0);
    XmFileSelectionDoSearch(w,p);
    XmStringFree (p);
}

static void MenuPosition (Widget w, int x, int y)
{
    XButtonPressedEvent ev;
    memset (&ev, 0, sizeof(ev));
    ev.type = 4; /* Must be button. */
    ev.x_root = x;
    ev.y_root = y;
    ev.button = 3; /* Is this required? */
    ev.same_screen = 1; /* Assume this. */
    XmMenuPosition (w, &ev);
}

static Handle XmIsSomething(TaskData *taskData, unsigned is_code, Widget widget)
{
  unsigned i;
  
  switch(is_code)
  {
    case  1: i = XmIsArrowButton        (widget); break;
    case  2: i = XmIsArrowButtonGadget  (widget); break;
    case  3: i = XmIsBulletinBoard      (widget); break;
    case  4: i = XmIsCascadeButton      (widget); break;
    case  5: i = XmIsCascadeButtonGadget(widget); break;
    case  6: i = XmIsCommand            (widget); break;
    case  7: i = XmIsDesktopObject      (widget); break; /* ok - SPF 9/8/94 */
    case  8: i = XmIsDialogShell        (widget); break;
/* Unsupported in Motif 1.2
    case  9: i = XmIsDisplayObject      (widget); break;
*/
    case 10: i = XmIsDrawingArea        (widget); break;
    case 11: i = XmIsDrawnButton        (widget); break;
    case 12: i = XmIsExtObject          (widget); break; /* ok - SPF 9/8/94 */
    case 13: i = XmIsFileSelectionBox   (widget); break;
    case 14: i = XmIsForm               (widget); break;
    case 15: i = XmIsFrame              (widget); break;
    case 16: i = XmIsGadget             (widget); break;
    case 17: i = XmIsLabel              (widget); break;
    case 18: i = XmIsLabelGadget        (widget); break;
    case 19: i = XmIsList               (widget); break;
    case 20: i = XmIsMainWindow         (widget); break;
    case 21: i = XmIsManager            (widget); break;
    case 22: i = XmIsMenuShell          (widget); break;
    case 23: i = XmIsMessageBox         (widget); break;
    case 24: i = XmIsMotifWMRunning     (widget); break;
    case 25: i = XmIsPanedWindow        (widget); break;
    case 26: i = XmIsPrimitive          (widget); break;
    case 27: i = XmIsPushButton         (widget); break;
    case 28: i = XmIsPushButtonGadget   (widget); break;
    case 29: i = XmIsRowColumn          (widget); break;
    case 30: i = XmIsScale              (widget); break;
/* Unsupported in Motif 1.2
    case 31: i = XmIsScreenObject       (widget); break;
*/
    case 32: i = XmIsScrollBar          (widget); break;
    case 33: i = XmIsScrolledWindow     (widget); break;
    case 34: i = XmIsSelectionBox       (widget); break;
    case 35: i = XmIsSeparator          (widget); break;
    case 36: i = XmIsSeparatorGadget    (widget); break;
#ifdef LESSTIF_VERSION
/* This is not supported in LessTif, at least not 0.89. */
    case 37: RaiseXWindows(taskData, "XmIsShellExt: not implemented");
#else
    case 37: i = XmIsShellExt           (widget); break; /* ok - SPF 9/8/94 */
#endif
    case 38: i = XmIsText               (widget); break;
    case 39: i = XmIsTextField          (widget); break;
    case 40: i = XmIsToggleButton       (widget); break;
    case 41: i = XmIsToggleButtonGadget (widget); break;
    case 42: i = XmIsVendorShell        (widget); break;
    case 43: i = XmIsVendorShellExt     (widget); break; /* ok - SPF 9/8/94 */
/* Unsupported in Motif 1.2
    case 44: i = XmIsWorldObject        (widget); break;
*/
    
    default: Crash ("Bad code (%d) in XmIsSomething",is_code);
            /* NOTREACHED*/
  }

  return Make_bool(i);
}


/******************************************************************************/
/*                                                                            */
/*      Wrappers for standard widget operations                               */
/*                                                                            */
/******************************************************************************/

/************************* 0 parameters, no result ****************************/

/* widget -> unit */
static void WidgetAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(taskData,func_name,arg1);
  applyFunc(w); 
}

/************************* 1 parameter, no result *****************************/

/* widget -> bool -> unit */
static void WidgetBoolAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, Boolean),
  X_Object *arg1,
  PolyWord arg2
)
{
  Widget w  = getWidget(taskData,func_name,arg1);
  Boolean b = (get_C_short(taskData, arg2) != 0);
  applyFunc(w,b);
}

/* widget -> int -> unit */
static void WidgetIntAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, int),
  X_Object *arg1,
  PolyWord arg2
)
{
  Widget w = getWidget(taskData,func_name,arg1);
  int i    = get_C_long(taskData, arg2);
  applyFunc(w,i);
}

/* widget -> int -> unit */
static void WidgetLongAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, long),
  X_Object *arg1,
  PolyWord arg2
)
{
  Widget w = getWidget(taskData,func_name,arg1);
  long i   = get_C_long(taskData, arg2);
  applyFunc(w,i);
}

/* widget -> string -> unit */
static void WidgetXmstringAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, XmString),
  X_Object *arg1,
  PolyWord arg2
)
{
  Widget w     = getWidget(taskData,func_name,arg1);
  XmString s;
  GetXmString(taskData, arg2, &s, 0);
  applyFunc(w,s);
  XmStringFree(s);
}


/* widget -> string list -> unit */
static void WidgetXmstringlistAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, XmString *, int),
  X_Object *arg1,
  ML_Cons_Cell *arg2
)
{
  Widget w          = getWidget(taskData,func_name,arg1);
  unsigned n             = ListLength(arg2);
  XmString *strings = (XmString *)alloca(n * sizeof(XmString));
  GetList4(taskData, arg2,strings,sizeof(XmString),GetXmString);
  applyFunc(w,strings,n);
  for (unsigned i = 0; i < n; i ++) XmStringFree(strings[i]);
}

/************************* 2 parameters, no result ****************************/

/* widget -> int -> bool -> unit */
static void WidgetIntBoolAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, int, Boolean),
  X_Object *arg1,
  PolyWord arg2,
  PolyWord arg3
)
{ 
  Widget w  = getWidget(taskData,func_name,arg1);
  int i     = get_C_long(taskData, arg2);
  Boolean b = (get_C_ushort(taskData, arg3) != 0);
  applyFunc(w,i,b);
}

/* widget -> int -> int -> unit */
static void WidgetIntIntAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, int, int),
  X_Object *arg1,
  PolyWord arg2,
  PolyWord arg3
)
{ 
  Widget w  = getWidget(taskData,func_name,arg1);
  int x     = get_C_long(taskData, arg2);
  int y     = get_C_long(taskData, arg3);
  applyFunc(w,x,y);
}

/* widget -> string -> bool -> unit */
static void WidgetXmstringBoolAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, XmString, Boolean),
  X_Object *arg1,
  PolyWord arg2,
  PolyWord arg3
)
{
  Widget w     = getWidget(taskData,func_name,arg1);
  XmString s;
  Boolean b    = (get_C_ushort(taskData, arg3) != 0);
  
  GetXmString(taskData, arg2, &s, 0);
  applyFunc(w,s,b);
  XmStringFree(s);
}


/* widget -> string -> int -> unit */
static void WidgetXmstringIntAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, XmString, int),
  X_Object *arg1,
  PolyWord arg2,
  PolyWord arg3
)
{
  Widget w     = getWidget(taskData,func_name,arg1);
  XmString s;
  int i        = get_C_long(taskData, arg3);
  GetXmString(taskData, arg2, &s, 0);
  applyFunc(w,s,i);
  XmStringFree(s);
}

/* widget -> string list -> int -> unit */
static void WidgetXmstringlistIntAction
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  void applyFunc(Widget, XmString *, int, int),
  X_Object *arg1,
  ML_Cons_Cell *arg2,
  PolyWord arg3
)
{
  Widget w          = getWidget(taskData,func_name,arg1);
  unsigned n             = ListLength(arg2);
  int i             = get_C_long(taskData, arg3);
  XmString *strings = (XmString *)alloca(n * sizeof(XmString));

  GetList4(taskData, arg2,strings,sizeof(XmString),GetXmString);
  applyFunc(w,strings,n,i);
  for (unsigned i = 0; i < n; i ++) XmStringFree(strings[i]);
}

/************************* n parameters, some result **************************/
static Handle int_ptr_to_arb(TaskData *taskData, void *p)
{
    return Make_arbitrary_precision(taskData, *(int *)p);
}

/* widget -> int */
static Handle WidgetToInt
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  int applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(taskData, func_name,arg1);
  int res  = applyFunc(w);
  return(Make_arbitrary_precision(taskData, res));
}

/* widget -> int */
static Handle WidgetToLong
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *taskData, char *, X_Object *),
  long applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(taskData, func_name,arg1);
  long res  = applyFunc(w);
  return(Make_arbitrary_precision(taskData, res));
}

#if 0
/* widget -> int */
static Handle WidgetToUnsigned
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  unsigned applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(taskData, func_name,arg1);
  unsigned res  = applyFunc(w);
  return(Make_arbitrary_precision(taskData, res));
}
#endif

/* widget -> bool */
static Handle WidgetToBool
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  Boolean applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w = getWidget(taskData, func_name,arg1);
  Boolean res  = applyFunc(w);
  return(Make_bool(res));
}

/* widget -> string */
static Handle WidgetToString
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  char *applyFunc(Widget),
  X_Object *arg1
)
{
  Widget w   = getWidget(taskData, func_name,arg1);
  char *s    = applyFunc(w);
  Handle res = Make_string(s); /* safe, even if C pointer is NULL */
  XtFree(s);
  return(res);
}

/* widget -> int list */
static Handle WidgetToIntlist
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  Boolean applyFunc(Widget, int**, int *),
  X_Object *arg1
)
{
  int item_count, *items;
  Boolean non_empty;
  Widget w  = getWidget(taskData,func_name,arg1);

  non_empty = applyFunc(w, &items, &item_count);
  
  if (non_empty != TRUE)
    /* nothing found, and Motif hasn't allocated any space */
    /* so just retun nil */
    {
       return (SAVE(ListNull));
    }
  else
    /* copy the list into the ML heap, then free it */
    {
      Handle res = CreateList4(taskData, item_count,items,sizeof(int),int_ptr_to_arb);
      XtFree((char *)items);
      return res;
    }
}

/* widget -> string -> int list */
static Handle WidgetXmstringToIntlist
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  Boolean applyFunc(Widget, XmString, int**, int *),
  X_Object *arg1,
  PolyWord arg2
)
{
  int item_count, *items;
  Boolean non_empty;
  Widget w     = getWidget(taskData,func_name,arg1);
  XmString s;
  
  GetXmString(taskData, arg2, &s, 0);
  non_empty = applyFunc(w, s, &items, &item_count);
  XmStringFree(s);
  
  if (non_empty != TRUE)
    /* nothing found, so just retun nil */
    {
       return (SAVE(ListNull));
    }
  else
    /* copy the list into the ML heap, then free it */
    {
      Handle res = CreateList4(taskData, item_count,items,sizeof(int),int_ptr_to_arb);
      XtFree((char *)items);
      return res;
    }
}

/* widget -> string -> int */
static Handle WidgetXmstringToInt
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  int applyFunc(Widget, XmString),
  X_Object *arg1,
  PolyWord arg2
)
{
  Widget w     = getWidget(taskData,func_name,arg1);
  XmString s;
  int res;
  
  GetXmString(taskData, arg2, &s, 0);
  res = applyFunc(w, s);
  XmStringFree(s);
  
  return (Make_int(res));
}

/* widget -> string -> bool */
static Handle WidgetXmstringToBool
(
  TaskData *taskData,
  char *func_name,
  Widget getWidget(TaskData *, char *, X_Object *),
  Boolean applyFunc(Widget, XmString),
  X_Object *arg1,
  PolyWord arg2
)
{
  Widget w     = getWidget(taskData,func_name,arg1);
  XmString s;
  Boolean res;

  GetXmString(taskData, arg2, &s, 0);
  res = applyFunc(w, s);
  XmStringFree(s);

  return (Make_bool(res));
}


/******************************************************************************/

/* code added SPF 25/2/95 */
static bool isPossibleString(PolyObject *P)
{  
    if (!OBJ_IS_DATAPTR(P)) return false;
    
    POLYUNSIGNED L = P->LengthWord();
    
    if (! OBJ_IS_BYTE_OBJECT(L)) return false;
    
    /* get object PolyWord count */
    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
    
    if (n < 1) return false;
    
    /* get string byte count */
    POLYUNSIGNED m = P->Get(0).AsUnsigned();
    
    /* number of words to hold the bytes */
    m = (m + 3) / 4;
    
    /* number of words to hold the bytes, plus the byte count */
    m = m + 1;
    
    /* If that's the same as the object PolyWord count, 
    we've probably got a genuine string! */
    return (m == n);
}

/* Prints out the contents of a PolyWord in the X interface tuple */
static void DebugPrintWord(PolyWord P /* was X_Object *P */)
{
    TaskData *taskData = processes->GetTaskDataForThread();
    if (IS_INT((P))) 
    {
        printf("Short %d", (int)UNTAGGED(P));
        return;
    }
    
    if (isPossibleString(P.AsObjPtr()))
    {
        if (((PolyStringObject*)P.AsObjPtr())->length <= 40)
        {
            printf("String: \"");
            print_string((PolyStringObject*) P.AsObjPtr());
            printf("\"");
            return;
        }
        else
        {
            printf("Long String: %p", P.AsAddress());    
            return;
        }
    }
    
    /* The problem with the following code was that we can't be sure
    that the object we have is really an X_Object - it might just
    look like one. If this is the case, when we try to validate
    the object using ResourceExists we may get a core dump because
    ResourceExists assumes it has a valid X_Object and calls
    hashId which dereferences fields within the so-called X_object.
    That's why we redefine ResourceExists to be SafeResourceExists
    which doesn't make any assumptions about the contents of the
    so-called X_object. SPF 6/4/95 */
    
#define XP ((X_Object *)P.AsObjPtr())
#define ResourceExists SafeResourceExists
    {
        switch(UNTAGGED(XP->type))
        {
        case X_GC:       (ResourceExists(XP)
                             ? printf("GC %lx", GetGC(taskData, XP)->gid)
                             : printf("Old GC <%lx>",P.AsUnsigned()));
            return;
            
        case X_Font:     (ResourceExists(XP)
                             ? printf("Font %lx",GetFont(taskData, XP))
                             : printf("Old Font <%x>",(int)P.AsUnsigned()));
            return;
            
        case X_Cursor:   (ResourceExists(XP)
                             ? printf("Cursor %lx",GetCursor(taskData, XP))
                             : printf("Old Cursor <%x>",(int)P.AsUnsigned()));
            return;
            
        case X_Window:   (ResourceExists(XP)
                             ? printf("Window %lx",GetWindow(taskData, XP))
                             : printf("Old Window <%p>",P.AsAddress()));
            return;
            
        case X_Pixmap:   (ResourceExists(XP)
                             ? printf("Pixmap %lx",GetPixmap(taskData, XP))
                             : printf("Old Pixmap <%p>",P.AsAddress()));
            return;
            
        case X_Colormap: (ResourceExists(XP)
                             ? printf("Colormap %lx",GetColormap(taskData, XP))
                             : printf("Old Colormap <%p>",P.AsAddress()));
            return;
            
        case X_Visual:   (ResourceExists(XP)
                             ? printf("Visual %lx",GetVisual(taskData, XP)->visualid)
                             : printf("Old Visual <%p>",P.AsAddress()));
            return;
            
        case X_Widget:   (ResourceExists(XP)
                             ? printf("Widget %p",GetNWidget(taskData, XP))
                             : printf("Old Widget <%p>",P.AsAddress()));
            return;
            
        case X_Trans:    (ResourceExists(XP)
                             ? printf("Trans %p",GetTrans(taskData, XP))
                             : printf("Old Trans <%p>",P.AsAddress()));
            return;
            
        case X_Acc:      (ResourceExists(XP)
                             ? printf("Acc %p",GetAcc(taskData, XP))
                             : printf("Old Acc <%p>",P.AsAddress()));
            return;
            
        case X_Display:  (ResourceExists(XP)
                             ? printf("Display %s", DisplayString(GetDisplay(taskData, XP)))
                             + printf(":%x", GetDisplay(taskData, XP)->fd)
                             : printf("Old Display <%p>",P.AsAddress()));
            return;
            
        default:         printf("Pointer "ZERO_X"%p",P.AsAddress());
            return;
        }
    }
#undef ResourceExists
#undef XP
}

/* Prints out the contents of the X interface tuple */
static void DebugPrintCode(PolyObject *pt)
{
    POLYUNSIGNED N = pt->Length();  
    POLYUNSIGNED i = 1;
    assert(IS_INT(pt->Get(0)));
    
    printf("%ld:(", UNTAGGED(pt->Get(0)));
    
    while(i < N)
    {
        DebugPrintWord(pt->Get(i++));
        if (i < N)
            printf(",");
    }
    
    printf(")\n");
}

#define P0  DEREFHANDLE(params)->Get(0)
#define P1  DEREFHANDLE(params)->Get(1)
#define P2  DEREFHANDLE(params)->Get(2)
#define P3  DEREFHANDLE(params)->Get(3)
#define P4  DEREFHANDLE(params)->Get(4)
#define P5  DEREFHANDLE(params)->Get(5)
#define P6  DEREFHANDLE(params)->Get(6)
#define P7  DEREFHANDLE(params)->Get(7)
#define P8  DEREFHANDLE(params)->Get(8)
#define P9  DEREFHANDLE(params)->Get(9)
#define P10 DEREFHANDLE(params)->Get(10)
#define P11 DEREFHANDLE(params)->Get(11)
#define P12 DEREFHANDLE(params)->Get(12)

#define XP1 ((X_Object *)P1.AsObjPtr())
#define XP2 ((X_Object *)P2.AsObjPtr())
#define XP3 ((X_Object *)P3.AsObjPtr())
#define XP4 ((X_Object *)P4.AsObjPtr())
#define XP5 ((X_Object *)P5.AsObjPtr())
#define XP6 ((X_Object *)P6.AsObjPtr())
#define XP7 ((X_Object *)P7.AsObjPtr())

/* Xwindows_c gets passed the address of an object in save_vec, */
/* which is itself a pointer to a tuple in the Poly heap.       */

Handle XWindows_c(TaskData *taskData, Handle params)
{
    int code = get_C_short(taskData, P0);
    
    if ((debugOptions & DEBUG_X)) DebugPrintCode(DEREFHANDLE(params));
    
    switch(code)
    {
    case XCALL_Not:
        return Make_arbitrary_precision(taskData, ~ get_C_ulong(taskData, P1));
        
    case XCALL_And:
        return Make_arbitrary_precision(taskData, get_C_ulong(taskData, P1) & get_C_ulong(taskData, P2));
        
    case XCALL_Or:
        return Make_arbitrary_precision(taskData, get_C_ulong(taskData, P1) | get_C_ulong(taskData, P2));
        
    case XCALL_Xor:
        return Make_arbitrary_precision(taskData, get_C_ulong(taskData, P1) ^ get_C_ulong(taskData, P2));
        
    case XCALL_DownShift:
        return Make_arbitrary_precision(taskData, get_C_ulong(taskData, P1) >> get_C_ulong(taskData, P2));
        
    case XCALL_UpShift:
        return Make_arbitrary_precision(taskData, get_C_ulong(taskData, P1) << get_C_ulong(taskData, P2));
        
    case XCALL_NoDrawable:
        return EmptyPixmap(taskData, SAVE(ListNull),(Pixmap)get_C_ulong(taskData, P1));
        
    case XCALL_NoCursor:
        return EmptyCursor(taskData, SAVE(ListNull),(Cursor)None);
        
    case XCALL_NoFont:
        return EmptyFont(taskData, SAVE(ListNull),(Font)None,(XFontStruct *)NULL);
        
    case XCALL_NoColormap:
        return EmptyColormap(taskData, SAVE(ListNull),(Colormap) None);
        
    case XCALL_NoVisual:
        return EmptyVisual(taskData, SAVE(ListNull),(Visual *)None);
        
    case XCALL_GetTimeOfDay:
        return GetTimeOfDay(taskData);
        
        /* Colorcells 100 */
    case XCALL_XAllocColor:
        return AllocColor(taskData, GetDisplay(taskData, XP1),GetColormap(taskData, XP1),GetXColor1(taskData, P2));
        
    case XCALL_XAllocColorCells:
        return AllocColorCells(taskData, GetDisplay(taskData, XP1),
            GetColormap(taskData, XP1),
            get_C_ulong(taskData, P2),
            get_C_ulong(taskData, P3),
            get_C_ulong(taskData, P4));
        
    case XCALL_XAllocColorPlanes:
        return AllocColorPlanes(taskData, GetDisplay(taskData, XP1),
            GetColormap(taskData, XP1),
            get_C_ulong(taskData, P2),
            get_C_ulong(taskData, P3),
            get_C_ulong(taskData, P4),
            get_C_ulong(taskData, P5),
            get_C_ulong(taskData, P6));
        
    case XCALL_XAllocNamedColor:
        return AllocNamedColor(taskData, GetDisplay(taskData, XP1),GetColormap(taskData, XP1),GetString(P2));
        
    case XCALL_XFreeColors:
        FreeColors(taskData, GetDisplay(taskData, XP1),GetColormap(taskData, XP1),SAVE(P2),get_C_ulong(taskData, P3));
        break;
        
    case XCALL_XLookupColor:
        return LookupColor(taskData, GetDisplay(taskData, XP1),GetColormap(taskData, XP1),GetString(P2));
        
    case XCALL_XParseColor:
        return ParseColor(taskData, GetDisplay(taskData, XP1),GetColormap(taskData, XP1),GetString(P2));
        
    case XCALL_XQueryColor:
        return QueryColor(taskData, GetDisplay(taskData, XP1),GetColormap(taskData, XP1),get_C_ulong(taskData, P2));
        
    case XCALL_XQueryColors:
        return QueryColors(taskData, GetDisplay(taskData, XP1),GetColormap(taskData, XP1),SAVE(P2));
        
    case XCALL_XStoreColor:
        XStoreColor(GetDisplay(taskData, XP1),GetColormap(taskData, XP1),GetXColor1(taskData, P2));
        break;
        
    case XCALL_XStoreColors:
        StoreColors(taskData, GetDisplay(taskData, XP1),GetColormap(taskData, XP1),SAVE(P2));
        break;
        
    case XCALL_XStoreNamedColor:
        StoreNamedColor(GetDisplay(taskData, XP1),
            GetColormap(taskData, XP1),
            GetString(P2),
            get_C_ulong(taskData, P3),
            get_C_ulong(taskData, P4),
            get_C_ulong(taskData, P5),
            get_C_ulong(taskData, P6));
        break;
        
    case XCALL_BlackPixel:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1); 
        return Make_arbitrary_precision(taskData, BlackPixel(DEREFDISPLAYHANDLE(dsHandle)->display,
            DEREFDISPLAYHANDLE(dsHandle)->screen)); }
        
    case XCALL_WhitePixel:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return Make_arbitrary_precision(taskData, WhitePixel(DEREFDISPLAYHANDLE(dsHandle)->display,
            DEREFDISPLAYHANDLE(dsHandle)->screen)); }
        
        /* Colormaps 150 */
    case XCALL_XCopyColormapAndFree:
        return EmptyColormap(taskData, GetDS(taskData, XP1),XCopyColormapAndFree(GetDisplay(taskData, XP1),GetColormap(taskData, XP1)));
        
    case XCALL_XCreateColormap:
        return EmptyColormap(taskData, GetDS(taskData, XP1),XCreateColormap(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetVisual(taskData, XP2),get_C_ulong(taskData, P3)));
        
    case XCALL_XInstallColormap:
        XInstallColormap(GetDisplay(taskData, XP1),GetColormap(taskData, XP1)); break;
        
    case XCALL_XListInstalledColormaps:
        return ListInstalledColormaps(taskData, GetDS(taskData, XP1),GetDrawable(taskData, XP1));
        
    case XCALL_XUninstallColormap:
        XUninstallColormap(GetDisplay(taskData, XP1),GetColormap(taskData, XP1)); break;
        
    case XCALL_DefaultColormap:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return EmptyColormap(taskData, dsHandle,
            DefaultColormap(DEREFDISPLAYHANDLE(dsHandle)->display,
            DEREFDISPLAYHANDLE(dsHandle)->screen));
        }
        
    case XCALL_DefaultVisual:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return EmptyVisual(taskData, dsHandle,
            DefaultVisual(DEREFDISPLAYHANDLE(dsHandle)->display,
            DEREFDISPLAYHANDLE(dsHandle)->screen));
        }
        
    case XCALL_DisplayCells:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1); 
        return Make_arbitrary_precision(taskData, DisplayCells(DEREFDISPLAYHANDLE(dsHandle)->display,
            DEREFDISPLAYHANDLE(dsHandle)->screen));
        }
        
    case XCALL_VisualClass:
        return Make_arbitrary_precision(taskData, GetVisual(taskData, XP1)->c_class);
        
    case XCALL_VisualRedMask:
        return Make_arbitrary_precision(taskData, GetVisual(taskData, XP1)->red_mask);
        
    case XCALL_VisualGreenMask:
        return Make_arbitrary_precision(taskData, GetVisual(taskData, XP1)->green_mask);
        
    case XCALL_VisualBlueMask:
        return Make_arbitrary_precision(taskData, GetVisual(taskData, XP1)->blue_mask);
        
        /* Cursors 200 */
    case XCALL_XCreateFontCursor:
        return CreateFontCursor(taskData, GetDS(taskData, XP1),get_C_ulong(taskData, P2));
        
    case XCALL_XCreateGlyphCursor:
        return CreateGlyphCursor(taskData, GetDS(taskData, XP1),
            GetFont(taskData, XP1),
            GetFont(taskData, XP2),
            get_C_ulong(taskData, P3),
            get_C_ulong(taskData, P4),
            GetXColor1(taskData, P5),
            GetXColor2(taskData, P6));
        
    case XCALL_XCreatePixmapCursor:
        return CreatePixmapCursor(taskData, GetDS(taskData, XP1),
            GetPixmap(taskData, XP1),  /* source     */
            GetPixmap(taskData, XP2),  /* mask       */
            GetXColor1(taskData, P3), /* foreground */
            GetXColor2(taskData, P4), /* background */
            GetOffsetX(taskData, P5), /* x          */
            GetOffsetY(taskData, P5)  /* y          */);
        
    case XCALL_XDefineCursor:
        XDefineCursor(GetDisplay(taskData, XP1),GetWindow(taskData, XP1),GetCursor(taskData, XP2));
        WindowObject(XP1)->cursor_object = CursorObject(XP2);
        break;
        
    case XCALL_XQueryBestCursor:
        CheckZeroRect(taskData, P2);
        return QueryBest(taskData, XQueryBestCursor,
            GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetRectW(taskData, P2),
            GetRectH(taskData, P2));
        
    case XCALL_XRecolorCursor:
        XRecolorCursor(GetDisplay(taskData, XP1),
            GetCursor(taskData, XP1),
            GetXColor1(taskData, P2),
            GetXColor2(taskData, P3));
        break;
        
    case XCALL_XUndefineCursor:
        XUndefineCursor(GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        WindowObject(XP1)->cursor_object = 0;
        break;
        
        /* Display Specifications 250 */
        
    case XCALL_XOpenDisplay:
        return OpenDisplay(taskData, GetString(XP1)); 
        
#define DODISPLAYOP(op) \
        {\
        Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);\
        return Make_arbitrary_precision(taskData, op(DEREFDISPLAYHANDLE(dsHandle)->display,\
        DEREFDISPLAYHANDLE(dsHandle)->screen));\
        }
        
    case XCALL_CellsOfScreen:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return Make_arbitrary_precision(taskData, CellsOfScreen(ScreenOfDisplay(DEREFDISPLAYHANDLE(dsHandle)->display,
            DEREFDISPLAYHANDLE(dsHandle)->screen)));
        }
        
    case XCALL_DefaultDepth:
        DODISPLAYOP(DefaultDepth)
            
    case XCALL_DisplayHeight:
        DODISPLAYOP(DisplayHeight)
            
    case XCALL_DisplayHeightMM:
        DODISPLAYOP(DisplayHeightMM)
            
    case XCALL_DisplayPlanes:
        DODISPLAYOP(DisplayPlanes)
            
    case XCALL_DisplayString:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return Make_string(DisplayString(DEREFDISPLAYHANDLE(dsHandle)->display));
        }
        
    case XCALL_DisplayWidth:
        DODISPLAYOP(DisplayWidth)
            
    case XCALL_DisplayWidthMM:
        DODISPLAYOP(DisplayWidthMM)
#undef DODISPLAYOP
            
            
#define DODISPLAYSCREENOP(op) \
        {\
        Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);\
        return Make_arbitrary_precision(taskData, op(ScreenOfDisplay(DEREFDISPLAYHANDLE(dsHandle)->display,\
        DEREFDISPLAYHANDLE(dsHandle)->screen)));\
        }
            
    case XCALL_DoesBackingStore:
        DODISPLAYSCREENOP(DoesBackingStore)
            
    case XCALL_DoesSaveUnders:
        DODISPLAYSCREENOP(DoesSaveUnders)
            
    case XCALL_EventMaskOfScreen:
        DODISPLAYSCREENOP(EventMaskOfScreen)
            
    case XCALL_MaxCmapsOfScreen:
        DODISPLAYSCREENOP(MaxCmapsOfScreen)
            
    case XCALL_MinCmapsOfScreen:
        DODISPLAYSCREENOP(MinCmapsOfScreen)
#undef DODISPLAYSCREENOP
            
    case XCALL_ProtocolRevision:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return Make_arbitrary_precision(taskData, ProtocolRevision(DEREFDISPLAYHANDLE(dsHandle)->display));
        }
        
    case XCALL_ProtocolVersion:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return Make_arbitrary_precision(taskData, ProtocolVersion(DEREFDISPLAYHANDLE(dsHandle)->display));
        }
        
    case XCALL_ServerVendor:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return Make_string  (ServerVendor(DEREFDISPLAYHANDLE(dsHandle)->display));
        }
        
    case XCALL_VendorRelease:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return Make_arbitrary_precision(taskData, VendorRelease(DEREFDISPLAYHANDLE(dsHandle)->display));
        }
        
        /* Drawing Primitives 300 */
    case XCALL_XClearArea:
        XClearArea(GetDisplay(taskData, XP1),
            GetWindow(taskData, XP1),
            GetRectX(taskData, P2),
            GetRectY(taskData, P2),
            GetRectW(taskData, P2),
            GetRectH(taskData, P2),
            get_C_ulong(taskData, P3));
        break;
        
    case XCALL_XClearWindow:
        XClearWindow(GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        break;
        
    case XCALL_XCopyArea:
        XCopyArea(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetDrawable(taskData, XP2),
            GetGC(taskData, XP3),
            GetPointX(taskData, P4),
            GetPointY(taskData, P4),
            GetRectW(taskData, P5),
            GetRectH(taskData, P5),
            GetRectX(taskData, P5),
            GetRectY(taskData, P5));
        break;
        
    case XCALL_XCopyPlane:
        XCopyPlane(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetDrawable(taskData, XP2),
            GetGC(taskData, XP3),
            GetPointX(taskData, P4),
            GetPointY(taskData, P4),
            GetRectW(taskData, P5),
            GetRectH(taskData, P5),
            GetRectX(taskData, P5),
            GetRectY(taskData, P5),
            get_C_ulong(taskData, P6));
        break;
        
    case XCALL_XDrawArc:
        XDrawArc(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetGC(taskData, XP2),
            GetRectX(taskData, GetArcR(P3)),
            GetRectY(taskData, GetArcR(P3)),
            GetRectW(taskData, GetArcR(P3)),
            GetRectH(taskData, GetArcR(P3)),
            GetArcA1(taskData, P3),
            GetArcA2(taskData, P3));
        break;
        
    case XCALL_XDrawArcs:
        {
            Handle list = SAVE(P3);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XArc *L = (XArc *)alloca(N * sizeof(XArc));
                GetList4(taskData, DEREFWORD(list), L, sizeof(XArc), GetArcs);
                XDrawArcs(GetDisplay(taskData, XP1), GetDrawable(taskData, XP1), GetGC(taskData, XP2), L, N);
            }
        }
        break;
        
    case XCALL_XDrawImageString:
        XDrawImageString(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetGC(taskData, XP2),
            GetPointX(taskData, P3),
            GetPointY(taskData, P3),
            GetString(P4)->chars,
            GetString(P4)->length);
        break;
        
    case XCALL_XDrawImageString16:
        {
            Handle list = SAVE(P4);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XChar2b *L = (XChar2b *)alloca(N * sizeof(XChar2b));
                GetList4(taskData, DEREFWORD(list),L, sizeof(XChar2b), GetChar2);
                XDrawImageString16(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetGC(taskData, XP2),GetPointX(taskData, P3),GetPointY(taskData, P3),L,N);
            }
        }
        break;
        
    case XCALL_XDrawLine:
        XDrawLine(GetDisplay(taskData, XP1), GetDrawable(taskData, XP1), GetGC(taskData, XP2), GetPointX(taskData, P3), GetPointY(taskData, P3),
            GetPointX(taskData, P4), GetPointY(taskData, P4));
        break;
        
    case XCALL_XDrawLines:
        {
            Handle list = SAVE(P3);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XPoint *L = (XPoint *)alloca(N * sizeof(XPoint));
                GetList4(taskData, DEREFWORD(list), L, sizeof(XPoint), GetPoints);
                XDrawLines(GetDisplay(taskData, XP1), GetDrawable(taskData, XP1), GetGC(taskData, XP2), L, N, get_C_ulong(taskData, P4));
            }
        }
        break;
        
    case XCALL_XDrawPoint:
        XDrawPoint(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetGC(taskData, XP2),
            GetPointX(taskData, P3),
            GetPointY(taskData, P3));
        break;
        
    case XCALL_XDrawPoints:
        {
            Handle list = SAVE(P3);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XPoint *L = (XPoint *)alloca(N * sizeof(XPoint));
                GetList4(taskData, DEREFWORD(list),L,sizeof(XPoint),GetPoints);
                XDrawPoints(GetDisplay(taskData, XP1), GetDrawable(taskData, XP1), GetGC(taskData, XP2), L, N, get_C_ulong(taskData, P4));
            }
        }
        break;
        
    case XCALL_XDrawRectangle:
        XDrawRectangle(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetGC(taskData, XP2),
            GetRectX(taskData, P3),
            GetRectY(taskData, P3),
            GetRectW(taskData, P3),
            GetRectH(taskData, P3));
        break;
        
    case XCALL_XDrawRectangles:
        {
            Handle list = SAVE(P3);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XRectangle *L = (XRectangle *)alloca(N * sizeof(XRectangle));
                GetList4(taskData, DEREFWORD(list),L,sizeof(XRectangle),GetRects);
                XDrawRectangles(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetGC(taskData, XP2),L,N);
            }
        }
        break;
        
    case XCALL_XDrawSegments:
        {
            Handle list = SAVE(P3);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XSegment *L = (XSegment *)alloca(N * sizeof(XSegment));
                GetList4(taskData, DEREFWORD(list),L,sizeof(XSegment),GetSegments);
                XDrawSegments(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetGC(taskData, XP2),L,N);
            }
        }
        break;
        
    case XCALL_XDrawString:
        XDrawString(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetGC(taskData, XP2),
            GetPointX(taskData, P3),
            GetPointY(taskData, P3),
            GetString(P4)->chars,
            GetString(P4)->length);
        break;
        
    case XCALL_XDrawString16:
        {
            Handle list = SAVE(P4);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XChar2b *L = (XChar2b *)alloca(N * sizeof(XChar2b));
        
                GetList4(taskData, DEREFWORD(list),L,sizeof(XChar2b),GetChar2);
        
                XDrawString16(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetGC(taskData, XP2),GetPointX(taskData, P3),GetPointY(taskData, P3),L,N);
            }
        }
        break;
        
    case XCALL_XDrawText:
        {
            Handle list = SAVE(P4);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XTextItem *L = (XTextItem *)alloca(N * sizeof(XTextItem));
        
                GetList4(taskData, DEREFWORD(list),L,sizeof(XTextItem),GetText);
                XDrawText(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetGC(taskData, XP2),GetPointX(taskData, P3),GetPointY(taskData, P3),L,N);

                while (N--) { free(L->chars); L++; }
            }
        }
        break;
        
    case XCALL_XDrawText16:
        {
            Handle list = SAVE(P4);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XTextItem16 *L = (XTextItem16 *)alloca(N * sizeof(XTextItem16));
                GetList4(taskData, DEREFWORD(list),L,sizeof(XTextItem16), GetText16);
                XDrawText16(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetGC(taskData, XP2),GetPointX(taskData, P3),GetPointY(taskData, P3),L,N);
        
                while (N--) { free(L->chars); L++; }
            }
        }
        break;
        
    case XCALL_XFillArc:
        XFillArc(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetGC(taskData, XP2),
            GetRectX(taskData, GetArcR(P3)),
            GetRectY(taskData, GetArcR(P3)),
            GetRectW(taskData, GetArcR(P3)),
            GetRectH(taskData, GetArcR(P3)),
            GetArcA1(taskData, P3),
            GetArcA2(taskData, P3));
        break;
        
    case XCALL_XFillArcs:
        {
            Handle list = SAVE(P3);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XArc *L = (XArc *)alloca(N * sizeof(XArc));
        
                GetList4(taskData, DEREFWORD(list),L,sizeof(XArc),GetArcs);
        
                XFillArcs(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetGC(taskData, XP2),L,N);
            }
        }
        break;
        
    case XCALL_XFillPolygon:
        {
            Handle list = SAVE(P3);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XPoint *L = (XPoint *)alloca(N * sizeof(XPoint));
        
                GetList4(taskData, DEREFWORD(list),L,sizeof(XPoint),GetPoints);
        
                XFillPolygon(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetGC(taskData, XP2),L,N,get_C_ulong(taskData, P4),get_C_ulong(taskData, P5));
            }
        }
        break;
        
    case XCALL_XFillRectangle:
        XFillRectangle(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetGC(taskData, XP2),
            GetRectX(taskData, P3),
            GetRectY(taskData, P3),
            GetRectW(taskData, P3),
            GetRectH(taskData, P3));
        break;
        
    case XCALL_XFillRectangles:
        {
            Handle list = SAVE(P3);
            if (NONNIL(DEREFWORD(list)))
            {
                unsigned  N = ListLength(DEREFWORD(list));
                XRectangle *L = (XRectangle *)alloca(N * sizeof(XRectangle));
                GetList4(taskData, DEREFWORD(list),L,sizeof(XRectangle),GetRects);
                XFillRectangles(GetDisplay(taskData, XP1),GetDrawable(taskData, XP1),GetGC(taskData, XP2),L,N);
            }
        }
        break;
        
        /* Events 350 */
        
    case XCALL_XSelectInput:
        (WindowObject(XP1))->eventMask->Set(0, PolyWord::FromUnsigned(get_C_ulong(taskData, P2)));
        XSelectInput(GetDisplay(taskData, XP1),GetWindow(taskData, XP1),XMASK((WindowObject(XP1))->eventMask->Get(0).AsUnsigned()));
        break;
        
    case XCALL_XSynchronize:
        XSynchronize(GetDisplay(taskData, XP1),get_C_ulong(taskData, P2));
        break;
        
    case XCALL_GetState:
        return GetState(taskData, WindowObject(XP1)); /* WindowObject added SPF */
        
    case XCALL_SetState:
        SetState(WindowObject(XP1),P2,P3); /* WindowObject added SPF */
        break;
        
    case XCALL_NextEvent:
        return NextEvent(taskData, GetDS(taskData, XP1));
        
    case XCALL_InsertTimeout:
        InsertTimeout(taskData, WindowObject(XP1),get_C_ulong(taskData, P2),P3,P4); /* WindowObject added SPF */
        break;
        
    case XCALL_XSetInputFocus:
        XSetInputFocus(GetDisplay(taskData, XP1),GetWindow(taskData, XP2),get_C_ulong(taskData, P3),get_C_ulong(taskData, P4));
        break;
        
    case XCALL_XGetInputFocus:
        return GetInputFocus(taskData, GetDS(taskData, XP1));
        
    case XCALL_XSetSelectionOwner:
        SetSelectionOwner(GetDS(taskData, XP1),get_C_ulong(taskData, P2),GetWindow(taskData, XP3),get_C_ulong(taskData, P4));
        break;
        
    case XCALL_XGetSelectionOwner:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return EmptyWindow(taskData, dsHandle,XGetSelectionOwner(DEREFDISPLAYHANDLE(dsHandle)->display,
            get_C_ulong(taskData, P2)));
        }
        
    case XCALL_XConvertSelection:
        XConvertSelection(GetDisplay(taskData, XP4),
            get_C_ulong(taskData, P1),
            get_C_ulong(taskData, P2),
            get_C_ulong(taskData, P3),
            GetWindow(taskData, XP4),
            get_C_ulong(taskData, P5));
        break;
        
    case XCALL_XSendSelectionNotify:
        SendSelectionNotify(GetDisplay(taskData, XP4),
            get_C_ulong(taskData, P1),
            get_C_ulong(taskData, P2),
            get_C_ulong(taskData, P3),
            GetWindow(taskData, XP4),
            get_C_ulong(taskData, P5));
        break;
        
    case XCALL_XDeleteProperty:
        XDeleteProperty(GetDisplay(taskData, XP1),GetWindow(taskData, XP1),get_C_ulong(taskData, P2));
        break;
        
    case XCALL_XInternAtom:
        return InternAtom(taskData, GetDisplay(taskData, XP1),GetString(P2),get_C_long(taskData, P3));
        
    case XCALL_XGetAtomName:
        return GetAtomName(taskData, GetDisplay(taskData, XP1),get_C_ulong(taskData, P2));
        
        /* Fonts 400 */
        
    case XCALL_XGetFontPath:
        return GetFontPath(taskData, GetDisplay(taskData, XP1));
        
    case XCALL_XListFonts:
        return ListFonts(taskData, GetDisplay(taskData, XP1),GetString(P2),get_C_ulong(taskData, P3));
        
    case XCALL_XListFontsWithInfo:
        return ListFontsWithInfo(taskData, GetDS(taskData, XP1),GetString(P2),get_C_ulong(taskData, P3));
        
    case XCALL_XLoadFont:
        return LoadFont(taskData, GetDS(taskData, XP1),GetString(P2));
        
    case XCALL_XLoadQueryFont:
        return LoadQueryFont(taskData, GetDS(taskData, XP1),GetString(P2));
        
    case XCALL_XQueryFont:
        return QueryFont(taskData, GetDS(taskData, XP1),GetFont(taskData, XP1));
        
    case XCALL_XSetFontPath:
        SetFontPath(taskData, GetDisplay(taskData, XP1),SAVE(P2));
        break;
        
        /* Grabbing 450 */
        
        /* Graphics Context 500 */
        
    case XCALL_DefaultGC:
        return GetDefaultGC(taskData, GetDS(taskData, XP1));
        
    case XCALL_UpdateGC:
        ChangeGC(taskData, GCObject(XP1),get_C_ulong(taskData, P2),P3);
        break;
        
    case XCALL_XCreateGC:
        return CreateGC(taskData, GetDS(taskData, XP1),GetDrawable(taskData, XP1));
        
    case XCALL_XSetClipRectangles:
        SetClipRectangles(taskData, GetDisplay(taskData, XP1),
            GetGC(taskData, XP1),
            GetPointX(taskData, P2),
            GetPointY(taskData, P2),
            SAVE(P3),
            get_C_ulong(taskData, P4));
        break;
        
    case XCALL_XSetDashes:
        SetDashes(taskData, GetDisplay(taskData, XP1),
            GetGC(taskData, XP1),
            get_C_ulong(taskData, P2),
            SAVE(P3));
        break;
        
        /* Images 550 */
        
    case XCALL_XAddPixel:
        AddPixel(GetXImage(taskData, GetDisplay(taskData, XP1),P2),get_C_ulong(taskData, P3));
        break;
        
    case XCALL_XGetImage:
        return GetImage(taskData, GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetRectX(taskData, P2),
            GetRectY(taskData, P2),
            GetRectW(taskData, P2),
            GetRectH(taskData, P2),
            get_C_ulong(taskData, P3),
            get_C_long(taskData, P4)); 
        
    case XCALL_XGetPixel:
        return GetPixel(taskData, GetXImage(taskData, GetDisplay(taskData, XP1),P2),
            GetPointX(taskData, P3),
            GetPointY(taskData, P3));
        
    case XCALL_XGetSubImage:
        GetSubImage(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetRectX(taskData, P2),
            GetRectY(taskData, P2),
            GetRectW(taskData, P2),
            GetRectH(taskData, P2),
            get_C_ulong(taskData, P3),
            get_C_long(taskData, P4),
            GetXImage(taskData, GetDisplay(taskData, XP1),P5),
            GetPointX(taskData, P6),
            GetPointY(taskData, P6));
        break;
        
    case XCALL_XPutImage:
        PutImage(GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetGC(taskData, XP2),
            GetXImage(taskData, GetDisplay(taskData, XP1),P3),
            GetPointX(taskData, P4),
            GetPointY(taskData, P4),
            GetRectX(taskData, P5),
            GetRectY(taskData, P5),
            GetRectW(taskData, P5),
            GetRectH(taskData, P5));
        break;
        
    case XCALL_XPutPixel:
        PutPixel(GetXImage(taskData, GetDisplay(taskData, XP1),P2),
            GetPointX(taskData, P3),
            GetPointY(taskData, P3),
            get_C_ulong(taskData, P4));
        break;
        
    case XCALL_XSubImage:
        return SubImage(taskData, GetXImage(taskData, GetDisplay(taskData, XP1),P2),
            GetRectX(taskData, P3),
            GetRectY(taskData, P3),
            GetRectW(taskData, P3),
            GetRectH(taskData, P3));
        
    case XCALL_BitmapBitOrder:
        return Make_arbitrary_precision(taskData, MLImageOrder(BitmapBitOrder(GetDisplay(taskData, XP1))));
        
    case XCALL_BitmapPad:
        return Make_arbitrary_precision(taskData, BitmapPad(GetDisplay(taskData, XP1)));
        
    case XCALL_BitmapUnit:
        return Make_arbitrary_precision(taskData, BitmapUnit(GetDisplay(taskData, XP1)));
        
    case XCALL_ByteOrder:
        return Make_arbitrary_precision(taskData, MLImageOrder(ImageByteOrder(GetDisplay(taskData, XP1))));
        
        /* Keyboard 600 */
    case XCALL_XLookupString:
        return LookupString(taskData, GetDisplay(taskData, XP1),get_C_ulong(taskData, P2),get_C_ulong(taskData, P3));
        
    case XCALL_XQueryKeymap:
        return QueryKeymap(taskData, GetDisplay(taskData, XP1));
        
    case XCALL_IsCursorKey:
        return Make_bool(IsCursorKey(get_C_ulong(taskData, P1)));
        
    case XCALL_IsFunctionKey:
        return Make_bool(IsFunctionKey(get_C_ulong(taskData, P1)));
        
    case XCALL_IsKeypadKey:
        return Make_bool(IsKeypadKey(get_C_ulong(taskData, P1)));
        
    case XCALL_IsMiscFunctionKey:
        return Make_bool(IsMiscFunctionKey(get_C_ulong(taskData, P1)));
        
    case XCALL_IsModifierKey:
        return Make_bool(IsModifierKey(get_C_ulong(taskData, P1)));
        
    case XCALL_IsPFKey:
        return Make_bool(IsPFKey(get_C_ulong(taskData, P1)));
        
        /* Output Buffer 650 */
    case XCALL_XFlush:
        XFlush(GetDisplay(taskData, XP1));
        break;
        
    case XCALL_XSync:
        XSync(GetDisplay(taskData, XP1),get_C_ulong(taskData, P2));
        break;
        
        /* Pointers 700 */
    case XCALL_XQueryPointer:
        return QueryPointer(taskData, GetDS(taskData, XP1),GetWindow(taskData, XP1));
        
        /* Regions 750*/
        
        /* SAVE Set 800 */
        
        /* Screen Saver 850 */
    case XCALL_XActivateScreenSaver:
        XActivateScreenSaver(GetDisplay(taskData, XP1));
        break;
        
    case XCALL_XForceScreenSaver:
        XForceScreenSaver(GetDisplay(taskData, XP1),get_C_ulong(taskData, P2));
        break;
        
    case XCALL_XGetScreenSaver:
        return GetScreenSaver(taskData, GetDisplay(taskData, XP1));
        
    case XCALL_XResetScreenSaver:
        XResetScreenSaver(GetDisplay(taskData, XP1));
        break;
        
    case XCALL_XSetScreenSaver:
        XSetScreenSaver(GetDisplay(taskData, XP1),
            get_C_long(taskData, P2),
            get_C_long(taskData, P3),
            get_C_ulong(taskData, P4),
            get_C_ulong(taskData, P5));
        break;
        
        /* Standard Geometry 900 */
    case XCALL_XTranslateCoordinates:
        return TranslateCoordinates(taskData, GetDS(taskData, XP1),
            GetWindow(taskData, XP1),
            GetWindow(taskData, XP2),
            GetPointX(taskData, P3),
            GetPointY(taskData, P3));
        
        /* Text 950 */
    case XCALL_XTextExtents:
        return TextExtents(taskData, GetFontStruct(taskData, P1),GetString(P2));
        
    case XCALL_XTextExtents16:
        return TextExtents16(taskData, GetFontStruct(taskData, P1),SAVE(P2));
        
    case XCALL_XTextWidth:
        return TextWidth(taskData, GetFontStruct(taskData, P1),GetString(P2));
        
    case XCALL_XTextWidth16:
        return TextWidth16(taskData, GetFontStruct(taskData, P1),SAVE(P2));
        
        /* Tiles, Pixmaps, Stipples and Bitmaps 1000 */
    case XCALL_XCreateBitmapFromData:
        {
            Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
            CheckZeroRect(taskData, P3);
            return EmptyPixmap(taskData, dsHandle,
                XCreateBitmapFromData(
                DEREFDISPLAYHANDLE(dsHandle)->display,
                GetDrawable(taskData, XP1),     /* drawable */
                GetString(P2)->chars, /* data     */
                GetRectW(taskData, P3),         /* width    */
                GetRectH(taskData, P3)));       /* height   */
        }
        
    case XCALL_XCreatePixmap:
        {
            Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
            CheckZeroRect(taskData, P2);
            return EmptyPixmap(taskData, dsHandle,
                XCreatePixmap(
                DEREFDISPLAYHANDLE(dsHandle)->display,
                GetDrawable(taskData, XP1),  /* drawable */
                GetRectW(taskData, P2),      /* width    */
                GetRectH(taskData, P2),      /* height   */
                get_C_ulong(taskData, P3))); /* depth    */
        }
        
    case XCALL_XCreatePixmapFromBitmapData:
        {
            Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
            CheckZeroRect(taskData, P3);
            
            return EmptyPixmap(taskData, dsHandle,
                XCreatePixmapFromBitmapData(
                DEREFDISPLAYHANDLE(dsHandle)->display,
                GetDrawable(taskData, XP1),     /* drawable */
                GetString(P2)->chars, /* data     */
                GetRectW(taskData, P3),         /* width    */
                GetRectH(taskData, P3),         /* height   */
                get_C_ulong(taskData, P4),      /* foreground */
                get_C_ulong(taskData, P5),      /* background */
                get_C_ulong(taskData, P6)));    /* depth    */
        }
        
    case XCALL_XQueryBestStipple:
        CheckZeroRect(taskData, P2);
        return QueryBest(taskData, XQueryBestStipple,
            GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetRectW(taskData, P2),
            GetRectH(taskData, P2));
        
    case XCALL_XQueryBestTile:
        CheckZeroRect(taskData, P2);
        return QueryBest(taskData, XQueryBestTile,
            GetDisplay(taskData, XP1),
            GetDrawable(taskData, XP1),
            GetRectW(taskData, P2),
            GetRectH(taskData, P2));
        
    case XCALL_XReadBitmapFile:
        return ReadBitmap(taskData, GetDS(taskData, XP1),GetDrawable(taskData, XP1),GetString(P2));
        
    case XCALL_XWriteBitmapFile:
        CheckZeroRect(taskData, P3);
        return WriteBitmapFile(taskData, GetString(XP1),
            GetDisplay(taskData, XP2),
            GetPixmap(taskData, XP2),
            GetRectW(taskData, P3),
            GetRectH(taskData, P3),
            GetPointX(taskData, P4),
            GetPointY(taskData, P4));
        
        /* User Preferences 1050 */
    case XCALL_XAutoRepeatOff:
        XAutoRepeatOff(GetDisplay(taskData, XP1));
        break;
        
    case XCALL_XAutoRepeatOn:
        XAutoRepeatOn (GetDisplay(taskData, XP1));
        break;
        
    case XCALL_XBell:
        XBell(GetDisplay(taskData, XP1),get_C_short(taskData, P2));
        break;
        
    case XCALL_XGetDefault:
        return GetDefault(taskData, GetDisplay(taskData, XP1),GetString(P2),GetString(P3));
        
        /* Window Attributes 1100 */
    case XCALL_ChangeWindow:
        ChangeWindowAttributes(taskData, WindowObject(XP1),get_C_ulong(taskData, P2),P3);
        break;
        
    case XCALL_XGetGeometry:
        return GetGeometry(taskData, GetDS(taskData, XP1),GetDrawable(taskData, XP1));
        
    case XCALL_XGetWindowAttributes:
        return GetWindowAttributes(taskData, GetDS(taskData, XP1),GetDrawable(taskData, XP1));
        
    case XCALL_XSetWindowBorderWidth:
        XSetWindowBorderWidth(GetDisplay(taskData, XP1),GetWindow(taskData, XP1),get_C_ulong(taskData, P2));
        break;
        
        /* Window Configuration 1150 */
    case XCALL_XCirculateSubwindows:
        XCirculateSubwindows(GetDisplay(taskData, XP1),GetWindow(taskData, XP1),get_C_ulong(taskData, P2));
        break;
        
    case XCALL_XConfigureWindow:
        ConfigureWindow(taskData, GetDisplay(taskData, XP1),GetWindow(taskData, XP1), P2);
        break;
        
    case XCALL_XLowerWindow:
        XLowerWindow(GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        break;
        
    case XCALL_XMapRaised:
        XMapRaised(GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        break;
        
    case XCALL_XMapSubwindows:
        XMapSubwindows(GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        break;
        
    case XCALL_XMapWindow:
        XMapWindow(GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        break;
        
    case XCALL_XMoveResizeWindow:
        CheckZeroRect(taskData, P3);
        XMoveResizeWindow(GetDisplay(taskData, XP1),
            GetWindow(taskData, XP1),
            GetPointX(taskData, P2),
            GetPointY(taskData, P2), 
            GetRectW(taskData, P3),
            GetRectH(taskData, P3));
        break;
        
    case XCALL_XMoveWindow:
        XMoveWindow(GetDisplay(taskData, XP1),
            GetWindow(taskData, XP1),
            GetPointX(taskData, P2),
            GetPointY(taskData, P2));
        break;
        
    case XCALL_XQueryTree:
        return QueryTree(taskData,GetDS(taskData, XP1),GetWindow(taskData, XP1));
        
    case XCALL_XRaiseWindow:
        XRaiseWindow(GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        break;
        
    case XCALL_XReparentWindow:
        XReparentWindow(GetDisplay(taskData, XP1),
            GetWindow(taskData, XP1),
            GetWindow(taskData, XP2),
            GetPointX(taskData, P3),
            GetPointY(taskData, P3));
        break;
        
    case XCALL_XResizeWindow:
        CheckZeroRect(taskData, P2);
        XResizeWindow(GetDisplay(taskData, XP1),
            GetWindow(taskData, XP1),
            GetRectW(taskData, P2),
            GetRectH(taskData, P2));
        break;
        
    case XCALL_XRestackWindows:
        RestackWindows(taskData, SAVE(P1));
        break;
        
    case XCALL_XUnmapSubwindows:
        XUnmapSubwindows(GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        break;
        
    case XCALL_XUnmapWindow:
        XUnmapWindow(GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        break;
        
        /* Window Existence 1200 */
    case XCALL_RootWindow:
        { Handle dsHandle /* Handle to (X_Display_Object *) */ = GetDS(taskData, XP1);
        return EmptyWindow(taskData, dsHandle,
            RootWindow(DEREFDISPLAYHANDLE(dsHandle)->display,
            DEREFDISPLAYHANDLE(dsHandle)->screen));
        }
        
    case XCALL_DestroyXObject:
        DestroyXObject(XP1);
        break;
        
    case XCALL_XDestroySubwindows:
        DestroySubwindows(XP1);
        break;
        
    case XCALL_XCreateSimpleWindow:
        CheckZeroRect(taskData, P3);
        return CreateSimpleWindow(taskData,
            SAVE(XP1),       /* parent      */
            GetPointX(taskData, P2),   /* x           */
            GetPointY(taskData, P2),   /* y           */
            GetRectW(taskData, P3),    /* w           */
            GetRectH(taskData, P3),    /* h           */
            get_C_ulong(taskData, P4), /* borderWidth */
            get_C_ulong(taskData, P5), /* border      */
            get_C_ulong(taskData, P6), /* background  */
            SAVE(P7),        /* handler     */
            SAVE(P8));       /* state       */
        
    case XCALL_XCreateWindow:
        CheckZeroRect(taskData, P3);
        return CreateWindow(taskData,
            SAVE(XP1),       /* parent      */
            GetPointX(taskData, P2),   /* x           */
            GetPointY(taskData, P2),   /* y           */
            GetRectW(taskData, P3),    /* w           */
            GetRectH(taskData, P3),    /* h           */
            get_C_ulong(taskData, P4), /* borderWidth */
            get_C_ulong(taskData, P5), /* depth       */
            get_C_ulong(taskData, P6), /* class       */
            GetVisual(taskData, XP7),  /* visual      */
            SAVE(P8),        /* handler     */
            SAVE(P9));       /* state       */
        
        /* Window Manager 1250 */
    case XCALL_XSetProperty:
        SetProperty(taskData, GetDisplay(taskData, XP1),
            GetWindow(taskData, XP1),
            get_C_ulong(taskData, P2),
            get_C_ulong(taskData, P3),
            SAVE(P4),
            get_C_ulong(taskData, P5));
        break;
        
    case XCALL_XGetTextProperty:
        return GetTextProperty(taskData, GetDisplay(taskData, XP1),GetWindow(taskData, XP1),get_C_ulong(taskData, P2));
        
    case XCALL_XGetWMHints:
        return GetWMHints(taskData, GetDS(taskData, XP1),GetWindow(taskData, XP1));
        
    case XCALL_XGetWMSizeHints:
        return GetWMSizeHints(taskData, GetDisplay(taskData, XP1),GetWindow(taskData, XP1),get_C_ulong(taskData, P2));
        
    case XCALL_XGetIconSizes:
        return GetIconSizes(taskData, GetDisplay(taskData, XP1),GetWindow(taskData, XP1));
        
    case XCALL_XGetTransientForHint:
        return GetTransientForHint(taskData, GetDS(taskData, XP1),GetWindow(taskData, XP1));
        
    case XCALL_XGetWMColormapWindows:
        return GetWMColormapWindows(taskData, GetDS(taskData, XP1),GetWindow(taskData, XP1));
        
    case XCALL_XGetRGBColormaps:
        return GetRGBColormaps(taskData, GetDS(taskData, XP1),GetWindow(taskData, XP1),get_C_ulong(taskData, P2));
        
    case XCALL_XWMGeometry:
        return WMGeometry(taskData, GetDS(taskData, XP1),
            GetString(P2),
            GetString(P3),
            get_C_ulong(taskData, P4),
            P5);
        
        /* Miscellaneous 1300 */
    case XCALL_GetID:
        return GetID(taskData, XP1);
        
    case XCALL_ResourceExists:
        return Make_bool(ResourceExists(XP1));
        
    case XCALL_GetDisplay:
        return GetDS(taskData, XP1);
        
        /******************************************************************************/
        /*                                                                            */
        /*      Xt Calls                                                              */
        /*                                                                            */
        /******************************************************************************/
    case XCALL_NoWidget:
        return EmptyWidget(taskData, SAVE(ListNull), (Widget)NULL);
        
    case XCALL_AppInitialise:
        return AppInitialise(taskData, P1, /* display name      */
            P2, /* application name  */
            P3, /* application class */
            SAVE(P4),     /* Fallback list     */
            SAVE(P5)      /* Arg list          */);
        
    case XCALL_XtRealizeWidget:
        XtRealizeWidget(GetWidget(taskData, XP1));
        break;
        
    case XCALL_XtManageChildren:
        ManageChildren(taskData, SAVE(P1));
        break;
        
    case XCALL_XtUnmanageChildren:
        UnmanageChildren(taskData, SAVE(P1));
        break;
        
    case XCALL_XtDestroyWidget:
        {
            Widget w = GetWidget(taskData, XP1);
            XtDestroyWidget(w);
            /* The following test seems necessary - sometimes the callback from  */
            /* the above call destroys the widget, sometimes it doesn't. I think */
            /* it always should, and I can't work out why this strange behaviour */
            /* occurs.                                               SPF 9/12/93 */
            if (ResourceExists(XP1)) 
            {
                DestroyXObject(XP1);
                PurgeCCallbacks((X_Widget_Object *)XP1,w);
            }
            break;
        }
        
    case XCALL_SetCallbacks:
        SetCallbacks (taskData, WidgetObject(taskData, XP1),P2,P3);
        break; /* WidgetObject added SPF */
        
    case XCALL_XtSetValues:
        SetValues(taskData, GetWidget(taskData, XP1),SAVE(P2));
        break;
        
    case XCALL_GetValue:
        return GetValue(taskData, GetDS(taskData, XP1),GetWidget(taskData, XP1),P2);
        
    case XCALL_XtParent:
        return EmptyWidget(taskData, GetDS(taskData, XP1),XtParent(GetWidget(taskData, XP1)));
        
    case XCALL_XtWindow:
        return EmptyWindow(taskData, GetDS(taskData, XP1),WindowOfWidget(GetWidget(taskData, XP1)));
        
    case XCALL_XtDisplay:
        return GetDS(taskData, XP1);
        
    case XCALL_XtUnrealizeWidget:
        XtUnrealizeWidget(GetWidget(taskData, XP1)); break;
        
    case XCALL_XtName:
        return Make_string(XtName(GetWidget(taskData, XP1)));
        
    case XCALL_XtParseTranslationTable:
        return ParseTranslationTable(taskData, GetString(XP1));
        
    case XCALL_XtOverrideTranslations:
        XtOverrideTranslations(GetWidget(taskData, XP1),GetTrans(taskData, XP2));
        break;
        
    case XCALL_XtAugmentTranslations:
        XtAugmentTranslations(GetWidget(taskData, XP1),GetTrans(taskData, XP2));
        break;
        
    case XCALL_XtUninstallTranslations: XtUninstallTranslations(GetWidget(taskData, XP1)); break;
        
    /*
    case XCALL_XtTranslateTablePrint: _XtTranslateTablePrint(GetTrans(taskData, XP1)); break;
        */
        
    case XCALL_XtCreatePopupShell:
        return CreatePopupShell(taskData, GetString(XP1),GetDS(taskData, XP2),GetWidget(taskData, XP2),SAVE(P3));
        
    case XCALL_InsertWidgetTimeout:
        InsertWidgetTimeout(taskData, WidgetObject(taskData, XP1),get_C_ulong(taskData, P2),P3,P4);
        break; /* WidgetObject added SPF */
        
    case XCALL_GetWidgetState:
        return SAVE(WidgetObjectToken(XP1)->state); /* was WidgetObject(XP1) (SPF) */
        
    case XCALL_SetWidgetState:
        WidgetObjectToken(XP1)->state = P2;
        break;  /* was WidgetObject(XP1) (SPF) */
        
    case XCALL_XtSetSensitive:
        XtSetSensitive(GetWidget(taskData, XP1),get_C_ulong(taskData, P2));
        break;
        
    case XCALL_XtIsSensitive:
        return Make_bool(XtIsSensitive(GetWidget(taskData, XP1)));
        
    case XCALL_GetSubresources:
        return GetSubresources(taskData, GetDS(taskData, XP1),
            GetWidget(taskData, XP1),
            GetString(P2),
            GetString(P3),
            SAVE(P4));
        
    case XCALL_Cast:
        return SAVE(P1);
        
    case XCALL_XtPopup:
        XtPopup(GetWidget(taskData, XP1),GetXtGrabKind(taskData, P2));
        break;
        
    case XCALL_XtPopdown:
        XtPopdown(GetWidget(taskData, XP1));
        break;
        
    case XCALL_XtMapWidget:
        XtMapWidget(GetRealizedWidget(taskData, (char *) "XtMapWidget",XP1));
        break;
        
    case XCALL_XtUnmapWidget:
        XtUnmapWidget(GetRealizedWidget(taskData, (char *) "XtUnmapWidget",XP1));
        break;
        
    case XCALL_XtIsManaged:
        return Make_bool(XtIsManaged(GetWidget(taskData, XP1)));
        
    case XCALL_XtIsRealized:
        return Make_bool(XtIsRealized(GetWidget(taskData, XP1)));
        
        /* Added DCJM. */
    case XCALL_XtGetApplicationResources:
        return GetApplicationResources (taskData, GetDS(taskData, XP1),GetWidget(taskData, XP1),SAVE(P2) ) ;
        
    case XCALL_XtAddEventHandler:
        AddEventhandler (taskData, WidgetObject(taskData, XP1), get_C_ulong(taskData, P2),
            get_C_ulong(taskData, P3), SAVE(P4)); break;
        
        
        /******************************************************************************/
        /*                                                                            */
        /*      Motif Calls - widget creation                                         */
        /*                                                                            */
        /******************************************************************************/
        /* Motif 4000 */
        
#define XMCREATE(number,name) \
    case number: return CreateXm(taskData, name, (char *)   \
#name   " failed",  \
    GetDS(taskData, XP1), \
    GetWidget(taskData, XP1), \
    GetString(P2),      \
        SAVE(P3))
        
        XMCREATE(XCALL_XmCreateArrowButton,XmCreateArrowButton);
        XMCREATE(XCALL_XmCreateArrowButtonGadget,XmCreateArrowButtonGadget);
        XMCREATE(XCALL_XmCreateBulletinBoard,XmCreateBulletinBoard);
        XMCREATE(XCALL_XmCreateBulletinBoardDialog,XmCreateBulletinBoardDialog);
        XMCREATE(XCALL_XmCreateCascadeButton,XmCreateCascadeButton);
        XMCREATE(XCALL_XmCreateCascadeButtonGadget,XmCreateCascadeButtonGadget);
        XMCREATE(XCALL_XmCreateCommand,XmCreateCommand);
        XMCREATE(XCALL_XmCreateDialogShell,XmCreateDialogShell);
        XMCREATE(XCALL_XmCreateDrawingArea,XmCreateDrawingArea);
        XMCREATE(XCALL_XmCreateDrawnButton,XmCreateDrawnButton);
        XMCREATE(XCALL_XmCreateErrorDialog,XmCreateErrorDialog);
        XMCREATE(XCALL_XmCreateFileSelectionBox,XmCreateFileSelectionBox);
        XMCREATE(XCALL_XmCreateFileSelectionDialog,XmCreateFileSelectionDialog);
        XMCREATE(XCALL_XmCreateForm,XmCreateForm);
        XMCREATE(XCALL_XmCreateFormDialog,XmCreateFormDialog);
        XMCREATE(XCALL_XmCreateFrame,XmCreateFrame);
        XMCREATE(XCALL_XmCreateInformationDialog,XmCreateInformationDialog);
        XMCREATE(XCALL_XmCreateLabel,XmCreateLabel);
        XMCREATE(XCALL_XmCreateLabelGadget,XmCreateLabelGadget);
        XMCREATE(XCALL_XmCreateList,XmCreateList);
        XMCREATE(XCALL_XmCreateMainWindow,XmCreateMainWindow);
        XMCREATE(XCALL_XmCreateMenuBar,XmCreateMenuBar);
        XMCREATE(XCALL_XmCreateMenuShell,XmCreateMenuShell);
        XMCREATE(XCALL_XmCreateMessageBox,XmCreateMessageBox);
        XMCREATE(XCALL_XmCreateMessageDialog,XmCreateMessageDialog);
        XMCREATE(XCALL_XmCreateOptionMenu,XmCreateOptionMenu);
        XMCREATE(XCALL_XmCreatePanedWindow,XmCreatePanedWindow);
        XMCREATE(XCALL_XmCreatePopupMenu,XmCreatePopupMenu);
        XMCREATE(XCALL_XmCreatePromptDialog,XmCreatePromptDialog);
        XMCREATE(XCALL_XmCreatePulldownMenu,XmCreatePulldownMenu);
        XMCREATE(XCALL_XmCreatePushButton,XmCreatePushButton);
        XMCREATE(XCALL_XmCreatePushButtonGadget,XmCreatePushButtonGadget);
        XMCREATE(XCALL_XmCreateQuestionDialog,XmCreateQuestionDialog);
        XMCREATE(XCALL_XmCreateRadioBox,XmCreateRadioBox);
        XMCREATE(XCALL_XmCreateRowColumn,XmCreateRowColumn);
        XMCREATE(XCALL_XmCreateScale,XmCreateScale);
        XMCREATE(XCALL_XmCreateScrollBar,XmCreateScrollBar);
        XMCREATE(XCALL_XmCreateScrolledList,XmCreateScrolledList);
        XMCREATE(XCALL_XmCreateScrolledText,XmCreateScrolledText);
        XMCREATE(XCALL_XmCreateScrolledWindow,XmCreateScrolledWindow);
        XMCREATE(XCALL_XmCreateSelectionBox,XmCreateSelectionBox);
        XMCREATE(XCALL_XmCreateSelectionDialog,XmCreateSelectionDialog);
        XMCREATE(XCALL_XmCreateSeparator,XmCreateSeparator);
        XMCREATE(XCALL_XmCreateSeparatorGadget,XmCreateSeparatorGadget);
        XMCREATE(XCALL_XmCreateSimpleCheckBox,XmCreateSimpleCheckBox);
        XMCREATE(XCALL_XmCreateSimpleMenuBar,XmCreateSimpleMenuBar);
        XMCREATE(XCALL_XmCreateSimpleOptionMenu,XmCreateSimpleOptionMenu);
        XMCREATE(XCALL_XmCreateSimplePopupMenu,XmCreateSimplePopupMenu);
        XMCREATE(XCALL_XmCreateSimplePulldownMenu,XmCreateSimplePulldownMenu);
        XMCREATE(XCALL_XmCreateSimpleRadioBox,XmCreateSimpleRadioBox);
        XMCREATE(XCALL_XmCreateText,XmCreateText);
        XMCREATE(XCALL_XmCreateTextField,XmCreateTextField);
        XMCREATE(XCALL_XmCreateToggleButton,XmCreateToggleButton);
        XMCREATE(XCALL_XmCreateToggleButtonGadget,XmCreateToggleButtonGadget);
        XMCREATE(XCALL_XmCreateWarningDialog,XmCreateWarningDialog);
        XMCREATE(XCALL_XmCreateWorkArea,XmCreateWorkArea);
        XMCREATE(XCALL_XmCreateWorkingDialog,XmCreateWorkingDialog);
        
#undef XMCREATE
        
        /******************************************************************************/
        /*                                                                            */
        /*      Motif Calls - miscellaneous                                           */
        /*                                                                            */
        /******************************************************************************/
    case XCALL_XmCascadeButtonHighlight:
        XmCascadeButtonHighlight(GetWidget(taskData, XP1),get_C_ulong(taskData, P2));
        break;
        
    case XCALL_XmCommandError:
        CommandError(taskData, GetWidget(taskData, XP1),P2);
        break;
        
    case XCALL_XmCommandGetChild:
        return EmptyWidget(taskData, GetDS(taskData, XP1),
            XmCommandGetChild(GetWidget(taskData, XP1),get_C_ulong(taskData, P2)));
        
    case XCALL_XmFileSelectionBoxGetChild:
        return EmptyWidget(taskData, GetDS(taskData, XP1),
            XmFileSelectionBoxGetChild(GetWidget(taskData, XP1),get_C_ulong(taskData, P2)));
        
    case XCALL_XmFileSelectionDoSearch:
        FileSelectionDoSearch(taskData, GetWidget(taskData, XP1),P2);
        break;
        
    case XCALL_XmIsSomething:
        return XmIsSomething(taskData, get_C_ulong(taskData, P1),GetWidget(taskData, XP2));
        
    case XCALL_XmMainWindowSetAreas:
        XmMainWindowSetAreas(GetWidget(taskData, XP1),
            GetNWidget(taskData, XP2),
            GetNWidget(taskData, XP3),
            GetNWidget(taskData, XP4),
            GetNWidget(taskData, XP5),
            GetNWidget(taskData, XP6));
        break;
        
    case XCALL_XmMainWindowSepX:
        switch(get_C_ulong(taskData, P2))
        {
        case 1:
            return EmptyWidget(taskData, GetDS(taskData, XP1),XmMainWindowSep1(GetWidget(taskData, XP1)));
            
        case 2:
            return EmptyWidget(taskData, GetDS(taskData, XP1),XmMainWindowSep2(GetWidget(taskData, XP1)));
            
        default:
            return EmptyWidget(taskData, GetDS(taskData, XP1),XmMainWindowSep3(GetWidget(taskData, XP1)));
        }
        
        case XCALL_XmMessageBoxGetChild:
            return EmptyWidget(taskData, GetDS(taskData, XP1),
                XmMessageBoxGetChild(GetWidget(taskData, XP1),get_C_ulong(taskData, P2)));
            
        case XCALL_XmOptionButtonGadget:
            return EmptyWidget(taskData, GetDS(taskData, XP1),XmOptionButtonGadget(GetWidget(taskData, XP1)));
            
        case XCALL_XmOptionLabelGadget:
            return EmptyWidget(taskData, GetDS(taskData, XP1),XmOptionLabelGadget (GetWidget(taskData, XP1)));
            
        case XCALL_XmSelectionBoxGetChild:
            return EmptyWidget(taskData, GetDS(taskData, XP1),
                XmSelectionBoxGetChild(GetWidget(taskData, XP1),get_C_ulong(taskData, P2)));
            
        case XCALL_XmSetMenuCursor:
            XmSetMenuCursor(GetDisplay(taskData, XP1),GetCursor(taskData, XP2)); break;
            
        case XCALL_XmScrolledWindowSetAreas:
            XmScrolledWindowSetAreas(GetWidget(taskData, XP1),
                GetNWidget(taskData, XP2),
                GetNWidget(taskData, XP3),
                GetNWidget(taskData, XP4));
            break;
            
            
            /******************************************************************************/
            /*                                                                            */
            /*      Operations on XmText widgets                                          */
            /*                                                                            */
            /******************************************************************************/
            
#define TextWidgetToLong(func) \
        case XCALL_ ## func : \
            return(WidgetToLong(taskData,(char *) #func,GetTextWidget,func,XP1))
            
#define TextWidgetToInt(func) \
        case XCALL_ ## func : \
            return(WidgetToInt(taskData,(char *) #func,GetTextWidget,func,XP1))
            
#define TextWidgetToBool(func) \
        case XCALL_ ## func : \
            return(WidgetToBool(taskData,(char *) #func,GetTextWidget,func,XP1))
            
#define TextWidgetToString(func) \
        case XCALL_ ## func : \
            return(WidgetToString(taskData,(char *) #func,GetTextWidget,func,XP1))
            
#define TextWidgetIntAction(func) \
        case XCALL_ ## func : \
        WidgetIntAction(taskData,(char *) #func,GetTextWidget,func,XP1,P2); \
            break
            
#define TextWidgetLongAction(func) \
        case XCALL_ ## func : \
        WidgetLongAction(taskData,(char *) #func,GetTextWidget,func,XP1,P2); \
            break
            
#define TextWidgetBoolAction(func) \
        case XCALL_ ## func : \
        WidgetBoolAction(taskData,(char *) #func,GetTextWidget,func,XP1,P2); \
            break
            
            
            /* XmTextClearSelection not supported */
            /* XmTextCopy not supported */
            /* XmTextCut not supported */
#ifdef LESSTIF_VERSION
            /* This is not supported in LessTif, at least not 0.89. */
        case XCALL_XmTextGetAddMode:
            RaiseXWindows(taskData, "XmTextGetAddMode: not implemented");
#else
            TextWidgetToBool(XmTextGetAddMode);
#endif
            TextWidgetToLong(XmTextGetCursorPosition);
            TextWidgetToInt(XmTextGetBaseline);
            TextWidgetToBool(XmTextGetEditable);
            TextWidgetToLong(XmTextGetInsertionPosition);
            TextWidgetToLong(XmTextGetLastPosition);
            TextWidgetToInt(XmTextGetMaxLength);
            TextWidgetToString(XmTextGetSelection);
            /* XmTextGetSelectionPosition not supported */
            TextWidgetToString(XmTextGetString);
            /* XmTextGetSource not supported */
            TextWidgetToLong(XmTextGetTopCharacter);
            
        case XCALL_XmTextInsert:
            {
          Widget w = GetTextWidget(taskData, (char *) "XmTextInsert",XP1);
                {
                    unsigned pos = get_C_ulong(taskData, P2);
                    PolyStringObject *s    = GetString(P3);
                    int   size   = s->length + 1;
                    char *buffer = (char *)alloca(size);
                    
                    Poly_string_to_C(s,buffer,size);
                    XmTextInsert(w,pos,buffer);
                    break;
                }
            }
            
            TextWidgetToBool(XmTextPaste); /* with side effect! */
            /* XmTextPosToXY not supported */
            TextWidgetToBool(XmTextRemove); /* with side effect! */
            
        case XCALL_XmTextReplace:
            {
          Widget w = GetTextWidget(taskData, (char *) "XmTextReplace",XP1);
                {
                    unsigned from_pos = get_C_ulong(taskData, P2);
                    unsigned to_pos   = get_C_ulong(taskData, P3);
                    PolyStringObject *s    = GetString(P4);
                    int   size   = s->length + 1;
                    char *buffer = (char *)alloca(size);
                    
                    Poly_string_to_C(s,buffer,size);
                    XmTextReplace(w,from_pos,to_pos,buffer);
                    break;
                }
            }
            
            TextWidgetIntAction(XmTextScroll); /* for side effect! */
            TextWidgetBoolAction(XmTextSetAddMode);
            TextWidgetLongAction(XmTextSetCursorPosition);
            TextWidgetBoolAction(XmTextSetEditable);
            /* XmTextSetHighlight not supported */
            TextWidgetLongAction(XmTextSetInsertionPosition);
            TextWidgetIntAction(XmTextSetMaxLength);
            /* XmTextSetSelection not supported */
            /* XmTextSetSource not supported */
            
            
            /* inlined SPF 15/2/94 */
        case XCALL_XmTextSetString:
            {
          Widget w = GetTextWidget(taskData, (char *) "XmTextSetString",XP1);
                {
                    PolyStringObject *s    = GetString(P2);
                    int   size   = s->length + 1;
                    char *buffer = (char *)alloca(size);
                    
                    Poly_string_to_C(s,buffer,size);
                    XmTextSetString(w,buffer);
                    break;
                }
            }
            
            TextWidgetLongAction(XmTextSetTopCharacter);
            TextWidgetLongAction(XmTextShowPosition);
            
        case XCALL_XmTextXYToPos:
            {
                Widget w = GetTextWidget(taskData, (char *) "XmTextXYToPos",XP1);
                {
                    int x = get_C_long(taskData, P2);
                    int y = get_C_long(taskData, P3);
                    return Make_int(XmTextXYToPos(w,x,y));
                }
            }
            
#undef TextWidgetToLong
#undef TextWidgetToInt
#undef TextWidgetToBool
#undef TextWidgetToString
#undef TextWidgetIntAction
#undef TextWidgetBoolAction
            
            /******************************************************************************/
            /*                                                                            */
            /*      Operations on XmTextField widgets                                     */
            /*                                                                            */
            /******************************************************************************/
            
#define TextFieldWidgetToLong(func) \
        case XCALL_ ## func : \
            return(WidgetToLong(taskData, (char *) #func,GetTextFieldWidget,func,XP1))
            
            
#define TextFieldWidgetToInt(func) \
        case XCALL_ ## func : \
            return(WidgetToInt(taskData, (char *) #func,GetTextFieldWidget,func,XP1))
            
#define TextFieldWidgetToBool(func) \
        case XCALL_ ## func : \
            return(WidgetToBool(taskData, (char *) #func,GetTextFieldWidget,func,XP1))
            
#define TextFieldWidgetToString(func) \
        case XCALL_ ## func : \
            return(WidgetToString(taskData, (char *) #func,GetTextFieldWidget,func,XP1))
            
#define TextFieldWidgetIntAction(func) \
        case XCALL_ ## func : \
        WidgetIntAction(taskData, (char *) #func,GetTextFieldWidget,func,XP1,P2); \
            break
            
#define TextFieldWidgetLongAction(func) \
        case XCALL_ ## func : \
        WidgetLongAction(taskData, (char *) #func,GetTextFieldWidget,func,XP1,P2); \
            break
            
#define TextFieldWidgetBoolAction(func) \
        case XCALL_ ## func : \
        WidgetBoolAction(taskData, (char *) #func,GetTextFieldWidget,func,XP1,P2); \
            break
            
            
            /* XmTextFieldClearSelection not supported */
            /* XmTextFieldCopy not supported */
            /* XmTextFieldCut not supported */
#ifdef LESSTIF_VERSION
            /* This is not supported in LessTif, at least not 0.89. */
        case XCALL_XmTextFieldGetAddMode:
            RaiseXWindows(taskData, "XmTextFieldGetAddMode: not implemented");
#else
            TextFieldWidgetToBool(XmTextFieldGetAddMode);
#endif
            TextFieldWidgetToInt(XmTextFieldGetBaseline);
            TextFieldWidgetToLong(XmTextFieldGetCursorPosition);
            TextFieldWidgetToBool(XmTextFieldGetEditable);
            TextFieldWidgetToLong(XmTextFieldGetInsertionPosition);
            TextFieldWidgetToLong(XmTextFieldGetLastPosition);
            TextFieldWidgetToInt(XmTextFieldGetMaxLength);
            TextFieldWidgetToString(XmTextFieldGetSelection);
            /* XmTextFieldGetSelectionPosition not supported */
            TextFieldWidgetToString(XmTextFieldGetString);
            /* XmTextFieldGetSource not supported */
            
        case XCALL_XmTextFieldInsert:
            {
                Widget w = GetTextFieldWidget(taskData, (char *) "XmTextFieldInsert",XP1);
                {
                    unsigned pos = get_C_ulong(taskData, P2);
                    PolyStringObject *s    = GetString(P3);
                    int   size   = s->length + 1;
                    char *buffer = (char *)alloca(size);
                    
                    Poly_string_to_C(s,buffer,size);
                    XmTextFieldInsert(w,pos,buffer);
                    break;
                }
            }
            
            TextFieldWidgetToBool(XmTextFieldPaste); /* for side effect! */
            /* XmTextFieldPosToXY not supported */
            TextFieldWidgetToBool(XmTextFieldRemove); /* for side effect! */
            
        case XCALL_XmTextFieldReplace:
            {
                Widget w = GetTextFieldWidget(taskData, (char *) "XmTextFieldReplace",XP1);
                {
                    unsigned from_pos = get_C_ulong(taskData, P2);
                    unsigned to_pos   = get_C_ulong(taskData, P3);
                    PolyStringObject *s    = GetString(P4);
                    int   size   = s->length + 1;
                    char *buffer = (char *)alloca(size);
                    
                    Poly_string_to_C(s,buffer,size);
                    XmTextFieldReplace(w,from_pos,to_pos,buffer);
                    break;
                }
            }
            
            TextFieldWidgetBoolAction(XmTextFieldSetAddMode);
            TextFieldWidgetLongAction(XmTextFieldSetCursorPosition);
            TextFieldWidgetBoolAction(XmTextFieldSetEditable);
            /* XmTextFieldSetHighlight not supported */
            TextFieldWidgetLongAction(XmTextFieldSetInsertionPosition);
            TextFieldWidgetIntAction(XmTextFieldSetMaxLength);
            /* XmTextFieldSetSelection not supported */
            
            
            /* inlined SPF 15/2/94 */
        case XCALL_XmTextFieldSetString:
            {
                Widget w = GetTextFieldWidget(taskData, (char *) "XmTextFieldSetString",XP1);
                {
                    PolyStringObject *s    = GetString(P2);
                    int   size   = s->length + 1;
                    char *buffer = (char *)alloca(size);
                    
                    Poly_string_to_C(s,buffer,size);
                    XmTextFieldSetString(w,buffer);
                    break;
                }
            }
            
            TextFieldWidgetLongAction(XmTextFieldShowPosition);  /* for side effect! */
            
        case XCALL_XmTextFieldXYToPos:
            {
                Widget w = GetTextFieldWidget(taskData, (char *) "XmTextFieldXYToPos",XP1);
                {
                    int x = get_C_long(taskData, P2);
                    int y = get_C_long(taskData, P3);
                    return Make_int(XmTextFieldXYToPos(w,x,y));
                }
            }
            
        case XCALL_XmTrackingLocate:
            return EmptyWidget(taskData, GetDS(taskData, XP1),
                XmTrackingLocate(GetWidget(taskData, XP1),GetCursor(taskData, XP2),get_C_ulong(taskData, P3)));
            
        case XCALL_XmUpdateDisplay:
            XmUpdateDisplay(GetWidget(taskData, XP1));
            break;
            
#undef TextFieldWidgetToLong
#undef TextFieldWidgetToInt
#undef TextFieldWidgetToBool
#undef TextFieldWidgetToString
#undef TextFieldWidgetIntAction
#undef TextFieldWidgetLongAction
#undef TextFieldWidgetBoolAction
            
            /******************************************************************************/
            /*                                                                            */
            /*      Operations on XmList widgets                                          */
            /*                                                                            */
            /******************************************************************************/
            
#define ListWidgetAction(func) \
        case XCALL_ ## func : \
        WidgetAction(taskData, (char *) #func,GetListWidget,func,XP1); \
            break
            
#define ListWidgetBoolAction(func) \
        case XCALL_ ## func : \
        WidgetBoolAction(taskData, (char *) #func,GetListWidget,func,XP1,P2); \
            break
            
#define ListWidgetXmstringAction(func) \
        case XCALL_ ## func : \
        WidgetXmstringAction(taskData, (char *) #func,GetListWidget,func,XP1,P2); \
            break
            
#define ListWidgetXmstringlistAction(func) \
        case XCALL_ ## func : \
        WidgetXmstringlistAction(taskData, (char *)  #func,GetListWidget,func,XP1,(ML_Cons_Cell *)XP2); \
            break
            
#define ListWidgetIntAction(func) \
        case XCALL_ ## func : \
      WidgetIntAction(taskData, (char *) #func,GetListWidget,func,XP1,P2); \
            break
            
#define ListWidgetIntIntAction(func) \
        case XCALL_ ## func : \
        WidgetIntIntAction(taskData, (char *) #func,GetListWidget,func,XP1,P2,P3); \
            break
            
#define ListWidgetXmstringIntAction(func) \
        case XCALL_ ## func : \
        WidgetXmstringIntAction(taskData, (char *) #func,GetListWidget,func,XP1,P2,P3); \
            break
            
#define ListWidgetIntBoolAction(func) \
        case XCALL_ ## func : \
        WidgetIntBoolAction(taskData, (char *) #func,GetListWidget,func,XP1,P2,P3); \
            break
            
#define ListWidgetXmstringBoolAction(func) \
        case XCALL_ ## func : \
        WidgetXmstringBoolAction(taskData, (char *) #func,GetListWidget,func,XP1,P2,P3); \
            break
            
#define ListWidgetXmstringlistIntAction(func) \
        case XCALL_ ## func : \
        WidgetXmstringlistIntAction(taskData, (char *) #func,GetListWidget,func,XP1,(ML_Cons_Cell *)XP2,P3); \
            break
            
#define ListWidgetXmstringToIntlist(func) \
        case XCALL_ ## func : \
            return(WidgetXmstringToIntlist(taskData, (char *)  #func,GetListWidget,func,XP1,P2))
            
#define ListWidgetToIntlist(func) \
        case XCALL_ ## func : \
            return(WidgetToIntlist(taskData, (char *) #func,GetListWidget,func,XP1))
            
#define ListWidgetXmstringToBool(func) \
        case XCALL_ ## func : \
            return(WidgetXmstringToBool(taskData, (char *) #func,GetListWidget,func,XP1,P2))
            
#define ListWidgetXmstringToInt(func) \
        case XCALL_ ## func : \
            return(WidgetXmstringToInt(taskData, (char *)  #func,GetListWidget,func,XP1,P2))
            
            /************************* Adding Items to List *******************************/
            ListWidgetXmstringIntAction(XmListAddItem);
            ListWidgetXmstringIntAction(XmListAddItemUnselected);
            ListWidgetXmstringlistIntAction(XmListAddItems); 
            
            /************************* Deleting Items from List ***************************/
            ListWidgetAction(XmListDeleteAllItems);
            ListWidgetXmstringAction(XmListDeleteItem);
            ListWidgetXmstringlistAction(XmListDeleteItems);
            ListWidgetIntAction(XmListDeletePos);
            ListWidgetIntIntAction(XmListDeleteItemsPos);
            
            /************************* Deselecting Items **********************************/
            ListWidgetAction(XmListDeselectAllItems);
            ListWidgetXmstringAction(XmListDeselectItem);
            ListWidgetIntAction(XmListDeselectPos);
            
            
            /************************* Query Functions ************************************/
            ListWidgetXmstringToIntlist(XmListGetMatchPos);
            ListWidgetToIntlist(XmListGetSelectedPos);
            ListWidgetXmstringToBool(XmListItemExists);
            ListWidgetXmstringToInt(XmListItemPos);
            
            /************************* Replacing Items in the List ************************/
    case XCALL_XmListReplaceItems: 
        /* Unpairing the strings is done in the ML, because it's easier there. */
        {
      Widget w = GetListWidget(taskData, (char *) "XmListReplaceItems",XP1);
            unsigned n    = ListLength(P2);
            unsigned n2   = ListLength(P3);
            
            if (n != n2) 
            {
                RaiseXWindows(taskData, "XmListReplaceItems: strings lists are different lengths");
            }
            else
            {
                XmString *oldstrings = (XmString *)alloca(n * sizeof(XmString));
                XmString *newstrings = (XmString *)alloca(n * sizeof(XmString));
                
                GetList4(taskData, P2,oldstrings,sizeof(XmString),GetXmString);
                GetList4(taskData, P3,newstrings,sizeof(XmString),GetXmString);
                XmListReplaceItems(w,oldstrings,n,newstrings);
                for (unsigned i = 0; i < n; i ++) XmStringFree(oldstrings[i]);
                for (unsigned i = 0; i < n; i ++) XmStringFree(newstrings[i]);
            }
            break;
        }
        
        ListWidgetXmstringlistIntAction(XmListReplaceItemsPos);
        
        /************************* Selecting Items in the List ************************/
        ListWidgetXmstringBoolAction(XmListSelectItem);
        ListWidgetIntBoolAction(XmListSelectPos);
        
        /************************* Set Add Mode ***************************************/
        ListWidgetBoolAction(XmListSetAddMode);
        
        /************************* Set Appearance *************************************/
        ListWidgetXmstringAction(XmListSetBottomItem); 
        ListWidgetIntAction(XmListSetBottomPos);
        ListWidgetIntAction(XmListSetHorizPos);
        ListWidgetXmstringAction(XmListSetItem); 
        ListWidgetIntAction(XmListSetPos);
        
#undef ListWidgetAction
#undef ListWidgetBoolAction
#undef ListWidgetXmstringAction
#undef ListWidgetXmstringlistAction
#undef ListWidgetIntAction
#undef ListWidgetIntIntAction
#undef ListWidgetXmstringIntAction
#undef ListWidgetXmstringBoolAction
#undef ListWidgetXmstringlistIntAction
#undef ListWidgetXmstringToIntlist
#undef ListWidgetToIntlist
#undef ListWidgetXmstringToBool
#undef ListWidgetXmstringToInt
        
        
        /* Calls added by DCJM. */
    case XCALL_XmMenuPosition:
        MenuPosition( GetWidget(taskData, XP1), get_C_ulong(taskData, P2), get_C_ulong(taskData, P3)); break; 
        /******************************************************************************/
        /*                                                                            */
        /*      Default case                                                          */
        /*                                                                            */
        /******************************************************************************/
        
    default: Crash ("Unimplemented X Windows call %d", code);
  }
  
  return Make_bool(False);
}

typedef struct
{
  int   code;
  const char *name;
} CodeName;

static CodeName ProtocolNames[] =
{
  { X_CreateWindow,"XCreateWindow"},
  { X_ChangeWindowAttributes,"XChangeWindowAttributes"},
  { X_GetWindowAttributes,"XGetWindowAttributes"},
  { X_DestroyWindow,"XDestroyWindow"},
  { X_DestroySubwindows,"XDestroySubwindows"},
  { X_ChangeSaveSet,"XChangeSAVESet"},
  { X_ReparentWindow,"XReparentWindow"},
  { X_MapWindow,"XMapWindow"},
  { X_MapSubwindows,"XMapSubwindows"},
  { X_UnmapWindow,"XUnmapWindow"},
  { X_UnmapSubwindows,"XUnmapSubwindows"},
  { X_ConfigureWindow,"XConfigureWindow"},
  { X_CirculateWindow,"XCirculateWindow"},
  { X_GetGeometry,"XGetGeometry"},
  { X_QueryTree,"XQueryTree"},
  { X_InternAtom,"XInternAtom"},
  { X_GetAtomName,"XGetAtomName"},
  { X_ChangeProperty,"XChangeProperty"},
  { X_DeleteProperty,"XDeleteProperty"},
  { X_GetProperty,"XGetProperty"},
  { X_ListProperties,"XListProperties"},
  { X_SetSelectionOwner,"XSetSelectionOwner"},
  { X_GetSelectionOwner,"XGetSelectionOwner"},
  { X_ConvertSelection,"XConvertSelection"},
  { X_SendEvent,"XSendEvent"},
  { X_GrabPointer,"XGrabPointer"},
  { X_UngrabPointer,"XUngrabPointer"},
  { X_GrabButton,"XGrabButton"},
  { X_UngrabButton,"XUngrabButton"},
  { X_ChangeActivePointerGrab,"XChangeActivePointerGrab"},
  { X_GrabKeyboard,"XGrabKeyboard"},
  { X_UngrabKeyboard,"XUngrabKeyboard"},
  { X_GrabKey,"XGrabKey"},
  { X_UngrabKey,"XUngrabKey"},
  { X_AllowEvents,"XAllowEvents"},
  { X_GrabServer,"XGrabServer"},
  { X_UngrabServer,"XUngrabServer"},
  { X_QueryPointer,"XQueryPointer"},
  { X_GetMotionEvents,"XGetMotionEvents"},
  { X_TranslateCoords,"XTranslateCoords"},
  { X_WarpPointer,"XWarpPointer"},
  { X_SetInputFocus,"XSetInputFocus"},
  { X_GetInputFocus,"XGetInputFocus"},
  { X_QueryKeymap,"XQueryKeymap"},
  { X_OpenFont,"XOpenFont"},
  { X_CloseFont,"XCloseFont"},
  { X_QueryFont,"XQueryFont"},
  { X_QueryTextExtents,"XQueryTextExtents"},
  { X_ListFonts,"XListFonts"},
  { X_ListFontsWithInfo,"XListFontsWithInfo"},
  { X_SetFontPath,"XSetFontPath"},
  { X_GetFontPath,"XGetFontPath"},
  { X_CreatePixmap,"XCreatePixmap"},
  { X_FreePixmap,"XFreePixmap"},
  { X_CreateGC,"XCreateGC"},
  { X_ChangeGC,"XChangeGC"},
  { X_CopyGC,"XCopyGC"},
  { X_SetDashes,"XSetDashes"},
  { X_SetClipRectangles,"XSetClipRectangles"},
  { X_FreeGC,"XFreeGC"},
  { X_ClearArea,"XClearArea"},
  { X_CopyArea,"XCopyArea"},
  { X_CopyPlane,"XCopyPlane"},
  { X_PolyPoint,"XPolyPoint"},
  { X_PolyLine,"XPolyLine"},
  { X_PolySegment,"XPolySegment"},
  { X_PolyRectangle,"XPolyRectangle"},
  { X_PolyArc,"XPolyArc"},
  { X_FillPoly,"XFillPoly"},
  { X_PolyFillRectangle,"XPolyFillRectangle"},
  { X_PolyFillArc,"XPolyFillArc"},
  { X_PutImage,"XPutImage"},
  { X_GetImage,"XGetImage"},
  { X_PolyText8,"XPolyText8"},
  { X_PolyText16,"XPolyText16"},
  { X_ImageText8,"XImageText8"},
  { X_ImageText16,"XImageText16"},
  { X_CreateColormap,"XCreateColormap"},
  { X_FreeColormap,"XFreeColormap"},
  { X_CopyColormapAndFree,"XCopyColormapAndFree"},
  { X_InstallColormap,"XInstallColormap"},
  { X_UninstallColormap,"XUninstallColormap"},
  { X_ListInstalledColormaps,"XListInstalledColormaps"},
  { X_AllocColor,"XAllocColor"},
  { X_AllocNamedColor,"XAllocNamedColor"},
  { X_AllocColorCells,"XAllocColorCells"},
  { X_AllocColorPlanes,"XAllocColorPlanes"},
  { X_FreeColors,"XFreeColors"},
  { X_StoreColors,"XStoreColors"},
  { X_StoreNamedColor,"XStoreNamedColor"},
  { X_QueryColors,"XQueryColors"},
  { X_LookupColor,"XLookupColor"},
  { X_CreateCursor,"XCreateCursor"},
  { X_CreateGlyphCursor,"XCreateGlyphCursor"},
  { X_FreeCursor,"XFreeCursor"},
  { X_RecolorCursor,"XRecolorCursor"},
  { X_QueryBestSize,"XQueryBestSize"},
  { X_QueryExtension,"XQueryExtension"},
  { X_ListExtensions,"XListExtensions"},
  { X_ChangeKeyboardMapping,"XChangeKeyboardMapping"},
  { X_GetKeyboardMapping,"XGetKeyboardMapping"},
  { X_ChangeKeyboardControl,"XChangeKeyboardControl"},
  { X_GetKeyboardControl,"XGetKeyboardControl"},
  { X_Bell,"XBell"},
  { X_ChangePointerControl,"XChangePointerControl"},
  { X_GetPointerControl,"XGetPointerControl"},
  { X_SetScreenSaver,"XSetScreenSaver"},
  { X_GetScreenSaver,"XGetScreenSaver"},
  { X_ChangeHosts,"XChangeHosts"},
  { X_ListHosts,"XListHosts"},
  { X_SetAccessControl,"XSetAccessControl"},
  { X_SetCloseDownMode,"XSetCloseDownMode"},
  { X_KillClient,"XKillClient"},
  { X_RotateProperties,"XRotateProperties"},
  { X_ForceScreenSaver,"XForceScreenSaver"},
  { X_SetPointerMapping,"XSetPointerMapping"},
  { X_GetPointerMapping,"XGetPointerMapping"},
  { X_SetModifierMapping,"XSetModifierMapping"},
  { X_GetModifierMapping,"XGetModifierMapping"},
  { X_NoOperation,"XNoOperation"}
};

static CodeName ProtocolErrors[] =
{
  { Success,"Success"},
  { BadRequest,"BadRequest"},
  { BadValue,"BadValue"},
  { BadWindow,"BadWindow"},
  { BadPixmap,"BadPixmap"},
  { BadAtom,"BadAtom"},
  { BadCursor,"BadCursor"},
  { BadFont,"BadFont"},
  { BadMatch,"BadMatch"},
  { BadDrawable,"BadDrawable"},
  { BadAccess,"BadAccess"},
  { BadAlloc,"BadAlloc"},
  { BadColor,"BadColor"},
  { BadGC,"BadGC"},
  { BadIDChoice,"BadIDChoice"},
  { BadName,"BadName"},
  { BadLength,"BadLength"},
  { BadImplementation,"BadImplementation"}
};

static int XWindowsError(Display *display, XErrorEvent *error)
{
  const char *errorName   = "unknown";
  const char *requestName = "unknown";
  int   i,n;
  char  buffer[500];
  
  n = sizeof(ProtocolErrors) / sizeof(ProtocolErrors[0]);
  
  for(i = 0; i < n; i++) 
    {
     if (ProtocolErrors[i].code == error->error_code)
       {
        errorName = ProtocolErrors[i].name;
       }
    }

  n = sizeof(ProtocolNames) / sizeof(ProtocolNames[0]);
  
  for(i = 0; i < n; i++) 
    {
     if (ProtocolNames[i].code == error->request_code)
       {
         requestName = ProtocolNames[i].name;
       }
    }

  sprintf(buffer,"%s in %s",errorName,requestName);

  printf("\nX Error %s\n\n", buffer);

#if NEVER
  /* Raise exception if we are running in synchronous mode */
  if (display->private15) RaiseXWindows(taskData, buffer);
#endif

  return 0; /* DUMMY value - SPF 6/1/94 */
}

struct _entrypts xwindowsEPT[] =
{
    { "PolyXWindowsGeneral",            (polyRTSFunction)&PolyXWindowsGeneral},

    { NULL, NULL} // End of list.
};

class XWinModule: public RtsModule
{
public:
    virtual void Init(void);
    void GarbageCollect(ScanAddress *process);
};

// Declare this.  It will be automatically added to the table.
static XWinModule xwinModule;


void XWinModule::GarbageCollect(ScanAddress *process)
{
    /* Process all the objects in the list. If an object */
    /* is not found from outside then it is removed.     */
    
    T_List **T = &TList;
    C_List **C = &CList;
    
    int i;
    
    /* process all XList headers */
    for (i = 0; i < XLISTSIZE; i++)
    {
        X_List *L = XList[i];
        
        while(L)
        {
            PolyObject *P = L->object;  /* copy object pointer */
            X_List   *N = L->next;    /* copy next   pointer */
            process->ScanRuntimeAddress(&P, ScanAddress::STRENGTH_WEAK);
            
            /* P may have been moved, or overwritten with a 0 if not accessible */
            
            if (P == 0)
                DestroyXObject(L->object);
            else
                L->object = (X_Object*)P;
            
            L = N;
        }
    }
    
    /* Process the timeout/message list */
    
    while (*T)
    {
        T_List *t = *T;
        
        process->ScanRuntimeAddress(&t->alpha, ScanAddress::STRENGTH_STRONG);
        process->ScanRuntimeAddress(&t->handler, ScanAddress::STRENGTH_STRONG);

        PolyObject *obj = t->window_object;
        process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_WEAK);
        t->window_object = (X_Window_Object*)obj;

        obj = t->widget_object;
        process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_STRONG);
        t->widget_object = (X_Widget_Object*)obj;
        
        // DCJM: I don't understand this.  The widget entry will never go
        // to zero since it's strong not weak.
        if (t->window_object == 0 && t->widget_object == 0)
        {
            *T = t->next;
            
            free(t);
        }
        else T = &t->next;
    }
    
    /* Process the callback list */
    
    while(*C)
    {
        C_List *c = *C;
        process->ScanRuntimeAddress(&c->function, ScanAddress::STRENGTH_STRONG);

        PolyObject *obj = c->widget_object;
        process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_STRONG);
        c->widget_object = (X_Widget_Object*)obj;
        
        /* DCJM: This doesn't make sense.  The widget entry will only
           go to zero if the G.C. operation was weak, not strong as in
           the line above. */
        if (c->widget_object == 0)
        {
            *C = c->next;
            
            free(c);
        }
        else C = &c->next;
    }
    
    /* Process the callback waiting list */
    if (! FList.IsTagged())
    {
        PolyObject *obj = FList.AsObjPtr();
        process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_STRONG);
        FList = obj;
    }
    
    /* and the Xt event waiting list. */
    if (! GList.IsTagged())
    {
        PolyObject *obj = GList.AsObjPtr();
        process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_STRONG) ;
        GList = obj;
    }
}


void XWinModule::Init(void)
{
    initXList(); /* added 9/12/93 SPF */

    XtToolkitThreadInitialize();
    XtToolkitInitialize();
    
    XSetErrorHandler(XWindowsError);
}

POLYUNSIGNED PolyXWindowsGeneral(POLYUNSIGNED threadId, POLYUNSIGNED params)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(params);
    Handle result = 0;

    try {
        result = XWindows_c(taskData, pushedArg);
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // May test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

#else
// We haven't got X or we haven't got Motif

#include "globals.h"
#include "run_time.h"
#include "sys.h"
#include "save_vec.h"
#include "machine_dep.h"
#include "processes.h"
#include "rtsentry.h"

#include "xwindows.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyXWindowsGeneral(POLYUNSIGNED threadId, POLYUNSIGNED params);
}

Handle XWindows_c(TaskData *taskData, Handle/*params*/)
{
    raise_exception_string(taskData, EXC_XWindows, "Not implemented");

    /*NOTREACHED*/
    return taskData->saveVec.push(TAGGED(0)); /* just to keep lint happy */
}

POLYUNSIGNED PolyXWindowsGeneral(POLYUNSIGNED threadId, POLYUNSIGNED /*params*/)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    taskData->PreRTSCall();

    try {
        raise_exception_string(taskData, EXC_XWindows, "Not implemented");
    } catch (...) { } // Handle the C++ exception

    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Return unit since we're raising an exception
}

struct _entrypts xwindowsEPT[] =
{
    { "PolyXWindowsGeneral",            (polyRTSFunction)&PolyXWindowsGeneral},

    { NULL, NULL} // End of list.
};

#endif


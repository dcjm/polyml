AUTOMAKE_OPTIONS=foreign
ACLOCAL_AMFLAGS= -I m4 --install

AM_CPPFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall
AM_CFLAGS = $(ARCHFLAG) $(OSFLAG) -Wall -fno-strict-aliasing

BOOTSTRAP_OPTIONS =

if INTINFISINT
POLYIMPORT_OPTIONS = --intIsIntInf
else
POLYIMPORT_OPTIONS =
endif

DIST_SUBDIRS = libpolyml libpolymain libffi modules

if INTERNAL_LIBFFI
SUBDIRS = libffi libpolyml libpolymain . modules
else
SUBDIRS = libpolyml libpolymain . modules
endif

bin_PROGRAMS = polyimport poly

dist_bin_SCRIPTS = polyc

man_MANS = poly.1 polyimport.1 polyc.1

# Select the architecture-specific pre-built compiler
if ARCHI386
POLYIMPORT = $(srcdir)/imports/polymli386.txt
else
if ARCHINTERPRET
POLYIMPORT = $(srcdir)/imports/polymlint.txt
else
if ARCHINTERPRET64
POLYIMPORT = $(srcdir)/imports/polymlint64.txt
else
if ARCHX86_64
if WINDOWS64CALLS
POLYIMPORT = $(srcdir)/imports/polymlx86_64win.txt
else
POLYIMPORT = $(srcdir)/imports/polymlx86_64sysv.txt
endif
else
endif
endif
endif
endif

noinst_HEADERS = polyexports.h

poly_LDFLAGS=
polyimport_LDFLAGS=
POLYRESOURCES=
EXTRALDFLAGS=

if NATIVE_WINDOWS
if ARCHINTERPRET64
EXTRALDFLAGS += -Wl,-u,WinMain
else
if ARCHX86_64
EXTRALDFLAGS += -Wl,-u,WinMain
else
EXTRALDFLAGS += -Wl,-u,_WinMain@16 -Wl,--large-address-aware
endif
endif

if WINDOWSGUI
EXTRALDFLAGS += -mwindows
polyimport_LDFLAGS += -mwindows
else
EXTRALDFLAGS += -mconsole
polyimport_LDFLAGS += -mconsole
endif

poly_LDFLAGS += $(EXTRALDFLAGS)

POLYOBJECTFILE = polyexport.obj
POLYRESOURCES += polyresource.o
OBJECTSUFFIX=obj
else
POLYOBJECTFILE = polyexport.o
OBJECTSUFFIX=o
endif

if EXPMACHO
poly_LDFLAGS += -Wl,-no_pie
endif

poly_SOURCES = 
poly_LDADD = $(POLYOBJECTFILE) $(POLYRESOURCES) libpolymain/libpolymain.la libpolyml/libpolyml.la 

polyimport_SOURCES = polyimport.c
polyimport_LDADD = $(POLYRESOURCES) libpolyml/libpolyml.la

EXTRA_DIST = \
	imports/polymli386.txt \
	imports/polymlint64.txt \
	imports/polymlint.txt \
	imports/polymlx86_64.txt \
	libpolyml/x86asm.asm \
	exportPoly.sml \
	poly.1 \
	polyimport.1

dist-hook:
	find {basis,mlsource}/ -type d -exec mkdir $(distdir)/{} \;
	find {basis,mlsource}/ -type f -not -name .svn \
	    -exec cp -pf {} $(distdir)/{} \;



# Unix.
polyexport.o: polyimport exportPoly.sml polytemp.txt
	./polyimport $(POLYIMPORT_OPTIONS) polytemp.txt -I $(srcdir) < $(srcdir)/exportPoly.sml

# Windows.  When building on Windows make sure that we provide both stdin and stdout to suppress the GUI.
polyexport.obj: polyimport exportPoly.sml polytemp.txt
	./polyimport $(POLYIMPORT_OPTIONS) polytemp.txt -I $(srcdir) < $(srcdir)/exportPoly.sml | cat

polyresource.o: PolyML.rc poly.ico
	$(WINDRES) -o polyresource.o $(srcdir)/PolyML.rc

polytemp.txt: $(POLYIMPORT)
	cp $(POLYIMPORT) polytemp.txt

#polyc: buildpolyc
#	./buildpolyc LINK=\"$(CC)\" BINDIR=\"$(bindir)\" LIBDIR=\"$(libdir)\" LIBS=\"$(LIBS)\" EXTRALDFLAGS=\"$(EXTRALDFLAGS)\" SUFFIX=\"$(OBJECTSUFFIX)\"

# This builds the compiler but does not update the files in the imports directory.
# It then builds a version of poly containing the new compiler.
compiler: all
	./poly $(BOOTSTRAP_OPTIONS) --error-exit < mlsource/BuildExport.sml
	$(MAKE)

reboot: compiler
	cp polytemp.txt $(POLYIMPORT)

# Retain this target for backwards compatibility.
cvs: compiler

clean-local:
	rm -f *.obj polytemp.txt polyc

# Run tests
check-local: all
	echo "val () = use \"$(srcdir)/Tests/RunTests\"; val () = OS.Process.exit(if runTests \"$(srcdir)/Tests\" then OS.Process.success else OS.Process.failure):unit;" | ./poly

# Retain this target for backwards compatibility
tests: check

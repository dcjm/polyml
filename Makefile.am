AUTOMAKE_OPTIONS=foreign
ACLOCAL_AMFLAGS= -I m4 --install

if INTINFISINT
BOOTSTRAP_OPTIONS = --intIsIntInf
else
BOOTSTRAP_OPTIONS =
endif

if COMPACTINTINF
POLYIMPORT_OPTIONS += --useCompactIntInf
endif

DIST_SUBDIRS = libpolyml libpolymain modules

SUBDIRS = libpolyml libpolymain . modules

bin_PROGRAMS = polyimport poly

dist_bin_SCRIPTS = polyc

man_MANS = poly.1 polyimport.1 polyc.1

# Select the pre-built compiler
if BOOT64
POLYIMPORT = $(srcdir)/bootstrap/bootstrap64.txt
else
POLYIMPORT = $(srcdir)/bootstrap/bootstrap32.txt
endif

noinst_HEADERS = polyexports.h

poly_LDFLAGS=
polyimport_LDFLAGS=
POLYRESOURCES=
EXTRALDFLAGS=

if NATIVE_WINDOWS
if ARCHI386
EXTRALDFLAGS += -Wl,-u,_WinMain@16 -Wl,--large-address-aware
else
EXTRALDFLAGS += -Wl,-u,WinMain
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

poly_SOURCES = 
poly_LDADD = $(POLYOBJECTFILE) $(POLYRESOURCES) libpolymain/libpolymain.la libpolyml/libpolyml.la 

polyimport_SOURCES = polyimport.c
polyimport_LDADD = $(POLYRESOURCES) libpolyml/libpolyml.la

EXTRA_DIST = \
	poly.1 \
	polyimport.1

dist-hook:
	find {bootstrap,basis,mlsource}/ -type d -exec mkdir $(distdir)/{} \;
	find {bootstrap,basis,mlsource}/ -type f -not -name .git \
	    -exec cp -pf {} $(distdir)/{} \;

# Unix.
polyexport.o: polyimport bootstrap/Stage1.sml $(POLYIMPORT)
	./polyimport $(BOOTSTRAP_OPTIONS) $(POLYIMPORT) -I $(srcdir) < $(srcdir)/bootstrap/Stage1.sml

# Windows.  When building on Windows make sure that we provide both stdin and stdout to suppress the GUI.
polyexport.obj: polyimport$(EXEEXT) bootstrap/Stage1.sml $(POLYIMPORT)
	./polyimport $(BOOTSTRAP_OPTIONS) $(POLYIMPORT) -I $(srcdir) < $(srcdir)/bootstrap/Stage1.sml | cat

polyresource.o: PolyML.rc poly.ico
	$(WINDRES) -o polyresource.o $(srcdir)/PolyML.rc

#polyc: buildpolyc
#	./buildpolyc LINK=\"$(CC)\" BINDIR=\"$(bindir)\" LIBDIR=\"$(libdir)\" LIBS=\"$(LIBS)\" EXTRALDFLAGS=\"$(EXTRALDFLAGS)\" SUFFIX=\"$(OBJECTSUFFIX)\"

# Recompiler the compiler using the last step of the bootstrap process.
compiler: all
	./poly $(BOOTSTRAP_OPTIONS) --error-exit < bootstrap/Stage6.sml
	$(MAKE)

reboot: compiler
	cp $(POLYIMPORT)

clean-local:
	rm -f *.obj polyc

# Run tests
check-local: all
	echo "val () = use \"$(srcdir)/Tests/RunTests\"; val () = OS.Process.exit(if runTests \"$(srcdir)/Tests\" then OS.Process.success else OS.Process.failure):unit;" | ./poly

# Retain this target for backwards compatibility
tests: check

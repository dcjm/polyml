AUTOMAKE_OPTIONS=foreign

moduledir = @moduledir@

AM_CPPFLAGS = $(CFLAGS) $(OSFLAG) $(GIT_VERSION) -Wall -DMODULEDIR=\"$(moduledir)\"
AM_CFLAGS = $(CFLAGS) $(OSFLAG) $(GIT_VERSION) -Wall -fno-strict-aliasing
AM_ASFLAGS = $(OSFLAG)
AM_CCASFLAGS = $(OSFLAG)

lib_LTLIBRARIES = libpolyml.la
libpolyml_la_LDFLAGS = -version-info 16:0:0

if NO_UNDEFINED
# Force all references to be defined to build the DLL.
libpolyml_la_LDFLAGS += -no-undefined
endif

# Select the architecture-specific modules
if ARCHI386
ARCHSOURCE = x86_dep.cpp x86assembly_gas32.S
else
if ARCHX86_64
ARCHSOURCE = x86_dep.cpp x86assembly_gas64.S
else
if ARCHARM_64
ARCHSOURCE = arm64.cpp arm64assembly.S
else
ARCHSOURCE = interpreter.cpp
AM_CPPFLAGS += -DCODEISNOTEXECUTABLE
endif
endif
endif

# Select the object-format-specific modules
if EXPPECOFF
EXPORTSOURCE = pecoffexport.cpp
else
if EXPELF
EXPORTSOURCE = elfexport.cpp
else
if EXPMACHO
EXPORTSOURCE = machoexport.cpp
endif
endif
endif

if NATIVE_WINDOWS
OSSOURCE = winstartup.cpp winbasicio.cpp winguiconsole.cpp windows_specific.cpp osmemwin.cpp
else
OSSOURCE = basicio.cpp unix_specific.cpp osmemunix.cpp
endif

noinst_HEADERS = \
	arb.h \
	basicio.h \
	bitmap.h \
	bytecode.h \
	check_objects.h \
	diagnostics.h \
	elfexport.h \
	errors.h \
	exporter.h \
	gc.h \
	gctaskfarm.h \
	gc_progress.h \
	globals.h \
	heapsizing.h \
	int_opcodes.h \
	io_internal.h \
	locking.h \
	machine_dep.h \
	machoexport.h \
	memmgr.h \
    modules.h \
	mpoly.h \
	network.h \
	noreturn.h \
	objsize.h \
	osmem.h \
	os_specific.h \
	pecoffexport.h \
	pexport.h \
	PolyControl.h \
	poly_specific.h \
	polyffi.h \
	polystring.h \
	process_env.h \
	processes.h \
	profiling.h \
	reals.h \
	rts_module.h \
	rtsentry.h \
	run_time.h \
	savestate.h \
	save_vec.h \
	scanaddrs.h \
	sharedata.h \
	sighandler.h \
	statistics.h \
	sys.h \
	timing.h \
	version.h \
	winguiconsole.h \
	winstartup.h \
	xcall_numbers.h \
	xwindows.h

libpolyml_la_SOURCES = \
    arb.cpp \
    bitmap.cpp \
	bytecode.cpp \
    check_objects.cpp \
    diagnostics.cpp \
    errors.cpp \
    exporter.cpp \
    gc.cpp \
    gc_check_weak_ref.cpp \
    gc_copy_phase.cpp \
    gc_mark_phase.cpp \
    gc_progress.cpp \
    gc_share_phase.cpp \
    gc_update_phase.cpp \
    gctaskfarm.cpp \
    heapsizing.cpp \
    locking.cpp \
    memmgr.cpp \
    modules.cpp \
    mpoly.cpp \
    network.cpp \
    objsize.cpp \
    pexport.cpp \
    poly_specific.cpp \
    polyffi.cpp \
    polystring.cpp \
    process_env.cpp \
    processes.cpp \
    profiling.cpp \
    quick_gc.cpp \
    reals.cpp \
    rts_module.cpp \
    rtsentry.cpp \
    run_time.cpp \
    save_vec.cpp \
    savestate.cpp \
    scanaddrs.cpp \
    sharedata.cpp \
    sighandler.cpp \
    statistics.cpp \
    timing.cpp \
    xwindows.cpp \
    $(ARCHSOURCE) $(EXPORTSOURCE) $(OSSOURCE)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = polyml.pc

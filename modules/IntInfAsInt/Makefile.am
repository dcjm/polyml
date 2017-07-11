AUTOMAKE_OPTIONS=foreign

all-local: IntInfAsInt

clean-local:
	-rm -f IntInfAsInt

install-exec-local:
	$(mkdir_p) $(DESTDIR)$(moduledir)
	$(INSTALL_PROGRAM) IntInfAsInt $(DESTDIR)$(moduledir)

uninstall-local:
	-rm -f $(DESTDIR)$(moduledir)/IntInfAsInt

IntInfAsInt: ../../poly$(EXEEXT) ROOT.sml
	echo "use \"$(srcdir)/ROOT.sml\";" | ../../poly$(EXEEXT) -q -error-exit

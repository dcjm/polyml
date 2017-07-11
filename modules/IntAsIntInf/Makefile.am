AUTOMAKE_OPTIONS=foreign

all-local: IntAsIntInf

clean-local:
	-rm -f IntAsIntInf

install-exec-local:
	$(mkdir_p) $(DESTDIR)$(moduledir)
	$(INSTALL_PROGRAM) IntAsIntInf $(DESTDIR)$(moduledir)

uninstall-local:
	-rm -f $(DESTDIR)$(moduledir)/IntAsIntInf

IntAsIntInf: ../../poly$(EXEEXT) ROOT.sml
	echo "use \"$(srcdir)/ROOT.sml\";" | ../../poly$(EXEEXT) -q -error-exit

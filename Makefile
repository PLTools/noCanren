OB=ocamlbuild -use-ocamlfind
TARGETS=src/MiniKanren.cmo plugin/mkshow.cmo camlp5/pa_minikanren.cmo
TESTS_ENVIRONMENT=./test.sh
TESTS=regression/test000.native regression/test001.native regression/test002.native \
	regression/test003.native regression/test004.native

.PHONY: all clean install uninstall check check-TESTS

all:
	$(OB) $(TARGETS) $(TARGETS:.cmo=.cmx)

regression/test000.native:
regression/test001.native:
regression/test002.native:
regression/test003.native:
regression/test004.native:
	$(OB) -Is src $@

check: all $(TESTS)
	$(OB) -Is src -Is plugin $(TESTS)

clean:
	rm -fr _build *.log  *.native *.byte

#TESTS=
REGRES_CASES=000 001 002 003 004
define TESTRULES
.PHONY: test_$(1)
tests: test_$(1)
native_tests: regression/test$(1).native
test_$(1): native_tests regression/test$(1).native
	@cd regression  && $(TESTS_ENVIRONMENT) ../test$(1).native; \
	if [ $$$$? -ne 0 ] ; then echo "$(1) FAILED"; else echo "$(1) PASSED"; fi
endef
$(foreach i,$(REGRES_CASES),$(eval $(call TESTRULES,$(i)) ) )

check-TESTS: check
	@failed=0; all=0; xfail=0; xpass=0; skip=0; \
	srcdir=`pwd`; export srcdir; \
	list='$(TESTS)'; \
	if test -n "$$list"; then \
	  for tst in $$list; do \
	    if test -f _build/$$tst; then dir="_build/"; \
	    elif test -f ./$$tst; then dir=./; \
	    elif test -f ./regression/$$tst; then dir="regression"; \
	    elif test -f _build/regression/$$tst; then dir="_build/"; \
	    else dir="./"; fi; \
	    if cd regression && $(TESTS_ENVIRONMENT) ../$${dir}$$tst; then \
	      cd ..; \
	      all=`expr $$all + 1`; \
	      case " $(XFAIL_TESTS) " in \
	      *" $$tst "*) \
		xpass=`expr $$xpass + 1`; \
		failed=`expr $$failed + 1`; \
		echo "XPASS: $$tst"; \
	      ;; \
	      *) \
		echo "PASS: $$tst"; \
	      ;; \
	      esac; \
	    elif test $$? -ne 77; then \
	      all=`expr $$all + 1`; \
	      case " $(XFAIL_TESTS) " in \
	      *" $$tst "*) \
		xfail=`expr $$xfail + 1`; \
		echo "XFAIL: $$tst"; \
	      ;; \
	      *) \
		failed=`expr $$failed + 1`; \
		echo "FAIL: $$tst"; \
	      ;; \
	      esac; \
	    else \
	      skip=`expr $$skip + 1`; \
	      echo "SKIP: $$tst"; \
	    fi; \
	  done; \
	  if test "$$failed" -eq 0; then \
	    if test "$$xfail" -eq 0; then \
	      banner="All $$all tests passed"; \
	    else \
	      banner="All $$all tests behaved as expected ($$xfail expected failures)"; \
	    fi; \
	  else \
	    if test "$$xpass" -eq 0; then \
	      banner="$$failed of $$all tests failed"; \
	    else \
	      banner="$$failed of $$all tests did not behave as expected ($$xpass unexpected passes)"; \
	    fi; \
	  fi; \
	  dashes="$$banner"; \
	  skipped=""; \
	  if test "$$skip" -ne 0; then \
	    skipped="($$skip tests were not run)"; \
	    test `echo "$$skipped" | wc -c` -le `echo "$$banner" | wc -c` || \
	      dashes="$$skipped"; \
	  fi; \
	  report=""; \
	  if test "$$failed" -ne 0 && test -n "$(PACKAGE_BUGREPORT)"; then \
	    report="Please report to $(PACKAGE_BUGREPORT)"; \
	    test `echo "$$report" | wc -c` -le `echo "$$banner" | wc -c` || \
	      dashes="$$report"; \
	  fi; \
	  dashes=`echo "$$dashes" | sed s/./=/g`; \
	  echo "$$dashes"; \
	  echo "$$banner"; \
	  test -z "$$skipped" || echo "$$skipped"; \
	  test -z "$$report" || echo "$$report"; \
	  echo "$$dashes"; \
	  test "$$failed" -eq 0; \
	else :; fi

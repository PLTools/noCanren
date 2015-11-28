OB=ocamlbuild -use-ocamlfind
TARGETS=src/MiniKanren.cmo plugin/mkshow.cmo camlp5/pa_minikanren.cmo
PPX_TARGET=ppx/smart_logger.native
TESTS_ENVIRONMENT=./test.sh
#TESTS=regression/test000.native #regression/test001.native regression/test002.native \
#	regression/test003.native #regression/test004.native

.PHONY: all celan clean install uninstall tests test regression compile_tests run_tests

all:
	$(OB) $(TARGETS) $(TARGETS:.cmo=.cmx) $(PPX_TARGET)

#check: all $(TESTS)
#	$(OB) -Is src -Is plugin $(TESTS)

celan: clean

clean:
	rm -fr _build *.log  *.native *.byte
	$(MAKE) -C regression clean

#TESTS=
REGRES_CASES=000 001 002 003 004 005
define TESTRULES
.PHONY: test_$(1) test$(1).native
test$(1).native: regression/test$(1).native
regression/test$(1).native:
	$(OB) -Is src $$@

compile_tests: regression/test$(1).native

run_tests: test_$(1)
test_$(1): #regression/test$(1).native
	@cd regression  && $(TESTS_ENVIRONMENT) ../test$(1).native; \
	if [ $$$$? -ne 0 ] ; then echo "$(1) FAILED"; else echo "$(1) PASSED"; fi
endef
$(foreach i,$(REGRES_CASES),$(eval $(call TESTRULES,$(i)) ) )

tests: compile_tests run_tests
regression: tests
test: tests



install:
	ocamlfind install MiniKanren META _build/src/MiniKanren.cm*

uninstall:
	ocamlfind remove MiniKanren

OB=ocamlbuild -use-ocamlfind -classic-display
TARGETS=src/MiniKanren.cmo
PPX_TARGETS=ppx/smart_logger_bin.native ppx/ppx_repr_bin.native
TESTS_ENVIRONMENT=./test.sh
#TESTS=regression/test000.native #regression/test001.native regression/test002.native \
#	regression/test003.native #regression/test004.native
JSOO_LIB=jsoo_runner/jsoo_runner.cma

.PHONY: all celan clean install uninstall tests test regression compile_tests run_tests toplevel jslib ppx minikanren_stuff

all: minikanren_stuff

minikanren_stuff:
	$(OB) $(TARGETS) $(TARGETS:.cmo=.cmx)

ppx:
	$(OB) $(TARGETS) $(PPX_TARGETS)

jslib: minikanren_stuff ppx
	$(OB) -Is src,ppx $(JSOO_LIB)

toplevel: ppx jslib
	$(MAKE) -C toplevel


celan: clean

clean:
	rm -fr _build *.log  *.native *.byte
	$(MAKE) -C regression clean

REGRES_CASES=$(shell seq -s " " -f %03g 0 0)
REGRES_CASES:=$(REGRES_CASES) bad000

#$(warning $(REGRES_CASES))
define TESTRULES
.PHONY: test_$(1) test$(1).native
test$(1).native: regression/test$(1).native
regression/test$(1).native: regression/test$(1).ml
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

unittests:
	$(OB) -I src src_test/test.byte && ./test.byte

install:
	ocamlfind install MiniKanren META _build/src/MiniKanren.cm* _build/src/*.cmi \
	_build/jsoo_runner/jsoo_runner.cm[ia] \
	_build/ppx/smart_logger.cmi _build/regression/tester.cm[io]

uninstall:
	ocamlfind remove MiniKanren

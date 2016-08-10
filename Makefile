MKDIR ?= mkdir -vp
CP    ?= cp

OB=ocamlbuild -use-ocamlfind -plugin-tag "package(str)" -classic-display
ifdef OBV
OB += -verbose 6
endif

CMA_TARGETS=src/MiniKanren.cma
CMO_TARGETS=src/tester.cmo
BYTE_TARGETS=$(CMA_TARGETS) $(CMO_TARGETS)
NATIVE_TARGETS= $(CMA_TARGETS:.cma=.cmxa) $(CMO_TARGETS:.cmo=.cmx)
PPX_TARGETS=ppx/smart_logger_bin.native ppx/ppx_repr_bin.native ppx/pa_minikanren_bin.native
TESTS_ENVIRONMENT=./test.sh
JSOO_LIB=jsoo_runner/jsoo_runner.cma

.PHONY: all celan clean clean_tests install uninstall tests test regression promote \
	only-toplevel toplevel jslib ppx minikanren_stuff tester bundle

.DEFAULT_GOAL: all

all: ppx minikanren_stuff
	$(MAKE) bundle

minikanren_stuff:
	$(OB) -Is src $(BYTE_TARGETS) $(NATIVE_TARGETS)

ppx:
	$(OB) $(PPX_TARGETS)

jslib: ppx minikanren_stuff
	$(OB) -Is src $(JSOO_LIB)

only-toplevel:
	rm -fr _build/toplevel
	$(OB) -Is ppx toplevel/indent.cmo toplevel/colorize.cmo toplevel/toplevel.cmo \
	toplevel/toplevel.js

toplevel: ppx jslib only-toplevel

tester:
	$(OB) -Is src,jsoo_runner regression/tester.cmo

celan: clean

clean: clean_tests
	$(RM) -r _build *.log  *.native *.byte
	$(MAKE) -C regression clean

######################## Tests related stuff  ##########################
REGRES_CASES := 000 004 005 009 0match 01match 02match 0domains

define TESTRULES
BYTE_TEST_EXECUTABLES += regression/test$(1).byte
NATIVE_TEST_EXECUTABLES += regression/test$(1).native
TEST_MLS += regression/test$(1).ml

.PHONY: test_$(1) test$(1).native compile_tests_native compile_tests_byte

test$(1).native: regression/test$(1).native
test$(1).byte:   regression/test$(1).byte

regression/test$(1).byte: regression/test$(1).ml
	$(OB) -Is src $$@

regression/test$(1).native: regression/test$(1).ml
	$(OB) -Is src $$@

run_tests: test_$(1)
test_$(1):
	@cd regression  && $(TESTS_ENVIRONMENT) ../test$(1).native; \
	if [ $$$$? -ne 0 ] ; then echo "$(1) FAILED"; else echo "$(1) PASSED"; fi
endef
$(foreach i,$(REGRES_CASES),$(eval $(call TESTRULES,$(i)) ) )

.PHONY: compile_tests_native compile_tests_byte compile_tests run_tests

compile_tests_native: $(TEST_MLS)
	$(OB) -Is src $(NATIVE_TEST_EXECUTABLES)

compile_tests_byte: $(TEST_MLS)
	$(OB) -Is src $(BYTE_TEST_EXECUTABLES)

compile_tests: compile_tests_native

clean_tests:
	$(RM) -r _build/regression

promote:
	$(MAKE) -C regression promote TEST=$(TEST)

tests: ppx minikanren_stuff bundle compile_tests run_tests
regression: tests
test: tests

######################## in demo files we do some experimentation ############
.PHONY: demo clean_demo demo.byte demo.native
demo: demo.native
demo.native:
	$(OB) -Is src $@
demo.byte:
	$(OB) -Is src $@
clean_demo:
	@$(RM) demo.native
clean: clean_demo
######################## Installation related stuff ##########################
INSTALL_TARGETS=META \
	$(wildcard _build/src/*.cmi) \
	_build/src/MiniKanren.cma \
	$(wildcard _build/src/MiniKanren.[oa]) \
	_build/src/MiniKanren.cmxa \
	$(wildcard _build/src/tester.cm[ox]) \
	_build/src/tester.o \
	_build/ppx/smart_logger.cmi \
	$(wildcard _build/ppx/*.native)

MAYBE_INSTALL_TARGETS=\
	_build/jsoo_runner/jsoo_runner.cmi \
	_build/jsoo_runner/jsoo_runner.cma \

define MAYBE_ADD_TARGET
ifneq (,$(wildcard $(1)))
INSTALL_TARGETS += $(1)
endif
endef

$(foreach i,$(MAYBE_INSTALL_TARGETS),$(eval $(call MAYBE_ADD_TARGET,$(i)) ) )

BUNDLEDIR=_build/bundle/miniKanren

define MAKE_BUNDLE_RULE
$(BUNDLEDIR)/$(notdir $(1)): $(1)
	cp $(1) $(BUNDLEDIR)
MAKE_BUNDLE_TARGETS += $(BUNDLEDIR)/$(notdir $(1))

endef
$(foreach i,$(INSTALL_TARGETS),$(eval $(call MAKE_BUNDLE_RULE,$(i)) ) )

rmbundledir:
	@$(RM) -r $(BUNDLEDIR)

$(BUNDLEDIR):
	@$(MKDIR) $@

bundle: rmbundledir $(BUNDLEDIR) $(MAKE_BUNDLE_TARGETS)

install: bundle
	ocamlfind install miniKanren $(BUNDLEDIR)/*

uninstall:
	ocamlfind remove miniKanren

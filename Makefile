MKDIR ?= mkdir -vp
CP    ?= cp

OB=ocamlbuild -use-ocamlfind -plugin-tag "package(str)"
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

all: minikanren_stuff ppx
	$(MAKE) bundle

minikanren_stuff:
	$(OB) -Is src $(BYTE_TARGETS) $(NATIVE_TARGETS)

ppx:
	$(OB) $(TARGETS) $(PPX_TARGETS)

jslib: minikanren_stuff ppx
	$(OB) $(JSOO_LIB)

only-toplevel:
	$(OB) toplevel/indent.cmo toplevel/colorize.cmo toplevel/toplevel.cmo \
	toplevel/toplevel.js

toplevel: ppx jslib only-toplevel

tester:
	$(OB) -Is src,jsoo_runner regression/tester.cmo

celan: clean

clean: clean_tests
	$(RM) -r _build *.log  *.native *.byte
	$(MAKE) -C regression clean

#REGRES_CASES=$(shell seq -s " " -f %03g 0 11)
#REGRES_CASES:=$(REGRES_CASES) bad000
#REGRES_CASES:=$(REGRES_CASES) Diseq000
REGRES_CASES := 000 004 005 009 0match 01match 02match 0domains

#$(warning $(REGRES_CASES))
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
test_$(1): #regression/test$(1).native
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

tests: minikanren_stuff ppx bundle compile_tests run_tests
regression: tests
test: tests

unittests:
	$(OB) -I src src_test/test.byte && ./test.byte

INSTALL_TARGETS=META \
	$(wildcard _build/src/*.cmi) \
	_build/src/MiniKanren.cma \
	$(wildcard _build/src/MiniKanren.[oa]) \
	_build/src/MiniKanren.cmxa \
	$(wildcard _build/src/tester.cm[ox]) \
	_build/src/tester.o \
	_build/ppx/smart_logger.cmi \
	$(wildcard _build/ppx/*.native)

$(info  )

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
	$(RM) -r $(BUNDLEDIR)

$(BUNDLEDIR):
	$(MKDIR) $@

#$(info MAKE_BUNDLE_TARGETS $(MAKE_BUNDLE_TARGETS))

bundle: rmbundledir $(BUNDLEDIR) $(MAKE_BUNDLE_TARGETS)

install:
	ocamlfind install miniKanren $(BUNDLEDIR)/*

uninstall:
	ocamlfind remove miniKanren

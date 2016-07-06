MKDIR ?= mkdir -vp
CP    ?= cp

OB=ocamlbuild -use-ocamlfind -plugin-tag "package(str)"
ifdef OBV
OB += -verbose 6
endif

CMA_TARGETS=src/MiniKanren.cmo
CMO_TARGETS=src/tester.cmo
BYTE_TARGETS=$(CMO_TARGETS) $(CMA_TARGETS) src/MiniKanren.cma
NATIVE_TARGETS=$(CMO_TARGETS:.cmo=.cmx) $(CMA_TARGETS:.cma=.cmxa) src/MiniKanren.cmxa
PPX_TARGETS=ppx/smart_logger_bin.native ppx/ppx_repr_bin.native ppx/pa_minikanren_bin.native
TESTS_ENVIRONMENT=./test.sh
JSOO_LIB=jsoo_runner/jsoo_runner.cma

.PHONY: all celan clean install uninstall tests test regression promote compile_tests run_tests\
	only-toplevel toplevel jslib ppx minikanren_stuff tester bundle

all: minikanren_stuff ppx

minikanren_stuff:
	$(OB) -Is src $(BYTE_TARGETS) $(NATIVE_TARGETS)

ppx:
	$(OB) $(TARGETS) $(PPX_TARGETS)

jslib: minikanren_stuff ppx
	$(OB) -Is src,ppx $(JSOO_LIB)

only-toplevel:
	$(OB) toplevel/indent.cmo toplevel/colorize.cmo toplevel/toplevel.cmo \
	toplevel/toplevel.js

toplevel: ppx jslib only-toplevel

tester:
	$(OB) -Is src,jsoo_runner regression/tester.cmo

celan: clean

clean:
	rm -fr _build *.log  *.native *.byte
	$(MAKE) -C regression clean

#REGRES_CASES=$(shell seq -s " " -f %03g 0 11)
#REGRES_CASES:=$(REGRES_CASES) bad000
#REGRES_CASES:=$(REGRES_CASES) Diseq000
REGRES_CASES := 000 004 005 009 0match 01match 02match 0domains

#$(warning $(REGRES_CASES))
define TESTRULES
.PHONY: test_$(1) test$(1).native
test$(1).native: regression/test$(1).native
regression/test$(1).native: regression/test$(1).ml
	OCAMLPATH=`pwd`/_build/bundle $(OB)  $$@

compile_tests: regression/test$(1).native

run_tests: test_$(1)
test_$(1): #regression/test$(1).native
	@cd regression  && $(TESTS_ENVIRONMENT) ../test$(1).native; \
	if [ $$$$? -ne 0 ] ; then echo "$(1) FAILED"; else echo "$(1) PASSED"; fi
endef
$(foreach i,$(REGRES_CASES),$(eval $(call TESTRULES,$(i)) ) )

promote:
	$(MAKE) -C regression promote TEST=$(TEST)

tests: ppx compile_tests run_tests
regression: tests
test: tests

unittests:
	$(OB) -I src src_test/test.byte && ./test.byte

INSTALL_TARGETS=META \
	$(wildcard _build/src/*.cmi) \
	_build/src/MiniKanren.cma \
	_build/src/MiniKanren.[oa] \
	_build/src/MiniKanren.cmxa \
	_build/src/tester.cm[iox] \
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
$(info add $(1) install target)
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

$(BUNDLEDIR):
	$(MKDIR) $@

$(info MAKE_BUNDLE_TARGETS $(MAKE_BUNDLE_TARGETS))

bundle: $(BUNDLEDIR) $(MAKE_BUNDLE_TARGETS)

install:
	ocamlfind install miniKanren $(BUNDLEDIR)/*

uninstall:
	ocamlfind remove miniKanren

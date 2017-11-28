MKDIR ?= mkdir -vp
CP    ?= cp

.NOTPARALLEL :

OB=ocamlbuild -use-ocamlfind -plugin-tag "package(str)" -classic-display
ifdef OBV
OB += -verbose 6
endif

CMA_TARGETS=src/MiniKanren.cma
CMO_TARGETS=regression/tester.cmo
#TESTER_TARGETS=regression/tester.cmo regression/tester.cmx
BYTE_TARGETS=$(CMA_TARGETS) $(CMO_TARGETS)
NATIVE_TARGETS= $(CMA_TARGETS:.cma=.cmxa) $(CMO_TARGETS:.cmo=.cmx)
TESTS_ENVIRONMENT=./test.sh
JSOO_LIB=jsoo_runner/jsoo_runner.cma

.PHONY: all celan clean clean_tests install uninstall tests test regression promote_all \
	ppx doc transl \
	only-toplevel toplevel minikanren_stuff tester bundle plugin

.DEFAULT_GOAL: all

all: minikanren_stuff plugin transl bundle

minikanren_stuff:
	$(OB) -Is src $(BYTE_TARGETS) $(NATIVE_TARGETS)

transl: minikanren_stuff
	$(OB) -Is transl transl/ml_to_mk.cma transl/ml2mk_pp.native transl/logic_run.native #transl/hm_inferencer_run.native 
	@# peano.native is not compilable at the moment because a plugin linking
	@# error loading shared library: /home/kakadu/asp/ocanren-eff/_build/transl/ml_to_mk.cmxs:
	@# undefined symbol: caml_int_of_string while loading argument of -plugin
	#$(OB) -Is transl,src peano_run.native peano_run.byte

celan: clean

clean: clean_tests
	$(RM) -r _build *.log  *.native *.byte *.docdir

######################## Tests related stuff  ##########################
REGRES_CASES := 666 667peano 668sorto 220simple 221appendo 222sorto 223binarith

define TESTRULES
BYTE_TEST_EXECUTABLES += regression/test$(1).byte
NATIVE_TEST_EXECUTABLES += regression/test$(1).native
TEST_MLS += regression/test$(1).ml

.PHONY: test_$(1) test$(1).native compile_tests_native compile_tests_byte \
	promote$(1) promote_test$(1)

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

promote_all: promote_$(1)
promite_test$(1): promote_$(1)
promote_$(1):
	./test$(1).native > regression/orig/test$(1).log

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



tests: plugin minikanren_stuff compile_tests run_tests
regression: tests
test: tests

######################## Installation related stuff ##########################
INSTALL_TARGETS=META \
	$(wildcard _build/regression/tester.cmi) \
	$(wildcard _build/regression/tester.cmo) \
	$(wildcard _build/regression/tester.cmx) \
	$(wildcard _build/regression/tester.o) \
	$(wildcard _build/src/*.cmi) \
	_build/src/MiniKanren.cmx \
	_build/src/MiniKanren.cma \
	_build/src/MiniKanren.cmxa \
	$(wildcard _build/ppx/ppx_ocanren_all.cma) \
	$(wildcard _build/ppx/ppx_ocanren_all.cmxa) \
	$(wildcard _build/ppx/ppx_ocanren_all.cmxs) \
	$(wildcard _build/ppx/ppx_ocanren_all.native) \
	$(wildcard _build/src/MiniKanren.[oa]) \
	$(wildcard _build/camlp5/pa_minikanren.cm[oi]) \


MAYBE_INSTALL_TARGETS=\
	_build/jsoo_runner/jsoo_runner.cmi \
	_build/jsoo_runner/jsoo_runner.cma \

define MAYBE_ADD_TARGET
ifneq (,$(wildcard $(1)))
INSTALL_TARGETS += $(1)
endif
endef

$(foreach i,$(MAYBE_INSTALL_TARGETS),$(eval $(call MAYBE_ADD_TARGET,$(i)) ) )

BUNDLEDIR=_build/bundle/ocanren

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

bundle: rmbundledir $(BUNDLEDIR)
	$(MAKE) really_make_bundle
	#cp _build/ppx/ppx_ocanren_all $(BUNDLEDIR)/

really_make_bundle: $(MAKE_BUNDLE_TARGETS)

install: bundle
	ocamlfind install ocanren $(BUNDLEDIR)/*

uninstall:
	ocamlfind remove ocanren

doc: all bundle compile_tests
	$(OB) MiniKanren.docdir/index.html -Is src

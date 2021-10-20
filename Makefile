OB = ocamlbuild -use-ocamlfind -classic-display

TESTS   = bottles bridge einstein GCW hanoi hanoi2
SAMPLES = bottles bridge einstein GCW hanoi hanoi2 hm_inferencer logic_interpreter lorry scheme_interpreter sudoku4x4 unify


compile:
	$(OB) -I src noCanren.native

multifiles_test_compile:
	mkdir -p _build/samples/multifiles/orig
	mkdir -p regression/multifiles

	cp samples/multifiles/test_nat.ml2mk.ml _build/samples/multifiles/orig/test_nat.ml2mk.ml
	cp samples/multifiles/test_add.ml2mk.ml _build/samples/multifiles/orig/test_add.ml2mk.ml
	cp samples/multifiles/test_mul.ml2mk.ml _build/samples/multifiles/orig/test_mul.ml2mk.ml

	./noCanren.native -o regression/multifiles/test_nat.ml -I _build/samples/multifiles/orig/ _build/samples/multifiles/orig/test_nat.ml2mk.ml
	cp _build/samples/multifiles/orig/test_nat.ml2mk.cmi _build/samples/multifiles/orig/test_nat.cmi

	./noCanren.native -o regression/multifiles/test_add.ml -I _build/samples/multifiles/orig/ _build/samples/multifiles/orig/test_add.ml2mk.ml
	cp _build/samples/multifiles/orig/test_add.ml2mk.cmi _build/samples/multifiles/orig/test_add.cmi

	./noCanren.native -o regression/multifiles/test_mul.ml -I _build/samples/multifiles/orig/ _build/samples/multifiles/orig/test_mul.ml2mk.ml

	$(OB) -Is samples/multifiles,regression/multifiles mul_run.native

define TESTRULES

test_$(1):
	mkdir -p regression/output
	mkdir -p _build/regression/
	cp samples/$(1).ml2mk.ml _build/regression/$(1).ml2mk.ml
	./noCanren.native -o regression/output/$(1).ml _build/regression/$(1).ml2mk.ml
	$(OB) -Is samples,regression/output $(1)_run.native

promote_test_$(1):
	mkdir -p regression/orig
	./$(1)_run.native > regression/orig/$(1).log
	./noCanren.native -o regression/orig/$(1).ml samples/$(1).ml2mk.ml

endef
$(foreach i,$(TESTS),$(eval $(call TESTRULES,$(i))))

define SAMPLERULES

sample_$(1):
	mkdir -p regression/output
	mkdir -p _build/samples/
	cp samples/$(1).ml2mk.ml _build/samples/$(1).ml2mk.ml
	cp samples/$(1)_run.ml _build/samples/$(1)_run.ml
	./noCanren.native -o regression/output/$(1).ml _build/samples/$(1).ml2mk.ml
	$(OB) -Is samples,regression/output $(1)_run.native

endef
$(foreach i,$(SAMPLES),$(eval $(call SAMPLERULES,$(i))))

compile_tests:
	$(foreach T, $(TESTS), make test_$(T);)

promote_tests:
	$(foreach T, $(TESTS), make promote_test_$(T);)

test:
	dune runtest

clean_tests:
	$(RM) -r regression/output
	$(RM) -r regression/multifiles

clean: clean_tests
	$(RM) -r _build *.native *.log regression/*.ml

sanitize:
	$(RM) micro/*.cmi samples/*.cmi samples/multifiles/*.cmi

install:
	dune build @install -p noCanren
	dune install noCanren

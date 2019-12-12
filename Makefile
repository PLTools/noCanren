OB = ocamlbuild -use-ocamlfind -classic-display

TESTS = bottles bridge einstein GCW

compile:
	$(OB) -I src noCanren.native

define TESTRULES

test_$(1):
	cp samples/$(1)_run.ml regression/$(1)_run.ml
	./noCanren.native -o regression/$(1).ml samples/$(1).ml2mk.ml
	$(OB) -I regression $(1)_run.native

promote_test_$(1):
	./$(1)_run.native > regression/orig/$(1).log
	./noCanren.native -o regression/orig/$(1).ml samples/$(1).ml2mk.ml

endef
$(foreach i,$(TESTS),$(eval $(call TESTRULES,$(i))))

tests: compile
	$(foreach T, $(TESTS), make test_$(T);)

promote_tests:
	$(foreach T, $(TESTS), make promote_test_$(T);)

run_tests: tests
	$(foreach T, $(TESTS), \
		./$(T)_run.native > regression $(T).log)

clean:
	$(RM) -r _build *.native *.log regression/*.ml regression/*.log

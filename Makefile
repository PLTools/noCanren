OB = ocamlbuild -use-ocamlfind -classic-display

TESTS = bottles bridge einstein GCW

compile:
	$(OB) -I src noCanren.native

define TESTRULES

test_$(1):
	mkdir -p regression/output
	./noCanren.native -o regression/output/$(1).ml samples/$(1).ml2mk.ml
	$(OB) -Is samples,regression/output $(1)_run.native

promote_test_$(1):
	mkdir -p regression/orig
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
		./$(T)_run.native > regression/output/$(T).log; \
		if diff -u regression/orig/${T}.log regression/output/${T}.log > regression/output/${T}.log.diff; \
			then \
				rm regression/output/${T}.log.diff; \
				if diff -u regression/orig/${T}.ml regression/output/${T}.ml > regression/output/${T}.ml.diff; \
					then \
						rm regression/output/${T}.ml.diff; \
						echo "${T}: PASSED"; \
					else echo "${T}: FAILED (see regression/output/${T}.ml.diff)"; \
				fi; \
			else echo "${T}: FAILED (see regression/output/${T}.log.diff)"; \
		fi;)

clean_tests:
	$(RM) -r regression/output

clean: clean_tests
	$(RM) -r _build *.native *.log regression/*.ml

REGRES_CASES=000 001 002 003 004

.PHONY: all tests clean

tests: all


define TESTRULES
.PHONY: test_$(1)
all: test_$(1)
#native_tests: regression/test_$(1).native
test_$(1).native: test_$(1).ml
	ocamlfind opt test_$(1).ml -o $<

test_$(1): native_tests regression/test$(1).native
	@cd regression  && $(TESTS_ENVIRONMENT) ../test$(1).native; \
	if [ $$$$? -ne 0 ] ; then echo "$(1) FAILED"; else echo "$(1) PASSED"; fi
endef
$(foreach i,$(REGRES_CASES),$(eval $(call TESTRULES,$(i)) ) )

clean:
	$(RM) -r *.cm[ioxa] *.native *.log *.diff

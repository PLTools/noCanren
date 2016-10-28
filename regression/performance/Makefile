print-%: ; @echo $*=$($*)

TESTS=001
EXPRTESTS=$(addprefix expr, $(TESTS))
CTESTS=$(addprefix c, $(TESTS))

DEFAULT_GOAL:= perf

define XXX
$$(warning arg = $(1))
$$(warning file = $$(wildcard test001*.ml) )

MLFILE = $$(wildcard test$(1)*.ml)
$$(warning R =  $$(eval $$R) )
perf$(1)_ml:
ifneq ($$(MLFILE),)
	# do some rules for ML file
	cd ../.. && ocamlbuild -use-ocamlfind -Is src,regression regression/performance/$$(MLFILE:.ml=.native)
	/usr/bin/time -f "%U" ../../$$(MLFILE:.ml=.native)
endif
perf: perf$(1)_ml

RKTFILE = $$(wildcard test$(1)*.rkt)
#$$(warning R =  $$(eval $$R) )
perf$(1)_rkt:
ifneq ($$(RKTFILE),)
	# do some rules for ML file
	raco exe --gui $$(RKTFILE) #-o $$(RKTFILE:.rkt=)
	/usr/bin/time -f "%U" ./$$(basename $$(RKTFILE) )
endif

perf: perf$(1)_rkt

endef

#$(info $(call XXX,001))
#$(eval $(call XXX,001))

$(foreach i,$(TESTS), $(eval $(call XXX,$(i)) ) )


# .PHONY: check $(EXPRTESTS) $(CTESTS) $(CTESTSO)
#
# check: prepare $(EXPRTESTS) $(CTESTS) $(CTESTSO) complete
#
#
# compile:
#
# complete:
# 	paste -d'\t' names.data o0.data expr.data expr-so.data o3.data >> data.gnuplot && gnuplot script.gnuplot
#
# prepare:
# 	rm -rf *.data && for i in $(TESTS) ; do echo $$i >> names.data ; done && echo "Test\tgcc-O0\texpr\texpr-so\tgcc-O3" > data.gnuplot
#
# $(CTESTS): %: %.c
# 	gcc -o $@-O0 -m32 -O0 $< && /usr/bin/time -o o0.data --append -f '%U' ./$@-O0 > $@.log && diff $@.log orig/`echo $@.log | sed 's/^c/expr/'`
# 	gcc -o $@-O3 -m32 -O3 $< && /usr/bin/time -o o3.data --append -f '%U' ./$@-O3 > $@.log && diff $@.log orig/`echo $@.log | sed 's/^c/expr/'`
#
# $(EXPRTESTS): %: %.expr
# 	RC_RUNTIME=../../runtime ../../rc.native     $< && /usr/bin/time -o expr.data    --append -f '%U' ./$@ > $@.log | tr -d '\n' >> data.gnuplot && diff $@.log orig/$@.log
# 	RC_RUNTIME=../../runtime ../../rc.native -so $< && /usr/bin/time -o expr-so.data --append -f '%U' ./$@ > $@.log | tr -d '\n' >> data.gnuplot && diff $@.log orig/$@.log
#
# clean:
# 	$(RM) -f *.log *.s *~ $(TESTS)
#

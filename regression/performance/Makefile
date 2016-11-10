OB=ocamlbuild -use-ocamlfind -plugin-tag "package(str)"
print-%: ; @echo $*=$($*)

DATAFILE=data.gnuplot
TESTS=001 002 004 003 005 006 007
EXPRTESTS=$(addprefix expr, $(TESTS))
CTESTS=$(addprefix c, $(TESTS))
MEASURE=/usr/bin/time -f "%U"
DUMMY_MEASURE=echo " -0.1"

.DEFAULT_GOAL := all
ML_TESTS_TO_COMPILE :=
.PHONY: compile_rkt compile_ml compile_scm compile_muscm\
	measure_rkt measure_ml measure_scm measure_muscm

define XXX
#ocanren
MLFILE_$(1) = $$(wildcard test$(1)*.ml)
ML_NATIVE_$(1) := regression/performance/$$(MLFILE_$(1):.ml=.native)
ML_TESTS_TO_COMPILE += $$(ML_NATIVE_$(1))
TEST$(1)_NAME = $$(MLFILE_$(1):test$(1)_%.ml=%)

compile$(1)_ml:
ifneq ($$(MLFILE_$(1)),)
	cd ../.. && $(OB) -Is src,regression $$(ML_NATIVE_$(1))
endif
measure$(1)_ml:
ifneq ($$(MLFILE_$(1)),)
	$(MEASURE) --append -o .$(1).data ../../$$(MLFILE_$(1):.ml=.native)
else
	$(DUMMY_MEASURE) >> .$(1).data
endif
perf: perf$(1)_ml

#racket
RKT_FILE_$(1) = $$(wildcard test$(1)*.rkt)
RKT_NATIVE_$(1) = $$(RKT_FILE_$(1)).native
#$$(warning RKTFILE_$(1) = $$(RKT_FILE_$(1)))
.PHONY: compile$(1)_rkt
ifneq ($$(RKT_FILE_$(1)),)
compile$(1)_rkt: $$(RKT_NATIVE_$(1))
$$(RKT_NATIVE_$(1)):
	raco exe -o $$(RKT_NATIVE_$(1)) $$(RKT_FILE_$(1))
compile_rkt: compile$(1)_rkt
else
compile$(1)_rkt:

endif

measure$(1)_rkt:
ifneq ($$(RKT_FILE_$(1)),)
	$(MEASURE) --append -o .$(1).data ./$$(RKT_NATIVE_$(1))
else
	$(DUMMY_MEASURE) >> .$(1).data
endif

# miniKanren in Scheme
SCM_FILE_$(1) = $(wildcard test$(1)*.chez)
SCM_NATIVE_$(1) = $$(SCM_FILE_$(1):.chez=).so

ifneq ("$$(SCM_FILE_$(1))","")
.PHONY: compile$(1)_scm
compile$(1)_scm: $$(SCM_NATIVE_$(1))
$$(SCM_NATIVE_$(1)):
	echo '(compile-file "$$(SCM_FILE_$(1))")' | scheme -q
compile_scm: compile$(1)_scm
else
compile$(1)_scm:
endif

measure$(1)_scm:
ifneq ($$(SCM_FILE_$(1)),)
	$(MEASURE) --append -o .$(1).data scheme --program $$(SCM_NATIVE_$(1))
else
	$(DUMMY_MEASURE) >> .$(1).data
endif

# muKanren in Scheme
MUSCM_FILE_$(1) = $(wildcard test$(1)*.mu.scm)
MUSCM_NATIVE_$(1) = $$(MUSCM_FILE_$(1):.scm=).so
ifneq ("$$(MUSCM_FILE_$(1))","")
.PHONY: compile$(1)_muscm
compile$(1)_muscm: $$(MUSCM_NATIVE_$(1))
$$(MUSCM_NATIVE_$(1)): $$(MUSCM_FILE_$(1))
	echo '(compile-file "$$(MUSCM_FILE_$(1))")' | scheme -q
compile_muscm: compile$(1)_muscm
else
compile$(1)_muscm:
endif

measure$(1)_muscm:
ifneq ($$(MUSCM_FILE_$(1)),)
	$(MEASURE) --append -o .$(1).data scheme --program $$(MUSCM_NATIVE_$(1))
else
	$(DUMMY_MEASURE) >> .$(1).data
endif
################################################################################
.PHONY: measure$(1) measure$(1)_prepare
measure$(1)_prepare:
	$(RM) .$(1).data .$(1).name
measure$(1): measure$(1)_prepare measure$(1)_ml measure$(1)_rkt measure$(1)_scm \
							measure$(1)_muscm
	@printf "$$(TEST$(1)_NAME) " >> $(DATAFILE)
	@tr '\n' ' ' < .$(1).data >> $(DATAFILE)
	@printf "\n" >> $(DATAFILE)
do_measure: measure$(1)

endef

.PHONY: prepare_header do_measure
prepare_header:
	echo "x     OCanren Racket Scheme uKanren/Scheme" > data.gnuplot

compile_ocanren:
	$(MAKE) -C ../.. -f Makefile.ob
	cd ../../ && $(OB) -Is src,regression $(ML_TESTS_TO_COMPILE)

.PHONY: check_submodules
check_submodules:
	if [ ! -d "../../faster-miniKanren" ]; then \
		(cd ../.. && git submodule init && git submodule update); fi

compile: check_submodules
compile: compile_ocanren compile_rkt compile_scm compile_muscm
measure: prepare_header do_measure
#$(info $(call XXX,002))
#$(eval $(call XXX,001))
all: compile measure graph
graph:
	gnuplot script.gnuplot && xdg-open graph.pdf

$(foreach i,$(TESTS), $(eval $(call XXX,$(i)) ) )

perf:
	$(MAKE) -C ../../ -f Makefile.ob plugin

.PHONY: clean
clean:
	$(RM) *.native *.so *~ ./compiled

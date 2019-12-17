#### PREAMBLE ####
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
  endif
  .RECIPEPREFIX = >

SEED = $(shell od -An -N2 -i /dev/random | tr -d ' ')
REPLICATIONS = 10
##################

### File lists

figures = \
  output/report/steps.png \
  output/report/l2_vs_nsimus.png

# Statistics

mean_std_l2_vs_nsimus = \
  output/formulas/figure_data/mean_std_l2_vs_nsimus

histo_steps = \
  output/formulas/figure_data/histo_steps/apmc_n5000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/figure_data/histo_steps/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100

# Simulation results

steps = \
  output/formulas/steps/apmc_n5000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/steps/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100
  
repli_run_l2_vs_nsimus = \
  output/formulas/repli/run/apmc_n501_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n555_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n625_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n1000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n5000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n501_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n555_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n625_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n1000_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n5000_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n501_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n555_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n625_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n1000_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n5000_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n501_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n555_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n625_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n1000_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_n5000_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n501_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n555_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n625_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n1000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n501_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n555_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n625_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n1000_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n5000_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n501_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n555_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n625_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n1000_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n5000_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n501_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n555_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n625_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n1000_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_n5000_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100


# Executables

haskfile = formulas/.stack-work/install/x86_64-linux/lts-12.14/8.4.3/bin/haskfile

#### Rules

# Avant de pouvoir générer les figures, les spécifications de
# simulations et les répertoires doivent exister et l'executable haskfile
# doit être compilé.  Les règles de simulation sont implicites
# et nécessitent que les fichiers de specification de simulations
# dans input/simu existent pour être déclenchées. On ne peut donc
# pas simplement ajouter les règles de génération de fichiers en
# dépendances, car elles ne seraient pas déclenchées.  D'autre part,
# on évite de mettre l'executable haskfile en dépendances dans les
# règles pour éviter que toutes les simulations soient refaites dès
# qu'il est recompilé (ça rallonge le temps de build quand on modifie
# des statistiques par exemple, sans toucher au code de simulation).
# Avant de générer les figures, il faut donc exécuter `make setup`.

all: $(figures)
.PHONY: all

setup: simulation-specifications directory-tree $(haskfile)
.PHONY: setup

directory-tree:
> mkdir -p output/formulas/figure_data/histo_steps
> mkdir -p output/formulas/run
> mkdir -p output/formulas/steps
> mkdir -p output/formulas/repli/steps
> mkdir -p output/formulas/repli/run
> mkdir -p output/report
> mkdir -p input/simu
.PHONY: directory-tree

simulation-specifications:
> util/populate_simu_specs.sh
.PHONY: simulation-specifications

memo: a b c a
> @echo $@: $+
> @echo '$$@' $@
> @echo '$$<' $<
> @echo '$$^' $^
> @echo '$$+' $+
.PHONY: memo

a b c:
.PHONY: a b c

test-seed:
> @echo $(SEED)
> @echo $(SEED)
.PHONY: test-seed

clean:
> rm -Rf output/formulas/*
> rm -Rf sentinel
.PHONY: clean


#### Figures ####

output/report/steps.png: \
  report/steps.gnuplot \
  sentinel/histo_steps
> gnuplot -c $< $@ $(histo_steps)

output/report/l2_vs_nsimus.png: \
  report/l2_vs_nsimus.gnuplot \
  $(mean_std_l2_vs_nsimus)
> gnuplot -c $< $@ $(mean_std_l2_vs_nsimus)



#### Statistics ####

sentinel/histo_steps : $(steps)
> $(haskfile) histo-steps \
>   $(foreach x, $(steps), --steps $(x)) \
>   $(foreach x, $(histo_steps), --histo $(x))
> mkdir -p $(@D)
> touch $@

$(mean_std_l2_vs_nsimus): $(repli_run_l2_vs_nsimus)
> $(haskfile) mean-std-l2-vs-nsimus \
>   $(foreach x, $(repli_run_l2_vs_nsimus), --run $(x)) \
>   --out $(mean_std_l2_vs_nsimus)


#### Simulations ####

output/formulas/run/%: input/simu/% 
> $(haskfile) steps $(SEED) $< $@

output/formulas/steps/%: input/simu/%   
> $(haskfile) steps $(SEED) $< $@

output/formulas/repli/run/%: input/simu/% 
> $(haskfile) repli-run $(SEED) $(REPLICATIONS) $< $@

output/formulas/repli/steps/%: input/simu/% 
> $(haskfile) repli-steps $(SEED) $(REPLICATIONS) $< $@



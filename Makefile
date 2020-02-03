#### PREAMBLE ####
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:

# This doesn't seem to work in Make 4 or above. Use the command-line option
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
  endif
  .RECIPEPREFIX = >

SEED = $(shell od -An -N2 -i /dev/random | tr -d ' ')
REPLICATIONS = 10
##################

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

all: figures
.PHONY: all

#### Executables ####

haskfile = formulas/.stack-work/install/x86_64-linux/lts-12.14/8.4.3/bin/haskfile


#### Definitions ####

## Simulations Run ##

# Don't use the sentinel pattern here. This is just a shorter way to write one
# rule per target file. Use static pattern rules rather than implicit pattern
# rules because implicit pattern rules are only triggered when the prerequisite
# exists and silent otherwise. I prefer to get an error that the prerequisite
# doesn't exist.
: output/formulas/run/%: input/simu/%
> mkdir -p output/formulas/run
> $(haskfile) run $(SEED) $< $@


## Simulation Steps ##

files_simu_steps = \
  output/formulas/steps/apmc_n5000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/steps/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100
  
# Don't use the sentinel pattern here.
$(files_simu_steps): output/formulas/steps/%: input/simu/%   
> mkdir -p output/formulas/steps
> $(haskfile) steps $(SEED) $< $@


## Simulation Repli Run ##

files_simu_repli_run_l2_vs_nsimus = \
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

# Don't use the sentinel pattern here.
$(files_simu_repli_run_l2_vs_nsimus): output/formulas/repli/run/%: input/simu/%
> mkdir -p output/formulas/repli/run
> $(haskfile) repli-run $(SEED) $(REPLICATIONS) $< $@


## Simulation Repli Steps ##

files_simu_repli_steps_l2_vs_time_k = \
  output/formulas/repli/steps/apmc_n5000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/steps/apmc_n5000_nAlpha500_pAccMin0.01_parallel2_modelToy_stepMax100 \
  output/formulas/repli/steps/apmc_n5000_nAlpha500_pAccMin0.01_parallel5_modelToy_stepMax100 \
  output/formulas/repli/steps/apmc_n5000_nAlpha500_pAccMin0.01_parallel10_modelToy_stepMax100 \
  output/formulas/repli/steps/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/steps/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel2_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/steps/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel5_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/steps/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel10_stopSampleSize4500_modelToy_stepMax100
  
# Don't use the sentinel pattern here.
$(files_simu_repli_steps_l2_vs_time_k): output/formulas/repli/steps/%: input/simu/% 
> mkdir -p output/formulas/repli/steps
> $(haskfile) repli-steps $(SEED) $(REPLICATIONS) $< $@


## Stats histo steps ##

files_stat_histo_steps = \
  output/formulas/figure_data/histo_steps/apmc_n5000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/figure_data/histo_steps/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100

$(files_stat_histo_steps) : sentinel/stat_histo_steps ;

sentinel/stat_histo_steps : $(files_simu_steps)
> mkdir -p output/formulas/figure_data/histo_steps
> $(haskfile) histo-steps \
>   $(foreach x, $(files_simu_steps), --steps $(x)) \
>   $(foreach x, $(files_stat_histo_steps), --histo $(x))
> mkdir -p $(@D)
> touch $@


## Stats mean std l2 vs nsimus ##

files_stat_mean_std_l2_vs_nsimus = output/formulas/figure_data/mean_std_l2_vs_nsimus

$(files_stat_mean_std_l2_vs_nsimus): sentinel/stat_mean_std_l2_vs_nsimus ;

sentinel/stat_mean_std_l2_vs_nsimus: $(files_simu_repli_run_l2_vs_nsimus)
> mkdir -p output/formulas/figure_data/
> $(haskfile) mean-std-l2-vs-nsimus \
>   $(foreach x, $(files_simu_repli_run_l2_vs_nsimus), --run $(x)) \
>   --out $(files_stat_mean_std_l2_vs_nsimus)
> mkdir -p $(@D)
> touch $@


## Stat L2 vs time ##

files_stat_l2_vs_time_k = \
  output/formulas/figure_data/l2_vs_time/apmc_n5000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_n5000_nAlpha500_pAccMin0.01_parallel2_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_n5000_nAlpha500_pAccMin0.01_parallel5_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_n5000_nAlpha500_pAccMin0.01_parallel10_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel2_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel5_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_n5000_nAlpha500_pAccMin0.01_stepSize1_parallel10_stopSampleSize4500_modelToy_stepMax100

# No sentinel here.
$(files_stat_l2_vs_time_k):output/formulas/figure_data/l2_vs_time/%: \
  output/formulas/repli/steps/%
> mkdir -p output/formulas/figure_data/l2_vs_time
> $(haskfile) l2-vs-time $< $@


## Figure Steps ##

files_figure_steps = output/report/steps.png

$(files_figure_steps) : sentinel/figure_steps ;

sentinel/figure_steps: \
  report/steps.gnuplot \
  $(files_stat_histo_steps)
> mkdir -p output/report
> gnuplot -c $< $(files_figure_steps) $(files_stat_histo_steps)
> mkdir -p $(@D)
> touch $@


## Figure L2 vs nsimus ## 

files_figure_l2_vs_nsimus = output/report/l2_vs_nsimus.png

$(files_figure_l2_vs_nsimus) : sentinel/figure_l2_vs_nsimus ;

sentinel/figure_l2_vs_nsimus: \
  report/l2_vs_nsimus.gnuplot \
  $(files_stat_mean_std_l2_vs_nsimus)
> mkdir -p output/report
> gnuplot -c $< $(files_figure_l2_vs_nsimus) \
>   $(files_stat_mean_std_l2_vs_nsimus)
> mkdir -p $(@D)
> touch $@


## Figure L2 vs Time K ##

files_figure_l2_vs_time_k = output/report/l2_vs_time_k.png

$(files_figure_l2_vs_time_k) : sentinel/figure_l2_vs_time_k ;

sentinel/figure_l2_vs_time_k: \
  report/l2_vs_time_k.gnuplot \
  $(files_stat_l2_vs_time_k)
> mkdir -p output/report
> gnuplot -c $< $(files_figure_l2_vs_time_k) $(files_stat_l2_vs_time_k)
> mkdir -p $(@D)
> touch $@

#### Helper rules ####

figures: \
  $(files_figure_steps) \
  $(files_figure_l2_vs_nsimus) \
  $(files_figure_l2_vs_time_k)
.PHONY: all-bottom

setup: simulation-specifications $(haskfile)
.PHONY: setup

simulation-specifications:
> mkdir -p input/simu
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

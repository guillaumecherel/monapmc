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
template = formulas/.stack-work/install/x86_64-linux/lts-12.14/8.4.3/bin/template

## Helpers ##

input/simu/%:
> $(template) $@ > $@


#### Definitions ####

## Simulations Run ##

# Don't use the sentinel pattern here. This is just a shorter way to write one
# rule per target file. Use static pattern rules rather than implicit pattern
# rules because implicit pattern rules are only triggered when the prerequisite
# exists and silent otherwise. I prefer to get an error that the prerequisite
# doesn't exist.

files_simu_run_time_bias = \
  output/formulas/run/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeBias_100_1_stepMax100 \
  output/formulas/run/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_1_stepMax10000 \
  output/formulas/run/mon-apmc_nGen1_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_1_stepMax400000

$(files_simu_run_time_bias) : output/formulas/run/%: input/simu/%
> mkdir -p output/formulas/run
> $(haskfile) run $(SEED) $< $@


## Simulation Steps ##

files_simu_steps = \
  output/formulas/steps/apmc_nGen4500_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/steps/mon-apmc_nGen4500_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100
  
files_simu_steps_time_bias = \
  output/formulas/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeBias_100_1_stepMax100 \
  output/formulas/steps/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_1_stepMax10000 \
  output/formulas/steps/mon-apmc_nGen1_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_1_stepMax400000

  
# Don't use the sentinel pattern here.
$(files_simu_steps) \
$(files_simu_steps_time_bias) \
: output/formulas/steps/%: input/simu/%   
> mkdir -p output/formulas/steps
> $(haskfile) steps $(SEED) $< $@


## Simulation Repli Run ##

files_simu_repli_run_l2_vs_nsimus = \
  output/formulas/repli/run/apmc_nGen1_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen55_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen25_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen500_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen4500_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen1_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen55_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen25_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen500_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen4500_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen1_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen55_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen25_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen500_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen4500_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen1_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen55_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen25_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen500_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/apmc_nGen4500_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen1_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen55_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen25_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen500_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen4500_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen1_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen55_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen25_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen500_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen4500_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen1_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen55_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen25_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen500_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen4500_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen1_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen55_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen25_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen500_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  output/formulas/repli/run/mon-apmc_nGen4500_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100

# Don't use the sentinel pattern here.
$(files_simu_repli_run_l2_vs_nsimus): output/formulas/repli/run/%: input/simu/%
> mkdir -p output/formulas/repli/run
> $(haskfile) repli-run $(SEED) $(REPLICATIONS) $< $@


## Simulation Repli Steps ##

files_simu_repli_steps_l2_vs_time_k = \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel2_modelToy_stepMax100 \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToy_stepMax100 \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToy_stepMax100 \
  output/formulas/repli/steps/mon-apmc_nGen4000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/repli/steps/mon-apmc_nGen2000_nAlpha500_pAccMin0.01_stepSize1_parallel2_stopSampleSize4500_modelToy_stepMax200 \
  output/formulas/repli/steps/mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToy_stepMax400 \
  output/formulas/repli/steps/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToy_stepMax10000
  
files_simu_repli_steps_l2_vs_time_k_v = \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeVar_1_1_stepMax100 \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeVar_1_100_stepMax100 \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeBias_1_1_stepMax100 \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeVar_1_1_stepMax100 \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeVar_1_100_stepMax100 \
  output/formulas/repli/steps/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeBias_1_1_stepMax100 \
  output/formulas/repli/steps/mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeVar_1_1_stepMax400 \
  output/formulas/repli/steps/mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeVar_1_100_stepMax400 \
  output/formulas/repli/steps/mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeBias_1_1_stepMax400 \
  output/formulas/repli/steps/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeVar_1_1_stepMax10000 \
  output/formulas/repli/steps/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeVar_1_100_stepMax10000 \
  output/formulas/repli/steps/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeBias_1_1_stepMax10000
  
# Don't use the sentinel pattern here.
# We use $(sort ...) to remove duplicates (see doc)
$(sort \
  $(files_simu_repli_steps_l2_vs_time_k) \
  $(files_simu_repli_steps_l2_vs_time_k_v)\
) \
: output/formulas/repli/steps/%: input/simu/% 
> mkdir -p output/formulas/repli/steps
> $(haskfile) repli-steps $(SEED) $(REPLICATIONS) $< $@


## Stats histo run ##

files_stat_histo_run_time_bias = \
  output/formulas/figure_data/histo_run/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeBias_100_1_stepMax100 \
  output/formulas/figure_data/histo_run/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_1_stepMax10000 \
  output/formulas/figure_data/histo_run/mon-apmc_nGen1_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_1_stepMax400000

# No sentinel.
$(files_stat_histo_run_time_bias) \
: output/formulas/figure_data/histo_run/% : output/formulas/run/%
> mkdir -p output/formulas/figure_data/histo_run
> $(haskfile) histo-run --run $< --histo $@


## Stats histo steps ##

files_stat_histo_steps = \
  output/formulas/figure_data/histo_steps/apmc_nGen4500_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/figure_data/histo_steps/mon-apmc_nGen4500_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100

# No sentinel.
$(files_stat_histo_steps) \
: output/formulas/figure_data/histo_steps/% : output/formulas/steps/%
> mkdir -p output/formulas/figure_data/histo_steps
> $(haskfile) histo-steps --steps $< --histo $@


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
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel2_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen4000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen2000_nAlpha500_pAccMin0.01_stepSize1_parallel2_stopSampleSize4500_modelToy_stepMax200 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToy_stepMax400 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToy_stepMax10000

files_stat_l2_vs_time_k_v = \
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeVar_1_1_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeVar_1_100_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeBias_1_1_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeVar_1_1_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeVar_1_100_stepMax100 \
  output/formulas/figure_data/l2_vs_time/apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeBias_1_1_stepMax100 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeVar_1_1_stepMax400 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeVar_1_100_stepMax400 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeBias_1_1_stepMax400 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeVar_1_1_stepMax10000 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeVar_1_100_stepMax10000 \
  output/formulas/figure_data/l2_vs_time/mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeBias_1_1_stepMax10000


# No sentinel here.
$(sort \
  $(files_stat_l2_vs_time_k) \
  $(files_stat_l2_vs_time_k_v) \
) \
:output/formulas/figure_data/l2_vs_time/%: \
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
> echo -e ""\
>   "output_path=\"$(files_figure_l2_vs_time_k)\"\n" \
>   "apmc_k1=\"$(word 1, $(files_stat_l2_vs_time_k))\"\n" \
>   "apmc_k2=\"$(word 2, $(files_stat_l2_vs_time_k))\"\n" \
>   "apmc_k5=\"$(word 3, $(files_stat_l2_vs_time_k))\"\n" \
>   "apmc_k10=\"$(word 4, $(files_stat_l2_vs_time_k))\"\n" \
>   "monApmc_k1=\"$(word 5, $(files_stat_l2_vs_time_k))\"\n" \
>   "monApmc_k2=\"$(word 6, $(files_stat_l2_vs_time_k))\"\n" \
>   "monApmc_k5=\"$(word 7, $(files_stat_l2_vs_time_k))\"\n" \
>   "monApmc_k10=\"$(word 8, $(files_stat_l2_vs_time_k))\"\n" \
> | gnuplot - $< 
> mkdir -p $(@D)
> touch $@


## Figure L2 vs Time K V ##

files_figure_l2_vs_time_k_v = output/report/l2_vs_time_k_v.png

$(files_figure_l2_vs_time_k_v) : sentinel/figure_l2_vs_time_k_v ;

sentinel/figure_l2_vs_time_k_v: \
  report/l2_vs_time_k_v.gnuplot \
  $(files_stat_l2_vs_time_k_v)
> mkdir -p output/report
> echo -e ""\
>   "output_path=\"$(files_figure_l2_vs_time_k_v)\"\n" \
>   "apmc_kLow_vLow=\"$(word 1, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "apmc_kLow_vHigh=\"$(word 2, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "apmc_kLow_vBias=\"$(word 3, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "apmc_kHigh_vLow=\"$(word 4, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "apmc_kHigh_vHigh=\"$(word 5, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "apmc_kHigh_vBias=\"$(word 6, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "monApmc_kLow_vLow=\"$(word 7, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "monApmc_kLow_vHigh=\"$(word 8, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "monApmc_kLow_vBias=\"$(word 9, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "monApmc_kHigh_vLow=\"$(word 10, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "monApmc_kHigh_vHigh=\"$(word 11, $(files_stat_l2_vs_time_k_v))\"\n" \
>   "monApmc_kHigh_vBias=\"$(word 12, $(files_stat_l2_vs_time_k_v))\"\n" \
> | gnuplot - $< 
> mkdir -p $(@D)
> touch $@


## Figure Time Bias ##

files_figure_time_bias = output/report/time_bias.png

$(files_figure_time_bias) : sentinel/figure_time_bias ;

sentinel/figure_time_bias: \
  report/time_bias.gnuplot \
  $(files_stat_histo_run_time_bias)
> mkdir -p output/report
> gnuplot -c $< $(files_figure_time_bias) $(files_stat_histo_run_time_bias)
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

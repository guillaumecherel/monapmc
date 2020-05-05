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

template := formulas/.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/template/template
haskfile := formulas/.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/haskfile/haskfile

## Helpers ##

input/simu/%:
> $(template) $@ > $@


#### Definitions ####

## Simulations ##

simulations := \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeBias_100_0_1_10_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeBias_100_0_1_10_stepMax100 \
  mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeBias_100_0_1_10_stepMax400 \
  mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeBias_100_0_1_10_stepMax10000 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeVar_1_10_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeVar_1_10_stepMax100 \
  mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeVar_1_10_stepMax400 \
  mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeVar_1_10_stepMax10000 \
  apmc_nGen1_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  apmc_nGen1_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  apmc_nGen1_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  apmc_nGen1_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  apmc_nGen25_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  apmc_nGen25_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  apmc_nGen25_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  apmc_nGen25_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToy_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeBias_100_0_1_100_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeVar_1_100_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeVar_1_1_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel2_modelToy_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToy_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeBias_100_0_1_100_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeVar_1_100_stepMax100 \
  apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeVar_1_1_stepMax100 \
  apmc_nGen4500_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  apmc_nGen4500_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  apmc_nGen4500_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  apmc_nGen4500_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  apmc_nGen500_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  apmc_nGen500_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  apmc_nGen500_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  apmc_nGen500_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  apmc_nGen55_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
  apmc_nGen55_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
  apmc_nGen55_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
  apmc_nGen55_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
  mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToy_stepMax400 \
  mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeBias_100_0_1_100_stepMax400 \
  mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeVar_1_100_stepMax400 \
  mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeVar_1_1_stepMax400 \
  mon-apmc_nGen1_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_0_1_100_stepMax400000 \
  mon-apmc_nGen1_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  mon-apmc_nGen1_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  mon-apmc_nGen1_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  mon-apmc_nGen1_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
  mon-apmc_nGen2000_nAlpha500_pAccMin0.01_stepSize1_parallel2_stopSampleSize4500_modelToy_stepMax200 \
  mon-apmc_nGen25_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  mon-apmc_nGen25_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  mon-apmc_nGen25_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  mon-apmc_nGen25_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
  mon-apmc_nGen4000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_0_1_100_stepMax10000 \
  mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToy_stepMax10000 \
  mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeBias_100_0_1_100_stepMax10000 \
  mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeVar_1_100_stepMax10000 \
  mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeVar_1_1_stepMax10000 \
  mon-apmc_nGen4500_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  mon-apmc_nGen4500_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  mon-apmc_nGen4500_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  mon-apmc_nGen4500_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
  mon-apmc_nGen500_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  mon-apmc_nGen500_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  mon-apmc_nGen500_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  mon-apmc_nGen500_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
  mon-apmc_nGen55_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  mon-apmc_nGen55_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  mon-apmc_nGen55_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
  mon-apmc_nGen55_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \

 



## Simulations Run ##

# Don't use the sentinel pattern here. This is just a shorter way to write one
# rule per target file. Use static pattern rules rather than implicit pattern
# rules because implicit pattern rules are only triggered when the prerequisite
# exists and silent otherwise. I prefer to get an error that the prerequisite
# doesn't exist.

$(foreach simu,$(simulations), output/formulas/run/$(simu)) \
: output/formulas/run/%: input/simu/%
> mkdir -p $(@D)
> $(haskfile) run $(SEED) $< $@


## Simulation Steps ##

# Don't use the sentinel pattern here.
$(foreach simu,$(simulations), output/formulas/steps/$(simu)) \
: output/formulas/steps/%: input/simu/%   
> mkdir -p $(@D)
> $(haskfile) steps $(SEED) $< $@


## Simulation Repli Run ##

# Don't use the sentinel pattern here.
$(foreach simu,$(simulations), output/formulas/repli/run/$(simu)) \
: output/formulas/repli/run/%: input/simu/%
> mkdir -p $(@D)
> $(haskfile) repli-run $(SEED) $(REPLICATIONS) $< $@


## Simulation Repli Steps ##

 
# Don't use the sentinel pattern here.
# We use $(sort ...) to remove duplicates (see doc)
$(foreach simu,$(simulations), output/formulas/repli/steps/$(simu)) \
: output/formulas/repli/steps/%: input/simu/% 
> mkdir -p $(@D)
> $(haskfile) repli-steps $(SEED) $(REPLICATIONS) $< $@


## Stats histo run ##

# No sentinel.
$(foreach simu,$(simulations),output/formulas/figure_data/histo_run/$(simu)) \
: output/formulas/figure_data/histo_run/% : output/formulas/run/%
> mkdir -p $(@D)
> $(haskfile) histo-run --run $< --histo $@


## Stats histo steps ##

# No sentinel.
$(foreach simu,$(simulations),output/formulas/figure_data/histo_steps/$(simu)) \
: output/formulas/figure_data/histo_steps/% : output/formulas/steps/%
> mkdir -p $(@D)
> $(haskfile) histo-steps --steps $< --histo $@


## Stats mean std l2 vs nsimus ##

mean_std_l2_vs_nsimus_simus := \
apmc_nGen1_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
apmc_nGen55_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
apmc_nGen25_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
apmc_nGen500_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
apmc_nGen4500_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
apmc_nGen1_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
apmc_nGen55_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
apmc_nGen25_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
apmc_nGen500_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
apmc_nGen4500_nAlpha500_pAccMin0.05_parallel1_modelToy_stepMax100 \
apmc_nGen1_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
apmc_nGen55_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
apmc_nGen25_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
apmc_nGen500_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
apmc_nGen4500_nAlpha500_pAccMin0.1_parallel1_modelToy_stepMax100 \
apmc_nGen1_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
apmc_nGen55_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
apmc_nGen25_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
apmc_nGen500_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
apmc_nGen4500_nAlpha500_pAccMin0.2_parallel1_modelToy_stepMax100 \
mon-apmc_nGen1_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
mon-apmc_nGen55_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
mon-apmc_nGen25_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
mon-apmc_nGen500_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
mon-apmc_nGen4500_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
mon-apmc_nGen1_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
mon-apmc_nGen55_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
mon-apmc_nGen25_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
mon-apmc_nGen500_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
mon-apmc_nGen4500_nAlpha500_pAccMin0.05_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
mon-apmc_nGen1_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
mon-apmc_nGen55_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
mon-apmc_nGen25_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
mon-apmc_nGen500_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
mon-apmc_nGen4500_nAlpha500_pAccMin0.1_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
mon-apmc_nGen1_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize1_modelToy_stepMax100 \
mon-apmc_nGen55_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize55_modelToy_stepMax100 \
mon-apmc_nGen25_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize125_modelToy_stepMax100 \
mon-apmc_nGen500_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize500_modelToy_stepMax100 \
mon-apmc_nGen4500_nAlpha500_pAccMin0.2_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100

mean_std_l2_vs_nsimus_input := \
$(foreach simu, $(mean_std_l2_vs_nsimus_simus), output/formulas/repli/run/$(simu))
mean_std_l2_vs_nsimus_output := output/formulas/figure_data/mean_std_l2_vs_nsimus
mean_std_l2_vs_nsimus_sentinel := sentinel/stat_mean_std_l2_vs_nsimus
  
$(mean_std_l2_vs_nsimus_output): $(mean_std_l2_vs_nsimus_sentinel) ;

$(mean_std_l2_vs_nsimus_sentinel): $(mean_std_l2_vs_nsimus_input)
> mkdir -p output/formulas/figure_data/
> $(haskfile) mean-std-l2-vs-nsimus \
>   $(foreach simu, $(mean_std_l2_vs_nsimus_simus), --run output/formulas/repli/run/$(simu)) \
>   --out $(mean_std_l2_vs_nsimus_output)
> mkdir -p $(@D)
> touch $@


## Stat L2 vs time ##

# No sentinel here.
$(foreach simu, $(simulations), output/formulas/figure_data/l2_vs_time/$(simu)) \
:output/formulas/figure_data/l2_vs_time/%: output/formulas/repli/steps/%
> mkdir -p $(@D)
> $(haskfile) l2-vs-time $< $@


## Figure Steps ##

steps_simus := \
apmc_nGen4500_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
mon-apmc_nGen4500_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100

steps_gnuplot_script := report/steps.gnuplot

steps_data := $(foreach simu,$(steps_simus),output/formulas/figure_data/histo_steps/$(simu))

steps_input := $(steps_gnuplot_script) $(steps_data)
steps_output := output/report/steps.png
steps_sentinel := sentinel/figure_steps

$(steps_output) : $(steps_sentinel) ;

$(steps_sentinel): $(steps_input)
> mkdir -p output/report
> gnuplot -c $(steps_gnuplot_script) $(steps_output) $(steps_data)
> mkdir -p $(@D)
> touch $@


## Figure L2 vs nsimus ## 

l2_vs_nsimus_gnuplot_script := report/l2_vs_nsimus.gnuplot
l2_vs_nsimus_data := output/formulas/figure_data/mean_std_l2_vs_nsimus

l2_vs_nsimus_output := output/report/l2_vs_nsimus.png
l2_vs_nsimus_input := $(l2_vs_nsimus_gnuplot_script) $(l2_vs_nsimus_data)
l2_vs_nsimus_sentinel := sentinel/figure_l2_vs_nsimus

$(l2_vs_nsimus_output) : $(l2_vs_nsimus_sentinel) ;

$(l2_vs_nsimus_sentinel): $(l2_vs_nsimus_input)
> mkdir -p output/report
> gnuplot -c $(l2_vs_nsimus_gnuplot_script) $(l2_vs_nsimus_output) $(l2_vs_nsimus_data)
> mkdir -p $(@D)
> touch $@


## Figure L2 vs Time K ##

l2_vs_time_k_gnuplot_script := report/l2_vs_time_k.gnuplot

l2_vs_time_k_simus := \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel1_modelToy_stepMax100 \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToy_stepMax100 \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToy_stepMax100 \
mon-apmc_nGen4000_nAlpha500_pAccMin0.01_stepSize1_parallel1_stopSampleSize4500_modelToy_stepMax100 \
mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToy_stepMax400 \
mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToy_stepMax10000

l2_vs_time_k_data := $(foreach simu,$(l2_vs_time_k_simus),output/formulas/figure_data/l2_vs_time/$(simu))

l2_vs_time_k_output := output/report/l2_vs_time_k.png
l2_vs_time_k_input := $(l2_vs_time_k_gnuplot_script) $(l2_vs_time_k_data)
l2_vs_time_k_sentinel := sentinel/figure_l2_vs_time_k

$(l2_vs_time_k_output) : $(l2_vs_time_k_sentinel) ;

$(l2_vs_time_k_sentinel): $(l2_vs_time_k_input)
> mkdir -p output/report
> echo -e ""\
>   "output_path=\"$(l2_vs_time_k_output)\"\n" \
>   "apmc_k_low=\"$(word 1, $(l2_vs_time_k_data))\"\n" \
>   "apmc_k_med=\"$(word 2, $(l2_vs_time_k_data))\"\n" \
>   "apmc_k_high=\"$(word 3, $(l2_vs_time_k_data))\"\n" \
>   "monApmc_k_low=\"$(word 4, $(l2_vs_time_k_data))\"\n" \
>   "monApmc_k_med=\"$(word 5, $(l2_vs_time_k_data))\"\n" \
>   "monApmc_k_high=\"$(word 6, $(l2_vs_time_k_data))\"\n" \
> | gnuplot - $(l2_vs_time_k_gnuplot_script)
> mkdir -p $(@D)
> touch $@


## Figure L2 vs Time K V ##

l2_vs_time_k_v_simus := \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToy_stepMax100 \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeVar_1_10_stepMax100 \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel4_modelToyTimeBias_100_0_1_10_stepMax100 \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToy_stepMax100 \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeVar_1_10_stepMax100 \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeBias_100_0_1_10_stepMax100 \
mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToy_stepMax400 \
mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeVar_1_10_stepMax400 \
mon-apmc_nGen1000_nAlpha500_pAccMin0.01_stepSize1_parallel4_stopSampleSize4500_modelToyTimeBias_100_0_1_10_stepMax400 \
mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToy_stepMax10000 \
mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeVar_1_10_stepMax10000 \
mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4500_modelToyTimeBias_100_0_1_10_stepMax10000

l2_vs_time_k_v_gnuplot_script := report/l2_vs_time_k_v.gnuplot

l2_vs_time_k_v_data := $(foreach simu,$(l2_vs_time_k_v_simus),output/formulas/figure_data/l2_vs_time/$(simu))

l2_vs_time_k_v_output := output/report/l2_vs_time_k_v.png
l2_vs_time_k_v_input := $(l2_vs_time_k_v_gnuplot_script) $(l2_vs_time_k_v_data)
l2_vs_time_k_v_sentinel := sentinel/figure_l2_vs_time_k_v

$(l2_vs_time_k_v_output) : $(l2_vs_time_k_v_sentinel) ;

$(l2_vs_time_k_v_sentinel): $(l2_vs_time_k_v_input)
> mkdir -p output/report
> echo -e ""\
>   "output_path=\"$(l2_vs_time_k_v_output)\"\n" \
>   "apmc_kLow_vLow=\"$(word 1, $(l2_vs_time_k_v_data))\"\n" \
>   "apmc_kLow_vHigh=\"$(word 2, $(l2_vs_time_k_v_data))\"\n" \
>   "apmc_kLow_vBias=\"$(word 3, $(l2_vs_time_k_v_data))\"\n" \
>   "apmc_kHigh_vLow=\"$(word 4, $(l2_vs_time_k_v_data))\"\n" \
>   "apmc_kHigh_vHigh=\"$(word 5, $(l2_vs_time_k_v_data))\"\n" \
>   "apmc_kHigh_vBias=\"$(word 6, $(l2_vs_time_k_v_data))\"\n" \
>   "monApmc_kLow_vLow=\"$(word 7, $(l2_vs_time_k_v_data))\"\n" \
>   "monApmc_kLow_vHigh=\"$(word 8, $(l2_vs_time_k_v_data))\"\n" \
>   "monApmc_kLow_vBias=\"$(word 9, $(l2_vs_time_k_v_data))\"\n" \
>   "monApmc_kHigh_vLow=\"$(word 10, $(l2_vs_time_k_v_data))\"\n" \
>   "monApmc_kHigh_vHigh=\"$(word 11, $(l2_vs_time_k_v_data))\"\n" \
>   "monApmc_kHigh_vBias=\"$(word 12, $(l2_vs_time_k_v_data))\"\n" \
> | gnuplot - $(l2_vs_time_k_v_gnuplot_script)
> mkdir -p $(@D)
> touch $@



## Figure Time Bias ##

time_bias_gnuplot_script := report/time_bias.gnuplot

time_bias_simus := \
apmc_nGen4000_nAlpha500_pAccMin0.01_parallel100_modelToyTimeBias_100_0_1_100_stepMax100 \
mon-apmc_nGen40_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_0_1_100_stepMax10000 \
mon-apmc_nGen1_nAlpha500_pAccMin0.01_stepSize1_parallel100_stopSampleSize4000_modelToyTimeBias_100_0_1_100_stepMax400000

time_bias_data := $(foreach simu,$(time_bias_simus),output/formulas/figure_data/histo_run/$(simu))
  
time_bias_input := $(time_bias_gnuplot_script) $(time_bias_data)
time_bias_output := output/report/time_bias.png
time_bias_sentinel := sentinel/figure_time_bias

$(time_bias_output) : $(time_bias_sentinel) ;

$(time_bias_sentinel): $(time_bias_input)
> mkdir -p output/report
> gnuplot -c $(time_bias_gnuplot_script) $(time_bias_output) $(time_bias_data)
> mkdir -p $(@D)
> touch $@



#### Helper rules ####

figures: \
  $(steps_output) \
  $(l2_vs_nsimus_output) \
  $(l2_vs_time_k_output) \
  $(l2_vs_time_k_v_output) \
  $(time_bias_output)
.PHONY: all-bottom

setup: $(haskfile)
.PHONY: setup

memo: a b c a
> @echo $@: $+
> @echo '$$@' = $@
> @echo '$$<' = $<
> @echo '$$^' = $^
> @echo '$$+' = $+
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

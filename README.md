# Introduction 

This repository contains the numerical experiments to test the algorithm MonAPMC
[REF]. This algorithm is an attempt to implement an approximate Bayesian
computation (ABC) algorithm that scales well with the number of computing cores
available to perform simulations and the complexity of the simulation model. It
is essentially the algorithm APMC [REF] equipped with a specific parallelization
scheme.

The next section explains how to rerun the simulation experiments and reproduce
the figures described in the rest of the document. 

The ABC algorithms used in these experiments and the statistical computations
are written in haskell, under the directory <./formulas>. The figures are
generated with [gnuplot](http://www.gnuplot.info/) and relevant gnuplot scripts
are located in <./report>. One set of simulation that is particularly demanding
computationally (see `Run Comp LHS` below) is run on the European Grid
Infrastructure through the virtual organization `vo.complex-systems.eu`, using
the software [OpenMOLE](https://openmole.org). The openmole script is the file
`openmole/lhs.oms`.

A consistent naming scheme accross this descriptive document, the haskell code,
gnuplot script files and the `Makefile` helps keeping track of where the
various definitions below are defined in code and data files. All the
definitions found below are associated to a code file when they are defined in
code and to data files when they stored as data (input or output, e.g.
simulation outputs or statistics). For example, the statistics `Stats Comp` is
defined in haskell code in <./formulas/src/Experiment.hs> as the function
`statsComp` and `Stats Comp LHS` is stored as data in
<./output/formulas/figure_data/stats_comp_lhs>.

The figures are also named consistently with their names in this document and
stored in <./output/report>.


# Usage: how to reproduce the results?

Dependencies:
- stack (https://tech.fpcomplete.com/haskell/get-started/)
- openmole (https://openmole.org/), version 11, git commit e56207450dd8640edf0a25465a05527a1bcb46b7

Both executables need to be on the path.

The repository uses `make` to run the simulations, compute statistics and generate the figures automatically.

To run the simulation experiment defined by the OpenMOLE script
`openmole/lhs.oms`, you will need either to have credentials to use the European
Grid Infrastructure (EGI) through the virtual organization
`vo.complex-systems.eu` or use another execution environment by changing the
`env` value in the script.

You may want to run openmole manually rather than through make to access
openmole's web interface, through which you can watch the simulation's
progression on the distant computing cluster. To do that, ensure that all the
prerequisites are built with `make openmole/formulas.container.tgz`, then run
the script <./openmole/lhs.oms> yourself using openmole.

The resulting figures are to be found in <./output/report>.


# Definitions 

Here is a list of definitions given in the companion paper. This lists will help
the reader make the correspondance between the paper and this repository by
specifying where each definition is defined as code, stored as data or as an
image file when appropriate.

- Comp
  - Code <./formulas/src/Experiment.hs>

For a first check of the previous claims, we plot the evolution of L2 against
time for replicated runs of a pair `(apmc, mon-apmc)` of comparable algorithms APMC and MonAPMC,
with different parallelism levels and model run time variance 

    (apmc, mon-apmc) = comp(nGen, nAlpha, pAccMin, parallel, stepMax, biasFactor, meanRunTime, varRunTime)
    
where

    nGen = 4000, 
    nAlpha = 500,
    pAccMin = 0.01,
    parallel = 4 and 100,
    stepMax = 100,
    biasFactor = 1,
    meanRunTime = 1
    varRunTime = 0.1, 1 and 10

- Fig L2 vs time K V
  - Image file: <./output/report/fig_l2_vs_time_k_v.png> 
- Fig Time Bias
  - Histograms showing the posterior sample for both algorithms.
- LHS
  - Code: <./openmole/lhs.oms>.
- Run Comp LHS
  - Code: <./openmole/lhs.oms>.
  - Data files: One file per pair `(apmc, mon_apmc)`, one per simulation, in 
    <./output/formulas/run_comp_lhs/>. Each file is the 
    corresponding haskell value `(comp, (run_apmc, run_mon_apmc))` formatted 
    via its Show instance, such that it can be read again easily in haskell.
- L2 Ratio
  - Code def: <./formulas/src/Experiment.hs>.
- Time Ratio
  - Code def: <./formulas/src/Experiment.hs>.
- Stats Comp
  - Code def: <./formulas/src/Experiment.hs> 
- Stats Comp LHS 
  - Data files: <./output/formulas/figure_data/stats_comp_lhs>. 
    A table where each line corresponds to a point in the LHS, 7 columns give
    the values for each parameter to comp and six additionnal columns the l2
    error and runtime for each algorithm and l2 ratio and time ratio.  The
    first line gives the columns labels.
- Fig L2 vs biasFactor
  - Code: Gnuplot 
  - Image file: <./output/report/l2_vs_bias_factor.png>.
- Fig Scatter L2 Time LHS
  - Code: <./report/l2_time_lhs.gnuplot> 
  - Image file: <./output/report/l2_time_lhs.png>. 
- Fig L2 Effects LHS
  - Code: <./report/fig_l2_effects_lhs.gnuplot>
  - Image file: <./output/report/fig_l2_effects_lhs.png>
- Fig L2 Ratio Effects LHS
  - Code: <./report/fig_l2_ratio_effects_lhs.gnuplot>
  - Image file: <./output/report/fig_l2_ratio_effects_lhs.png>
- Fig Time Effects LHS
  - Code: <./report/fig_time_effects_lhs.gnuplot>
  - Image file: <./output/report/fig_time_effects_lhs.png>
- Fig Time Ratio Effects LHS
  - Code: <./report/fig_time_ratio_effects_lhs.gnuplot>
  - Image file: <./output/report/fig_time_ratio_effects_lhs.png>
- Test Cases
  - Code: <./Makefile>
- Stats Comp Test Cases
  - Code: <./formulas/src/Experiment.hs>
  - Data file: <./output/formulas/figure_data/stats_comp_test_cases>
- Fig Comp Test Cases
  - Code: <./report/fig_comp_test_cases.gnuplot>
  - Image file: <./output/report/fig_comp_test_cases.png>

# Directory structure

- input/
  - simu: simulation specifications for all the experiments
- output/
  - blocks/: results of the simulation implemented in scala.
  - easyABC/: results of the simulations from the R package EasyABC.
  - formulas/: results of the simulations and statistics implemented in haskell.
    - figure_data: input data for figures
    - run: simulation results
    - steps: simulation steps
    - repli/ 
      - run: replicated simulation results
      - steps: replicated simulation steps
  - report/: generated figures.
- report/: scripts generating the figures.
- sentinel/: sentinel files (used in the makefile).
- formulas/: haskell code implementing the algorithms, simulations and statistics.
- blocks/: scala implementation of the algorithms and simulations (currently not used).
- util/: utility scripts.



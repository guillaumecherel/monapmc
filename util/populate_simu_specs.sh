#!/usr/bin/env bash

# Populate the directory input/simu with the desired simulation specifications.

# make bash stricter
set -euo pipefail

# make bash verbose
# set -x

apmc () {
  output=input/simu/apmc_nGen$1_nAlpha$2_pAccMin$3_parallel$4_model$5_stepMax$6
  stack exec -- template apmc $1 $2 $3 $4 $5 $6 > $output
}

mon_apmc () {
  # Set step sample size to n - nAlpha to reproduce the behaviour of APMC.
  output=input/simu/mon-apmc_nGen$1_nAlpha$2_pAccMin$3_stepSize$4_parallel$5_stopSampleSize$6_model$7_stepMax$8
  stack exec -- template mon-apmc $1 $2 $3 $4 $5 $6 $7 $8 > $output
}

fill_simulation_spec () {
  A=(`echo $1 | xargs basename | sed 's/_/ /g'`)
  algo=${A[0]}
  if test $algo = "apmc"
  then 
    nGen=${A[1]##nGen}
    test ${nGen:?[populate_simu_specs] Error: No value for nGen.}
    nAlpha=${A[2]##nAlpha}
    test ${nAlpha:?[populate_simu_specs] Error: No value for nAlpha.}
    pAccMin=${A[3]##pAccMin}
    test ${pAccMin:?[populate_simu_specs] Error: No value for pAccMin.}
    parallel=${A[4]##parallel}
    test ${parallel:?[populate_simu_specs] Error: No value for parallel.}
    model=${A[5]##model}
    test ${model:?[populate_simu_specs] Error: No value for model.}
    stepMax=${A[6]##stepMax}
    test ${stepMax:?[populate_simu_specs] Error: No value for stepMax.}

    command=(apmc $nGen $nAlpha $pAccMin $parallel $model $stepMax) 

  elif test $algo = "mon-apmc"
  then
    nGen=${A[1]##nGen}
    test ${nGen:?[populate_simu_specs] Error: No value for nGen.}
    nAlpha=${A[2]##nAlpha}
    test ${nAlpha:?[populate_simu_specs] Error: No value for nAlpha.}
    pAccMin=${A[3]##pAccMin}
    test ${pAccMin:?[populate_simu_specs] Error: No value for pAccMin.}
    stepSize=${A[4]##stepSize}
    test ${stepSize:?[populate_simu_specs] Error: No value for stepSize.}
    parallel=${A[5]##parallel}
    test ${parallel:?[populate_simu_specs] Error: No value for parallel.}
    stopSampleSize=${A[6]##stopSampleSize}
    test ${stopSampleSize:?[populate_simu_specs] Error: No value for stopSampleSize.}
    model=${A[7]##model}
    test ${model:?[populate_simu_specs] Error: No value for model.}
    stepMax=${A[8]##stepMax}
    test ${stepMax:?[populate_simu_specs] Error: No value for stepMax.}

    command=(mon_apmc $nGen $nAlpha $pAccMin $stepSize $parallel $stopSampleSize $model $stepMax)

  else
    echo "Unknown algorithm type: " ${A[*]}
    exit
  fi

  if ! eval ${command[*]}
  then 
    echo "[populate_simu_specs.sh] Error running command:" ${command[*]}
  fi
}

cd formulas

fill_simulation_spec $1

# apmc    501   500   0.01   1   Toy   100
# apmc    555   500   0.01   1   Toy   100
# apmc    625   500   0.01   1   Toy   100
# apmc   1000   500   0.01   1   Toy   100
# apmc   5000   500   0.01   1   Toy   100
# apmc   5000   500   0.01   2   Toy   100
# apmc   5000   500   0.01   5   Toy   100
# apmc   5000   500   0.01   10  Toy   100
# apmc   5000   500   0.01   500 Toy   100
# apmc    501   500   0.05   1   Toy   100
# apmc    555   500   0.05   1   Toy   100
# apmc    625   500   0.05   1   Toy   100
# apmc   1000   500   0.05   1   Toy   100
# apmc   5000   500   0.05   1   Toy   100
# apmc    501   500   0.1    1   Toy   100
# apmc    555   500   0.1    1   Toy   100
# apmc    625   500   0.1    1   Toy   100
# apmc   1000   500   0.1    1   Toy   100
# apmc   5000   500   0.1    1   Toy   100
# apmc    501   500   0.2    1   Toy   100
# apmc    555   500   0.2    1   Toy   100
# apmc    625   500   0.2    1   Toy   100
# apmc   1000   500   0.2    1   Toy   100
# apmc   5000   500   0.2    1   Toy   100
# mon_apmc    501   500   0.01   1   1   Toy   100
# mon_apmc    555   500   0.01   1   1   Toy   100
# mon_apmc    625   500   0.01   1   1   Toy   100
# mon_apmc   1000   500   0.01   1   1   Toy   100
# mon_apmc   5000   500   0.01   1   1   Toy   100
# mon_apmc   5000   500   0.01   1   2   Toy   100
# mon_apmc   5000   500   0.01   1   5   Toy   100
# mon_apmc   5000   500   0.01   1   10  Toy   100
# mon_apmc   5000   500   0.01   1   500 Toy   100
# mon_apmc    501   500   0.05   1   1   Toy   100
# mon_apmc    555   500   0.05   1   1   Toy   100
# mon_apmc    625   500   0.05   1   1   Toy   100
# mon_apmc   1000   500   0.05   1   1   Toy   100
# mon_apmc   5000   500   0.05   1   1   Toy   100
# mon_apmc    501   500   0.1    1   1   Toy   100
# mon_apmc    555   500   0.1    1   1   Toy   100
# mon_apmc    625   500   0.1    1   1   Toy   100
# mon_apmc   1000   500   0.1    1   1   Toy   100
# mon_apmc   5000   500   0.1    1   1   Toy   100
# mon_apmc    501   500   0.2    1   1   Toy   100
# mon_apmc    555   500   0.2    1   1   Toy   100
# mon_apmc    625   500   0.2    1   1   Toy   100
# mon_apmc   1000   500   0.2    1   1   Toy   100
# mon_apmc   5000   500   0.2    1   1   Toy   100
# 


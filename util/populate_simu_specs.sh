#!/usr/bin/env bash

# make bash stricter
set -euo pipefail

# make bash verbose
# set -x

apmc () {
  output=input/simu/apmc_n$1_nAlpha$2_pAccMin$3_parallel$4_model$5_stepMax$6
  stack exec -- template apmc $1 $2 $3 $4 $5 $6 > $output
}

mon_apmc () {
  # Set step sample size to n - nAlpha to reproduce the behaviour of APMC.
  sss=`echo $1 - $2 | bc`
  output=input/simu/mon-apmc_n$1_nAlpha$2_pAccMin$3_stepSize$4_parallel$5_stopSampleSize${sss}_model$6_stepMax$7
  stack exec -- template mon-apmc $1 $2 $3 $4 $5 $sss $6 $7 > $output
}

cd formulas

apmc    501   500   0.01   1   Toy   100
apmc    555   500   0.01   1   Toy   100
apmc    625   500   0.01   1   Toy   100
apmc   1000   500   0.01   1   Toy   100
apmc   5000   500   0.01   1   Toy   100
apmc    501   500   0.05   1   Toy   100
apmc    555   500   0.05   1   Toy   100
apmc    625   500   0.05   1   Toy   100
apmc   1000   500   0.05   1   Toy   100
apmc   5000   500   0.05   1   Toy   100
apmc    501   500   0.1    1   Toy   100
apmc    555   500   0.1    1   Toy   100
apmc    625   500   0.1    1   Toy   100
apmc   1000   500   0.1    1   Toy   100
apmc   5000   500   0.1    1   Toy   100
apmc    501   500   0.2    1   Toy   100
apmc    555   500   0.2    1   Toy   100
apmc    625   500   0.2    1   Toy   100
apmc   1000   500   0.2    1   Toy   100
apmc   5000   500   0.2    1   Toy   100
mon_apmc    501   500   0.01   1   1   Toy   100
mon_apmc    555   500   0.01   1   1   Toy   100
mon_apmc    625   500   0.01   1   1   Toy   100
mon_apmc   1000   500   0.01   1   1   Toy   100
mon_apmc   5000   500   0.01   1   1   Toy   100
mon_apmc    501   500   0.05   1   1   Toy   100
mon_apmc    555   500   0.05   1   1   Toy   100
mon_apmc    625   500   0.05   1   1   Toy   100
mon_apmc   1000   500   0.05   1   1   Toy   100
mon_apmc   5000   500   0.05   1   1   Toy   100
mon_apmc    501   500   0.1    1   1   Toy   100
mon_apmc    555   500   0.1    1   1   Toy   100
mon_apmc    625   500   0.1    1   1   Toy   100
mon_apmc   1000   500   0.1    1   1   Toy   100
mon_apmc   5000   500   0.1    1   1   Toy   100
mon_apmc    501   500   0.2    1   1   Toy   100
mon_apmc    555   500   0.2    1   1   Toy   100
mon_apmc    625   500   0.2    1   1   Toy   100
mon_apmc   1000   500   0.2    1   1   Toy   100
mon_apmc   5000   500   0.2    1   1   Toy   100



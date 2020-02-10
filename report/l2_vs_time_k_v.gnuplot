# This script requires the following variables:
# output_path
# apmc_kLow_vLow
# apmc_kLow_vHigh
# apmc_kHigh_vLow
# apmc_kHigh_vHigh
# monApmc_kLow_vLow
# monApmc_kLow_vHigh
# monApmc_kHigh_vLow
# monApmc_kHigh_vHigh

set terminal pngcairo truecolor size 1000,700 font ',12'
set output output_path

# set xrange [0:*]
set yrange [0:.3]
set grid

# set xtics border autofreq
# set xtics rotate by -45
# set format x "%.1g" 

set multiplot layout 2,3 rowsfirst

set xrange [0:17000]
set xtics 5000
set title "K 4 V 1"
plot monApmc_kLow_vLow u 1:4 with lines t 'MonAPMC', \
     apmc_kLow_vLow u 1:4 with lines t 'APMC'

set xrange [0:17000]
set xtics 5000
set title "K 4 V 100"
plot monApmc_kLow_vHigh u 1:4 with lines t 'MonAPMC', \
     apmc_kLow_vHigh u 1:4 with lines t 'APMC'


set xrange [*:*]
set xtics autofreq # 5000
set title "K 4 V Bias"
plot monApmc_kLow_vBias u 1:4 with lines t 'MonAPMC', \
     apmc_kLow_vBias u 1:4 with lines t 'APMC'

set xrange [0:700]
set xtics 200
set title "APMC K 100 V 1"
plot monApmc_kHigh_vLow u 1:4 with lines t 'MonAPMC', \
     apmc_kHigh_vLow u 1:4 with lines t 'APMC'

set xrange [0:5000]
set xtics 1000
set title "APMC K 100 V 100"
plot monApmc_kHigh_vHigh u 1:4 with lines t 'MonAPMC', \
     apmc_kHigh_vHigh u 1:4 with lines t 'APMC'

set xrange [*:*]
set xtics autofreq # 200
set title "APMC K 100 V Bias"
plot monApmc_kHigh_vBias u 1:4 with lines t 'MonAPMC', \
     apmc_kHigh_vBias u 1:4 with lines t 'APMC'

unset multiplot


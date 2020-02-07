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

set terminal pngcairo truecolor size 700,700 font ',12'
set output output_path

# set xrange [0:*]
set yrange [0:.3]
set grid

# set xtics border autofreq
# set xtics rotate by -45
# set format x "%.1g" 

set multiplot layout 2,2 rowsfirst

set xrange [0:2e4]
# set xtics 5e3

set title "K 4 V 0.01"
plot apmc_kLow_vLow u 1:4 with lines t 'APMC', \
     monApmc_kLow_vLow u 1:4 with lines t 'MonAPMC'

set title "K 4 V 100"
plot apmc_kLow_vHigh u 1:4 with lines t 'APMC', \
     monApmc_kLow_vHigh u 1:4 with lines t 'MonAPMC'




set xrange [0:*]
# set xtics 250

set title "APMC K 100 V 0.01"
plot apmc_kHigh_vLow u 1:4 with lines t 'APMC', \
     monApmc_kHigh_vLow u 1:4 with lines t 'MonAPMC'

set title "APMC K 100 V 100"
plot apmc_kHigh_vHigh u 1:4 with lines t 'APMC', \
     monApmc_kHigh_vHigh u 1:4 with lines t 'MonAPMC'

unset multiplot


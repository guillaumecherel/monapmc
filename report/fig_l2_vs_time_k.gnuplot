# This script requires the following variables:
# output_path
# apmc_k_low
# apmc_k_med
# apmc_k_high
# monApmc_k_low
# monApmc_k_med
# monApmc_k_high

set terminal pngcairo truecolor size 1000,300 font ',12'
set output output_path

set xrange [0:*]
set yrange [0:.25]
set grid

set ylabel "L2"
set xlabel "time"

set xtics border autofreq
set xtics rotate by -45
set format x "%.g" 

set multiplot layout 1,3 columnsfirst

#set xrange [0:8e4]
#set xtics 2e4
set title "K = 1"
plot apmc_k_low u 1:4 with lines t 'APMC', \
     monApmc_k_low u 1:4 with lines t 'MonAPMC'

#set xrange [0:8e4]
#set xtics 2e4
set title "K = 4"
plot apmc_k_med u 1:4 with lines t 'APMC', \
     monApmc_k_med u 1:4 with lines t 'MonAPMC'

#set xrange [0:8e4]
#set xtics 2e4
set title "K = 100"
plot apmc_k_high u 1:4 with lines t 'APMC', \
     monApmc_k_high u 1:4 with lines t 'MonAPMC'

unset multiplot

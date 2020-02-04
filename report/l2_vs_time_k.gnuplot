# This script requires the following variables:
# output_path
# apmc_k1
# apmc_k2
# apmc_k5
# apmc_k10
# monApmc_k1
# monApmc_k2
# monApmc_k5
# monApmc_k10

set terminal pngcairo truecolor size 1000,1400 font ',12'
set output output_path

set xrange [0:*]
set yrange [0:.22]
set grid

set xtics border autofreq
set format x "%.g" 

set multiplot layout 4,2 rowsfirst

set xrange [0:6e4]
set xtics 2e4
set title "APMC K = 1"
plot apmc_k1 u 1:4 with linespoint t ''

set title "MonAPMC K = 1"
plot monApmc_k1 u 1:4 with linespoint t ''

#set xrange [0:4e4]
#set xtics 1e4
set title "APMC K = 2"
plot apmc_k2 u 1:4 with linespoint t ''

set title "MonAPMC K = 2"
plot monApmc_k2 u 1:4 with linespoint t ''

#set xrange [0:2e4]
#set xtics 0.5e4
set title "APMC K = 4"
plot apmc_k5 u 1:4 with linespoint t ''

set title "MonAPMC K = 4"
plot monApmc_k5 u 1:4 with linespoint t ''

#set xrange [0:*]
#set xtics 0.2e4
set title "APMC K = 10"
plot apmc_k10 u 1:4 with linespoint t ''

set title "MonAPMC K = 10"
plot monApmc_k10 u 1:4 with linespoint t ''

unset multiplot

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

set terminal pngcairo truecolor size 700,1000 font ',12'
set output output_path

set xrange [0:*]
set yrange [0:.25]
set grid

set ylabel "L2"
set xlabel "time"

set xtics border autofreq
set format x "%.g" 

set multiplot layout 4,2 rowsfirst

set xrange [0:8e4]
set xtics 2e4
set title "APMC K = 1"
plot apmc_k1 u 1:4 with lines t ''

set title "MonAPMC K = 1"
plot monApmc_k1 u 1:4 with lines t ''

set xrange [0:4e4]
set xtics 1e4
set title "APMC K = 2"
plot apmc_k2 u 1:4 with lines t ''

set title "MonAPMC K = 2"
plot monApmc_k2 u 1:4 with lines t ''

set xrange [0:2e4]
set xtics 0.5e4
set title "APMC K = 4"
plot apmc_k5 u 1:4 with lines t ''

set title "MonAPMC K = 4"
plot monApmc_k5 u 1:4 with lines t ''

set xrange [0:800]
set xtics 200 # autofreq rotate
unset format x
set title "APMC K = 100"
plot apmc_k10 u 1:4 with lines t ''

set title "MonAPMC K = 100"
plot monApmc_k10 u 1:4 with lines t ''

unset multiplot

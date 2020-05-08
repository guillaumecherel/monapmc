output_path=ARG1
data_apmc=ARG2
data_mon_apmc_nGen40=ARG3

set terminal pngcairo truecolor size 500,200 font ',12'
set xrange [-1.5:1.5]
set yrange [0:1]
# set samples 500
# set isosamples 500
set style fill solid 1.0

set key off

set output output_path

ROW=1
COL=2
set multiplot layout ROW,COL rowsfirst

set title "APMC"
plot data_apmc using 1:2 linecolor 3 with boxes fillstyle solid 1.0

set title "MonAPMC"
plot data_mon_apmc_nGen40 using 1:2 linecolor 3 with boxes fillstyle solid 1.0

unset multiplot


output_path=ARG1
data_apmc=ARG2
data_mon_apmc_nGen40=ARG3

set terminal pngcairo truecolor size 500,200 font ',12'
set xrange [-3:3]
set yrange [0:2.2]
# set samples 500
# set isosamples 500
set style fill solid 1.0

set key off

set output output_path

posterior(x) = 0.5 * normal(x, 0.0, 1.0 / 100.0) + 0.5 * normal(x, 0.0, 1.0)
normal(x, mean, var) = exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)

ROW=1
COL=2
set multiplot layout ROW,COL rowsfirst

set title "APMC"
plot data_apmc using 1:2 linecolor 3 with boxes fillstyle solid 1.0, \
     posterior(x) linecolor 0 dashtype 1 with line

set title "MonAPMC"
plot data_mon_apmc_nGen40 using 1:2 linecolor 3 with boxes fillstyle solid 1.0, \
    posterior(x) linecolor 0 dashtype 1 with line

unset multiplot


output_path=ARG1
data_apmc=ARG2
data_mon_apmc=ARG3

set terminal pngcairo truecolor size 1000,500 font ',12'
set xrange [-3:3]
set yrange [0:2.2]
# set samples 500
# set isosamples 500
set style fill solid 1.0

ROW=2
COL=5

set key off

posterior(x) = 0.5 * normal(x, 0.0, 1.0 / 100.0) + 0.5 * normal(x, 0.0, 1.0)
normal(x, mean, var) = exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)

set output output_path

set multiplot layout ROW,COL rowsfirst

# stats data_apmc nooutput

# do for [i=1:STATS_blocks] {
do for [i in "1 2 3 4 5"] {
  set title "APMC\nStep ".i
  plot \
    data_apmc \
      index (i - 1) using 1:2 linecolor 3 with boxes fillstyle solid 1.0, \
    posterior(x) linecolor 0 dashtype 1 with line
}

# do for [i=1:COL - STATS_blocks] {set multiplot next}

# stats data_mon_apmc nooutput

# do for [i=1:STATS_blocks] {
do for [i in "1 2 3 4 5"] {
  set title "MonAPMC\nStep ".i
  plot \
    data_mon_apmc \
      index (i - 1) using 1:2 linecolor 3 with boxes fillstyle solid 1.0, \
    posterior(x) linecolor 0 dashtype 1 with line
}

# do for [i=1:COL - STATS_blocks] {set multiplot next}

unset multiplot

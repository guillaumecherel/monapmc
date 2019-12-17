output_path=ARG1
datafile=ARG2


set terminal png truecolor
set grid
set key on inside horizontal
# set yrange [0:.25]
set ytics 0,.05
# set xrange [0.2e5:33e5]
set logscale x 2
# set xtics (0.125e5, 0.25e5, 0.5e5, 1e5, 2e5, 4e5, 8e5, 16e5, 32e5)
set xtics 3125, 2

set output output_path

set title 'L2 vs nSimus'

set key autotitle columnhead
    
# plot for [i=0:*] datafile index i linecolor 1 pointtype 1 with xyerrorbars

plot \
  datafile index 0 linecolor 1 pointtype 1 with xyerrorbars, \
  datafile index 1 linecolor 1 pointtype 2 with xyerrorbars, \
  datafile index 2 linecolor 1 pointtype 3 with xyerrorbars, \
  datafile index 3 linecolor 1 pointtype 4 with xyerrorbars, \
  datafile index 4 linecolor 2 pointtype 1 with xyerrorbars, \
  datafile index 5 linecolor 2 pointtype 2 with xyerrorbars, \
  datafile index 6 linecolor 2 pointtype 3 with xyerrorbars, \
  datafile index 7 linecolor 2 pointtype 4 with xyerrorbars

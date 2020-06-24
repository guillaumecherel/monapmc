output_path=ARG1
datafile=ARG2

set datafile separator ","

set output output_path

set key off 

set multiplot layout 4,2

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "Run time"
set yrange [100:5e7]
set log y
set ytics format "%g"

set style circle radius screen 0.001
set style fill solid noborder 

parToEnhanced(par) = \
par eq "nAlpha"?"Nα": \
par eq "parallel"?"K": \
par

do for [par in parameters] {
  set xlabel parToEnhanced(par)
  set xtics rotate by -60
  plot datafile using par:"compTimeApmc" with circles lc 1, \
       datafile using par:"compTimeMonApmc" with circles lc 2
}

set style circle radius 2
set style fill solid noborder 
set key on right center opaque nobox
unset border
unset xlabel
unset ylabel
unset xtics 
unset ytics 
unset log
set xrange [0:1]
set yrange [0:1]
plot 1/0 with circles lc 1 t "APMC", \
     1/0 with circles lc 2 t "MonAPMC"

unset multiplot

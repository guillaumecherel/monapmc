output_path=ARG1
datafile=ARG2

set datafile separator ","

set terminal png truecolor size 1200,500 font ',12'
set output output_path

set key off 

set multiplot layout 2,4

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "Run time"
set yrange [100:5e7]
set log y
set ytics format "%g"

do for [par in parameters] {
  set xlabel par
  set xtics rotate by -60
  plot datafile using par:"compTimeApmc" with dots lc 1, \
       datafile using par:"compTimeMonApmc" with dots lc 2
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

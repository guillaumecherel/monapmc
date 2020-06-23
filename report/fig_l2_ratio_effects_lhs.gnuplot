output_path=ARG1
datafile=ARG2

set datafile separator ","

set terminal png truecolor size 700,1000 font ',12'
set output output_path

set key off

set multiplot layout 4,2

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "L2 error ratio"
set yrange [0.1:5]
#set log y
set ytics format "%g"

do for [par in parameters] {
  set xlabel par
  set xtics rotate by -45
  plot datafile using par:"compL2Ratio" with dots lc 3, \
       1 with line lw 2 lc black
}

unset multiplot

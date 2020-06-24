output_path=ARG1
datafile=ARG2

set datafile separator ","

set output output_path

set key off

set multiplot layout 4,2

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "L2 error ratio"
set yrange [0.1:5]
#set log y
set ytics format "%g"

set style circle radius screen 0.001
set style fill solid noborder 

parToEnhanced(par) = \
par eq "nAlpha"?"Nα": \
par eq "parallel"?"K": \
par

do for [par in parameters] {
  set xlabel parToEnhanced(par)
  set xtics rotate by -45
  plot datafile using par:"compL2Ratio" with circle lc 3, \
       1 with line lw 2 lc black
}

unset multiplot

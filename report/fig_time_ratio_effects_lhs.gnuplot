output_path=ARG1
datafile=ARG2

set datafile separator ","

set output output_path

set key off

set multiplot layout 4,2

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "Run time ratio"
set yrange [0.5:50]
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
  set xtics rotate by -45
  plot datafile using par:"compTimeRatio" with circle lc 3, \
       1 with line lw 2 lc black
}

set xlabel "nGen / parallel"
set xrange [0:6]
plot datafile using (column("nGen") / column("parallel")):"compTimeRatio" with circle lc 3, \
     1 with line lw 2 lc black

unset multiplot


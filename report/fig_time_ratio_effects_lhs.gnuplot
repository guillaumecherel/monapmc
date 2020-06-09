output_path=ARG1
datafile=ARG2

set datafile separator ","

set terminal png truecolor size 1200,500 font ',12'
set output output_path

set key off

set multiplot layout 2,4

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "Run time ratio"
set yrange [0.5:50]
set log y
set ytics format "%g"

do for [par in parameters] {
  set xlabel par
  set xtics rotate by -45
  plot datafile using par:"compTimeRatio" with dots lc 3, \
       1 with line lw 2 lc black
}

set xlabel "nGen / parallel"
set xrange [0:6]
plot datafile using (column("nGen") / column("parallel")):"compTimeRatio" with dots lc 3, \
     1 with line lw 2 lc black

unset multiplot


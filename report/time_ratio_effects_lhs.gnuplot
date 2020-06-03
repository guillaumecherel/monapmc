output_path=ARG1
datafile=ARG2

set datafile separator ","

set terminal png truecolor size 1200,500 font ',12'
set output output_path

set key off

set multiplot layout 2,4

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "Run time ratio"
set yrange [*:10 ** 6]
set log y

do for [par in parameters] {
  set xlabel par
  set xtics rotate by -45
  plot datafile using par:"compTimeRatio" with dots lc 0.8, \
       1 lc black lw 2
}

unset multiplot

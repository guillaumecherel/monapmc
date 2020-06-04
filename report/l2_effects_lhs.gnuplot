output_path=ARG1
datafile=ARG2

set datafile separator ","

set terminal png truecolor size 1200,500 font ',12'
set output output_path

set key off

set multiplot layout 2,4

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

filter(x,p,g) = g > p ? x : 1/0

do for [par in parameters] {
  set ylabel "L2 Error"
  set xlabel par
  set xtics rotate by -60
  plot datafile using par:"compL2Apmc" with dots lc 1 t "APMC", \
       datafile using par:"compL2MonApmc" with dots lc 2 t "MonAPMC"
}

unset multiplot

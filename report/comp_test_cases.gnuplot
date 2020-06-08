output_path=ARG1
datafile=ARG2

set terminal png truecolor size 1500,500 font ',12'
set output output_path

set key off

set style circle radius 0.03
set style fill solid  

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "Run time ratio"
set xlabel "sqrt(varRunTime) / meanRunTime"

set xtics rotate by -60
# set log x
set xtics format "%g"

set xrange [0:*]

set offset graph 0.0, 0.05, 0.05, 0.05

set multiplot layout 1,3
plot datafile index 0 using 1:2:3 with errorlines lc 1 t "Test case 1"
plot datafile index 1 using 1:2:3 with errorlines lc 2 t "Test case 2"
plot datafile index 2 using 1:2:3 with errorlines lc 3 t "Test case 3"
unset multiplot


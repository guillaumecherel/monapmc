output_path=ARG1
datafile=ARG2

set terminal png truecolor size 700,300 font ',12'
set output output_path

set key off

set style circle radius 0.03
set style fill solid  

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set ylabel "Run time ratio"
set xlabel "√varRunTime / meanRunTime"

set xtics rotate by -60
# set log x
set xtics format "%g"

set xrange [0:4.1]

set multiplot layout 1,3
plot datafile index 0 using 1:2:3 with errorlines lc 1 t "Test case 1", 1 lc black lw 2
plot datafile index 1 using 1:2:3 with errorlines lc 2 t "Test case 2", 1 lc black lw 2
plot datafile index 2 using 1:2:3 with errorlines lc 3 t "Test case 3", 1 lc black lw 2
unset multiplot


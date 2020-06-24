output_path=ARG1
datafile=ARG2

set output output_path

set key on left top

set style circle radius 0.03
set style fill solid  

parameters = "nGen nAlpha pAccMin parallel biasFactor meanRunTime varRunTime"

set log y
set ylabel "Run time ratio"
set xlabel "sqrt(varRunTime) / meanRunTime"

set xtics 1

set xrange [0:4.1]
set yrange [0.5:*]

plot datafile index 0 using 1:2:3 with errorlines lc 1 t "Test case 1", \
     datafile index 1 using 1:2:3 with errorlines lc 2 t "Test case 2", \
     datafile index 2 using 1:2:3 with errorlines lc 3 t "Test case 3"


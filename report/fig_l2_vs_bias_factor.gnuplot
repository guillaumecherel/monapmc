output_path=ARG1
datafile=ARG2

set datafile separator ","

set output output_path

set key off

set ylabel "L2 error"
set xlabel "Bias factor"
set xtics 0,20,100
set xrange [0:100]

set style circle radius 0.5 
set style fill solid

plot datafile using "biasFactor":"compL2MonApmc" with circles lc 3

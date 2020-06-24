output_path=ARG1
datafile=ARG2

set datafile separator ","

set output output_path
set style fill solid 1.0

set key off

set multiplot layout 1,2


set ylabel "APMC L2 error"
set yrange [0:0.3]
set ytics 0.1
set xlabel "MonAPMC L2 error"
set xrange [0:0.3]
set xtics 0.1

set style data circles 
set style circle radius screen 0.001 nowedge noclip 
set style fill transparent solid 0.5 noborder

plot datafile using "compL2MonApmc":"compL2Apmc" lc 3, \
     x with line lc black lw 2


set ylabel "APMC run time"
set yrange [0:1e6]
set ytics 5e5 format "%.0e"
set xlabel "MonAPMC run time"
set xrange [0:1e6]
set xtics 5e5 format "%.0e"

plot datafile using "compTimeMonApmc":"compTimeApmc" lc 3, \
     x with line lc black lw 2



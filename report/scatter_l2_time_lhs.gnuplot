output_path=ARG1
datafile=ARG2

set datafile separator ","

set terminal png truecolor size 800,400 font ',12'
set output output_path
# set xrange [-4:4]
# set yrange [0:3]
# set samples 500
# set isosamples 500
set style fill solid 1.0

set key off

set multiplot layout 1,2

set ylabel "APMC L2 error"
set xlabel "MonAPMC L2 error"

set yrange [0:*]
set xrange [0:*]

set style data circles 
set style circle radius screen 0.003 nowedge noclip 
set style fill transparent solid 1 noborder

plot datafile using "compL2MonApmc":"compL2Apmc" lc 0.8, \
     x with line lc black lw 2

set ylabel "APMC run time"
set xlabel "MonAPMC run time"

set yrange [0:3e8]
set xrange [0:1e6]

set xtics format "%.0e"
set ytics format "%.0e"

plot datafile using "compTimeMonApmc":"compTimeApmc" lc 0.8, \
     x with line lc black lw 2



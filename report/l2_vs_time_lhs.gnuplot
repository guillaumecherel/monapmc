output_path=ARG1
datafile=ARG2

set terminal png truecolor size 800,400 font ',12'
set output output_path
# set xrange [-4:4]
# set yrange [0:3]
# set samples 500
# set isosamples 500
set style fill solid 1.0

set key off

set multiplot layout 1,2

set xlabel "MonAPMC L2 error"
set ylabel "APMC L2 error"

set xrange [0:.6]
set yrange [0:.6]

set style data circles 
set style circle radius screen 0.003 nowedge noclip
set style fill transparent solid 0.2 noborder

plot datafile using "compL2MonApmc":"compL2Apmc" linecolor 1

set xlabel "MonAPMC run time"
set ylabel "APMC run time"

set xrange [0:1e7]
set yrange [0:1e9]


plot datafile using "compTimeMonApmc":"compTimeApmc" linecolor 2 

